/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// auto-maintained by iwyu
// clang-format off
#include "parsers.h"
#include <assert.h>   // for assert
#include <stdbool.h>  // for bool, false, true
#include <stdio.h>    // for fread, fgetc, EOF
#include <stdlib.h>   // for free, malloc
#include "errors.h"   // for Error, eof_or_error, ERR_LEFTOVER_DATA, Error::(anonymous), ERR_HEXBINARY_ALLOC, ERR_PARSE_BOOL
#include "p_endian.h" // for be64toh, le64toh, be32toh, le32toh
// clang-format on

// Helper macros to get "n" highest bits from a byte's high end

#define BYTE_WIDTH 8
#define BIG_ENDIAN_DATA 1
#define LITTLE_ENDIAN_DATA 0
#define LOW_MASK(n) ((1 << n) - 1)
#define HIGH_MASK(n) (LOW_MASK(n) << (BYTE_WIDTH - n))
#define HIGH_BITS(byte, n) ((byte & HIGH_MASK(n)) >> (BYTE_WIDTH - n))

// Helper method to read bits using whole bytes while storing
// remaining bits not yet read within a fragment byte; returns last
// bits of last byte already shifted to left end

// Note callers must check pstate->pu.error after calling read_bits and
// update pstate->pu.bitPos0b themselves after successful parses

static void
read_bits(uint8_t *bytes, size_t num_bits, PState *pstate)
{
    // Copy as many bytes directly from stream as possible
    size_t ix_bytes = 0;
    if (!pstate->numUnreadBits)
    {
        size_t num_bytes = num_bits / BYTE_WIDTH;
        if (num_bytes)
        {
            size_t count = fread(bytes, 1, num_bytes, pstate->pu.stream);
            if (count < num_bytes)
            {
                pstate->pu.error = eof_or_error(pstate->pu.stream);
                return;
            }
            num_bits -= count * BYTE_WIDTH;
            ix_bytes += count;
        }
    }

    // Copy and fill the fragment byte as many times as needed
    while (num_bits > pstate->numUnreadBits)
    {
        // Copy one whole byte from stream to temporary storage
        size_t whole_byte = 0;
        size_t count = fread(&whole_byte, 1, 1, pstate->pu.stream);
        if (count < 1)
        {
            pstate->pu.error = eof_or_error(pstate->pu.stream);
            return;
        }

        // Copy bits from whole byte to fill fragment byte
        size_t num_bits_fill = BYTE_WIDTH - pstate->numUnreadBits;
        pstate->unreadBits <<= num_bits_fill;
        pstate->unreadBits |= HIGH_BITS(whole_byte, num_bits_fill);
        pstate->numUnreadBits += num_bits_fill;
        whole_byte <<= num_bits_fill;

        // Copy bits from fragment byte to `bytes`
        size_t num_bits_read = BYTE_WIDTH;
        if (num_bits_read > num_bits) num_bits_read = num_bits;
        num_bits -= num_bits_read;
        bytes[ix_bytes++] = pstate->unreadBits & HIGH_MASK(num_bits_read);
        pstate->numUnreadBits -= num_bits_read;

        // Copy rest of bits from whole byte to fragment byte
        size_t num_bits_unread = BYTE_WIDTH - num_bits_fill;
        assert(num_bits_unread + pstate->numUnreadBits < BYTE_WIDTH);
        if (num_bits_unread)
        {
            pstate->unreadBits <<= num_bits_unread;
            pstate->unreadBits |= HIGH_BITS(whole_byte, num_bits_unread);
            pstate->numUnreadBits += num_bits_unread;
        }
    }

    // Copy bits from fragment byte to `bytes` one last time, keeping
    // unread bits in right end (NOT shifted left)
    assert(num_bits <= pstate->numUnreadBits);
    if (num_bits)
    {
        // Shift the unread bits to the left end so we can copy some
        // of them starting from the first unread bit
        size_t shift = BYTE_WIDTH - pstate->numUnreadBits;
        pstate->unreadBits <<= shift;
        bytes[ix_bytes] = pstate->unreadBits & HIGH_MASK(num_bits);
        pstate->numUnreadBits -= num_bits;

        // Shift any remaining unread bits back to the right end since
        // we append new unread bits from the right
        pstate->unreadBits >>= shift;
    }
}

// Helper method to read doubles depending on data endianness;
// num_bits must be exactly 64 bits

static void
parse_endian_double(bool big_endian_data, double *number, size_t num_bits, PState *pstate)
{
    // Parse all doubles in ths helper function
    union
    {
        uint8_t bytes[sizeof(double)];
        double number;
        uint64_t integer;
    } buffer;

    // Read data bits
    buffer.integer = 0;
    read_bits(buffer.bytes, num_bits, pstate);
    if (pstate->pu.error) return;

    // Convert data endianness to host endianness
    if (big_endian_data)
    {
        buffer.integer = be64toh(buffer.integer);
    }
    else
    {
        buffer.integer = le64toh(buffer.integer);
    }

    // Don't need to remove any padding bits since doubles must be
    // exactly 64 bits, otherwise SDE would happen sooner
    assert(num_bits == sizeof(double) * BYTE_WIDTH);

    // Return successfully parsed number and update our last
    // successful parse position
    *number = buffer.number;
    pstate->pu.bitPos0b += num_bits;
}

// Helper method to read floats depending on data endianness; note
// num_bits must be exactly 32 bits

static void
parse_endian_float(bool big_endian_data, float *number, size_t num_bits, PState *pstate)
{
    // Parse all floats in this helper function
    union
    {
        uint8_t bytes[sizeof(float)];
        float number;
        uint32_t integer;
    } buffer;

    // Read data bits
    buffer.integer = 0;
    read_bits(buffer.bytes, num_bits, pstate);
    if (pstate->pu.error) return;

    // Convert data endianness to host endianness
    if (big_endian_data)
    {
        buffer.integer = be32toh(buffer.integer);
    }
    else
    {
        buffer.integer = le32toh(buffer.integer);
    }

    // Don't need to remove any padding bits since floats must be
    // exactly 32 bits, otherwise SDE would happen sooner
    assert(num_bits == sizeof(float) * BYTE_WIDTH);

    // Return successfully parsed number and update our last
    // successful parse position
    *number = buffer.number;
    pstate->pu.bitPos0b += num_bits;
}

// Helper method to read signed integers using fragment byte shifts
// depending on data endianness (note not tested on big-endian
// architecture; might work only on low-endian architecture)

// When shifting signed integers, it is important that the type be
// signed, otherwise the shift will not preserve the sign bit; this is
// why we need a separate helper method for signed integers

static void
parse_endian_int64(bool big_endian_data, int64_t *number, size_t num_bits, PState *pstate)
{
    // Parse all signed integers in this helper function
    union
    {
        uint8_t bytes[sizeof(int64_t)];
        int64_t integer;
    } buffer;

    // Read data bits
    buffer.integer = 0;
    read_bits(buffer.bytes, num_bits, pstate);
    if (pstate->pu.error) return;

    // Shift data bits differently on endianness
    if (big_endian_data)
    {
        // Convert data endianness to host endianness
        buffer.integer = be64toh(buffer.integer);

        // Need only num_bits so remove any padding bits
        size_t shift = sizeof(int64_t) * 8 - num_bits;
        buffer.integer >>= shift; // type must be signed
    }
    else
    {
        // Convert data endianness to host endianness
        buffer.integer = le64toh(buffer.integer);

        // Need only num_bits so remove any padding bits
        size_t msb_ix = (num_bits - 1) / BYTE_WIDTH;
        int8_t msb_byte = (int8_t)buffer.bytes[msb_ix];
        size_t shift = (BYTE_WIDTH - num_bits % BYTE_WIDTH) % BYTE_WIDTH;
        buffer.bytes[msb_ix] = msb_byte >> shift; // type must be signed

        // Also, negative low-endian integers require setting
        // following bytes to -1 to preserve the sign bit too
        if (msb_byte < 0)
        {
            for (msb_ix++; msb_ix < sizeof(int64_t); msb_ix++)
            {
                buffer.bytes[msb_ix] = -1;
            }
        }
    }

    // Return successfully parsed number and update our last
    // successful parse position
    *number = buffer.integer;
    pstate->pu.bitPos0b += num_bits;
}

// Helper method to read unsigned integers using fragment byte shifts
// depending on data endianness (note not tested on big-endian
// architecture; might work only on low-endian architecture)

static void
parse_endian_uint64(bool big_endian_data, uint64_t *number, size_t num_bits, PState *pstate)
{
    // Parse all unsigned integers in this helper function
    union
    {
        uint8_t bytes[sizeof(uint64_t)];
        uint64_t integer;
    } buffer;

    // Read data bits
    buffer.integer = 0;
    read_bits(buffer.bytes, num_bits, pstate);
    if (pstate->pu.error) return;

    // Shift data bits differently on endianness
    if (big_endian_data)
    {
        // Convert data endianness to host endianness
        buffer.integer = be64toh(buffer.integer);

        // Need only num_bits so remove any padding bits
        size_t shift = sizeof(uint64_t) * BYTE_WIDTH - num_bits;
        buffer.integer >>= shift;
    }
    else
    {
        // Convert data endianness to host endianness
        buffer.integer = le64toh(buffer.integer);

        // Need only num_bits so remove any padding bits
        size_t msb_ix = (num_bits - 1) / BYTE_WIDTH;
        size_t shift = (BYTE_WIDTH - num_bits % BYTE_WIDTH) % BYTE_WIDTH;
        buffer.bytes[msb_ix] >>= shift;
    }

    // Return successfully parsed number and update our last
    // successful parse position
    *number = buffer.integer;
    pstate->pu.bitPos0b += num_bits;
}

// Helper method to read booleans depending on data endianness;
// num_bits should be 1 to 32 bits

static void
parse_endian_bool(bool big_endian_data, bool *number, size_t num_bits, int64_t true_rep, uint32_t false_rep,
                  PState *pstate)
{
    // Parse all booleans in this helper function
    const size_t last_successful_parse = pstate->pu.bitPos0b;
    uint64_t integer;

    // Booleans are limited to 32 bits in the DFDL spec, but we read
    // all unsigned integers with parse_endian_uint64 using num_bits
    assert(num_bits && num_bits <= sizeof(uint32_t) * BYTE_WIDTH);

    // parse_endian_uint64 will change position of last successful parse
    parse_endian_uint64(big_endian_data, &integer, num_bits, pstate);
    if (pstate->pu.error) return;

    // Recognize true or false representation and assign boolean value
    // negative true_rep means it is absent and only false_rep needs
    // to be checked, otherwise true_rep must fit within uint32_t
    assert(true_rep <= UINT32_MAX);
    if (true_rep < 0)
    {
        *number = (integer != false_rep);
    }
    else if (integer == (uint32_t)true_rep)
    {
        *number = true;
    }
    else if (integer == false_rep)
    {
        *number = false;
    }
    else
    {
        static Error error = {ERR_PARSE_BOOL, {0}};
        error.arg.d64 = (int64_t)integer;
        pstate->pu.error = &error;

        // Restore original position of last successful parse
        pstate->pu.bitPos0b = last_successful_parse;
    }
}

// Parse all binary booleans, real numbers, and integers in helper
// functions, but wrap calls for type safety and simpler calls

void
parse_be_bool(bool *number, size_t num_bits, int64_t true_rep, uint32_t false_rep, PState *pstate)
{
    parse_endian_bool(BIG_ENDIAN_DATA, number, num_bits, true_rep, false_rep, pstate);
}

void
parse_be_double(double *number, size_t num_bits, PState *pstate)
{
    parse_endian_double(BIG_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_be_float(float *number, size_t num_bits, PState *pstate)
{
    parse_endian_float(BIG_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_be_int16(int16_t *number, size_t num_bits, PState *pstate)
{
    int64_t integer;
    parse_endian_int64(BIG_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (int16_t)integer;
}

void
parse_be_int32(int32_t *number, size_t num_bits, PState *pstate)
{
    int64_t integer;
    parse_endian_int64(BIG_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (int32_t)integer;
}

void
parse_be_int64(int64_t *number, size_t num_bits, PState *pstate)
{
    parse_endian_int64(BIG_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_be_int8(int8_t *number, size_t num_bits, PState *pstate)
{
    int64_t integer;
    parse_endian_int64(BIG_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (int8_t)integer;
}

void
parse_be_uint16(uint16_t *number, size_t num_bits, PState *pstate)
{
    uint64_t integer;
    parse_endian_uint64(BIG_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (uint16_t)integer;
}

void
parse_be_uint32(uint32_t *number, size_t num_bits, PState *pstate)
{
    uint64_t integer;
    parse_endian_uint64(BIG_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (uint32_t)integer;
}

void
parse_be_uint64(uint64_t *number, size_t num_bits, PState *pstate)
{
    parse_endian_uint64(BIG_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_be_uint8(uint8_t *number, size_t num_bits, PState *pstate)
{
    uint64_t integer;
    parse_endian_uint64(BIG_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (uint8_t)integer;
}

void
parse_le_bool(bool *number, size_t num_bits, int64_t true_rep, uint32_t false_rep, PState *pstate)
{
    parse_endian_bool(LITTLE_ENDIAN_DATA, number, num_bits, true_rep, false_rep, pstate);
}

void
parse_le_double(double *number, size_t num_bits, PState *pstate)
{
    parse_endian_double(LITTLE_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_le_float(float *number, size_t num_bits, PState *pstate)
{
    parse_endian_float(LITTLE_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_le_int16(int16_t *number, size_t num_bits, PState *pstate)
{
    int64_t integer;
    parse_endian_int64(LITTLE_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (int16_t)integer;
}

void
parse_le_int32(int32_t *number, size_t num_bits, PState *pstate)
{
    int64_t integer;
    parse_endian_int64(LITTLE_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (int32_t)integer;
}

void
parse_le_int64(int64_t *number, size_t num_bits, PState *pstate)
{
    parse_endian_int64(LITTLE_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_le_int8(int8_t *number, size_t num_bits, PState *pstate)
{
    int64_t integer;
    parse_endian_int64(LITTLE_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (int8_t)integer;
}

void
parse_le_uint16(uint16_t *number, size_t num_bits, PState *pstate)
{
    uint64_t integer;
    parse_endian_uint64(LITTLE_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (uint16_t)integer;
}

void
parse_le_uint32(uint32_t *number, size_t num_bits, PState *pstate)
{
    uint64_t integer;
    parse_endian_uint64(LITTLE_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (uint32_t)integer;
}

void
parse_le_uint64(uint64_t *number, size_t num_bits, PState *pstate)
{
    parse_endian_uint64(LITTLE_ENDIAN_DATA, number, num_bits, pstate);
}

void
parse_le_uint8(uint8_t *number, size_t num_bits, PState *pstate)
{
    uint64_t integer;
    parse_endian_uint64(LITTLE_ENDIAN_DATA, &integer, num_bits, pstate);
    *number = (uint8_t)integer;
}

// Allocate memory for hexBinary array

void
alloc_hexBinary(HexBinary *hexBinary, size_t num_bytes, PState *pstate)
{
    // Free old byte array
    assert(hexBinary->dynamic);
    free(hexBinary->array);

    // Allocate new byte array
    hexBinary->array = malloc(num_bytes);
    hexBinary->lengthInBytes = num_bytes;

    // Return error if necessary
    if (num_bytes && hexBinary->array == NULL)
    {
        static Error error = {ERR_HEXBINARY_ALLOC, {0}};
        error.arg.d64 = (int64_t)num_bytes;
        pstate->pu.error = &error;
    }
}

// Parse opaque bytes into hexBinary array

void
parse_hexBinary(HexBinary *hexBinary, PState *pstate)
{
    read_bits(hexBinary->array, hexBinary->lengthInBytes * BYTE_WIDTH, pstate);
    if (pstate->pu.error) return;
    pstate->pu.bitPos0b += hexBinary->lengthInBytes * BYTE_WIDTH;
}

// Parse alignment bits up to alignmentInBits or end_bitPos0b

void
parse_align_to(size_t alignmentInBits, PState *pstate)
{
    size_t end_bitPos0b = ((pstate->pu.bitPos0b + alignmentInBits - 1) / alignmentInBits) * alignmentInBits;
    parse_alignment_bits(end_bitPos0b, pstate);
}

void
parse_alignment_bits(size_t end_bitPos0b, PState *pstate)
{
    assert(pstate->pu.bitPos0b <= end_bitPos0b);

    size_t fill_bits = end_bitPos0b - pstate->pu.bitPos0b;
    uint8_t bytes[1];
    while (fill_bits)
    {
        size_t num_bits = (fill_bits >= BYTE_WIDTH) ? BYTE_WIDTH : fill_bits;
        read_bits(bytes, num_bits, pstate);
        if (pstate->pu.error) return;
        fill_bits -= num_bits;
    }

    // If we got all the way here, update our last successful parse position
    pstate->pu.bitPos0b = end_bitPos0b;
}

// Check for any data left over after end of parse

void
no_leftover_data(PState *pstate)
{
    // Skip the check if we already have an error
    if (!pstate->pu.error)
    {
        // Check for any unread bits left in pstate's fragment byte
        if (pstate->numUnreadBits)
        {
            // We have some unread bits remaining, so report leftover data
            static Error error = {ERR_LEFTOVER_DATA, {0}};
            error.arg.c = pstate->numUnreadBits;
            pstate->pu.error = &error;
        }
        else
        {
            // Check for any unread bytes left in input stream
            int c = fgetc(pstate->pu.stream);
            if (c != EOF)
            {
                // We have some unread bytes remaining, so report leftover data
                static Error error = {ERR_LEFTOVER_DATA, {0}};
                error.arg.c = BYTE_WIDTH;
                pstate->pu.error = &error;
            }
        }
    }
}
