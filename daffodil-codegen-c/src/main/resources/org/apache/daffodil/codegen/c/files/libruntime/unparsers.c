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
#include "unparsers.h"
#include <assert.h>   // for assert
#include <stdbool.h>  // for bool
#include <stdio.h>    // for fwrite
#include "errors.h"   // for eof_or_error
#include "p_endian.h" // for htobe64, htole64, htobe32, htole32
// clang-format on

// Helper macros to get "n" highest bits from a byte's high end

#define BYTE_WIDTH 8
#define BIG_ENDIAN_DATA 1
#define LITTLE_ENDIAN_DATA 0
#define LOW_MASK(n) ((1 << n) - 1)
#define HIGH_MASK(n) (LOW_MASK(n) << (BYTE_WIDTH - n))
#define HIGH_BITS(byte, n) ((byte & HIGH_MASK(n)) >> (BYTE_WIDTH - n))

// Helper method to write bits using whole bytes while storing
// remaining bits not yet written within a fragment byte; expects last
// bits of last byte to be already shifted to left end

// Note callers must check ustate->pu.error after calling write_bits and
// update ustate->pu.bitPos0b themselves after successful unparses

static void
write_bits(const uint8_t *bytes, size_t num_bits, UState *ustate)
{
    // Copy as many bytes directly to stream as possible
    size_t ix_bytes = 0;
    if (!ustate->numUnwritBits)
    {
        size_t num_bytes = num_bits / BYTE_WIDTH;
        if (num_bytes)
        {
            size_t count = fwrite(bytes, 1, num_bytes, ustate->pu.stream);
            if (count < num_bytes)
            {
                ustate->pu.error = eof_or_error(ustate->pu.stream);
                return;
            }
            num_bits -= count * BYTE_WIDTH;
            ix_bytes += count;
        }
    }

    // Fill and copy the fragment byte as many times as needed
    while (num_bits + ustate->numUnwritBits >= BYTE_WIDTH)
    {
        // Fill the fragment byte
        uint8_t whole_byte = bytes[ix_bytes++];
        size_t num_bits_fill = BYTE_WIDTH - ustate->numUnwritBits;
        ustate->unwritBits <<= num_bits_fill;
        ustate->unwritBits |= HIGH_BITS(whole_byte, num_bits_fill);
        ustate->numUnwritBits += num_bits_fill;
        num_bits -= num_bits_fill;
        whole_byte <<= num_bits_fill;

        // Copy the fragment byte to stream
        size_t num_bits_write = BYTE_WIDTH;
        size_t count = fwrite(&ustate->unwritBits, 1, 1, ustate->pu.stream);
        if (count < 1)
        {
            ustate->pu.error = eof_or_error(ustate->pu.stream);
            return;
        }
        ustate->numUnwritBits -= num_bits_write;

        // Copy any remaining unused bits from the whole byte to the
        // fragment byte
        size_t num_bits_unused = BYTE_WIDTH - num_bits_fill;
        if (num_bits_unused > num_bits) num_bits_unused = num_bits;
        if (num_bits_unused)
        {
            ustate->unwritBits <<= num_bits_unused;
            ustate->unwritBits |= HIGH_BITS(whole_byte, num_bits_unused);
            ustate->numUnwritBits += num_bits_unused;
            num_bits -= num_bits_unused;
        }
    }

    // Fill the fragment byte one last time
    if (num_bits)
    {
        assert(num_bits + ustate->numUnwritBits < BYTE_WIDTH);

        ustate->unwritBits <<= num_bits;
        ustate->unwritBits |= HIGH_BITS(bytes[ix_bytes], num_bits);
        ustate->numUnwritBits += num_bits;
    }
}

// Helper method to write doubles depending on data endianness;
// num_bits must be exactly 64 bits

static void
unparse_endian_double(bool big_endian_data, double number, size_t num_bits, UState *ustate)
{
    // Unparse all doubles in ths helper function
    union
    {
        uint8_t bytes[sizeof(double)];
        double number;
        uint64_t integer;
    } buffer;

    // Don't need to fill with any padding bits since doubles must be
    // exactly 64 bits, otherwise SDE would happen sooner
    assert(num_bits == sizeof(double) * BYTE_WIDTH);

    // Convert host endianness to data endianness
    buffer.number = number;
    if (big_endian_data)
    {
        buffer.integer = htobe64(buffer.integer);
    }
    else
    {
        buffer.integer = htole64(buffer.integer);
    }

    // Write data bits and update our last successful write position
    write_bits(buffer.bytes, num_bits, ustate);
    if (ustate->pu.error) return;
    ustate->pu.bitPos0b += num_bits;
}

// Helper method to write floats depending on data endianness;
// num_bits must be exactly 32 bits

void
unparse_endian_float(bool big_endian_data, float number, size_t num_bits, UState *ustate)
{
    // Unparse all floats in this helper function
    union
    {
        uint8_t bytes[sizeof(float)];
        float number;
        uint32_t integer;
    } buffer;

    // Don't need to fill with any padding bits since floats must be
    // exactly 32 bits, otherwise SDE would happen sooner
    assert(num_bits == sizeof(float) * BYTE_WIDTH);

    // Convert host endianness to data endianness
    buffer.number = number;
    if (big_endian_data)
    {
        buffer.integer = htobe32(buffer.integer);
    }
    else
    {
        buffer.integer = htole32(buffer.integer);
    }

    // Write data bits and update our last successful write position
    write_bits(buffer.bytes, num_bits, ustate);
    if (ustate->pu.error) return;
    ustate->pu.bitPos0b += num_bits;
}

// Helper method to write signed integers using fragment byte shifts
// depending on data endianness (note not tested on big-endian
// architecture; might work only on low-endian architecture)

static void
unparse_endian_int64(bool big_endian_data, int64_t number, size_t num_bits, UState *ustate)
{
    // Unparse all signed integers in this helper function
    union
    {
        uint8_t bytes[sizeof(int64_t)];
        int64_t integer;
    } buffer;

    // Shift data bits differently on endianness
    buffer.integer = number;
    if (big_endian_data)
    {
        // Shift data bits and pad with fill bits
        size_t shift = sizeof(int64_t) * BYTE_WIDTH - num_bits;
        buffer.integer <<= shift;

        // Convert host endianness to data endianness
        buffer.integer = htobe64(buffer.integer);
    }
    else
    {
        // Pad with any bits needed to fill most significant byte
        size_t msb_ix = (num_bits - 1) / BYTE_WIDTH;
        size_t shift = (BYTE_WIDTH - num_bits % BYTE_WIDTH) % BYTE_WIDTH;
        buffer.bytes[msb_ix] <<= shift;

        // Convert host endianness to data endianness
        buffer.integer = htole64(buffer.integer);
    }

    // Write data bits and update our last successful write position
    write_bits(buffer.bytes, num_bits, ustate);
    if (ustate->pu.error) return;
    ustate->pu.bitPos0b += num_bits;
}

// Helper method to write unsigned integers using fragment byte shifts
// depending on data endianness (note not tested on big-endian
// architecture; might work only on low-endian architecture)

// Also note that we probably could use this helper function to write
// signed integers too but let's make debugging easier

static void
unparse_endian_uint64(bool big_endian_data, uint64_t number, size_t num_bits, UState *ustate)
{
    // Unparse all unsigned integers in this helper function
    union
    {
        uint8_t bytes[sizeof(uint64_t)];
        uint64_t integer;
    } buffer;

    // Shift data bits differently on endianness
    buffer.integer = number;
    if (big_endian_data)
    {
        // Shift data bits and pad with fill bits
        size_t shift = sizeof(uint64_t) * BYTE_WIDTH - num_bits;
        buffer.integer <<= shift;

        // Convert host endianness to data endianness
        buffer.integer = htobe64(buffer.integer);
    }
    else
    {
        // Pad with any bits needed to fill most significant byte
        size_t msb_ix = (num_bits - 1) / BYTE_WIDTH;
        size_t shift = (BYTE_WIDTH - num_bits % BYTE_WIDTH) % BYTE_WIDTH;
        buffer.bytes[msb_ix] <<= shift;

        // Convert host endianness to data endianness
        buffer.integer = htole64(buffer.integer);
    }

    // Write data bits and update our last successful write position
    write_bits(buffer.bytes, num_bits, ustate);
    if (ustate->pu.error) return;
    ustate->pu.bitPos0b += num_bits;
}

// Unparse all binary booleans, real numbers, and integers in helper
// functions, but wrap calls for type safety and simpler calls

void
unparse_be_bool(bool number, size_t num_bits, uint32_t true_rep, uint32_t false_rep, UState *ustate)
{
    uint64_t integer = number ? true_rep : false_rep;
    unparse_endian_uint64(BIG_ENDIAN_DATA, integer, num_bits, ustate);
}

void
unparse_be_double(double number, size_t num_bits, UState *ustate)
{
    unparse_endian_double(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_float(float number, size_t num_bits, UState *ustate)
{
    unparse_endian_float(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_int16(int16_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_int32(int32_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_int64(int64_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_int8(int8_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_uint16(uint16_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_uint32(uint32_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_uint64(uint64_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_be_uint8(uint8_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(BIG_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_bool(bool number, size_t num_bits, uint32_t true_rep, uint32_t false_rep, UState *ustate)
{
    uint64_t integer = number ? true_rep : false_rep;
    unparse_endian_uint64(LITTLE_ENDIAN_DATA, integer, num_bits, ustate);
}

void
unparse_le_double(double number, size_t num_bits, UState *ustate)
{
    unparse_endian_double(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_float(float number, size_t num_bits, UState *ustate)
{
    unparse_endian_float(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_int16(int16_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_int32(int32_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_int64(int64_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_int8(int8_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_int64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_uint16(uint16_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_uint32(uint32_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_uint64(uint64_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

void
unparse_le_uint8(uint8_t number, size_t num_bits, UState *ustate)
{
    unparse_endian_uint64(LITTLE_ENDIAN_DATA, number, num_bits, ustate);
}

// Unparse opaque bytes from hexBinary field

void
unparse_hexBinary(HexBinary hexBinary, UState *ustate)
{
    write_bits(hexBinary.array, hexBinary.lengthInBytes * BYTE_WIDTH, ustate);
    if (ustate->pu.error) return;
    ustate->pu.bitPos0b += hexBinary.lengthInBytes * BYTE_WIDTH;
}

// Unparse alignment bits up to alignmentInBits or end_bitPos0b

void
unparse_align_to(size_t alignmentInBits, const uint8_t fill_byte, UState *ustate)
{
    size_t end_bitPos0b = ((ustate->pu.bitPos0b + alignmentInBits - 1) / alignmentInBits) * alignmentInBits;
    unparse_alignment_bits(end_bitPos0b, fill_byte, ustate);
}

void
unparse_alignment_bits(size_t end_bitPos0b, const uint8_t fill_byte, UState *ustate)
{
    assert(ustate->pu.bitPos0b <= end_bitPos0b);

    size_t fill_bits = end_bitPos0b - ustate->pu.bitPos0b;
    while (fill_bits)
    {
        size_t num_bits = (fill_bits >= BYTE_WIDTH) ? BYTE_WIDTH : fill_bits;
        write_bits(&fill_byte, num_bits, ustate);
        if (ustate->pu.error) return;
        fill_bits -= num_bits;
    }

    // If we got all the way here, update our last successful write position
    ustate->pu.bitPos0b = end_bitPos0b;
}

// Flush the fragment byte if not done yet

void
flush_fragment_byte(const uint8_t fill_byte, UState *ustate)
{
    // Skip the flush if we already have an error
    if (!ustate->pu.error)
    {
        // Do we have any unwritten bits left in the fragment byte?
        if (ustate->numUnwritBits)
        {
            // Fill the fragment byte
            size_t num_bits_fill = BYTE_WIDTH - ustate->numUnwritBits;
            ustate->unwritBits <<= num_bits_fill;
            ustate->unwritBits |= HIGH_BITS(fill_byte, num_bits_fill);

            // Flush the fragment byte
            size_t num_bits_write = ustate->numUnwritBits;
            size_t count = fwrite(&ustate->unwritBits, 1, 1, ustate->pu.stream);
            if (count < 1)
            {
                ustate->pu.error = eof_or_error(ustate->pu.stream);
                num_bits_write = 0;
            }
            ustate->numUnwritBits -= num_bits_write;
            ustate->pu.bitPos0b += num_bits_write;
        }
    }
}
