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

// clang-format off
#include <criterion/criterion.h>   // for CR_VA_HEAD_HELPER_2, CRI_IF_DEFINED_NODEFER_2, CR_VA_TAIL_HELPER_2, CRITERION_APPLY_3, CRI_IF_DEFINED_2, CR_VA_HEAD_HELPER_1, Test
#include <criterion/new/assert.h>  // for CRI_ASSERT_OP_VAR_TAGGED, CRI_ASSERT_OP_TYPE_TAGGED, CRI_ASSERT_OP_VAL_TAGGED, CRI_ASSERT_TEST_TAG_u8, cr_user_u8_tostr, CRI_ASSERT_OP_MKNODE_TAGGED, CRI_ASSERT_OP_NAME_TAGGED, CRI_ASSERT_TEST_TAG_sz, cr_user_sz_tostr, CRI_ASSERT_TYPE_TAG_ID_u8, cr_user_u8_eq, CRI_ASSERT_TEST_TAG_ptr, cr_user_ptr_tostr, CRI_ASSERT_MKLIST_2, CRI_ASSERT_SPECIFIER_OP1, CRI_ASSERT_SPECIFIER_eq, CRI_ASSERT_SPEC_OPLEN_2, CRI_ASSERT_TEST_SPECIFIER_eq, cr_expect, CRI_ASSERT_TYPE_TAG_u8, CRI_ASSERT_TYPE_TAG_ID_sz, cr_user_sz_eq, CRI_ASSERT_TYPE_TAG_sz, CRI_ASSERT_TYPE_TAG_ID_ptr, cr_user_ptr_eq, CRI_ASSERT_TEST_TAGC_u8, CRI_ASSERT_TEST_TAG_int, cr_user_int_tostr, CRI_ASSERT_TYPE_TAG_ptr, CRI_ASSERT_TEST_TAGC_sz, CRI_ASSERT_TYPE_TAG_ID_int, cr_user_int_eq, CRI_ASSERT_TEST_TAGC_ptr, CRI_ASSERT_TYPE_TAG_int, CRI_ASSERT_TEST_TAG_i16, CRI_ASSERT_TEST_TAG_i32, CRI_ASSERT_TEST_TAG_i64, CRI_ASSERT_TEST_TAG_i8, CRI_ASSERT_TEST_TAG_u16, CRI_ASSERT_TEST_TAG_u32, CRI_ASSERT_TE...
#include <criterion/stats.h>       // for CR_CHECKERROR_1
#include <stdbool.h>               // for false, true, bool
#include <stdint.h>                // for uint32_t, int16_t, int32_t, int64_t, int8_t, uint16_t, uint64_t, uint8_t
#include <stdio.h>                 // for fclose, NULL, fflush, fmemopen, open_memstream, FILE, size_t
#include <stdlib.h>                // for free
#include "infoset.h"               // for UState, PState, flushUState
#include "parsers.h"               // for parse_be_bool, parse_le_bool, parse_be_int16, parse_be_int32, parse_be_int64, parse_be_int8, parse_be_uint16, parse_be_uint32, parse_be_uint64, parse_be_uint8, parse_le_int16, parse_le_int32, parse_le_int64, parse_le_int8, parse_le_uint16, parse_le_uint32, parse_le_uint64, parse_le_uint8
#include "unparsers.h"             // for unparse_be_bool, unparse_le_bool, unparse_be_int16, unparse_be_int32, unparse_be_int64, unparse_be_int8, unparse_be_uint16, unparse_be_uint32, unparse_be_uint64, unparse_be_uint8, unparse_le_int16, unparse_le_int32, unparse_le_int64, unparse_le_int8, unparse_le_uint16, unparse_le_uint32, unparse_le_uint64, unparse_le_uint8
// clang-format on

Test(bits, be_bool_24)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true24_rep = 076543210; // 0b_111_110_101_100_011_010_001_000
    uint32_t false_rep = 0;

    // Verify that ustate writes 11111010, 11000110, 10001000
    unparse_be_bool(true, 24, true24_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 24), "ustate should advance 24 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0), "ustate should hold nothing");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 3), "stream should hold 3 bytes");
    cr_expect(eq(u8, buffer[0], 0372), "stream should hold 0b_11_111_010");
    cr_expect(eq(u8, buffer[1], 0306), "stream should hold 0b_11_000_110");
    cr_expect(eq(u8, buffer[2], 0210), "stream should hold 0b_10_001_000");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads true_rep correctly
    bool number = false;
    parse_be_bool(&number, 24, true24_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 24), "pstate should advance 24 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0), "pstate should hold nothing");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, be_bool_4)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true4_rep = 012; // 0b_1_010
    uint32_t false_rep = 015; // 0b_1_101

    // Verify that ustate writes 10101101
    unparse_be_bool(true, 4, true4_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 4), "ustate should advance 4 bits");
    cr_expect(eq(u8, ustate.unwritBits, 012), "ustate should hold 0b_1_010");
    cr_expect(eq(u8, ustate.unwritLen, 4), "ustate should buffer 4 bits");
    cr_expect(eq(sz, size, 0), "stream should be empty");
    unparse_be_bool(false, 4, true4_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 8), "ustate should advance 4 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0255), "ustate should hold 0b_10_101_101");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 1), "stream should have 1 byte");
    cr_expect(eq(u8, buffer[0], 0255), "stream should hold 0b_10_101_101");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads true4_rep, false_rep correctly
    bool number = false;
    parse_be_bool(&number, 4, true4_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 4), "pstate should advance 4 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0255), "pstate should hold 0b_10_101_101");
    cr_expect(eq(u8, pstate.unreadLen, 4), "pstate should buffer 4 bits");
    number = false;
    parse_be_bool(&number, 4, true4_rep, false_rep, &pstate);
    cr_expect(eq(int, number, false), "boolean number should be false");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 8), "pstate should advance 4 bits");
    cr_expect(eq(u8, pstate.unreadBits, 015), "pstate should hold 0b_1_101");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, be_bool_7_7)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true7_rep = 0146; // 0b_1_100_110
    uint32_t false_rep = 0157; // 0b_1_101_111

    // Verify that ustate writes 11001101
    unparse_be_bool(true, 7, true7_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 7), "ustate should advance 7 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0146), "ustate should hold 0b_01_100_110");
    cr_expect(eq(u8, ustate.unwritLen, 7), "ustate should buffer 7 bits");
    cr_expect(eq(sz, size, 0), "stream should be empty");
    unparse_be_bool(false, 7, true7_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 14), "ustate should advance 7 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0157), "ustate should hold 0b_01_101_111");
    cr_expect(eq(u8, ustate.unwritLen, 6), "ustate should buffer 6 bits");
    cr_expect(eq(sz, size, 1), "stream should have 1 byte");
    cr_expect(eq(u8, buffer[0], 0315), "stream should hold 0b_11_001_101");

    // Verify that flushing ustate writes 10111100
    flushUState(&ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 16), "ustate should advance 2 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0274), "ustate should hold 0b_10_111_100");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 2), "stream should have 2 bytes");
    cr_expect(eq(u8, buffer[1], 0274), "stream should hold 0b_10_111_100");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads true7_rep, false_rep correctly
    bool number = false;
    parse_be_bool(&number, 7, true7_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 7), "pstate should advance 7 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0315), "pstate should hold 0b_11_001_101");
    cr_expect(eq(u8, pstate.unreadLen, 1), "pstate should buffer 1 bit");
    number = false;
    parse_be_bool(&number, 7, true7_rep, false_rep, &pstate);
    cr_expect(eq(int, number, false), "boolean number should be false");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 14), "pstate should advance 7 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0274), "pstate should hold 0b_10_111_100");
    cr_expect(eq(u8, pstate.unreadLen, 2), "pstate should buffer 2 bits");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, be_bool_9_7)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true9_rep = 0747; // 0b_111_100_111
    uint32_t true7_rep = 0167; //   0b_1_110_111
    uint32_t false_rep = 0;

    // Verify that ustate writes 11110011,1 1110111
    unparse_be_bool(true, 9, true9_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 9), "ustate should advance 9 bits");
    cr_expect(eq(u8, ustate.unwritBits, 01), "ustate should hold 0b_00_000_001");
    cr_expect(eq(u8, ustate.unwritLen, 1), "ustate should buffer 1 bit");
    cr_expect(eq(sz, size, 1), "stream should hold 1 byte");
    cr_expect(eq(u8, buffer[0], 0363), "stream should hold 0b_11_110_011");
    unparse_be_bool(true, 7, true7_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 16), "ustate should advance 7 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0367), "ustate should hold 0b_11_110_111");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 2), "stream should hold 2 bytes");
    cr_expect(eq(u8, buffer[1], 0367), "stream should hold 0b_11_110_111");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads true9_rep, true7_rep correctly
    bool number = false;
    parse_be_bool(&number, 9, true9_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 9), "pstate should advance 9 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0367), "pstate should hold 0b_11_110_111");
    cr_expect(eq(u8, pstate.unreadLen, 7), "pstate should buffer 7 bits");
    number = false;
    parse_be_bool(&number, 7, true7_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 16), "pstate should advance 7 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0167), "pstate should hold 0b_01_110_111");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, be_signed_integers)
{
    // Open memory stream for writing to dynamic buffer
    char * buffer = NULL;
    size_t size = 0;
    FILE * stream = open_memstream(&buffer, &size);
    UState ustate = {stream, 0, NULL, NULL, 0, 0};

    // Write 8 integers to the buffer
    const int8_t  int1 = -1; // a signed 1-bit integer can only be 0 or -1
    const int8_t  int7 = 7;
    const int16_t int9 = -9;
    const int16_t int15 = 15;
    const int32_t int17 = -17;
    const int32_t int31 = 31;
    const int64_t int33 = -33;
    const int64_t int63 = 63;
    unparse_be_int8(int1, 1, &ustate);
    unparse_be_int8(int7, 7, &ustate);
    unparse_be_int16(int9, 9, &ustate);
    unparse_be_int16(int15, 15, &ustate);
    unparse_be_int32(int17, 17, &ustate);
    unparse_be_int32(int31, 31, &ustate);
    unparse_be_int64(int33, 33, &ustate);
    unparse_be_int64(int63, 63, &ustate);
    fflush(stream);

    // Verify that these 8 integers were packed into 176 bits as expected
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 176), "ustate should advance 176 bits");
    cr_expect(eq(sz, size, 22), "stream should have 22 bytes");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Read 8 integers from the buffer
    int8_t  tin1 = 0;
    int8_t  tin7 = 0;
    int16_t tin9 = 0;
    int16_t tin15 = 0;
    int32_t tin17 = 0;
    int32_t tin31 = 0;
    int64_t tin33 = 0;
    int64_t tin63 = 0;
    parse_be_int8(&tin1, 1, &pstate);
    parse_be_int8(&tin7, 7, &pstate);
    parse_be_int16(&tin9, 9, &pstate);
    parse_be_int16(&tin15, 15, &pstate);
    parse_be_int32(&tin17, 17, &pstate);
    parse_be_int32(&tin31, 31, &pstate);
    parse_be_int64(&tin33, 33, &pstate);
    parse_be_int64(&tin63, 63, &pstate);
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 176), "pstate should advance 176 bits");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Verify that these 8 integers are the same integers originally written
    cr_expect(eq(i8, int1, tin1), "numbers should be the same");
    cr_expect(eq(i8, int7, tin7), "numbers should be the same");
    cr_expect(eq(i16, int9, tin9), "numbers should be the same");
    cr_expect(eq(i16, int15, tin15), "numbers should be the same");
    cr_expect(eq(i32, int17, tin17), "numbers should be the same");
    cr_expect(eq(i32, int31, tin31), "numbers should be the same");
    cr_expect(eq(i64, int33, tin33), "numbers should be the same");
    cr_expect(eq(i64, int63, tin63), "numbers should be the same");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, be_unsigned_integers)
{
    // Open memory stream for writing to dynamic buffer
    char * buffer = NULL;
    size_t size = 0;
    FILE * stream = open_memstream(&buffer, &size);
    UState ustate = {stream, 0, NULL, NULL, 0, 0};

    // Write 8 integers to the buffer
    const uint8_t  uint1 = 1; // an unsigned 1-bit integer can only be 0 or 1
    const uint8_t  uint7 = 7;
    const uint16_t uint9 = 9;
    const uint16_t uint15 = 15;
    const uint32_t uint17 = 17;
    const uint32_t uint31 = 31;
    const uint64_t uint33 = 33;
    const uint64_t uint63 = 63;
    unparse_be_uint8(uint1, 1, &ustate);
    unparse_be_uint8(uint7, 7, &ustate);
    unparse_be_uint16(uint9, 9, &ustate);
    unparse_be_uint16(uint15, 15, &ustate);
    unparse_be_uint32(uint17, 17, &ustate);
    unparse_be_uint32(uint31, 31, &ustate);
    unparse_be_uint64(uint33, 33, &ustate);
    unparse_be_uint64(uint63, 63, &ustate);
    fflush(stream);

    // Verify that these 8 integers were packed into 176 bits as expected
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 176), "ustate should advance 176 bits");
    cr_expect(eq(sz, size, 22), "stream should have 22 bytes");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Read 8 integers from the buffer
    uint8_t  tniu1 = 0;
    uint8_t  tniu7 = 0;
    uint16_t tniu9 = 0;
    uint16_t tniu15 = 0;
    uint32_t tniu17 = 0;
    uint32_t tniu31 = 0;
    uint64_t tniu33 = 0;
    uint64_t tniu63 = 0;
    parse_be_uint8(&tniu1, 1, &pstate);
    parse_be_uint8(&tniu7, 7, &pstate);
    parse_be_uint16(&tniu9, 9, &pstate);
    parse_be_uint16(&tniu15, 15, &pstate);
    parse_be_uint32(&tniu17, 17, &pstate);
    parse_be_uint32(&tniu31, 31, &pstate);
    parse_be_uint64(&tniu33, 33, &pstate);
    parse_be_uint64(&tniu63, 63, &pstate);
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 176), "pstate should advance 176 bits");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Verify that these 8 integers are the same integers originally written
    cr_expect(eq(u8, uint1, tniu1), "numbers should be the same");
    cr_expect(eq(u8, uint7, tniu7), "numbers should be the same");
    cr_expect(eq(u16, uint9, tniu9), "numbers should be the same");
    cr_expect(eq(u16, uint15, tniu15), "numbers should be the same");
    cr_expect(eq(u32, uint17, tniu17), "numbers should be the same");
    cr_expect(eq(u32, uint31, tniu31), "numbers should be the same");
    cr_expect(eq(u64, uint33, tniu33), "numbers should be the same");
    cr_expect(eq(u64, uint63, tniu63), "numbers should be the same");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, le_bool_24)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true24_rep = 076543210; // 0b_111_110_101_100_011_010_001_000
    uint32_t false_rep = 0;

    // Verify that ustate writes 10001000, 11000110, 11111010
    unparse_le_bool(true, 24, true24_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 24), "ustate should advance 24 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0), "ustate should hold nothing");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 3), "stream should hold 3 bytes");
    cr_expect(eq(u8, buffer[0], 0210), "stream should hold 0b_10_001_000");
    cr_expect(eq(u8, buffer[1], 0306), "stream should hold 0b_11_000_110");
    cr_expect(eq(u8, buffer[2], 0372), "stream should hold 0b_11_111_010");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads 11111010, 11000100, 11001000
    bool number = false;
    parse_le_bool(&number, 24, true24_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 24), "pstate should advance 24 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0), "pstate should hold nothing");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, le_bool_4_4)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true4_rep = 012; // 0b_1_010
    uint32_t false_rep = 015; // 0b_1_101

    // Verify that ustate writes 10101101
    unparse_le_bool(true, 4, true4_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 4), "ustate should advance 4 bits");
    cr_expect(eq(u8, ustate.unwritBits, 012), "ustate should hold 0b_1_010");
    cr_expect(eq(u8, ustate.unwritLen, 4), "ustate should buffer 4 bits");
    cr_expect(eq(sz, size, 0), "stream should be empty");
    unparse_le_bool(false, 4, true4_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 8), "ustate should advance 4 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0255), "ustate should hold 0b_10_101_101");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 1), "stream should have 1 byte");
    cr_expect(eq(u8, buffer[0], 0255), "stream should hold 0b_10_101_101");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads true4_rep, false_rep correctly
    bool number = false;
    parse_le_bool(&number, 4, true4_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 4), "pstate should advance 4 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0255), "pstate should hold 0b_10_101_101");
    cr_expect(eq(u8, pstate.unreadLen, 4), "pstate should buffer 4 bits");
    number = false;
    parse_le_bool(&number, 4, true4_rep, false_rep, &pstate);
    cr_expect(eq(int, number, false), "boolean number should be false");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 8), "pstate should advance 4 bits");
    cr_expect(eq(u8, pstate.unreadBits, 015), "pstate should hold 0b_1_101");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, le_bool_7_7)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true7_rep = 0146; // 0b_1_100_110
    uint32_t false_rep = 0157; // 0b_1_101_111

    // Verify that ustate writes 11001101
    unparse_le_bool(true, 7, true7_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 7), "ustate should advance 7 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0146), "ustate should hold 0b_01_100_110");
    cr_expect(eq(u8, ustate.unwritLen, 7), "ustate should buffer 7 bits");
    cr_expect(eq(sz, size, 0), "stream should be empty");
    unparse_le_bool(false, 7, true7_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 14), "ustate should advance 7 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0157), "ustate should hold 0b_01_101_111");
    cr_expect(eq(u8, ustate.unwritLen, 6), "ustate should buffer 6 bits");
    cr_expect(eq(sz, size, 1), "stream should have 1 byte");
    cr_expect(eq(u8, buffer[0], 0315), "stream should hold 0b_11_001_101");

    // Verify that flushing ustate writes 10111100
    flushUState(&ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 16), "ustate should advance 2 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0274), "ustate should hold 0b_10_111_100");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 2), "stream should have 2 bytes");
    cr_expect(eq(u8, buffer[1], 0274), "stream should hold 0b_10_111_100");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads true7_rep, false_rep correctly
    bool number = false;
    parse_le_bool(&number, 7, true7_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 7), "pstate should advance 7 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0315), "pstate should hold 0b_11_001_101");
    cr_expect(eq(u8, pstate.unreadLen, 1), "pstate should buffer 1 bit");
    number = false;
    parse_le_bool(&number, 7, true7_rep, false_rep, &pstate);
    cr_expect(eq(int, number, false), "boolean number should be false");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 14), "pstate should advance 7 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0274), "pstate should hold 0b_10_111_100");
    cr_expect(eq(u8, pstate.unreadLen, 2), "pstate should buffer 2 bits");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, le_bool_9_7)
{
    // Open memory stream for writing to dynamic buffer
    char *   buffer = NULL;
    size_t   size = 0;
    FILE *   stream = open_memstream(&buffer, &size);
    UState   ustate = {stream, 0, NULL, NULL, 0, 0};
    uint32_t true9_rep = 0747; // 0b_111_100_111
    uint32_t true7_rep = 0167; //   0b_1_110_111
    uint32_t false_rep = 0;

    // Verify that ustate writes 11100111,1 1110111
    unparse_le_bool(true, 9, true9_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 9), "ustate should advance 9 bits");
    cr_expect(eq(u8, ustate.unwritBits, 01), "ustate should hold 0b_00_000_001");
    cr_expect(eq(u8, ustate.unwritLen, 1), "ustate should buffer 1 bit");
    cr_expect(eq(sz, size, 1), "stream should hold 1 byte");
    cr_expect(eq(u8, buffer[0], 0347), "stream should hold 0b_11_100_111");
    unparse_le_bool(true, 7, true7_rep, false_rep, &ustate);
    fflush(stream);
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 16), "ustate should advance 7 bits");
    cr_expect(eq(u8, ustate.unwritBits, 0367), "ustate should hold 0b_11_110_111");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");
    cr_expect(eq(sz, size, 2), "stream should hold 2 bytes");
    cr_expect(eq(u8, buffer[1], 0367), "stream should hold 0b_11_110_111");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Verify that pstate reads true9_rep, true7_rep correctly
    bool number = false;
    parse_le_bool(&number, 9, true9_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 9), "pstate should advance 9 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0367), "pstate should hold 0b_11_110_111");
    cr_expect(eq(u8, pstate.unreadLen, 7), "pstate should buffer 7 bits");
    number = false;
    parse_le_bool(&number, 7, true7_rep, false_rep, &pstate);
    cr_expect(eq(int, number, true), "boolean number should be true");
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 16), "pstate should advance 7 bits");
    cr_expect(eq(u8, pstate.unreadBits, 0167), "pstate should hold 0b_01_110_111");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, le_signed_integers)
{
    // Open memory stream for writing to dynamic buffer
    char * buffer = NULL;
    size_t size = 0;
    FILE * stream = open_memstream(&buffer, &size);
    UState ustate = {stream, 0, NULL, NULL, 0, 0};

    // Write 8 integers to the buffer
    const int8_t  int1 = -1; // a signed 1-bit integer can only be 0 or -1
    const int8_t  int7 = 7;
    const int16_t int9 = -9;
    const int16_t int15 = 15;
    const int32_t int17 = -17;
    const int32_t int31 = 31;
    const int64_t int33 = -33;
    const int64_t int63 = 63;
    unparse_le_int8(int1, 1, &ustate);
    unparse_le_int8(int7, 7, &ustate);
    unparse_le_int16(int9, 9, &ustate);
    unparse_le_int16(int15, 15, &ustate);
    unparse_le_int32(int17, 17, &ustate);
    unparse_le_int32(int31, 31, &ustate);
    unparse_le_int64(int33, 33, &ustate);
    unparse_le_int64(int63, 63, &ustate);
    fflush(stream);

    // Verify that these 8 integers were packed into 176 bits as expected
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 176), "ustate should advance 176 bits");
    cr_expect(eq(sz, size, 22), "stream should have 22 bytes");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Read 8 integers from the buffer
    int8_t  tin1 = 0;
    int8_t  tin7 = 0;
    int16_t tin9 = 0;
    int16_t tin15 = 0;
    int32_t tin17 = 0;
    int32_t tin31 = 0;
    int64_t tin33 = 0;
    int64_t tin63 = 0;
    parse_le_int8(&tin1, 1, &pstate);
    parse_le_int8(&tin7, 7, &pstate);
    parse_le_int16(&tin9, 9, &pstate);
    parse_le_int16(&tin15, 15, &pstate);
    parse_le_int32(&tin17, 17, &pstate);
    parse_le_int32(&tin31, 31, &pstate);
    parse_le_int64(&tin33, 33, &pstate);
    parse_le_int64(&tin63, 63, &pstate);
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 176), "pstate should advance 176 bits");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Verify that these 8 integers are the same integers originally written
    cr_expect(eq(i8, int1, tin1), "numbers should be the same");
    cr_expect(eq(i8, int7, tin7), "numbers should be the same");
    cr_expect(eq(i16, int9, tin9), "numbers should be the same");
    cr_expect(eq(i16, int15, tin15), "numbers should be the same");
    cr_expect(eq(i32, int17, tin17), "numbers should be the same");
    cr_expect(eq(i32, int31, tin31), "numbers should be the same");
    cr_expect(eq(i64, int33, tin33), "numbers should be the same");
    cr_expect(eq(i64, int63, tin63), "numbers should be the same");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}

Test(bits, le_unsigned_integers)
{
    // Open memory stream for writing to dynamic buffer
    char * buffer = NULL;
    size_t size = 0;
    FILE * stream = open_memstream(&buffer, &size);
    UState ustate = {stream, 0, NULL, NULL, 0, 0};

    // Write 8 integers to the buffer
    const uint8_t  uint1 = 1; // an unsigned 1-bit integer can only be 0 or 1
    const uint8_t  uint7 = 7;
    const uint16_t uint9 = 9;
    const uint16_t uint15 = 15;
    const uint32_t uint17 = 17;
    const uint32_t uint31 = 31;
    const uint64_t uint33 = 33;
    const uint64_t uint63 = 63;
    unparse_le_uint8(uint1, 1, &ustate);
    unparse_le_uint8(uint7, 7, &ustate);
    unparse_le_uint16(uint9, 9, &ustate);
    unparse_le_uint16(uint15, 15, &ustate);
    unparse_le_uint32(uint17, 17, &ustate);
    unparse_le_uint32(uint31, 31, &ustate);
    unparse_le_uint64(uint33, 33, &ustate);
    unparse_le_uint64(uint63, 63, &ustate);
    fflush(stream);

    // Verify that these 8 integers were packed into 176 bits as expected
    cr_expect(eq(ptr, (void *)ustate.error, 0), "ustate should have no error");
    cr_expect(eq(sz, ustate.bitPos0b, 176), "ustate should advance 176 bits");
    cr_expect(eq(sz, size, 22), "stream should have 22 bytes");
    cr_expect(eq(u8, ustate.unwritLen, 0), "ustate should be empty");

    // Reopen stream for reading from same dynamic buffer
    fclose(stream);
    stream = fmemopen(buffer, size, "r");
    PState pstate = {stream, 0, NULL, NULL, 0, 0};

    // Read 8 integers from the buffer
    uint8_t  tniu1 = 0;
    uint8_t  tniu7 = 0;
    uint16_t tniu9 = 0;
    uint16_t tniu15 = 0;
    uint32_t tniu17 = 0;
    uint32_t tniu31 = 0;
    uint64_t tniu33 = 0;
    uint64_t tniu63 = 0;
    parse_le_uint8(&tniu1, 1, &pstate);
    parse_le_uint8(&tniu7, 7, &pstate);
    parse_le_uint16(&tniu9, 9, &pstate);
    parse_le_uint16(&tniu15, 15, &pstate);
    parse_le_uint32(&tniu17, 17, &pstate);
    parse_le_uint32(&tniu31, 31, &pstate);
    parse_le_uint64(&tniu33, 33, &pstate);
    parse_le_uint64(&tniu63, 63, &pstate);
    cr_expect(eq(ptr, (void *)pstate.error, 0), "pstate should have no error");
    cr_expect(eq(sz, pstate.bitPos0b, 176), "pstate should advance 176 bits");
    cr_expect(eq(u8, pstate.unreadLen, 0), "pstate should be empty");

    // Verify that these 8 integers are the same integers originally written
    cr_expect(eq(u8, uint1, tniu1), "numbers should be the same");
    cr_expect(eq(u8, uint7, tniu7), "numbers should be the same");
    cr_expect(eq(u16, uint9, tniu9), "numbers should be the same");
    cr_expect(eq(u16, uint15, tniu15), "numbers should be the same");
    cr_expect(eq(u32, uint17, tniu17), "numbers should be the same");
    cr_expect(eq(u32, uint31, tniu31), "numbers should be the same");
    cr_expect(eq(u64, uint33, tniu33), "numbers should be the same");
    cr_expect(eq(u64, uint63, tniu63), "numbers should be the same");

    // Close stream and free dynamic buffer
    fclose(stream);
    free(buffer);
}
