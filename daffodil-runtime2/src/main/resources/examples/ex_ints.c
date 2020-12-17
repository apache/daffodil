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

#include "ex_ints.h" // for generated code structs
#include <endian.h>         // for be32toh, htobe32, etc.
#include <stddef.h>         // for ptrdiff_t
#include <stdio.h>          // for NULL, fread, fwrite, size_t, FILE

// Prototypes needed for compilation

static void        ex_ints_initSelf(ex_ints *instance);
static const char *ex_ints_parseSelf(ex_ints *instance, const PState *pstate);
static const char *ex_ints_unparseSelf(const ex_ints *instance, const UState *ustate);

// Metadata singletons

static const ERD be_uint64_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_uint32_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_uint16_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_uint8_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_int64_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_int32_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_int16_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_int8_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_uint64_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_uint32_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_uint16_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_uint8_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_int64_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_int32_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_int16_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_int8_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ex_ints ex_ints_compute_ERD_offsets;

static const ptrdiff_t ex_ints_offsets[16] = {
    (char *)&ex_ints_compute_ERD_offsets.be_uint64 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.be_uint32 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.be_uint16 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.be_uint8 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.be_int64 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.be_int32 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.be_int16 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.be_int8 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_uint64 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_uint32 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_uint16 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_uint8 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_int64 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_int32 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_int16 - (char *)&ex_ints_compute_ERD_offsets,
    (char *)&ex_ints_compute_ERD_offsets.le_int8 - (char *)&ex_ints_compute_ERD_offsets
};

static const ERD *ex_ints_childrenERDs[16] = {
    &be_uint64_ERD,
    &be_uint32_ERD,
    &be_uint16_ERD,
    &be_uint8_ERD,
    &be_int64_ERD,
    &be_int32_ERD,
    &be_int16_ERD,
    &be_int8_ERD,
    &le_uint64_ERD,
    &le_uint32_ERD,
    &le_uint16_ERD,
    &le_uint8_ERD,
    &le_int64_ERD,
    &le_int32_ERD,
    &le_int16_ERD,
    &le_int8_ERD
};

static const ERD ex_ints_ERD = {
    {
        NULL, // namedQName.prefix
        "ex_ints", // namedQName.local
        "http://example.com", // namedQName.ns
    },
    COMPLEX,                         // typeCode
    16,                               // numChildren
    ex_ints_offsets,                      // offsets
    ex_ints_childrenERDs,                 // childrenERDs
    (ERDInitSelf)&ex_ints_initSelf,       // initSelf
    (ERDParseSelf)&ex_ints_parseSelf,     // parseSelf
    (ERDUnparseSelf)&ex_ints_unparseSelf, // unparseSelf
};

// Return a root element to be used for parsing or unparsing

InfosetBase *
rootElement()
{
    static ex_ints    instance;
    InfosetBase *root = &instance._base;
    ex_ints_ERD.initSelf(root);
    return root;
}

// Degenerate cases of endian-conversion functions called by code
// generator since <endian.h> handles only 16, 32, and 64-bit cases

static inline uint8_t htobe8(uint8_t h8b) { return h8b; }
static inline uint8_t htole8(uint8_t h8b) { return h8b; }
static inline uint8_t be8toh(uint8_t be8b) { return be8b; }
static inline uint8_t le8toh(uint8_t le8b) { return le8b; }

// Methods to initialize, parse, and unparse infoset nodes

static void
ex_ints_initSelf(ex_ints *instance)
{
    instance->be_uint64 = 0xCCCCCCCCCCCCCCCC;
    instance->be_uint32 = 0xCCCCCCCC;
    instance->be_uint16 = 0xCCCC;
    instance->be_uint8 = 0xCC;
    instance->be_int64 = 0xCCCCCCCCCCCCCCCC;
    instance->be_int32 = 0xCCCCCCCC;
    instance->be_int16 = 0xCCCC;
    instance->be_int8 = 0xCC;
    instance->le_uint64 = 0xCCCCCCCCCCCCCCCC;
    instance->le_uint32 = 0xCCCCCCCC;
    instance->le_uint16 = 0xCCCC;
    instance->le_uint8 = 0xCC;
    instance->le_int64 = 0xCCCCCCCCCCCCCCCC;
    instance->le_int32 = 0xCCCCCCCC;
    instance->le_int16 = 0xCCCC;
    instance->le_int8 = 0xCC;
    instance->_base.erd = &ex_ints_ERD;
}

static const char *
ex_ints_parseSelf(ex_ints *instance, const PState *pstate)
{
    const char *error_msg = NULL;
    if (!error_msg)
    {
        char   buffer[sizeof(uint64_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_uint64 = be64toh(*((uint64_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint32_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_uint32 = be32toh(*((uint32_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint16_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_uint16 = be16toh(*((uint16_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint8_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_uint8 = be8toh(*((uint8_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint64_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_int64 = be64toh(*((uint64_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint32_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_int32 = be32toh(*((uint32_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint16_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_int16 = be16toh(*((uint16_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint8_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->be_int8 = be8toh(*((uint8_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint64_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_uint64 = le64toh(*((uint64_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint32_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_uint32 = le32toh(*((uint32_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint16_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_uint16 = le16toh(*((uint16_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint8_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_uint8 = le8toh(*((uint8_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint64_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_int64 = le64toh(*((uint64_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint32_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_int32 = le32toh(*((uint32_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint16_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_int16 = le16toh(*((uint16_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[sizeof(uint8_t)];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->le_int8 = le8toh(*((uint8_t *)(&buffer)));
    }
    return error_msg;
}

static const char *
ex_ints_unparseSelf(const ex_ints *instance, const UState *ustate)
{
    const char *error_msg = NULL;
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint64_t)];
            uint64_t i_val;
        } buffer;
        buffer.i_val = htobe64(instance->be_uint64);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint32_t)];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->be_uint32);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint16_t)];
            uint16_t i_val;
        } buffer;
        buffer.i_val = htobe16(instance->be_uint16);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint8_t)];
            uint8_t i_val;
        } buffer;
        buffer.i_val = htobe8(instance->be_uint8);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint64_t)];
            uint64_t i_val;
        } buffer;
        buffer.i_val = htobe64(instance->be_int64);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint32_t)];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->be_int32);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint16_t)];
            uint16_t i_val;
        } buffer;
        buffer.i_val = htobe16(instance->be_int16);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint8_t)];
            uint8_t i_val;
        } buffer;
        buffer.i_val = htobe8(instance->be_int8);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint64_t)];
            uint64_t i_val;
        } buffer;
        buffer.i_val = htole64(instance->le_uint64);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint32_t)];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htole32(instance->le_uint32);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint16_t)];
            uint16_t i_val;
        } buffer;
        buffer.i_val = htole16(instance->le_uint16);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint8_t)];
            uint8_t i_val;
        } buffer;
        buffer.i_val = htole8(instance->le_uint8);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint64_t)];
            uint64_t i_val;
        } buffer;
        buffer.i_val = htole64(instance->le_int64);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint32_t)];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htole32(instance->le_int32);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint16_t)];
            uint16_t i_val;
        } buffer;
        buffer.i_val = htole16(instance->le_int16);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[sizeof(uint8_t)];
            uint8_t i_val;
        } buffer;
        buffer.i_val = htole8(instance->le_int8);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    return error_msg;
}

