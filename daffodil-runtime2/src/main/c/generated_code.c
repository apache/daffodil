#include "generated_code.h"
#include <endian.h> // for be32toh, htobe32
#include <errno.h>  // for errno
#include <stddef.h> // for ptrdiff_t
#include <stdio.h>  // for NULL, fread, fwrite, size_t, feof, ferror, FILE
#include <string.h> // for strerror

// Function prototypes to allow compilation

static void        C1_init_self(C1 *instance);
static const char *C1_parse_self(C1 *instance, const PState *pstate);
static const char *C1_unparse_self(const C1 *instance, const UState *ustate);
static void        C2_init_self(C2 *instance);
static const char *C2_parse_self(C2 *instance, const PState *pstate);
static const char *C2_unparse_self(const C2 *instance, const UState *ustate);

// Metadata singletons

static const ERD e1ERD = {
    {"e1"},        // namedQName
    PRIMITIVE_INT, // typeCode
    0,             // count_children
    NULL,          // offsets
    NULL,          // childrenERDs
    NULL,          // initSelf
    NULL,          // parseSelf
    NULL,          // unparseSelf
};

static const ERD e2ERD = {
    {"e2"},        // namedQName
    PRIMITIVE_INT, // typeCode
    0,             // count_children
    NULL,          // offsets
    NULL,          // childrenERDs
    NULL,          // initSelf
    NULL,          // parseSelf
    NULL,          // unparseSelf
};

static const ERD e3ERD = {
    {"e3"},        // namedQName
    PRIMITIVE_INT, // typeCode
    0,             // count_children
    NULL,          // offsets
    NULL,          // childrenERDs
    NULL,          // initSelf
    NULL,          // parseSelf
    NULL,          // unparseSelf
};

static const C2 C2_compute_ERD_offsets;

static const ptrdiff_t C2_offsets[2] = {
    (char *)&C2_compute_ERD_offsets.e2 - (char *)&C2_compute_ERD_offsets,
    (char *)&C2_compute_ERD_offsets.e3 - (char *)&C2_compute_ERD_offsets,
};

static const ERD *C2_childrenERDs[2] = {
    &e2ERD,
    &e3ERD,
};

static const ERD C2ERD = {
    {"C2"},                         // namedQName
    COMPLEX,                        // typeCode
    2,                              // count_children
    C2_offsets,                     // offsets
    C2_childrenERDs,                // childrenERDs
    (Init_Self)&C2_init_self,       // initSelf
    (Parse_Self)&C2_parse_self,     // parseSelf
    (Unparse_Self)&C2_unparse_self, // unparseSelf
};

static const C1 C1_compute_ERD_offsets;

static const ptrdiff_t C1_offsets[2] = {
    (char *)&C1_compute_ERD_offsets.e1 - (char *)&C1_compute_ERD_offsets,
    (char *)&C1_compute_ERD_offsets.c2 - (char *)&C1_compute_ERD_offsets,
};

static const ERD *C1_childrenERDs[2] = {
    &e1ERD,
    &C2ERD,
};

static const ERD C1ERD = {
    {"C1"},                         // namedQName
    COMPLEX,                        // typeCode
    2,                              // count_children
    C1_offsets,                     // offsets
    C1_childrenERDs,                // childrenERDs
    (Init_Self)&C1_init_self,       // initSelf
    (Parse_Self)&C1_parse_self,     // parseSelf
    (Unparse_Self)&C1_unparse_self, // unparseSelf
};

// Methods to initialize, parse, and unparse infoset nodes

static const char *
eof_or_error_msg(FILE *stream)
{
    if (feof(stream))
    {
        static const char *error_msg = "Got EOF while expecting more input";
        return error_msg;
    }
    else if (ferror(stream))
    {
        return strerror(errno);
    }
    else
    {
        return NULL;
    }
}

static void
C1_init_self(C1 *instance)
{
    // If InfosetBase adds more members, we need to set them too
    instance->_base.erd = &C1ERD;
    C2_init_self(&instance->c2);
}

static const char *
C1_parse_self(C1 *instance, const PState *pstate)
{
    const char *error_msg = NULL;
    if (error_msg == NULL)
    {
        char   buffer[4];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->e1 = be32toh(*((uint32_t *)(&buffer)));
    }
    if (error_msg == NULL)
    {
        error_msg = C2_parse_self(&instance->c2, pstate);
    }
    return error_msg;
}

static const char *
C1_unparse_self(const C1 *instance, const UState *ustate)
{
    const char *error_msg = NULL;
    if (error_msg == NULL)
    {
        union
        {
            char     c_val[4];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->e1);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (error_msg == NULL)
    {
        error_msg = C2_unparse_self(&instance->c2, ustate);
    }
    return error_msg;
}

static void
C2_init_self(C2 *instance)
{
    // If InfosetBase adds more members, we need to set them too
    instance->_base.erd = &C2ERD;
}

static const char *
C2_parse_self(C2 *instance, const PState *pstate)
{
    const char *error_msg = NULL;
    if (error_msg == NULL)
    {
        char   buffer[4];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->e2 = be32toh(*((uint32_t *)(&buffer)));
    }
    if (error_msg == NULL)
    {
        char   buffer[4];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->e3 = be32toh(*((uint32_t *)(&buffer)));
    }
    return error_msg;
}

static const char *
C2_unparse_self(const C2 *instance, const UState *ustate)
{
    const char *error_msg = NULL;
    if (error_msg == NULL)
    {
        union
        {
            char     c_val[4];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->e2);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (error_msg == NULL)
    {
        union
        {
            char     c_val[4];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->e3);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    return error_msg;
}

// Return the root of an infoset to be used for parsing or unparsing

InfosetBase *
rootInfoset()
{
    static C1    instance;
    InfosetBase *root = &instance._base;
    C1ERD.initSelf(root);
    return root;
}
