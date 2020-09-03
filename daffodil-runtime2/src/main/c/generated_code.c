#include "generated_code.h"
#include <endian.h> // for be32toh, htobe32
#include <errno.h>  // for errno
#include <stddef.h> // for ptrdiff_t
#include <stdio.h>  // for NULL, fread, fwrite, size_t, feof, ferror, FILE
#include <string.h> // for strerror

// Function prototypes to allow compilation

static void        c1_init_self(c1 *instance);
static const char *c1_parse_self(c1 *instance, const PState *pstate);
static const char *c1_unparse_self(const c1 *instance, const UState *ustate);
static void        c2_init_self(c2 *instance);
static const char *c2_parse_self(c2 *instance, const PState *pstate);
static const char *c2_unparse_self(const c2 *instance, const UState *ustate);

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

static const c2 c2_compute_ERD_offsets;

static const ptrdiff_t c2_offsets[2] = {
    (char *)&c2_compute_ERD_offsets.e2 - (char *)&c2_compute_ERD_offsets,
    (char *)&c2_compute_ERD_offsets.e3 - (char *)&c2_compute_ERD_offsets,
};

static const ERD *c2_childrenERDs[2] = {
    &e2ERD,
    &e3ERD,
};

static const ERD c2ERD = {
    {"c2"},                         // namedQName
    COMPLEX,                        // typeCode
    2,                              // count_children
    c2_offsets,                     // offsets
    c2_childrenERDs,                // childrenERDs
    (Init_Self)&c2_init_self,       // initSelf
    (Parse_Self)&c2_parse_self,     // parseSelf
    (Unparse_Self)&c2_unparse_self, // unparseSelf
};

static const c1 c1_compute_ERD_offsets;

static const ptrdiff_t c1_offsets[2] = {
    (char *)&c1_compute_ERD_offsets.e1 - (char *)&c1_compute_ERD_offsets,
    (char *)&c1_compute_ERD_offsets.c2 - (char *)&c1_compute_ERD_offsets,
};

static const ERD *c1_childrenERDs[2] = {
    &e1ERD,
    &c2ERD,
};

static const ERD c1ERD = {
    {"c1"},                         // namedQName
    COMPLEX,                        // typeCode
    2,                              // count_children
    c1_offsets,                     // offsets
    c1_childrenERDs,                // childrenERDs
    (Init_Self)&c1_init_self,       // initSelf
    (Parse_Self)&c1_parse_self,     // parseSelf
    (Unparse_Self)&c1_unparse_self, // unparseSelf
};

// Return the root of an infoset to be used for parsing or unparsing

InfosetBase *
rootInfoset()
{
    static c1    instance;
    InfosetBase *root = &instance._base;
    c1ERD.initSelf(root);
    return root;
}

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
c1_init_self(c1 *instance)
{
    // If InfosetBase adds more members, we need to set them too
    instance->_base.erd = &c1ERD;
    c2_init_self(&instance->c2);
}

static const char *
c1_parse_self(c1 *instance, const PState *pstate)
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
        error_msg = c2_parse_self(&instance->c2, pstate);
    }
    return error_msg;
}

static const char *
c1_unparse_self(const c1 *instance, const UState *ustate)
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
        error_msg = c2_unparse_self(&instance->c2, ustate);
    }
    return error_msg;
}

static void
c2_init_self(c2 *instance)
{
    // If InfosetBase adds more members, we need to set them too
    instance->_base.erd = &c2ERD;
}

static const char *
c2_parse_self(c2 *instance, const PState *pstate)
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
c2_unparse_self(const c2 *instance, const UState *ustate)
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
