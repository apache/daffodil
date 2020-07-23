#include <endian.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/*
    // Handwritten translation of following DFDL schema:
    <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format representation="binary" ref="tns:GeneralFormat"/>,
    <xs:element name="r">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="e1" type="xs:int"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
*/

// PState - parser state while parsing input

typedef struct PState
{
	FILE *stream; // input to read from
} PState;

typedef struct UState
{
	FILE *stream; // output to write to
} UState;

PState *new_pstate(FILE *stream)
{
	PState *pstate = malloc(sizeof(PState));
	pstate->stream = stream;
	return pstate;
}

// GlobalQName - names of infoset elements

typedef struct GlobalQName
{
	char *name;
} GlobalQName;

GlobalQName *new_qname(char *name)
{
	GlobalQName *qname = malloc(sizeof(GlobalQName));
	qname->name = name;
	return qname;
}

// InfosetOutputter - methods to output XML infoset

void infoset_init(FILE *stream)
{
	fprintf(stream, u8"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
}

void infoset_start(FILE *stream, char *name, bool toplevel)
{
	if (toplevel)
	{
		fprintf(stream, u8"<%s>\n", name);
	}
	else
	{
		fprintf(stream, u8"  <%s>", name);
	}
}

void infoset_end(FILE *stream, char *name)
{
	fprintf(stream, u8"</%s>\n", name);
}

void infoset_prim_int(FILE *stream, int value)
{
	fprintf(stream, u8"%i", value);
}

// R - toplevel element

typedef struct R_Class R_Class;
typedef struct R
{
	int e1;
	R_Class *_class;
} R;

// R_Class - methods to parse and output R objects

typedef void (*R_Parse_Self)(R *r, PState *pstate);
typedef void (*R_Visit_Self)(R *r, FILE *stream);
typedef R *(*R_New_Instance)(R_Class *r_class_singleton);

typedef struct R_Class
{
	GlobalQName *qname;
	R_Parse_Self parseSelf;
	R_Visit_Self visitSelf;
	R_New_Instance newInstance;
} R_Class;

void r_parse_self(R *r, PState *pstate)
{
	// should read 4 bytes from pstate->stream
	// should handle insufficient number of bytes
	char buffer[4];
	int count = fread(&buffer, sizeof(buffer), 1, pstate->stream);
	if (count < sizeof(buffer)) {
		// error handling - what do we do?
		// longjmp to an error routine, push an error and print it, exit immediately?
	}
	r->e1 = be32toh(*((uint32_t*)(&buffer)));
}

void r_unparse_self(R* r, UState* ustate) {
	// should fill this buffer and then write this buffer to ustate->stream
	union {
		char c_val[4];
		uint32_t i_val;
	} buffer;
	buffer.i_val = htobe32(r->e1);
	int count = fwrite(buffer.c_val, sizeof(buffer), 1, ustate->stream);
	if (count < sizeof(buffer)) {
		// error handling goes here...
	}
}

void r_visit_self(R *r, FILE *stream)
{
	infoset_init(stream);
	infoset_start(stream, r->_class->qname->name, true);
	infoset_start(stream, "e1", false);
	infoset_prim_int(stream, r->e1);
	infoset_end(stream, "e1");
	infoset_end(stream, r->_class->qname->name);
}

R *r_new_instance(R_Class *r_class_singleton)
{
	R *r = malloc(sizeof(R));
	r->_class = r_class_singleton;
	return r;
}

R_Class *r_new_class()
{
	R_Class *r_class_singleton = malloc(sizeof(R_Class));
	r_class_singleton->qname = new_qname("r");
	r_class_singleton->parseSelf = &r_parse_self;
	r_class_singleton->visitSelf = &r_visit_self;
	r_class_singleton->newInstance = &r_new_instance;
	return r_class_singleton;
}

// Run the test

int main()
{
	PState *pstate = new_pstate(stdin);
	R_Class *r_class = r_new_class();
	R *r = r_class->newInstance(r_class);
	r_class->parseSelf(r, pstate);
	r_class->visitSelf(r, stdout);
}
