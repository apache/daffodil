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

// ElementRuntimeData - data used to parse/unparse objects

typedef struct ElementRuntimeData ERD;

typedef struct InfosetBase
{
	ERD *erd;
} InfosetBase;

typedef void (*Parse_Self)(ERD *erd, PState *pstate);
typedef void (*Unparse_Self)(InfosetBase *infoNode, UState *ustate);
typedef ERD *(*New_Instance)(ERD *erd);

enum TypeCode {
	COMPLEX,
	PRIMITIVE_INT
};

typedef struct ElementRuntimeData
{
	GlobalQName *namedQName;
	enum TypeCode typeCode;
	uint32_t count_children;
	size_t *offsets;
	ERD *childrenERD;

	Parse_Self parseSelf;
	Unparse_Self unparseSelf;
	New_Instance newInstance;
} ERD;

typedef void (*VisitInit)();
typedef void (*VisitStart)();
typedef void (*VisitEnd)();
typedef void (*VisitInt)();

typedef struct VisitEventHandler {
	VisitInit *visitInit;
	VisitStart *visitStart;
	VisitEnd *visitEnd;
	VisitInt *visitInt;
} VisitEventHandler;

// InfosetOutputter - methods to output XML infoset
VisitEventHandler xmlWriter;
typedef struct XMLWriter {
	VisitEventHandler handler;
	FILE *stream;
} XMLWriter;

void xml_init(XMLWriter *writer)
{
	fprintf(writer->stream, u8"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
}

void xml_start(XMLWriter *writer, ERD *erd, bool toplevel)
{
	if (toplevel)
	{
		fprintf(writer->stream, u8"<%s>\n", erd->namedQName->name);
	}
	else
	{
		fprintf(writer->stream, u8"  <%s>", erd->namedQName->name);
	}
}

void xml_end(XMLWriter *writer, ERD *erd)
{
	fprintf(writer->stream, u8"</%s>\n", erd->namedQName->name);
}

void xml_int(XMLWriter *writer, ERD *erd, int *location)
{
	int value = *location;
	fprintf(writer->stream, u8"%i", value);
}

// R - toplevel element

typedef struct R
{
	InfosetBase _base;
	int e1;
} R;

// R_Class - methods to parse and output R objects

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

void visit_node_self(VisitEventHandler *handler, InfosetBase *infoNode)
{
	visit_start(handler, infoNode->erd, true);
	// Iterate through children...
	ERD* childERD = infoNode->erd->childrenERD;
	size_t *p_offset = infoNode->erd->offsets;
	for (int i = 0; i = infoNode->erd->count_children; i++) {
		// We also need to dereference childERD and offset 
		// using [i] in the loop code below.. or perhaps
		// increment them, yes, we do increment the pointers
		size_t offset = *p_offset;
		visit_start(stream, childERD, false);
		// switch on the type...
		switch (childERD->typeCode) {
			case COMPLEX: {
				// get the childNode (how?)
				InfosetBase* childNode;
				childNode = (InfosetBase*)((char*)infoNode + offset);;
				childERD->visitSelf(childNode, stream);
				break;
			}
			case PRIMITIVE_INT: {
				int* childLocation;
				childLocation = (int*)((char*)infoNode + offset);
				visit_int(childERD, childLocation, stream);
				break;
			}
		}
		visit_end(stream, childERD);
		p_offset++;
		childERD++;
	}
	visit_end(stream, infoNode->erd);
}

R *r_new_instance(ERD *erd)
{
	R *r = malloc(sizeof(R));
	// If InfosetBase adds more members, we need to set them too
	r->_base.erd = erd;
	return r;
}

size_t *r_new_offsets() {
	// One offset - the position of the e1 member
	size_t *offsets = calloc(sizeof(size_t), 1);
	// Allocate one R object on stack, get address of e1 member, and subtract 
	// R object's address
	R r;
	void *base = &r;
	void *e1 = &r.e1;
	offsets[0] = e1 - base;
	return offsets;
}

ERD *r_new_children_erd() {
	// One ERD - the qname of the e1 member
	ERD *childrenERD = e1_new_erd();
	return childrenERD;
}

ERD *r_new_erd()
{
	ERD *erd = malloc(sizeof(ERD));
	erd->namedQName = new_qname("r");
	erd->count_children = 1;
	erd->offsets = r_new_offsets();
	erd->childrenERD = r_new_children_erd(); // need to generate each such function
	erd->parseSelf = &r_parse_self;
	erd->unparseSelf = &r_unparse_self;
	erd->newInstance = &r_new_instance;
	return erd;
}

ERD *e1_new_erd()
{
	ERD *erd = calloc(sizeof(ERD), 1);
	erd->namedQName = new_qname("e1");
	erd->typeCode = PRIMITIVE_INT;
	return erd;
}

// Run the test

int main()
{
	PState *pstate = new_pstate(stdin);
	VisitEventHandler xmlWriterMethods = { &xml_start, &xml_end, &xml_int };
	XMLWriter xmlWriter = { xmlWriterMethods, stdout };
	ERD *r_erd = r_new_erd();
	R *r = r_erd->newInstance(r_erd);
	r_erd->parseSelf(r, pstate);
	InfosetBase infoNode = r->_base;
	visit_node_self((VisitEventHandler*)&xmlWriter, &infoNode);
}
