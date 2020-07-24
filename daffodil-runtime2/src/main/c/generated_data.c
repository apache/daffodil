#include <endian.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "generated_data.h"
#include "xml_writer.h"

// Function prototypes to allow compilation

size_t *r_new_offsets();
ERD *r_new_children_erd();
void r_parse_self(R *r, PState *pstate);
void r_unparse_self(R *r, UState *ustate);
R *r_new_instance();

// Metadata singletons

ERD e1ERD;
ERD rERD;

void fillE1ERD()
{
	e1ERD.namedQName = new_qname("e1");
	e1ERD.typeCode = PRIMITIVE_INT;
}

void fillRERD()
{
	rERD.namedQName = new_qname("r");
	rERD.count_children = 1;
	rERD.offsets = r_new_offsets();
	rERD.childrenERD = r_new_children_erd(); // need to generate each such function
	rERD.parseSelf = (Parse_Self)&r_parse_self;
	rERD.unparseSelf = (Unparse_Self)&r_unparse_self;
	rERD.newInstance = (New_Instance)&r_new_instance;
}

// R - methods to construct R and metadata objects

size_t *r_new_offsets()
{
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

ERD *r_new_children_erd()
{
	// One ERD - the qname of the e1 member
	ERD *childrenERD = &e1ERD;
	return childrenERD;
}

// R - methods to parse, unparse, and create R objects

void r_parse_self(R *r, PState *pstate)
{
	// should read 4 bytes from pstate->stream
	// should handle insufficient number of bytes
	char buffer[4];
	int count = fread(&buffer, sizeof(buffer), 1, pstate->stream);
	if (count < sizeof(buffer))
	{
		// error handling - what do we do?
		// longjmp to an error routine, push an error and print it, exit immediately?
	}
	r->e1 = be32toh(*((uint32_t *)(&buffer)));
}

void r_unparse_self(R *r, UState *ustate)
{
	// should fill this buffer and then write this buffer to ustate->stream
	union {
		char c_val[4];
		uint32_t i_val;
	} buffer;
	buffer.i_val = htobe32(r->e1);
	int count = fwrite(buffer.c_val, sizeof(buffer), 1, ustate->stream);
	if (count < sizeof(buffer))
	{
		// error handling goes here...
	}
}

R *r_new_instance()
{
	R *r = malloc(sizeof(R));
	// If InfosetBase adds more members, we need to set them too
	r->_base.erd = &rERD;
	return r;
}

// Run the test

int main(int argc, char *argv[])
{
	// Read our input from stdin or a filename argument.
	FILE *stream = stdin;
	if (argc == 2)
	{
		stream = fopen(argv[1], "r");
		if (stream == NULL)
		{
			perror("Error opening file: ");
			return EXIT_FAILURE;
		}
	}

	// Parse the input stream into our infoset.
	fillE1ERD();
	fillRERD();
	PState *pstate = new_pstate(stream);
	XMLWriter xmlWriter = {xmlWriterMethods, stdout};
	R *r = (R *)rERD.newInstance();
	r_parse_self(r, pstate);

	// Close the input stream if we opened it from a filename argument.
	if (stream != stdin && fclose(stream) != 0)
	{
		perror("Error closing file: ");
		return EXIT_FAILURE;
	}

	// Visit the infoset and print XML from it.
	InfosetBase *infoNode = &r->_base;
	xml_init(&xmlWriter);
	visit_node_self((VisitEventHandler *)&xmlWriter, infoNode);

	// Return success if we got this far.
	return EXIT_SUCCESS;
}
