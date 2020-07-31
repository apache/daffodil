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

ERD e1ERD = 
{
	{ "e1" }, 			// namedQName
	PRIMITIVE_INT,		// typeCode
	0,					// count_children
	NULL,				// offsets
	NULL,				// childrenERDs
	NULL,				// parseSelf
	NULL,				// unparseSelf
	NULL				// newInstance
};

R r_compute_ERD_offsets;

size_t r_offsets[1] = {
	(void*)&r_compute_ERD_offsets.e1 - (void*)&r_compute_ERD_offsets
};

ERD* r_childrenERDs[1] = {
	&e1ERD
};

ERD rERD =
{
	{ "r" },			// namedQName
	COMPLEX,			// typeCode
	1,					// count_children
	r_offsets,			// offsets
	r_childrenERDs,		// childrenERDs
	(Parse_Self)&r_parse_self,		// parseSelf
	(Unparse_Self)&r_unparse_self,	// unparseSelf
	(New_Instance)&r_new_instance	// newInstance
};

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
	R *r = calloc(sizeof(R), 1);
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
	PState pstate = { stream };
	XMLWriter xmlWriter = { xmlWriterMethods, stdout };
	R r = { { &rERD }, 0 };
	rERD.parseSelf((InfosetBase*)&r, &pstate);

	// Close the input stream if we opened it from a filename argument.
	if (stream != stdin && fclose(stream) != 0)
	{
		perror("Error closing file: ");
		return EXIT_FAILURE;
	}

	// Visit the infoset and print XML from it.
	InfosetBase *infoNode = &r._base;
	xml_init(&xmlWriter);
	visit_node_self((VisitEventHandler *)&xmlWriter, infoNode);

	// Return success if we got this far.
	return EXIT_SUCCESS;
}
