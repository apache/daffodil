#include <endian.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "argp_code.h"
#include "generated_data.h"
#include "xml_writer.h"

// Function prototypes to allow compilation

void C1_parse_self(C1 *instance, PState *pstate);
void C1_unparse_self(C1 *instance, UState *ustate);
void C1_init_self(C1 *instance);
void C2_parse_self(C2 *instance, PState *pstate);
void C2_unparse_self(C2 *instance, UState *ustate);
void C2_init_self(C2 *instance);

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

ERD e2ERD =
{
	{ "e2" }, 			// namedQName
	PRIMITIVE_INT,		// typeCode
	0,					// count_children
	NULL,				// offsets
	NULL,				// childrenERDs
	NULL,				// parseSelf
	NULL,				// unparseSelf
	NULL				// newInstance
};

ERD e3ERD =
{
	{ "e3" }, 			// namedQName
	PRIMITIVE_INT,		// typeCode
	0,					// count_children
	NULL,				// offsets
	NULL,				// childrenERDs
	NULL,				// parseSelf
	NULL,				// unparseSelf
	NULL				// newInstance
};

C2 C2_compute_ERD_offsets;

size_t C2_offsets[2] = {
	(void*)&C2_compute_ERD_offsets.e2 - (void*)&C2_compute_ERD_offsets,
	(void*)&C2_compute_ERD_offsets.e3 - (void*)&C2_compute_ERD_offsets,
};

ERD* C2_childrenERDs[2] = {
	&e2ERD,
	&e3ERD,
};

ERD C2ERD =
{
	{ "C2" },			// namedQName
	COMPLEX,			// typeCode
	2,					// count_children
	C2_offsets,			// offsets
	C2_childrenERDs,		// childrenERDs
	(Parse_Self)&C2_parse_self,		// parseSelf
	(Unparse_Self)&C2_unparse_self,	// unparseSelf
	(Init_Self)&C2_init_self,	// initSelf
};

C1 C1_compute_ERD_offsets;

size_t C1_offsets[2] = {
	(void*)&C1_compute_ERD_offsets.e1 - (void*)&C1_compute_ERD_offsets,
	(void*)&C1_compute_ERD_offsets.c2 - (void*)&C1_compute_ERD_offsets,
};

ERD* C1_childrenERDs[2] = {
	&e1ERD,
	&C2ERD,
};

ERD C1ERD =
{
	{ "C1" },			// namedQName
	COMPLEX,			// typeCode
	2,					// count_children
	C1_offsets,			// offsets
	C1_childrenERDs,		// childrenERDs
	(Parse_Self)&C1_parse_self,		// parseSelf
	(Unparse_Self)&C1_unparse_self,	// unparseSelf
	(Init_Self)&C1_init_self,	// initSelf
};


// Methods to parse, unparse, and create objects

void C1_parse_self(C1 *instance, PState *pstate)
{
	{
		// Read 4 bytes from pstate->stream
		// should handle insufficient number of bytes
		char buffer[4];
		int count = fread(&buffer, sizeof(buffer), 1, pstate->stream);
		if (count < sizeof(buffer))
		{
			// error handling - what do we do?
			// longjmp to an error routine, push an error and print it, exit immediately?
		}
		instance->e1 = be32toh(*((uint32_t *)(&buffer)));
	}
	C2_parse_self(&instance->c2, pstate);
}

void C1_unparse_self(C1 *instance, UState *ustate)
{
	{
		// Fill 4-byte buffer and write it to ustate->stream
		union {
			char c_val[4];
			uint32_t i_val;
		} buffer;
		buffer.i_val = htobe32(instance->e1);
		int count = fwrite(buffer.c_val, sizeof(buffer), 1, ustate->stream);
		if (count < sizeof(buffer))
		{
			// error handling goes here...
		}
	}
	C2_unparse_self(&instance->c2, ustate);
}

void C1_init_self(C1 *instance)
{
	// If InfosetBase adds more members, we need to set them too
	instance->_base.erd = &C1ERD;
	C2_init_self(&instance->c2);
}

void C2_parse_self(C2 *instance, PState *pstate)
{
	{
		// Read 4 bytes from pstate->stream
		// should handle insufficient number of bytes
		char buffer[4];
		int count = fread(&buffer, sizeof(buffer), 1, pstate->stream);
		if (count < sizeof(buffer))
		{
			// error handling - what do we do?
			// longjmp to an error routine, push an error and print it, exit immediately?
		}
		instance->e2 = be32toh(*((uint32_t *)(&buffer)));
	}
	{
		// Read 4 bytes from pstate->stream
		// should handle insufficient number of bytes
		char buffer[4];
		int count = fread(&buffer, sizeof(buffer), 1, pstate->stream);
		if (count < sizeof(buffer))
		{
			// error handling - what do we do?
			// longjmp to an error routine, push an error and print it, exit immediately?
		}
		instance->e3 = be32toh(*((uint32_t *)(&buffer)));
	}
}

void C2_unparse_self(C2 *instance, UState *ustate)
{
	{
		// Fill 4-byte buffer and write it to ustate->stream
		union {
			char c_val[4];
			uint32_t i_val;
		} buffer;
		buffer.i_val = htobe32(instance->e2);
		int count = fwrite(buffer.c_val, sizeof(buffer), 1, ustate->stream);
		if (count < sizeof(buffer))
		{
			// error handling goes here...
		}
	}
	{
		// Fill 4-byte buffer and write it to ustate->stream
		union {
			char c_val[4];
			uint32_t i_val;
		} buffer;
		buffer.i_val = htobe32(instance->e3);
		int count = fwrite(buffer.c_val, sizeof(buffer), 1, ustate->stream);
		if (count < sizeof(buffer))
		{
			// error handling goes here...
		}
	}
}

void C2_init_self(C2 *instance)
{
	// If InfosetBase adds more members, we need to set them too
	instance->_base.erd = &C2ERD;
}

// Open a file or exit if it can't be opened

static FILE *fopen_or_exit(FILE *stream, char *pathname, char *mode) {
  if (strcmp(pathname, "-") != 0) {
    stream = fopen(pathname, mode);
    if (stream == NULL) {
      perror("Error opening file: ");
      exit(EXIT_FAILURE);
    }
  }
  return stream;
}

// Close a stream or exit if it can't be closed

static void fclose_or_exit(FILE *stream, FILE *stdin_stdout) {
  if (stream != stdin_stdout && fclose(stream) != 0) {
    perror("Error closing file: ");
    exit(EXIT_FAILURE);
  }
}

// Our main entry point

int main(int argc, char *argv[]) {
  // Parse command line options
  error_t status = parse_cli(argc, argv);

  if (status == 0) {
    FILE *input = stdin;
    FILE *output = stdout;

    if (global.subcommand == PARSE) {
      // Open our input and output files if given as arguments.
      input = fopen_or_exit(input, parse.infile, "r");
      output = fopen_or_exit(output, parse.outfile, "w");

      // Parse the input into our infoset.
      PState pstate = {input};
      XMLWriter xmlWriter = {xmlWriterMethods, output};
      C1 instance;
	  C1ERD.initSelf((InfosetBase *)&instance);
      C1ERD.parseSelf((InfosetBase *)&instance, &pstate);

      // Visit the infoset and print XML from it.
      InfosetBase *infoNode = &instance._base;
      xml_init(&xmlWriter);
      visit_node_self((VisitEventHandler *)&xmlWriter, infoNode);
    } else {
      // ??? - We can unparse, but we can't read XML yet
      printf("unparse: can't read XML infile yet\n");
    }

    // Close our input and out files if we opened them.
    fclose_or_exit(input, stdin);
    fclose_or_exit(output, stdout);
  }

  return status;
}
