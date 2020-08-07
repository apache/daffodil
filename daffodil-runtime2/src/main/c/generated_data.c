#include <endian.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "argp_code.h"
#include "generated_data.h"
#include "xml_writer.h"

// Function prototypes to allow compilation

void C_parse_self(C *instance, PState *pstate);
void C_unparse_self(C *instance, UState *ustate);
C *C_new_instance();

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

C C_compute_ERD_offsets;

size_t C_offsets[1] = {
	(void*)&C_compute_ERD_offsets.e1 - (void*)&C_compute_ERD_offsets
};

ERD* C_childrenERDs[1] = {
	&e1ERD
};

ERD CERD =
{
	{ "C" },			// namedQName
	COMPLEX,			// typeCode
	1,					// count_children
	C_offsets,			// offsets
	C_childrenERDs,		// childrenERDs
	(Parse_Self)&C_parse_self,		// parseSelf
	(Unparse_Self)&C_unparse_self,	// unparseSelf
	(New_Instance)&C_new_instance	// newInstance
};


// Methods to parse, unparse, and create objects

void C_parse_self(C *instance, PState *pstate)
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
}

void C_unparse_self(C *instance, UState *ustate)
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
}

C *C_new_instance()
{
	C *instance = calloc(sizeof(C), 1);
	// If InfosetBase adds more members, we need to set them too
	instance->_base.erd = &CERD;
	return instance;
}

// Main entry point

int main(int argc, char *argv[])
{
  // Parse command line options
  error_t status = global_cmd(argc, argv);
  if (status == 0) {
    FILE *input = stdin;
    FILE *output = stdout;

    // Read our input from stdin or a filename argument.
    if (strcmp(parse.infile, "-")) {
      input = fopen(parse.infile, "r");
      if (input == NULL)
        {
          perror("Error opening input file: ");
          return EXIT_FAILURE;
        }
    }

    // Write our output to stdout or a filename argument.
    if (strcmp(parse.outfile, "-")) {
      output = fopen(parse.outfile, "w");
      if (output == NULL)
        {
          perror("Error opening output file: ");
          return EXIT_FAILURE;
        }
    }
    
    // Parse the input into our infoset.
    PState pstate = { input };
    XMLWriter xmlWriter = { xmlWriterMethods, output };
    C instance = { { &CERD }, 0 };
    CERD.parseSelf((InfosetBase*)&instance, &pstate);

    // Visit the infoset and print XML from it.
    InfosetBase *infoNode = &instance._base;
    xml_init(&xmlWriter);
    visit_node_self((VisitEventHandler *)&xmlWriter, infoNode);

    // Close the input if we opened it from a filename argument.
    if (input != stdin && fclose(input) != 0)
      {
        perror("Error closing input file: ");
        return EXIT_FAILURE;
      }

    // Close the output if we opened it from a filename argument.
    if (output != stdout && fclose(output) != 0)
      {
        perror("Error closing output file: ");
        return EXIT_FAILURE;
      }

    // Return success if we got this far.
    return EXIT_SUCCESS;
  }
}
