#ifndef VM_H_
#define VM_H_

#include <stdio.h>

enum OPCode {
	OP_ENTER,
	OP_LEAVE,

	OP_CONST,

	OP_LOAD_W4,	// Load 4 bytes from working_stack
	OP_STOR_W4,	// Store 4 bytes to working_stack

	OP_LOAD_P4, // Load 4 bytes from a pointer
	OP_STOR_P4, // Store 4 bytes to a pointer

	OP_LEA_W,	// Load the effective address for the location on the working_stack

	OP_JMP,		// jmp
	OP_JZ,		// jmp if zero
	OP_JNZ, 	// jmp if not zero

	OP_CALL,	// call function
	OP_EQ,		// equal
	OP_NEQ,		// not equal

	OP_MULI,
	OP_DIVI,
	OP_ADDI,
	OP_SUBI,

	OP_DUP,		// duplicate the top item on the stack -- can be used to store a result for use again
	OP_SWAP, 	// swap top item in stack with the Nth back.
};


/**
 * Functions and Code Segments...
 *
 * Part of a function definition is a reference to its segment. A segment is a loadable binary collection of data and code.
 * All ebmeded offsets are to locations within the contained segment. When a function is invoked in a different segment, we must
 * switch to the new segment before execution can continue.
 *
 *
 */


#define VM_NATIVE (1<<31)

typedef void (*native_function)( int* args );

#define VM_VERIFY

/**
 * Operands are pushed/poped off the stack, all computation instructions work on operands.
 *
 * Each "module" has its own code/data segment. Local code/data addresses are detected by
 * masking the pointer, as there should no be "real" pointers under 1 meg.
 *
 * Locals and Argument space is pre-allocated from working stack
 * the BSP register always points to the top of the args / bottom of the locals
 *
 * Any space used to pass parameters to other functions allocated as part of the locals (at the end)
 *
 * Return values are stored in arg0, technically passes as a hidden first parameter.
 *
 * TODO: sp validation for Enter/Leave
 */

class vm_context {

#define WHEAD sizeof(working_stack)/sizeof(working_stack[0])-1

public:
	static int executed;

	int execute( int* code, int* args )
	{
		int operand_stack[256];
		int working_stack[1024];
		int* psp;	// Parameter stack pointer (pointer to the first parameter passed to current function)
		int* bsp; 	// Base pointer, pointer to bottom of args/locals (working stack on entry)
		int* wsp;   // Top of the working stack (where locals are allocated from)
		int* sp;	// Stack pointer, top of byte code operands
		int* cp;	// Code pointer, next instruction to execute
		int v0,v1;  // volatile registers, for scratch use in the current instruction

		// mark the stacks to detech overrun
		operand_stack[0] = 0xDEAD;
		working_stack[WHEAD] = 0xDEAD;

		// setup registers
		cp = code;
		sp = operand_stack + 1;
		wsp = working_stack + WHEAD;
		psp = args;

#define PROLOGUE_SIZE 3

		*--wsp = 0;	 // saved base pointer
		*--wsp = 0;	 // saved parameter pointer
		*--wsp = -1; // return address

		while( 1 )
		{
			// Read registers
			// r0 -- result of last operation / top of stack
			// r1 -- top of stack - 1

			// Write registers
			// s0 -- top of stack (for storing)
			// s1 -- top of stack - 1 (for storing)

			// Code registers (read)
			// c0 -- 4 byte arg to opcode

			// Volatile registers (only valid for the current instructions use)
			// v0
			// v1

			// psp -- Parameter stacl pointer (first param passed)
			// bsp -- Base pointer to bottom of args/locals
			// wsp -- Working stack (local allocations)
			// sp  -- Stack pinter
			// cp  -- Code Pointer

			int op, r0, r1;
#define s0	sp[0]
#define s1	sp[-1]
#define c0	cp[0]

#define NEXT 		goto next;
#define NEXT_0		goto next_optimized_r0;
#define NEXT_0_1	goto next_optimized;

		next:
			r0 = sp[0];

		next_optimized_r0:
			r1 = sp[-1];

		next_optimized:
			op = *cp++;
			executed++;
			switch( (OPCode)op )
			{
			default:
				printf("error");
				goto next;

			case OP_ENTER:
				v0 = c0;	// get my stack size

				bsp = wsp;	// initialize the bsp
				// TODO Figure out how to initialize this correctly for the first OP_ENTER (native->vm call)
				if( cp == code + 1 )
					; // native -> vm call
				else
					psp = bsp+PROLOGUE_SIZE;

				wsp -= v0;	// allocate my local space
                    
                #ifdef VM_VERIFY
                // store off the SP so it can be validated on LEAVE.
                *--wsp = (int)sp;
                #endif
                    
				cp++;
                    
				NEXT_0_1;

			case OP_LEAVE:
				v0 = c0;	// get my stack size (including return)
				v1 = r0;    // store off the return value

                #ifdef VM_VERIFY
                if( *wsp != (int)sp ) {
                    printf( "Stack pointer corrupted\n" );
                }
                #endif
                    
				// restore the old bsp/cp
				wsp = bsp;
				cp = (int*)*wsp++;
				psp = (int*)*wsp++;
				bsp = (int*)*wsp++;

				// top level entry
				if( cp == (void*)-1 )
					goto done;

				NEXT_0_1;
			case OP_CONST:
				sp++;
				r1 = r0;
				r0 = c0;
				s0 = r0;

				cp++;
				NEXT_0_1;
			case OP_LOAD_W4:
				v0 = c0; // offset
				sp++;
				r1 = r0;
				if( v0 < 0 )
					r0 = *(bsp + v0);
				else
					r0 = *(psp + v0);

				s0 = r0;

				cp++;
				NEXT_0_1;
			case OP_STOR_W4:
				v0 = c0; // offset
				sp--;
				if( c0 < 0 )
					*(bsp + v0) = r0;
				else
					*(psp + v0) = r0;

				cp++;

				r0 = r1;
				NEXT_0;

			case OP_LOAD_P4:
				v0 = c0; // offset
				r0 = *(((int*)r0) + v0);
				s0 = r0;
				cp++;
				NEXT_0_1;
			case OP_STOR_P4:
				v0 = c0; // offset
                sp--;
                sp--;
				*(((int*)r0) + v0) = r1;

				cp++;

				r0 = r1;

				NEXT_0;
			case OP_LEA_W:
				v0 = c0; // offset
				sp++;
				r1 = r0;
				if( v0 < 0 )
					r0 = (int)(bsp + v0);
				else
					r0 = (int)(psp + v0);

				s0 = r0;
				cp++;

				NEXT_0_1;

			case OP_JMP:
				v0 = c0; // offset

				cp = cp + v0 + 1;

				NEXT_0_1;
			case OP_JZ:
				sp--;
				if( r0 != 0 )
				{
					cp++;
					goto next; // should this be r0=r1; NEXT_0; ?
				}

				v0 = c0; // offset

				cp = cp + v0 + 1;

				r0 = r1;
				NEXT_0;
			case OP_JNZ:
				sp--;
				if( r0 == 0 )
				{
					cp++;
					goto next; // should this be r0=r1; NEXT_0; ?
				}

				v0 = c0; // offset

				cp = cp + v0 + 1;

				r0 = r1;
				NEXT_0;
			case OP_CALL:
				v0 = c0;	// get the address to call
				cp++;

				*--wsp = (int)bsp;
				*--wsp = (int)psp;
				*--wsp = (int)cp;

				if( v0 & VM_NATIVE )
				{
					// save off the WSP pointer to the context (for recursive calls)
					// this->wsp = wsp;
					native_function f = (native_function)(v0 & ~VM_NATIVE);
					f( wsp+PROLOGUE_SIZE );

					// restore the wsp (nomally done via the OP_LEAVE)
					wsp+=PROLOGUE_SIZE;
				}
				else
					// update cp
					// TODO: Validate code pointer? load dataseg?
					cp = (int*)v0;//code + v0;

				NEXT_0_1; // is this really correct?
			case OP_EQ:
                sp--;
                r0 = (r1 == r0);
                s0 = r0;
				NEXT_0;
			case OP_NEQ:
                sp--;
                r0 = (r1 != r0);
                s0 = r0;
                NEXT_0;
			case OP_MULI:
                sp--;
                r0 = (r1 * r0);
                s0 = r0;
                NEXT_0;
            case OP_DIVI:
                sp--;
                r0 = (r1 / r0);
                s0 = r0;
                NEXT_0;
			case OP_ADDI:
                sp--;
                r0 = (r1 + r0);
                s0 = r0;
				NEXT_0;
			case OP_SUBI:
                sp--;
                r0 = (r1 - r0);
				s0 = r0;
				NEXT_0;
			case OP_DUP:
				sp++;
				r1 = r0;
				s0 = r0;
				NEXT_0_1;
			case OP_SWAP:
				v0 = c0; // offset
				cp++;
				v1 = r0;
				r0 = sp[-v0];
				sp[-v0]=v1;
                s0 = r0;
				NEXT_0;
			}
		}

	done:

		#ifdef VM_VERIFY
		if( sp != operand_stack+1 || operand_stack[0] != 0xDEAD )
			printf( "Operand Stack corrupted\n" );
		if( wsp != working_stack + WHEAD || working_stack[WHEAD] != 0xDEAD )
			printf( "Working Stack corrupted\n" );
		#endif
		return s0;
	}
};


struct vm_symbol {
	char* symbol;	// Name of the symbol
	int   offset;	// ptr to the global var in dseg.
};

class vm_module {
	int header;

	vm_symbol* symbols; // location of all symbols. (both import and export)
	int* dseg;			// data seg, all class info,
	int* cseg;
};

class vm_engine {
};

#endif /*VM_H_*/
