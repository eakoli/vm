#include "vm.h"
#include <time.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include <stdarg.h>

int vm_context::executed = 0;

int context_switches = 0;

	// REGISTERS :
	// 0 - EAX
	// 3 - EBX
	// 1 - ECX
	// 2 - EDX
	// 6 - ESI
	// 7 - EDI
	// 5 - EBP
	// 4 - ESP

	// [mod]x2 [reg]x3 [reg/mem]x3
	//
	//
	// /n == [11] [n] [reg]
	//	     [00] [n] [mem] -- ie [edx], [edi]
	//
	// /r == [11] [reg] [reg]
	// 		 [00] [reg] [mem]

#define MRM_REG 0xC0
#define MRM_MEM 0

#define OFS( r ) (0xC0|r)

#define ModRM_digit_reg( digit, reg ) (MRM_REG | (digit<<3) | reg)
#define ModRM_digit_mem( digit, mem ) (MRM_MEM | (digit<<3) | mem)


#define ModRM_reg_reg( reg_s, reg_d ) ( MRM_REG | (reg_s<<3) | reg_d )
#define ModRM_reg_mem( reg_s, reg_d ) ( MRM_MEM | (reg_s<<3) | reg_d )

#define ModRM_r( reg_s, reg_d ) ((((reg_s)&0x7)<<3) | ((reg_d)&0x7) | ((~reg_d) & 0xC0))
#define ModRM_d( digit, dest ) ((digit<<3) | ((dest)&0x7) | ((~(dest)) & 0xC0) )

#define R_EAX	0
#define R_EBX	3
#define R_ECX	1
#define R_EDX	2
#define R_ESI	6
#define R_EDI	7
#define R_EBP	5
#define R_ESP	4

void make_executable( void* ptr, int size ) {
	size_t pagesize = sysconf( _SC_PAGESIZE );
	size = (size + pagesize - 1) & ~(pagesize-1);

	int ret = mprotect(ptr, size, PROT_READ | PROT_WRITE | PROT_EXEC );
	if( ret < 0 )
		printf("errno %d : %s\n", errno, strerror(errno));
}

void* mem_alloc_executable( size_t size ) {
	void* p;

	size_t pagesize = sysconf( _SC_PAGESIZE );
	size = (size + pagesize - 1) & ~(pagesize-1);

	if (posix_memalign(&p, pagesize, size) )
		return NULL;

	make_executable(p,size);

	return p;
}

typedef struct vm_asm_t {

	unsigned char* b;
	unsigned char* p;
	// allocated executable code;
	unsigned char* e;

	vm_asm_t( int size ) {
		b = p = e = (unsigned char*)mem_alloc_executable( size );
	}

	vm_asm_t( unsigned char* buf )
	{
		b = p = buf;
		e = NULL;
	}

	unsigned char* pointer(){
		return p;
	}

	unsigned char* base(){
		return b;
	}

	vm_asm_t& Emit1(int v)
	{
		*p++ = v;

		return *this;
	}

	vm_asm_t& Emit2(int v)
	{
		Emit1(v & 255);
		Emit1((v>>8)&255);
		return *this;
	}

	vm_asm_t& Emit4(int v)
	{
		Emit1(v & 255);
		Emit1((v>>8)&255);
		Emit1((v>>16)&255);
		Emit1((v>>24)&255);

		return *this;
	}

	int Hex( char c ) {
		if( c >= 'a' && c <= 'f' )
			return 10 + c - 'a';
		if( c >= 'A' && c <= 'F' )
			return 10 + c - 'A';
		if( c >= '0' && c <= '9' )
			return 0 + c - '0';

		return 0;
	}

	vm_asm_t& Emit( const char* s )
	{
		int c1,c2;
		int c;
		while( true ) {
			c1 = s[0];
			c2 = s[1];

			c = (Hex(c1) << 4) | Hex(c2);

			Emit1(c);

			if( !s[2] )
				break;
			s+=3;
		}

		return *this;
	}

	int size() {
		return p - b;
	}

	/**
	 * allocate an executable hunk of memory and move the current "program" into it.
	 *
	 */
	void* create() {
		if( e == NULL ) {
			e = (unsigned char*)mem_alloc_executable(size());
			memcpy(e,base(),size());
		}
		return e;
	}
};

/**
 * Fabricated functions that can be used to call into the VM
 */
typedef int (*vm_function_t)(...);//va_list args);
typedef struct vm_stub_t
{
	char 	name[32];
	int*	pcode;
	unsigned char* prologue_byte_code;
	static vm_stub_t* current_function;

	vm_function_t initialize( const char* name, int* pcode)
	{
		//strcpy( this->name, name );
		this->pcode = pcode;

		return this->getFunc();
	}

	vm_function_t getFunc()
	{
		int fixup;

		vm_asm_t am(20);

		// Push AX
		am.Emit("50");
		// Move eax, offset to func_ptr
		am.Emit("B8");
		am.Emit4((int)&current_function);
		// move [eax], address of vm_stub_t object
		am.Emit("C7 00");
		am.Emit4((int)this);
		// pop ax
		am.Emit("58");

		// jump to stub (relative, address - cp - self(4)
		am.Emit("E9");
		am.Emit4((int)((unsigned char*)&stub - am.pointer() - 4));
		// explicit ret...(just in case)
		am.Emit("C3");

		prologue_byte_code = (unsigned char*)am.create();

		return (vm_function_t)prologue_byte_code;
	}

	static int stub( int args, ... )
	{
		vm_context context;

		// execution expects a result parameter as the first arg
		// so we must backup one int before the first arg, save
		// that value and restore it when were done
		
        //int* params = ((int*)&args)-1;
#ifdef __APPLE__
        va_list v_args;
        va_start( v_args, args );
        
        int* params = (int*)( ((int)v_args) - sizeof(int) - sizeof(int) );
#endif
        int  save_0 = *params;

		context.execute(current_function->pcode, params);

		int ret = *params;

		*params = save_0;

		return ret;
	}

	static int compiled_to_pcode( int* pcode, int* args )
	{
		vm_context context;

		context_switches++;

		// execute expects the first arg to be the return storage, so pass starting from the pcode,
		// as we only need the pcode to invoke the call.
		int* params = args;//((int*)&pcode);

		context.execute(pcode, params);

		int ret = *params;

		return ret;
	}
};

static int compiled_to_pcode = (int)&vm_stub_t::compiled_to_pcode;

vm_stub_t* vm_stub_t::current_function = NULL;


#define IS_COMPILED( x ) ((*(int*)x) != OP_ENTER)

/**
 * The virtual vm has 2 registers and a stack, the 2 registers represent the top 2 places on the stack at all times.
 *
 * The real stack will be used as the working stack (0 mem allocation) does mean that "threaded" state code cannot be compiled, as the stack is volatile
 *
 * Possible Optimizations :
 * 		switch between EAX/EBX bing r0/r1 to lessen the movment.
 * 		Ie. OP_CONST moves r0->r1, then c->r0. What it instead it moved c->r1,
 * 		then flipped state to say that r1 was now r0. This would forgo the MOV EAX->EBX
 *
 *  Read registers
 * r0 -- result of last operation / top of stack
 *   ==> EAX
 * r1 -- top of stack - 1
 *   ==> EBX

 * Write registers
 * s0 -- top of stack (for storing)
 * s1 -- top of stack - 1 (for storing)

 * Code registers (read)
 * c0 -- 4 byte arg to opcode

 * Volatile registers (only valid for the current instructions use)
 * v0
 * v1

 * psp -- Parameter stack pointer (first param passed)
 *   ==> EDI
 * bsp -- Base pointer to bottom of args/locals
 *   ==> BSP
 * wsp -- Working stack (local allocations)
 *   ==> edx
 * sp  -- Stack pinter
 * cp  -- Code Pointer
 *
 * NOTES:
 * 	Will probably need an array of all the offsets to all the generated instructions for resolving relative jump addresses.
 *
 *  Optimizations, if last operand was a push eax (50) and the next instruction will start with a pop eax (58), ignore both.
 *
 */
typedef struct vm_compiler_t {
	unsigned char code[256];
	unsigned char* p;


	int* compile( int* pcode )
	{
		if( IS_COMPILED( pcode ) )
			return pcode;

		//unsigned char  buffer[4096];
		//code = buffer;

		// instruction offset table. for each pcode instruction this points to the first compiled instruction
		// implementing the pcode. This is later used to calculate relative offsets for jmp/jne/je
		unsigned char* offsets[256]; // max 256 instructions per function...to low..
		int offset_optimizable[256]; // flags indicate which optmiziations are applicable.
		int offset_count = 0;

		// clear
		for( int i =0; i < 256; i++ )
		{
			offsets[i] = 0;
			offset_optimizable[i] = 0;
		}

		// Relative jump fixups.
		// Each time a jump is encountered, the address of offset param is added to the fixups
		// array, with the effective pcode offset that it needs to point to. After compilation
		// each fixup is processed by setting the param to the relative distance to offsets[*fixup]
		int*	fixups[256];	// fixup locations. *fixup = offsets[*fixup] - (int)fixup;
		int		fixup_count = 0;

		int op;
		#undef c0
		int v0;
		int* cp = pcode;

		#define OPTIMIZE_NONE 	0x11
		#define OPTIMIZE_R0		0x10
		#define OPTIMIZE_R0_R1	0x00

		vm_asm_t am( code );

		while( true )
		{
			next:
				offset_optimizable[cp - pcode] = OPTIMIZE_NONE;

			next_optimized_r0:
				offset_optimizable[cp - pcode] |= OPTIMIZE_R0;

			next_optimized:
				offset_optimizable[cp - pcode] |= OPTIMIZE_R0_R1;

				offset_count = cp - pcode;
				offsets[offset_count++]=am.pointer();
				op = *cp++;

			switch( (OPCode)op )
			{
			default:
				printf("error");
				goto next;

			case OP_ENTER:
				v0 = *cp++; // my stack size

				// push ebp
				am.Emit("55");
				// mov  esp, ebp
				am.Emit("89").Emit1( ModRM_r(R_ESP, R_EBP) );
				// sub  <size locals>, esp
				am.Emit("81").Emit1( ModRM_digit_reg(5, R_ESP ) ).Emit4(v0*4);

				NEXT_0_1;

			case OP_LEAVE:
				// EAX has any return value
				v0 = *cp++; // my stack size

				// add  <size locals>, esp
				am.Emit("81").Emit1( ModRM_digit_reg(0, R_ESP ) ).Emit4(v0*4);
				// pop ebp
				am.Emit("5D");
				// return
				am.Emit("C3");

				goto next_function;

			case OP_CONST:
				v0 = *cp++;

				// mov <imm32>, eax
				am.Emit("B8"); am.Emit4( v0 );
				// push eax
				am.Emit("50");

				NEXT_0_1;

			case OP_LOAD_W4:
				// mov <offs>(ebp), eax
				v0 = *cp++;

				if( v0 < 0 )
					// local ebp - offs
					am.Emit("8B 45").Emit1(v0*4);
				else // parameter ebp+0x8+offs
					am.Emit("8B 45").Emit1(0x8 + (v0*4));
				// push eax
				am.Emit("50");

				NEXT_0_1;

			case OP_STOR_W4:
				// pop eax
				am.Emit("58");

				// mov eax, <offs>(ebp)
				v0 = *cp++;

				if( v0 < 0 )
					// local ebp - offs
					am.Emit("89 45").Emit1(v0*4);
				else // parameter ebp+0x8+offs
					am.Emit("89 45").Emit1(0x8 + (v0*4));

				NEXT_0;

			case OP_LOAD_P4:
				v0 = *cp++;
				// pop eax
				am.Emit("58");
				// mov <ofs>(eax),eax
				am.Emit("8B 40").Emit1(v0*4);
				// push eax
				am.Emit("50");

				NEXT_0_1;

			case OP_STOR_P4:
				v0 = *cp++;
				// pop eax
				am.Emit("58");
				// mov <ofs>(eax),eax
				am.Emit("B8 40").Emit1(v0*4);
				// push eax
				am.Emit("50");

				NEXT_0;

			case OP_LEA_W:
				v0 = *cp++;

				// mov ebp, eax
				am.Emit("89").Emit1( ModRM_r(R_EBP, R_EAX) );

				// add <offs>, ebp
				am.Emit("81").Emit1( ModRM_d(0, R_EAX ) );
				if( v0 < 0 )
					am.Emit4( v0*4 );
				else
					am.Emit4( 0x8 + (v0*4) );

				// push eax
				am.Emit("50");

				NEXT_0_1;

			case OP_CALL:
				v0 = *cp++; // address to call (currently the absoulte address of the pcode entry point)

				// TODO: call direct into native/compiled?

				// push <params address> -- second arg to vm_stub_t::compiled_to_pcode
				// push ebp
				am.Emit("55");

				// push <pcode address> -- first parameter of vm_stub_t::compiled_to_pcode
				am.Emit("68").Emit4(v0);

				// call (eax)
				am.Emit("FF 15").Emit4((int)&compiled_to_pcode);

				// pop eax -- remove the pcode address arg
				am.Emit("58");
				// pop eax -- remove the params address arg
				am.Emit("58");

				// TODO: Move return off to local(0)

				NEXT;

			case OP_JMP:
				v0 = *cp++; // offset

				// jmp [0x12345678]
				am.Emit("E9");
				fixups[fixup_count++] = (int*)am.pointer();
				am.Emit4( offset_count + v0 + 1);

				NEXT_0_1;
			case OP_JZ:
				v0 = *cp++;
				// pop eax
				am.Emit("58");
				// cmp eax, 0
				am.Emit("83").Emit1( ModRM_d(7,R_EAX) ).Emit1(0);

				// jz
				am.Emit("0F 84");
				fixups[fixup_count++] = (int*)am.pointer();
				am.Emit4( offset_count + v0 + 1);

				NEXT_0;
			case OP_JNZ:
				v0 = *cp++;
				// pop eax
				am.Emit("58");
				// cmp eax, 0
				am.Emit("83").Emit1( ModRM_d(7,R_EAX) ).Emit1(0);
				// jnz
				am.Emit("0F 85");
				fixups[fixup_count++] = (int*)am.pointer();
				am.Emit4( offset_count + v0 + 1);

				NEXT_0;

			case OP_EQ:
				// pop eax
				am.Emit("58");
				// pop ebx
				am.Emit("5B");
				// cmp ebx,eax
				am.Emit("39").Emit1( ModRM_r(R_EBX, R_EAX) );
				// sete al
				am.Emit("0F 94").Emit1( ModRM_d(0, R_EAX) );
				// movzbl al, eax
				am.Emit("0F B6").Emit1( ModRM_r(R_EAX, R_EAX) );
				// push eax
				am.Emit("50");

				NEXT_0;
			case OP_NEQ:
				// pop eax
				am.Emit("58");
				// pop ebx
				am.Emit("5B");
				// cmp ebx,eax
				am.Emit("39").Emit1( ModRM_r(R_EBX, R_EAX) );
				// setne al
				am.Emit("0F 95").Emit1( ModRM_d(0, R_EAX) );
				// movzbl al, eax
				am.Emit("0F B6").Emit1( ModRM_r(R_EAX, R_EAX) );
				// push eax
				am.Emit("50");

				NEXT_0;
			case OP_MULI:
				// pop eax
				am.Emit("58");
				// pop ebx
				am.Emit("5B");
				// mul ebx
				am.Emit("F7").Emit1( ModRM_digit_reg(4, R_EBX));
				// push eax
				am.Emit("50");

				NEXT_0;
			case OP_ADDI:
				// pop eax
				am.Emit("58");
				// pop ebx
				am.Emit("5B");
				// add ebx,eax
				am.Emit("03").Emit1( ModRM_r( R_EBX, R_EAX));
				// push eax
				am.Emit("50");

				NEXT_0;
			case OP_SUBI:
				// pop eax
				am.Emit("58");
				// pop ebx
				am.Emit("5B");
				// sub always subtracts the src from the dest., sive what we want is eax = ebx - eax, we swap them first.
				// xchg ebx, eaxexchange
				am.Emit("93");
				// sub ebx,eax
				am.Emit("29").Emit1( ModRM_r( R_EBX, R_EAX));
				// push eax
				am.Emit("50");
				NEXT_0;
/*

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

				NEXT_0_1;
		*/
			}
		}

		next_function:

		// optimize.
		// Any place where the last instruction of the previous op is a push_eax, and the next op starts with a pop eax, we can
		// optimize the pops out. PROVIDED that the op is not the target of a jmp (no idea what state the registers are in coming from a jmp).
		//
		// to do this we spin through all the offsets looking for valid instructions starts (offset != 0) and look back one instruction.
		//
		// TODO :can ebx be optimized the same way?
		//
		// TODO : what happens when the previous insturction just HAPPENS to end with a 0x50 ? (ie OP_Jxx,OP_Const, etc)

		for( int i = 0; i < offset_count; i++ )
		{
			// is it a valid opcode start.
			if( ! offsets[i] )
				continue;
			// is it optimizable
			if( offset_optimizable[i] == OPTIMIZE_NONE )
				continue;

			// check that its not the target of a jmp.
			int k;
			for( k = 0; k < fixup_count; k++ )
				if( i == *fixups[k] )
					break;
			if( k != fixup_count )
				continue;

			// check for the optimization...
			if( *offsets[i] == 0x58 )
			{
				*offsets[i] = 0x90;
				*(offsets[i]-1) = 0x90;
			}
		}

		// process fixups
		for( int i = 0; i < fixup_count; i++ )
		{
			if( *fixups[i] >= offset_count )
			{
				printf("reference to non existant offset : %d\n", *fixups[i] );
			}

			int target = *fixups[i];

			int from = (int)fixups[i] + 4; // jmp is relative to the NEXT opcode.
			int to = (int)offsets[target];

			int delta = to - from;

			*fixups[i] = delta;

			// calculate the delta from the address of the fixup to the address of the offset
			//*fixups[i] = (int)offsets[*fixups[i]] - (int)(fixups+i);
		}

		return (int*)am.create();

//		return (int*)&code;
	}
};

int run_jit( int* pcode, ... ) {
    vm_compiler_t& compiler = *new vm_compiler_t();
    
	int* compiled = compiler.compile(pcode);
    
    if( ! IS_COMPILED( compiled ) )
        printf("FAILED TO COMPILE CODE!!!");
    
    va_list args;
	va_start(args,pcode);
    
	((vm_function_t)compiled)(0, va_arg(args,int), va_arg(args,int) );
    
	va_end(args);
}
