{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit machexc;

{$mode objfpc}{$H+}

interface

uses
  machapi;

//  mach_exception_types.h

const
  {*	Machine-independent exception definitions. }
  EXC_BAD_ACCESS      = 1;  { Could not access memory  }
                            { Code contains kern_return_t describing error.  }
                            { Subcode contains bad memory address.  }
  EXC_BAD_INSTRUCTION = 2;  { Instruction failed  }
                            { Illegal or undefined instruction or operand  }
  EXC_ARITHMETIC      = 3;  { Arithmetic exception  }
                            { Exact nature of exception is in code field  }
  EXC_EMULATION       = 4;  { Emulation instruction  }
                            { Emulation support instruction encountered  }
                            { Details in code and subcode fields	 }
  EXC_SOFTWARE        = 5;  { Software generated exception  }
                            { Exact exception is in code field.  }
                            { Codes 0 - 0xFFFF reserved to hardware  }
                            { Codes 0x10000 - 0x1FFFF reserved for OS emulation (Unix)  }
  EXC_BREAKPOINT      = 6;  { Trace, breakpoint, etc.  }
                            { Details in code field.  }
  EXC_SYSCALL         = 7;  { System calls.  }
  EXC_MACH_SYSCALL    = 8;  { Mach system calls.  }
  EXC_RPC_ALERT       = 9;  { RPC alert  }
  EXC_CRASH           = 10; { Abnormal process exit  }

  {	Machine-independent exception behaviors  }

  EXCEPTION_DEFAULT = 1; {	Send a catch_exception_raise message including the identity. }
  EXCEPTION_STATE = 2;   {	Send a catch_exception_raise_state message including the	thread state. }
  EXCEPTION_STATE_IDENTITY = 3; {	Send a catch_exception_raise_state_identity message includingthe thread identity and state. }
  MACH_EXCEPTION_CODES = $80000000; {	Send 64-bit code and subcode in the exception header  }


  {* Masks for exception definitions, above
   * bit zero is unused, therefore 1 word = 31 exception types  }

  EXC_MASK_BAD_ACCESS = 1 shl EXC_BAD_ACCESS;
  EXC_MASK_BAD_INSTRUCTION = 1 shl EXC_BAD_INSTRUCTION;
  EXC_MASK_ARITHMETIC = 1 shl EXC_ARITHMETIC;
  EXC_MASK_EMULATION = 1 shl EXC_EMULATION;
  EXC_MASK_SOFTWARE = 1 shl EXC_SOFTWARE;
  EXC_MASK_BREAKPOINT = 1 shl EXC_BREAKPOINT;
  EXC_MASK_SYSCALL = 1 shl EXC_SYSCALL;
  EXC_MASK_MACH_SYSCALL = 1 shl EXC_MACH_SYSCALL;
  EXC_MASK_RPC_ALERT = 1 shl EXC_RPC_ALERT;
  EXC_MASK_CRASH = 1 shl EXC_CRASH;
  EXC_MASK_ALL = EXC_MASK_BAD_ACCESS or
                 EXC_MASK_BAD_INSTRUCTION or
                 EXC_MASK_ARITHMETIC or
                 EXC_MASK_EMULATION or
                 EXC_MASK_SOFTWARE or
                 EXC_MASK_BREAKPOINT or
                 EXC_MASK_SYSCALL or
                 EXC_MASK_MACH_SYSCALL or
                 EXC_MASK_RPC_ALERT or
                 EXC_MASK_CRASH;
  { ZERO is illegal  }
  FIRST_EXCEPTION = 1;
  {* Machine independent codes for EXC_SOFTWARE
   * Codes 0x10000 - 0x1FFFF reserved for OS emulation (Unix)
   * 0x10000 - 0x10002 in use for unix signals }
  { Unix signal exceptions  }
  EXC_SOFT_SIGNAL = $10003;

// mach/exc.h

type
  function_ptr_t = procedure (machport: mach_port_t; name : pchar; msg_type_num: mach_msg_type_number_t); cdecl;

  function_table_entry = packed record
    name      : pchar;
    _function : function_ptr_t;
  end;

const
  exc_MSG_COUNT	= 3;

function exception_raise (
  exception_port, thread, task: mach_port_t;
	exception   : exception_type_t;
	code        : exception_data_t;
	codeCnt     : mach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine exception_raise_state *}
function exception_raise_state
(
	exception_port   : mach_port_t;
	exception        : exception_type_t;
	const code       : exception_data_t;
	codeCnt          : mach_msg_type_number_t;
	flavor           : pinteger;
	const old_state  : thread_state_t;
	old_stateCnt     : mach_msg_type_number_t;
	new_state        : thread_state_t;
	var new_stateCnt : mach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine exception_raise_state_identity *}
function  exception_raise_state_identity (
	exception_port   : mach_port_t;
	thread           : mach_port_t;
	task             : mach_port_t;
	exception        : exception_type_t;
	code             : exception_data_t;
	codeCnt          : mach_msg_type_number_t;
	flavor           : pinteger;
	old_state        : thread_state_t;
	old_stateCnt     : mach_msg_type_number_t;
	new_state        : thread_state_t;
	var new_stateCnt : mach_msg_type_number_t
): kern_return_t; cdecl; external;

// catch_exception_* are not implemented in system library
// they must be implemented by user program.
// They are "global callbacks"

function catch_exception_raise (
	exception_port  : mach_port_t;
	thread          : mach_port_t;
	task            : mach_port_t;
	exception       : exception_type_t;
	code            : exception_data_t;
	codeCnt         : mach_msg_type_number_t
): kern_return_t; cdecl; external;

function catch_exception_raise_state(
	exception_port    : mach_port_t;
	exception         : exception_type_t;
	const code        : exception_data_t;
	codeCnt           : mach_msg_type_number_t;
	flavor            : pinteger;
	const old_state   : thread_state_t;
	old_stateCnt      : mach_msg_type_number_t;
	new_state         : thread_state_t;
	var new_stateCnt  : mach_msg_type_number_t
): kern_return_t; cdecl; external;

function catch_exception_raise_state_identity (
	exception_port    : mach_port_t;
	thread            : mach_port_t;
	task              : mach_port_t;
	exception         : exception_type_t;
	code              : exception_data_t;
	codeCnt           : mach_msg_type_number_t;
	flavor            : pinteger;
	old_state         : thread_state_t;
	old_stateCnt      : mach_msg_type_number_t;
	new_state         : thread_state_t;
	var new_stateCnt  : mach_msg_type_number_t
): kern_return_t; cdecl; external;

{
from: http://web.mit.edu/darwin/src/modules/xnu/osfmk/man/exc_server.html

PARAMETERS

in_msg
[pointer to in structure] The exception message received from the kernel.

out_msg
[out structure] A reply message.

DESCRIPTION

The exc_server function is the MIG generated server handling function to handle
messages from the kernel relating to the occurrence of an exception in a thread.
Such messages are delivered to the exception port set via thread_set_exception_ports
or task_set_exception_ports. When an exception occurs in a thread, the thread sends
an exception message to its exception port, blocking in the kernel waiting for
the receipt of a reply. The exc_server function performs all necessary argument
handling for this kernel message and calls catch_exception_raise,
catch_exception_raise_state or catch_exception_raise_state_identity,
which should handle the exception.

If the called routine returns KERN_SUCCESS, a reply message will be sent,
allowing the thread to continue from the point of the exception; otherwise,
no reply message is sent and the called routine must have dealt with the
exception thread directly.
}

function exc_server(InHeadP : pmach_msg_header_t;
  OutHeadP : pmach_msg_header_t ): boolean_t; cdecl; external;

{********************** Caution **************************}
{* The following data types should be used to calculate  *}
{* maximum message sizes only. The actual message may be *}
{* smaller, and the position of the arguments within the *}
{* message layout may vary from what is presented here.  *}
{* For example, if any of the arguments are variable-    *}
{* sized, and less than the maximum is sent, the data    *}
{* will be packed tight in the actual message to reduce  *}
{* the presence of holes.                                *}
{********************** Caution **************************}

{* typedefs for all requests *}

// #pragma pack(4) //??? {$pack 4} ???
type
	__Request__exception_raise_t = packed record
		Head        : mach_msg_header_t;
		{* start of the kernel processed data *}
		msgh_body   : mach_msg_body_t;
		thread      : mach_msg_port_descriptor_t;
		task        : mach_msg_port_descriptor_t;
		{* end of the kernel processed data *}
		NDR         : NDR_record_t;
		exception   : exception_type_t;
		codeCnt     : mach_msg_type_number_t;
		code        : array [0..1] of integer_t;
	end;

	__Request__exception_raise_state_t = packed record
		Head          : mach_msg_header_t;
		NDR           : NDR_record_t;
		exception     : exception_type_t;
		codeCnt       : mach_msg_type_number_t;
		code          : array [0..1] of integer_t;
		flavor        : integer;
		old_stateCnt  : mach_msg_type_number_t;
		old_state     : array [0..143] of natural_t;
  end;

  __Request__exception_raise_state_identity_t = packed record
    Head      : mach_msg_header_t;
		{* start of the kernel processed data *}
		msgh_body : mach_msg_body_t;
		thread    : mach_msg_port_descriptor_t;
		task      : mach_msg_port_descriptor_t;
		{* end of the kernel processed data *}
		NDR       : NDR_record_t;
		exception : exception_type_t;
		codeCnt   : mach_msg_type_number_t;
		code      : array [0..2] of integer_t;
		flavor    : integer;
		old_stateCnt  : mach_msg_type_number_t;
		old_state     : array [0..143] of natural_t;
  end;

{* union of all requests *}

  __RequestUnion__exc_subsystem = packed record
  case byte of
    0:(req_raise           : __Request__exception_raise_t);
    1:(req_raise_state     : __Request__exception_raise_state_t);
  	2:(req_raise_identity  : __Request__exception_raise_state_identity_t);
  end;
  PRequestUnion__exc_subsystem = ^__RequestUnion__exc_subsystem;

{* typedefs for all replies *}

	__Reply__exception_raise_t = packed record
		Head    : mach_msg_header_t;
		NDR     : NDR_record_t;
		RetCode : kern_return_t;
	end;

	__Reply__exception_raise_state_t = packed record
		Head    : mach_msg_header_t;
		NDR     : NDR_record_t;
		RetCode : kern_return_t;
		flavor  : integer;
		new_stateCnt  : mach_msg_type_number_t;
		new_state     : array [0..143] of natural_t;
	end;

	__Reply__exception_raise_state_identity_t = packed record
		Head      : mach_msg_header_t;
		NDR       : NDR_record_t;
		RetCode   : kern_return_t;
		flavor    : integer;
		new_stateCnt  : mach_msg_type_number_t;
		new_state     : array [0..143] of natural_t;
  end;

{* union of all replies *}

  __ReplyUnion__exc_subsystem = packed record
	  rep_raise           : __Reply__exception_raise_t ;
  	rep_raise_state     : __Reply__exception_raise_state_t;
  	rep_raise_identity  : __Reply__exception_raise_state_identity_t;
  end;
  PReplyUnion__exc_subsystem = ^__ReplyUnion__exc_subsystem;

const
  msgid_exception_raise                = 2401;
  msgid_exception_raise_state          = 2402;
  msgid_exception_raise_state_identity = 2403;

{*
 * No machine dependent types for the 80386
 *}

const
  EXC_TYPES_COUNT	= 11;	{* incl. illegal exception 0 *}

{*
 *	Codes and subcodes for 80386 exceptions.
 *}

  EXCEPTION_CODE_MAX	= 2;	{* currently code and subcode *}


{*
 *	machine dependent exception masks
 *}
  EXC_MASK_MACHINE	= 0;

{*
 *	EXC_BAD_INSTRUCTION
 *}

  EXC_PPC_INVALID_SYSCALL	 = 1;    {* invalid syscall number *}
  EXC_PPC_UNIPL_INST	     = 2;    {* unimplemented instruction *}
  EXC_PPC_PRIVINST	       = 3;    {* priviledged instruction *}
  EXC_PPC_PRIVREG		       = 4;    {* priviledged register *}
  EXC_PPC_TRACE		         = 5;    {* trace/single-step *}
  EXC_PPC_PERFMON          = 6;    {* performance monitor *}

  EXC_I386_INVOP			= 1;

  EXC_ARM_UNDEFINED	  = 1;	{* Undefined *}

{*
 *	EXC_ARITHMETIC
 *}

  EXC_PPC_OVERFLOW		      = 1;    {* integer overflow *}
  EXC_PPC_ZERO_DIVIDE		    = 2;    {* integer divide by zero *}
  EXC_PPC_FLT_INEXACT		    = 3;    {* IEEE inexact exception *}
  EXC_PPC_FLT_ZERO_DIVIDE		= 4;    {* IEEE zero divide *}
  EXC_PPC_FLT_UNDERFLOW		  = 5;    {* IEEE floating underflow *}
  EXC_PPC_FLT_OVERFLOW		  = 6;    {* IEEE floating overflow *}
  EXC_PPC_FLT_NOT_A_NUMBER	= 7;    {* IEEE not a number *}

  EXC_I386_DIV			 = 1;
  EXC_I386_INTO			 = 2;
  EXC_I386_NOEXT		 = 3;
  EXC_I386_EXTOVR		 = 4;
  EXC_I386_EXTERR		 = 5;
  EXC_I386_EMERR		 = 6;
  EXC_I386_BOUND		 = 7;
  EXC_I386_SSEEXTERR = 8;

{*
 * EXC_PPC_NOEMULATION should go away when we add software emulation
 * for floating point. Right now we don't support this.
 *}

  EXC_PPC_NOEMULATION		= 8;	{* no floating point emulation *}
  EXC_PPC_ALTIVECASSIST	= 9;	{* Altivec Denorm Assist *}


{*
 *	EXC_SOFTWARE
 *	Note: 0x10000-0x10003 in use for unix signal
 *}


  EXC_PPC_TRAP	  	= 1;	{* Program trap *}
  EXC_PPC_MIGRATE		= $10100;		{* Time to bolt *}

{*
 *	EXC_BAD_ACCESS
 *}

  EXC_PPC_VM_PROT_READ  = $101; {* error reading syscall args *}
  EXC_PPC_BADSPACE		  = $102; {* bad space referenced *}
  EXC_PPC_UNALIGNED		  = $103; {* unaligned data reference *}

  EXC_ARM_DA_ALIGN	= $101;	{* Alignment Fault *}
  EXC_ARM_DA_DEBUG	= $102;	{* Debug (watch/break) Fault *}

{*
 *	EXC_BREAKPOINT
 *}

  EXC_PPC_BREAKPOINT	 = EXC_PPC_TRAP;  {* breakpoint trap *}

  EXC_I386_SGL		=	1;
  EXC_I386_BPT		= 2;

  EXC_ARM_BREAKPOINT	= 1;


  EXC_I386_DIVERR		  = 0;	{* divide by 0 eprror		*}
  EXC_I386_SGLSTP		  = 1;	{* single step			*}
  EXC_I386_NMIFLT		  = 2;	{* NMI				*}
  EXC_I386_BPTFLT		  = 3;	{* breakpoint fault		*}
  EXC_I386_INTOFLT	  = 4;	{* INTO overflow fault		*}
  EXC_I386_BOUNDFLT	  = 5;	{* BOUND instruction fault	*}
  EXC_I386_INVOPFLT	  = 6;	{* invalid opcode fault		*}
  EXC_I386_NOEXTFLT	  = 7;	{* extension not available fault*}
  EXC_I386_DBLFLT		  = 8;	{* double fault			*}
  EXC_I386_EXTOVRFLT  = 9;	{* extension overrun fault	*}
  EXC_I386_INVTSSFLT  = 10;	{* invalid TSS fault		*}
  EXC_I386_SEGNPFLT	  = 11;	{* segment not present fault	*}
  EXC_I386_STKFLT		  = 12;	{* stack fault			*}
  EXC_I386_GPFLT		  = 13;	{* general protection fault	*}
  EXC_I386_PGFLT		  = 14;	{* page fault			*}
  EXC_I386_EXTERRFLT	= 16;	{* extension error fault	*}
  EXC_I386_ALIGNFLT	  = 17;	{* Alignment fault *}
  EXC_I386_ENDPERR	  = 33;	{* emulated extension error flt	*}
  EXC_I386_ENOEXTFLT	= 32;	{* emulated ext not present	*}

implementation

end.

