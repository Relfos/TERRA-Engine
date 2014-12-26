{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit machapi;
// mach_vm_size_t processor dependant!?

// Mach man pages
// http://www.opensource.apple.com/source/xnu/
// http://web.mit.edu/darwin/src/modules/xnu/osfmk/man/


{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

type
  NDR_record_t = packed record
    mig_vers      : byte;    
    if_vers       : byte;    
    reserved1     : byte;    
    mig_encoding  : byte;    
    int_rep       : byte;    
    char_rep      : byte;    
    float_rep     : byte;    
    reserved2     : byte;    
  end;
  
const
  { MIG supported protocols for Network Data Representation }
  NDR_PROTOCOL_2_0     =  0;

  { NDR 2.0 format flag type definition and values. }
  NDR_INT_BIG_ENDIAN    = 0;
  NDR_INT_LITTLE_ENDIAN = 1;
  NDR_FLOAT_IEEE        = 0;
  NDR_FLOAT_VAX         = 1;
  NDR_FLOAT_CRAY        = 2; 
  NDR_FLOAT_IBM         = 3;
  NDR_CHAR_ASCII        = 0;
  NDR_CHAR_EBCDIC       = 1;
  
{*	File:	h/kern_return.h
 *	Author:	Avadis Tevanian, Jr.
 *	Date:	1985
 *
 *	Kernel return codes. *}

const
  KERN_SUCCESS= 0;
  KERN_INVALID_ADDRESS	  =	1;  {* Specified address is not currently valid. *}
  KERN_PROTECTION_FAILURE	=	2;  {* Specified memory is valid, but does not permit the required forms of access.*}
  KERN_NO_SPACE		        =	3;  {* The address range specified is already in use, or no address range of the size specified could be found. *}
  KERN_INVALID_ARGUMENT	  =	4;  {* The function requested was not applicable to this type of argument, or an argument is invalid		 *}
  KERN_FAILURE			      = 5;  {* The function could not be performed.  A catch-all.	 *}
  KERN_RESOURCE_SHORTAGE	=	6;  {* A system resource could not be allocated to fulfill  this request.  This failure may not be permanent.  *}
  KERN_NOT_RECEIVER	      = 7;  {* The task in question does not hold receive rights for the port argument. *}
  KERN_NO_ACCESS		      =	8;  {* Bogus access restriction.*}
  KERN_MEMORY_FAILURE	    =	9;
		{* During a page fault, the target address refers to a
		 * memory object that has been destroyed.  This
		 * failure is permanent.		 *}
  KERN_MEMORY_ERROR 	=	10;
		{* During a page fault, the memory object indicated
		 * that the data could not be returned.  This failure
		 * may be temporary; future attempts to access this
		 * same data may succeed, as defined by the memory
		 * object. *}
 	KERN_ALREADY_IN_SET	= 11; {* The receive right is already a member of the portset. *}
  KERN_NOT_IN_SET		  =	12;	{* The receive right is not a member of a port set. *}
  KERN_NAME_EXISTS	  =	13; {* The name already denotes a right in the task. *}
  KERN_ABORTED		    =	14;	{* The operation was aborted.  Ipc code will catch this and reflect it as a message error. *}
  KERN_INVALID_NAME	  =	15;	{* The name doesn't denote a right in the task. *}
 	KERN_INVALID_TASK	  =	16;	{* Target task isn't an active task. *}
  KERN_INVALID_RIGHT	=	17; {* The name denotes a right, but not an appropriate right. *}
  KERN_INVALID_VALUE	=	18;	{* A blatant range error.	 *}
 	KERN_UREFS_OVERFLOW	=	19;	{* Operation would overflow limit on user-references. *}
 	KERN_INVALID_CAPABILITY	=	20;	{* The supplied (port) capability is improper. *}
  KERN_RIGHT_EXISTS		    = 21;	{* The task already has send or receive rights for the port under another name. *}
 	KERN_INVALID_HOST	  =	22;	{* Target host isn't actually a host. *}
  KERN_MEMORY_PRESENT	=	23;	{* An attempt was made to supply "precious" data for memory that is already present in a memory object.*}

  KERN_MEMORY_DATA_MOVED		= 24;
		{* A page was requested of a memory manager via
		 * memory_object_data_request for an object using
		 * a MEMORY_OBJECT_COPY_CALL strategy, with the
		 * VM_PROT_WANTS_COPY flag being used to specify
		 * that the page desired is for a copy of the
		 * object, and the memory manager has detected
		 * the page was pushed into a copy of the object
		 * while the kernel was walking the shadow chain
		 * from the copy to the object. This error code
		 * is delivered via memory_object_data_error
		 * and is handled by the kernel (it forces the
		 * kernel to restart the fault). It will not be
		 * seen by users.
		 *}

  KERN_MEMORY_RESTART_COPY	= 25;	{* A strategic copy was attempted of an object *}
                                	{* upon which a quicker copy is now possible.  *}
                                	{* The caller should retry the copy using      *}
                                 	{* vm_object_copy_quickly. This error code     *}
                                 	{* is seen only by the kernel.                 *}
  KERN_INVALID_PROCESSOR_SET	= 26;	{* An argument applied to assert processor set privilege  was not a processor set control port. *}
  KERN_POLICY_LIMIT	          =	27;	{* The specified scheduling attributes exceed the thread's limits. *}
  KERN_INVALID_POLICY		      = 28;	{* The specified scheduling policy is not currently enabled for the processor set. *}
  KERN_INVALID_OBJECT	        =	29; {* The external memory manager failed to initialize the  memory object. *}
  KERN_ALREADY_WAITING	      =	30;	{* A thread is attempting to wait for an event for which there is already a waiting thread. *}
  KERN_DEFAULT_SET		        = 31;	{* An attempt was made to destroy the default processor set. *}
  KERN_EXCEPTION_PROTECTED	  = 32; {* An attempt was made to fetch an exception port that is protected, *}
                                    {*  or to abort a thread while processing a protected exception.     *}
  KERN_INVALID_LEDGER		      = 33;	{* A ledger was required but not supplied. *}
  KERN_INVALID_MEMORY_CONTROL	= 34;	{* The port was not a memory cache control port. *}
  KERN_INVALID_SECURITY		= 35;	{* An argument supplied to assert security privilege was not a host security port. *}
  KERN_NOT_DEPRESSED		  = 36;	{* thread_depress_abort was called on a thread which  was not currently depressed.	 *}
  KERN_TERMINATED		      =	37; {* Object has been terminated and is no longer available *}
  KERN_LOCK_SET_DESTROYED	=	38;	{* Lock set has been destroyed and is no longer available.*}
  KERN_LOCK_UNSTABLE	    =	39;	{* The thread holding the lock terminated before releasing the lock *}
  KERN_LOCK_OWNED			    = 40;	{* The lock is already owned by another thread *}
  KERN_LOCK_OWNED_SELF		    = 41; {* The lock is already owned by the calling thread *}
  KERN_SEMAPHORE_DESTROYED    = 42; {* Semaphore has been destroyed and is no longer available. *}
  KERN_RPC_SERVER_TERMINATED  =	43;	{* Return from RPC indicating the target server was terminated before it successfully replied *}
  KERN_RPC_TERMINATE_ORPHAN	= 44;	  {* Terminate an orphaned activation. *}
  KERN_RPC_CONTINUE_ORPHAN	= 45; 	{* Allow an orphaned activation to continue executing. *}
  KERN_NOT_SUPPORTED  = 46; {* Empty thread activation (No thread linked to it)*}
  KERN_NODE_DOWN		  =	47;	{* Remote node down or inaccessible.	 *}
  KERN_NOT_WAITING		= 48;	{* A signalled thread was not actually waiting. *}
  KERN_OPERATION_TIMED_OUT  = 49;  {* Some thread-oriented operation (semaphore_wait) timed out *}
  KERN_RETURN_MAX			      = $100;	    {* Maximum return value allowable *}


type
//  boolean_t  = Integer;
  boolean_t  = LongBool;
//  integer_t  = Integer;
//  natural_t  = LongWord;
  uint64_t   = QWord;
  int64_t    = Int64;


{*
 *	Header file for basic, machine-dependent data types.  i386 version.
 *}

  int16_t = SmallInt;
  int32_t = Integer;
  uint32_t = LongWord;

  MACH_MSG_TYPE_REAL_32 = single;
  MACH_MSG_TYPE_REAL_64 = double;


// from ISO/IEC 988:1999 spec *}
// 7.18.1.4 Integer types capable of hgolding object pointers *}
{*
 * The [u]intptr_t types for the native
 * integer type, e.g. 32 or 64 or.. whatever
 * register size the machine has.  They are
 * used for entities that might be either
 * [unsigned] integers or pointers, and for
 * type-casting between the two.
 *
 * For instance, the IPC system represents
 * a port in user space as an integer and
 * in kernel space as a pointer.
 *}
//#if defined(__LP64__)
//type uintptr_t = uint64_t;
//type intptr_t = int64_t;
//#else
//type uintptr_t = uint32_t;
//type intptr_t = int32_t;
//#endif

{*
 * These are the legacy Mach types that are
 * the [rough] equivalents of the standards above.
 * They were defined in terms of int, not
 * long int, so they remain separate.
 *}
{if defined(__LP64__)
type register_t = int64_t;
#else
type register_t = int32_t;
#endif}
 register_t = int32_t;
 integer_t = int32_t;
 natural_t = uint32_t;


 pinteger_t = ^integer_t;
 pnatural_t = ^natural_t;


{*
 * These are the VM types that scale with the address
 * space size of a given process.
 *}

{if defined(__LP64__)
type vm_address_t = uint64_t;
type vm_offset_t = uint64_t;
type vm_size_t = uint64_t;
#else}
  vm_address_t = natural_t;
  vm_offset_t = natural_t;
  vm_size_t = natural_t;

  pvm_address_t = ^vm_address_t;
  pvm_offset_t = ^vm_offset_t;
  pvm_size_t = ^vm_size_t;
//endif

{*
 * The mach_vm_xxx_t types are sized to hold the
 * maximum pointer, offset, etc... supported on the
 * platform.
 *}
  mach_vm_address_t = uint64_t;
  pmach_vm_address_t = ^mach_vm_address_t;
  mach_vm_offset_t = uint64_t;

  mach_vm_size_t = uint64_t;
  pmach_vm_size_t = ^mach_vm_size_t;

{	Time value returned by kernel.}
  time_value = record
  	seconds      : integer_t;
	  microseconds : integer_t;
  end;
  time_value_t = time_value;

// mach_types.h

type
  kern_return_t = Integer;

  mach_port_t = Integer;
  pmach_port_t = ^mach_port_t;

  task_t   = mach_port_t;
  task_name_t = mach_port_t;
	thread_act_t = mach_port_t;
  ipc_space_t = mach_port_t		;
  thread_t = mach_port_t;
  host_t = mach_port_t;
  host_priv_t = mach_port_t;
  host_security_t = mach_port_t;
  processor_t = mach_port_t;
  processor_set_t = mach_port_t;
  processor_set_control_t = mach_port_t;
  semaphore_t = mach_port_t;
  lock_set_t = mach_port_t;
  ledger_t = mach_port_t;
  alarm_t = mach_port_t;
  clock_serv_t = mach_port_t;
  clock_ctrl_t = mach_port_t;

  mach_msg_type_number_t = Integer;
  Pmach_msg_type_number_t = ^mach_msg_type_number_t;

{ * Mig doesn't translate the components of an array.
  * For example, Mig won't use the thread_t translations
  * to translate a thread_array_t argument.  So, these definitions
  * are not completely accurate at the moment for other kernel
  * components. }

//typedef task_t			*task_array_t;
  Ttask_array = array [word] of task_t;
  Ptask_array = ^Ttask_array;
  //task_array_t = ^task_t;
  task_array_t = Ptask_array;
  //todo:
  thread_array_t = ^thread_t;
  processor_set_array_t = ^processor_set_t;
  processor_set_name_array_t = ^processor_set_t;
  processor_array_t  = ^processor_t;

  Tthread_act_array_t = array [word] of thread_act_t;
  Pthread_act_array_t = ^Tthread_act_array_t;
  thread_act_array_t = Pthread_act_array_t;

  //thread_act_array_t = ^thread_act_t;


  ledger_array_t     = ^ledger_t;

{* These aren't really unique types.  They are just called
 * out as unique types at one point in history.  So we list
 * them here for compatibility. }
  processor_set_name_t =  processor_set_t;

  { These types are just hard-coded as ports }
  clock_reply_t = mach_port_t;
  bootstrap_t = mach_port_t;
  mem_entry_name_port_t =	mach_port_t;
  exception_handler_t = mach_port_t;

  Texception_handler_array = array [word] of exception_handler_t;
  Pexception_handler_array = ^Texception_handler_array;
  exception_handler_array_t = Pexception_handler_array;
  //exception_handler_array_t = ^exception_handler_t;

  vm_task_entry_t = mach_port_t;
  io_master_t = mach_port_t;
  UNDServerRef = mach_port_t;

const
  TASK_NULL	= 0;
  TASK_NAME_NULL = 0;
  THREAD_NULL		 = 0;
  THR_ACT_NULL 	 = 0;
  IPC_SPACE_NULL = 0;
  HOST_NULL		   = 0;
  HOST_PRIV_NULL = 0;
  HOST_SECURITY_NULL = 0;
  PROCESSOR_SET_NULL = 0;
  PROCESSOR_NULL	= 0;
  SEMAPHORE_NULL	= 0;
  LOCK_SET_NULL		= 0;
  LEDGER_NULL 		= 0;
  ALARM_NULL		  = 0;
  CLOCK_NULL		  = 0;
  UND_SERVER_NULL	= 0;

type
  ledger_item_t = natural_t;

  Temulation_vector = array [word] of mach_vm_offset_t;
  Pemulation_vector = ^Temulation_vector;
  //emulation_vector_t = ^mach_vm_offset_t;
  emulation_vector_t = Pemulation_vector;

  user_subsystem_t = Pchar;
  labelstr_t = Pchar;

{* File:	mach/error.h
 * Purpose:
 *	error module definitions *}

{*	error number layout as follows:
 *
 *	hi		 		       lo
 *	| system(6) | subsystem(12) | code(14) | *}

const
  err_none		    = 0;
  ERR_SUCCESS		  = 0;
  ERR_ROUTINE_NIL	=	nil;

  system_emask	= $3f  shr 26;
  sub_emask		  = $fff shr 14;
  code_emask	  =	$3fff;

(*	major error systems	*)

const
  //err_system(x)		(((x)&0x3f)<<26)       ((| and $3f) shr 26)
  err_kern	      = (0 shr 26);   //	err_system(0x1)	/* kernel */
  err_us		      = (1 shr 26);   //	err_system(0x1)		/* user space library */
  err_server      = (2 shr 26);	  //	err_system(0x2)		/* user space servers */
  err_ipc		      = (3 shr 26);	  //  err_system(0x3)		/* old ipc errors */
  err_mach_ipc    =	(4 shr 26);	  //  err_system(0x4)		/* mach-ipc errors */
  err_dipc		    = (7 shr 26);   //  err_system(0x7)		/* distributed ipc */
  err_local		    = ($3e shr 26);  //  err_system(0x3e)	/* user defined errors */
  err_ipc_compat  = ($ef shr 26);  //	err_system(0x3f)	/* (compatibility) mach-ipc errors */
	err_max_system	= $3f;

type
  mach_error_t   = kern_return_t;
  mach_error_fn_t = function : mach_error_t; cdecl;


// port.h

{*
 *	File:	mach/port.h
 *
 *	Definition of a Mach port
 *
 *	Mach ports are the endpoints to Mach-implemented communications
 *	channels (usually uni-directional message queues, but other types
 *	also exist).
 *
 *	Unique collections of these endpoints are maintained for each
 *	Mach task.  Each Mach port in the task's collection is given a
 *	[task-local] name to identify it - and the the various "rights"
 *	held by the task for that specific endpoint.
 *
 *	This header defines the types used to identify these Mach ports
 *	and the various rights associated with them.  For more info see:
 *
 *	<mach/mach_port.h> - manipulation of port rights in a given space
 *	<mach/message.h> - message queue [and port right passing] mechanism
 *
 *}


{*
*	mach_port_name_t - the local identity for a Mach port
*
*	The name is Mach port namespace specific.  It is used to
*	identify the rights held for that port by the task whose
*	namespace is implied [or specifically provided].
*
*	Use of this type usually implies just a name - no rights.
*	See mach_port_t for a type that implies a "named right."
*
*}

type
  mach_port_name_t = natural_t;
  mach_port_name_array_t = ^mach_port_name_t;

  pmach_port_name_t = ^mach_port_name_t;
  pmach_port_name_array_t = ^mach_port_name_array_t;

{*
 *	mach_port_t - a named port right
 *
 *	In user-space, "rights" are represented by the name of the
 *	right in the Mach port namespace.  Even so, this type is
 *	presented as a unique one to more clearly denote the presence
 *	of a right coming along with the name.
 *
 *	Often, various rights for a port held in a single name space
 *	will coalesce and are, therefore, be identified by a single name
 *	[this is the case for send and receive rights].  But not
 *	always [send-once rights currently get a unique name for
 *	each right].
 *
 *}
type
  mach_port_array_t = ^mach_port_t;

{*
 *  MACH_PORT_NULL is a legal value that can be carried in messages.
 *  It indicates the absence of any port or port rights.  (A port
 *  argument keeps the message from being "simple", even if the
 *  value is MACH_PORT_NULL.)  The value MACH_PORT_DEAD is also a legal
 *  value that can be carried in messages.  It indicates
 *  that a port right was present, but it died.
 *}

const
  MACH_PORT_NULL		= 0;   {* intentional loose typing *}
  MACH_PORT_DEAD		= mach_port_name_t($FFFFFFFF);
{  MACH_PORT_VALID(name)				\
		(((name) != MACH_PORT_NULL) && 		\
		 ((name) != MACH_PORT_DEAD))}


{*
 *  These are the different rights a task may have for a port.
 *  The MACH_PORT_RIGHT_* definitions are used as arguments
 *  to mach_port_allocate, mach_port_get_refs, etc, to specify
 *  a particular right to act upon.  The mach_port_names and
 *  mach_port_type calls return bitmasks using the MACH_PORT_TYPE_*
 *  definitions.  This is because a single name may denote
 *  multiple rights.
 *}

type
  mach_port_right_t =  natural_t;

const
  MACH_PORT_RIGHT_SEND		  = 0;
  MACH_PORT_RIGHT_RECEIVE		= 1;
  MACH_PORT_RIGHT_SEND_ONCE	= 2;
  MACH_PORT_RIGHT_PORT_SET	= 3;
  MACH_PORT_RIGHT_DEAD_NAME	= 4;
  MACH_PORT_RIGHT_LABELH	  = 5;
  MACH_PORT_RIGHT_NUMBER		= 6;

type
  mach_port_type_t        = natural_t;
  mach_port_type_array_t  = ^mach_port_type_t;

  pmach_port_type_t       = ^mach_port_type_t;
  pmach_port_type_array_t = ^mach_port_type_array_t;

{#define MACH_PORT_TYPE(right)						\
  (
     (mach_port_type_t)
       (  1 << ((right) + (16)) }

const
  MACH_PORT_TYPE_NONE	      = 0;
  MACH_PORT_TYPE_SEND	      = 1 shl (MACH_PORT_RIGHT_SEND+16);
  MACH_PORT_TYPE_RECEIVE	  = 1 shl (MACH_PORT_RIGHT_RECEIVE+16);
  MACH_PORT_TYPE_SEND_ONCE  = 1 shl (MACH_PORT_RIGHT_SEND_ONCE+16);
  MACH_PORT_TYPE_PORT_SET	  = 1 shl (MACH_PORT_RIGHT_PORT_SET+16);
  MACH_PORT_TYPE_DEAD_NAME  = 1 shl (MACH_PORT_RIGHT_DEAD_NAME+16);
  MACH_PORT_TYPE_LABELH     = 1 shl (MACH_PORT_RIGHT_LABELH+16);

{ Convenient combinations. }

  MACH_PORT_TYPE_SEND_RECEIVE =	(MACH_PORT_TYPE_SEND or MACH_PORT_TYPE_RECEIVE);
  MACH_PORT_TYPE_SEND_RIGHTS  =	(MACH_PORT_TYPE_SEND or MACH_PORT_TYPE_SEND_ONCE);
  MACH_PORT_TYPE_PORT_RIGHTS	=	(MACH_PORT_TYPE_SEND_RIGHTS or MACH_PORT_TYPE_RECEIVE);
  MACH_PORT_TYPE_PORT_OR_DEAD =	(MACH_PORT_TYPE_PORT_RIGHTS or MACH_PORT_TYPE_DEAD_NAME);
  MACH_PORT_TYPE_ALL_RIGHTS	  =	(MACH_PORT_TYPE_PORT_OR_DEAD or MACH_PORT_TYPE_PORT_SET);

// Dummy type bits that mach_port_type/mach_port_names can return.

  MACH_PORT_TYPE_DNREQUEST	= $80000000;

// User-references for capabilities.
type
  mach_port_urefs_t = natural_t;
  mach_port_delta_t = integer_t;			{ change in urefs }

  pmach_port_urefs_t = ^mach_port_urefs_t;

{ Attributes of ports.  (See mach_port_get_receive_status.) }

  mach_port_seqno_t = natural_t;		{ sequence number }
  mach_port_mscount_t = natural_t;		{ make-send count }
  mach_port_msgcount_t = natural_t;		{ number of msgs }
  mach_port_rights_t = natural_t;		{ number of rights }
  pmach_port_rights_t = ^mach_port_rights_t;

{*
 *	Are there outstanding send rights for a given port?
 *}
const
	MACH_PORT_SRIGHTS_NONE		= 0;		{ no srights }
  MACH_PORT_SRIGHTS_PRESENT	= 1;		{ srights }

type
  mach_port_srights_t = LongWord;	{ status of send rights }

type
  mach_port_status = record
  	mps_pset      : mach_port_rights_t;	{ count of containing port sets }
	  mps_seqno     : mach_port_seqno_t	;	{ sequence number }
  	mps_mscount   : mach_port_mscount_t;	{ make-send count }
  	mps_qlimit    : mach_port_msgcount_t;	{ queue limit }
  	mps_msgcount  : mach_port_msgcount_t;	{ number in the queue }
  	mps_sorights  : mach_port_rights_t;	{ how many send-once rights }
  	mps_srights   : boolean_t;	{ do send rights exist? }
  	mps_pdrequest : boolean_t;	{ port-deleted requested? }
  	mps_nsrequest : boolean_t;	{ no-senders requested? }
  	mps_flags     : natural_t;		{ port flags }
  end;
  mach_port_status_t = mach_port_status;

{ System-wide values for setting queue limits on a port }
const
  MACH_PORT_QLIMIT_ZERO		 = 0;
  MACH_PORT_QLIMIT_BASIC	 = 5;
  MACH_PORT_QLIMIT_SMALL	 = 16;
  MACH_PORT_QLIMIT_LARGE 	 = 1024;
  MACH_PORT_QLIMIT_MIN		 = MACH_PORT_QLIMIT_ZERO;
  MACH_PORT_QLIMIT_DEFAULT = MACH_PORT_QLIMIT_BASIC;
  MACH_PORT_QLIMIT_MAX		 = MACH_PORT_QLIMIT_LARGE;

type
  mach_port_limits = record
  	mpl_qlimit: mach_port_msgcount_t;	{ number of msgs }
  end;
  mach_port_limits_t = mach_port_limits;

  mach_port_info_t = ^integer_t;		{ varying array of natural_t }

{ Flavors for mach_port_get/set_attributes() }
  mach_port_flavor_t = integer;

const
  MACH_PORT_LIMITS_INFO		  = 1; { uses mach_port_status_t }
  MACH_PORT_RECEIVE_STATUS	= 2; { uses mach_port_limits_t }
  MACH_PORT_DNREQUESTS_SIZE	= 3; { info is int }

  MACH_PORT_LIMITS_INFO_COUNT	= sizeof(mach_port_limits_t) div sizeof(natural_t);
  MACH_PORT_RECEIVE_STATUS_COUNT = sizeof(mach_port_status_t) div sizeof(natural_t);
  MACH_PORT_DNREQUESTS_SIZE_COUNT = 1;

{*
 * Structure used to pass information about port allocation requests.
 * Must be padded to 64-bits total length.
 *}
type
  mach_port_qos = record
    //unsigned int		name:1;		{* name given *}
  	//unsigned int 		prealloc:1;	{* prealloced message *}
  	//boolean_t		pad1:30;
    flags : LongWord;
   	len   : natural_t;
  end;
  mach_port_qos_t = mach_port_qos;
  pmach_port_qos_t = ^mach_port_qos_t;

//#if	!__DARWIN_UNIX03 && !defined(_NO_PORT_T_FROM_MACH)
{*
 *  Mach 3.0 renamed everything to have mach_ in front of it.
 *  These types and macros are provided for backward compatibility
 *	but are deprecated.
 *}
type
  port_t = mach_port_t;
  port_name_t = mach_port_name_t	;
  port_name_array_t = ^mach_port_name_t;

const
  PORT_NULL	= 0;
  PORT_DEAD	= port_t($FFFFFFFF);
//#define PORT_VALID(name) \
//		((port_t)(name) != PORT_NULL && (port_t)(name) != PORT_DEAD)

{*******************************************************************************
 *	File:	mach/message.h                                                       *
 *                                                                             *
 *	Mach IPC message and primitive function definitions.                       *
 *******************************************************************************}


{*
 *  The timeout mechanism uses mach_msg_timeout_t values,
 *  passed by value.  The timeout units are milliseconds.
 *  It is controlled with the MACH_SEND_TIMEOUT
 *  and MACH_RCV_TIMEOUT options.
 *}

type
  mach_msg_timeout_t = natural_t ;

{*
 *  The value to be used when there is no timeout.
 *  (No MACH_SEND_TIMEOUT/MACH_RCV_TIMEOUT option.)
 *}
const
  MACH_MSG_TIMEOUT_NONE	= 0;

{*
 *  The kernel uses MACH_MSGH_BITS_COMPLEX as a hint.  It it isn't on, it
 *  assumes the body of the message doesn't contain port rights or OOL
 *  data.  The field is set in received messages.  A user task must
 *  use caution in interpreting the body of a message if the bit isn't
 *  on, because the mach_msg_type's in the body might "lie" about the
 *  contents.  If the bit isn't on, but the mach_msg_types
 *  in the body specify rights or OOL data, the behavior is undefined.
 *  (Ie, an error may or may not be produced.)
 *
 *  The value of MACH_MSGH_BITS_REMOTE determines the interpretation
 *  of the msgh_remote_port field.  It is handled like a msgt_name.
 *
 *  The value of MACH_MSGH_BITS_LOCAL determines the interpretation
 *  of the msgh_local_port field.  It is handled like a msgt_name.
 *
 *  MACH_MSGH_BITS() combines two MACH_MSG_TYPE_* values, for the remote
 *  and local fields, into a single value suitable for msgh_bits.
 *
 *  MACH_MSGH_BITS_CIRCULAR should be zero; is is used internally.
 *
 *  The unused bits should be zero and are reserved for the kernel
 *  or for future interface expansion.
 *}

const
  MACH_MSGH_BITS_ZERO	 	     = $00000000;
  MACH_MSGH_BITS_REMOTE_MASK = $000000ff;
  MACH_MSGH_BITS_LOCAL_MASK	 = $0000ff00;
  MACH_MSGH_BITS_COMPLEX		 = $80000000;
  MACH_MSGH_BITS_USER        = $8000ffff;

  MACH_MSGH_BITS_CIRCULAR	 	 = $40000000; { internal use only }
 	MACH_MSGH_BITS_USED		     = $c000ffff;

  MACH_MSGH_BITS_PORTS_MASK  = (MACH_MSGH_BITS_REMOTE_MASK or MACH_MSGH_BITS_LOCAL_MASK);

{#define MACH_MSGH_BITS(remote, local)				\
		((remote) | ((local) << 8))
#define	MACH_MSGH_BITS_REMOTE(bits)				\
		((bits) & MACH_MSGH_BITS_REMOTE_MASK)
#define	MACH_MSGH_BITS_LOCAL(bits)				\
		(((bits) & MACH_MSGH_BITS_LOCAL_MASK) >> 8)
#define	MACH_MSGH_BITS_PORTS(bits)				\
		((bits) & MACH_MSGH_BITS_PORTS_MASK)
#define	MACH_MSGH_BITS_OTHER(bits)				\
		((bits) &~ MACH_MSGH_BITS_PORTS_MASK)}

{*
 *  Every message starts with a message header.
 *  Following the message header are zero or more pairs of
 *  type descriptors (mach_msg_type_t/mach_msg_type_long_t) and
 *  data values.  The size of the message must be specified in bytes,
 *  and includes the message header, type descriptors, inline
 *  data, and inline pointer for out-of-line data.
 *
 *  The msgh_remote_port field specifies the destination of the message.
 *  It must specify a valid send or send-once right for a port.
 *
 *  The msgh_local_port field specifies a "reply port".  Normally,
 *  This field carries a send-once right that the receiver will use
 *  to reply to the message.  It may carry the values MACH_PORT_NULL,
 *  MACH_PORT_DEAD, a send-once right, or a send right.
 *
 *  The msgh_seqno field carries a sequence number associated with the
 *  received-from port.  A port's sequence number is incremented every
 *  time a message is received from it.  In sent messages, the field's
 *  value is ignored.
 *
 *  The msgh_id field is uninterpreted by the message primitives.
 *  It normally carries information specifying the format
 *  or meaning of the message.
 *}

type
  mach_msg_bits_t = LongWord;
  mach_msg_size_t =	natural_t;
  mach_msg_id_t   = integer_t ;

const
  MACH_MSG_SIZE_NULL = 0; // (mach_msg_size_t *) 0

type
  mach_msg_type_name_t = LongWord;
  pmach_msg_type_name_t = ^mach_msg_type_name_t;

const
  MACH_MSG_TYPE_MOVE_RECEIVE	 = 16; { Must hold receive rights }
  MACH_MSG_TYPE_MOVE_SEND		   = 17; { Must hold send rights }
  MACH_MSG_TYPE_MOVE_SEND_ONCE = 18; { Must hold sendonce rights }
  MACH_MSG_TYPE_COPY_SEND		   = 19; { Must hold send rights }
  MACH_MSG_TYPE_MAKE_SEND		   = 20; { Must hold receive rights }
  MACH_MSG_TYPE_MAKE_SEND_ONCE = 21; { Must hold receive rights }
  MACH_MSG_TYPE_COPY_RECEIVE	 = 22; { Must hold receive rights }

type
  mach_msg_copy_options_t = LongWord;

const
  MACH_MSG_PHYSICAL_COPY = 0;
  MACH_MSG_VIRTUAL_COPY = 1;
  MACH_MSG_ALLOCATE = 2;
  MACH_MSG_OVERWRITE_ = 3;
  MACH_MSG_KALLOC_COPY_T = 4;

type
  mach_msg_descriptor_type_t = LongWord;

const
  MACH_MSG_PORT_DESCRIPTOR = 0;
  MACH_MSG_OOL_DESCRIPTOR  = 1;
  MACH_MSG_OOL_PORTS_DESCRIPTOR =	2;
  MACH_MSG_OOL_VOLATILE_DESCRIPTOR = 3;

//#pragma pack(4), not packed records?

type
  mach_msg_type_descriptor_t = packed record
    pad1      : natural_t;
    pad2      : mach_msg_size_t;
    pad3_type : LongWord
  end;

  mach_msg_port_descriptor_t = packed record
    name        : mach_port_t;
    pad1        : mach_msg_size_t;
    pad2        : word;
    disposition : byte;
    _type       : byte;
  end;

  mach_msg_ool_descriptor32_t = packed record
    address     : uint32_t;
    size        : mach_msg_size_t;
    deallocate  : boolean;
    copy        : byte; // as mach_msg_copy_options_t
    pad1        : byte;
    _type       : byte; // as mach_msg_descriptor_type_t
  end;

  mach_msg_ool_descriptor64_t = packed record
    address     : uint64_t;
    deallocate  : boolean;
    copy        : byte; // as mach_msg_copy_options_t
    pad1        : byte;
    _type       : byte; // as mach_msg_descriptor_type_t
    size        : mach_msg_size_t;
  end;

  {$ifdef CPU64}
  mach_msg_ool_descriptor_t = mach_msg_ool_descriptor64_t;
  {$else}
  mach_msg_ool_descriptor_t = mach_msg_ool_descriptor32_t;
  {$endif}

  mach_msg_ool_ports_descriptor32_t = packed record
    address     : uint32_t;
    count       : mach_msg_size_t;
    deallocate  : boolean;
    copy        : byte; // as mach_msg_copy_options_t
    disposition : byte; // as mach_msg_type_name_t
    _type       : byte; // as mach_msg_descriptor_type_t
  end;

  mach_msg_ool_ports_descriptor64_t = packed record
    address     : uint64_t;
    deallocate  : boolean;
    copy        : byte; // as mach_msg_copy_options_t
    disposition : byte; // as mach_msg_type_name_t
    _type       : byte; // as mach_msg_descriptor_type_t
    count       : mach_msg_size_t;
  end;

  {$ifdef CPU64}
  mach_msg_ool_ports_descriptor_t=mach_msg_ool_ports_descriptor64_t;
  {$else}
  mach_msg_ool_ports_descriptor_t=mach_msg_ool_ports_descriptor32_t;
  {$endif}

{*
 * LP64support - This union definition is not really
 * appropriate in LP64 mode because not all descriptors
 * are of the same size in that environment.
 *}
  mach_msg_descriptor_t = packed record
  case byte of
    0: (port        : mach_msg_port_descriptor_t);
    1: (out_of_line : mach_msg_ool_descriptor_t);
    2: (ool_ports   : mach_msg_ool_ports_descriptor_t);
    3: (_type       : mach_msg_type_descriptor_t);
  end;

  mach_msg_body_t = packed record
    msgh_descriptor_count : mach_msg_size_t;
  end;

const
  MACH_MSG_BODY_NULL       = nil;
  MACH_MSG_DESCRIPTOR_NULL = nil;

type
  mach_msg_header_t = packed record
    msgh_bits        : mach_msg_bits_t;
    msgh_size        : mach_msg_size_t;
    msgh_remote_port : mach_port_t;
    msgh_local_port  : mach_port_t;
    msgh_reserved    : mach_msg_size_t;
    msgh_id          : mach_msg_id_t;
  end;
  pmach_msg_header_t = ^mach_msg_header_t;

const
  MACH_MSG_NULL = nil;

type
  mach_msg_base_t = packed record
    header  : mach_msg_header_t;
    body    : mach_msg_body_t;
  end;
  pmach_msg_base_t = ^mach_msg_base_t;

  mach_msg_trailer_type_t = LongWord;

const
  MACH_MSG_TRAILER_FORMAT_0	= 0;

type
  mach_msg_trailer_size_t = LongWord;

  mach_msg_trailer_t = packed record
    msgh_trailer_type : mach_msg_trailer_type_t;
    msgh_trailer_size : mach_msg_trailer_size_t;
  end;

  mach_msg_seqno_trailer_t = packed record
    msgh_trailer_type : mach_msg_trailer_type_t;
    msgh_trailer_size : mach_msg_trailer_size_t;
    msgh_seqno        : mach_port_seqno_t;
  end;

  security_token_t = packed record
    val : array [0..1] of LongWord;
  end;

  mach_msg_security_trailer_t = packed record
    msgh_trailer_type : mach_msg_trailer_type_t;
    msgh_trailer_size : mach_msg_trailer_size_t;
    msgh_seqno        : mach_port_seqno_t;
    msgh_sender       : security_token_t;
  end;

{*
 * The audit token is an opaque token which identifies
 * Mach tasks and senders of Mach messages as subjects
 * to the BSM audit system.  Only the appropriate BSM
 * library routines should be used to interpret the
 * contents of the audit token as the representation
 * of the subject identity within the token may change
 * over time.
 *}
  audit_token_t = packed record
    val   : array [0..7] of LongWord;
  end;

  mach_msg_audit_trailer_t = packed record
    msgh_trailer_type : mach_msg_trailer_type_t;
    msgh_trailer_size : mach_msg_trailer_size_t;
    msgh_seqno  : mach_port_seqno_t;
    msgh_sender : security_token_t;
    msgh_audit  : audit_token_t;
  end;

  msg_labels_t = packed record
    sender : mach_port_name_t;
  end;

{* Trailer type to pass MAC policy label info as a mach message trailer. *}

  mach_msg_mac_trailer_t = packed record
    msgh_trailer_type : mach_msg_trailer_type_t;
    msgh_trailer_size : mach_msg_trailer_size_t;
    msgh_seqno    : mach_port_seqno_t;
    msgh_sender   : security_token_t;
    msgh_audit    : audit_token_t;
    msgh_labels   : msg_labels_t;
    msgh_ad       : integer;
  end;

const
  MACH_MSG_TRAILER_MINIMUM_SIZE = sizeof(mach_msg_trailer_t);

{*
 * These values can change from release to release - but clearly
 * code cannot request additional trailer elements one was not
 * compiled to understand.  Therefore, it is safe to use this
 * constant when the same module specified the receive options.
 * Otherwise, you run the risk that the options requested by
 * another module may exceed the local modules notion of
 * MAX_TRAILER_SIZE.
 *}
type
  mach_msg_max_trailer_t = mach_msg_mac_trailer_t;

const
  MAX_TRAILER_SIZE = sizeof(mach_msg_max_trailer_t);

{*
 * Legacy requirements keep us from ever updating these defines (even
 * when the format_0 trailers gain new option data fields in the future).
 * Therefore, they shouldn't be used going forward.  Instead, the sizes
 * should be compared against the specific element size requested using
 * REQUESTED_TRAILER_SIZE.
 *}
type
  mach_msg_format_0_trailer_t = mach_msg_security_trailer_t;

{*typedef mach_msg_mac_trailer_t mach_msg_format_0_trailer_t; *}

const
  MACH_MSG_TRAILER_FORMAT_0_SIZE = sizeof(mach_msg_format_0_trailer_t);

(*
#define   KERNEL_SECURITY_TOKEN_VALUE  { {0, 1} }
extern security_token_t KERNEL_SECURITY_TOKEN;

#define   KERNEL_AUDIT_TOKEN_VALUE  { {0, 0, 0, 0, 0, 0, 0, 0} }
extern audit_token_t KERNEL_AUDIT_TOKEN;
*)

type
  mach_msg_options_t = integer_t;

  mach_msg_empty_send_t = packed record
    header : mach_msg_header_t;
  end;

  mach_msg_empty_rcv_t = packed record
    header  : mach_msg_header_t;
    trailer : mach_msg_trailer_t;
   end;

  mach_msg_empty_t = packed record
  case byte of
    0: (send : mach_msg_empty_send_t);
    1: (rcv: mach_msg_empty_rcv_t);
  end;

//#pragma pack()

{* utility to round the message size - will become machine dependent *}

//#define round_msg(x)	(((mach_msg_size_t)(x) + sizeof (natural_t) - 1) & \
//				~(sizeof (natural_t) - 1))

{*
 *  There is no fixed upper bound to the size of Mach messages.
 *}

//#define	MACH_MSG_SIZE_MAX	((mach_msg_size_t) ~0)

{*
 *  Compatibility definitions, for code written
 *  when there was a msgh_kind instead of msgh_seqno.
 *}

const
  MACH_MSGH_KIND_NORMAL	 	    = $00000000;
  MACH_MSGH_KIND_NOTIFICATION	= $00000001;
//#define	msgh_kind			msgh_seqno
//#define mach_msg_kind_t			mach_port_seqno_t

{*
 *  The msgt_number field specifies the number of data elements.
 *  The msgt_size field specifies the size of each data element, in bits.
 *  The msgt_name field specifies the type of each data element.
 *  If msgt_inline is TRUE, the data follows the type descriptor
 *  in the body of the message.  If msgt_inline is FALSE, then a pointer
 *  to the data should follow the type descriptor, and the data is
 *  sent out-of-line.  In this case, if msgt_deallocate is TRUE,
 *  then the out-of-line data is moved (instead of copied) into the message.
 *  If msgt_longform is TRUE, then the type descriptor is actually
 *  a mach_msg_type_long_t.
 *
 *  The actual amount of inline data following the descriptor must
 *  a multiple of the word size.  For out-of-line data, this is a
 *  pointer.  For inline data, the supplied data size (calculated
 *  from msgt_number/msgt_size) is rounded up.  This guarantees
 *  that type descriptors always fall on word boundaries.
 *
 *  For port rights, msgt_size must be 8*sizeof(mach_port_t).
 *  If the data is inline, msgt_deallocate should be FALSE.
 *  The msgt_unused bit should be zero.
 *  The msgt_name, msgt_size, msgt_number fields in
 *  a mach_msg_type_long_t should be zero.
 *}

type
  mach_msg_type_size_t  = natural_t;
  pmach_msg_type_size_t = ^mach_msg_type_size_t;

{*
 *  Values received/carried in messages.  Tells the receiver what
 *  sort of port right he now has.
 *
 *  MACH_MSG_TYPE_PORT_NAME is used to transfer a port name
 *  which should remain uninterpreted by the kernel.  (Port rights
 *  are not transferred, just the port name.)
 *}

const
  MACH_MSG_TYPE_PORT_NONE		   = 0;
  MACH_MSG_TYPE_PORT_NAME	     = 15;
  MACH_MSG_TYPE_PORT_RECEIVE   = MACH_MSG_TYPE_MOVE_RECEIVE;
  MACH_MSG_TYPE_PORT_SEND		   = MACH_MSG_TYPE_MOVE_SEND;
  MACH_MSG_TYPE_PORT_SEND_ONCE = MACH_MSG_TYPE_MOVE_SEND_ONCE;

  MACH_MSG_TYPE_LAST		=  22;		{* Last assigned *}

{*  A dummy value.  Mostly used to indicate that the actual value
 *  will be filled in later, dynamically.
 *}

  MACH_MSG_TYPE_POLYMORPHIC	= -1;

{*
 *	Is a given item a port type?
 *}
{
#define MACH_MSG_TYPE_PORT_ANY(x)			\
	(((x) >= MACH_MSG_TYPE_MOVE_RECEIVE) &&		\
	 ((x) <= MACH_MSG_TYPE_MAKE_SEND_ONCE))

#define	MACH_MSG_TYPE_PORT_ANY_SEND(x)			\
	(((x) >= MACH_MSG_TYPE_MOVE_SEND) &&		\
	 ((x) <= MACH_MSG_TYPE_MAKE_SEND_ONCE))

#define	MACH_MSG_TYPE_PORT_ANY_RIGHT(x)			\
	(((x) >= MACH_MSG_TYPE_MOVE_RECEIVE) &&		\
	 ((x) <= MACH_MSG_TYPE_MOVE_SEND_ONCE))
}

type
  mach_msg_option_t = integer_t;

const
  MACH_MSG_OPTION_NONE = $00000000;

  MACH_SEND_MSG		     = $00000001;
  MACH_RCV_MSG		     = $00000002;
  MACH_RCV_LARGE	 	   = $00000004;

  MACH_SEND_TIMEOUT  	 = $00000010;
  MACH_SEND_INTERRUPT	 = $00000040;	{* libmach implements *}
  MACH_SEND_CANCEL	   = $00000080;
  MACH_SEND_ALWAYS	   = $00010000;	{* internal use only *}
  MACH_SEND_TRAILER  	 = $00020000;

  MACH_RCV_TIMEOUT	   = $00000100;
  MACH_RCV_NOTIFY		   = $00000200;
  MACH_RCV_INTERRUPT	 = $00000400;	{* libmach implements *}
  MACH_RCV_OVERWRITE	 = $00001000;

{*
 * NOTE: a 0x00------ RCV mask implies to ask for
 * a MACH_MSG_TRAILER_FORMAT_0 with 0 Elements,
 * which is equivalent to a mach_msg_trailer_t.
 *
 * XXXMAC: unlike the rest of the MACH_RCV_* flags, MACH_RCV_TRAILER_LABELS
 * and MACH_RCV_TRAILER_AV need their own private bit since we only calculate
 * their fields when absolutely required.  This will cause us problems if
 * Apple adds new trailers.
 *}
  MACH_RCV_TRAILER_NULL   = 0;
  MACH_RCV_TRAILER_SEQNO  = 1;
  MACH_RCV_TRAILER_SENDER = 2;
  MACH_RCV_TRAILER_AUDIT  = 3;
  MACH_RCV_TRAILER_LABELS = 4;
  MACH_RCV_TRAILER_AV     = 8;

//#define MACH_RCV_TRAILER_TYPE(x)     (((x) & 0xf) << 28)
//#define MACH_RCV_TRAILER_ELEMENTS(x) (((x) & 0xf) << 24)
  MACH_RCV_TRAILER_MASK 	= $ff shl 24;

 // #define GET_RCV_ELEMENTS(y) (((y) >> 24) & 0xf)

{*
 * XXXMAC: note that in the case of MACH_RCV_TRAILER_AV and
 * MACH_RCV_TRAILER_LABELS, we just fall through to mach_msg_max_trailer_t.
 * This is correct behavior since mach_msg_max_trailer_t is defined as
 * mac_msg_mac_trailer_t which is used for the LABELS and AV trailers.
 * It also makes things work properly if MACH_RCV_TRAILER_AV or
 * MACH_RCV_TRAILER_LABELS are ORed with one of the other options.
 *}
{
#define REQUESTED_TRAILER_SIZE(y) 				\
	((mach_msg_trailer_size_t)				\
	 ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_NULL) ?	\
	  sizeof(mach_msg_trailer_t) :				\
	  ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_SEQNO) ?	\
	   sizeof(mach_msg_seqno_trailer_t) :			\
	  ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_SENDER) ?	\
	   sizeof(mach_msg_security_trailer_t) :		\
	   ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_AUDIT) ?	\
	    sizeof(mach_msg_audit_trailer_t) :      		\
	    sizeof(mach_msg_max_trailer_t))))))
}

{*
 *  Much code assumes that mach_msg_return_t == kern_return_t.
 *  This definition is useful for descriptive purposes.
 *
 *  See <mach/error.h> for the format of error codes.
 *  IPC errors are system 4.  Send errors are subsystem 0;
 *  receive errors are subsystem 1.  The code field is always non-zero.
 *  The high bits of the code field communicate extra information
 *  for some error codes.  MACH_MSG_MASK masks off these special bits.
 *}

type
  mach_msg_return_t = kern_return_t;

const
  MACH_MSG_SUCCESS	= $00000000;
  MACH_MSG_MASK			= $00003e00;

	{* All special error code bits defined below. *}

  MACH_MSG_IPC_SPACE		= $00002000;	{* No room in IPC name space for another capability name. *}
  MACH_MSG_VM_SPACE		  = $00001000; 		{* No room in VM address space for out-of-line memory. *}
  MACH_MSG_IPC_KERNEL		= $00000800;	{* Kernel resource shortage handling an IPC capability. *}
  MACH_MSG_VM_KERNEL		= $00000400;	{* Kernel resource shortage handling out-of-line memory. *}

  MACH_SEND_IN_PROGRESS		= $10000001;	{* Thread is waiting to send.  (Internal use only.) *}
  MACH_SEND_INVALID_DATA	= $10000002;	{* Bogus in-line data. *}
  MACH_SEND_INVALID_DEST	= $10000003;	{* Bogus destination port. *}
  MACH_SEND_TIMED_OUT		  = $10000004;  {* Message not sent before timeout expired. *}
  MACH_SEND_INTERRUPTED		= $10000007; 	{* Software interrupt. *}
  MACH_SEND_MSG_TOO_SMALL	=	$10000008;	{* Data doesn't contain a complete message. *}
  MACH_SEND_INVALID_REPLY	= $10000009;	{* Bogus reply port. *}
  MACH_SEND_INVALID_RIGHT	= $1000000a;	{* Bogus port rights in the message body. *}
  MACH_SEND_INVALID_NOTIFY = $1000000b;	{* Bogus notify port argument. *}
  MACH_SEND_INVALID_MEMORY = $1000000c;	{* Invalid out-of-line memory pointer. *}
  MACH_SEND_NO_BUFFER		   = $1000000d;   		{* No message buffer is available. *}
  MACH_SEND_TOO_LARGE		   = $1000000e;   		{* Send is too large for port *}
  MACH_SEND_INVALID_TYPE	  = $1000000f;	{* Invalid msg-type specification. *}
  MACH_SEND_INVALID_HEADER	= $10000010;	{* A field in the header had a bad value. *}
  MACH_SEND_INVALID_TRAILER	= $10000011;	{* The trailer to be sent does not match kernel format. *}
  MACH_SEND_INVALID_RT_OOL_SIZE	= $10000015;	{* compatibility: no longer a returned error *}

  MACH_RCV_IN_PROGRESS	= $10004001;	  {* Thread is waiting for receive.  (Internal use only.) *}
  MACH_RCV_INVALID_NAME	= $10004002;	  {* Bogus name for receive port/port-set. *}
  MACH_RCV_TIMED_OUT		= $10004003; 		{* Didn't get a message within the timeout value. *}
  MACH_RCV_TOO_LARGE		= $10004004;	  {* Message buffer is not large enough for inline data. *}
  MACH_RCV_INTERRUPTED	= $10004005;	  {* Software interrupt. *}
  MACH_RCV_PORT_CHANGED	= $10004006;	    {* compatibility: no longer a returned error *}
  MACH_RCV_INVALID_NOTIFY	= $10004007;	  {* Bogus notify port argument. *}
  MACH_RCV_INVALID_DATA		= $10004008;	  {* Bogus message buffer for inline data. *}
  MACH_RCV_PORT_DIED		  = $10004009;	  {* Port/set was sent away/died during receive. *}
  MACH_RCV_IN_SET			    = $1000400a; 		{* compatibility: no longer a returned error *}
  MACH_RCV_HEADER_ERROR		= $1000400b;	  {* Error receiving message header.  See special bits. *}
  MACH_RCV_BODY_ERROR		  = $1000400c; 		{* Error receiving message body.  See special bits. *}
  MACH_RCV_INVALID_TYPE		= $1000400d;	  {* Invalid msg-type specification in scatter list. *}
  MACH_RCV_SCATTER_SMALL	= $1000400e;	  {* Out-of-line overwrite region is not large enough *}
  MACH_RCV_INVALID_TRAILER  = $1000400f;	{* trailer type or number of trailer elements not supported *}
  MACH_RCV_IN_PROGRESS_TIMED = $10004011; {* Waiting for receive with timeout. (Internal use only.) *}



{*
 *	Routine:	mach_msg_overwrite
 *	Purpose:
 *		Send and/or receive a message.  If the message operation
 *		is interrupted, and the user did not request an indication
 *		of that fact, then restart the appropriate parts of the
 *		operation silently (trap version does not restart).
 *
 *		Distinct send and receive buffers may be specified.  If
 *		no separate receive buffer is specified, the msg parameter
 *		will be used for both send and receive operations.
 *
 *		In addition to a distinct receive buffer, that buffer may
 *		already contain scatter control information to direct the
 *		receiving of the message.
 *}

function mach_msg_overwrite( msg: pmach_msg_header_t; option: mach_msg_option_t;
  send_size: mach_msg_size_t;	rcv_size: mach_msg_size_t; rcv_name: mach_port_name_t;
	rcv_limit: mach_msg_size_t): mach_msg_return_t; cdecl; external;

{*
 *	Routine:	mach_msg
 *	Purpose:
 *		Send and/or receive a message.  If the message operation
 *		is interrupted, and the user did not request an indication
 *		of that fact, then restart the appropriate parts of the
 *		operation silently (trap version does not restart).
 *}
function mach_msg(
  msg         : pmach_msg_header_t;
  option      : mach_msg_option_t;
  send_size   : mach_msg_size_t;
  rcv_size    : mach_msg_size_t;
  rcv_name    : mach_port_name_t;
	timeout     : mach_msg_timeout_t;
  notify      : mach_port_name_t): mach_msg_return_t; cdecl; external;

// ----- policy.h --------------------------------------------------------------

// *	Definitions for scheduing policy.

{*
 *  All interfaces defined here are obsolete.
 *}
type
  policy_t = integer;
  policy_info_t = ^integer_t;
  policy_base_t = ^integer_t;
  policy_limit_t = ^integer_t;

{*
 *	Policy definitions.  Policies should be powers of 2,
 *	but cannot be or'd together other than to test for a
 *	policy 'class'.
 *}
const
	POLICY_NULL		   = 0;	{* none			*}
 	POLICY_TIMESHARE = 1;	{* timesharing		*}
	POLICY_RR		  = 2;	{* fixed round robin	*}
  POLICY_FIFO		= 4;	{* fixed fifo		*}

  POLICYCLASS_FIXEDPRI=	(POLICY_RR or POLICY_FIFO) ;
{
 *	Check if policy is valid.

#define invalid_policy(policy)			\
	((policy) != POLICY_TIMESHARE &&	\
	 (policy) != POLICY_RR &&		\
	 (policy) != POLICY_FIFO)
}

{*
 * 	Types for TIMESHARE policy
 *}
type
  policy_timeshare_base = record
  	base_priority: integer_t		;
  end;

  policy_timeshare_limit = record
  	max_priority : integer_t;
  end;

  policy_timeshare_info = record
  	max_priority     : integer_t;
	  base_priority    : integer_t;
	  cur_priority     : integer_t;
  	depressed        : boolean_t;
  	depress_priority : integer_t;
  end;

  policy_timeshare_base_t = ^policy_timeshare_base;
  policy_timeshare_limit_t = ^policy_timeshare_limit;
  policy_timeshare_info_t = ^policy_timeshare_info;

  policy_timeshare_base_data_t = policy_timeshare_base;
  policy_timeshare_limit_data_t = policy_timeshare_limit;
  policy_timeshare_info_data_t = policy_timeshare_info;

const
  POLICY_TIMESHARE_BASE_COUNT  = sizeof(policy_timeshare_base) div sizeof(integer_t);
  POLICY_TIMESHARE_LIMIT_COUNT = sizeof(policy_timeshare_limit) div sizeof(integer_t);
  POLICY_TIMESHARE_INFO_COUNT	 = sizeof(policy_timeshare_info) div sizeof(integer_t);

{*
 *	Types for the ROUND ROBIN (RR) policy
 *}

type
  policy_rr_base = record
  	base_priority : integer_t;
    quantum       :	integer_t;
  end;

  policy_rr_limit = record
  	max_priority  : integer_t;
  end;

  policy_rr_info = record
  	max_priority  : integer_t;
	  base_priority : integer_t;
  	quantum       : integer_t;
	  depressed     : boolean_t;
	  depress_priority : integer_t;
  end;

  policy_rr_base_t = ^policy_rr_base;
  policy_rr_limit_t = ^policy_rr_limit;
  policy_rr_info_t = ^policy_rr_info;

  policy_rr_base_data_t = policy_rr_base;
  policy_rr_limit_data_t = policy_rr_limit;
  policy_rr_info_data_t = policy_rr_info;

const
  POLICY_RR_BASE_COUNT = sizeof(policy_rr_base) div sizeof(integer_t);
  POLICY_RR_LIMIT_COUNT	= sizeof(policy_rr_limit) div sizeof(integer_t);
  POLICY_RR_INFO_COUNT= sizeof(policy_rr_info) div sizeof(integer_t);

{*
 * 	Types for the FIRST-IN-FIRST-OUT (FIFO) policy
 *}

type
  policy_fifo_base = record
  	base_priority: integer_t;
  end;

  policy_fifo_limit = record
	  max_priority : integer_t;
  end;

  policy_fifo_info = record
    max_priority  : integer_t;
 	  base_priority : integer_t;
    depressed     : boolean_t;
 	  depress_priority  : integer_t;
  end;

  policy_fifo_base_t = ^policy_fifo_base;
  policy_fifo_limit_t = ^policy_fifo_limit;
  policy_fifo_info_t = ^policy_fifo_info;

  policy_fifo_base_data_t = policy_fifo_base;
  policy_fifo_limit_data_t = policy_fifo_limit	;
  policy_fifo_info_data_t = policy_fifo_info;

const
	POLICY_FIFO_BASE_COUNT = sizeof(policy_fifo_base) div sizeof(integer_t);
  POLICY_FIFO_LIMIT_COUNT	= sizeof(policy_fifo_limit) div sizeof(integer_t);
  POLICY_FIFO_INFO_COUNT = sizeof(policy_fifo_info) div sizeof(integer_t);


{*
 * 	Aggregate policy types
 *}

type
  policy_bases = record
  	ts : policy_timeshare_base_data_t;
   	rr : policy_rr_base_data_t;
  	fifo : policy_fifo_base_data_t;
  end;

  policy_limits = record
  	ts: policy_timeshare_limit_data_t;
	  rr: policy_rr_limit_data_t;
  	fifo: policy_fifo_limit_data_t;
  end;

  policy_infos = record
  	ts: policy_timeshare_info_data_t;
  	rr: policy_rr_info_data_t;
    fifo: policy_fifo_info_data_t;
  end;

  policy_base_data_t = policy_bases;
  policy_limit_data_t = policy_limits;
  policy_info_data_t = policy_infos;


// thread_policy.h

{*
 * These are the calls for accessing the policy parameters
 * of a particular thread.
 *
 * The extra 'get_default' parameter to the second call is
 * IN/OUT as follows:
 * 1) if asserted on the way in it indicates that the default
 * values should be returned, not the ones currently set, in
 * this case 'get_default' will always be asserted on return;
 * 2) if unasserted on the way in, the current settings are
 * desired and if still unasserted on return, then the info
 * returned reflects the current settings, otherwise if
 * 'get_default' returns asserted, it means that there are no
 * current settings due to other parameters taking precedence,
 * and the default ones are being returned instead.
 *}

type
  thread_policy_flavor_t = natural_t;
  thread_policy_t = ^integer_t;

{*
 * Defined flavors.
 *}
{*
 * THREAD_STANDARD_POLICY:
 *
 * This is the standard (fair) scheduling mode, assigned to new
 * threads.  The thread will be given processor time in a manner
 * which apportions approximately equal share to long running
 * computations.
 *
 * Parameters:
 *	[none]
 *}

const
  THREAD_STANDARD_POLICY		= 1;

type
  _thread_standard_policy = record
  	no_data: natural_t;
  end;

  thread_standard_policy_data_t = _thread_standard_policy;
  thread_standard_policy_t = ^_thread_standard_policy;

const
  THREAD_STANDARD_POLICY_COUNT	= 0;

{*
 * THREAD_EXTENDED_POLICY:
 *
 * Extended form of THREAD_STANDARD_POLICY, which supplies a
 * hint indicating whether this is a long running computation.
 *
 * Parameters:
 *
 * timeshare: TRUE (the default) results in identical scheduling
 * behavior as THREAD_STANDARD_POLICY.
 *}

const
  THREAD_EXTENDED_POLICY			= 1;

type
  _thread_extended_policy = record
  	timeshare: boolean_t;
  end;

  thread_extended_policy_data_t = _thread_extended_policy;
  thread_extended_policy_t = ^_thread_extended_policy;

const
  THREAD_EXTENDED_POLICY_COUNT = sizeof (thread_extended_policy_data_t) div sizeof (integer_t);

{*
 * THREAD_TIME_CONSTRAINT_POLICY:
 *
 * This scheduling mode is for threads which have real time
 * constraints on their execution.
 *
 * Parameters:
 *
 * period: This is the nominal amount of time between separate
 * processing arrivals, specified in absolute time units.  A
 * value of 0 indicates that there is no inherent periodicity in
 * the computation.
 *
 * computation: This is the nominal amount of computation
 * time needed during a separate processing arrival, specified
 * in absolute time units.
 *
 * constraint: This is the maximum amount of real time that
 * may elapse from the start of a separate processing arrival
 * to the end of computation for logically correct functioning,
 * specified in absolute time units.  Must be (>= computation).
 * Note that latency = (constraint - computation).
 *
 * preemptible: This indicates that the computation may be
 * interrupted, subject to the constraint specified above.
 *}

const
  THREAD_TIME_CONSTRAINT_POLICY	= 2;

type
  _thread_time_constraint_policy = record
  	period      : uint32_t;
  	computation : uint32_t;
   	constraint  : uint32_t;
  	preemptible : boolean_t;
  end;

  thread_time_constraint_policy_data_t = _thread_time_constraint_policy;
  thread_time_constraint_policy_t = ^_thread_time_constraint_policy;

const
  THREAD_TIME_CONSTRAINT_POLICY_COUNT =
	  sizeof (thread_time_constraint_policy_data_t) div sizeof (integer_t);

{*
 * THREAD_PRECEDENCE_POLICY:
 *
 * This may be used to indicate the relative value of the
 * computation compared to the other threads in the task.
 *
 * Parameters:
 *
 * importance: The importance is specified as a signed value.
 *}

const
 THREAD_PRECEDENCE_POLICY		= 3;

type
  _thread_precedence_policy = record
  	importance : integer_t;
  end;

  thread_precedence_policy_data_t = _thread_precedence_policy;
  thread_precedence_policy_t = ^_thread_precedence_policy;

const
  THREAD_PRECEDENCE_POLICY_COUNT =sizeof (thread_precedence_policy_data_t) div sizeof (integer_t);

{*
 * THREAD_AFFINITY_POLICY:
 *
 * This policy is experimental.
 * This may be used to express affinity relationships
 * between threads in the task. Threads with the same affinity tag will
 * be scheduled to share an L2 cache if possible. That is, affinity tags
 * are a hint to the scheduler for thread placement.
 *
 * The namespace of affinity tags is generally local to one task. However,
 * a child task created after the assignment of affinity tags by its parent
 * will share that namespace. In particular, a family of forked processes
 * may be created with a shared affinity namespace.
 *
 * Parameters:
 * tag: The affinity set identifier.
 *}

const
  THREAD_AFFINITY_POLICY		= 4;

type
  _thread_affinity_policy = record
  	affinity_tag: integer_t	;
  end;

type
  thread_affinity_policy_data_t = _thread_affinity_policy;
  thread_affinity_policy_t = ^_thread_affinity_policy;

const
  THREAD_AFFINITY_TAG_NULL		= 0;
  THREAD_AFFINITY_POLICY_COUNT = sizeof (thread_affinity_policy_data_t) div sizeof (integer_t);

// task_info.h

{
 *	Generic information structure to allow for expansion.
 *}

type
  task_flavor_t  = natural_t;
  task_info_t    = ^integer_t;		// varying array of int

const
  TASK_INFO_MAX	   = 1024;		// maximum array size
type
  task_info_data_t = array [0..TASK_INFO_MAX-1] of Integer;

{Currently defined information structures.}

//#pragma pack(4)
const
  TASK_BASIC_INFO_32     = 4;      // basic information
const
  TASK_BASIC2_INFO_32    = 6;

type
  _task_basic_info_32 = record
    suspend_count  : integer_t;    // suspend count for task
    virtual_size   : natural_t;    // virtual memory size (bytes)
    resident_size  : natural_t;    // resident memory size (bytes)
    user_time      : time_value_t; // total user run time for terminated threads
    system_time    : time_value_t; // total system run time for terminated threads
	  policy         : policy_t;		 // default policy for new threads *}
  end;
  task_basic_info_32_data_t = _task_basic_info_32;
  task_basic_info_32_t = ^_task_basic_info_32;

const
  TASK_BASIC_INFO_32_COUNT  = sizeoF(task_basic_info_32_data_t) div sizeof(natural_t);


const
  TASK_BASIC_INFO_64      = 5;      // 64-bit capable basic info
type
  _task_basic_info_64 = record
    suspend_count  : integer_t;       // suspend count for task
    virtual_size   : mach_vm_size_t;  // virtual memory size (bytes)
    resident_size  : mach_vm_size_t;  // resident memory size (bytes)
    user_time      : time_value_t;    // total user run time for terminated threads
    system_time    : time_value_t;    // total system run time for terminated threads
   	policy         : policy_t;		    // default policy for new threads
  end;
  task_basic_info_64_data_t = _task_basic_info_64;
  task_basic_info_64_t = ^_task_basic_info_64;
const
  TASK_BASIC_INFO_64_COUNT = sizeof(task_basic_info_64_data_t) div sizeof(natural_t);


// localized structure - cannot be safely passed between tasks of differing sizes

type
  task_basic_info = record
    suspend_count : integer_t;    { suspend count for task }
    virtual_size  : vm_size_t;    { virtual memory size (bytes) }
    resident_size : vm_size_t;    { resident memory size (bytes) }
    user_time     : time_value_t; { total user run time for terminated threads }
    system_time   : time_value_t; { total system run time for terminated threads }
	  policy        : policy_t; 		{ default policy for new threads }
  end;
  task_basic_info_data_t = task_basic_info;
  task_basic_info_t = ^task_basic_info;
const
  TASK_BASIC_INFO_COUNT =(sizeof(task_basic_info_data_t) div sizeof(natural_t));

{#if !defined(__LP64__)
#define TASK_BASIC_INFO TASK_BASIC_INFO_32
#else
#define TASK_BASIC_INFO TASK_BASIC_INFO_64
#endif}

const
	TASK_EVENTS_INFO	= 2;	{ various event counts }
type
  _task_events_info = record
  	faults            : integer_t;	{ number of page faults }
	  pageins           : integer_t;	{ number of actual pageins }
	  cow_faults        : integer_t;	{ number of copy-on-write faults }
	  messages_sent     : integer_t;	{ number of messages sent }
	  messages_received : integer_t;  { number of messages received }
    syscalls_mach     : integer_t;  { number of mach system calls }
	  syscalls_unix     : integer_t;  { number of unix system calls }
	  csw               : integer_t;  { number of context switches }
  end;
  task_events_info_data_t = _task_events_info;
  task_events_info_t  = ^_task_events_info;
const
  TASK_EVENTS_INFO_COUNT =	(sizeof(task_events_info_data_t) div sizeof(natural_t));

const
	TASK_THREAD_TIMES_INFO	= 3;	{ total times for live threads - only accurate if suspended }

type
  _task_thread_times_info = record
  	user_time   : time_value_t;	{ total user run time for live threads }
	  system_time : time_value_t;	{ total system run time for live threads }
  end;

  task_thread_times_info_data_t = _task_thread_times_info;
  task_thread_times_info_t = ^_task_thread_times_info;
const
  TASK_THREAD_TIMES_INFO_COUNT=(sizeof(task_thread_times_info_data_t) div sizeof(natural_t));


const
  TASK_ABSOLUTETIME_INFO	= 1;
type
  _task_absolutetime_info = record
  	total_user      : uint64_t;		{ total time }
  	total_system    : uint64_t;
  	threads_user    : uint64_t;{ existing threads only }
  	threads_system  : uint64_t;
  end;

  task_absolutetime_info_data_t = _task_absolutetime_info;
  task_absolutetime_info_t = ^_task_absolutetime_info;
const
  TASK_ABSOLUTETIME_INFO_COUNT = (sizeof (task_absolutetime_info_data_t) div sizeof (natural_t));


{const
  TASK_SECURITY_TOKEN	      =	13;
  TASK_SECURITY_TOKEN_COUNT	= (sizeof(security_token_t) div sizeof(natural_t));}


{const
  TASK_AUDIT_TOKEN		    = 15;
  TASK_AUDIT_TOKEN_COUNT	= (sizeof(audit_token_t) / sizeof(natural_t));}


const
  TASK_AFFINITY_TAG_INFO	= 16;	{ This is experimental. }

type
  _task_affinity_tag_info = record
  	set_count   : integer_t;
  	min         : integer_t;
  	max         : integer_t;
  	task_count  : integer_t;
  end;
  task_affinity_tag_info_data_t = _task_affinity_tag_info;
  task_affinity_tag_info_t = ^_task_affinity_tag_info;
const
  TASK_AFFINITY_TAG_INFO_COUNT =	(sizeof(task_affinity_tag_info_data_t) div sizeof(natural_t));

//#pragma pack()

{ Obsolete interfaces.}
{
const
  TASK_SCHED_TIMESHARE_INFO	= 10;
  TASK_SCHED_RR_INFO		    = 11;
  TASK_SCHED_FIFO_INFO		  = 12;
  TASK_SCHED_INFO			      = 14;
}

// thread_state.h

const
  PPC_THREAD_STATE_MAX  = 144;    { Size of biggest state possible }
  I386_THREAD_STATE_MAX	= 144;    { Size of biggest state possible }
  // ARM ?

  // todo: should be platform dependant {$ifdef}
  THREAD_STATE_MAX = I386_THREAD_STATE_MAX;

// thread_status.h

type
  thread_state_t = ^natural_t;	{ Variable-length array }

  { THREAD_STATE_MAX is now defined in <mach/machine/thread_state.h> }
  thread_state_data_t = array [0..THREAD_STATE_MAX-1] of 	natural_t;

const
 	THREAD_STATE_FLAVOR_LIST	   = 0;	 { List of valid flavors }
  THREAD_STATE_FLAVOR_LIST_NEW = 128;

type
  thread_state_flavor_t =	integer;

  Tthread_state_flavor_array = array [word] of thread_state_flavor_t;
  Pthread_state_flavor_array = ^Tthread_state_flavor_array;
  //thread_state_flavor_array_t = ^thread_state_flavor_t;
  thread_state_flavor_array_t = Pthread_state_flavor_array;

{ **
  ** Process Management Interface
  ** }

// on-line manual pages:
// http://web.mit.edu/darwin/src/modules/xnu/osfmk/man/

{ *
  * Task Interface
  * }

type
  exception_type_t = integer;
  exception_data_type_t = integer_t;
  mach_exception_data_type_t = int64_t;
  exception_behavior_t = longword;
  exception_data_t = ^exception_data_type_t;
  mach_exception_data_t = ^mach_exception_data_type_t;
  exception_mask_t = LongWord;

  Texception_mask_array = array [word] of exception_mask_t;
  Pexception_mask_array = ^Texception_mask_array;
  exception_mask_array_t = Pexception_mask_array;
  //exception_mask_array_t = ^exception_mask_t; // original declaration

  Texception_behavior_array = array [word] of exception_behavior_t;
  Pexception_behavior_array = ^Texception_behavior_array;
  exception_behavior_array_t = Pexception_behavior_array;
  //exception_behavior_array_t = ^exception_behavior_t; // original declaration

  Texception_flavor_array = array [word] of thread_state_flavor_t;
  Pexception_flavor_array = ^Texception_flavor_array;
  exception_flavor_array_t = Pexception_flavor_array;
  //exception_flavor_array_t = ^thread_state_flavor_t; // original declaration

  Texception_port_array = array [word] of mach_port_t;
  Pexception_port_array = ^Texception_port_array;
  //exception_port_array_t = ^mach_port_t;
  exception_port_array_t = Pexception_port_array;

  mach_exception_code_t = ^mach_exception_data_type_t;
  mach_exception_subcode_t = ^mach_exception_data_type_t;


//mach_ports_lookup - Provide caller with an array of the target task's well-known ports.
function mach_ports_lookup(target_task: task_t; var init_port_set: mach_port_array_t;
	var init_port_setCnt : mach_msg_type_number_t): kern_return_t;
  cdecl external name 'mach_ports_lookup';

//mach_ports_register - Register an array of well-known ports on behalf of the target task.
function mach_ports_register(target_task: task_t;
  init_port_set: mach_port_array_t;	init_port_setCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'mach_ports_register';

//mach_task_self - Return a send right to the caller's task_self port.
function mach_task_self: mach_port_t; cdecl external name 'mach_task_self';

//task_create - Create a new task.
function task_create(target_task: task_t;
  ledgers : ledger_array_t; ledgersCnt : mach_msg_type_number_t;
  inherit_memory : boolean_t; var child_task : task_t): kern_return_t; cdecl external name 'task_create';

//task_get_emulation_vector - Return an array identifying the target task's user-level system call handlers.
function task_get_emulation_vector (task: task_t; vector_start: Pinteger;
  var emulation_vector: emulation_vector_t; var emulation_vectorCnt: mach_msg_type_number_t
): kern_return_t; cdecl external name 'task_get_emulation_vector';

//task_get_exception_ports - Return send rights to the target task's exception ports.
function task_get_exception_ports (
  task: task_t;
  exception_mask : exception_mask_t;
	masks : exception_mask_array_t;
  var masksCnt: mach_msg_type_number_t;
	old_handlers: exception_handler_array_t;
  old_behaviors: exception_behavior_array_t;
  old_flavors: exception_flavor_array_t): kern_return_t;
  cdecl external name 'task_get_exception_ports';

//task_get_special_port - Return a send write to the indicated special port.
function task_get_special_port(task: task_t;which_port: integer;
  var special_port: mach_port_t): kern_return_t; cdecl external name 'task_get_special_port';

//task_info - Return per-task information according to specified flavor.
function task_info(task: task_t; flavor: task_flavor_t; task_info: task_info_t;
  var task_info_count: mach_msg_type_number_t): kern_return_t; cdecl external name 'task_info';

//task_resume - Decrement the target task's suspend count.

function task_resume(target_task: task_t): kern_return_t;
  cdecl external name 'task_resume';


//task_sample - Sample the target task's thread program counters periodically.
function task_sample(task: task_t;reply: mach_port_t): kern_return_t;
  cdecl external name 'task_sample';

//task_set_emulation - Establish a user-level handler for a system call.
function task_set_emulation(target_port: task_t; routine_entry_pt: vm_address_t;
  routine_number: integer): kern_return_t; cdecl external name 'task_set_emulation';


//task_set_emulation_vector - Establish the target task's user-level system call handlers.
function task_set_emulation_vector (task: task_t; vector_start: integer;
  emulation_vector: emulation_vector_t; emulation_vectorCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'task_set_emulation_vector';

//task_set_exception_ports - Set target task's exception ports.
function task_set_exception_ports(task: task_t; 	exception_mask: exception_mask_t;
	new_port: mach_port_t; behavior: exception_behavior_t;
  new_flavor: thread_state_flavor_t): kern_return_t;
  cdecl external name 'task_set_exception_ports';


//task_set_info - Set task-specific information state.
function task_set_info(target_task: task_t; flavor: task_flavor_t;
  task_info_in: task_info_t; task_info_inCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'task_set_info';


//task_set_port_space - Set the size of the target task's port name space table.
// where?

//task_set_special_port - Set the indicated special port.
function task_set_special_port(task : task_t; which_port: integer;
  special_port: mach_port_t): kern_return_t; cdecl external name 'task_set_special_port';

//task_suspend - Suspend the target task.
function task_suspend(target_task: task_t): kern_return_t; cdecl
  external name 'task_suspend';

//task_swap_exception_ports - Set target task's exception ports, returning the previous exception ports.
function task_swap_exception_ports(task: task_t; exception_mask: exception_mask_t;
	new_port: mach_port_t; 	behavior: exception_behavior_t; new_flavor: thread_state_flavor_t;
	masks: exception_mask_array_t; var masksCnt:  mach_msg_type_number_t;
	old_handlerss: exception_handler_array_t;	old_behaviors: exception_behavior_array_t;
	old_flavors:   exception_flavor_array_t): kern_return_t;
  cdecl external name 'task_swap_exception_ports';

//task_terminate - Terminate the target task and deallocate its resources.
function task_terminate(target_task: task_t): kern_return_t;
  cdecl external name 'task_terminate';

//task_threads - Return the target task's list of threads.
function task_threads(target_task: task_t; var act_list: thread_act_array_t;
  var act_listCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'task_threads';

{ *
  * Thread Interface
  * }


// thread_info.h
{*
 *	Generic information structure to allow for expansion.
 *}
type
  thread_flavor_t =	natural_t	;
  thread_info_t = ^integer_t;		{ varying array of int }

const
  THREAD_INFO_MAX	=	1024; 	{ maximum array size *}
type
  thread_info_data_t = array [0..THREAD_INFO_MAX-1] of integer_t;

{*
 *	Currently defined information.
 *}
const
  THREAD_BASIC_INFO         =	3;   { basic information }

type
// struct
  _thread_basic_info = packed record
    user_time   : time_value_t; { user run time }
    system_time : time_value_t; { system run time }
    cpu_usage   : integer_t;    { scaled cpu usage percentage }
	  policy      : policy_t;		  { scheduling policy in effect }
    run_state   : integer_t;    { run state (see below) }
    flags       : integer_t;    { various flags (see below) }
    suspend_count : integer_t;  { suspend count for thread }
    sleep_time    : integer_t;  { number of seconds that thread has been sleeping }
  end;

  thread_basic_info_data_t =  _thread_basic_info;
  thread_basic_info_t = ^_thread_basic_info;

const
  THREAD_BASIC_INFO_COUNT = sizeof(thread_basic_info_data_t) div sizeof(natural_t);

{*
 *	Scale factor for usage field.
 *}

const
  TH_USAGE_SCALE =	1000;

{*
 *	Thread run states (state field).
 *}

const
  TH_STATE_RUNNING	= 1;	{ thread is running normally }
  TH_STATE_STOPPED	= 2;	{ thread is stopped }
  TH_STATE_WAITING	= 3;	{ thread is waiting normally }
  TH_STATE_UNINTERRUPTIBLE = 4;	{ thread is in an uninterruptible wait }
  TH_STATE_HALTED		= 5;	{ thread is halted at a clean point {

{*
 *	Thread flags (flags field).
 *}
  TH_FLAGS_SWAPPED = $01;	{ thread is swapped out }
  TH_FLAGS_IDLE		 = $02;	{ thread is an idle thread }

{*
 * Obsolete interfaces.
 *}

  THREAD_SCHED_TIMESHARE_INFO	= 10;
  THREAD_SCHED_RR_INFO		    = 11;
  THREAD_SCHED_FIFO_INFO		  = 12;


// thread_act.h

const
  // platform dependant?
 	thread_act_MSG_COUNT =	25;

// thread_terminate - Destroy a thread.
function thread_terminate(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_terminate';

function act_get_state (target_act: thread_act_t;flavor: integer;
	old_state: thread_state_t; var old_stateCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'act_get_state';

function act_set_state (target_act: thread_act_t;	flavor: integer;
	new_state: thread_state_t; new_stateCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'act_set_state';

// thread_get_state - Return the execution state for a thread.
function thread_get_state(target_act: thread_act_t; flavor: thread_state_flavor_t;
	old_state: thread_state_t; var old_stateCnt: mach_msg_type_number_t
  ): kern_return_t;
  cdecl external name 'thread_get_state';

//thread_set_state - Set the target thread's user-mode execution state.
function thread_set_state(target_act: thread_act_t;
	flavor: thread_state_flavor_t; new_state: thread_state_t;
	new_stateCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_set_state';

//thread_suspend - Suspend a thread.
function thread_suspend(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_suspend';

//thread_resume - Resume a thread.
function thread_resume(target_act: thread_act_t): kern_return_t ;
  cdecl external name 'thread_resume';

//thread_abort - Abort a thread.
function thread_abort(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_abort';

//thread_abort_safely - Abort a thread, restartably
function thread_abort_safely(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_abort_safely';

// thread_depress_abort - Cancel thread scheduling depression.
function thread_depress_abort(thread: thread_act_t): kern_return_t;
  cdecl external name 'thread_depress_abort';

//thread_get_special_port - Return a send right to the caller-specified special port.
function thread_get_special_port (thr_act: thread_act_t; which_port: integer;
  var special_port: mach_port_t): kern_return_t ;
  cdecl external name 'thread_get_special_port';

//thread_set_special_port - Set caller-specified special port belonging to the target thread.
function thread_set_special_port(thr_act: thread_act_t;	which_port: integer;
	special_port: mach_port_t): kern_return_t;
  cdecl external name 'thread_set_special_port';

//thread_info - Return information about a thread.
function thread_info(target_act: thread_act_t;flavor: thread_flavor_t;
  thread_info_out: thread_info_t;
  var thread_info_outCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_info';

//thread_set_exception_ports - Set exception ports for a thread.
function thread_set_exception_ports(thread: thread_act_t;
  exception_mask: exception_mask_t; new_port:	mach_port_t;
	behavior: exception_behavior_t; new_flavor:	thread_state_flavor_t): kern_return_t;
  cdecl external name 'thread_set_exception_ports';

//thread_get_exception_ports - Return a send right to an exception port.
function thread_get_exception_ports(thread: thread_act_t;
	exception_mask: exception_mask_t;  masks: exception_mask_array_t;
	var masksCnt: mach_msg_type_number_t;	old_handlers: exception_handler_array_t;
	old_behaviors: exception_behavior_array_t;
  old_flavors: exception_flavor_array_t): kern_return_t;
  cdecl external name 'thread_get_exception_ports';

//thread_swap_exception_ports - Swap exception ports for a thread.
function thread_swap_exception_ports(thread: thread_act_t;
	exception_mask: exception_mask_t; new_port: mach_port_t;
	behavior: exception_behavior_t; new_flavor: thread_state_flavor_t;
	masks: exception_mask_array_t; 	var masksCnt: mach_msg_type_number_t;
	old_handlers: exception_handler_array_t; old_behaviors: exception_behavior_array_t;
  old_flavors: exception_flavor_array_t): kern_return_t;
  cdecl external name 'thread_swap_exception_ports';

// Scheduling Thread Interface

// thread_policy - Set target thread's scheduling policy state.
function thread_policy (thr_act: thread_act_t; policy: policy_t;
	base: policy_base_t; baseCnt: mach_msg_type_number_t;
	set_limit: boolean_t): kern_return_t;
  cdecl external name 'thread_policy';

// ??? where?, don't mix with thread_set_policy()
function thread_policy_set(thread: thread_act_t;
	flavor: thread_policy_flavor_t; policy_info: thread_policy_t;
	policy_infoCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_policy_set';

//??? where?
function thread_policy_get(thread: thread_act_t; flavor: thread_policy_flavor_t;
	policy_info: thread_policy_t;	var policy_infoCnt: mach_msg_type_number_t;
	var get_default : boolean_t): kern_return_t;
  cdecl external name 'thread_policy_get';

//thread_sample - Perform periodic PC sampling for a thread.
function thread_sample(thread: thread_act_t;
  reply: mach_port_t): kern_return_t;
  cdecl external name 'thread_sample';

function etap_trace_thread(target_act: thread_act_t;
  trace_status: boolean_t): kern_return_t;
  cdecl external name 'etap_trace_thread';

// thread_assign - Assign a thread to a processor set.
function thread_assign(thread: thread_act_t;
  new_set: processor_set_t): kern_return_t;
  cdecl external name 'thread_assign';

//thread_assign_default - Assign a thread to the default processor set.
function thread_assign_default(thread: thread_act_t): kern_return_t;
  cdecl external name 'thread_assign_default';

// thread_get_assignment - Return the processor set to which a thread is assigned.
function thread_get_assignment(thread: thread_act_t;
  var assigned_set: processor_set_name_t): kern_return_t;
  cdecl external name 'thread_get_assignment';

// thread_set_policy - Set target thread's scheduling policy state.
function thread_set_policy(thr_act: thread_act_t;	pset: processor_set_t;
	policy: policy_t; base: policy_base_t; 	baseCnt: mach_msg_type_number_t;
	limit: policy_limit_t; limitCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_set_policy';


// mach_traps.h

function swtch_pri(pri: integer): boolean_t; cdecl external name 'swtch_pri';

function switch: boolean_t; cdecl external name 'switch';

function thread_switch(thread_name: mach_port_name_t;	option: integer;
	option_time: mach_msg_timeout_t): kern_return_t; cdecl external name 'thread_switch';

function task_self_trap: mach_port_name_t; cdecl external name 'task_self_trap';

{*
 *	Obsolete interfaces.
 *}

function task_for_pid(target_tport: mach_port_name_t; pid: integer;
  var t: mach_port_name_t): kern_return_t; cdecl external{ name 'task_for_pid'};

function task_name_for_pid(target_tport: mach_port_name_t; pid: integer;
	var tn: mach_port_name_t): kern_return_t; cdecl external name 'task_name_for_pid';

function pid_for_task(t: mach_port_name_t; var x: integer): kern_return_t;
  cdecl external name 'pid_for_task';

//#if		!defined(__LP64__)
// these should go away altogether - so no 64 legacy please *}

function map_fd(fd: integer; offset: vm_offset_t;	var va: vm_offset_t;
	findspace: boolean_t; 	size: vm_size_t): kern_return_t;
  cdecl external name 'map_fd';

//#endif	{* !defined(__LP64__) *}

{*
 *	File:	memory_object.h
 *	Author:	Michael Wayne Young
 *
 *	External memory management interface definition.
 *}

{*
 *	User-visible types used in the external memory
 *	management interface:
 *}

//#define VM_64_BIT_DATA_OBJECTS

type
  memory_object_offset_t = QWord;
  memory_object_size_t   = QWord;
  memory_object_cluster_size_t = natural_t;

  pmemory_object_offset_t = ^memory_object_offset_t;
  pmemory_object_size_t = ^memory_object_size_t;

{*
 * Temporary until real EMMI version gets re-implemented
 *}
  memory_object_t = mach_port_t;
  memory_object_control_t = mach_port_t;

  memory_object_array_t = ^memory_object_t;
					{* A memory object ... *}
					{*  Used by the kernel to retrieve *}
					{*  or store data *}

  memory_object_name_t = mach_port_t;
					{* Used to describe the memory ... *}
					{*  object in vm_regions() calls *}

  memory_object_default_t = mach_port_t;
					{* Registered with the host ... *}
					{*  for creating new internal objects *}

const
  MEMORY_OBJECT_NULL		     = 0;
  MEMORY_OBJECT_CONTROL_NULL = 0;
  MEMORY_OBJECT_NAME_NULL		 = 0;
  MEMORY_OBJECT_DEFAULT_NULL = 0;

type
  memory_object_copy_strategy_t = Integer;

{* How memory manager handles copy: *}
const
 	MEMORY_OBJECT_COPY_NONE		= 0;					{* ... No special support *}
  MEMORY_OBJECT_COPY_CALL		= 1;					{* ... Make call on memory manager *}
  MEMORY_OBJECT_COPY_DELAY 	= 2;					{* ... Memory manager doesn't	change data externally. *}
  MEMORY_OBJECT_COPY_TEMPORARY = 3;				{* ... Memory manager doesn't change data externally, and doesn't need to see changes. *}
  MEMORY_OBJECT_COPY_SYMMETRIC = 4;				{* ... Memory manager doesn't change data externally,	doesn't need to see changes,
                                    			 *     and object will not be multiply mapped. XXX Not yet safe for non-kernel use. *}

   MEMORY_OBJECT_COPY_INVALID	 = 5; 			{* ...	An invalid copy strategy,	for external objects which
                                					 *	have not been initialized.Allows copy_strategy to be examined without also
                                 					 *	examining pager_ready and internal.  *}

type
  memory_object_return_t = integer;

  {* Which pages to return to manager this time (lock_request) *}
const
  MEMORY_OBJECT_RETURN_NONE	= 0;	{* ... don't return any. *}
  MEMORY_OBJECT_RETURN_DIRTY	= 1;	{* ... only dirty pages. *}
  MEMORY_OBJECT_RETURN_ALL	= 2;		{* ... dirty and precious pages. *}
  MEMORY_OBJECT_RETURN_ANYTHING	= 3;	{* ... any resident page. *}

{*	Data lock request flags *}

	MEMORY_OBJECT_DATA_FLUSH 	    = $1;
	MEMORY_OBJECT_DATA_NO_CHANGE  =	$2;
	MEMORY_OBJECT_DATA_PURGE	    = $4;
	MEMORY_OBJECT_COPY_SYNC	 	    = $8;
	MEMORY_OBJECT_DATA_SYNC		    = $10;
  MEMORY_OBJECT_IO_SYNC         = $20;

{*	Types for the memory object flavor interfaces }

const
  MEMORY_OBJECT_INFO_MAX      = 1024;

type
  memory_object_info_t = ^Integer;
  memory_object_flavor_t = Integer;
  memory_object_info_data_t = array [0..MEMORY_OBJECT_INFO_MAX-1] of Integer;

const
  MEMORY_OBJECT_PERFORMANCE_INFO	= 11;
  MEMORY_OBJECT_ATTRIBUTE_INFO	  = 14;
  MEMORY_OBJECT_BEHAVIOR_INFO 	  = 15;

type
  memory_object_perf_info = packed record
  	cluster_size  : memory_object_cluster_size_t;
	  may_cache     : boolean_t			;
  end;

  memory_object_attr_info = packed record
  	copy_strategy     : memory_object_copy_strategy_t;
	  cluster_size      : memory_object_cluster_size_t;
  	may_cache_object  : boolean_t;
	  temporary         : boolean_t;
  end;

  memory_object_behave_info = packed record
  	copy_strategy : memory_object_copy_strategy_t;
	  temporary : boolean_t;
  	boolean_t : boolean_t;
  	silent_overwrite  : boolean_t;
  	advisory_pageout  : boolean_t;
  end;

type
  memory_object_behave_info_t = ^memory_object_behave_info;
  memory_object_behave_info_data_t= memory_object_behave_info;

  memory_object_perf_info_t = ^memory_object_perf_info;
  memory_object_perf_info_data_t = memory_object_perf_info	;

  memory_object_attr_info_t = ^memory_object_attr_info;
  memory_object_attr_info_data_t = memory_object_attr_info	;

const
  MEMORY_OBJECT_BEHAVE_INFO_COUNT = sizeof(memory_object_behave_info_data_t) div sizeof(integer);
  MEMORY_OBJECT_PERF_INFO_COUNT	  = sizeof(memory_object_perf_info_data_t) div sizeof(integer);
  MEMORY_OBJECT_ATTR_INFO_COUNT	  = sizeof(memory_object_attr_info_data_t) div sizeof(integer);

(*
#define invalid_memory_object_flavor(f)					\
	(f != MEMORY_OBJECT_ATTRIBUTE_INFO && 				\
	 f != MEMORY_OBJECT_PERFORMANCE_INFO && 			\
	 f != OLD_MEMORY_OBJECT_BEHAVIOR_INFO &&			\
	 f != MEMORY_OBJECT_BEHAVIOR_INFO &&				\
	 f != OLD_MEMORY_OBJECT_ATTRIBUTE_INFO)
*)

{* Used to support options on memory_object_release_name call *}
const
  MEMORY_OBJECT_TERMINATE_IDLE	= $1;
  MEMORY_OBJECT_RESPECT_CACHE	  = $2;
  MEMORY_OBJECT_RELEASE_NO_OP	  = $4;

{* named entry processor mapping options *}
{* enumerated *}
const
  MAP_MEM_NOOP	 	 = 0;
  MAP_MEM_COPYBACK = 1;
  MAP_MEM_IO	 	   = 2;
  MAP_MEM_WTHRU		 = 3;
  MAP_MEM_WCOMB		 = 4;	{* Write combining mode *}
					{* aka store gather     *}

{#define GET_MAP_MEM(flags)	\
	((((unsigned int)(flags)) >> 24) & $FF)

#define SET_MAP_MEM(caching, flags)	\
	((flags) = ((((unsigned int)(caching)) << 24) \
			& $FF000000) | ((flags) & $FFFFFF));
  }

{* leave room for vm_prot bits *}
const
  MAP_MEM_ONLY		      = $10000;	{* change processor caching  *}
  MAP_MEM_NAMED_CREATE	= $20000; {* create extant object      *}
  MAP_MEM_PURGABLE	    = $40000;	{* create a purgable VM object *}
  MAP_MEM_NAMED_REUSE	  = $80000;	{* reuse provided entry if identical *}


{*
 *  Universal Page List data structures
 *
 *  A UPL describes a bounded set of physical pages
 *  associated with some range of an object or map
 *  and a snapshot of the attributes associated with
 *  each of those pages.
 *}

type
  upl_page_info = packed record
	  opaque: array [0..1] of LongWord;	{* use upl_page_xxx() accessor funcs *}
  end;

  upl_page_info_t = upl_page_info	;
  upl_page_info_array_t = ^upl_page_info_t;
  upl_page_list_ptr_t = upl_page_info_array_t	;

  upl_offset_t = uint32_t;	{* page-aligned byte offset *}
  upl_size_t   = uint32_t;	{* page-aligned byte size *}

{* upl invocation flags *}
{* top nibble is used by super upl *}

const
  UPL_FLAGS_NONE		= $00000000;
  UPL_COPYOUT_FROM	= $00000001;
  UPL_PRECIOUS	 	  = $00000002;
  UPL_NO_SYNC		    = $00000004;
  UPL_CLEAN_IN_PLACE	= $00000008;
  UPL_NOBLOCK		      = $00000010;
  UPL_RET_ONLY_DIRTY	= $00000020;
  UPL_SET_INTERNAL	  = $00000040;
  UPL_QUERY_OBJECT_TYPE	= $00000080;
  UPL_RET_ONLY_ABSENT	  = $00000100; {* used only for COPY_FROM = FALSE *}
  UPL_FILE_IO           = $00000200;
  UPL_SET_LITE		      = $00000400;
  UPL_SET_INTERRUPTIBLE	= $00000800;
  UPL_SET_IO_WIRE		    = $00001000;
  UPL_FOR_PAGEOUT		    = $00002000;
  UPL_WILL_BE_DUMPED    = $00004000;
  UPL_FORCE_DATA_SYNC   = $00008000;
{* continued after the ticket bits... *}

  UPL_PAGE_TICKET_MASK	= $000F0000;
  UPL_PAGE_TICKET_SHIFT = 16;

{* ... flags resume here *}
  UPL_BLOCK_ACCESS	= $00100000;
  UPL_ENCRYPT		    = $00200000;
  UPL_NOZEROFILL		= $00400000;
  UPL_WILL_MODIFY		= $00800000; {* caller will modify the pages *}

  UPL_NEED_32BIT_ADDR	= $01000000;

{* UPL flags known by this kernel *}
  UPL_VALID_FLAGS		= $01FFFFFF;


{* upl abort error flags *}
  UPL_ABORT_RESTART	= $1;
  UPL_ABORT_UNAVAILABLE	= $2;
  UPL_ABORT_ERROR		= $4;
  UPL_ABORT_FREE_ON_EMPTY	= $8;  {* only implemented in wrappers *}
  UPL_ABORT_DUMP_PAGES	= $10;
  UPL_ABORT_NOTIFY_EMPTY	= $20;
  UPL_ABORT_ALLOW_ACCESS	= $40;

{* upl pages check flags *}
  UPL_CHECK_DIRTY         = $1;

{*
 *  upl pagein/pageout  flags
 *
 *
 * when I/O is issued from this UPL it should be done synchronously
 *}
  UPL_IOSYNC	= $1;

{*
 * the passed in UPL should not have either a commit or abort
 * applied to it by the underlying layers... the site that
 * created the UPL is responsible for cleaning it up.
 *}
  UPL_NOCOMMIT	= $2;

{*
 * turn off any speculative read-ahead applied at the I/O layer
 *}
  UPL_NORDAHEAD	= $4;

{*
 * pageout request is targeting a real file
 * as opposed to a swap file.
 *}

  UPL_VNODE_PAGER	= $8;
{*
 * this pageout is being originated as part of an explicit
 * memory synchronization operation... no speculative clustering
 * should be applied, only the range specified should be pushed.
 *}
  UPL_MSYNC		= $10;

{*
 *
 *}

{*
 * this pageout is being originated as part of an explicit
 * memory synchronization operation that is checking for I/O
 * errors and taking it's own action... if an error occurs,
 * just abort the pages back into the cache unchanged
 *}
  UPL_KEEPCACHED		= $40;

{* upl commit flags *}
  UPL_COMMIT_FREE_ON_EMPTY = $1; {* only implemented in wrappers *}
  UPL_COMMIT_CLEAR_DIRTY	 = $2;
  UPL_COMMIT_SET_DIRTY		= $4;
  UPL_COMMIT_INACTIVATE		= $8;
  UPL_COMMIT_NOTIFY_EMPTY	= $10;
  UPL_COMMIT_ALLOW_ACCESS	= $20;

{* flags for return of state from vm_map_get_upl,  vm_upl address space *}
{* based call *}
  UPL_DEV_MEMORY		= $1;
  UPL_PHYS_CONTIG		= $2;

{*
 * Flags for the UPL page ops routine.  This routine is not exported
 * out of the kernel at the moment and so the defs live here.
 *}
  UPL_POP_DIRTY		  = $1;
  UPL_POP_PAGEOUT		= $2;
  UPL_POP_PRECIOUS	= $4;
  UPL_POP_ABSENT		= $8;
  UPL_POP_BUSY			= $10;

  UPL_POP_PHYSICAL	= $10000000;
  UPL_POP_DUMP		  = $20000000;
  UPL_POP_SET		= $40000000;
  UPL_POP_CLR		= $80000000;

{*
 * Flags for the UPL range op routine.  This routine is not exported
 * out of the kernel at the moemet and so the defs live here.
 *}
{*
 * UPL_ROP_ABSENT: Returns the extent of the range presented which
 * is absent, starting with the start address presented
 *}
  UPL_ROP_ABSENT		= $01;
{*
 * UPL_ROP_PRESENT: Returns the extent of the range presented which
 * is present (i.e. resident), starting with the start address presented
 *}
  UPL_ROP_PRESENT		= $02;
{*
 * UPL_ROP_DUMP: Dump the pages which are found in the target object
 * for the target range.
 *}
  UPL_ROP_DUMP			= $04;

// vm_types.h

type
{*
 * We use addr64_t for 64-bit addresses that are used on both
 * 32 and 64-bit machines.  On PPC, they are passed and returned as
 * two adjacent 32-bit GPRs.  We use addr64_t in places where
 * common code must be useable both on 32 and 64-bit machines.
 *}
  addr64_t = uint64_t;		{* Basic effective address *}

{*
 * We use reg64_t for addresses that are 32 bits on a 32-bit
 * machine, and 64 bits on a 64-bit machine, but are always
 * passed and returned in a single GPR on PPC.  This type
 * cannot be used in generic 32-bit c, since on a 64-bit
 * machine the upper half of the register will be ignored
 * by the c compiler in 32-bit mode.  In c, we can only use the
 * type in prototypes of functions that are written in and called
 * from assembly language.  This type is basically a comment.
 *}
  reg64_t =	uint32_t;

{*
 * To minimize the use of 64-bit fields, we keep some physical
 * addresses (that are page aligned) as 32-bit page numbers.
 * This limits the physical address space to 16TB of RAM.
 *}
  ppnum_t = uint32_t ;		{* Physical page number *}
  //UINT32_MAX = PPNUM_MAX;

  vm_map_t = mach_port_t;
  pvm_map_t = ^vm_map_t;

const
  VM_MAP_NULL = 0; // or nil?

{*
 *	File:	mach/vm_prot.h
 *	Author:	Avadis Tevanian, Jr., Michael Wayne Young
 *
 *	Virtual memory protection definitions.
 *
 *}

{*
 *	Types defined:
 *
 *	vm_prot_t		VM protection values.
 *}

type
  vm_prot_t = integer;
  pvm_prot_t = ^vm_prot_t;

{*
 *	Protection values, defined as bits within the vm_prot_t type
 *}

const
 	VM_PROT_NONE =$00;
  VM_PROT_READ =$01;	{* read permission *}
  VM_PROT_WRITE	  = $02;	{* write permission *}
  VM_PROT_EXECUTE	= $04;	{* execute permission *}
  VM_PROT_DEFAULT	= VM_PROT_READ or VM_PROT_WRITE; {* The default protection for newly-created virtual memory *}
  VM_PROT_ALL	= VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE; {*	The maximum privileges possible, for parameter checking. *}

{*
 *	An invalid protection value.
 *	Used only by memory_object_lock_request to indicate no change
 *	to page locks.  Using -1 here is a bad idea because it
 *	looks like VM_PROT_ALL and then some.
 *}
  VM_PROT_NO_CHANGE	= $08;

{*
 *      When a caller finds that he cannot obtain write permission on a
 *      mapped entry, the following flag can be used.  The entry will
 *      be made "needs copy" effectively copying the object (using COW),
 *      and write permission will be added to the maximum protections
 *      for the associated entry.
 *}
  VM_PROT_COPY = $10;

{*
 *	Another invalid protection value.
 *	Used only by memory_object_data_request upon an object
 *	which has specified a copy_call copy strategy. It is used
 *	when the kernel wants a page belonging to a copy of the
 *	object, and is only asking the object as a result of
 *	following a shadow chain. This solves the race between pages
 *	being pushed up by the memory manager and the kernel
 *	walking down the shadow chain.
 *}
  VM_PROT_WANTS_COPY = $10;


{*	File:	mach/vm_inherit.h
 *	Author:	Avadis Tevanian, Jr., Michael Wayne Young
 *
 *	Virtual memory map inheritance definitions. *}

{*	Types defined:
 *	vm_inherit_t	inheritance codes. *}

type
  vm_inherit_t = LongWord;	{* might want to change this *}

{* Enumeration of valid values for vm_inherit_t. *}

const
 	VM_INHERIT_SHARE	      = 0;	{* share with child *}
 	VM_INHERIT_COPY		      = 1;	{* copy into child *}
  VM_INHERIT_NONE		      = 2;	{* absent from child *}
 	VM_INHERIT_DONATE_COPY	= 3;	{* copy and delete *}
  VM_INHERIT_DEFAULT      = VM_INHERIT_COPY;
  VM_INHERIT_LAST_VALID   = VM_INHERIT_NONE;

{*	File:	mach/vm_sync.h
 *	Virtual memory synchronisation definitions.  *}

type
  vm_sync_t= LongWord;

{**	Synchronization flags, defined as bits within the vm_sync_t type *}

const
 	VM_SYNC_ASYNCHRONOUS =	$01;
  VM_SYNC_SYNCHRONOUS	 =  $02;
  VM_SYNC_INVALIDATE	 =  $04;
  VM_SYNC_KILLPAGES    =  $08;
  VM_SYNC_DEACTIVATE   =  $10;
  VM_SYNC_CONTIGUOUS   =  $20;

{*	File:	mach/vm_behavior.h
 *
 *	Virtual memory map behavior definitions.
 *}

{*	Types defined:
 *
 *	vm_behavior_t	behavior codes.
 *}

type
  vm_behavior_t = integer;

{*	Enumeration of valid values for vm_behavior_t.
 *	These describe expected page reference behavior for
 *	for a given range of virtual memory.  For implementation
 *	details see vm/vm_fault.c
 *}

const
  VM_BEHAVIOR_DEFAULT	   = 0;	{* default *}
  VM_BEHAVIOR_RANDOM	   = 1;	{* random *}
  VM_BEHAVIOR_SEQUENTIAL = 2;	{* forward sequential *}
  VM_BEHAVIOR_RSEQNTL	   = 3;	{* reverse sequential *}
  VM_BEHAVIOR_WILLNEED	 = 4;	{* will need in near future *}
  VM_BEHAVIOR_DONTNEED	 = 5;	{* dont need in near future *}


{*	File:	mach/vm_region.h
 *
 *	Define the attributes of a task's memory region
 *
 *}


//#pragma pack(4)

{*
 *	Types defined:
 *
 *	vm_region_info_t	memory region attributes
 *}

const
  VM_REGION_INFO_MAX = 1024;

type
  vm_region_info_t = ^Integer;
  vm_region_info_64_t = ^Integer;
  vm_region_recurse_info_t = ^Integer;
  vm_region_recurse_info_64_t = ^Integer;
  vm_region_flavor_t = Integer;
  vm_region_info_data_t = array [0..VM_REGION_INFO_MAX-1] of Integer;

const
  VM_REGION_BASIC_INFO_64		= 9;

type
  vm_region_basic_info_64_ = packed record
  	protection : vm_prot_t;
	  max_protection: vm_prot_t;
  	inheritance : vm_inherit_t;
  	shared: boolean_t;
  	reserved: boolean_t;
  	offset: memory_object_offset_t;
  	behavior: vm_behavior_t;
	  user_wired_count: Word; //unsigned short		;
 end;
 vm_region_basic_info_64_t = ^vm_region_basic_info_64_;
 vm_region_basic_info_data_64_t = vm_region_basic_info_64_;

const
  VM_REGION_BASIC_INFO_COUNT_64 = sizeof(vm_region_basic_info_data_64_t) div sizeof(integer);

{*
 * Passing VM_REGION_BASIC_INFO to vm_region_64
 * automatically converts it to a VM_REGION_BASIC_INFO_64.
 * Please use that explicitly instead.
 *}
  VM_REGION_BASIC_INFO	=	10;

{*
 * This is the legacy basic info structure.  It is
 * deprecated because it passes only a 32-bit memory object
 * offset back - too small for many larger objects (e.g. files).
 *}
type
  vm_region_basic_info_ = packed record
  	protection: vm_prot_t;
  	max_protection: vm_prot_t;
  	inheritance: vm_inherit_t;
  	shared: boolean_t;
  	reserved: boolean_t;
  	offset: uint32_t; { too small for a real offset }
  	behavior: vm_behavior_t;
  	user_wired_count: Word; //unsigned short		;
  end;
  vm_region_basic_info_t = ^vm_region_basic_info_;
  vm_region_basic_info_data_t = vm_region_basic_info_;

const
  VM_REGION_BASIC_INFO_COUNT = sizeof(vm_region_basic_info_data_t) div sizeof(integer);

const
  VM_REGION_EXTENDED_INFO	= 11;

  SM_COW             = 1;
  SM_PRIVATE         = 2;
  SM_EMPTY           = 3;
  SM_SHARED          = 4;
  SM_TRUESHARED      = 5;
  SM_PRIVATE_ALIASED = 6;
  SM_SHARED_ALIASED  = 7;

{*
 * For submap info,  the SM flags above are overlayed when a submap
 * is encountered.  The field denotes whether or not machine level mapping
 * information is being shared.  PTE's etc.  When such sharing is taking
 * place the value returned is SM_TRUESHARED otherwise SM_PRIVATE is passed
 * back.
 *}
type
  vm_region_extended_info_ = packed record
    protection : vm_prot_t;
    user_tag   : LongWord;
    pages_resident           : LongWord;
    pages_shared_now_private : LongWord;
    pages_swapped_out        : LongWord;
    pages_dirtied            : LongWord;
    ref_count                : LongWord;
    shadow_depth: Word;
    external_pager : byte;
    share_mode: byte;
  end;
  vm_region_extended_info_t = ^vm_region_extended_info_;
  vm_region_extended_info_data_t = vm_region_extended_info_;

const
  VM_REGION_EXTENDED_INFO_COUNT = sizeof(vm_region_extended_info_data_t) div sizeof(integer);

const
  VM_REGION_TOP_INFO	= 12;

type
  vm_region_top_info_ = packed record
    obj_id: LongWord;
    ref_count: LongWord;
    private_pages_resident : LongWord;
    shared_pages_resident : LongWord;
    share_mode : byte
  end;
  vm_region_top_info_t = ^vm_region_top_info_;
  vm_region_top_info_data_t = vm_region_top_info_;

const
  VM_REGION_TOP_INFO_COUNT = sizeof(vm_region_top_info_data_t) div sizeof(integer);

{*
 * vm_region_submap_info will return information on a submap or object.
 * The user supplies a nesting level on the call.  When a walk of the
 * user's map is done and a submap is encountered, the nesting count is
 * checked. If the nesting count is greater than 1 the submap is entered and
 * the offset relative to the address in the base map is examined.  If the
 * nesting count is zero, the information on the submap is returned.
 * The caller may thus learn about a submap and its contents by judicious
 * choice of the base map address and nesting count.  The nesting count
 * allows penetration of recursively mapped submaps.  If a submap is
 * encountered as a mapped entry of another submap, the caller may bump
 * the nesting count and call vm_region_recurse again on the target address
 * range.  The "is_submap" field tells the caller whether or not a submap
 * has been encountered.
 *
 * Object only fields are filled in through a walking of the object shadow
 * chain (where one is present), and a walking of the resident page queue.
 *
 *}

type
  vm_region_submap_info = packed record
	  protection: vm_prot_t;     {* present access protection *}
	  max_protection: vm_prot_t; {* max avail through vm_prot *}
	  inheritance: vm_inherit_t;{* behavior of map/obj on fork *}
	  offset: uint32_t;		{* offset into object/map *}
    user_tag: LongWord;	{* user tag on map entry *}
    pages_resident: LongWord;	{* only valid for objects *}
    pages_shared_now_private: LongWord; {* only for objects *}
    pages_swapped_out: LongWord; {* only for objects *}
    pages_dirtied: LongWord;   {* only for objects *}
    ref_count: LongWord;	 {* obj/map mappers, etc *}
    shadow_depth: Word; 	{* only for obj *}
    external_pager: byte;  {* only for obj *}
    share_mode: byte;	{* see enumeration *}
  	is_submap: boolean_t;	{* submap vs obj *}
  	behavior: vm_behavior_t;	{* access behavior hint *}
	  object_id: vm_offset_t;	{* obj/map name, not a handle *}
	  user_wired_count: Word;
  end;
  vm_region_submap_info_t = ^vm_region_submap_info;
  vm_region_submap_info_data_t = vm_region_submap_info;

const
  VM_REGION_SUBMAP_INFO_COUNT = sizeof(vm_region_submap_info_data_t) div sizeof(integer);

type
  vm_region_submap_info_64 = packed record
 	  protection : vm_prot_t		;     {* present access protection *}
  	max_protection: vm_prot_t; {* max avail through vm_prot *}
  	inheritance: vm_inherit_t;{* behavior of map/obj on fork *}
  	offset: memory_object_offset_t;		{* offset into object/map *}
    user_tag  : LongWord;	{* user tag on map entry *}
    pages_resident: LongWord;	{* only valid for objects *}
    pages_shared_now_private: LongWord; {* only for objects *}
    pages_swapped_out: LongWord; {* only for objects *}
    pages_dirtied: LongWord;   {* only for objects *}
    ref_count: LongWord;	 {* obj/map mappers, etc *}
    shadow_depth: Word; 	{* only for obj *}
    external_pager: Byte;  {* only for obj *}
    share_mode: Byte;	{* see enumeration *}
  	is_submap: boolean_t;	{* submap vs obj *}
  	behavior: vm_behavior_t;	{* access behavior hint *}
  	object_id: vm_offset_t;	{* obj/map name, not a handle *}
  	user_wired_count: Word;
  end;
  vm_region_submap_info_64_t = ^vm_region_submap_info_64;
  vm_region_submap_info_data_64_t = vm_region_submap_info_64;

const
  VM_REGION_SUBMAP_INFO_COUNT_64 = sizeof(vm_region_submap_info_data_64_t) div sizeof(integer);

type
  vm_region_submap_short_info_64  = packed record
  	protection: vm_prot_t;     {* present access protection *}
  	max_protection: vm_prot_t; {* max avail through vm_prot *}
  	inheritance : vm_inherit_t;{* behavior of map/obj on fork *}
  	offset: memory_object_offset_t;		{* offset into object/map *}
    user_tag : LongWord;	{* user tag on map entry *}
    ref_count: LongWord;	 {* obj/map mappers, etc *}
    shadow_depth: Word; 	{* only for obj *}
    external_pager: Byte;  {* only for obj *}
    share_mode: Byte;	{* see enumeration *}
  	is_submap: boolean_t;	{* submap vs obj *}
  	behavior: vm_behavior_t;	{* access behavior hint *}
  	object_id: vm_offset_t		;	{* obj/map name, not a handle *}
  	user_wired_count : Word;
  end;
  vm_region_submap_short_info_64_t = ^vm_region_submap_short_info_64;
  vm_region_submap_short_info_data_64_t =vm_region_submap_short_info_64	 ;

const
  VM_REGION_SUBMAP_SHORT_INFO_COUNT_64 = sizeof(vm_region_submap_short_info_data_64_t) div sizeof(integer);

type
  mach_vm_read_entry = packed record
  	address : mach_vm_address_t;
  	size    : mach_vm_size_t;
  end;

  vm_read_entry = packed record
  	address : vm_address_t;
	  size    : vm_size_t;
  end;

const
  VM_MAP_ENTRY_MAX  = 256;

type
  mach_vm_read_entry_t = array [0..VM_MAP_ENTRY_MAX-1] of mach_vm_read_entry;
  vm_read_entry_t      = array [0..VM_MAP_ENTRY_MAX-1] of vm_read_entry;


{*
 *	File:	mach/vm_attributes.h
 *	Author:	Alessandro Forin
 *
 *	Virtual memory attributes definitions.
 *
 *	These definitions are in addition to the machine-independent
 *	ones (e.g. protection), and are only selectively supported
 *	on specific machine architectures.
 *
 *}

{*	Types of machine-dependent attributes *}
type
  vm_machine_attribute_t = LongWord;

const
  MATTR_CACHE		  = 1;	{* cachability *}
  MATTR_MIGRATE		= 2;	{* migrability *}
  MATTR_REPLICATE	=	4; {* replicability *}

{* Values for the above, e.g. operations on attribute *}
type
  vm_machine_attribute_val_t = Integer;
  pvm_machine_attribute_val_t = ^vm_machine_attribute_val_t;

const
  MATTR_VAL_OFF	=	0;	{* (generic) turn attribute off *}
  MATTR_VAL_ON	=	1;	{* (generic) turn attribute on *}
  MATTR_VAL_GET	=	2;	{* (generic) return current value *}

  MATTR_VAL_CACHE_FLUSH	 = 6;	{* flush from all caches *}
  MATTR_VAL_DCACHE_FLUSH = 7;	{* flush from data caches *}
  MATTR_VAL_ICACHE_FLUSH = 8;	{* flush from instruction caches *}
  MATTR_VAL_CACHE_SYNC	 = 9;	{* sync I+D caches *}

  MATTR_VAL_GET_INFO	= 10;	{* get page info (stats) *}

{*
 * Virtual memory map purgeable object definitions.
 * Objects that will be needed in the future (forward cached objects) should be queued LIFO.
 * Objects that have been used and are cached for reuse (backward cached) should be queued FIFO.
 * Every user of purgeable memory is entitled to using the highest volatile group (7).
 * Only if a client wants some of its objects to definitely be purged earlier, it can put those in
 * another group. This could be used to make all FIFO objects (in the lower group) go away before
 * any LIFO objects (in the higher group) go away.
 * Objects that should not get any chance to stay around can be marked as "obsolete". They will
 * be emptied before any other objects or pages are reclaimed. Obsolete objects are not emptied
 * in any particular order.
 * 'purgeable' is recognized as the correct spelling. For historical reasons, definitions
 * in this file are spelled 'purgable'.
 *}

{*
 *	Types defined:
 *
 *	vm_purgable_t	purgeable object control codes.
 *}

type
  vm_purgable_t = integer;

{* Enumeration of valid values for vm_purgable_t.*}
const
  VM_PURGABLE_SET_STATE	= 0;	{* set state of purgeable object *}
  VM_PURGABLE_GET_STATE	= 1;	{* get state of purgeable object *}

{*
 * Volatile memory ordering groups (group zero objects are purged before group 1, etc...
 * It is implementation dependent as to whether these groups are global or per-address space.
 * (for the moment, they are global).
 *}
  VM_VOLATILE_GROUP_SHIFT		= 8;
  VM_VOLATILE_GROUP_MASK		= (7 shl VM_VOLATILE_GROUP_SHIFT);

  VM_VOLATILE_GROUP_0			=(0 shl VM_VOLATILE_GROUP_SHIFT);
  VM_VOLATILE_GROUP_1			=(1 shl VM_VOLATILE_GROUP_SHIFT);
  VM_VOLATILE_GROUP_2			=(2 shl VM_VOLATILE_GROUP_SHIFT);
  VM_VOLATILE_GROUP_3			=(3 shl VM_VOLATILE_GROUP_SHIFT);
  VM_VOLATILE_GROUP_4			=(4 shl VM_VOLATILE_GROUP_SHIFT);
  VM_VOLATILE_GROUP_5			=(5 shl VM_VOLATILE_GROUP_SHIFT);
  VM_VOLATILE_GROUP_6			=(6 shl VM_VOLATILE_GROUP_SHIFT);
  VM_VOLATILE_GROUP_7			=(7 shl VM_VOLATILE_GROUP_SHIFT);

  VM_VOLATILE_GROUP_DEFAULT = VM_VOLATILE_GROUP_7;

{*
 * Purgeable behavior
 * Within the same group, FIFO objects will be emptied before objects that are added later.
 * LIFO objects will be emptied after objects that are added later.
 * - Input only, not returned on state queries.
 *}
  VM_PURGABLE_BEHAVIOR_SHIFT  = 6;
  VM_PURGABLE_BEHAVIOR_MASK   = (1 shl VM_PURGABLE_BEHAVIOR_SHIFT);
  VM_PURGABLE_BEHAVIOR_FIFO   = (0 shl VM_PURGABLE_BEHAVIOR_SHIFT);
  VM_PURGABLE_BEHAVIOR_LIFO   = (1 shl VM_PURGABLE_BEHAVIOR_SHIFT);

{*
 * Obsolete object.
 * Disregard volatile group, and put object into obsolete queue instead, so it is the next object
 * to be purged.
 * - Input only, not returned on state queries.
 *}
  VM_PURGABLE_ORDERING_SHIFT	=	5;
  VM_PURGABLE_ORDERING_MASK		= (1 shl VM_PURGABLE_ORDERING_SHIFT);
  VM_PURGABLE_ORDERING_OBSOLETE	= (1 shl VM_PURGABLE_ORDERING_SHIFT);
  VM_PURGABLE_ORDERING_NORMAL		= (0 shl VM_PURGABLE_ORDERING_SHIFT);


{*
 * Obsolete parameter - do not use
 *}
  VM_VOLATILE_ORDER_SHIFT			= 4;
  VM_VOLATILE_ORDER_MASK			= (1 shl VM_VOLATILE_ORDER_SHIFT);
  VM_VOLATILE_MAKE_FIRST_IN_GROUP	= (1 shl VM_VOLATILE_ORDER_SHIFT);
  VM_VOLATILE_MAKE_LAST_IN_GROUP	= (0 shl VM_VOLATILE_ORDER_SHIFT);

{*
 * Valid states of a purgeable object.
 *}
  VM_PURGABLE_STATE_MIN	 = 0;		{* minimum purgeable object state value *}
  VM_PURGABLE_STATE_MAX	 = 3;		{* maximum purgeable object state value *}
  VM_PURGABLE_STATE_MASK = 3;		{* mask to separate state from group *}

  VM_PURGABLE_NONVOLATILE	= 0;	{* purgeable object is non-volatile *}
  VM_PURGABLE_VOLATILE	  = 1;	{* purgeable object is volatile *}
  VM_PURGABLE_EMPTY		    = 2;	{* purgeable object is volatile and empty *}
  VM_PURGABLE_DENY		    = 3; 	{* (mark) object not purgeable *}


// mach_vm.h

{ Module mach_vm }

type
  function_ptr_t = procedure (port: mach_port_t; n : PChar; cnt: mach_msg_type_number_t); cdecl;

  function_table_entry = packed record
    name      : pchar;
    _function : function_ptr_t;
  end;
  function_table_t = ^function_table_entry;

const
  mach_vm_MSG_COUNT	= 19;

{* Routine mach_vm_allocate
 *	Allocate zero-filled memory in the address space
 *	of the target task, either at the specified address,
 *	or wherever space can be found (controlled by flags),
 *	of the specified size.  The address at which the
 *	allocation actually took place is returned.
*}

function  mach_vm_allocate(target: vm_map_t; address : pmach_vm_address_t;
	size: mach_vm_size_t; flags: integer): kern_return_t; cdecl; external;

{* Routine mach_vm_deallocate
 *	Deallocate the specified range from the virtual
 *	address space of the target virtual memory map.
 *}
function mach_vm_deallocate(target: vm_map_t; address : mach_vm_address_t;
	size : mach_vm_size_t): kern_return_t; cdecl; external;

{* Routine mach_vm_protect
 *	Set the current or maximum protection attribute
 *	for the specified range of the virtual address
 *	space of the target virtual memory map.  The current
 *	protection limits the memory access rights of threads
 *	within the map; the maximum protection limits the accesses
 *	that may be given in the current protection.
 *	Protections are specified as a set of (read, write, execute)
 *	*permissions*.
*}
function mach_vm_protect (target_task: vm_map_t;	address: mach_vm_address_t;
	size: mach_vm_size_t;	set_maximum: boolean_t;	new_protection: vm_prot_t): kern_return_t; cdecl; external;

{* Routine mach_vm_inherit
 *	Set the inheritance attribute for the specified range
 *	of the virtual address space of the target address space.
 *	The inheritance value is one of (none, copy, share), and
 *	specifies how the child address space should acquire
 *	this memory at the time of a task_create call.
*}
function mach_vm_inherit (target_task: vm_map_t; address: mach_vm_address_t;	size: mach_vm_size_t;
	new_inheritance: vm_inherit_t): kern_return_t; cdecl; external;

{* Routine mach_vm_read
 *	Returns the contents of the specified range of the
 *	virtual address space of the target task.  [The
 *	range must be aligned on a virtual page boundary,
 *	and must be a multiple of pages in extent.  The
 *	protection on the specified range must permit reading.]
*}

function mach_vm_read(target_task: vm_map_t; address: mach_vm_address_t;
	size: mach_vm_size_t; data: pvm_offset_t; dataCnt: Pmach_msg_type_number_t): kern_return_t; cdecl; external;

{* Routine mach_vm_read_list
 * List corrollary to vm_read, returns mapped contents of specified
 * ranges within target address space.
*}

function mach_vm_read_list(target_task: vm_map_t;	data_list: mach_vm_read_entry_t;
	count: natural_t): kern_return_t; cdecl; external;

{* Routine mach_vm_write
 *	Writes the contents of the specified range of the
 *	virtual address space of the target task.  [The
 *	range must be aligned on a virtual page boundary,
 *	and must be a multiple of pages in extent.  The
 *	protection on the specified range must permit writing.]
*}

function mach_vm_write(target_task: vm_map_t;	address: mach_vm_address_t;	data: mach_vm_address_t;
	dataCnt: mach_msg_type_number_t): kern_return_t; cdecl; external;

{* Routine mach_vm_copy
 *	Copy the contents of the source range of the virtual
 *	address space of the target task to the destination
 *	range in that same address space.  [Both of the
 *	ranges must be aligned on a virtual page boundary,
 *	and must be multiples of pages in extent.  The
 *	protection on the source range must permit reading,
 *	and the protection on the destination range must
 *	permit writing.]
*}

function mach_vm_copy(target_task: vm_map_t;source_address: mach_vm_address_t;
  size: mach_vm_size_t; 	dest_address: mach_vm_address_t): kern_return_t; cdecl; external;

{* Routine mach_vm_read_overwrite
 *	Returns the contents of the specified range of the
 *	virtual address space of the target task.  [There
 *	are no alignment restrictions, and the results will
 *      overwrite the area pointed to by data - which must
 *      already exist. The protection on the specified range
 *	must permit reading.]
*}
function mach_vm_read_overwrite(target_task: vm_map_t;	address: mach_vm_address_t;
	size: mach_vm_size_t;	data: mach_vm_address_t; var outsize: mach_vm_size_t): kern_return_t; cdecl; external;

{* Routine mach_vm_msync
   no def :(
*}

function  mach_vm_msync(target_task: vm_map_t;	address: mach_vm_address_t;
	size: mach_vm_size_t; sync_flags: vm_sync_t): kern_return_t; cdecl; external;

{* Routine mach_vm_behavior_set
 *	Set the paging behavior attribute for the specified range
 *	of the virtual address space of the target task.
 *	The behavior value is one of (default, random, forward
 *	sequential, reverse sequential) and indicates the expected
 *	page reference pattern for the specified range.
*}

function mach_vm_behavior_set (target_task: vm_map_t; address: mach_vm_address_t;
	size: mach_vm_size_t;	new_behavior: vm_behavior_t): kern_return_t; cdecl; external;

{* Routine mach_vm_map
 *	Map a user-supplie memory object into the virtual address
 *	space of the target task.  If desired (anywhere is TRUE),
 *	the kernel will find a suitable address range of the
 *	specified size; else, the specific address will be allocated.
 *
 *	The beginning address of the range will be aligned on a virtual
 *	page boundary, be at or beyond the address specified, and
 *	meet the mask requirements (bits turned on in the mask must not
 *	be turned on in the result); the size of the range, in bytes,
 *	will be rounded	up to an integral number of virtual pages.
 *
 *	The memory in the resulting range will be associated with the
 *	specified memory object, with the beginning of the memory range
 *	referring to the specified offset into the memory object.
 *
 *	The mapping will take the current and maximum protections and
 *	the inheritance attributes specified; see the vm_protect and
 *	vm_inherit calls for a description of these attributes.
 *
 *	If desired (copy is TRUE), the memory range will be filled
 *	with a copy of the data from the memory object; this copy will
 *	be private to this mapping in this target task.  Otherwise,
 *	the memory in this mapping will be shared with other mappings
 *	of the same memory object at the same offset (in this task or
 *	in other tasks).  [The Mach kernel only enforces shared memory
 *	consistency among mappings on one host with similar page alignments.
 *	The user-defined memory manager for this object is responsible
 *	for further consistency.]
*}

function mach_vm_map(target_task: pvm_map_t; address: pmach_vm_address_t; size: mach_vm_size_t;
	mask: mach_vm_offset_t;	flags:integer;	_object :mem_entry_name_port_t; offset: memory_object_offset_t;
	copy: boolean_t;	cur_protection: vm_prot_t;	max_protection: vm_prot_t;
  inheritance: vm_inherit_t): kern_return_t; cdecl; external;

{* Routine mach_vm_machine_attribute
 *	Set/Get special properties of memory associated
 *	to some virtual address range, such as cachability,
 *	migrability, replicability.  Machine-dependent.
*}

function mach_vm_machine_attribute (target_task: vm_map_t; 	address: mach_vm_address_t; 	size: mach_vm_size_t;
	attribute: vm_machine_attribute_t; value: pvm_machine_attribute_val_t): kern_return_t; cdecl; external;

{* Routine mach_vm_remap
 *      Map portion of a task's address space.
*}

function mach_vm_remap (target_task: vm_map_t;	target_address: pmach_vm_address_t;
	size: mach_vm_size_t;	mask: mach_vm_offset_t;	anywhere: boolean_t; 	src_task: vm_map_t;
	src_address: mach_vm_address_t; copy: boolean_t;	cur_protection: pvm_prot_t;
	max_protection: pvm_prot_t; inheritance: vm_inherit_t): kern_return_t; cdecl; external;

{* Routine mach_vm_page_query
*      Give the caller information on the given location in a virtual
*      address space.  If a page is mapped return ref and dirty info.
*}

function mach_vm_page_query(target_map: vm_map_t;	offset: mach_vm_offset_t;
	disposition: pinteger_t;	ref_count : pinteger_t): kern_return_t; cdecl; external;

{* Routine mach_vm_region_recurse
  no def :(
*}

function  mach_vm_region_recurse(target_task: vm_map_t; 	address: pmach_vm_address_t;
	size: pmach_vm_size_t; 	nesting_depth: pnatural_t; 	info: vm_region_recurse_info_t;
	infoCnt: pmach_msg_type_number_t): kern_return_t; cdecl; external;

{* Routine mach_vm_region
 *      Returns information about the contents of the virtual
 *      address space of the target task at the specified
 *      address.  The returned protection, inheritance, sharing
 *      and memory object values apply to the entire range described
 *      by the address range returned; the memory object offset
 *      corresponds to the beginning of the address range.
 *      [If the specified address is not allocated, the next
 *      highest address range is described.  If no addresses beyond
 *      the one specified are allocated, the call returns KERN_NO_SPACE.]
*}

function mach_vm_region(target_task: vm_map_t;
	address: pmach_vm_address_t;size: pmach_vm_size_t;flavor: vm_region_flavor_t;
	info: vm_region_info_t;	infoCnt:
  pmach_msg_type_number_t;	object_name: pmach_port_t): kern_return_t; cdecl; external;

{* Routine _mach_make_memory_entry
 *	Allow application level processes to create named entries which
 *	correspond to mapped portions of their address space.  These named
 *	entries can then be manipulated, shared with other processes in
 *	other address spaces and ultimately mapped in ohter address spaces
 *
 *	THIS INTERFACE IS STILL EVOLVING.
*}

function _mach_make_memory_entry(	target_task: vm_map_t;	size: pmemory_object_size_t;
	offset: pmemory_object_offset_t;	permission: vm_prot_t;	object_handle: mem_entry_name_port_t;
	parent_handle: mem_entry_name_port_t): kern_return_t; cdecl; external;

{* Routine mach_vm_purgable_control
 *	Control behavior and investigate state of a "purgable" object in
 *	the virtual address space of the target task.  A purgable object is
 *	created via a call to mach_vm_allocate() with VM_FLAGS_PURGABLE
 *	specified.  See the routine implementation for a complete
 *	definition of the routine.
*}

function mach_vm_purgable_control(target_task: vm_map_t;	address: mach_vm_address_t;
	control: vm_purgable_t;	state: pinteger): kern_return_t;  cdecl; external;


// mach/error.h

// macroses

function err_system(err: integer): Integer; inline;
function err_sub(err: Integer): Integer; inline;

function err_get_system(err: Integer): integer; inline;
function err_get_sub(err: Integer): integer; inline;
function err_get_code(err: Integer): integer; inline;

{*	unix errors get lumped into one subsystem  *}
function unix_err(errno: Integer): integer; inline;



implementation

function err_system(err: integer): Integer; inline;
begin
  Result := ((err and $3f) shr 26);
end;

function err_sub(err: Integer): Integer; inline;
begin
  Result := ((err and $fff) shr 14);
end;

function err_get_system(err: Integer): integer; inline;
begin
  Result := ((err shr 26) and $3f);
end;

function err_get_sub(err: Integer): integer; inline;
begin
  Result := ((err shr 14) and $FFF)
end;

function err_get_code(err: Integer): integer; inline;
begin
  Result := ((err shr 14) and $FFF);
end;

function unix_err(errno: Integer): integer; inline;
begin
  Result := err_kern or err_sub(3) or errno;
end;

//err_get_system(err)	(((err)>>26)&0x3f) ((| shr 26) and $3f);
//err_get_sub(err)	(((err)>>14)&0xfff)  ((| shr 14) and $FFF)
//err_get_code(err)	((err)&0x3fff)       ((| and $3FFF);

//#define	unix_err(errno)		(err_kern|err_sub(3)|errno)


end.

