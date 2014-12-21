{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_Threads
 * Implements portable threads
 ***********************************************************************************************************************
}

Unit TERRA_Threads;

{$I terra.inc}

{$IFNDEF DISABLETHREADS}
{$DEFINE USEPASCALTHREADS}
{$ENDIF}

Interface
Uses TERRA_Utils, TERRA_Log, TERRA_Application, TERRA_Mutex
{$IFNDEF DISABLETHREADS}
{$IFDEF WINDOWS},Windows{$ENDIF}
{$IFDEF ANDROID},TERRA_Java{$ENDIF}
{$IFDEF USEPASCALTHREADS},Classes{$ENDIF}
{$ENDIF}
;

{-$DEFINE USEPTHREADS}

{$IFDEF USEPTHREADS}
type
 ppthread_t = ^pthread_t;
 ppthread_attr_t = ^pthread_attr_t;
 ppthread_cond_t = ^pthread_cond_t;
 ppthread_condattr_t = ^pthread_condattr_t;

 __start_routine_t = pointer;

Const
 PTHREAD_CREATE_DETACHED = 1;

Type
  TStartRoutine = Function(P:Pointer):Integer; Cdecl;

function pthread_create(__thread:ppthread_t; __attr:ppthread_attr_t;__start_routine:TStartRoutine;__arg:pointer):longint;cdecl;external 'libc.so';
procedure pthread_exit(value_ptr:Pointer); cdecl;external 'libc.so';
{$IFNDEF ANDROID}
function pthread_cancel(__thread:pthread_t):Integer; cdecl;external 'libc.so';
{$ENDIF}
function pthread_attr_init(__attr:ppthread_attr_t):longint;cdecl;external 'libc.so';
function pthread_attr_setdetachstate(__attr:ppthread_attr_t; __detachstate:longint):longint;cdecl;external 'libc.so';

function pthread_cond_init(__cond:ppthread_cond_t; __cond_attr:ppthread_condattr_t):longint;cdecl;external 'libc.so';
function pthread_cond_destroy(__cond:ppthread_cond_t):longint;cdecl;external 'libc.so';
function pthread_cond_signal(__cond:ppthread_cond_t):longint;cdecl;external 'libc.so';
function pthread_cond_broadcast(__cond:ppthread_cond_t):longint;cdecl;external 'libc.so';
function pthread_cond_wait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t):longint;cdecl;external 'libc.so';

{$ENDIF}

(* function sem_init(__sem:Psem_t; __pshared:cint;
__value:dword):cint;cdecl;external libthreads;
  function sem_destroy(__sem:Psem_t):cint;cdecl;external libthreads;
  function sem_close(__sem:Psem_t):cint;cdecl;external libthreads;
  function sem_unlink(__name:PAnsiChar):cint;cdecl;external libthreads;
  function sem_wait(__sem:Psem_t):cint;cdecl;external libthreads;
  function sem_trywait(__sem:Psem_t):cint;cdecl;external libthreads;
  function sem_post(__sem:Psem_t):cint;cdecl;external libthreads;
  function sem_getvalue(__sem:Psem_t; __sval:pcint):cint;cdecl;external
libthreads;
  function sem_timedwait(__sem: Psem_t; __abstime:
Ptimespec):cint;cdecl; external libthreads;
*)

Const
  DefaultThreadDiscardTime = 3000;
  DefaultPriority = 0;

Type
  Task = Class;

  TaskGroup = Class(TERRAObject)
    Protected
      _Tasks:Array Of Task;
      _TaskCount:Integer;

      Function GetProgress():Integer;

    Public

      Destructor Destroy; Override;

      Function IsFinished():Boolean;

      Property Progress:Integer Read GetProgress;
  End;

  Task = Class(TERRAObject)
    Protected
	    _Time:Cardinal;
	    _Priority:Integer;

      _Group:TaskGroup;
      _Progress:Integer;

      Function GetProgress():Integer;

    Public
      Procedure Execute; Virtual;

      Property Progress:Integer Read GetProgress;
  End;

  Thread = Class{$IFDEF USEPASCALTHREADS}(TThread){$ENDIF}
    Protected
		  _ID:Cardinal;

      {$IFNDEF DISABLETHREADS}
      {$IFNDEF USEPASCALTHREADS}
      {$IFDEF WINDOWS}
		  _Handle:Cardinal;
      {$ELSE}
      _Handle:pthread_t;
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}

      Procedure Terminate;

    Public
      Constructor Create();

      {$IFNDEF USEPASCALTHREADS}
      Procedure Execute; Virtual; Abstract;
      {$ENDIF}

      Procedure Shutdown();
  End;

  ThreadPool = Class;

  TaskDispatcher = Class(Thread)
    Protected
      _Pool:ThreadPool;
      _Active:Boolean;

    Public
      Constructor Create(Parent:ThreadPool);
      Procedure Execute; Override;
  End;

  ThreadPool = Class(ApplicationComponent)
	  Protected
		  _Threads:Array Of TaskDispatcher;
      _MaxThreads:Integer;

		  _PendingTaskList:Array Of Task;
      _PendingTaskCount:Integer;

		  _RunningTaskList:Array Of Task;
      _RunningTaskCount:Integer;

		  _Active:Boolean;
		  _ThreadCount:Integer;

      _CriticalSection:CriticalSection;
      _Semaphore:Semaphore;

      Procedure KillTask(MyTask:Task);

	  Public
		  Procedure Init; Override;

		  Destructor Destroy; Override;

		  Function GetNextTask:Task;
		  Property Active:Boolean Read _Active;

		  Procedure RunTask(MyTask:Task; InBackGround:Boolean = True; Group:TaskGroup = Nil; Priority:Integer = DefaultPriority);
		  Function TasksPending:Integer;

		  Procedure CancelTasks;

		  Class Function Instance:ThreadPool;
    End;

Implementation
Uses TERRA_OS;

Var
  _ThreadPool_Instance:ApplicationObject  = Nil;

{$IFDEF WINDOWS}
{ SyncEvent is an Event handle that is signaled every time a thread wishes to
  synchronize with the main thread or is terminating.  This handle us suitable
  for use with WaitForMultipleObjects.  When this object is signaled,
  CheckSynchronize *must* be called in order to reset the event.  Do not call
  ResetEvent on this handle, or background threads may hang waiting for
  Synchronize to return.
}
{$IFNDEF USEPASCALTHREADS}
  SyncEvent: THandle;
{$ENDIF}
{$ENDIF}


Function InternalThreadDispatcher(P:Pointer):Integer; {$IFNDEF WINDOWS}Cdecl; {$ENDIF}
Begin
  Log(logDebug, 'Threads', 'Running new thread...');
  Thread(P).Execute();

  Log(logDebug, 'Threads', 'Thread finished...');

  Thread(P).Destroy();
  Thread(P).Terminate();
  Result := 0;
End;

{ Thread }
Constructor Thread.Create();
Begin
  Log(logDebug, 'Threads', 'Starting thread: '+Self.ClassName);
{$IFDEF DISABLETHREADS}
  Self.Execute();
{$ELSE}

{$IFDEF USEPASCALTHREADS}
  Inherited Create(False);
{$ELSE}
{$IFDEF WINDOWS}
	_Handle := BeginThread(Nil, 0, InternalThreadDispatcher, Self, 0, _ID);
{$ELSE}
  pthread_create(@_Handle, Nil, InternalThreadDispatcher, Self);
{$ENDIF}
{$ENDIF}
{$ENDIF}
End;

Procedure Thread.Terminate;
Begin
{$IFNDEF DISABLETHREADS}
  {$IFDEF ANDROID}
  Java_DetachThread();
  {$ENDIF}

  {$IFNDEF USEPASCALTHREADS}
  {$IFDEF WINDOWS}
  ExitThread(0);
  {$ELSE}
  pthread_exit(Nil);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
End;

Procedure Thread.Shutdown;
Begin
{$IFNDEF DISABLETHREADS}
  {$IFDEF USEPASCALTHREADS}
  Self.Terminate();
  {$ELSE}
    {$IFDEF WINDOWS}
    TerminateThread(_Handle, 0);
    {$ELSE}
    {$IFNDEF ANDROID}
    pthread_cancel(_Handle);
    {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
End;

Class Function ThreadPool.Instance;
Begin
  If (Not Assigned(_ThreadPool_Instance)) Then
  Begin
      Log(logDebug, 'ThreadPool','Creating thread pool!');
    _ThreadPool_Instance := InitializeApplicationComponent(ThreadPool, Nil);
  End;

  Result := ThreadPool(_ThreadPool_Instance.Instance);
End;

Constructor TaskDispatcher.Create(Parent:ThreadPool);
Begin
  Self._Pool := Parent;
  _Active := True;
  Inherited Create;
End;

Procedure TaskDispatcher.Execute();
Var
  MyTask:Task;
Begin
  Repeat
    _Pool._CriticalSection.Lock();
    MyTask := _Pool.GetNextTask();
    _Pool._CriticalSection.Unlock();
    If (Assigned(MyTask)) Then
    Begin
      MyTask.Execute();
      MyTask._Progress := 100;
      _Pool.KillTask(MyTask);
    End Else
    Begin
      _Pool._Semaphore.Wait();
    End;
  Until (Not _Pool.Active) {$IFDEF USEPASCALTHREADS}Or (Self.Terminated){$ENDIF};

  _Active := False;
End;

Function ThreadPool.TasksPending:Integer;
Begin
	Result := _PendingTaskCount;
End;

{Function ThreadPool.HasTask(Data:Pointer):Boolean;
Var
  I:Integer;
Begin
  Result := False;
  For I:=0 To Pred(Length(_TaskList)) Do
  If (_TaskList[I].Data = Data) Then
  Begin
    Result := True;
    Exit;
  End;
End;
}
Procedure ThreadPool.RunTask(MyTask:Task; InBackGround:Boolean; Group:TaskGroup; Priority:Integer);
Begin
  If MyTask = Nil Then
    Exit;

	MyTask._Time := GetTime();
	MyTask._Priority := Priority;

  {$IFDEF DISABLETHREADS}
    MyTask.Execute();
    If Group = Nil Then
      MyTask.Destroy();
    Exit;
  {$ENDIF}

  If Assigned(Group) Then
  Begin
    MyTask._Group := Group;
    Inc(Group._TaskCount);
    SetLength(Group._Tasks, Group._TaskCount);
    Group._Tasks[Pred(Group._TaskCount)] := MyTask;
  End;

  If (Not InBackGround) Then
  Begin
    MyTask.Execute();
    If Group = Nil Then
      MyTask.Destroy();
    Exit;
  End;

  {$IFNDEF DISABLETHREADS}
  _CriticalSection.Lock();
	If (_ThreadCount<_MaxThreads) Then
	Begin
		_Threads[_ThreadCount] := TaskDispatcher.Create(Self);
		Inc(_ThreadCount);
	End;

  Inc(_PendingTaskCount);

  If (_PendingTaskCount>Length(_PendingTaskList)) Then
	  SetLength(_PendingTaskList, _PendingTaskCount);

  _PendingTaskList[Pred(_PendingTaskCount)] := MyTask;

	_CriticalSection.Unlock();

  _Semaphore.Release();  
  {$ENDIF}
End;

Function ThreadPool.GetNextTask:Task;
Var
  HP,k:Cardinal;
	I, Next:Integer;
  Temp:Task;
Begin
  If (_PendingTaskCount<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;

	HP := 0;
	Next := -1;
	For I:=0 To Pred(_PendingTaskCount) Do
	Begin
		k := _PendingTaskList[i]._Time; // - _PendingTaskList[i]._Priority;
		If (next<0) Or (k<HP) Then
		Begin
			HP := k;
			next := i;
		End;
	End;

	If (Next>=0) Then
	Begin
    Temp := _PendingTaskList[Next];
   _PendingTaskList[0] := Temp;
    _PendingTaskList[Next] := Temp;

		Result := _PendingTaskList[0];
    For I:=0 To _PendingTaskCount - 2 Do
		  _PendingTaskList[I] := _PendingTaskList[I+1];

    Inc(_RunningTaskCount);
    If _RunningTaskCount>Length(_RunningTaskList) Then
      SetLength(_RunningTaskList, _RunningTaskCount);

    _RunningTaskList[Pred(_RunningTaskCount)] := Result;

    Dec(_PendingTaskCount);
	End Else
		Result := Nil;
End;

Procedure ThreadPool.Init;
Var
  I:Integer;
Begin
  If (Application.Instance<>Nil) Then
    _MaxThreads := Application.Instance.CPUCores
  Else
    _MaxThreads := 2;
    
  _CriticalSection := CriticalSection.Create(Self.ClassName);
  _Semaphore := Semaphore.Create(Self.ClassName, _MaxThreads);

	_Active := True;
	_ThreadCount := 0;
  _PendingTaskCount := 0;
  _RunningTaskCount := 0;

  SetLength(_Threads, _MaxThreads);
	For I:=0 To Pred(_MaxThreads) Do
		_Threads[i] := Nil;
End;

Procedure ThreadPool.CancelTasks;
Begin
	_CriticalSection.Lock;
	_PendingTaskCount := 0;
	_CriticalSection.Unlock;
End;

Destructor ThreadPool.Destroy;
Var
  I, Count:Integer;
Begin
	_Active := False;

  Repeat
    Count := 0;
	  For I:=0 To Pred(_MaxThreads) Do
  	Begin
		  If (Not Assigned(_Threads[i])) Then
			  Continue;

      If (_Threads[I]._Active) Then
        Inc(Count);
	  End;

    Application.Instance.Yeld();
    _Semaphore.Release();
  Until (Count<=0);

  For I:=0 To Pred(_MaxThreads) Do
  If (Assigned(_Threads[i])) Then
  Begin
    _Threads[I].Shutdown();
		_Threads[I].Destroy;
  End;

  _CriticalSection.Destroy;
  _Semaphore.Destroy();

  _ThreadPool_Instance := Nil;
End;

Procedure ThreadPool.KillTask(MyTask: Task);
Var
  I,N:Integer;
Begin
  If (MyTask=Nil) Then
    Exit;

  _CriticalSection.Lock();

  N := -1;
  For I:=0 To Pred(_RunningTaskCount) Do
  If (_RunningTaskList[I] = MyTask) Then
  Begin
    N := I;
    Break;
  End;

  If (N>=0) Then
  Begin
    _RunningTaskList[N] := _RunningTaskList[Pred(_RunningTaskCount)];
    Dec(_RunningTaskCount);
  End;

  If Not Assigned(MyTask._Group) Then
    MyTask.Destroy();

  _CriticalSection.Unlock();
End;

{ Task }
Procedure Task.Execute;
Begin
  // do nothing
End;


Function Task.GetProgress: Integer;
Begin
  Result := _Progress;
  If (Result<0) Then
    Result := 0
  Else
  If (Result>100) Then
    Result := 100;
End;

{ TaskGroup }
Destructor TaskGroup.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TaskCount) Do
    _Tasks[I].Destroy();

  SetLength(_Tasks, 0);
  _TaskCount := 0;
End;

Function TaskGroup.GetProgress: Integer;
Var
  I:Integer;
  N,S:Single;
Begin
  If (_TaskCount<=0) Then
  Begin
    Result := 100;
    Exit;
  End;

  N := 0;
  S := 1/_TaskCount;
  For I:=0 To Pred(_TaskCount) Do
    N := N + _Tasks[I].Progress * S;

  Result := Trunc(N);

  If (Result<0) Then
    Result := 0
  Else
  If (Result>100) Then
    Result := 100;
End;

Function TaskGroup.IsFinished:Boolean;
Begin
  Result := Self.Progress>=100;
End;

Initialization
{$IFNDEF DISABLETHREADS}
{$IFNDEF USEPASCALTHREADS}
{$IFDEF WINDOWS}
  SyncEvent := CreateEvent(nil, True, False, '');
	If (syncEvent = 0) Then
	Begin
		RaiseError('Could not initialize thread system.');
		Exit;
	End;
{$ENDIF}
{$ENDIF}
{$ENDIF}

  
Finalization
{$IFNDEF DISABLETHREADS}
{$IFNDEF USEPASCALTHREADS}
{$IFDEF WINDOWS}
  CloseHandle(SyncEvent);
{$ENDIF}
{$ENDIF}
{$ENDIF}
End.

