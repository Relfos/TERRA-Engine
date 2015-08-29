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

{$IFDEF ANDROID}
{.$DEFINE USEJAVATHREADS}
{$DEFINE USEPASCALTHREADS}
{$ELSE}

{$DEFINE USEPASCALTHREADS}

{$IFNDEF USEPASCALTHREADS}
{$IFNDEF WINDOWS}
{$DEFINE USEPTHREADS}
{$ENDIF}
{$ENDIF}


{$ENDIF}
{$ENDIF}

Interface
Uses TERRA_Utils, TERRA_Log, TERRA_Application, TERRA_Mutex
{$IFNDEF DISABLETHREADS}
{$IFDEF WINDOWS},Windows{$ENDIF}
{$IFDEF ANDROID},TERRA_Java{$ENDIF}
{$IFDEF USEPASCALTHREADS},Classes{$ENDIF}
{$ENDIF}
;

Type
   ThreadHandle = Cardinal;

{$IFDEF USEPTHREADS}
  PThreadHandle = ^ThreadHandle;
  
  __start_routine_t = pointer;

Const
  PTHREAD_CREATE_DETACHED = 1;

Type
  ThreadStartRoutine = Function(P:Pointer):Integer; Cdecl;

Function pthread_create(Var Handle:ThreadHandle; Attr:Pointer; start_routine:ThreadStartRoutine; Arg:Pointer):Integer; cdecl; external 'libc.so';
Procedure pthread_exit(value_ptr:Pointer); cdecl;external 'libc.so';
{$IFNDEF ANDROID}
Function pthread_cancel(Handle:ThreadHandle):Integer; cdecl;external 'libc.so';
{$ENDIF}
//function pthread_attr_init(__attr:ppthread_attr_t):longint;cdecl;external 'libc.so';
//Function pthread_attr_setdetachstate(Var attr:Pointer; __detachstate:longint):longint;cdecl;external 'libc.so';

(*
Type
  ppthread_cond_t = ^pthread_cond_t;
  ppthread_condattr_t = ^pthread_condattr_t;

function pthread_cond_init(__cond:ppthread_cond_t; __cond_attr:ppthread_condattr_t):longint;cdecl;external 'libc.so';
function pthread_cond_destroy(__cond:ppthread_cond_t):longint;cdecl;external 'libc.so';
function pthread_cond_signal(__cond:ppthread_cond_t):longint;cdecl;external 'libc.so';
function pthread_cond_broadcast(__cond:ppthread_cond_t):longint;cdecl;external 'libc.so';
function pthread_cond_wait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t):longint;cdecl;external 'libc.so';*)
{$ENDIF}

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

      Procedure Release; Override;

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

  TERRAThread = Class{$IFDEF USEPASCALTHREADS}(TThread){$ELSE}(TERRAObject){$ENDIF}
    Protected
		  _ID:Cardinal;

      {$IFNDEF DISABLETHREADS}
      {$IFNDEF USEJAVATHREADS}
      {$IFNDEF USEPASCALTHREADS}
		  _Handle:ThreadHandle;
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}

      Procedure Finish;

    Public
      Constructor Create();

      {$IFDEF USEPASCALTHREADS}
      Procedure Release; Virtual;
      {$ELSE}
      Procedure Execute; Virtual; Abstract;
      {$ENDIF}

      Procedure Shutdown();
  End;

  ThreadPool = Class;

  TaskDispatcher = Class(TERRAThread)
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

      {$IFNDEF DISABLETHREADS}
      _MainThread:PtrUInt;
      {$ENDIF}

      _CriticalSection:CriticalSection;
      _Semaphore:Semaphore;

      Procedure KillTask(MyTask:Task);

      {$IFNDEF DISABLETHREADS}
      Function AddThreadToPool(MyTask:TERRAObject):Boolean;
      {$ENDIF}

	  Public
		  Procedure Init; Override;

		  Procedure Release; Override;

		  Function GetNextTask:Task;
		  Property Active:Boolean Read _Active;

		  Procedure RunTask(MyTask:Task; InBackGround:Boolean = True; Group:TaskGroup = Nil; Priority:Integer = DefaultPriority);
		  Function TasksPending:Integer;

		  Procedure CancelTasks;

      {$IFNDEF DISABLETHREADS}
      Property MainThread:PtrUInt Read _MainThread;
      {$ENDIF}

		  Class Function Instance:ThreadPool;
    End;

{$IFDEF ANDROID}
Function InternalThreadDispatcher(P:Pointer):Integer; Cdecl;
{$ENDIF}

Implementation
Uses TERRA_Error, TERRA_OS;

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
Var
  T:TERRAThread;
Begin
  T := TERRAThread(P);

  Log(logDebug, 'Threads', 'Running new thread...');
  T.Execute();

  Log(logDebug, 'Threads', 'Thread finished...');

  T.Finish();

  ReleaseObject(T);

{$IFDEF USEPASCALTHREADS}
  T.Destroy();
{$ENDIF}

  Result := 0;
End;

{ Thread }
Constructor TERRAThread.Create();
Begin
  Log(logDebug, 'Threads', 'Starting thread: '+Self.ClassName);
{$IFDEF DISABLETHREADS}
  Self.Execute();
{$ELSE}

{$IFDEF USEJAVATHREADS}
  AndroidApplication(Application.Instance).SpawnThread(Self);
{$ELSE}
{$IFDEF USEPASCALTHREADS}
  Inherited Create(False);
{$ELSE}
{$IFDEF WINDOWS}
	_Handle := BeginThread(Nil, 0, InternalThreadDispatcher, Self, 0, _ID);
{$ELSE}
  pthread_create(_Handle, Nil, InternalThreadDispatcher, Self);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

// SetThreadPriority(_Handle, THREAD_PRIORITY_TIME_CRITICAL);
//self.Priority := tpTimeCritical;

End;

{$IFDEF USEPASCALTHREADS}
Procedure TERRAThread.Release;
Begin
  // dummy pseudo-destructor
End;
{$ENDIF}

Procedure TERRAThread.Finish;
Begin
  Log(logDebug, 'Thread','Terminating...');
{$IFNDEF DISABLETHREADS}
  {$IFDEF ANDROID}
  Java_DetachThread();
  {$ELSE}

  {$IFNDEF USEPASCALTHREADS}
  {$IFDEF WINDOWS}
  ExitThread(0);
  {$ELSE}
  pthread_exit(Nil);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
End;

Procedure TERRAThread.Shutdown;
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
  Self.Finish();
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

	MyTask._Time := Application.GetTime();
	MyTask._Priority := Priority;

  {$IFDEF DISABLETHREADS}
    MyTask.Execute();
    If Group = Nil Then
      ReleaseObject(MyTask);
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
      ReleaseObject(MyTask);
    Exit;
  End;

  {$IFNDEF DISABLETHREADS}
  If PtrUInt(GetCurrentThreadId()) <> _MainThread Then
    Application.Instance.PostCallback(AddThreadToPool, MyTask)
  Else
    AddThreadToPool(MyTask);
  {$ENDIF}
End;

{$IFNDEF DISABLETHREADS}
Function ThreadPool.AddThreadToPool(MyTask:TERRAObject):Boolean;
Begin
  _CriticalSection.Lock();
	If (_ThreadCount<_MaxThreads) Then
	Begin
		_Threads[_ThreadCount] := TaskDispatcher.Create(Self);
		Inc(_ThreadCount);
	End;

  Inc(_PendingTaskCount);

  If (_PendingTaskCount>Length(_PendingTaskList)) Then
	  SetLength(_PendingTaskList, _PendingTaskCount);

  _PendingTaskList[Pred(_PendingTaskCount)] := Task(MyTask);

	_CriticalSection.Unlock();

  _Semaphore.Signal();

  Result := False;
End;
{$ENDIF}

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

  _CriticalSection := CriticalSection.Create({Self.ClassName});
  _Semaphore := Semaphore.Create(_MaxThreads);

	_Active := True;
	_ThreadCount := 0;
  _PendingTaskCount := 0;
  _RunningTaskCount := 0;

  {$IFNDEF DISABLETHREADS}
  _MainThread := PtrUInt(GetCurrentThreadId());
  {$ENDIF}

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

Procedure ThreadPool.Release;
Var
  I, Count:Integer;
Begin
	_Active := False;

  Self.CancelTasks();

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
    _Semaphore.Signal();
  Until (Count<=0);

  For I:=0 To Pred(_MaxThreads) Do
  If (Assigned(_Threads[i])) Then
  Begin
    _Threads[I].Shutdown();

  {$IFDEF USEPASCALTHREADS}
    _Threads[I].Release();
    _Threads[I].Destroy();
    _Threads[I] := Nil;
  {$ELSE}
    ReleaseObject(_Threads[I]);
  {$ENDIF}
  End;

  ReleaseObject(_CriticalSection);
  ReleaseObject(_Semaphore);

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
    ReleaseObject(MyTask);

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
Procedure TaskGroup.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TaskCount) Do
    ReleaseObject(_Tasks[I]);

  SetLength(_Tasks, 0);
  _TaskCount := 0;
End;

Function TaskGroup.GetProgress: Integer;
Var
  I, Finished:Integer;
  N,S:Single;
Begin
  If (_TaskCount<=0) Then
  Begin
    Result := 100;
    Exit;
  End;

  N := 0;
  S := 1/_TaskCount;
  Finished := 0;

  For I:=0 To Pred(_TaskCount) Do
  Begin
    N := N + _Tasks[I].Progress * S;
    If _Tasks[I].Progress>=100 Then
      Inc(Finished);
  End;

  If (Finished>= _TaskCount) Then
  Begin
    Result := 100;
    Exit;
  End;

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

