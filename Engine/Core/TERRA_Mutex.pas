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
}

{ Implements a portable mutex class }
Unit TERRA_Mutex;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils,
{$IFDEF DEBUG_LOCKS} TERRA_Log, {$ENDIF}
{$IFDEF WINDOWS}Windows
{$ELSE}cmem, {ctypes,baseunix,}unixtype{$ENDIF};

{$DEFINE HAS_SEMAPHORES}
  
{$IFNDEF WINDOWS}
Type
 ppthread_mutex_t = ^pthread_mutex_t;
 ppthread_mutexattr_t = ^pthread_mutexattr_t;

 ppthread_sem_t = ^pthread_sem_t;
 pthread_sem_t = integer;

Const
  LibC = 'libc.so';

Function pthread_mutex_init(__mutex:ppthread_mutex_t; __mutex_attr:ppthread_mutexattr_t):longint; cdecl; External LibC;
Function pthread_mutex_destroy(__mutex:ppthread_mutex_t):longint; Cdecl; External LibC;
Function pthread_mutex_lock(__mutex: ppthread_mutex_t):longint; Cdecl; External LibC;
Function pthread_mutex_unlock(__mutex: ppthread_mutex_t):longint; Cdecl; External LibC;

{$IFNDEF HAS_SEMAPHORES}
Function sem_init(sem:ppthread_sem_t; pshared:Integer; value:Cardinal):Integer; Cdecl; External LibC;
Function sem_wait(sem:ppthread_sem_t):Integer; Cdecl; External LibC;
Function sem_post(sem:ppthread_sem_t):Integer; Cdecl; External LibC;
Function sem_destroy(sem:ppthread_sem_t):Integer; Cdecl; External LibC;
{$ENDIF}
{$ENDIF}

Type
  { Used to support thread-safe code}
  CriticalSection = Class(TERRAObject)
    Protected
//      _Name:String;

      _Locked:Boolean;
      _LockID:Cardinal;
      _LockCounter:Integer;

      {$IFDEF WINDOWS}
      _Handle:TRTLCriticalSection;
      {$ELSE}
      _Handle:pthread_mutex_t;
      {$ENDIF}

    Public
      { Creates a new critical section}
      Constructor Create();
      { Release the object}
      Procedure Release; Override;

      { Enter a critical code section. Other threads that try to lock this object while already locked will block until Unlock is called. }
      Procedure Lock;

      { Unlock this critical section }
      Procedure Unlock;

      { True if this object is currently locked }
      Property Locked:Boolean Read _Locked;
  End;

  { A semaphore primitive, used for multithreaded code }
  Semaphore = Class(TERRAObject)
    Protected
      {$IFDEF WINDOWS}
      _Handle:THandle;
      {$ELSE}
      _Handle:Integer;
      {$ENDIF}

    Public
      { Create a new semaphore }
      Constructor Create(Count:Integer);

      { Release the object}
      Procedure Release; Override;

      Procedure Wait();
      Procedure Signal();
  End;

Implementation

{$IFDEF DEBUG_LOCKS}
Var
  Sections:Array Of CriticalSection;
  SectionCount:Integer;
{$ENDIF}

Constructor CriticalSection.Create({Const Name:TERRAString});
Begin
//  _Name := Name;

  {$IFDEF DEBUG_LOCKS}
  Inc(SectionCount);
  SetLength(Sections, SectionCount);
  Sections[Pred(SectionCount) ] := Self;
  {$ENDIF}

{$IFDEF WINDOWS}
	InitializeCriticalSection(_Handle);
{$ELSE}
  pthread_mutex_init(@_Handle, Nil);
{$ENDIF}
End;

Procedure CriticalSection.Release;
{$IFDEF DEBUG_LOCKS}
Var
  I:Integer;
{$ENDIF}  
Begin
  {$IFDEF DEBUG_LOCKS}
  I := 0;
  While (I<SectionCount) Do
  If (Sections[I] = Self) Then
  Begin
    Sections[I] := Sections[Pred(SectionCount) ];
    Dec(SectionCount);
  End Else
    Inc(I);
  {$ENDIF}

{$IFDEF WINDOWS}
	DeleteCriticalSection(_Handle);
{$ELSE}
  pthread_mutex_destroy(@_Handle);
{$ENDIF}
End;

Procedure CriticalSection.Lock;
Var
  ThreadID:Cardinal;
Begin
  ThreadID := Cardinal(GetCurrentThreadId());

  If (_Locked) Then
  Begin
    Inc(_LockCounter);

    If (_LockID = ThreadID) Then
      Exit;
  End Else
    _LockCounter := 1;


{$IFDEF WINDOWS}
  {$IFDEF DEBUGMUTEX}
  If (_Name<>'') Then
    WriteLn(_Name,'.Lock() - '+IntToString(GetCurrentThread()));
  {$ENDIF}
	EnterCriticalSection(_Handle);
{$ELSE}
  pthread_mutex_lock(@_Handle);
{$ENDIF}

  _Locked := True;
  _LockID := ThreadID;
End;

Procedure CriticalSection.Unlock;
Var
  ThreadID:Cardinal;
Begin
  ThreadID := Cardinal(GetCurrentThreadId());

  Dec(_LockCounter);
  If (_Locked) And (_LockID = ThreadID) Then
  Begin
    If _LockCounter>0 Then
      Exit;
  End;

{$IFDEF WINDOWS}
  {$IFDEF DEBUGMUTEX}
  If (_Name<>'') Then
    WriteLn(_Name,'.Unlock() - '+IntToString(GetCurrentThread()));
  {$ENDIF}
	LeaveCriticalSection(_Handle);
{$ELSE}
  pthread_mutex_unlock(@_Handle);
{$ENDIF}

  _Locked := False;
End;

{ Semaphore }
Constructor Semaphore.Create(Count:Integer);
Begin
  {$IFDEF WINDOWS}
  _Handle := CreateSemaphore(Nil, 0, Count, Nil);
  {$ELSE}

  {$IFNDEF HAS_SEMAPHORES}
  sem_init(@_Handle, 0, Count);
  {$ENDIF}
  
  {$ENDIF}
End;

Procedure Semaphore.Release;
Begin
  {$IFDEF WINDOWS}
  CloseHandle(_Handle);
  {$ELSE}

  {$IFNDEF HAS_SEMAPHORES}
  sem_destroy(@_Handle);
  {$ENDIF}
  
  {$ENDIF}
End;

Procedure Semaphore.Signal();
Begin
  {$IFDEF WINDOWS}
  ReleaseSemaphore(_Handle,1, Nil); // unblock all the threads
  {$ELSE}

  {$IFNDEF HAS_SEMAPHORES}
  sem_post(@_Handle);
  {$ENDIF}

  {$ENDIF}
End;

Procedure Semaphore.Wait();
Begin
  {$IFDEF WINDOWS}
  WaitForSingleObject(_Handle, INFINITE);
  {$ELSE}

  {$IFNDEF HAS_SEMAPHORES}
  sem_wait(@_Handle);
  {$ENDIF}
  
  {$ENDIF}
End;

{$IFDEF DEBUG_LOCKS}
Var
  I:Integer;
{$ENDIF}  

Initialization
Finalization
{$IFDEF DEBUG_LOCKS}
  For I:=0 To Pred(SectionCount) Do
    Log(logWarning, 'App', 'The following mutex was not released: '+Sections[I]._Name);
{$ENDIF}
End.