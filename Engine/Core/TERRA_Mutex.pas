Unit TERRA_Mutex;

{$I terra.inc}
Interface
Uses TERRA_Utils,
{$IFDEF DEBUG_LOCKS} TERRA_Log, {$ENDIF}
{$IFDEF WINDOWS}Windows
{$ELSE}cmem, ctypes,baseunix,unixtype{$ENDIF};

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

Function sem_init(sem:ppthread_sem_t; pshared:Integer; value:Cardinal):Integer; Cdecl; External LibC;
Function sem_wait(sem:ppthread_sem_t):Integer; Cdecl; External LibC;
Function sem_post(sem:ppthread_sem_t):Integer; Cdecl; External LibC;
Function sem_destroy(sem:ppthread_sem_t):Integer; Cdecl; External LibC;
{$ENDIF}

Type
  CriticalSection = Class(TERRAObject)
    Protected
      _Name:String;
      {$IFDEF WINDOWS}
      _Handle:TRTLCriticalSection;
      {$ELSE}
      _Handle:pthread_mutex_t;
      {$ENDIF}

    Public
      Constructor Create(Name:AnsiString);
      Destructor Destroy; Override;

      Procedure Lock;
      Procedure Unlock;
  End;

  Semaphore = Class(TERRAObject)
    Protected
      _Name:AnsiString;

      {$IFDEF WINDOWS}
      _Handle:THandle;
      {$ELSE}
      _Handle:Integer;
      {$ENDIF}

    Public
      Constructor Create(Name:AnsiString; Count:Integer);
      Destructor Destroy; Override;

      Procedure Release;
      Procedure Wait;
  End;

Implementation

{$IFDEF DEBUG_LOCKS}
Var
  Sections:Array Of CriticalSection;
  SectionCount:Integer;
{$ENDIF}

Constructor CriticalSection.Create(Name:AnsiString);
Begin
  _Name := Name;

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

Destructor CriticalSection.Destroy;
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
Begin
{$IFDEF WINDOWS}
  {$IFDEF DEBUGMUTEX}
  If (_Name<>'') Then
    WriteLn(_Name,'.Lock() - '+IntToString(GetCurrentThread()));
  {$ENDIF}
	EnterCriticalSection(_Handle);
{$ELSE}
  pthread_mutex_lock(@_Handle);
{$ENDIF}
End;

Procedure CriticalSection.Unlock;
Begin
{$IFDEF WINDOWS}
  {$IFDEF DEBUGMUTEX}
  If (_Name<>'') Then
    WriteLn(_Name,'.Unlock() - '+IntToString(GetCurrentThread()));
  {$ENDIF}
	LeaveCriticalSection(_Handle);
{$ELSE}
  pthread_mutex_unlock(@_Handle);
{$ENDIF}
End;

{ Semaphore }
Constructor Semaphore.Create(Name: AnsiString; Count:Integer);
Begin
  Self._Name := Name;
  {$IFDEF WINDOWS}
  _Handle := CreateSemaphore(Nil, 0, Count, Nil);
  {$ELSE}
  sem_init(@_Handle, 0, Count);
  {$ENDIF}
End;

Destructor Semaphore.Destroy;
Begin
  {$IFDEF WINDOWS}
  CloseHandle(_Handle);
  {$ELSE}
  sem_destroy(@_Handle);
  {$ENDIF}
End;

Procedure Semaphore.Release;
Begin
  {$IFDEF WINDOWS}
  ReleaseSemaphore(_Handle,1, Nil); // unblock all the threads
  {$ELSE}
  sem_post(@_Handle);
  {$ENDIF}
End;

Procedure Semaphore.Wait;
Begin
  {$IFDEF WINDOWS}
  WaitForSingleObject(_Handle, INFINITE);
  {$ELSE}
  sem_wait(@_Handle);
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
