Unit TERRA_StackObject;

{$I terra.inc}
Interface
Uses TERRA_Utils;

{$IFNDEF DISABLEALLOCOPTIMIZATIONS}
Function StackAlloc(Size:Integer):Pointer;
{$ENDIF}

Implementation
Uses TERRA_Mutex;

{$IFNDEF DISABLEALLOCOPTIMIZATIONS}
Const
  _StackObjectAllocSize = 1024 * 128;

Var
  _StackObjectStack:Array[0..Pred(_StackObjectAllocSize)] Of Byte;
  _StackObjectPointer:Cardinal = 0;
{$IFNDEF DISABLETHREADS}
  _StackObjectMutex:CriticalSection;
{$ENDIF}

Function StackAlloc(Size:Integer):Pointer;
Begin
{$IFNDEF DISABLETHREADS}
  _StackObjectMutex.Lock();
{$ENDIf}

  If (_StackObjectPointer + Size >= _StackObjectAllocSize) Then
    _StackObjectPointer := 0;

  Result := @_StackObjectStack[_StackObjectPointer];

  Inc(_StackObjectPointer, Size);

{$IFNDEF DISABLETHREADS}
  _StackObjectMutex.Unlock();
{$ENDIf}
End;

(*Class Function StackObject.NewInstance: TObject;
Var
  ObjSize, GlobalSize:Integer;
Begin
  ObjSize := InstanceSize();

  Result := StackAlloc(ObjSize);

  InitInstance(Result);
End;

Procedure StackObject.FreeInstance;
Begin
  // do nothing -> extremely fast deallocs
End;*)
{$ENDIF}

{$IFNDEF DISABLETHREADS}
Initialization
  _StackObjectMutex := CriticalSection.Create();
Finalization
  ReleaseObject(_StackObjectMutex);
{$ENDIF}
End.