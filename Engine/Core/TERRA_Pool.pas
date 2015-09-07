Unit TERRA_Pool;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Mutex;

Type
  TERRAPool = Class(TERRAObject)
    Protected
      _Objects:Array Of TERRAObject;
      _ObjectCount:Integer;

      _Mutex:CriticalSection;

    Public
      Constructor Create();
      Procedure Release(); Override;

      Procedure Grow(Size:Integer);

      Function Fetch(ObjType:TERRAObjectType):TERRAObject;
      Procedure Recycle(Obj:TERRAObject);

      Property Count:Integer Read _ObjectCount;
  End;

Implementation

{ TERRAPool }
Constructor TERRAPool.Create;
Begin
  _Mutex := CriticalSection.Create();
End;

Procedure TERRAPool.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ObjectCount) Do
  If Assigned(_Objects[I]) Then
  Begin
    ReleaseObject(_Objects[I]);
  End;

  _ObjectCount := 0;

  ReleaseObject(_Mutex);
End;

Procedure TERRAPool.Recycle(Obj: TERRAObject);
Begin
  _Mutex.Lock();
  Inc(_ObjectCount);

  If (Length(_Objects)< _ObjectCount) Then
    SetLength(_Objects, _ObjectCount);

  _Objects[Pred(_ObjectCount)] := Obj;

  _Mutex.Unlock();
End;

Function TERRAPool.Fetch(ObjType:TERRAObjectType):TERRAObject;
Var
  I:Integer;
Begin
  Result := Nil;

  _Mutex.Lock();
  I := 0;
  While (I<_ObjectCount) Do
  If (_Objects[I] Is ObjType) Then
  Begin
    Result := _Objects[I];
    _Objects[I] := _Objects[Pred(_ObjectCount)];
    Dec(_ObjectCount);
    Break;
  End Else
    Inc(I);

  _Mutex.Unlock();
End;

Procedure TERRAPool.Grow(Size: Integer);
Begin
  _Mutex.Lock();

  If (Length(_Objects)<Size) Then
    SetLength(_Objects, Size);

  _Mutex.Unlock();
End;


End.
