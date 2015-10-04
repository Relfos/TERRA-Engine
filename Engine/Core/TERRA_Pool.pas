Unit TERRA_Pool;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Mutex;

Type
  TERRAPool = Class(TERRAObject)
    Protected
      {$IFNDEF DISABLEMEMORYPOOL}
      _Objects:Array Of TERRAObject;
      _ObjectCount:Integer;

      _Mutex:CriticalSection;
      {$ENDIF}

    Public
      Constructor Create();
      Procedure Release(); Override;

      Procedure Grow(Size:Integer);

      Function Fetch(ObjType:TERRAObjectType):TERRAObject;
      Procedure Recycle(Obj:TERRAObject);

      {$IFNDEF DISABLEMEMORYPOOL}
      Property Count:Integer Read _ObjectCount;
      {$ENDIF}
  End;

Implementation
Uses TERRA_Engine;

{ TERRAPool }
Constructor TERRAPool.Create;
Begin
  {$IFNDEF DISABLEMEMORYPOOL}
  _Mutex := CriticalSection.Create();

  SetLength(_Objects, 16);
  {$ENDIF}
End;

Procedure TERRAPool.Release;
Var
  I:Integer;
Begin
  {$IFNDEF DISABLEMEMORYPOOL}
  _Mutex.Lock();
  For I:=0 To Pred(_ObjectCount) Do
  If Assigned(_Objects[I]) Then
  Begin
    ReleaseObject(_Objects[I], False);
  End;

  _ObjectCount := 0;
  _Mutex.Unlock();

  ReleaseObject(_Mutex);
  {$ENDIF}
End;

Procedure TERRAPool.Recycle(Obj: TERRAObject);
Begin
  {$IFNDEF DISABLEMEMORYPOOL}
  _Mutex.Lock();
  Inc(_ObjectCount);

  If (Length(_Objects)< _ObjectCount) Then
    SetLength(_Objects, _ObjectCount);

  _Objects[Pred(_ObjectCount)] := Obj;

  _Mutex.Unlock();
  {$ENDIF}
End;

Function TERRAPool.Fetch(ObjType:TERRAObjectType):TERRAObject;
Var
  I:Integer;
Begin
  Result := Nil;

  {$IFNDEF DISABLEMEMORYPOOL}
  If Not ObjType.CanBePooled Then
  Begin
    Engine.RaiseError(ObjType.ClassName + ' type cannot be pooled!');
    Exit;
  End;

  _Mutex.Lock();
  I := 0;
  While (I<_ObjectCount) Do
  If (_Objects[I].ClassType = ObjType) Then
  Begin
    Result := _Objects[I];
    _Objects[I] := _Objects[Pred(_ObjectCount)];
    Dec(_ObjectCount);
    Break;
  End Else
    Inc(I);

  _Mutex.Unlock();
  {$ENDIF}
End;

Procedure TERRAPool.Grow(Size: Integer);
Begin
  {$IFNDEF DISABLEMEMORYPOOL}
  _Mutex.Lock();

  While (Length(_Objects)<Size) Do
    SetLength(_Objects, Length(_Objects) * 2);

  _Mutex.Unlock();
  {$ENDIF}
End;


End.
