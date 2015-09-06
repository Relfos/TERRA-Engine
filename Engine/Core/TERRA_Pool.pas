Unit TERRA_Pool;

{$I terra.inc}

Interface
Uses TERRA_Object;

Type
  TERRAPool = Class(TERRAObject)
    Protected
      _Objects:Array Of TERRAObject;
      _ObjectCount:Integer;

    Public
      Procedure Release(); Override;

      Procedure Grow(Size:Integer);

      Function Fetch(ObjType:TERRAObjectType):TERRAObject;
      Procedure Recycle(Obj:TERRAObject);

      Property Count:Integer Read _ObjectCount;
  End;

Implementation

{ TERRAPool }
Procedure TERRAPool.Recycle(Obj: TERRAObject);
Begin
  Inc(_ObjectCount);
  Grow(_ObjectCount);
  _Objects[Pred(_ObjectCount)] := Obj;
End;

Function TERRAPool.Fetch(ObjType:TERRAObjectType):TERRAObject;
Var
  I:Integer;
Begin
  I := 0;
  While (I<_ObjectCount) Do
  If (_Objects[I] Is ObjType) Then
  Begin
    Result := _Objects[I];
    _Objects[I] := _Objects[Pred(_ObjectCount)];
    Dec(_ObjectCount);
    Exit;
  End Else
    Inc(I);

  Result := Nil;
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
End;

Procedure TERRAPool.Grow(Size: Integer);
Begin
  If (Length(_Objects)<Size) Then
    SetLength(_Objects, Size);
End;

End.
