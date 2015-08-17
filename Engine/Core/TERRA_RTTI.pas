Unit TERRA_RTTI;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String;

Type
  TERRAObjectType = Class Of TERRAObject;

  RTTI = Class
    Public
      Class Procedure RegisterType(Obj:TERRAObject);
      Class Function FindType(Const Name:TERRAString):TERRAObjectType;
  End;

Implementation

{ RTTI }

Var
  _RTTIList:Array Of TERRAObjectType;
  _RTTICount:Integer;

Class Procedure RTTI.RegisterType(Obj: TERRAObject);
Begin
  If (Obj = Nil) Then
    Exit;

  If Assigned(RTTI.FindType(Obj.ClassName)) Then
    Exit;

  Inc(_RTTICount);
  SetLength(_RTTIList, _RTTICount);
  _RTTIList[Pred(_RTTICount)] := TERRAObjectType(Obj.ClassType);
End;


Class Function RTTI.FindType(const Name: TERRAString):TERRAObjectType;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_RTTICount) Do
  If (StringEquals(_RTTIList[I].ClassName, Name)) Then
  Begin
    Result := _RTTIList[I];
    Exit;
  End;

  Result := Nil;
End;


End.
