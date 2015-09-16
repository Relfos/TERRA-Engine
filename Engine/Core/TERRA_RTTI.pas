Unit TERRA_RTTI;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String;

Type
  RTTIEntry = Record
    Alias:TERRAString;
    ObjectType:TERRAObjectType;
  End;

  RTTIManager = Class
    Protected
      _RTTIList:Array Of RTTIEntry;
      _RTTICount:Integer;

    Public
      Procedure RegisterType(Obj:TERRAObjectType; Alias:TERRAString = '');
      Function FindType(Const Name:TERRAString):TERRAObjectType;
  End;

Implementation

{ RTTIManager }
Procedure RTTIManager.RegisterType(Obj:TERRAObjectType; Alias:TERRAString = '');
Begin
  If (Obj = Nil) Then
    Exit;

  If (Alias = '') Then
    Alias := Obj.ClassName;

  If Assigned(FindType(Alias)) Then
    Exit;

  Inc(_RTTICount);
  SetLength(_RTTIList, _RTTICount);
  _RTTIList[Pred(_RTTICount)].Alias := Alias;
  _RTTIList[Pred(_RTTICount)].ObjectType := Obj;
End;


Function RTTIManager.FindType(const Name: TERRAString):TERRAObjectType;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_RTTICount) Do
  If (StringEquals(_RTTIList[I].Alias, Name)) Then
  Begin
    Result := _RTTIList[I].ObjectType;
    Exit;
  End;

  Result := Nil;
End;


End.
