Unit TERRA_EnumProperty;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_String;

Type
  EnumAssociation = Record
    Name:TERRAString;
    Value:Integer;
  End;

  EnumCollection = Class(TERRAObject)
    Protected
      _List:Array Of EnumAssociation;
      _Count:Integer;

    Public
      Function GetByValue(Const Value:Integer):TERRAString;
      Function GetByName(Const Value:TERRAString):Integer;
      Function GetByIndex(Index:Integer):TERRAString;


      Procedure Add(Const Name:TERRAString; Const Value:Integer);

      Property Count:Integer Read _Count;
  End;

  EnumProperty = Class(TERRAObject)
    Protected
      _Value:Integer;
      _Collection:EnumCollection;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Integer; Collection:EnumCollection);

      Function GetObjectType:TERRAString; Override;

      Function IsValueObject():Boolean; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:Integer Read _Value Write _Value;

      Property Collection:EnumCollection Read _Collection;
  End;


Implementation

{ EnumProperty }
Constructor EnumProperty.Create(const Name: TERRAString; const InitValue: Integer; Collection: EnumCollection);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
  Self._Collection := Collection;
End;


Function EnumProperty.GetBlob: TERRAString;
Begin
  Result := _Collection.GetByValue(_Value);
End;

Procedure EnumProperty.SetBlob(const Blob: TERRAString);
Begin
  Value := _Collection.GetByName(Blob);
End;

Function EnumProperty.GetObjectType: TERRAString;
Begin
  Result := 'enum';
End;

Function EnumProperty.IsValueObject: Boolean;
Begin
  Result := True;
End;


{ EnumCollection }
Procedure EnumCollection.Add(const Name: TERRAString; const Value: Integer);
Begin
  Inc(_Count);
  SetLength(_List, _Count);
  _List[Pred(_Count)].Name := Name;
  _List[Pred(_Count)].Value := Value;
End;

Function EnumCollection.GetByIndex(Index: Integer): TERRAString;
Begin
  If (Index<0) Or (Index>=_Count) Then
    Result := '#'
  Else
    Result := _List[Index].Name;
End;

Function EnumCollection.GetByName(const Value: TERRAString): Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
  If (StringEquals(_List[I].Name, Value)) Then
  Begin
    Result := _List[I].Value;
    Exit;
  End;

  Result := 0;
End;

Function EnumCollection.GetByValue(const Value: Integer): TERRAString;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Count) Do
  If (_List[I].Value = Value) Then
  Begin
    Result := _List[I].Name;
    Exit;
  End;

  Result := '#';
End;

End.
