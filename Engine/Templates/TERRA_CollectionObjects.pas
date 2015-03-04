Unit TERRA_CollectionObjects;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils, TERRA_Collections;

Type
  // sample list objects
  StringObject=Class(ListObject)
    Public
      Value:TERRAString;

      Constructor Create(S:TERRAString);
      Function ToString():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

  IntegerObject = Class(ListObject)
    Public
      Value:Integer;

      Constructor Create(S:Integer);
      Function ToString():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

  CardinalObject = Class(ListObject)
    Public
      Value:Cardinal;

      Constructor Create(S:Cardinal);
      Function ToString():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

Implementation

{ StringObject }
Procedure StringObject.CopyValue(Other: ListObject);
Begin
  Self.Value := StringObject(Other).Value;
End;

Constructor StringObject.Create(S:TERRAString);
Begin
  Self.Value := S;
End;

Function StringObject.GetHashKey: HashKey;
Begin
  Result := GetStringHashKey(Value);
End;

Function StringObject.Sort(Other: ListObject): Integer;
Var
  S:TERRAString;
Begin
  S := StringObject(Other).Value;
  Result := GetStringSort(Self.Value, S);
End;

Function StringObject.ToString:TERRAString;
Begin
  Result := Value;
End;


{ IntegerObject }
Procedure IntegerObject.CopyValue(Other: ListObject);
Begin
  Self.Value := IntegerObject(Other).Value;
End;

Function IntegerObject.Sort(Other: ListObject): Integer;
Var
  S:Integer;
Begin
  S := IntegerObject(Other).Value;
  If (S<Value) Then
    Result := 1
  Else
  If (S>Value) Then
    Result := -1
  Else
    Result := 0;
End;

Function IntegerObject.GetHashKey: HashKey;
Begin
  Result := Value;
End;

Constructor IntegerObject.Create(S:Integer);
Begin
  Self.Value := S;
End;


Function IntegerObject.ToString:TERRAString;
Begin
  Result := IntToString(Value);
End;

{ CardinalObject }
Procedure CardinalObject.CopyValue(Other: ListObject);
Begin
  Self.Value := CardinalObject(Other).Value;
End;

Function CardinalObject.Sort(Other: ListObject): Integer;
Var
  S:Integer;
Begin
  S := CardinalObject(Other).Value;
  If (S<Value) Then
    Result := 1
  Else
  If (S>Value) Then
    Result := -1
  Else
    Result := 0;
End;

Function CardinalObject.GetHashKey: HashKey;
Begin
  Result := Value;
End;

Constructor CardinalObject.Create(S:Cardinal);
Begin
  Self.Value := S;
End;


Function CardinalObject.ToString:TERRAString;
Begin
  Result := CardinalToString(Value);
End;

End.
