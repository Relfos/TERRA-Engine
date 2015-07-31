Unit TERRA_CollectionObjects;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils, TERRA_Collections;

Type
  // sample list objects
  StringObject=Class(CollectionObject)
    Public
      Value:TERRAString;

      Constructor Create(S:TERRAString);
      Function GetBlob():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:CollectionObject); Override;
      Function Sort(Other:CollectionObject):Integer; Override;
  End;

  IntegerObject = Class(CollectionObject)
    Public
      Value:Integer;

      Constructor Create(S:Integer);
      Function GetBlob():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:CollectionObject); Override;
      Function Sort(Other:CollectionObject):Integer; Override;
  End;

  CardinalObject = Class(CollectionObject)
    Public
      Value:Cardinal;

      Constructor Create(S:Cardinal);
      Function GetBlob():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:CollectionObject); Override;
      Function Sort(Other:CollectionObject):Integer; Override;
  End;

Implementation

{ StringObject }
Procedure StringObject.CopyValue(Other: CollectionObject);
Begin
  Self.Value := StringObject(Other).Value;
End;

Constructor StringObject.Create(S:TERRAString);
Begin
  Self.Value := S;
End;

Function StringObject.Sort(Other: CollectionObject): Integer;
Var
  S:TERRAString;
Begin
  S := StringObject(Other).Value;
  Result := GetStringSort(Self.Value, S);
End;

Function StringObject.GetBlob:TERRAString;
Begin
  Result := Value;
End;


{ IntegerObject }
Procedure IntegerObject.CopyValue(Other: CollectionObject);
Begin
  Self.Value := IntegerObject(Other).Value;
End;

Function IntegerObject.Sort(Other: CollectionObject): Integer;
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

Constructor IntegerObject.Create(S:Integer);
Begin
  Self.Value := S;
End;

Function IntegerObject.GetBlob:TERRAString;
Begin
  Result := IntToString(Value);
End;

{ CardinalObject }
Procedure CardinalObject.CopyValue(Other: CollectionObject);
Begin
  Self.Value := CardinalObject(Other).Value;
End;

Function CardinalObject.Sort(Other: CollectionObject): Integer;
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

Constructor CardinalObject.Create(S:Cardinal);
Begin
  Self.Value := S;
End;


Function CardinalObject.GetBlob:TERRAString;
Begin
  Result := CardinalToString(Value);
End;

End.
