Unit TERRA_UIDimension;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Object;

Type
  UIDimensionTarget = (
    uiDimensionWidth,
    uiDimensionHeight
  );

  UIDimension = Record
      IsPercent:Boolean;
      Value:Single;
  End;

  DimensionProperty = Class(TERRAObject)
    Protected
      _Dimension:UIDimension;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:UIDimension);

      Function IsValueObject():Boolean; Override;

      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:UIDimension Read _Dimension Write _Dimension;
  End;

Function UIPixels(Const Pixels:Single):UIDimension;
Function UIPercent(Const Percent:Single):UIDimension;

Function UISnap(Const X:Single):Single;

Var
  UISnapSize:Single = 0;

Implementation

Function UISnap(Const X:Single):Single;
Begin
  If (UISnapSize>0) Then
  Begin
    Result := Trunc(X/UISnapSize) * UISnapSize;
  End Else
    Result := X;
End;

Function UIPixels(Const Pixels:Single):UIDimension;
Begin
  Result.IsPercent := False;
  Result.Value := Trunc(Pixels);
End;

Function UIPercent(Const Percent:Single):UIDimension;
Begin
  Result.IsPercent := True;
  Result.Value := Percent;
End;

{ DimensionProperty }
Constructor DimensionProperty.Create(const Name: TERRAString; Const InitValue: UIDimension);
Begin
  Self._ObjectName := Name;
  Self._Dimension := InitValue;
End;

Function DimensionProperty.GetObjectType: TERRAString;
Begin
  Result := 'dimension';
End;

Function DimensionProperty.IsValueObject: Boolean;
Begin
  Result := True;
End;


Function DimensionProperty.GetBlob: TERRAString;
Begin
  Result := FloatToString(_Dimension.Value);

  If _Dimension.IsPercent Then
    Result := Result +'%';
End;

Procedure DimensionProperty.SetBlob(const Blob: TERRAString);
Var
  S:TERRAString;
Begin
  S := Blob;

  If StringLastChar(S) = Ord('%') Then
  Begin
    _Dimension.IsPercent := True;

    StringDropChars(S, -1);
  End Else
    _Dimension.IsPercent := False;

  _Dimension.Value := StringToFloat(S);
End;

End.