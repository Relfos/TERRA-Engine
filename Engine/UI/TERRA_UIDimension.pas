Unit TERRA_UIDimension;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Object;

Type
  UIDimensionTarget = (
    uiDimensionHorizontal,
    uiDimensionVertical,
    uiDimensionWidth,
    uiDimensionHeight,
    uiDimensionLeft,
    uiDimensionTop
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

      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:UIDimension Read _Dimension Write _Dimension;
  End;

  MarginProperty = Class(TERRAObject)
    Protected
      _Left:DimensionProperty;
      _Top:DimensionProperty;
      _Right:DimensionProperty;
      _Bottom:DimensionProperty;

      Function GetBottom: UIDimension;
      Function GetLeft: UIDimension;
      Function GetRight: UIDimension;
      Function GetTop: UIDimension;
      Procedure SetBottom(const Value: UIDimension);
      Procedure SetLeft(const Value: UIDimension);
      Procedure SetRight(const Value: UIDimension);
      Procedure SetTop(const Value: UIDimension);

    Public
      Constructor Create(Const Name:TERRAString);

      Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Left:UIDimension Read GetLeft Write SetLeft;
      Property Top:UIDimension Read GetTop Write SetTop;
      Property Right:UIDimension Read GetRight Write SetRight;
      Property Bottom:UIDimension Read GetBottom Write SetBottom;
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


Function DimensionProperty.GetBlob: TERRAString;
Begin
  Result := FloatProperty.Stringify(_Dimension.Value);

  If _Dimension.IsPercent Then
    Result := Result +'%';
End;

Procedure DimensionProperty.SetBlob(const Blob: TERRAString);
Var
  S:TERRAString;
Begin
  S := Blob;

  If StringLastChar(S) = '%' Then
  Begin
    _Dimension.IsPercent := True;

    StringDropChars(S, -1);
  End Else
    _Dimension.IsPercent := False;

  _Dimension.Value := StringToFloat(S);
End;

{ MarginProperty }
Constructor MarginProperty.Create(const Name: TERRAString);
Begin
  _Left := DimensionProperty.Create('left', UIPixels(0));
  _Top := DimensionProperty.Create('top', UIPixels(0));
  _Right := DimensionProperty.Create('right', UIPixels(0));
  _Bottom := DimensionProperty.Create('bottom', UIPixels(0));
End;

Function MarginProperty.GetObjectType: TERRAString;
Begin
  Result := 'margin';
End;

Function MarginProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := _Left;
  1:  Result := _Top;
  2:  Result := _Right;
  3:  Result := _Bottom;
    Else
      Result := Nil;
  End;
End;

Function MarginProperty.GetBottom: UIDimension;
Begin
  Result := _Bottom.Value;
End;

Function MarginProperty.GetLeft: UIDimension;
begin
  Result := _Left.Value;
end;

Function MarginProperty.GetRight: UIDimension;
Begin
  Result := _Right.Value;
End;

Function MarginProperty.GetTop: UIDimension;
Begin
  Result := _Top.Value;
End;

Procedure MarginProperty.SetBottom(const Value: UIDimension);
Begin
  _Bottom.Value := Value;
End;

Procedure MarginProperty.SetLeft(const Value: UIDimension);
Begin
  _Left.Value := Value;
End;

Procedure MarginProperty.SetRight(const Value: UIDimension);
Begin
  _Right.Value := Value;
End;

Procedure MarginProperty.SetTop(const Value: UIDimension);
Begin
  _Top.Value := Value;
End;

End.