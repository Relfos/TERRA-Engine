Unit TERRA_UICaption;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font;

Type
  UICaption = Class(Widget)
    Protected
      _Caption:StringProperty;
      _CaptionIndex:Integer;

      _TextRect:Vector2D;
      _PreviousFont:TERRAFont;
      _OriginalValue:TERRAString;

      Function GetLocalizationKey: TERRAString;

    Public
      Constructor Create(Const Name:TERRAString; Parent:Widget; Const ComponentName:TERRAString);
      Procedure Release(); Override;

      Procedure UpdateRects; Override;

      Function GetPropertyByIndex(Index: Integer): TERRAObject; Override;

      Function GetSize:Vector2D; Override;

			Procedure OnLanguageChange(); Override;

      Property Caption:StringProperty Read _Caption;
      Property LocalizationKey:TERRAString Read GetLocalizationKey;
  End;

Function GetLocalizedString(Value:TERRAString):TERRAString;

Implementation
Uses TERRA_Localization;

Type
  CaptionProperty = Class(StringProperty)
    Protected
      _Owner:UICaption;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:TERRAString; Owner:UICaption);

      Procedure SetBlob(Const Value:TERRAString); Override;
  End;


Function GetLocalizedString(Value:TERRAString):TERRAString;
Begin
  If (Value<>'') And (Value[1]='#') Then
  Begin
    Value := Copy(Value, 2, MaxInt);
    Result := LocalizationManager.Instance.GetString(Value);
  End Else
    Result := Value;
End;

Constructor UICaption.Create(const Name:TERRAString; Parent:Widget; Const ComponentName: TERRAString);
Begin
  Inherited Create(Name, Parent, ComponentName);
  _Caption := CaptionProperty.Create('caption', '', Self);

  Self.ExpandProperties(1);
  _CaptionIndex := _BasePropertiesIndex;
End;

Procedure UICaption.Release();
Begin
  ReleaseObject(_Caption);
End;

Function UICaption.GetLocalizationKey: TERRAString;
Begin
  If StringFirstChar(_OriginalValue) = Ord('#') Then
  Begin
    Result := StringCopy(_OriginalValue, 2, MaxInt);
  End Else
    Result := '';
End;

Function UICaption.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If Index = _CaptionIndex Then
    Result := Self.Caption
  Else
    Result := Inherited GetPropertyByIndex(Index);
End;

Function UICaption.GetSize: Vector2D;
Begin
  If (_NeedsUpdate) Then
    Self.UpdateRects();

  Result := Inherited GetSize;
End;

Procedure UICaption.OnLanguageChange;
Begin
  Self.Caption.Value := Self._OriginalValue;
End;

Procedure UICaption.UpdateRects;
Var
  Fnt:TERRAFont;
Begin
  Inherited;

  Fnt := Self.GetFont();

  If ((_NeedsUpdate) Or (Fnt<>_PreviousFont)) And (Assigned(FontRenderer)) Then
  Begin
    _TextRect := FontRenderer.GetTextRect(_Caption.Value, 1.0);
    _PreviousFont := Fnt;
    _NeedsUpdate := False;
  End;
End;

{ CaptionProperty }
Constructor CaptionProperty.Create(const Name, InitValue: TERRAString; Owner: UICaption);
Begin
  Inherited Create(Name, InitValue);
  _Owner := Owner;
End;

Procedure CaptionProperty.SetBlob(const Value: TERRAString);
Var
  S:TERRAString;
Begin
  _Owner._OriginalValue := Value;
  _Owner._NeedsUpdate := True;

  S := GetLocalizedString(Value);
  S := ConvertFontCodes(S);

  _Value := S;
End;

End.