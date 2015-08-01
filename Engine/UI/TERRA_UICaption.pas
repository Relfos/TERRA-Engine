Unit TERRA_UICaption;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font;

Type
  UICaption = Class;

  CaptionProperty = Class(StringProperty)
    Protected
      _Text:TERRAString;
      _Owner:UICaption;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:TERRAString; Owner:UICaption);

      Procedure SetBlob(Const Value:TERRAString); Override;

      Property Text:TERRAString Read _Text;
  End;


  UICaption = Class(Widget)
    Protected
      _Caption:CaptionProperty;
      _CaptionIndex:Integer;

      _TextRect:Vector2D;
      _PreviousFont:TERRAFont;

      Function GetLocalizationKey: TERRAString;

    Public
      Constructor Create(Const Name:TERRAString; Parent:Widget; Const ComponentName:TERRAString);
      Procedure Release(); Override;

      Procedure UpdateRects; Override;

      Function GetPropertyByIndex(Index: Integer): TERRAObject; Override;

      Function GetSize:Vector2D; Override;

			Procedure OnLanguageChange(); Override;

      Property Caption:CaptionProperty Read _Caption;
      Property LocalizationKey:TERRAString Read GetLocalizationKey;
  End;

Function GetLocalizedString(Value:TERRAString):TERRAString;

Implementation
Uses TERRA_Localization;

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
  If StringFirstChar(_Caption.Value) = Ord('#') Then
  Begin
    Result := StringCopy(Caption.Value, 2, MaxInt);
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
  Self.Caption.SetBlob(Self._Caption._Value);
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
  S, S2, Data:TERRAString;
  It:StringIterator;
Begin
  _Value := Value;
  _Owner._NeedsUpdate := True;

  Data := _Owner.GetDataValue();

  S := Value;
  _Text := '';
  Repeat
    If StringCharPosIterator(Ord('#'), S, It, True) Then
    Begin
      It.Split(S2, S);
      _Text := _Text + S2;

      S2 := StringGetNextSplit(S, Ord(' '));

      If S2='$' Then
        S2 := Data
      Else
        S2 := GetLocalizedString(S2);

      _Text := _Text + S2 + ' ';
    End Else
    Begin
      _Text := _Text + S;
      Break;
    End;

  Until False;

  S := ConvertFontCodes(S);
End;

End.