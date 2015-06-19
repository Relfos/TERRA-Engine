Unit TERRA_UICaption;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font;

Type
  UICaption = Class(Widget)
    Protected
      _Caption:TERRAString;
      _TextRect:Vector2D;
      _PreviousFont:Font;
      _OriginalValue:TERRAString;

      Function GetLocalizationKey: TERRAString;

    Public
      Procedure SetCaption(Value:TERRAString);

      Procedure UpdateRects; Override;

      Function GetSize:Vector2D; Override;

			Procedure OnLanguageChange(); Override;

      Property Caption:TERRAString Read _Caption Write SetCaption;
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

Function UICaption.GetLocalizationKey: TERRAString;
Begin
  If StringFirstChar(_OriginalValue) = Ord('#') Then
  Begin
    Result := StringCopy(_OriginalValue, 2, MaxInt);
  End Else
    Result := '';
End;

Function UICaption.GetSize: Vector2D;
Begin
  If (_NeedsUpdate) Then
    Self.UpdateRects();

  Result := Inherited GetSize;
End;

Procedure UICaption.OnLanguageChange;
Begin
  Self.SetCaption(Self._OriginalValue);
End;

Procedure UICaption.SetCaption(Value:TERRAString);
Begin
  _OriginalValue := Value;
  _NeedsUpdate := True;

  _Caption := GetLocalizedString(Value);

  _Caption := ConvertFontCodes(_Caption);
End;

Procedure UICaption.UpdateRects;
Var
  Fnt:TERRA_Font.Font;
Begin
  Inherited;
  
  Fnt := Self.GetFont();

  If ((_NeedsUpdate) Or (Fnt<>_PreviousFont)) And (Assigned(FontRenderer)) Then
  Begin
    _TextRect := FontRenderer.GetTextRect(_Caption, 1.0);
    _PreviousFont := Fnt;
    _NeedsUpdate := False;
  End;
End;


End.