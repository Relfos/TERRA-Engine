Unit TERRA_UILabel;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_Viewport, TERRA_DebugDraw, TERRA_UIText;

Type
  UILabel = Class(UIText)
    Protected
      _NeedCaptionUpdate:Boolean;

      Function GetLocalizationKey: TERRAString;

      Procedure UpdateSprite(View:TERRAViewport); Override;

      Procedure UpdateCaption();

      Procedure SetText(Const S:TERRAString); Override;

    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension; Const Text:TERRAString);

      Function SupportDrag(Mode:UIDragMode):Boolean; Override;

			Procedure OnLanguageChange(); Override;

      Property LocalizationKey:TERRAString Read GetLocalizationKey;
  End;

Implementation
Uses TERRA_Localization, TERRA_FontRenderer;

Constructor UILabel.Create(const Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension; Const Text:TERRAString);
Begin
  Inherited Create(Name, Parent, X, Y, Z, Width, Height, Text);

  _NeedCaptionUpdate := True;
End;

Function UILabel.GetLocalizationKey: TERRAString;
Begin
  If StringFirstChar(Self.Content) = Ord('#') Then
  Begin
    Result := StringCopy(Self.Content, 2, MaxInt);
  End Else
    Result := '';
End;

Procedure UILabel.OnLanguageChange;
Begin
  _NeedCaptionUpdate := True;
End;

Procedure UILabel.UpdateSprite(View:TERRAViewport);
Begin
//  If (_NeedCaptionUpdate) Then
  Begin
    _NeedCaptionUpdate := False;
    Self.UpdateCaption();
  End;

  Inherited UpdateSprite(View);
End;

Function UILabel.SupportDrag(Mode: UIDragMode): Boolean;
Begin
  Result := (Mode = UIDrag_Move);
End;

Procedure UILabel.UpdateCaption();
Var
  Result, S, S2:TERRAString;
  It:StringIterator;
Begin
  S := Self.Content;
  Result := '';
  Repeat
    If StringCharPosIterator(UIMacroBeginChar, S, It, True) Then
    Begin
      It.Split(S2, S);
      Result := Result + S2;

      S2 := StringGetNextSplit(S, UIMacroEndChar);

      S2 := Self.ResolveMacro(S2);

      Result := Result + S2 + ' ';
    End Else
    Begin
      Result := Result + S;
      Break;
    End;

  Until False;

  _TextValue.Value := ConvertFontCodes(Result);
End;


Procedure UILabel.SetText(const S: TERRAString);
Begin
  Inherited;

  Self.UpdateCaption();
End;

End.