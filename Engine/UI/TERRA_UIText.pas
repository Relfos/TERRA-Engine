Unit TERRA_UIText;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_Viewport, TERRA_DebugDraw;

Type
  FontStyleProperty = Class(TERRAObject)
    Protected
      _Size:IntegerProperty;
      _Outline:ColorProperty;
      _Family:FontProperty;

      Function GetFamily: TERRAFont;
      Procedure SetFamily(const Value: TERRAFont);

    Public
      Constructor Create(Const Name:TERRAString);
      Procedure Release(); Override;

      Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Outline:ColorProperty Read _Outline;

      Property Family:TERRAFont Read GetFamily Write SetFamily;
  End;

  UIText = Class(UIWidget)
    Protected
      _Text:TERRAString;
      _Style:FontStyleProperty;

      _TextRect:Vector2D;
      _PreviousFont:TERRAFont;

      Procedure UpdateSprite(View:TERRAViewport); Override;


    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);

      Procedure UpdateRects; Override;

      Property Style:FontStyleProperty Read _Style;
  End;

Implementation
Uses TERRA_Localization, TERRA_FontRenderer, TERRA_FontManager, TERRA_EngineManager, TERRA_Math;

Constructor UIText.Create(const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);
Begin
  Inherited Create(Name, Parent);

  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;

  Self.Width := Width;
  Self.Height := Height;

  _Style := FontStyleProperty(Self.AddProperty(FontStyleProperty.Create('style'), False));
End;


Procedure UIText.UpdateRects;
Var
  TextRect:Vector2D;
Begin
  Inherited;

  If (Assigned(FontRenderer))  Then
  Begin
    TextRect := Self.FontRenderer.GetTextRect(_Text);

    _Size.X := FloatMin(_Size.X, TextRect.X);
    _Size.Y := FloatMin(_Size.Y, TextRect.Y);
  End;

(*  If ((_NeedsUpdate) Or (Fnt<>_PreviousFont)) And (Assigned(FontRenderer)) Then
  Begin
    _TextRect := FontRenderer.GetTextRect(_Caption.Value, 1.0);
    _PreviousFont := Fnt;
    _NeedsUpdate := False;
  End;*)
End;

Procedure UIText.UpdateSprite(View:TERRAViewport);
Var
  FR:TERRAFontRenderer;
Begin
  //ReleaseObject(_Sprite);

  If _Sprite = Nil Then
    _Sprite := FontSprite.Create();

  FR := Self.FontRenderer;

  FR.SetColor(Self.GetColor());
  FR.SetGlow(Self.GetGlow());
  FR.SetFont(_Style._Family.Value);
  FR.SetOutline(_Style._Outline.Value);
  FR.SetSize(_Style._Size.Value);

  FR.SetTransform(_Transform);
  FR.SetClipRect(Self.ClipRect);

  //TextArea := Vector2D_Create(Trunc(Self.GetDimension(Self.Width, uiDimensionWidth)),  Trunc(Self.GetDimension(Self.Height, uiDimensionHeight)));
  //TextRect := Self.FontRenderer.GetTextRect(Self.Caption._Text);

  FR.DrawTextToSprite(View, (*(TextArea.X - TextRect.X) * 0.5,  (TextArea.Y - TextRect.Y) * 0.5*)0, 0, Self.GetLayer(), _Text, FontSprite(_Sprite));

  //DrawClipRect(View, Self.ClipRect, ColorRed);
End;

{ FontStyleProperty }
Constructor FontStyleProperty.Create(const Name: TERRAString);
Begin
  _ObjectName := Name;
  _Outline := ColorProperty.Create('outline', ColorBlack);
  _Size := IntegerProperty.Create('size', 30);
  _Family := FontProperty.Create('family', Engine.Fonts['droid']);
End;

Function FontStyleProperty.GetObjectType: TERRAString;
Begin
  Result := 'fontstyle';
End;

Function FontStyleProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := _Family;
  1:  Result := _Size;
  2:  Result := _Outline;
  Else
    Result := Nil;
  End;
End;

Procedure FontStyleProperty.Release;
Begin
  ReleaseObject(_Family);
  ReleaseObject(_Size);
  ReleaseObject(_Outline);
End;

Function FontStyleProperty.GetFamily: TERRAFont;
Begin
  Result := _Family.Value;
End;

Procedure FontStyleProperty.SetFamily(const Value: TERRAFont);
Begin
  _Family.Value := Value;
End;


End.