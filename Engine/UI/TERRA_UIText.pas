Unit TERRA_UIText;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_UICursor, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_FontRenderer, TERRA_Viewport, TERRA_DebugDraw;

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

      _AutoWrap:Boolean;

      _FontRenderer:TERRAFontRenderer;

      Procedure UpdateSprite(View:TERRAViewport); Override;


    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);
      Procedure Release(); Override;

      Property Style:FontStyleProperty Read _Style;
  End;

Implementation
Uses TERRA_Localization, TERRA_FontManager, TERRA_EngineManager, TERRA_Math;

Constructor UIText.Create(const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);
Begin
  Inherited Create(Name, Parent);

  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;

  Self.Width := Width;
  Self.Height := Height;

  _FontRenderer := TERRAFontRenderer.Create();

  _AutoWrap := True;

  _Style := FontStyleProperty(Self.AddProperty(FontStyleProperty.Create('style'), False));
End;

Procedure UIText.Release;
Begin
  Inherited;

  ReleaseObject(_FontRenderer);
End;

Procedure UIText.UpdateSprite(View:TERRAViewport);
Var
  FR:TERRAFontRenderer;
  TX, TY:Single;
Begin
  //ReleaseObject(_Sprite);

  If _Sprite = Nil Then
    _Sprite := FontSprite.Create();

  FR := _FontRenderer;

  FR.SetColor(Self.GetColor());
  FR.SetGlow(Self.GetGlow());
  FR.SetFont(_Style._Family.Value);
  FR.SetOutline(_Style._Outline.Value);
  FR.SetSize(_Style._Size.Value);
  FR.SetAutoWrap(Self._AutoWrap);

  FR.SetTransform(_Transform);
  FR.SetClipRect(Self.ClipRect);

  FR.SetAreaLimit(Self.CurrentSize.X, Self.CurrentSize.Y);

  _FullSize := FR.GetTextRect(_Text);

  //TextArea := Vector2D_Create(Trunc(Self.GetDimension(Self.Width, uiDimensionWidth)),  Trunc(Self.GetDimension(Self.Height, uiDimensionHeight)));
  //TextRect := Self.FontRenderer.GetTextRect(Self.Caption._Text);

  TX := (_CurrentSize.X - _FullSize.X) * 0.5;
  TY := (_CurrentSize.Y - _FullSize.Y) * 0.5;
  FR.DrawTextToSprite(View, TX, TY, Self.GetLayer(), _Text, FontSprite(_Sprite));

  //DrawClipRect(View, Self.ClipRect, ColorRed);
  //DrawRectangle(View, Self.AbsolutePosition, Vector2D_Create(Self.AbsolutePosition.X + Self.FullSize.X, Self.AbsolutePosition.Y + Self.FullSize.Y), ColorGreen);
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