Unit TERRA_UIText;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_UICursor, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_EnumProperty, TERRA_FontRenderer, TERRA_Viewport, TERRA_DebugDraw;

Type
  FontStyleProperty = Class(TERRAObject)
    Protected
      _Size:IntegerProperty;
      _Outline:ColorProperty;
      _Family:FontProperty;
      _Align:EnumProperty;

      Function GetFamily: TERRAFont;
      Procedure SetFamily(const Value: TERRAFont);

    Public
      Constructor Create(Const Name:TERRAString);
      Procedure Release(); Override;

      Class Function GetObjectType:TERRAString; Override;

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

      _ShouldStretch:Boolean;
      _ShouldWrap:Boolean;

      _FontRenderer:TERRAFontRenderer;

      Procedure UpdateSprite(); Override;


    Public
      Constructor Create(Const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);
      Procedure Release(); Override;

      Property Style:FontStyleProperty Read _Style;
  End;

Implementation
Uses TERRA_Localization, TERRA_FontManager, TERRA_Engine, TERRA_Math;

Var
  _AlignEnums:EnumCollection;

Constructor UIText.Create(const Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);
Begin
  Inherited Create(Name, Parent);

  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;

  Self.Width := Width;
  Self.Height := Height;

  _FontRenderer := TERRAFontRenderer.Create();

  _Style := FontStyleProperty(Self.AddProperty(FontStyleProperty.Create('style'), False));
End;

Procedure UIText.Release;
Begin
  Inherited;

  ReleaseObject(_FontRenderer);
End;

Procedure UIText.UpdateSprite();
Var
  FR:TERRAFontRenderer;
  TX, TY:Single;
Begin
  //ReleaseObject(_Sprite);

  If _Sprite = Nil Then
    _Sprite := FontSprite.Create();

  If (_Text = '') Then
  Begin
    _Sprite.Clear();
    Exit;
  End;

  FR := _FontRenderer;

  FR.SetColor(Self.GetColor());
  FR.SetGlow(Self.GetGlow());
  FR.SetFont(_Style._Family.Value);
  FR.SetAlign(TextAlign(_Style._Align.Value));
  FR.SetOutline(_Style._Outline.Value);
  FR.SetSize(_Style._Size.Value);
  FR.SetAutoWrap(Self._ShouldWrap);

  FR.SetTransform(_Transform);
  FR.SetClipRect(Self.ClipRect);

  FR.SetAreaLimit(Self.CurrentSize.X, Self.CurrentSize.Y);

  _FullSize := FR.GetTextRect(_Text);

  //TextArea := Vector2D_Create(Trunc(Self.GetDimension(Self.Width, uiDimensionWidth)),  Trunc(Self.GetDimension(Self.Height, uiDimensionHeight)));
  //TextRect := Self.FontRenderer.GetTextRect(Self.Caption._Text);

  If (Self._ShouldStretch) Then
  Begin
    If (_CurrentSize.X > _FullSize.X) Then
      TX := (_CurrentSize.X - _FullSize.X) * 0.5
    Else
      TX := 0;

    If (_CurrentSize.Y > _FullSize.Y) Then
      TY := (_CurrentSize.Y - _FullSize.Y) * 0.5
    Else
      TY := 0;
  End Else
  Begin
    Self.SetNativeSize();
    TX := 0;
    TY:=0;
  End;

  FR.DrawTextToSprite(TX, TY, Self.GetLayer(), _Text, FontSprite(_Sprite));

  //DrawClipRect(View, Self.ClipRect, ColorRed);
  //DrawRectangle(View, Vector2D_Create(TX + Self.AbsolutePosition.X, TY + Self.AbsolutePosition.Y), Vector2D_Create(TX + Self.AbsolutePosition.X + Self.FullSize.X, TY + Self.AbsolutePosition.Y + Self.FullSize.Y), ColorGreen);
  //DrawRectangle(View, Vector2D_Create(Self.AbsolutePosition.X, Self.AbsolutePosition.Y), Vector2D_Create(Self.AbsolutePosition.X + Self.CurrentSize.X, Self.AbsolutePosition.Y + Self.CurrentSize.Y), ColorGreen);
End;

{ FontStyleProperty }
Constructor FontStyleProperty.Create(const Name: TERRAString);
Begin
  _ObjectName := Name;
  _Outline := ColorProperty.Create('outline', ColorBlack);
  _Size := IntegerProperty.Create('size', 30);
  _Family := FontProperty.Create('family', Engine.Fonts['droid']);
  _Align := EnumProperty.Create('align', 0, _AlignEnums);
End;

Class Function FontStyleProperty.GetObjectType: TERRAString;
Begin
  Result := 'fontstyle';
End;

Function FontStyleProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := _Family;
  1:  Result := _Size;
  2:  Result := _Outline;
  3:  Result := _Align;
  Else
    Result := Nil;
  End;
End;

Procedure FontStyleProperty.Release;
Begin
  ReleaseObject(_Family);
  ReleaseObject(_Size);
  ReleaseObject(_Outline);
  ReleaseObject(_Align);
End;

Function FontStyleProperty.GetFamily: TERRAFont;
Begin
  Result := _Family.Value;
End;

Procedure FontStyleProperty.SetFamily(const Value: TERRAFont);
Begin
  _Family.Value := Value;
End;


Initialization
  _AlignEnums := EnumCollection.Create();
  _AlignEnums.Add('Left', Integer(TextAlign_Left));
  _AlignEnums.Add('Right', Integer(TextAlign_Right));
  _AlignEnums.Add('Center', Integer(TextAlign_Center));
  _AlignEnums.Add('Justify', Integer(TextAlign_Justify));
Finalization
  ReleaseObject(_AlignEnums);
End.