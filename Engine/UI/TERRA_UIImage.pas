Unit TERRA_UIImage;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UIWidget, TERRA_Color,
  TERRA_UIDimension, TERRA_Sprite, TERRA_Texture, TERRA_Renderer, TERRA_Vector2D, TERRA_Viewport;

Type
  UIImage = Class(UIWidget)
    Protected
      _Texture:TextureProperty;
      _U1, _V1, _U2, _V2:FloatProperty;


      Function GetTexture: TERRATexture;

      Procedure UpdateSprite(View:TERRAViewport); Override;

      Function GetObjectType:TERRAString; Override;

    Public
      Anchor:Vector2D;
      Flip, Mirror:Boolean;
      Filter:TextureFilterMode;

      Constructor Create(Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension);

      Procedure SetTexture(Tex:TERRATexture);

      Procedure UpdateRects; Override;

      Property Texture:TERRATexture Read GetTexture Write SetTexture;
  End;


Implementation
Uses TERRA_Log, TERRA_EngineManager;

{ UIImage }
Constructor UIImage.Create(Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension);
Begin
  Inherited Create(Name, Parent);

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;
  Self.Filter := filterLinear;

  Self._Texture := TextureProperty(Self.AddProperty(TextureProperty.Create('image', Nil), False));
  _U1 := FloatProperty(Self.AddProperty(FloatProperty.Create('u1', 0), False));
  _V1 := FloatProperty(Self.AddProperty(FloatProperty.Create('v1', 0), False));
  _U2 := FloatProperty(Self.AddProperty(FloatProperty.Create('u2', 1), False));
  _V2 := FloatProperty(Self.AddProperty(FloatProperty.Create('v2', 1), False));

  Self.Width := Width;
  Self.Height := Height;

  Self.Pivot := VectorCreate2D(0, 0);
  Self.Anchor := VectorCreate2D(0, 0);
End;

{Function UIImage.OnRegion(X, Y: Integer): Boolean;
Var
  WH, Pos:Vector2d;
  OfsX, OfsY:Single;
Begin
  If (OutsideClipRect(X,Y)) Then
  Begin
    Result := False;
    Exit;
  End;

  Pos := Self.GetAbsolutePosition;
  Self.GetScrollOffset(OfsX, OfsY);
  Pos.X := Pos.X + OfsX;
  Pos.Y := Pos.Y + OfsY;

  WH := Self.Size;

  Result := (X>=Pos.X) And (Y>=Pos.Y) And (X<=Pos.X+WH.X*Scale) And (Y<=Pos.Y+WH.Y*Scale);
End;}

Function UIImage.GetObjectType: TERRAString;
Begin
  Result := 'UIImage';
End;

Function UIImage.GetTexture: TERRATexture;
Begin
  Result := _Texture.Value;
End;

Procedure UIImage.SetTexture(Tex: TERRATexture);
Begin
  If Tex = Nil Then
    Tex := Engine.Textures.WhiteTexture;

  _Texture.Value := Tex;
  {Self.Rect.Width := Tex.Width;
  Self.Rect.Height := Tex.Height;
  Self.Rect.U1 := 0.0;
  Self.Rect.V1 := 0.0;
  Self.Rect.U2 := 1.0;
  Self.Rect.V2 := 1.0;}
End;

Procedure UIImage.UpdateRects();
Begin
  If Assigned(Self.Texture) Then
  Begin
    Self.Texture.Prefetch();

    If (Self.Width.Value<=0) Then
      Self.Width := UIPixels(Trunc(SafeDiv(Self.Texture.Width, Self.Texture.Ratio.X)));

    If (Self.Height.Value<=0) Then
      Self.Height := UIPixels(Trunc(SafeDiv(Self.Texture.Height, Self.Texture.Ratio.Y)));
  End;

  Inherited;
End;

Procedure UIImage.UpdateSprite(View:TERRAViewport);
Begin
  If _Sprite = Nil Then
  Begin
    _Sprite := QuadSprite.Create();
  End;

  _Sprite.SetTransform(Self._Transform);
  _Sprite.Texture := Self.Texture;

  QuadSprite(_Sprite).Position := VectorCreate2D(0, 0);
  _Sprite.Layer := Self.GetLayer();
  _Sprite.Saturation := Self.GetSaturation();
  _Sprite.Glow := Self.GetGlow();

  If Texture=Nil Then
    Texture := Engine.Textures.WhiteTexture;
  Texture.Filter := Filter;

  QuadSprite(_Sprite).Anchor := Anchor;
  QuadSprite(_Sprite).SetColor(Self.Color);
  QuadSprite(_Sprite).Rect.U1 := _U1.Value;
  QuadSprite(_Sprite).Rect.V1 := _U1.Value;
  QuadSprite(_Sprite).Rect.U2 := _U2.Value;
  QuadSprite(_Sprite).Rect.V2 := _V2.Value;
  QuadSprite(_Sprite).Rect.Width := Trunc(Self.GetDimension(Self.Width, uiDimensionWidth));
  QuadSprite(_Sprite).Rect.Height := Trunc(Self.GetDimension(Self.Height, uiDimensionHeight));
  QuadSprite(_Sprite).Flip := Self.Flip;
  QuadSprite(_Sprite).Mirror := Self.Mirror;
  _Sprite.SetTransform(_Transform);
  _Sprite.ClipRect := Self.ClipRect;
End;

End.