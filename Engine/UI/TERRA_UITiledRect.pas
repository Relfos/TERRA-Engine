Unit TERRA_UITiledRect;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UIWidget, TERRA_Color,
  TERRA_UIDimension, TERRA_Sprite, TERRA_Texture, TERRA_Renderer, TERRA_Vector2D, TERRA_Viewport;

Type
  UITiledRect = Class(UIWidget)
    Protected
      _Texture:TextureProperty;
      _U1, _V1, _U2, _V2:FloatProperty;

      Function GetTexture: TERRATexture;

      Procedure UpdateSprite(View:TERRAViewport); Override;
      Function GetObjectType:TERRAString; Override;

    Public
      Constructor Create(Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension;
        Const U1:Single=0.25; Const V1:Single=0.25; Const U2:Single=0.25; Const V2:Single=0.25);

      Procedure SetTexture(Tex:TERRATexture);

      Procedure UpdateRects; Override;

      Property Texture:TERRATexture Read GetTexture Write SetTexture;
  End;


Implementation
Uses TERRA_Log, TERRA_EngineManager, TERRA_Scale9Sprite;

{ UITiledRect }
Constructor UITiledRect.Create(Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension; Const U1, V1, U2, V2:Single);
Begin
  Inherited Create(Name, Parent);


  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;

  Self._Texture := TextureProperty(Self.AddProperty(TextureProperty.Create('image', Nil), False));
  _U1 := FloatProperty(Self.AddProperty(FloatProperty.Create('u1', U1), False));
  _V1 := FloatProperty(Self.AddProperty(FloatProperty.Create('v1', V1), False));
  _U2 := FloatProperty(Self.AddProperty(FloatProperty.Create('u2', U2), False));
  _V2 := FloatProperty(Self.AddProperty(FloatProperty.Create('v2', V2), False));

  Self.Width := Width;
  Self.Height := Height;

  Self.Pivot := Vector2D_Create(0, 0);
End;

Function UITiledRect.GetObjectType: TERRAString;
Begin
  Result := 'UITiledRect';
End;

Function UITiledRect.GetTexture: TERRATexture;
Begin
  Result := _Texture.Value;
End;

Procedure UITiledRect.SetTexture(Tex: TERRATexture);
Begin
  If Tex = Nil Then
    Tex := Engine.Textures.WhiteTexture;

  _Texture.Value := Tex;
End;

Procedure UITiledRect.UpdateRects();
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

Procedure UITiledRect.UpdateSprite(View:TERRAViewport);
Var
  OfsX, OfsY:Single;
  //Temp, Pos, Center:Vector2D;
Begin
  If _Sprite = Nil Then
    _Sprite := Scale9Sprite.Create()
  Else
    _Sprite.Clear();

  If Texture=Nil Then
    Texture := Engine.Textures.WhiteTexture;

  _Sprite.SetTransform(Self._Transform);
  _Sprite.ClipRect := Self.ClipRect;
  _Sprite.Texture := Self.Texture;
  _Sprite.Layer := Self.GetLayer();
  _Sprite.Saturation := Self.GetSaturation();
  _Sprite.Glow := Self.GetGlow();
  _Sprite.BlendMode := blendBlend;

  Scale9Sprite(_Sprite).SetPosition(Vector2D_Create(0, 0));
  Scale9Sprite(_Sprite).SetColor(Self.GetColor());
  Scale9Sprite(_Sprite).SetSize(Trunc(Self.GetDimension(Self.Width, uiDimensionWidth)),  Trunc(Self.GetDimension(Self.Height, uiDimensionHeight)));
  Scale9Sprite(_Sprite).SetUVRect(_U1.Value, _V1.Value, _U2.Value, _V2.Value);

  Scale9Sprite(_Sprite).Update();
End;

End.
