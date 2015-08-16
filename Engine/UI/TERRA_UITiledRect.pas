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
      Constructor Create(Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension; Const U1, V1, U2, V2:Single);

      Procedure SetTexture(Tex:TERRATexture);

      Procedure UpdateRects; Override;

      Property Texture:TERRATexture Read GetTexture Write SetTexture;
  End;


Implementation
Uses TERRA_Log, TERRA_Scale9Sprite;

{ UITiledRect }
Constructor UITiledRect.Create(Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension; Const U1, V1, U2, V2:Single);
Begin
  Inherited Create(Name, Parent);


  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  Self._Texture := TextureProperty(Self.AddProperty(TextureProperty.Create('image', Nil), False));
  _U1 := FloatProperty(Self.AddProperty(FloatProperty.Create('u1', U1), False));
  _V1 := FloatProperty(Self.AddProperty(FloatProperty.Create('v1', V1), False));
  _U2 := FloatProperty(Self.AddProperty(FloatProperty.Create('u2', U2), False));
  _V2 := FloatProperty(Self.AddProperty(FloatProperty.Create('v2', V2), False));

  Self.Width := Width;
  Self.Height := Height;

  Self.Pivot := VectorCreate2D(0, 0);
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
    Tex := TextureManager.Instance.WhiteTexture;

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
  Begin
    _Sprite := Scale9Sprite.Create();
  End;

  (*Self.GestScrollOffset(OfsX, OfsY);
  If (OfsX<>0) Or (OfsY<>0) Then
  Begin
    Temp := Self.RelativePosition;
    Self.SetRelativePosition(VectorAdd2D(Temp, VectorCreate2D(OfsX, OfsY)));
    Self._TransformChanged := True;
    Self.UpdateTransform();

    Pos := Self.AbsolutePosition;
    Self.SetRelativePosition(Temp);
  End Else
    Pos := Self.AbsolutePosition;*)

(*  If (Pos.X>UIManager.Instance.Width) Or (Pos.Y>UIManager.Instance.Height)
  Or (Pos.X<-Size.X) Or (Pos.Y<-Size.Y) Then
    Exit;*)

  If Texture=Nil Then
    Texture := TextureManager.Instance.WhiteTexture;

  _Sprite.SetTransform(Self._Transform);
  _Sprite.ClipRect := Self.ClipRect;
  _Sprite.Texture := Self.Texture;
  _Sprite.Layer := Self.GetLayer();
  _Sprite.Saturation := Self.GetSaturation();
  _Sprite.BlendMode := blendBlend;

  Scale9Sprite(_Sprite).SetPosition(VectorCreate2D(0, 0));
  Scale9Sprite(_Sprite).SetColor(Self.GetColor());
  Scale9Sprite(_Sprite).SetSize(Trunc(Self.GetDimension(Self.Width, uiDimensionWidth)),  Trunc(Self.GetDimension(Self.Height, uiDimensionHeight)));
  Scale9Sprite(_Sprite).SetUVRect(_U1.Value, _V1.Value, _U2.Value, _V2.Value);
End;

End.
