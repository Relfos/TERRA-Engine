Unit TERRA_UITiledRect;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UI, TERRA_UIWidget, TERRA_Color,
  TERRA_UIDimension, TERRA_Sprite, TERRA_Texture, TERRA_Renderer, TERRA_Vector2D;

Type
  UITiledRect = Class(UIWidget)
    Protected
      _Texture:TextureProperty;
      _TextureIndex:Integer;
      _U1, _V1, _U2, _V2:FloatProperty;

      Function GetTexture: TERRATexture;

      Procedure UpdateSprite; Override;
      Function GetObjectType:TERRAString; Override;

    Public
      Constructor Create(Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension; Const U1, V1, U2, V2:Single);

      Procedure SetTexture(Tex:TERRATexture);

      Procedure Release; Override;
      Procedure UpdateRects; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Texture:TERRATexture Read GetTexture Write SetTexture;
  End;


Implementation
Uses TERRA_Log, TERRA_Scale9Sprite;

{ UITiledRect }
Constructor UITiledRect.Create(Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension; Const U1, V1, U2, V2:Single);
Begin
  Inherited Create(Name, Parent);

  Self._Texture := TextureProperty.Create('image', Nil);

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  _U1 := FloatProperty.Create('u1', U1);
  _V1 := FloatProperty.Create('v1', V1);
  _U2 := FloatProperty.Create('u2', U2);
  _V2 := FloatProperty.Create('v2', V2);

  Self.Width := Width;
  Self.Height := Height;

  Self.Pivot := VectorCreate2D(0, 0);

  Self.ExpandProperties(5);
  _TextureIndex := _BasePropertiesIndex;
End;

Function UITiledRect.GetObjectType: TERRAString;
Begin
  Result := 'UITiledRect';
End;

Function UITiledRect.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If Index = _TextureIndex + 0 Then
    Result := Self._Texture
  Else
  If Index = _TextureIndex + 1 Then
    Result := Self._U1
  Else
  If Index = _TextureIndex + 2 Then
    Result := Self._V1
  Else
  If Index = _TextureIndex + 3 Then
    Result := Self._U2
  Else
  If Index = _TextureIndex + 4 Then
    Result := Self._V2
  Else
    Result := Inherited GetPropertyByIndex(Index);
End;

Function UITiledRect.GetTexture: TERRATexture;
Begin
  Result := _Texture.Value;
End;

Procedure UITiledRect.Release;
Begin
  Inherited;
  ReleaseObject(Self._Texture);
  ReleaseObject(Self._U1);
  ReleaseObject(Self._V1);
  ReleaseObject(Self._U2);
  ReleaseObject(Self._V2);
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

Procedure UITiledRect.UpdateSprite;
Var
  OfsX, OfsY:Single;
  Temp, Pos, Center:Vector2D;
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
  End Else*)
    Pos := Self.AbsolutePosition;

(*  If (Pos.X>UIManager.Instance.Width) Or (Pos.Y>UIManager.Instance.Height)
  Or (Pos.X<-Size.X) Or (Pos.Y<-Size.Y) Then
    Exit;*)

  If Texture=Nil Then
    Texture := TextureManager.Instance.WhiteTexture;

  _Sprite.SetTransform(Self._Transform);
  _Sprite.ClipRect := Self.GetClipRect();
  _Sprite.Texture := Self.Texture;
  _Sprite.Layer := Self.GetLayer();
  _Sprite.Saturation := Self.GetSaturation();
  _Sprite.BlendMode := blendBlend;

  Scale9Sprite(_Sprite).SetPosition(Pos);
  Scale9Sprite(_Sprite).SetColor(Self.Color);
  Scale9Sprite(_Sprite).SetSize(Trunc(Self.GetDimension(Self.Width, uiDimensionWidth)),  Trunc(Self.GetDimension(Self.Height, uiDimensionHeight)));
  Scale9Sprite(_Sprite).SetUVRect(_U1.Value, _V1.Value, _U2.Value, _V2.Value);
End;

End.
