Unit TERRA_UIImage;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UI, TERRA_UIWidget, TERRA_Color,
  TERRA_UIDimension, TERRA_Sprite, TERRA_Texture, TERRA_Renderer, TERRA_Vector2D;

Type
  UIImage = Class(UIWidget)
    Protected
      _Texture:TextureProperty;
      _TextureIndex:Integer;

      _U1, _V1, _U2, _V2:FloatProperty;


      Function GetTexture: TERRATexture;

      Procedure UpdateSprite; Override;

      Function GetObjectType:TERRAString; Override;

    Public
      Anchor:Vector2D;
      Flip, Mirror:Boolean;
      Filter:TextureFilterMode;

      Constructor Create(Name:TERRAString; Parent:UIWidget; X,Y,Z:Single; Const Width, Height:UIDimension);

      Procedure SetTexture(Tex:TERRATexture);

      Procedure Release; Override;
      Procedure UpdateRects; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Texture:TERRATexture Read GetTexture Write SetTexture;
  End;


Implementation
Uses TERRA_Log;

{ UIImage }
Constructor UIImage.Create(Name:TERRAString; Parent:UIWidget; X, Y, Z: Single; Const Width, Height:UIDimension);
Begin
  Inherited Create(Name, Parent);

  Self.TabIndex := TabIndex;
  Self._Texture := TextureProperty.Create('image', Nil);

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;
  Self.Filter := filterLinear;

  _U1 := FloatProperty.Create('u1', 0);
  _V1 := FloatProperty.Create('v1', 0);
  _U2 := FloatProperty.Create('u2', 1);
  _V2 := FloatProperty.Create('v2', 1);
  
  Self.Width := Width;
  Self.Height := Height;

  Self.Pivot := VectorCreate2D(0, 0);
  Self.Anchor := VectorCreate2D(0, 0);

  Self.ExpandProperties(5);
  _TextureIndex := _BasePropertiesIndex;
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

Function UIImage.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If Index = _TextureIndex Then
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

Function UIImage.GetTexture: TERRATexture;
Begin
  Result := _Texture.Value;
End;

Procedure UIImage.Release;
Begin
  Inherited;
  ReleaseObject(Self._Texture);
  ReleaseObject(Self._U1);
  ReleaseObject(Self._V1);
  ReleaseObject(Self._U2);
  ReleaseObject(Self._V2);  
End;

Procedure UIImage.SetTexture(Tex: TERRATexture);
Begin
  If Tex = Nil Then
    Tex := TextureManager.Instance.WhiteTexture;

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

Procedure UIImage.UpdateSprite;
Var
  OfsX, OfsY:Single;
  Temp, Pos, Center:Vector2D;
Begin
  If _Sprite = Nil Then
  Begin
    _Sprite := QuadSprite.Create();
  End;

  _Sprite.SetTransform(Self._Transform);
  _Sprite.Texture := Self.Texture;

  Self.GetScrollOffset(OfsX, OfsY);
  If (OfsX<>0) Or (OfsY<>0) Then
  Begin
    Temp := Self.RelativePosition;
    Self.SetRelativePosition(VectorAdd2D(Temp, VectorCreate2D(OfsX, OfsY)));
    Self._TransformChanged := True;
    Self.UpdateTransform();

    Pos := Self.AbsolutePosition;
    Self.SetRelativePosition(Temp);
  End Else
    Pos := Self.AbsolutePosition;

(*  If (Pos.X>UIManager.Instance.Width) Or (Pos.Y>UIManager.Instance.Height)
  Or (Pos.X<-Size.X) Or (Pos.Y<-Size.Y) Then
    Exit;*)

  Center := Self.GetSize();
  Center.X := Center.X * _Pivot.X * Scale;
  Center.Y := Center.Y * _Pivot.Y * Scale;
  Center.Add(Pos);

  QuadSprite(_Sprite).Position := Pos;
  QuadSprite(_Sprite).Layer := Self.GetLayer();
  QuadSprite(_Sprite).Saturation := Self.GetSaturation();

  If Texture=Nil Then
    Texture := TextureManager.Instance.WhiteTexture;
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
  _Sprite.ClipRect := Self.GetClipRect();
End;

End.