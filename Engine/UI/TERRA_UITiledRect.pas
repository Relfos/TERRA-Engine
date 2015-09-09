Unit TERRA_UITiledRect;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UIWidget, TERRA_Color,
  TERRA_UIDimension, TERRA_Sprite, TERRA_Texture, TERRA_Renderer, TERRA_Vector2D, TERRA_Viewport;

Type
  UITiledRect = Class(UIWidget)
    Protected
      _Texture:TextureProperty;
      _PX1, _PY1, _PX2, _PY2:IntegerProperty;

      Function GetTexture: TERRATexture;

      Procedure UpdateSprite(); Override;
      Class Function GetObjectType:TERRAString; Override;

    Public
      Constructor Create(Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension; Const PX1, PY1, PX2, PY2:Integer);

      Procedure SetTexture(Tex:TERRATexture);

      Procedure UpdateRects; Override;

      Property Texture:TERRATexture Read GetTexture Write SetTexture;
  End;


Implementation
Uses TERRA_Log, TERRA_Engine;

{ UITiledRect }
Constructor UITiledRect.Create(Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension; Const PX1, PY1, PX2, PY2:Integer);
Begin
  Inherited Create(Name, Parent);


  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;

  Self._Texture := TextureProperty(Self.AddProperty(TextureProperty.Create('image', Nil), False));
  _PX1 := IntegerProperty(Self.AddProperty(IntegerProperty.Create('x1', PX1), False));
  _PY1 := IntegerProperty(Self.AddProperty(IntegerProperty.Create('y1', PY1), False));
  _PX2 := IntegerProperty(Self.AddProperty(IntegerProperty.Create('x2', PX2), False));
  _PY2 := IntegerProperty(Self.AddProperty(IntegerProperty.Create('y2', PY2), False));

  Self.Width := Width;
  Self.Height := Height;

  Self.Pivot := Vector2D_Create(0, 0);
End;

Class Function UITiledRect.GetObjectType: TERRAString;
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

Procedure UITiledRect.UpdateSprite();
Var
  I,J:Integer;
  LeftX, LeftY:Integer;
  MidX, MidY:Integer;
  CompX, CompY:Integer;
  N, FracX, FracY:Single;

  U1, V1, U2, V2:Single;
  //Temp, Pos, Center:Vector2D;
Begin
  _FullSize := _CurrentSize;

  If Texture=Nil Then
    Texture := Engine.Textures.WhiteTexture;

  If Not Texture.IsReady Then
    Exit;

  If _Sprite = Nil Then
    _Sprite := TERRASprite.Create()
  Else
    _Sprite.Clear();

  _Sprite.SetTransform(Self._Transform);
  _Sprite.ClipRect := Self.ClipRect;
  _Sprite.Texture := Self.Texture;
  _Sprite.Layer := Self.GetLayer();
  _Sprite.Saturation := Self.GetSaturation();
  _Sprite.Glow := Self.GetGlow();
  _Sprite.BlendMode := blendBlend;
  _Sprite.SetColor(Self.GetColor());


  LeftX := (Texture.Width  - Self._PX2.Value);
  LeftY := (Texture.Height  - Self._PY2.Value);

  MidX := Self._PX2.Value - Self._PX1.Value;
  MidY := Self._PY2.Value - Self._PY1.Value;

  N := (Self.CurrentSize.X - (Self._PX1.Value + LeftX)) / MidX;
  FracX := Frac(N);
  CompX := Trunc(N);

  N := (Self.CurrentSize.Y - (Self._PY1.Value + LeftY)) / MidY;
  FracY := Frac(N);
  CompY := Trunc(N);

  U1 := Self._PX1.Value / Texture.Width;
  V1 := Self._PY1.Value / Texture.Height;
  U2 := Self._PX2.Value / Texture.Width;
  V2 := Self._PY2.Value / Texture.Height;

  _Sprite.SetUVs(0.0, 0.0, U1, V1);
  _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Self._PX1.Value, Self._PY1.Value);

  _Sprite.SetUVs(U2, 0.0, 1.0, V1);
  _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(_CurrentSize.X - LeftX, 0), 0.0, LeftX, Self._PY1.Value);

  _Sprite.SetUVs(0.0, V2, U1, 1.0);
  _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, _CurrentSize.Y - LeftY), 0.0, Self._PX1.Value, LeftY);

  _Sprite.SetUVs(U2, V2, 1.0, 1.0);
  _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(_CurrentSize.X - LeftX, _CurrentSize.Y - LeftY), 0.0, LeftX, LeftY);

  _Sprite.SetUVs(U1, 0.0, U2, V1);
  For I:=0 To Pred(CompX) Do
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value  + I * MidX, 0.0), 0.0, MidX, Self._PY1.Value);

  If (FracX>0) Then
  Begin
    _Sprite.SetUVs(U1, 0.0, U1 + (U2 - U1) * FracX, V1);
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value  + CompX * MidX, 0.0), 0.0, MidX * FracX, Self._PY1.Value);
  End;

  _Sprite.SetUVs(U1, V2, U2, 1.0);
  For I:=0 To Pred(CompX) Do
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value  + I * MidX, _CurrentSize.Y - LeftY), 0.0, MidX, LeftY);

  If (FracX>0) Then
  Begin
    _Sprite.SetUVs(U1, V2, U1 + (U2 - U1) * FracX, 1.0);
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value  + CompX * MidX, _CurrentSize.Y - LeftY), 0.0, MidX * FracX, LeftY);
  End;

  _Sprite.SetUVs(0.0, V1, U1, V2);
  For J:=0 To Pred(CompY) Do
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, Self._PY1.Value  + J * MidY), 0.0, Self._PX1.Value, MidY);

  If (FracY>0) Then
  Begin
    _Sprite.SetUVs(0.0, V1, U1,  V1 + (V2 - V1) * FracY);
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, Self._PY1.Value  + CompY * MidY), 0.0, Self._PX1.Value, MidY * FracY);
  End;

  _Sprite.SetUVs(U2, V1, 1.0, V2);
  For J:=0 To Pred(CompY) Do
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(_CurrentSize.X - LeftX, Self._PY1.Value  + J * MidY), 0.0, LeftX, MidY);

  If (FracY>0) Then
  Begin
    _Sprite.SetUVs(U2, V1, 1.0, V1 + (V2 - V1) * FracY);
    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(_CurrentSize.X - LeftX, Self._PY1.Value  + CompY * MidY), 0.0, LeftX, MidY * FracY);
  End;


  _Sprite.SetUVs(U1, V1, U2, V2);
  For J:=0 To Pred(CompY) Do
    For I:=0 To Pred(CompX) Do
    Begin
      _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value + I * MidX, Self._PY1.Value  + J * MidY), 0.0, MidX, MidY);
    End;

  If (FracX>0) Then
  Begin
    _Sprite.SetUVs(U1, V1, U1 + (U2 - U1) * FracX, V2);

    For J:=0 To Pred(CompY) Do
      _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value + CompX * MidX, Self._PY1.Value  + J * MidY), 0.0, MidX * FracX, MidY);
  End;

  If (FracY>0) Then
  Begin
    _Sprite.SetUVs(U1, V1, U2, V1 + (V2 - V1) * FracY);

    For I:=0 To Pred(CompX) Do
      _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value + I * MidX, Self._PY1.Value  + CompY * MidY), 0.0, MidX, MidY * FracY);
  End;

  If (FracY>0) And (FracX>0) Then
  Begin
    _Sprite.SetUVs(U1, V1, U1 + (U2 - U1) * FracX, V1 + (V2 - V1) * FracY);

    _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(Self._PX1.Value + CompX * MidX, Self._PY1.Value  + CompY * MidY), 0.0, MidX * FracX, MidY * FracY);
  End;

  //Scale9Sprite(_Sprite).SetSize(Trunc(Self.GetDimension(Self.Width, uiDimensionWidth)),  Trunc(Self.GetDimension(Self.Height, uiDimensionHeight)));
  //Scale9Sprite(_Sprite).SetUVRect(_U1.Value, _V1.Value, _U2.Value, _V2.Value);
End;

End.
