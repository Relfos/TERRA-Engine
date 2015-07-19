Unit TERRA_UISprite;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_UIDimension, TERRA_SpriteManager, TERRA_Texture, TERRA_Renderer;

Type
  UISprite = Class(Widget)
    Protected
      _Texture:TextureProperty;
      _TextureIndex:Integer;

      Function GetTexture: TERRATexture;
      
    Public
      U1, V1, U2, V2:Single;

      Anchor:Vector2D;
      Flip, Mirror:Boolean;
      Filter:TextureFilterMode;

      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Picture:TERRAString = ''; TabIndex:Integer=-1);

      Procedure SetTexture(Tex:TERRATexture);

      Procedure Render; Override;
      Procedure Release; Override;
      Procedure UpdateRects; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Texture:TERRATexture Read GetTexture Write SetTexture;
  End;


Implementation
Uses TERRA_Log;

{ UISprite }
Constructor UISprite.Create(Name:TERRAString; Parent:Widget; X, Y, Z: Single;  Picture:TERRAString; TabIndex: Integer);
Begin
  Inherited Create(Name, Parent, '');

  Self.TabIndex := TabIndex;
  Self._Texture := TextureProperty.Create('image', Nil);

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;
  Self.Filter := filterLinear;

  Self.U1 := 0;
  Self.U2 := 1.0;
  Self.V1 := 0;
  Self.V2 := 1.0;

  If (Picture<>'') Then
  Begin
    Self.Texture := TextureManager.Instance.GetTexture(Picture);

    If (Assigned(Self.Texture)) Then
      Self.Texture.PreserveQuality := True
    Else
      Log(logWarning, 'UI', 'Missing texture for SpriteWidget: '+Picture);
  End Else
    Self.Texture := Nil;


  Self.Pivot := VectorCreate2D(0, 0);
  Self.Anchor := VectorCreate2D(0, 0);

  Self.ExpandProperties(1);
  _TextureIndex := _BasePropertiesIndex;
End;

{Function UISprite.OnRegion(X, Y: Integer): Boolean;
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

Function UISprite.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  If Index = _TextureIndex Then
    Result := Self._Texture
  Else
    Result := Inherited GetPropertyByIndex(Index);
End;

Function UISprite.GetTexture: TERRATexture;
Begin
  Result := _Texture.Value;
End;

Procedure UISprite.Release;
Begin
  Inherited;
  ReleaseObject(Self._Texture);
End;

Procedure UISprite.Render;
Var
  ID:Integer;
  MyColor:TERRA_Color.Color;
  S:QuadSprite;
  Temp, Pos, Center, TC1, TC2:Vector2D;
  OfsX, OfsY:Single;
Begin
  Self.UpdateRects();
  Self.UpdateTransform();

  If (Not DisableHighlights) And (Assigned(Self.OnMouseClick)) Then
    Self.UpdateHighlight();

  MyColor := Self.Color;

  {$IFDEF OUYA}
  If (UI.Highlight<>Nil) And (Not Self.IsHighlighted) And (Assigned(Self.OnMouseClick)) Then
    MyColor := ColorGrey(64);
  {$ENDIF}

  If Texture=Nil Then
    Texture := TextureManager.Instance.WhiteTexture;

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

  If (Pos.X>UIManager.Instance.Width) Or (Pos.Y>UIManager.Instance.Height)
  Or (Pos.X<-Size.X) Or (Pos.Y<-Size.Y) Then
    Exit;


  Center := Self.GetSize();
  Center.X := Center.X * _Pivot.X * Scale;
  Center.Y := Center.Y * _Pivot.Y * Scale;
  Center.Add(Pos);

  S := SpriteManager.Instance.DrawSprite(Pos.X, Pos.Y, Self.GetLayer(), Texture, Nil, BlendBlend, Self.GetSaturation(), Filter);
  S.Anchor := Anchor;
  S.SetColor(MyColor);
  S.Rect.U1 := U1;
  S.Rect.V1 := U1;
  S.Rect.U2 := U2;
  S.Rect.V2 := V2;
  S.Rect.Width := Trunc(Self.GetDimension(Self.Width, uiDimensionWidth));
  S.Rect.Height := Trunc(Self.GetDimension(Self.Height, uiDimensionHeight));
  S.SetTransform(_Transform);
  S.Flip := Self.Flip;
  S.ClipRect := Self.GetClipRect();
  S.Mirror := Self.Mirror;

  Inherited;
End;

Procedure UISprite.SetTexture(Tex: TERRATexture);
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

Procedure UISprite.UpdateRects();
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

End.