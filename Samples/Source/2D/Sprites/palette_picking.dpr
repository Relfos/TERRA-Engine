{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Application, TERRA_Utils, TERRA_DemoApplication, TERRA_Color, TERRA_Texture, TERRA_Image, TERRA_Viewport,
  TERRA_Resource, TERRA_Renderer, TERRA_Sprite, TERRA_Vector2D, TERRA_Engine;

Type
  Game = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;

      Procedure OnRender2D(V:TERRAViewport); Override;

      Procedure OnMouseDown(Const X,Y:Single; Const Button:Word); Override;
  End;


Const
  SpriteX = 100;
  SpriteY = 100;
  SpriteScale = 32;

Var
  PaletteTex:TERRATexture;

Function MakeTexture(Hues, Shades:Integer):TERRATexture;
Var
  Img, Dest:TERRAImage;
  I, J, M, N:Integer;
  K:Single;
  T:ColorHSL;
  C, B:ColorRGBA;
Begin
  Dest := TERRAImage.Create(Shades, Hues);

  M := (Shades Shr 1);
  For J:=0 To Pred(Hues) Do
  Begin
    N := J;

    For I:=0 To Pred(Shades) Do
    Begin
      K := I / Pred(Shades);

      T.H := Trunc(255 * (N/Hues));

      If (I>M) Then
        Inc(T.H, Trunc((1 -K) * 16))
      Else
      If (I<M) Then
        Dec(T.H, Trunc((1 -K) * 16));

      T.L := 8 + Trunc((I/Shades)*242);

      K := Abs(I-M)/M;

      T.S := 48 + Trunc(128* (1.0-K));

      T.A := 255;

      C := ColorHSLToRGB(T);

      Dest.SetPixel(I, J, C);
    End;
  End;

  Result := TERRATexture.Create(rtDynamic);
  Result.InitFromImage(Dest);
  Result.Filter := filterBilinear;
  Result.WrapMode := wrapNothing;
  Result.MipMapped := False;

  ReleaseObject(Dest);
End;

{ Game }
Procedure Game.OnCreate;
Begin
  Inherited;


  Self.GUI.Viewport.Visible := True;

  PaletteTex := MakeTexture(16, 8);
End;

Procedure Game.OnDestroy;
Begin
  ReleaseObject(PaletteTex);

  Inherited;
End;

procedure Game.OnMouseDown(Const X,Y:Single; Const Button:Word);
Var
  PickedColor:ColorRGBA;
  PX, PY:Integer;
Begin
  PX := Trunc(Self.GUI.Viewport.Width * X);
  PY := Trunc(Self.GUI.Viewport.Height * Y);

  Dec(PX, SpriteX);
  Dec(PY, SpriteY);

  PX := Trunc(PX / SpriteScale);
  PY := Trunc(PY / SpriteScale);

  If (PX<0) Or (PY<0) Or (PX>=PaletteTex.Width) Or (PY>=PaletteTex.Height) Then
    Exit;

  PickedColor := PaletteTex.GetPixel(PX, PY);
  Engine.Graphics.DeviceViewport.BackgroundColor := PickedColor;
End;

Procedure Game.OnRender2D(V:TERRAViewport);
Var
  PaletteSprite:TERRASprite;
Begin
  PaletteSprite := Engine.FetchSprite();
  PaletteSprite.Translate(SpriteX, SpriteY);
  PaletteSprite.Layer := 20;
  PaletteSprite.SetTexture(PaletteTex);

  PaletteSprite.AddQuad(SpriteAnchor_TopLeft, Vector2D_Zero, 0.0, PaletteTex.Width * SpriteScale, PaletteTex.Height * SpriteScale);

  Engine.Graphics.AddRenderable(V, PaletteSprite);

  Inherited;
End;

Begin
  Game.Create();
End.

