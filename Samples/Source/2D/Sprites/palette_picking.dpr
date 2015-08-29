{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_String, TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Image, TERRA_Viewport,
  TERRA_PNG, TERRA_Texture, TERRA_Renderer, TERRA_Mesh, TERRA_ShaderFactory, TERRA_Resource,
  TERRA_FileManager, TERRA_Scene,  TERRA_Skybox, TERRA_Color, TERRA_Math, TERRA_Matrix4x4,
  TERRA_ScreenFX, TERRA_SpriteManager, TERRA_VertexFormat, TERRA_InputManager;

Type
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

  Game = Class(Application)
    Protected
      _Scene:MyScene;

    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;
  End;


Const
  SpriteX = 100;
  SpriteY = 100;
  SpriteScale = 32;

Var
  PaletteTex:Texture;

Function MakeTexture(Hues, Shades:Integer):Texture;
Var
  Img, Dest:Image;
  I, J, M, N:Integer;
  K:Single;
  T:ColorHSL;
  C, B:Color;
Begin
  Dest := Image.Create(Shades, Hues);

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

  Result := Texture.Create(rtDynamic, '');
  Result.InitFromImage(Dest);
  Result.Filter := filterBilinear;
  Result.WrapMode := wrapNothing;
  Result.MipMapped := False;

  ReleaseObject(Dest);
End;

{ Game }
Procedure Game.OnCreate;
Begin
  PaletteTex := MakeTexture(16, 8);

  _Scene := MyScene.Create();
  GraphicsManager.Instance.SetScene(_Scene);
End;

Procedure Game.OnDestroy;
Begin
  ReleaseObject(_Scene);
  ReleaseObject(PaletteTex);
End;

Procedure Game.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  GraphicsManager.Instance.TestDebugKeys();
End;


procedure Game.OnMouseDown(X, Y: Integer; Button: Word);
Begin
  Dec(X, SpriteX);
  Dec(Y, SpriteY);

  X := Trunc(X / SpriteScale);
  Y := Trunc(Y / SpriteScale);

  If (X>=PaletteTex.Width) Or (Y>=PaletteTex.Height) Then
    Exit;

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := PaletteTex.GetPixel(X, Y);
End;

{ MyScene }
Procedure MyScene.RenderSprites;
Var
  PaletteSprite:QuadSprite;
Begin
  PaletteSprite := SpriteManager.Instance.DrawSprite(SpriteX, SpriteY, 20, PaletteTex);
  PaletteSprite.Rect.Width := PaletteTex.Width * SpriteScale;
  PaletteSprite.Rect.Height := PaletteTex.Height * SpriteScale;
End;

Begin
  Game.Create();
End.

