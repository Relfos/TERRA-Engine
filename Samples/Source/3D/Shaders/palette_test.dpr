{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_String, TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Image, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_Texture, TERRA_Renderer, TERRA_Mesh, TERRA_ShaderFactory,
  TERRA_FileManager, TERRA_Scene,  TERRA_Skybox, TERRA_Color, TERRA_Math, TERRA_Matrix4x4,
  TERRA_ScreenFX, TERRA_SpriteManager, TERRA_VertexFormat, TERRA_InputManager;

Type
  MyScene = Class(Scene)

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderSky(V:Viewport); Override;
  End;

  Game = Class(Application)
    Protected
      _Scene:MyScene;

    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;


Var
  Fnt:Font;
  Palette, Tex1, Tex2:Texture;

Function MakeTexture(Purple, Green, Yellow:Color):Texture;
Var
  Img:Image;
  I, J:Integer;
  N, K, Dist:Single;
  T:ColorHSL;
  A, B, C, S:Color;
Begin
  Img := Image.Create('newpalette.png');

  For J:=0 To Pred(Img.Height) Do
  Begin
    C := Img.GetPixel(4, J);

    For I:=0 To Pred(Img.Width) Do
    Begin
{      If I = 4 Then
        Continue;}

      K := I / Pred(Img.Width);

      C := Img.GetPixel(I, J);
      Dist := ColorLuminance(C);

      T.H := Trunc(255 * (J / Pred(Img.Height)));
      T.L := Trunc(K*255);
      T.S := 128;
      T.A := 255;
      C := ColorHSLToRGB(T);

      S := ColorMix(Yellow, Green, K);

      N := K - 0.5;

      A.R := Trunc(FloatMin(255, FloatMax(0, C.R + N * S.R)));
      A.G := Trunc(FloatMin(255, FloatMax(0, C.G + N * S.G)));
      A.B := Trunc(FloatMin(255, FloatMax(0, C.B + N * S.B)));

      S := ColorMix(Yellow, Purple, K);
      B := ColorMix(S, A, 0.333);

      //B := colorGrey(Trunc(255*K));

      B.A := 255;

      Img.SetPixel(I, J, B);
    End;
  End;

  Result := Texture.Create();
  Result.CreateFromImage(CardinalToString(Application.Instance.GetTime()), Img);
  Result.Filter := filterBilinear;
  Result.WrapMode := wrapNothing;
  Result.MipMapped := False;
End;

{ Game }
Procedure Game.OnCreate;
Var
  Purple, Green, Yellow:Color;
Begin
  Fnt := FontManager.Instance.DefaultFont;


  Palette := TextureManager.Instance.GetTexture('newpalette.png');
  Palette.Filter := filterLinear;
  Palette.WrapMode := wrapNothing;
  Palette.MipMapped := False;

  Yellow := ColorCreate(255, 255, 64);
  Green := ColorCreate(64, 255, 64);
  Purple := ColorCreate(255, 64, 255);
  Tex1 := MakeTexture(Purple, Green, Yellow);

  Yellow := ColorCreate(255, 255, 255);
  Green := ColorCreate(255, 64, 64);
  Purple := ColorCreate(64, 64, 255);
  Tex2 := MakeTexture(Purple, Green, Yellow);

  _Scene := MyScene.Create();
  GraphicsManager.Instance.SetScene(_Scene);
End;

Procedure Game.OnDestroy;
Begin
  ReleaseObject(_Scene);
  ReleaseObject(Tex1);
  ReleaseObject(Tex2);
End;

Procedure Game.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate();

  GraphicsManager.Instance.TestDebugKeys();

  GraphicsManager.Instance.ActiveViewport.Camera.FreeCam;
End;


{ MyScene }
Constructor MyScene.Create;
Begin
End;

Procedure MyScene.Release;
Begin
End;

Procedure MyScene.RenderSprites;
Var
  S:QuadSprite;
Begin
  S := SpriteManager.Instance.DrawSprite(0, 0, 20, Palette);
  S.Rect.Width := 256;
  S.Rect.Height := S.Rect.Width * 2;

  S := SpriteManager.Instance.DrawSprite(300, 0, 20, Tex1);
  S.Rect.Width := 256;
  S.Rect.Height := S.Rect.Width * 2;

  S := SpriteManager.Instance.DrawSprite(600, 0, 20, Tex2);
  S.Rect.Width := 256;
  S.Rect.Height := S.Rect.Width * 2;
End;


Procedure MyScene.RenderSky;
Begin
//  Sky.Render;
End;

Begin
  Game.Create();
End.

