{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

{-$DEFINE ARTSY}

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_String, TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Image, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_Texture, TERRA_Renderer, TERRA_Mesh, TERRA_ShaderFactory,
  TERRA_FileManager, TERRA_Scene,  TERRA_Skybox, TERRA_Color, TERRA_Math, TERRA_Matrix4x4,
  TERRA_ScreenFX, TERRA_VertexFormat, TERRA_InputManager, MinimonPlayer;

Type
  MyScene = Class(Scene)
      Sky:Skybox;

      Constructor Create;
      Procedure Release; Override;

      Procedure RenderSprites(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
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
  MonsterInstance:MeshInstance;
  StageInstance:MeshInstance;

  Sun:DirectionalLight;

  Fnt:Font;
  Tex:Texture;

Function CreatePreparedMesh(MyMesh:Mesh; Toon:Boolean):MeshInstance;
Var
  TexName:TERRAString;
  I:Integer;
Begin
  If Assigned(MyMesh) Then
  Begin
    Result :=MeshInstance.Create(MyMesh);
    Result.Animation.Play('idle');

    For I:=0 To Pred(MyMesh.GroupCount) Do
    Begin
      Result.SetOutlineColor(I, ColorCreate(0, 0, 0, 64));

      Tex := MyMesh.GetGroup(I).DiffuseMap;
      If Tex = Nil Then
        Continue;

      TexName := Tex.Name;

      {$IFDEF ARTSY}
      Tex := TextureManager.Instance.GetTexture(TexName+'_ZONES.png');
      If Assigned(Tex) Then
      Begin
        Tex.Filter := filterLinear;
        Tex.WrapMode := wrapNothing;
        Tex.MipMapped := False;
      End;
      {$ENDIF}

      Result.SetDiffuseMap(I, Tex);
      Result.SetNormalMap(I, TextureManager.Instance.GetTexture(TexName+'_NRM.png'));
      Result.SetDisplacementMap(I, TextureManager.Instance.GetTexture(TexName+'_DISP.png'));
      //Result.SetSpecularMap(I, TextureManager.Instance.GetTexture('monster022_SPEC.png'));

      {$IFDEF ARTSY}
      Tex := TextureManager.Instance.GetTexture('dither8.png');
      Result.SetDitherPatternMap(I, Tex);
      {$ELSE}

      If Toon Then
      Begin
        Tex := TextureManager.Instance.GetTexture('toonramp.png');
        Result.SetToonRamp(I, Tex);
      End;
      {$ENDIF}
    End;
  End Else
    Result := Nil;
End;

{ Game }
Procedure Game.OnCreate;
Var
  Pal, Pat:Texture;
Begin
//  FileManager.Instance.AddPath('D:\code\TERRA-Engine\Samples\Binaries\Assets');
  //FileManager.Instance.AddPath('D:\code\minimonhd\trunk\monsterpal');

  FileManager.Instance.AddPath('D:\code\minimonhd\trunk\output\export');
  FileManager.Instance.AddPath('D:\code\minimonhd\trunk\output\textures');
  FileManager.Instance.AddPath('D:\code\minimonhd\trunk\output\mesh');


  Fnt := FontManager.Instance.DefaultFont;

  GraphicsManager.Instance.Renderer.Settings.NormalMapping.SetValue(True);

  GraphicsManager.Instance.Renderer.Settings.DynamicShadows.SetValue(True);

  GraphicsManager.Instance.Renderer.Settings.CartoonHues.SetValue(True);

//  GraphicsManager.Instance.LightModel := lightModelSimple;

  Pal := TextureManager.Instance.GetTexture('newpalette.png');
  Pal.Filter := filterLinear;
  Pal.WrapMode := wrapNothing;
  Pal.MipMapped := False;

  Pat := TextureManager.Instance.GetTexture('dither8.png');
  Pat.Filter := filterLinear;
  Pat.WrapMode := wrapAll;
  Pat.MipMapped := False;

{$IFDEF ARTSY}
  GraphicsManager.Instance.MainViewport.FXChain.AddEffect(DitherFX.Create(Pat, Pal));
{$ENDIF}

//  GraphicsManager.Instance.MainViewport.FXChain.AddEffect(OutlineFX.Create(3.0));
  GraphicsManager.Instance.MainViewport.FXChain.AddEffect(VibranceFX.Create(1.2));
//  GraphicsManager.Instance.MainViewport.FXChain.AddEffect(ColorGradingFX.Create(TextureManager.Instance.GetTexture('colortable.png')));
  GraphicsManager.Instance.MainViewport.FXChain.AddEffect(BloomFX.Create(0.65));
  GraphicsManager.Instance.MainViewport.FXChain.AntiAlias := True;

  Sun := DirectionalLight.Create(VectorCreate(0.1, 0.8, 0.3));

  MonsterInstance := CreatePreparedMesh(GetMonsterMesh(22), True);
  StageInstance := CreatePreparedMesh(MeshManager.Instance.GetMesh('map101'), False);
  //StageInstance := CreatePreparedMesh(MeshManager.Instance.GetMesh('arena_base0'), False);

//  LightManager.Instance.AmbientColor := ColorWhite;

  _Scene := MyScene.Create;
  GraphicsManager.Instance.Scene := _Scene;

  If Assigned(StageInstance) Then
  Begin
    GraphicsManager.Instance.ActiveViewport.Camera.SetPosition(VectorCreate(100, 85, 180));
    GraphicsManager.Instance.ActiveViewport.Camera.SetView(VectorCreate(0, -0.5, -0.5));
  End Else
    GraphicsManager.Instance.ActiveViewport.Camera.SetPosition(VectorCreate(0, 5, 30));
End;

Procedure Game.OnDestroy;
Begin
  ReleaseObject(_Scene);

  ReleaseObject(Sun);
  ReleaseObject(MonsterInstance);
  ReleaseObject(StageInstance);
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
  Sky := Skybox.Create('sky');
End;

Procedure MyScene.Release;
Begin
  Sky.Release;
End;

Procedure MyScene.RenderSprites;
Begin
End;

Procedure MyScene.RenderViewport(V:Viewport);
Var
  N:Single;
Begin
{  If Assigned(StageInstance) Then
  Begin
    N := Application.Instance.GetTime() / 5000;
    Sun.SetDirection(VectorCreate(Cos(N), 0.5, -Sin(N)));
  End;}

  LightManager.Instance.AddLight(Sun);

  GraphicsManager.Instance.AddRenderable(StageInstance);

  If Assigned(StageInstance) Then
  Begin
    MonsterInstance.SetPosition(VectorCreate(92, 0, 50));
  End Else
    MonsterInstance.SetRotation(VectorCreate(0, Application.Instance.GetTime() / 1000, 0));

//  GraphicsManager.Instance.AddRenderable(MonsterInstance);

End;

Procedure MyScene.RenderSky;
Begin
//  Sky.Render;
End;

Begin
  Game.Create();
End.

