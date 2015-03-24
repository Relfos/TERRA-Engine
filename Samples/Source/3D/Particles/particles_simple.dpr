{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_Stream, TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_Viewport, TERRA_RenderTarget,
  TERRA_JPG, TERRA_PNG, TERRA_Lights, TERRA_ShaderFactory, TERRA_TTF, TERRA_Math,
  TERRA_UI, TERRA_Widgets, TERRA_Texture, TERRA_SpriteManager, TERRA_Shader,
  TERRA_MeshAnimation, TERRA_Image, TERRA_ScreenFX, TERRA_BoundingBox, TERRA_InputManager,
  TERRA_ParticleRenderer, TERRA_ParticleEmitters,
  TERRA_FileManager, TERRA_Scene, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_Matrix4x4;

Const
  SpriteRes = 512;

  DefaultZoom = 45;

  FXName = 'smoke';

Type
  Game = Class(AppClient)
    Public
      Procedure SelectResolution3D(Var Width, Height:Integer); Override;
			Procedure OnCreate; Override;
			Procedure OnIdle; Override;

      Function GetWidth:Word; Override;
      Function GetHeight:Word; Override;
  End;

  MyScene = Class(Scene)

      Constructor Create;

      Procedure RenderViewport(V:Viewport); Override;
  End;

Var
  Zoom:Single;

  Target:Integer;

  Particles:ParticleCollection;

  LastSpawn:Cardinal;


{ Game }
Function Game.GetHeight: Word;
Begin
  Result := SpriteRes;
End;

Function Game.GetWidth: Word;
Begin
  Result := SpriteRes;
End;

Procedure Game.OnIdle;
Var
  Pos:Vector3D;
  Emitter:ParticleEmitter;
Begin
  If InputManager.Instance.Keys.IsDown(keyEscape) Then
    Application.Instance.Terminate;

  // if space key is pressed, spawn a particle emitter
  If (InputManager.Instance.Keys.WasPressed(keySpace)) And (GetTime - LastSpawn>1000) And (Particles = Nil) Then
  Begin
    Pos := VectorCreate(0, 0, -30);

    LastSpawn := GetTime();

    Emitter := ParticleSettingsEmitter.Create(FXName, Pos);

    If InputManager.Instance.Keys.IsDown(keyShift) Then
      Particles := ParticleCollection.Create(Emitter)
    Else
      ParticleManager.Instance.Spawn(Emitter);
  End;

  GraphicsManager.Instance.ActiveViewport.Camera.FreeCam();
End;


Procedure Game.SelectResolution3D(var Width, Height: Integer);
Begin
  Width := SpriteRes;
  Height := SpriteRes;
End;

{ MyScene }
Constructor MyScene.Create;
Begin
End;

Procedure Game.OnCreate;
Var
  A,B,C:Vector3D;
Begin
  FileManager.Instance.AddPath('assets');

  // set background color
  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorGrey(64);

  GraphicsManager.Instance.Scene := MyScene.Create;
End;


Procedure MyScene.RenderViewport(V:Viewport);
Begin
  GraphicsManager.Instance.AddRenderable(Particles);
End;


{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  ApplicationStart(Game.Create);
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

