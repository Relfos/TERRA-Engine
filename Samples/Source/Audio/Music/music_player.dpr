{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_IO, TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Viewport, TERRA_Texture,
  TERRA_PNG, TERRA_Lights, TERRA_ShaderFactory, TERRA_SpriteManager, TERRA_Vector2D,
  TERRA_FileManager, TERRA_Scene, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_Ogg,
  TERRA_MusicManager;

Type
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

  Game = Class(AppClient)
    Public
      _Scene:MyScene;
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;

Var
  Fnt:Font;

{ Game }
Procedure Game.OnCreate;
Begin
  Fnt := FontManager.Instance.DefaultFont;
  _Scene := MyScene.Create;

  FileManager.Instance.AddPath('Assets');

  GraphicsManager.Instance.Scene := _Scene;
  GraphicsManager.Instance.BackgroundColor := ColorBlue;

  MusicManager.Instance.Play('nox');
End;

Procedure Game.OnDestroy;
Begin
  MusicManager.Instance.Stop();
  _Scene.Destroy;
End;

Procedure Game.OnIdle;
Begin
  If Keys[keyEscape] Then
    Application.Instance.Terminate;
End;


{ MyScene }
Procedure MyScene.RenderSprites;
Var
  S:Sprite;
Begin
  If Not Assigned(Fnt) Then
    Exit;

  //Fnt.DrawText(5, 5, 5, 'FPS: '+IntToString(Renderer.Instance.Stats.FramesPerSecond), ColorWhite);
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

