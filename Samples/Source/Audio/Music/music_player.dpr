{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Application, TERRA_Client, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Viewport, TERRA_Texture,
  TERRA_PNG, TERRA_Lights, TERRA_ShaderFactory, TERRA_SpriteManager, TERRA_Vector2D, TERRA_TTF,
  TERRA_FileManager, TERRA_Scene, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_FileUtils, TERRA_OGG,
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
  _Scene := MyScene.Create;

  FileManager.Instance.AddPath('Assets');
  Fnt := FontManager.Instance.GetFont('droid@50');

  GraphicsManager.Instance.Scene := _Scene;
  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorBlue;

  MusicManager.Instance.Play('nox');
  //MusicManager.Instance.Play('mar');
End;

Procedure Game.OnDestroy;
Begin
  MusicManager.Instance.Stop();
  _Scene.Destroy;
End;

Procedure Game.OnIdle;
Begin
  If Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;

  If (Keys.WasPressed(keyLeft)) Then
    MusicManager.Instance.SetVolume(MusicManager.Instance.Volume - 0.1)
  Else
  If (Keys.WasPressed(keyRight)) Then
    MusicManager.Instance.SetVolume(MusicManager.Instance.Volume + 0.1);
End;


{ MyScene }
Procedure MyScene.RenderSprites;
Var
  S:Sprite;
Begin
  If Not Assigned(Fnt) Then
    Exit;

  Fnt.DrawText(5, 5, 5, 'Volume: '+IntToString(Trunc(MusicManager.Instance.Volume * 100))+'%', ColorWhite);

  If Assigned(MusicManager.Instance.CurrentTrack) Then
  Begin
    Fnt.DrawText(5, 40, 5, 'Title: '+ GetFileName(MusicManager.Instance.CurrentTrack.FileName, False), ColorWhite);
    Fnt.DrawText(5, 75, 5, 'Type: '+ MusicManager.Instance.CurrentTrack.ClassName, ColorWhite);
  End;
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

