{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Application, TERRA_Utils, TERRA_ResourceManager, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Viewport, TERRA_Texture,
  TERRA_PNG, TERRA_Lights, TERRA_ShaderFactory, TERRA_SpriteManager, TERRA_Vector2D, TERRA_TTF,
  TERRA_FileManager, TERRA_Scene, TERRA_Mesh, TERRA_Skybox, TERRA_Color, TERRA_FileUtils, TERRA_OGG,
  TERRA_MusicManager, TERRA_FontRenderer, TERRA_InputManager;

Type
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

  Demo = Class(Application)
    Public
      _Scene:MyScene;
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;

Var
  Fnt:FontRenderer;

{ Demo }
Procedure Demo.OnCreate;
Begin
  _Scene := MyScene.Create;

  FileManager.Instance.AddPath('Assets');
  Fnt := FontRenderer.Create();
  Fnt.SetFont(FontManager.Instance.GetFont('droid'));

  GraphicsManager.Instance.Scene := _Scene;
  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorBlue;

  MusicManager.Instance.Play('mar');
End;

Procedure Demo.OnDestroy;
Begin
  MusicManager.Instance.Stop();
  ReleaseObject(_Scene);
End;

Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;

  If (InputManager.Instance.Keys.WasPressed(keyLeft)) Then
    MusicManager.Instance.SetVolume(MusicManager.Instance.Volume - 0.1)
  Else
  If (InputManager.Instance.Keys.WasPressed(keyRight)) Then
    MusicManager.Instance.SetVolume(MusicManager.Instance.Volume + 0.1);
End;


{ MyScene }
Procedure MyScene.RenderSprites;
Var
  S:Sprite;
Begin
  If Not Assigned(Fnt.Font) Then
    Exit;

  Fnt.DrawText(5, 60, 5, 'Volume: '+IntToString(Trunc(MusicManager.Instance.Volume * 100))+'%');

  If Assigned(MusicManager.Instance.CurrentTrack) Then
  Begin
    Fnt.DrawText(5, 80, 5, 'Title: '+ GetFileName(MusicManager.Instance.CurrentTrack.FileName, False));
    Fnt.DrawText(5, 100, 5, 'Type: '+ MusicManager.Instance.CurrentTrack.ClassName);
  End;
End;

{$IFDEF IPHONE}
Procedure StartDemo; cdecl; export;
{$ENDIF}
Begin
  Demo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

