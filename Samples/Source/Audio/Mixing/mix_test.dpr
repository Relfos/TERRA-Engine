{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Application, TERRA_Scene, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS, TERRA_PNG,
  TERRA_SpriteManager, TERRA_FileManager, TERRA_Math, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Renderer, TERRA_InputManager, 
  TERRA_SoundManager, TERRA_Sound, TERRA_SoundSource, TERRA_WAVE, TERRA_OGG;


Type
  // A client is used to process application events
  Demo = Class(Application)
    Protected
      _Scene:Scene;

			Procedure OnCreate; Override;
			Procedure OnIdle; Override;
  End;

  // A scene is used to render objects
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

Var
  Tex:Texture = Nil;

  GhostPos:Vector3D;

{ Game }
Procedure Demo.OnCreate;
Begin
  // Added Asset folder to search path
  FileManager.Instance.AddPath('assets');

  // Load a Tex
  Tex := TextureManager.Instance['ghost'];

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorBlue;
End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnIdle;
Var
  Sound:SoundSource;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;

  Sound := Nil;

  If InputManager.Instance.Keys.WasPressed(keyEnter) Then
    Sound := SoundManager.Instance.Play('ghost');

  If InputManager.Instance.Keys.WasPressed(keyZ) Then
    Sound := SoundManager.Instance.Play('ghost3');

  If InputManager.Instance.Keys.WasPressed(keyX) Then
    Sound := SoundManager.Instance.Play('attack');

  If InputManager.Instance.Keys.WasPressed(keyC) Then
    Sound := SoundManager.Instance.Play('sfx_beep');

  If Assigned(Sound) Then
  Begin
    Sound.Position := GhostPos;
  End;
End;

{ MyScene }
Procedure MyScene.RenderSprites;
Var
  I:Integer;
  Angle:Single;
  S:QuadSprite;
Begin
  If (Tex = Nil) Then
    Exit;

  GhostPos.X := Abs(Sin(Application.GetTime()/3500));
  GhostPos.Y := 0.5;
  GhostPos.Z := 0;

  S := SpriteManager.Instance.DrawSprite(GhostPos.X * 960,  GhostPos.Y * 640, 50, Tex);
  S.SetScale(2.0);
End;


Begin
  // Start the application
  Demo.Create();
End.
