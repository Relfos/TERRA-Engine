{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
  TERRA_Renderer,
  TERRA_Utils,
  TERRA_ResourceManager,
  TERRA_GraphicsManager,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Font,
  TERRA_Texture,
  TERRA_FileManager,
  TERRA_InputManager,
  TERRA_Collections,
  TERRA_Viewport,
  TERRA_EngineManager,
  TERRA_Matrix3x3,
  TERRA_Math,
  TERRA_Vector3D,
  TERRA_Sound,
  TERRA_SoundSource,
  TERRA_Color,
  TERRA_String,
  TERRA_Sprite;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
      Procedure OnRender2D(View:TERRAViewport); Override;

      Procedure OnIdle; Override;
  End;

Var
  Tex:TERRATexture = Nil;
  GhostPos:Vector3D;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  // Enable 2D viewport for rendering
  Self.GUI.Viewport.Visible := True;

  // load and cache a texture called ghost.png (located in the samples/binaries/assets/ folder
  Tex := Engine.Textures['ghost'];
End;

Procedure MyDemo.OnIdle;
Var
  Sound:SoundSource;
Begin
  Inherited;

  Sound := Nil;

  If Engine.Input.Keys.WasPressed(keyEnter) Then
    Sound := Engine.Audio.Play('ghost');

  If Engine.Input.Keys.WasPressed(keyZ) Then
    Sound := Engine.Audio.Play('ghost3');

  If Engine.Input.Keys.WasPressed(keyX) Then
    Sound := Engine.Audio.Play('attack');

  If Engine.Input.Keys.WasPressed(keyC) Then
    Sound := Engine.Audio.Play('sfx_beep');

  If Assigned(Sound) Then
  Begin
    Sound.Position := GhostPos;
  End;
End;

Procedure MyDemo.OnRender2D(View: TERRAViewport);
Var
  I:Integer;
  Angle:Single;
  S:TERRASprite;
Begin
  Inherited;

  GhostPos.X := Abs(Sin(Application.GetTime()/3500));
  GhostPos.Y := 0.5;
  GhostPos.Z := 0;

  If (Tex = Nil) Then
    Exit;

  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 50;
  S.SetTexture(Tex);
  S.Translate(GhostPos.X * 960,  GhostPos.Y * 640);
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
  View.SpriteRenderer.QueueSprite(S);
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}


End.


