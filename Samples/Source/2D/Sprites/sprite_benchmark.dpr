{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Application, TERRA_Scene, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS, TERRA_PNG, TERRA_Vector2D,
  TERRA_SpriteManager, TERRA_FileManager, TERRA_Math, TERRA_Vector3D, TERRA_Utils,
  TERRA_InputManager, TERRA_UI;

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
      Procedure RenderSprites(V:TERRAViewport); Override;
  End;

Const
  Limit = 500;

Var
  Tex:TERRATexture = Nil;

  Pos:Array[0..Pred(Limit)]Of Vector3D;
  Dir:Array[0..Pred(Limit)]Of Vector2D;

{ Game }
Procedure Demo.OnCreate;
Var
  I:Integer;
  W,H:Single;
Begin
  // Added Asset folder to search path
  FileManager.Instance.AddPath('assets');

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  // Load a Tex
  Tex := TextureManager.Instance.GetTexture('ghost');

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorBlue;

  W := UIManager.Instance.Width;
  H := UIManager.Instance.Height;

  For I:=0 To Pred(Limit) Do
  Begin
    Pos[I] := VectorCreate(RandomFloat(0, W), RandomFloat(0, H), Trunc(RandomFloat(20, 40)));
    Dir[I] := VectorCreate2D(RandomFloat(-1, 1), RandomFloat(-1, 1));
  End;
End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;
End;

{ MyScene }
Procedure MyScene.RenderSprites;
Var
  I:Integer;
  W,H,Z:Single;
  S:QuadSprite;
Begin
  W := UIManager.Instance.Width;
  H := UIManager.Instance.Height;

  For I:=0 To Pred(Limit) Do
  Begin
    S := SpriteManager.Instance.DrawSprite(Pos[I].X, Pos[I].Y, Pos[I].Z, Tex);
    S.Mirror := Odd(I);    // Each odd sprite in line will be reflected
    //S.SetScaleAndRotation(1, RAD * (I*360 Div 8));

    Pos[I].X := Pos[I].X + Dir[I].X;
    Pos[I].Y := Pos[I].Y + Dir[I].Y;

    If (Pos[I].X>W) Then
    Begin
      Pos[I].X := W;
      Dir[I].X := -Dir[I].X;
    End;

    If (Pos[I].Y>H) Then
    Begin
      Pos[I].Y := H;
      Dir[I].Y := -Dir[I].Y;
    End;

    If (Pos[I].X<0) Then
    Begin
      Pos[I].X := 0;
      Dir[I].X := -Dir[I].X;
    End;

    If (Pos[I].Y<0) Then
    Begin
      Pos[I].Y := 0;
      Dir[I].Y := -Dir[I].Y;
    End;

  End;

  Application.Instance.SetTitle(IntToString(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));
End;

Begin
  // Start the application
  Demo.Create();
End.