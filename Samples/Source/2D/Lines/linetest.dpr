{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Utils, TERRA_Application, TERRA_Scene, TERRA_Client, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS, TERRA_PNG, TERRA_Vector2D,
  TERRA_DebugDraw, TERRA_FileManager, TERRA_Math, TERRA_Vector3D, TERRA_Font, TERRA_Tween;

Type
  // A client is used to process application events
  MyGame = Class(AppClient)
    Protected
      _Scene:Scene;

			Procedure OnCreate; Override;
			Procedure OnIdle; Override;
  End;

  // A scene is used to render objects
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

Const
  EaseNames:Array[0..37] Of String = (
	'Linear',
	'InQuad',
	'OutQuad',
	'InOutQuad',
	'OutInQuad',
	'InCubic',
	'OutCubic',
	'InOutCubic',
	'OutInCubic',
	'InQuart',
	'OutQuart',
	'InOutQuart',
	'OutInQuart',
	'InSine',
	'OutSine',
	'InOutSine',
	'OutInSine',
	'InExpo',
	'OutExpo',
	'InOutExpo',
	'OutInExpo',
	'InCirc',
	'OutCirc',
	'InOutCirc',
	'OutInCirc',
	'InElastic',
	'OutElastic',
	'InOutElastic',
	'OutInElastic',
	'InBack',
	'OutBack',
	'InOutBack',
	'OutInBack',
	'InBounce',
	'OutBounce',
	'InOutBounce',
	'OutInBounce',
  'Wiggle');


Var
  EaseType :Integer;

{ Game }
Procedure MyGame.OnCreate;
Begin
  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  GraphicsManager.Instance.BackgroundColor := ColorBlue;
End;

// OnIdle is called once per frame, put your game logic here
Procedure MyGame.OnIdle;
Begin
  If Keys[keyEscape] Then
    Application.Instance.Terminate;

  If Application.Instance.KeyPressed(keyUp) Then
    Dec(EaseType);
  If Application.Instance.KeyPressed(keyDown) Then
    Inc(EaseType);
End;

{ MyScene }
Procedure MyScene.RenderSprites;
Const
  X = 100;
  Y = 500;
  W = 4;
  Amp = 100 * W;
  Ofs = 0.5;
Var
  I:Integer;
  Last, Cur, Delta:Single;
Begin
  If (EaseType<0) Then
    EaseType := 0;
  If (EaseType>37) Then
    EaseType := 37;
    
  FontManager.Instance.DefaultFont.DrawText(10, 10, 10, 'Ease :'+EaseNames[EaseType], ColorWhite);
  Last := -1;
  For I:=0 To 100 Do
  Begin
    Delta := GetEase(I/100, EaseType);
    If Delta<0 Then
      IntToString(I);

    Last := Cur;
    Cur := SmoothCurveWithOffset(Delta, Ofs) * Amp;

    If I>0 Then
      DrawLine2D(VectorCreate2D(X+Pred(I)*W, Y-Last), VectorCreate2D(X+I*W, Y-Cur), ColorGreen, 50);
  End;

  DrawLine2D(VectorCreate2D(X+50*W, 0), VectorCreate2D(X+50*W, GraphicsManager.Instance.Height), ColorRed, 50);
  DrawLine2D(VectorCreate2D(0, Y), VectorCreate2D(GraphicsManager.Instance.Width, Y), ColorYellow, 50);
  DrawLine2D(VectorCreate2D(X+(Ofs*100)*W, 0), VectorCreate2D(X+(Ofs*100)*W, GraphicsManager.Instance.Height), ColorYellow, 50);
End;

Begin
  // Start the application
  ApplicationStart(MyGame.Create);
End.