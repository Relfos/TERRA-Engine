{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Utils, TERRA_Application, TERRA_Scene, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS, TERRA_PNG, TERRA_Vector2D, TERRA_InputManager,
  TERRA_DebugDraw, TERRA_FileManager, TERRA_Math, TERRA_Vector3D, TERRA_Font, TERRA_Tween, TERRA_UI;

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
Procedure Demo.OnCreate;
Begin
  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorBlue;
End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnIdle;
Begin
  If InputManager.Instance.Keys.WasPressed(keyEscape) Then
    Application.Instance.Terminate;

  If InputManager.Instance.Keys.WasPressed(keyLeft) Then
    Dec(EaseType);

  If InputManager.Instance.Keys.WasPressed(keyRight) Then
    Inc(EaseType);
End;

{ MyScene }
Procedure MyScene.RenderSprites(V:Viewport);
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

  UIManager.Instance.FontRenderer.DrawText(10, 10, 10, 'Ease :'+EaseNames[EaseType]);
  Last := -1;
  For I:=0 To 100 Do
  Begin
    Delta := GetEase(I/100, EaseType);
    If Delta<0 Then
      IntToString(I);

    Last := Cur;
    Cur := SmoothCurveWithOffset(Delta, Ofs) * Amp;

    If I>0 Then
      DrawLine2D(V, VectorCreate2D(X+Pred(I)*W, Y-Last), VectorCreate2D(X+I*W, Y-Cur), ColorGreen);
  End;

  DrawLine2D(V, VectorCreate2D(X+50*W, 0), VectorCreate2D(X+50*W, GraphicsManager.Instance.Height), ColorRed, 2);
  DrawLine2D(V, VectorCreate2D(0, Y), VectorCreate2D(GraphicsManager.Instance.Width, Y), ColorYellow, 2);
  DrawLine2D(V, VectorCreate2D(X+(Ofs*100)*W, 0), VectorCreate2D(X+(Ofs*100)*W, GraphicsManager.Instance.Height), ColorYellow, 2);
End;

Begin
  // Start the application
  Demo.Create();
End.