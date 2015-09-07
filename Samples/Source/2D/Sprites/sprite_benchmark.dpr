{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
  TERRA_Utils,
  TERRA_Engine,
  TERRA_GraphicsManager,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Vector3D,
  TERRA_Font,
  TERRA_Texture,
  TERRA_FileManager,
  TERRA_InputManager,
  TERRA_Collections,
  TERRA_Viewport,
  TERRA_UIView,
  TERRA_PNG,
  TERRA_Math,
  TERRA_Color,
  TERRA_Profiler,
  TERRA_String,
  TERRA_Sprite;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
      Procedure OnRender2D(View:TERRAViewport); Override;
  End;

Const
  Limit = 500;

Var
  Tex:TERRATexture = Nil;

  Pos:Array[0..Pred(Limit)]Of Vector3D;
  Dir:Array[0..Pred(Limit)]Of Vector2D;

{ Game }
Procedure MyDemo.OnCreate;
Var
  I:Integer;
  W,H:Single;
Begin
  Inherited;

  // Load a Tex
  Tex := Engine.Textures['ghost'];

  W := Self.GUI.Viewport.Width;
  H := Self.GUI.Viewport.Height;

  For I:=0 To Pred(Limit) Do
  Begin
    Pos[I] := Vector3D_Create(RandomFloat(0, W), RandomFloat(0, H), Trunc(RandomFloat(20, 40)));
    Dir[I] := Vector2D_Create(RandomFloat(-1, 1), RandomFloat(-1, 1));
  End;
End;

Procedure MyDemo.OnRender2D(View: TERRAViewport);
Var
  I:Integer;
  W,H,Z:Single;
  S:TERRASprite;
Begin
  W := Self.GUI.Viewport.Width;
  H := Self.GUI.Viewport.Height;

  For I:=0 To Pred(Limit) Do
  Begin
    S := View.SpriteRenderer.FetchSprite();
    S.Layer := Pos[I].Z;
    S.SetTexture(Tex);
    S.Translate(Pos[I].X, Pos[I].Y);
    S.Mirror := Odd(I);    // Each odd sprite in line will be reflected
    S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
    View.SpriteRenderer.QueueSprite(S);

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

  //Application.Instance.SetTitle(IntToString(GraphicsManager.Instance.Renderer.Stats.FramesPerSecond));

  Inherited;
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


