{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
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
  TERRA_Engine,
  TERRA_Matrix3x3,
  TERRA_Math,
  TERRA_Color,
  TERRA_String,
  TERRA_Sprite;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
      Procedure OnRender2D(View:TERRAViewport); Override;
  End;

Var
  Tex:TERRATExture;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  // Enable 2D viewport for rendering
  Self.GUI.Viewport.Visible := True;

  Tex := Engine.Textures['linepat2'];
//  Tex := Engine.Textures.WhiteTexture;
End;                  

Procedure MyDemo.OnRender2D(View: TERRAViewport);
Var
  I:Integer;
  Angle:Single;
  S:TERRASprite;
  Pos:Array[0..3] Of Vector2D;
Begin
  Inherited;

  If (Tex = Nil) Then
    Exit;

  Pos[0] := Vector2D_Create(0, 0);
  Pos[1] := Vector2D_Create(200, 200);
  Pos[2] := Vector2D_Create(300, Engine.Input.Mouse.Y * View.Height);
  Pos[3] := Vector2D_Create(Engine.Input.Mouse.X * View.Width, Engine.Input.Mouse.Y * View.Height);

  S := Engine.FetchSprite();
  S.Layer := 50;
  S.SetTexture(Tex);
  S.SetLineColor(ColorRed, ColorBlue);
  S.AddArrow(Vector2D_Create(200, 200), Vector2D_Create(Engine.Input.Mouse.X * View.Width, Engine.Input.Mouse.Y * View.Height), 0.0, 14, 40);
  //S.AddPath(Pos, 0.0, 8);
  Engine.Graphics.AddRenderable(View, S);
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




