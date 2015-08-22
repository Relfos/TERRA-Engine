{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  MemCheck,
  TERRA_Object,
  TERRA_MemoryManager,
  TERRA_Application,
  TERRA_DemoApplication,
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
  TERRA_PNG,
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

  // load and cache a texture called ghost.png (located in the samples/binaries/assets/ folder
  Tex := Engine.Textures['ghost'];
End;

Procedure MyDemo.OnRender2D(View: TERRAViewport);
Var
  I:Integer;
  Angle:Single;
  S:QuadSprite;
Begin
  Inherited;

    If (Tex = Nil) Then
    Exit;

  // This is how sprite rendering works with TERRA.
  // 1st we ask the Renderer to create a new sprite, using a Tex and position.
  // Note that this sprite instance is only valid during the frame its created.
  // If needed we can configure the sprite properties.

  // Note - The third argument of VectorCreate is the sprite Layer, should be a value between 0 and 100
  //        Sprites with higher layer values appear below the others

  // Create a simple fliped sprite
  S := View.SpriteRenderer.DrawSprite(620, 60, 50, Tex);
  S.Flip := True;


  // An alpha blended sprite
  S := View.SpriteRenderer.DrawSprite(700, 60, 55, Tex);
  S.SetColor(ColorCreate(255, 255, 255, 128));

  // Create a line of sprites
  For I:=0 To 8 Do
  Begin
    S := View.SpriteRenderer.DrawSprite(16 + Tex.Width * I, 10, 50, Tex);
    S.Mirror := Odd(I);    // Each odd sprite in line will be reflected
  End;

  // Create a line of rotated sprites
  For I:=0 To 8 Do
  Begin
    S := View.SpriteRenderer.DrawSprite(16 + Tex.Width * I, 300, 50, Tex);
    S.SetScaleAndRotationRelative(VectorCreate2D(0.5, 0.5), 1, RAD * (I*360 Div 8));
  End;

  // Some scaled sprites
  S := View.SpriteRenderer.DrawSprite(10,120,55, Tex);
  S.SetScale(2.0);    // Double size

  S := View.SpriteRenderer.DrawSprite(110,130,55, Tex);
  S.SetScale(1.5);    // 1.5 Size

  S := View.SpriteRenderer.DrawSprite(180,145,55, Tex);
  S.SetScale(0.5);    // Half size

  // Some colored sprites
  For I:=0 To 4 Do
  Begin
    S := View.SpriteRenderer.DrawSprite(300 + Tex.Width * I,120,50, Tex);

    Case I Of
    0:  S.SetColor(ColorCreate(255,128,255)); // Purple tint
    1:  S.SetColor(ColorCreate(255,128,128)); // Red tint
    2:  S.SetColor(ColorCreate(128,255,128)); // Green tint
    3:  S.SetColor(ColorCreate(128,128,255)); // Blue tint
    4:  S.SetColor(ColorCreate(255,255,128)); // Yellow tint
    End;
  End;

  // A rotating sprite in the bottom, with Scale = 2x
  Angle := RAD * ((Application.GetTime() Div 15) Mod 360);
  S := View.SpriteRenderer.DrawSprite(300, 400, 50, Tex);
  S.SetScaleAndRotationRelative(VectorCreate2D(0.5, 0.5), 2.0, Angle);  // Calculate rotation, in degrees, from current time
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




