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
  TERRA_EngineManager,
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

  // load and cache a texture called ghost.png (located in the samples/binaries/assets/ folder
  Tex := Engine.Textures['ghost'];
End;

Procedure MyDemo.OnRender2D(View: TERRAViewport);
Var
  I:Integer;
  Angle:Single;
  S:TERRASprite;
Begin
  Inherited;

  If (Tex = Nil) Then
    Exit;

  Tex.WrapMode := wrapNothing;

  // This is how sprite rendering works with TERRA.
  // 1st we ask the Renderer to create a new sprite, using a Tex and position.
  // Note that this sprite instance is only valid during the frame its created.
  // If needed we can configure the sprite properties.

  // Note - The first argument of VectorCreate is the sprite Layer, should be a value between 0 and 100
  //        Sprites with lower layer values appear below the others

  // Create a simple fliped sprite
  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 50;
  S.SetTexture(Tex);
  S.Translate(620, 60);
  S.Flip := True;
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
  View.SpriteRenderer.QueueSprite(S);

  // An alpha blended sprite
  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 55;
  S.SetTexture(Tex);
  S.Translate(700, 60);
  S.SetColor(ColorCreate(255, 255, 255, 128));
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
  View.SpriteRenderer.QueueSprite(S);

  // Create a line of sprites
  For I:=0 To 8 Do
  Begin
    S := View.SpriteRenderer.FetchSprite();
    S.Layer := 50;
    S.SetTexture(Tex);
    S.Translate(16 + Tex.Width * I, 10);
    S.Mirror := Odd(I);    // Each odd sprite in line will be reflected

    S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
    View.SpriteRenderer.QueueSprite(S);
  End;

  // Create a line of rotated sprites
  For I:=0 To 8 Do
  Begin
    S := View.SpriteRenderer.FetchSprite();
    S.Layer := 50;
    S.SetTexture(Tex);
    // the order of transformations matter, apply rotation first, then scale, tehn translation always as the last step
    S.Rotate(RAD * (I*360 Div 8));
    S.Scale(1.5);
    // after we rotate and scale, now we can finally apply a translation
    S.Translate(64 + Tex.Width * I * 1.5, 300);

    // notice how spriteAnchor_Center is used, to make sure the rotation is applyed to the center 
    S.AddQuad(spriteAnchor_Center, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
    View.SpriteRenderer.QueueSprite(S);
  End;

  // Some scaled sprites
  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 55;
  S.SetTexture(Tex);
  S.Scale(2.0);    // Double size
  S.Translate(10,120);
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
  View.SpriteRenderer.QueueSprite(S);

  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 55;
  S.SetTexture(Tex);
  S.Scale(1.5);    // 1.5 Size
  S.Translate(110,130);
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
  View.SpriteRenderer.QueueSprite(S);

  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 55;
  S.SetTexture(Tex);
  S.Scale(0.5);    // Half size
  S.Translate(180,145);
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
  View.SpriteRenderer.QueueSprite(S);

  // Some colored sprites
  For I:=0 To 4 Do
  Begin
    S := View.SpriteRenderer.FetchSprite();
    S.Layer := 50;
    S.SetTexture(Tex);
    S.Translate(300 + Tex.Width * I, 120);

    Case I Of
    0:  S.SetColor(ColorCreate(255,128,255)); // Purple tint
    1:  S.SetColor(ColorCreate(255,128,128)); // Red tint
    2:  S.SetColor(ColorCreate(128,255,128)); // Green tint
    3:  S.SetColor(ColorCreate(128,128,255)); // Blue tint
    4:  S.SetColor(ColorCreate(255,255,128)); // Yellow tint
    End;

    S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
    View.SpriteRenderer.QueueSprite(S);
  End;

  // A rotating sprite in the bottom, with Scale = 2x
  Angle := RAD * ((Application.GetTime() Div 15) Mod 360);
  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 50;
  S.SetTexture(Tex);
  S.Rotate(Angle);
  S.Scale(2);
  S.Translate(300, 400);
  S.AddQuad(spriteAnchor_Center, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);
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




