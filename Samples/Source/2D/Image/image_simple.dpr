{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

uses
  TERRA_Application,
  TERRA_GraphicsManager,
  TERRA_Viewport,
  TERRA_DemoApplication,
  TERRA_ResourceManager,
  TERRA_Texture,
  TERRA_Utils,
  TERRA_Object,
  TERRA_OS,
  TERRA_Engine,
  TERRA_PNG,
  TERRA_Sprite,
  TERRA_FileManager,
  TERRA_Math,
  TERRA_Image,
  TERRA_Color,
  TERRA_Resource,
  TERRA_Vector3D,
  TERRA_Vector2D,
  TERRA_Renderer,
  TERRA_InputManager;

Type
  // A client is used to process application events
  Demo = Class(DemoApplication)
    Protected
			Procedure OnCreate; Override;
			Procedure OnRender2D(V:TERRAViewport); Override;
  End;


Var
  Tex:TERRATexture = Nil;

{ Game }
Procedure Demo.OnCreate;
Var
  I:Integer;
  Img:TERRAImage;
  It:ImageIterator;

  U, V:Single;
  R:Integer;
Begin
  Inherited;

  Self.GUI.Viewport.Visible := True;

  // create a image at run-time
  Img := TERRAImage.Create(256, 256);

  // fill with red color
  It := Img.RectangleByUV(0, 0, 1, 1, [image_Write, image_Fill]);
  While It.HasNext() Do
  Begin
    It.Value := ColorRed;
  End;
  ReleaseObject(It);

  // put some random circles into it
  For I:=1 To 20 Do
  Begin
    U := RandomFloat(0, 1);
    V := RandomFloat(0, 1);
    R := 50;

    It := Img.CircleByUV(U, V, R, [image_Write, image_Fill]);
    While It.HasNext() Do
    Begin
      It.Value := ColorWhite;
    End;
    ReleaseObject(It);

    It := Img.CircleByUV(U, V, R, [image_Write]);
    While It.HasNext() Do
    Begin
      It.Value := ColorBlack;
    End;
    ReleaseObject(It);

  End;

  // Create a texture from a image
  Tex := TERRATexture.Create(rtDynamic);
  Tex.InitFromImage(Img);

  ReleaseObject(Img);
End;

Procedure Demo.OnRender2D(V:TERRAViewport);
Var
  S:TERRASprite;
Begin
  S := Engine.FetchSprite();
  S.SetTexture(Tex);
  S.Layer := 50;
  S.Translate(20, 20);
  S.AddQuad(SpriteAnchor_TopLeft, Vector2D_Zero, 0, Tex.Width, Tex.Height);
  Engine.Graphics.AddRenderable(V, S);

  Inherited;
End;

Begin
  // Start the application
  Demo.Create();
End.
