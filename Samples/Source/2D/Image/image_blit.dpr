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
  TERRA_Math,
  TERRA_Sprite,
  TERRA_FileManager,
  TERRA_Image,
  TERRA_Color,
  TERRA_Rasterizer,
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
  Src, Dest:TERRAImage;
  It:ImageIterator;

  U, V:Single;
  R:Integer;
Begin
  Inherited;

  Self.GUI.Viewport.Visible := True;

  // load a image from disk
  Src := TERRAImage.Create('ghost.png');

  // create a image at run-time
  Dest := TERRAImage.Create(256, 256);

  // fill with red color
  It := Dest.RectangleByUV(0, 0, 1, 1, [image_Write, image_Fill]);
  While It.HasNext() Do
  Begin
    It.DestColor := ColorRed;
  End;
  ReleaseObject(It);

  // blit the src image into random positions
  For I:=1 To 20 Do
  Begin
    U := RandomFloat(0, 1);
    V := RandomFloat(0, 1);

    //Dest.BlitWithAlphaByUV(U, V, 0, 0, 1, 1, Src);

    It := Dest.BlithWithRotationAndScale(128, 128, Src.Width, Src.Height, 45*RAD, 1);
    It.BlendMode := combineBlend;
    While It.HasNext() Do
    Begin
      U := TERRARasterizer(It).GetRasterValue(2);
      V := TERRARasterizer(It).GetRasterValue(3);

      It.DestColor := Src.GetPixelByUV(U, V);
    End;
    ReleaseObject(It);
    Break;

    (*It := Dest.CircleByUV(U, V, R, [image_Write]);
    While It.HasNext() Do
    Begin
      It.Value := ColorBlack;
    End;
    ReleaseObject(It);*)
  End;

  // Create a texture from a image
  Tex := TERRATexture.Create(rtDynamic);
  Tex.InitFromImage(Dest);

  ReleaseObject(Dest);
  ReleaseObject(Src);
End;

Procedure Demo.OnRender2D(V:TERRAViewport);
Var
  S:TERRASprite;
Begin
  S := Engine.FetchSprite();
  S.SetTexture(Tex);
  S.Layer := 50;
  S.Translate(300, 200);
  S.AddQuad(SpriteAnchor_Center, Vector2D_Zero, 0, Tex.Width, Tex.Height, 0, Cos(Application.GetTime()/1000)*180  *RAD);
  Engine.Graphics.AddRenderable(V, S);

  Inherited;
End;

Begin
  // Start the application
  Demo.Create();
End.
