{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

uses
  TERRA_Application,
  TERRA_Scene,
  TERRA_GraphicsManager,
  TERRA_Viewport,
  TERRA_DemoApplication,
  TERRA_ResourceManager,
  TERRA_Texture,
  TERRA_Utils,
  TERRA_Object,
  TERRA_OS,
  TERRA_PNG,
  TERRA_Sprite,
  TERRA_FileManager,
  TERRA_Math,
  TERRA_Image,
  TERRA_ImageDrawing,
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
			Procedure OnRender(V:TERRAViewport); Override;
  End;


Var
  Tex:TERRATexture = Nil;

{ Game }
Procedure Demo.OnCreate;
Var
  I:Integer;
  Img:Image;
  It:ImageIterator;

  C:ColorRGBA;
Begin
  Inherited;

  FileManager.Instance.AddPath('assets');

  // create a image at run-time
  Img := Image.Create('dilate_test.png');
  //Img := Image.Create('sample_test.png');

  It := Img.Pixels([image_Kernel, image_Read, image_Write], maskRGB);
  While It.HasNext() Do
  Begin
    C := It.ApplyKernel(Kernel_EdgeDetect);
    It.Value := C;
  End;
  ReleaseObject(It);

  // Create a texture from a image
  Tex := TERRATexture.Create(rtDynamic, '');
  Tex.InitFromImage(Img);

  ReleaseObject(Img);
End;

Procedure Demo.OnRender(V:TERRAViewport);
Var
  S:QuadSprite;
Begin
  S := V.SpriteRenderer.DrawSprite(20, 20, 50, Tex);
End;

Begin
  // Start the application
  Demo.Create();
End.


