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
  TERRA_String,
  TERRA_Image,
  TERRA_Color,
  TERRA_LineDrawing,
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
  Img:TERRAImage;
  S:TERRAString;
  Count:Integer;
  I,W,H:Integer;
  LX,LY:Integer;
  HX, HY:Integer;
  PX, PY:Integer;
  ID:Cardinal;
  Scale:Integer;
  CA, CB:TERRAChar;
  Moving:Boolean;
  X1, Y1, X2, Y2:Integer;

  Function Hershey(C:TERRAChar):Integer;
  Begin
    Result := Ord(C) - Ord('R');
  End;
Begin
  Inherited;

  Self.GUI.Viewport.Visible := True;


  // create a image at run-time
  W := 50;
  H := 50;
  Img := TERRAImage.Create(W*2, H*2);

  S := '    8  9MWOMOV RUMUV ROQUQ';
  //S := '   34 20MXRMPNOPOSPURVSVUUVSVPUNSMRM RQQTR RTQQR';
//  S := '2 16MWOMOV ROMSMUNUPSQ ROQSQURUUSVOV';
//  S := '2 32K[QOOPNQMSMUNWPXQXSWUUWRXO RQOOQNSNUOWPX RQOSOUPWWXX RSOTPVWXXYX';
S := '607 23I\XMX]W`VaTbQbOa RXPVNTMQMONMPLSLUMXOZQ[T[VZXX';


  Scale := 4;

  ID := StringToCardinal(StringGetNextSplit(S, ' '));
  CA := StringGetChar(S, 2);
  If (CA>='0') And (CA<='9') Then
    I := 2
  Else
    I := 1;

  Count := StringToInt(StringCopy(S, 1, I));
  S := StringCopy(S, I+1, MaxInt);

  HX := Hershey(StringGetChar(S, 1));
  HY := Hershey(StringGetChar(S, 2));
  S := StringCopy(S, 3, MaxInt);

  Moving := True;

  Dec(Count);
  While Count>0 Do
  Begin
    Dec(Count);
    CA := StringGetChar(S, 1);
    CB := StringGetChar(S, 2);
    S := StringCopy(S, 3, MaxInt);

    If (CA=' ') And (CB='R') Then
    Begin
      Moving := True;
      Continue;
    End;

    If (Moving) Then
    Begin
      Moving := False;
      LX := Hershey(CA);
      LY := Hershey(CB);
      Continue;
    End;

    PX := Hershey(CA);
    PY := Hershey(CB);

    X1 := Scale * LX+W;
    Y1 := Scale * LY+H;
    X2 := Scale * PX+W;
    Y2 := Scale * PY+H;

    WuLine(Img, x1, y1, x2, y2);
    (*It := Img.Line(X1, Y1, X2, Y2, [image_write]);
    While It.HasNext() Do
    Begin
      It.Value := ColorWhite;
    End;
    ReleaseObject(It);*)

    LX := PX;
    LY := PY;
  End;

  Img.SetPixel(Scale * HX+W, Scale * HY+H, ColorRed);


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



