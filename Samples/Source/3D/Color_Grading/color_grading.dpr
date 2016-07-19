{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses TERRA_Application, TERRA_DemoApplication, TERRA_Utils, TERRA_GraphicsManager, TERRA_Viewport,
  TERRA_ResourceManager, TERRA_Color, TERRA_Texture, TERRA_OS,
  TERRA_Sprite, TERRA_FileManager, TERRA_Math, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Renderer, TERRA_Engine;

Type
  // A client is used to process application events
  Demo = Class(DemoApplication)
    Protected
			Procedure OnCreate; Override;

      Procedure OnRender2D(V:TERRAViewport); Override;
			Procedure OnMouseMove(Const X,Y:Single); Override;
  End;

Var
  Tex:TERRATexture = Nil;
  GradRamp:TERRATexture = Nil;
  CurrentGrad:TERRATexture;

  Percent:Single;

{ Game }
Procedure Demo.OnCreate;
Begin
  Inherited;

  Self.GUI.Viewport.Visible := True;

  // Load a Tex
  Tex := Engine.Textures['forest'];
  If Assigned(Tex) Then
  Begin
    Tex.PreserveQuality := True;
    Tex.Uncompressed := True;
    Tex.Filter := filterBilinear;
    Tex.WrapMode := wrapNothing;
  End;

  //GradRamp := TextureManager.Instance.GetTexture('negative');
  GradRamp := Engine.Textures['sepia'];
  //GradRamp := TextureManager.Instance.GetTexture('monochrome');

  CurrentGrad := GradRamp;

  //CurrentGrad := TextureManager.Instance.DefaultColorTable;

  Percent := 0.5;
End;

Procedure Demo.OnRender2D(V:TERRAViewport);
Var
  I:Integer;
  S:TERRASprite;
Begin
  Inherited;

  If Not Assigned(Tex) Then
    Exit;

  S := Engine.FetchSprite();
  S.Layer := 50;
  S.SetTexture(Tex);
  S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0) , 0.0, V.Width, V.Height);
  Engine.Graphics.AddRenderable(V, S);

  If (Percent>0) Then
  Begin
    S := Engine.FetchSprite();
    S.Layer := 55;
    S.SetTexture(Tex);
    S.ColorTable := CurrentGrad;
    S.SetUVs(0.0, 0.0, Percent, 1.0);
    S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0) , 0.0, V.Width * Percent, V.Height);
    Engine.Graphics.AddRenderable(V, S);
  End;
End;

// Called every time the mouse moves
Procedure Demo.OnMouseMove(Const X, Y: Single);
Begin
  Percent := X;
End;

Begin
  // Start the application
  Demo.Create();
End.
