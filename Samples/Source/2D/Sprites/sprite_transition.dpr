{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  TERRA_Object,
  TERRA_DemoApplication,
  TERRA_GraphicsManager,
  TERRA_OS,
  TERRA_Vector2D,
  TERRA_Texture,
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
  Tex, MaskTex:TERRATExture;

{ Game }
Procedure MyDemo.OnCreate;
Begin
  Inherited;

  // Enable 2D viewport for rendering
  Self.GUI.Viewport.Visible := True;

  Tex := Engine.Textures['cobble'];
  MaskTex := Engine.Textures['radial_texture'];
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

  S := View.SpriteRenderer.FetchSprite();
  S.Layer := 50;
  S.SetTexture(Tex);
  S.SetDissolve(MaskTex, Abs(Cos(Application.GetTime / 1000)));
  S.Translate(200, 200);
//  S.AddQuad(spriteAnchor_TopLeft, VectorCreate2D(0, 0), 0.0, Tex.Width, Tex.Height);
  S.AddEllipse(spriteAnchor_TopLeft, VectorCreate2D(0, 0), 0.0, Tex.Width, Tex.Height);
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




