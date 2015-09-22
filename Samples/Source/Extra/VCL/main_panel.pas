unit main_panel;

interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  StdCtrls, Menus;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
Uses TERRA_Utils, TERRA_Object, TERRA_Application, TERRA_VCLApplication, TERRA_OS, TERRA_Texture,
  TERRA_Viewport, TERRA_FileManager, TERRA_Sprite, TERRA_PNG,
  TERRA_Engine, TERRA_GraphicsManager, TERRA_Math, TERRA_Vector2D, TERRA_Color;

{$R *.dfm}

Type
  MyVCLApp = Class(VCLApplication)
    Public
      Procedure RenderMainViewport(V:TERRAViewport);
      Procedure RenderSideViewport(V:TERRAViewport);
  End;

Var
  _Tex:TERRATexture = Nil;
  _ExtraView:TERRAVCLViewport;

  _MyApp:MyVCLApp;

Procedure MyVCLApp.RenderMainViewport(V: TERRAViewport);
Var
  S:TERRASprite;
  Angle:Single;
Begin
  // A rotating sprite in the bottom, with Scale = 4x
  Angle := RAD * ((Application.GetTime() Div 15) Mod 360);
  S := Engine.FetchSprite();
  S.SetTexture(_Tex);
  S.Layer := 50;
  S.Rotate(Angle);
  S.Scale(3);
  S.Translate(100, 100);

  S.AddQuad(SpriteAnchor_Center, Vector2D_Zero, 0.0, _Tex.Width, _Tex.Height);
  Engine.Graphics.AddRenderable(V, S);

  Inherited;
End;

Procedure MyVCLApp.RenderSideViewport(V: TERRAViewport);
Var
  S:TERRASprite;
  Scale:Single;
Begin
  // A rotating sprite in the bottom, with Scale = 4x
  Scale := 2 * Abs(Cos(RAD * ((Application.GetTime() Div 15) Mod 360))) + 0.25;
  S := Engine.FetchSprite();
  S.SetTexture(_Tex);
  S.Layer := 50;
  S.Scale(Scale);
  S.Translate(100, 100);

  S.AddQuad(SpriteAnchor_Center, Vector2D_Zero, 0.0, _Tex.Width, _Tex.Height);
  Engine.Graphics.AddRenderable(V, S);

  Inherited;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  _MyApp := MyVCLApp.Create(Panel1);
  _MyApp.Viewport.OnRender := _MyApp.RenderMainViewport;
  _MyApp.Viewport.BackgroundColor := ColorRed;

  _ExtraView := TERRAVCLViewport.Create(Image1);
  _MyApp.AddRenderTarget(_ExtraView);

  _ExtraView.Viewport.OnRender := _MyApp.RenderSideViewport;
  _ExtraView.Viewport.BackgroundColor := ColorBlue;

  // Added Asset folder to search path
  Engine.Files.AddFolder('assets');

  // Load a Tex
  _Tex := Engine.Textures['ghost'];

  // set background color
  Engine.Graphics.DeviceViewport.BackgroundColor := ColorGreen;
End;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  Application.Instance.Terminate();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  If OpenDialog1.Execute Then
    _Tex := TERRATexture.LoadFromFile(OpenDialog1.FileName);
end;

end.
