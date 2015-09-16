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
Uses TERRA_Utils, TERRA_Object, TERRA_Application, TERRA_VCLApplication, TERRA_OS, TERRA_Scene, TERRA_Texture,
  TERRA_Viewport, TERRA_FileManager, TERRA_Sprite, TERRA_PNG,
  TERRA_Engine, TERRA_GraphicsManager, TERRA_Math, TERRA_Vector2D, TERRA_Color;

{$R *.dfm}

Type
  MyScene = Class(TERRAScene)
    Procedure RenderViewport(V:TERRAViewport); Override;
  End;

Var
  _Tex:TERRATexture = Nil;
  _Scene:MyScene;
  _ExtraView:TERRAVCLViewport;

  _MyApp:VCLApplication;

{ MyScene }
Procedure MyScene.RenderViewport(V: TERRAViewport);
Var
  S:QuadSprite;
  Angle:Single;
Begin
  If (V<>_ExtraView.Viewport) Then
    Exit;

  // A rotating sprite in the bottom, with Scale = 4x
  Angle := RAD * ((Application.GetTime() Div 15) Mod 360);
  S := V.SpriteRenderer.DrawSprite(100, 100, 50, _Tex);
  S.SetScaleAndRotationRelative(VectorCreate2D(0.5, 0.5), 4.0, Angle);  // Calculate rotation, in degrees, from current time
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  _MyApp := VCLApplication.Create(Panel1);

  // Added Asset folder to search path
  FileManager.Instance.AddPath('assets');

  // Load a Tex
  _Tex := Engine.Textures['ghost'];

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  // set background color
  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorGreen;

  _ExtraView := TERRAVCLViewport.Create(Image1);
  _MyApp.AddRenderTarget(_ExtraView);
End;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  ReleaseObject(_Scene);
  Application.Instance.Terminate();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  If OpenDialog1.Execute Then
    _Tex := TERRATexture.LoadFromFile(OpenDialog1.FileName);
end;

end.
