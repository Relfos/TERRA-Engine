unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, IceTabSet, Grids, ValEdit, ComCtrls;

type
  TForm1 = class(TForm)
    IceTabSet1: TIceTabSet;
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    Project1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Project2: TMenuItem;
    N3: TMenuItem;
    View1: TMenuItem;
    Component1: TMenuItem;
    View2: TMenuItem;
    Add1: TMenuItem;
    ValueListEditor1: TValueListEditor;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public


    Procedure AddNewTab();
  end;

var
  Form1: TForm1;

implementation
Uses TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_VCLApplication, TERRA_OS, TERRA_Scene, TERRA_Texture,
  TERRA_Viewport, TERRA_FileManager, TERRA_SpriteManager, TERRA_PNG,
  TERRA_GraphicsManager, TERRA_Math, TERRA_Vector2D, TERRA_Color;

{$R *.dfm}

Type
  MyScene = Class(Scene)
    Procedure RenderSprites(V:Viewport); Override;
  End;

Var
  _Tex:Texture = Nil;
  _Scene:MyScene;

{ MyScene }
Procedure MyScene.RenderSprites(V: Viewport);
Var
  S:QuadSprite;
  Angle:Single;
Begin
  // A rotating sprite in the bottom, with Scale = 4x
  Angle := RAD * ((Application.GetTime() Div 15) Mod 360);
  S := SpriteManager.Instance.DrawSprite(100, 100, 50, _Tex);
  S.SetScaleAndRotationRelative(VectorCreate2D(0.5, 0.5), 4.0, Angle);  // Calculate rotation, in degrees, from current time
End;

procedure TForm1.AddNewTab;
begin
  IceTabSet1.AddTab('Untitled');
end;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  VCLApplication.Create(Panel1);

  // Added Asset folder to search path
  FileManager.Instance.AddPath('..\..\samples\binaries\assets');

  // Load a Tex
  _Tex := TextureManager.Instance['ghost'];

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  // set background color
  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorGreen;

  Self.AddNewTab();
End;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  ReleaseObject(_Scene);
  Application.Instance.Terminate();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ValueListEditor1.Top := TreeView1.Height + TreeView1.Top;
  ValueListEditor1.Height := Self.Height - ValueListEditor1.Top;
  Panel1.Left := ValueListEditor1.Width + ValueListEditor1.Left;
  Panel1.Width := Self.Width - Panel1.Left;
  Panel1.Height := Self.Height - Panel1.Top;
end;

end.
