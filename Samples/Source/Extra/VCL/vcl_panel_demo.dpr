program vcl_panel_demo;

uses
  Forms,
  main_panel in 'main_panel.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
