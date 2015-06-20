program UIEditor;

uses
  Forms,
  Main in 'Main.pas' {UIEditForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TUIEditForm, UIEditForm);
  Application.Run;
end.
