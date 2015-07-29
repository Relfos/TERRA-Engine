program UIEditor;

uses
  Forms,
  Main in 'Main.pas' {UIEditForm},
  FormProjectSettings in 'FormProjectSettings.pas' {ProjectSettingsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TUIEditForm, UIEditForm);
  Application.CreateForm(TProjectSettingsForm, ProjectSettingsForm);
  Application.Run;
end.
