program UIEditor;

uses
  Forms,
  Main in 'Main.pas' {UIEditForm},
  FormProjectSettings in 'FormProjectSettings.pas' {ProjectSettingsForm},
  FormListEdit in 'FormListEdit.pas' {ListEditForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TUIEditForm, UIEditForm);
  Application.CreateForm(TProjectSettingsForm, ProjectSettingsForm);
  Application.CreateForm(TListEditForm, ListEditForm);
  Application.Run;
end.
