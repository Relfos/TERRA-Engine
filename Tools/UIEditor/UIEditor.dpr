program UIEditor;

uses
  Forms,
  Main in 'Main.pas' {UIEditForm},
  FormProjectSettings in 'FormProjectSettings.pas' {ProjectSettingsForm},
  FormListEdit in 'FormListEdit.pas' {ListEditForm},
  DataSourceBrowser in 'DataSourceBrowser.pas' {DataSourceBrowserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TUIEditForm, UIEditForm);
  Application.CreateForm(TProjectSettingsForm, ProjectSettingsForm);
  Application.CreateForm(TListEditForm, ListEditForm);
  Application.CreateForm(TDataSourceBrowserForm, DataSourceBrowserForm);
  Application.Run;
end.
