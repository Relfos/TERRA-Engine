program UIEditor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
{$IFDEF UNIX}Cthreads, cmem,{$ENDIF}
 Interfaces,
{$ENDIF}
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
