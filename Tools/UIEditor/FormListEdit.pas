unit FormListEdit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TERRA_Object, TERRA_Collections, TERRA_List, TERRA_FileUtils;

type
  TListEditForm = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    _Target:TERRAList;
    _FolderOnly:Boolean;

  public
    { Public declarations }

    Procedure ShowWithTarget(Target:TERRAList; FolderOnly:Boolean);
  end;

var
  ListEditForm: TListEditForm;

Procedure LoadDataSources(Target:TERRAList);

implementation
Uses TERRA_DataSource, TERRA_FileManager;

{$R *.dfm}

Procedure LoadDataSources(Target:TERRAList);
Var
  It:Iterator;
  S:StringProperty;
Begin
  DataSourceManager.Instance.Clear;
  If Target = Nil Then
    Exit;

  It := Target.GetIterator();
  While It.HasNext() Do
  Begin
    S := StringProperty(It.Value);
    DataSourceManager.Instance.AddFromSession(S.Value);
  End;
  ReleaseObject(It);
End;

{ TListEditForm }
procedure TListEditForm.ShowWithTarget(Target:TERRAList; FolderOnly:Boolean);
Var
  I:Integer;
  Key:TERRAObject;
begin
  _Target := Target;
  _FolderOnly := FolderOnly;
  Self.ShowModal();

  Self.ListBox1.Clear;

  I := 0;
  Repeat
    Key := Target.GetPropertyByIndex(I);
    If Key = Nil Then
      Exit;

    Self.ListBox1.Items.Add(Key.GetBlob());

    Inc(I);
  Until False;
End;

procedure TListEditForm.Button1Click(Sender: TObject);
Var
  S:String;
begin
  If OpenDialog1.Execute Then
  Begin
    S := OpenDialog1.FileName;
    If Self._FolderOnly Then
      S := GetFilePath(S);

    If Self._FolderOnly Then
      _Target.Add(PathProperty.Create('path', S))
    Else
      _Target.Add(StringProperty.Create('string', S));
    ListBox1.Items.Add(S);
  End;
end;

procedure TListEditForm.Button3Click(Sender: TObject);
begin
  If Self._Target.Name='datasources' Then
    LoadDataSources(_Target);

  Self.Close;
end;

end.
