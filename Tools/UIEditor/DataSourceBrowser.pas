unit DataSourceBrowser;

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
  Dialogs, StdCtrls, ComCtrls, TERRA_DataSource, TERRA_Object, TERRA_CustomPropertyEditor;

type
  TDataSourceBrowserForm = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    _Cell:TDataSourceCell;
    _Target:DataSourceProperty;

    Procedure AddRecursive(Obj:TERRAObject; Parent:TTreeNode);

  public
    Procedure ShowWithTarget(Cell:TDataSourceCell);
  end;

var
  DataSourceBrowserForm: TDataSourceBrowserForm;

implementation

{$R *.dfm}

procedure TDataSourceBrowserForm.AddRecursive(Obj: TERRAObject; Parent:TTreeNode);
var
  I:Integer;
  Key:TERRAObject;
  Node:TTreeNode;
begin
  If Obj = Nil Then
    Exit;

  Node := Self.TreeView1.Items.AddChild(Parent, Obj.Name);

  I := 0;
  Repeat
    Key := Obj.GetPropertyByIndex(I);
    If Key = Nil Then
      Exit;


    Self.AddRecursive(Key, Node);
    Inc(I);
  Until (False);
end;

procedure TDataSourceBrowserForm.Button1Click(Sender: TObject);
begin
  Self.Close();
end;

procedure TDataSourceBrowserForm.FormShow(Sender: TObject);
Var
  I:Integer;
begin
  Self.TreeView1.Items.Clear;

  For I:=0 To Pred(DataSourceManager.Instance.SourceCount) Do
  Begin
    AddRecursive(DataSourceManager.Instance.GetObject(I), Nil);
  End;

  Edit1.Text := _Target.GetBlob();
end;

procedure TDataSourceBrowserForm.ShowWithTarget(Cell:TDataSourceCell);
begin
  Self._Cell := Cell;
  Self._Target := DataSourceProperty(Cell.Prop);
  Self.ShowModal;
end;

procedure TDataSourceBrowserForm.Button2Click(Sender: TObject);
begin
  _Target.Value := Edit1.Text;
  _Cell.Update();
  Self.Close();
end;

procedure TDataSourceBrowserForm.TreeView1Click(Sender: TObject);
Var
  Path:String;
  Node:TTreeNode;
begin
  If Assigned(Self.TreeView1.Selected) Then
  Begin
    Path := '';
    Node := Self.TreeView1.Selected;
    While Assigned(Node) Do
    Begin
      If Path<>'' Then
        Path := '.' + Path;
      Path := Node.Text + Path;
      Node := Node.Parent;
    End;

    Edit1.Text := Path;
  End;
end;

end.
