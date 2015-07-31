unit FormListEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TERRA_Object, TERRA_Collections, TERRA_CollectionObjects, TERRA_FileUtils;

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
    _Target:List;
    _FolderOnly:Boolean;

  public
    { Public declarations }

    Procedure ShowWithTarget(Target:List; FolderOnly:Boolean);
  end;

var
  ListEditForm: TListEditForm;

implementation

{$R *.dfm}

{ TListEditForm }

procedure TListEditForm.ShowWithTarget(Target:List; FolderOnly:Boolean);
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

    _Target.Add(StringObject.Create(S));
    ListBox1.Items.Add(S);
  End;
end;

procedure TListEditForm.Button3Click(Sender: TObject);
begin
  Self.Close;
end;

end.
