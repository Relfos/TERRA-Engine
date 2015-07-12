unit TERRA_CustomPropertyEditor;

interface

uses SysUtils, Classes, Messages, ExtCtrls, Controls, StdCtrls,
  Dialogs, Graphics, Buttons,
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_OS, TERRA_Color, TERRA_VCLApplication;

Const
  MarginTop = 10;
  MarginSide = 10;
  ExpandSize = 15;
  CellHeight = 25;
  MarginColor = TColor($DDDDDD);


type
  TCustomPropertyEditor = Class;

  TPropertyCell = Class(TERRAObject)
    Protected
      _Owner:TCustomPropertyEditor;
      _Index:Integer;
      _Parent:TERRAObject;
      _Prop:TERRAObject;
      _Visible:Boolean;

      _Label:TLabel;
      _Editor:TControl;
      _Expand:TSpeedButton;

      Function CreateEditor():TControl; Virtual; Abstract;
      Procedure Update(); Virtual; Abstract;

      Procedure Resize();

      Procedure ExpandProps(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X, Y: Integer);

    Public
      Constructor Create(Owner:TCustomPropertyEditor; Parent, Prop:TERRAObject);
      Procedure Release(); Override;

      Procedure SetVisible(Value:Boolean);
  End;

  TPropertyCellType = Class Of TPropertyCell;

  TTextCell = Class(TPropertyCell)
    Protected
      _Edit:TEdit;

      Function CreateEditor():TControl; Override;
      Procedure Update(); Override;

      Procedure OnKeyDown(Sender:TObject; var Key: Word; Shift: TShiftState);
      Procedure OnChange(Sender:TObject);
  End;

  TColorCell = Class(TPropertyCell)
    Protected
      _Shape:TShape;
      _Dialog:TColorDialog;

      Function CreateEditor():TControl; Override;
      Procedure Update(); Override;

      Procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X, Y: Integer);
  End;

  TBooleanCell = Class(TPropertyCell)
    Protected
      _Check:TCheckbox;

      Function CreateEditor():TControl; Override;
      Procedure Update(); Override;

      Procedure OnClick(Sender: TObject);
  End;

  TCustomPropertyEditor = class(TPanel)
    private
      _Bevel:TBevel;

      _Target: TERRAObject;
      _Cells:Array Of TPropertyCell;
      _CellCount:Integer;

      procedure SetTarget(Target: TERRAObject);

    protected

      Procedure InsertRow(Parent, Prop:TERRAObject);

      Procedure Clear();

      Function GetMiddle():Integer;

      Function FindCell(Prop:TERRAObject):TPropertyCell;


      procedure Paint; override;
      procedure Resize; override;

    public
      constructor Create(AOwner: TComponent);

      Procedure AddPropertiesFromObject(Parent, Source:TERRAObject);

      Procedure RequestUpdate();


      Property Target:TERRAObject Read _Target Write SetTarget;

    published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter Default bvLowered;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
//    property OnValidate: TOnValidateEvent read FOnValidate write FOnValidate;
  end;


procedure Register;

implementation
uses Forms, TypInfo, Variants, Consts;


procedure Register;
begin
  RegisterComponents('Samples', [TCustomPropertyEditor]);
end;

{ TCustomPropertyEditor }
constructor TCustomPropertyEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 306;
  Height := 300;

  _Bevel := TBevel.Create(Self);
  _Bevel.Parent := Self;
  _Bevel.Width := 20;
  _Bevel.Height := Height;
  _Bevel.Top := 10;
  _Bevel.Left := Width Div 2;
 // _Bevel.Shape := bsLeftLine;
end;

Procedure TCustomPropertyEditor.AddPropertiesFromObject(Parent, Source:TERRAObject);
Var
  Index, Row:Integer;
  Prop:TERRAObject;
  S:TERRAString;
Begin
  Index := 0;
  Repeat
    Prop := Source.GetPropertyByIndex(Index);
    If Prop = Nil Then
      Break;

    S := Prop.GetBlob();
    If S<>'' Then
    Begin
      Self.InsertRow(Parent, Prop);
    End;

    If Not Prop.IsValueObject() Then
    Begin
      AddPropertiesFromObject(Prop, Prop);
    End;

    Inc(Index);
  Until False;
End;

Procedure TCustomPropertyEditor.SetTarget(Target: TERRAObject);
Begin
  If Target <> _Target Then
    Self.Clear();

  _Target := Target;
  If Target = Nil Then
    Exit;

  AddPropertiesFromObject(Nil, Target);
End;

Procedure TCustomPropertyEditor.InsertRow(Parent, Prop: TERRAObject);
Var
  CellType:TPropertyCellType;
  Cell:TPropertyCell;
  S:TERRAString;
Begin
  S := Prop.GetObjectType();

  If StringEquals(S, 'color') Then
    CellType := TColorCell
  Else
  If StringEquals(S, 'bool') Then
    CellType := TBooleanCell
  Else
    CellType := TTextCell;

  Cell := CellType.Create(Self, Parent, Prop);

  Inc(_CellCount);
  SetLength(_Cells, _CellCount);
  _Cells[Pred(_CellCount)] := Cell;
End;

procedure TCustomPropertyEditor.Clear;
Var
  I:Integer;
begin
  For I:=0 To Pred(_CellCount) Do
    ReleaseObject(_Cells[I]);

  _CellCount := 0;

  Self.Repaint();
end;

procedure TCustomPropertyEditor.Paint;
Var
  I, N, MidW, H:Integer;
begin
  inherited;

  If _CellCount<=0 Then
    Exit;

  MidW := GetMiddle();

  Canvas.Pen.Style := psDash;
  Canvas.Pen.Color := MarginColor;
  Canvas.Pen.Width := 2;

  Canvas.MoveTo(MidW, 0);
  Canvas.LineTo(MidW, Self.Height);

  //Canvas.Pen.Style := psDot;
  Canvas.Pen.Width := 1;

  N := 0;
  For I:=0 To Pred(_CellCount) Do
  Begin
    If Not _Cells[I]._Visible Then
      Continue;

    H := (MarginTop Shr 1) + Succ(N) * CellHeight;
    Canvas.MoveTo(0, H);
    Canvas.LineTo(Self.Width, H);

    Inc(N);
  End;
end;

function TCustomPropertyEditor.GetMiddle: Integer;
begin
  Result := Trunc(Self.Width * 0.4);
end;

procedure TCustomPropertyEditor.Resize;
Var
  I, N:Integer;
begin
  N := 0;
  For I:=0 To Pred(_CellCount) Do
  If _Cells[I]._Visible Then
  Begin
    _Cells[I]._Index := N;
    _Cells[I].Resize();
    Inc(N);
  End;
end;

procedure TCustomPropertyEditor.RequestUpdate;
Var
  I:Integer;
begin
  For I:=0 To Pred(_CellCount) Do
    _Cells[I].Update();
end;

function TCustomPropertyEditor.FindCell(Prop: TERRAObject): TPropertyCell;
Var
  I:Integer;
begin
  For I:=0 To Pred(_CellCount) Do
  If _Cells[I]._Prop = Prop Then
  Begin
    Result := _Cells[I];
    Exit;
  End;
  Result := Nil;
end;

{ TPropertyCell }
Constructor TPropertyCell.Create(Owner:TCustomPropertyEditor; Parent, Prop: TERRAObject);
Var
  I:Integer;
  S:TERRAString;
Begin
  _Owner := Owner;
  _Parent := Parent;
  _Prop := Prop;

  _Index := 0;
  For I:=0 To Pred(_Owner._CellCount) Do
  If (_Owner._Cells[I]._Visible) Then
    Inc(_Index);

  _Label := TLabel.Create(Owner);
  _Label.Parent := Owner;
  _Label.Caption := Prop.ObjectName;

  _Editor := Self.CreateEditor();
  _Editor.Parent := _Owner;
  // Chk.Anchors :=  [akLeft, akTop, akRight, akBottom];

  If (Not _Prop.IsValueObject()) Then
  Begin
    _Expand := TSpeedButton.Create(_Owner);
    _Expand.Parent := _Owner;
    _Expand.Width := ExpandSize;
    _Expand.Height := _Expand.Width;
    _Expand.Caption := '+';
    _Expand.OnMouseDown := ExpandProps;
  End Else
    _Expand := Nil;

  SetVisible(_Owner.FindCell(_Parent) = Nil);

  Self.Update();
  Self.Resize();
end;

procedure TPropertyCell.ExpandProps(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  I:Integer;
  Value:Boolean;
Begin
  Value := _Expand.Caption = '+';

  If Value Then
    _Expand.Caption := '-'
  Else
    _Expand.Caption := '+';

  For I:=0 To Pred(_Owner._CellCount) Do
  If (_Owner._Cells[I]._Parent = Self._Prop) Then
  Begin
    _Owner._Cells[I].SetVisible(Value);
  End;

  _Owner.RequestUpdate();
  _Owner.Resize();
End;

Procedure TPropertyCell.Release;
Begin
  FreeAndNil(_Label);
  FreeAndNil(_Editor);
  FreeAndNil(_Expand);
End;

procedure TPropertyCell.Resize;
begin
  _Label.Left := 10;
  _Label.Top := MarginTop + _Index * CellHeight;

  _Editor.Top := _Label.Top;
  _Editor.Left := _Owner.GetMiddle() + MarginSide;
  _Editor.Width := _Owner.Width - (_Editor.Left + MarginSide);

  If Assigned(_Expand) Then
  Begin
    _Expand.Top := _Label.Top;
    _Expand.Left := _Owner.GetMiddle() - ((MarginSide Shr 1) + _Expand.Width);
  End;
end;

procedure TPropertyCell.SetVisible(Value: Boolean);
begin
  _Visible := Value;

  If Assigned(_Label) Then
    _Label.Visible := Value;

  If Assigned(_Editor) Then
    _Editor.Visible := Value;

  If Assigned(_Expand) Then
    _Expand.Visible := Value;
end;

{ TTextCell }
Function TTextCell.CreateEditor: TControl;
Begin
  _Edit := TEdit.Create(_Owner);
  //_Edit.OnKeyDown := Self.OnKeyDown;
  _Edit.OnChange := Self.OnChange;
  Result := _Edit;
End;

procedure TTextCell.OnChange(Sender: TObject);
begin
  _Prop.SetBlob(_Edit.Text);
end;

procedure TTextCell.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key<>keyEnter Then
    Exit;

  _Prop.SetBlob(_Edit.Text);
end;

Procedure TTextCell.Update;
Begin
  _Edit.Text := _Prop.GetBlob();
End;

{ TColorCell }
Function TColorCell.CreateEditor: TControl;
Begin
  _Shape := TShape.Create(_Owner);
  _Shape.Shape := stRoundRect;
  _Shape.Width := CellHeight - 10;
  _Shape.Height := _Shape.Width;
  _Shape.OnMouseDown := Self.OnMouseDown;
  Result := _Shape;
End;

procedure TColorCell.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  C:Color;
begin
  If _Dialog = Nil Then
  Begin
    _Dialog := TColorDialog.Create(_Owner);
  End;

  //_Dialog.Color :=
  If _Dialog.Execute Then
  Begin
    C := TERRAColorPack(_Dialog.Color);
    _Prop.SetBlob(TERRA_Color.ColorToString(C));
    _Owner.RequestUpdate();
  End;
end;

procedure TColorCell.Update;
Var
  S:TERRAString;
begin
  S := _Prop.GetBlob();
  _Shape.Brush.Color := TERRAColorUnpack(ColorCreateFromString(S));
end;

{ TBooleanCell }
Function TBooleanCell.CreateEditor: TControl;
Begin
  _Check := TCheckbox.Create(_Owner);
  _Check.OnClick := OnClick;
  Result := _Check;
End;

procedure TBooleanCell.Update;
begin
  _Check.Checked := StringToBool(_Prop.GetBlob());
end;

Procedure TBooleanCell.OnClick(Sender: TObject);
begin
  _Prop.SetBlob(BoolToString(_Check.Checked));
end;

end.
