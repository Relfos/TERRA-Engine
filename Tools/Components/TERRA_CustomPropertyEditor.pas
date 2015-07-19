unit TERRA_CustomPropertyEditor;

interface

uses Windows, SysUtils, Classes, Messages, ExtCtrls, Controls, StdCtrls,
  Dialogs, Graphics, Buttons,
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_OS, TERRA_Color, TERRA_VCLApplication,
  TERRA_FileManager, TERRA_FileUtils, TERRA_EnumProperty, TERRA_Math;

Const
  MarginTop = 30;
  MarginSide = 10;
  ExpandSize = 15;
  CellHeight = 25;

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
      _Expand:TPanel;

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

      //Procedure OnKeyDown(Sender:TObject; var Key: Word; Shift: TShiftState);
      Procedure OnChange(Sender:TObject); Virtual;
  End;

  TAngleCell = Class(TTextCell)
    Protected
      Procedure Update(); Override;
      Procedure OnChange(Sender:TObject); Override;
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

  TTextureCell = Class(TPropertyCell)
    Protected
      _Name:TLabel;
      _Dialog:TOpenDialog;

      Function CreateEditor():TControl; Override;
      Procedure Update(); Override;

      Procedure OnClick(Sender: TObject);
  End;

  TEnumCell = Class(TPropertyCell)
    Protected
      _List:TComboBox;

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

      _MarginColor:TColor;
      _EditColor:TColor;

      _ScrollRow:Integer;

//      _Scroll:TScrollBar;

      procedure SetTarget(Target: TERRAObject);
      procedure SetMarginColor(const Value: TColor);
      procedure SetEditColor(const Value: TColor);

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

      Function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      Function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

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

      Property MarginColor:TColor Read _MarginColor Write SetMarginColor;
      Property EditColor:TColor Read _EditColor Write SetEditColor;

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

(*  _Scroll := TScrollBar.Create(Self);
  _Scroll.Align := alClient;
  _Scroll.Kind := sbVertical;*)

  (*
  _Bevel := TBevel.Create(Self);
  _Bevel.Parent := Self;
  _Bevel.Width := 20;
  _Bevel.Height := Height;
  _Bevel.Top := 10;
  _Bevel.Left := Width Div 2;*)

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
  If Assigned(Target) Then
    AddPropertiesFromObject(Nil, Target);

  Self.Repaint();
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
  If StringEquals(S, 'texture') Then
    CellType := TTextureCell
  Else
  If StringEquals(S, 'angle') Then
    CellType := TAngleCell
  Else
  If StringEquals(S, 'enum') Then
    CellType := TEnumCell
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
  _ScrollRow := 0;
end;

procedure TCustomPropertyEditor.Paint;
Var
  I, N, MidW, TW, H, FY:Integer;
  S:TERRAString;
begin
//  inherited;

  MidW := GetMiddle();

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Self.Color;
  Canvas.Rectangle(0, 0, Self.Width, Self.Height);

  Canvas.Pen.Style := psDash;
  Canvas.Pen.Color := MarginColor;

  //Canvas.Pen.Style := psDot;
  Canvas.Pen.Width := 1;

  N := -_ScrollRow;
  For I:=0 To Pred(_CellCount) Do
  Begin
    If Not _Cells[I]._Visible Then
      Continue;

    H := MarginTop + N * CellHeight - (CellHeight Shr 2);
    Canvas.MoveTo(0, H);
    Canvas.LineTo(Self.Width, H);
    Inc(N);

    If I = 0 Then
      FY := H;
  End;

  H := MarginTop + N * CellHeight - (CellHeight Shr 2);
  Canvas.MoveTo(0, H);
  Canvas.LineTo(Self.Width, H);

  Canvas.Pen.Width := 2;
  Canvas.MoveTo(MidW, FY);
  Canvas.LineTo(MidW, Self.Height);

  If (Assigned(_Target)) And (_ScrollRow=0) Then
  Begin
    Canvas.Font.Color := Self.Font.Color;
    S := _Target.Name + ' ('+_Target.GetObjectType()+')';
    TW := Canvas.TextWidth(S);
    Canvas.TextOut(MidW -  (TW Shr 1), 5, S);
  End;
End;

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
(*  _Scroll.Width := 50;
  _Scroll.Height := Self.Height;
  _Scroll.Left := Self.Left;
  _Scroll.Top := Self.Top;
  _Scroll.Visible := True;*)

  For I:=0 To Pred(_CellCount) Do
    _Cells[I].Update();

  Self.Repaint();
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

procedure TCustomPropertyEditor.SetMarginColor(const Value: TColor);
begin
  _MarginColor := Value;
  Self.Repaint();
end;

procedure TCustomPropertyEditor.SetEditColor(const Value: TColor);
begin
  _EditColor := Value;
  Self.Repaint();
end;

function TCustomPropertyEditor.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    if _ScrollRow < Pred(Self._CellCount) then
    Begin
      Inc(_ScrollRow);
      Self.Resize();
      Self.Repaint();
    End;

    Result := True;
  end;
end;

function TCustomPropertyEditor.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    if _ScrollRow > 0 then
    Begin
      Dec(_ScrollRow);
      Self.Resize();
      Self.Repaint();
    End;

    Result := True;
  end;
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
  _Label.Caption := Prop.Name;
  _Label.Transparent := True;

  _Editor := Self.CreateEditor();
  _Editor.Parent := _Owner;
  // Chk.Anchors :=  [akLeft, akTop, akRight, akBottom];

  If (Not _Prop.IsValueObject()) Then
  Begin
    _Expand := TPanel.Create(_Owner);
    _Expand.Parent := _Owner;
    _Expand.Width := ExpandSize;
    _Expand.Height := _Expand.Width;
    _Expand.Caption := '+';
    _Expand.Color := _Owner.Color;
    _Expand.Ctl3D := False;
    _Expand.OnMouseDown := ExpandProps;
    _Expand.Cursor := crHandPoint;
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
  _Label.Top := MarginTop + (_Index - _Owner._ScrollRow) * CellHeight;

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
  _Edit.Color := _Owner.EditColor;
  _Edit.Font.Color := clWhite;
  _Edit.OnChange := Self.OnChange;
  _Edit.Cursor := crIBeam;
  Result := _Edit;
End;

procedure TTextCell.OnChange(Sender: TObject);
begin
  _Prop.SetBlob(_Edit.Text);
end;

(*procedure TTextCell.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key<>keyEnter Then
    Exit;

  _Prop.SetBlob(_Edit.Text);
end;*)

Procedure TTextCell.Update;
Begin
  _Edit.Text := _Prop.GetBlob();
End;


{ TAngleCell }              
Procedure TAngleCell.OnChange(Sender: TObject);
Begin
  _Prop.SetBlob(FloatToString(StringToFloat(_Edit.Text) * RAD));
End;

Procedure TAngleCell.Update;
Var
  Angle:Single;
Begin
  Angle := Round(StringToFloat(_Prop.GetBlob()) * DEG * 100) Div 100;
  _Edit.Text := FloatToString(Angle);
End;

{ TColorCell }
Function TColorCell.CreateEditor: TControl;
Begin
  _Shape := TShape.Create(_Owner);
  _Shape.Shape := stRoundRect;
  _Shape.Width := CellHeight - 10;
  _Shape.Height := _Shape.Width;
  _Shape.OnMouseDown := Self.OnMouseDown;
  _Shape.Cursor := crHandPoint;

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
  _Check.Caption := '';
  _Check.Color := _Owner.Color;
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

{ TTextureCell }
Function TTextureCell.CreateEditor: TControl;
Begin
  _Name := TLabel.Create(_Owner);
  _Name.OnClick := Self.OnClick;
  _Name.Color := _Owner.EditColor;
  _Name.Font.Color := clWhite;
  _Name.Cursor := crHandPoint;
  Result := _Name;
End;

Procedure TTextureCell.OnClick(Sender: TObject);
Begin
  If _Dialog = Nil Then
  Begin
    _Dialog := TOpenDialog.Create(_Owner);
    _Dialog.Options := [ofNoChangeDir, ofPathMustExist, ofFileMustExist];
  End;

  //_Dialog.Color :=
  If _Dialog.Execute Then
  Begin
    FileManager.Instance.AddPath(GetFilePath(_Dialog.FileName));
    _Prop.SetBlob(_Dialog.FileName);
    _Owner.RequestUpdate();
  End;
End;

Procedure TTextureCell.Update;
Begin
  _Name.Caption := _Prop.GetBlob();
End;

{ TEnumCell }
Function TEnumCell.CreateEditor: TControl;
Begin
  _List := TComboBox.Create(_Owner);
  _List.OnClick := Self.OnClick;
  _List.Color := _Owner.EditColor;
  _List.Style := csDropDownList;
  _List.Font.Color := clWhite;
  _List.Cursor := crHandPoint;
  Result := _List;
End;

Procedure TEnumCell.Update;
Var
  I:Integer;
  Enum:EnumProperty;
Begin
  If (_List.Items.Count<=0) Then
  Begin
    Enum := EnumProperty(Self._Prop);
    For I:=0 To Pred(Enum.Collection.Count) Do
      _List.Items.Add(Enum.Collection.GetByIndex(I));

    _List.ItemIndex := Enum.Collection.GetByName(_Prop.GetBlob());
  End;
End;

Procedure TEnumCell.OnClick(Sender: TObject);
Begin
  _Prop.SetBlob(_List.Items[_List.ItemIndex]);
End;

end.

