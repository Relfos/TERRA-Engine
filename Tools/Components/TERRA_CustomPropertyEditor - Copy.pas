unit TERRA_CustomPropertyEditor;

interface

uses Windows, SysUtils, Classes, Messages, Controls, Grids, StdCtrls,
  TERRA_String, TERRA_Object;

type

{ Forward Declarations }

  TItemProp = class;
  TValueListStrings = class;

{ TCustomPropertyEditor }

  TDisplayOption = (doColumnTitles, doAutoColResize, doKeyColFixed);
  TDisplayOptions = set of TDisplayOption;

  TKeyOption = (keyEdit, keyAdd, keyDelete, keyUnique);
  TKeyOptions = set of TKeyOption;

  TGetPickListEvent = procedure(Sender: TObject; const KeyName: string;
    Values: TStrings) of object;

  TOnValidateEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    const KeyName, KeyValue: string) of object;

  TPropertyCell = Record
    Prop:TERRAObject;
    Editor:TComponent;
    Rects:Array[0..1] Of TRect;
  End;

  TCustomPropertyEditor = class(TCustomDrawGrid)
  private
    FTitleCaptions: TStrings;
    FStrings: TValueListStrings;
    FKeyOptions: TKeyOptions;
    FDisplayOptions: TDisplayOptions;
    FDropDownRows: Integer;
    FDupKeySave: string;
    FDeleting: Boolean;
    FAdjustingColWidths: Boolean;
    FEditUpdate: Integer;
    FCountSave: Integer;
    FEditList: TInplaceEditList;
    FOnGetPickList: TGetPickListEvent;
    FOnEditButtonClick: TNotifyEvent;
    FOnValidate: TOnValidateEvent;

    _Target: TERRAObject;

    _CellData:Array Of TPropertyCell;

    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    function GetItemProp(const KeyOrIndex: Variant): TItemProp;
    function GetKey(Index: Integer): string;
    function GetValue(const Key: string): string;
    function GetOnStringsChange: TNotifyEvent;
    function GetOnStringsChanging: TNotifyEvent;
    function GetStrings: TStrings;
    procedure PutItemProp(const KeyOrIndex: Variant; const Value: TItemProp);
    procedure SetDisplayOptions(const Value: TDisplayOptions);
    procedure SetDropDownRows(const Value: Integer);
    procedure SetKey(Index: Integer; const Value: string);
    procedure SetKeyOptions(Value: TKeyOptions);
    procedure SetTitleCaptions(const Value: TStrings);
    procedure SetValue(const Key, Value: string);
    procedure SetOnStringsChange(const Value: TNotifyEvent);
    procedure SetOnStringsChanging(const Value: TNotifyEvent);
    procedure SetOnEditButtonClick(const Value: TNotifyEvent);
    function GetOptions: TGridOptions;
    procedure SetOptions(const Value: TGridOptions);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  protected
    procedure AdjustColWidths; virtual;
    procedure AdjustRowCount; virtual;
    procedure ColWidthsChanged; override;
    function CanEditModify: Boolean; override;
    function CreateEditor: TInplaceEdit; override;
    procedure CreateWnd; override;
    procedure DoExit; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure DoOnValidate; virtual;
    procedure EditListGetItems(ACol, ARow: Integer; Items: TStrings);
    function GetCell(ACol, ARow: Integer): string; virtual;
    function GetColCount: Integer;
    function GetEditLimit: Integer; override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    function GetEditText(ACol, ARow: Integer): string; override;
    function GetPickList(Values: TStrings; ClearFirst: Boolean = True): Boolean;
    function GetRowCount: Integer;
    function IsEmptyRow: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetCell(ACol, ARow: Integer; const Value: string); virtual;
    procedure SetEditText(ACol, ARow: Integer; const Value: string); override;
    procedure SetStrings(const Value: TStrings); virtual;
    procedure StringsChanging; dynamic;
    function TitleCaptionsStored: Boolean;
    property EditList: TInplaceEditList read FEditList;

    procedure DeleteRow(ARow: Integer); override;
    function FindRow(const KeyName: string; var Row: Integer): Boolean;
    function InsertRow(const KeyName, Value: string; Append: Boolean): Integer;

    procedure AddPropertiesFromObject(Const Prev:TERRAString; Source:TERRAObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure SetTarget(Target:TERRAObject);

    procedure Refresh;
    function RestoreCurrentRow: Boolean;
    property Cells[ACol, ARow: Integer]: string read GetCell write SetCell;
    property ColCount read GetColCount;
    property ItemProps[const KeyOrIndex: Variant]: TItemProp read GetItemProp write PutItemProp;
    property Keys[Index: Integer]: string read GetKey write SetKey;
    property RowCount: Integer read GetRowCount;
    property Values[const Key: string]: string read GetValue write SetValue;
    property VisibleColCount;
    property VisibleRowCount;

    Property Target:TERRAObject Read _Target;

  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultColWidth default 150;
    property DefaultDrawing;
    property DefaultRowHeight default 18;
    property DisplayOptions: TDisplayOptions read FDisplayOptions
      write SetDisplayOptions default [doColumnTitles, doAutoColResize, doKeyColFixed];
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownRows: Integer read FDropDownRows write SetDropDownRows default 8;
    property Enabled;
    property FixedColor;
    property FixedCols default 0;
    property Font;
    property GridLineWidth;
    property KeyOptions: TKeyOptions read FKeyOptions write SetKeyOptions default [];
    property Options: TGridOptions read GetOptions write SetOptions default [goFixedVertLine, goFixedHorzLine,
      goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor,
      goThumbTracking];
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property Strings: TStrings read GetStrings write SetStrings;
    property TabOrder;
    property TitleCaptions: TStrings read FTitleCaptions write SetTitleCaptions stored TitleCaptionsStored;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write SetOnEditButtonClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnGetPickList: TGetPickListEvent read FOnGetPickList write FOnGetPickList;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnStringsChange: TNotifyEvent read GetOnStringsChange
      write SetOnStringsChange;
    property OnStringsChanging: TNotifyEvent read GetOnStringsChanging
      write SetOnStringsChanging;
    property OnTopLeftChanged;
    property OnValidate: TOnValidateEvent read FOnValidate write FOnValidate;
  end;

  (*$HPPEMIT 'class DELPHICLASS TItemProp;' *)

{ TValueListStrings }

  TItemProps = array of TItemProp;

  TValueListStrings = class(TStringList)
  private
    FItemProps: TItemProps;
    FEditor: TCustomPropertyEditor;
    function GetItemProp(const KeyOrIndex: Variant): TItemProp;
    procedure PutItemProp(const KeyOrIndex: Variant; const Value: TItemProp);
  protected
    procedure Changed; override;
    procedure Changing; override;
    function FindItemProp(const KeyOrIndex: Variant; Create: Boolean = False): TItemProp;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(AEditor: TCustomPropertyEditor); reintroduce;
    procedure Assign(Source: TPersistent); override;
    function KeyIsValid(const Key: string; RaiseError: Boolean = True): Boolean;
    procedure Clear; override;
    procedure CustomSort(Compare: TStringListSortCompare); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    property ItemProps[const KeyOrIndex: Variant]: TItemProp read GetItemProp write PutItemProp;
  end;

{ TItemProp }

  TItemProp = class(TPersistent)
  private
    FEditor: TCustomPropertyEditor;
    FEditMask: string;
    FEditStyle: TEditStyle;
    FPickList: TStrings;
    FMaxLength: Integer;
    FReadOnly: Boolean;
    FKeyDesc: string;
    function GetPickList: TStrings;
    procedure PickListChange(Sender: TObject);
    procedure SetEditMask(const Value: string);
    procedure SetMaxLength(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetEditStyle(const Value: TEditStyle);
    procedure SetPickList(const Value: TStrings);
    procedure SetKeyDesc(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateEdit;
  public
    constructor Create(AEditor: TCustomPropertyEditor);
    destructor Destroy; override;
    function HasPickList: Boolean;
  published
    property EditMask: string read FEditMask write SetEditMask;
    property EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property KeyDesc: string read FKeyDesc write SetKeyDesc;
    property PickList: TStrings read GetPickList write SetPickList;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

procedure Register;

implementation
uses Forms, Dialogs, TypInfo, Variants, Consts;


procedure Register;
begin
  RegisterComponents('Samples', [TCustomPropertyEditor]);
end;

{ TERRACustomPropertyEditor }

constructor TCustomPropertyEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TValueListStrings.Create(Self);
  FTitleCaptions := TStringList.Create;
  FTitleCaptions.Add(SKeyCaption);
  FTitleCaptions.Add(SValueCaption);
  ColCount := 2;
  inherited RowCount := 2;
  FixedCols := 0;
  DefaultColWidth := 150;
  DefaultRowHeight := 18;
  Width := 306;
  Height := 300;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking];
  FDisplayOptions := [doColumnTitles, doAutoColResize, doKeyColFixed];
  Col := 1;
  FDropDownRows := 8;
end;

destructor TCustomPropertyEditor.Destroy;
begin
  inherited;
  FTitleCaptions.Free;
  FStrings.Free;
end;

function TCustomPropertyEditor.CreateEditor: TInplaceEdit;
begin
  FEditList := TInplaceEditList.Create(Self);
  EditList.DropDownRows := FDropDownRows;
  EditList.OnEditButtonClick := FOnEditButtonClick;
  EditList.OnGetPickListitems := EditListGetItems;
  Result := FEditList;
end;

{ Helper Functions }

function FormatLine(const Key, Value: string): string;
begin
  Result := Format('%s=%s', [Key, Value]);
end;

function TCustomPropertyEditor.IsEmptyRow: Boolean;
begin
  Result := ((Row - FixedRows) < Strings.Count) and
    (Cells[0, Row] = '') and (Cells[1, Row] = '');
end;

function TCustomPropertyEditor.FindRow(const KeyName: string;
  var Row: Integer): Boolean;
begin
  Row := Strings.IndexOfName(KeyName);
  Result := Row <> -1;
  if Result then Inc(Row, FixedRows);
end;

{ Property Set/Get }

function TCustomPropertyEditor.GetColCount: Integer;
begin
  Result := inherited ColCount;
end;

function TCustomPropertyEditor.GetRowCount: Integer;
begin
  Result := inherited RowCount;
end;

function TCustomPropertyEditor.GetCell(ACol, ARow: Integer): string;
var
  Index: Integer;
  ValPos: Integer;
begin
  if (ARow = 0) and (doColumnTitles in DisplayOptions) then
  begin
    if ACol < FTitleCaptions.Count then
      Result := FTitleCaptions[ACol] else
      Result := '';
  end
  else if Strings.Count = 0 then
    Result := ''
  else
  begin
    Index := ARow - FixedRows;
    if ACol = 0 then
      Result := Strings.Names[Index]
    else
    begin
      Result := Strings.Strings[Index];
      ValPos := Pos('=', Result);
      if ValPos > 0 then
        Delete(Result, 1, ValPos);
    end;
  end;
end;

procedure TCustomPropertyEditor.SetCell(ACol, ARow: Integer;
  const Value: string);
var
  Index: Integer;
  Line: string;
begin
  Index := ARow - FixedRows;
  if ACol = 0 then
    Line := FormatLine(Value, Cells[1, ARow]) else
    Line := FormatLine(Cells[0, ARow], Value);
  if Index >= Strings.Count then
    Strings.Add(Line) else
    Strings[Index] := Line;
end;

procedure TCustomPropertyEditor.SetDropDownRows(const Value: Integer);
begin
  FDropDownRows := Value;
  if Assigned(EditList) then
    EditList.DropDownRows := Value;
end;

function TCustomPropertyEditor.GetKey(Index: Integer): string;
begin
  Result := GetCell(0, Index);
end;

procedure TCustomPropertyEditor.SetKey(Index: Integer; const Value: string);
begin
  SetCell(0, Index, Value);
end;

function TCustomPropertyEditor.GetValue(const Key: string): string;
var
  I: Integer;
begin
  if FindRow(Key, I) then
    Result := Cells[1, I] else
    Result := '';
end;

procedure TCustomPropertyEditor.SetValue(const Key, Value: string);
var
  I: Integer;
begin
  if FindRow(Key, I) then Cells[1, I] := Value
  else InsertRow(Key, Value, True);
end;

procedure TCustomPropertyEditor.SetTitleCaptions(const Value: TStrings);
begin
  FTitleCaptions.Assign(Value);
  Refresh;
end;

function TCustomPropertyEditor.TitleCaptionsStored: Boolean;
begin
  Result := TitleCaptions.Text <> (SKeyCaption+SLineBreak+SValueCaption+SLineBreak);
end;

function TCustomPropertyEditor.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TCustomPropertyEditor.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

procedure TCustomPropertyEditor.SetDisplayOptions(const Value: TDisplayOptions);

  procedure SetColumnTitles(Visible: Boolean);
  begin
    if Visible then
    begin
      if RowCount < 2 then inherited RowCount := 2;
      FixedRows := 1;
    end else
      FixedRows := 0;
  end;

begin
  if (doColumnTitles in DisplayOptions) <> (doColumnTitles in Value) then
    SetColumnTitles(doColumnTitles in Value);
  FDisplayOptions := Value;
  AdjustColWidths;
  Refresh;
end;

function TCustomPropertyEditor.GetItemProp(const KeyOrIndex: Variant): TItemProp;
begin
  Result := FStrings.GetItemProp(KeyOrIndex);
end;

procedure TCustomPropertyEditor.PutItemProp(const KeyOrIndex: Variant; const Value: TItemProp);
begin
  FStrings.PutItemProp(KeyOrIndex, Value);
end;

procedure TCustomPropertyEditor.SetKeyOptions(Value: TKeyOptions);
begin
  { Need to be able to Edit when you can Add }
  if not (keyEdit in Value) and (keyEdit in FKeyOptions) then
    Value := Value - [keyAdd];
  if (keyAdd in Value) and not (keyAdd in FKeyOptions) then
    Value := Value + [keyEdit];
  if not (keyEdit in Value) and (Col = 0) then
    Col := 1;
  FKeyOptions := Value;
end;

function TCustomPropertyEditor.GetOnStringsChange: TNotifyEvent;
begin
  Result := FStrings.OnChange;
end;

function TCustomPropertyEditor.GetOnStringsChanging: TNotifyEvent;
begin
  Result := FStrings.OnChanging;
end;

procedure TCustomPropertyEditor.SetOnStringsChange(const Value: TNotifyEvent);
begin
  FStrings.OnChange := Value;
end;

procedure TCustomPropertyEditor.SetOnStringsChanging(const Value: TNotifyEvent);
begin
  FStrings.OnChanging := Value;
end;

procedure TCustomPropertyEditor.SetOnEditButtonClick(const Value: TNotifyEvent);
begin
  FOnEditButtonClick := Value;
  if Assigned(EditList) then
    EditList.OnEditButtonClick := FOnEditButtonClick;
end;

{ Display / Refresh }

procedure TCustomPropertyEditor.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  CellText: string;
  ItemProp: TItemProp;
begin
  If Length(_CellData)<=ARow Then
    SetLength(_CellData, Succ(ARow));

  If (ACol>=0) And (ACol<=1) Then
    _CellData[aRow].Rects[ACol] := ARect;

  if DefaultDrawing then
  begin
    if (ACol = 0) and (ARow > FixedRows-1) then
      ItemProp := FStrings.FindItemProp(ARow-FixedRows, False)
    else
      ItemProp := nil;
    if (ItemProp <> nil) and (ItemProp.KeyDesc <> '') then
      CellText := ItemProp.KeyDesc
    else
      CellText := Cells[ACol, ARow];
    Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, CellText);
  end;

  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TCustomPropertyEditor.AdjustColWidths;

begin
  if not FAdjustingColWidths and HandleAllocated and Showing and
     (doAutoColResize in DisplayOptions) then
  begin
    FAdjustingColWidths := True;
    try
      if (ColWidths[0] + ColWidths[1]) <> (ClientWidth - 2) then
      begin
        if doKeyColFixed in DisplayOptions then
          ColWidths[1] := ClientWidth - ColWidths[0] - 2
        else
        begin
          ColWidths[0] := (ClientWidth - 2) div 2;
          ColWidths[1] := ColWidths[0] + ((ClientWidth - 2) mod 2);
        end;
      end;
    finally
      FAdjustingColWidths := False;
    end;
  end;
end;

procedure TCustomPropertyEditor.AdjustRowCount;
var
  NewRowCount: Integer;
begin
  if Strings.Count > 0 then
    NewRowCount := Strings.Count + FixedRows else
    NewRowCount := FixedRows + 1;
  if NewRowCount <> RowCount then
  begin
    if NewRowCount < Row then
      Row := NewRowCount - 1;
    if (doColumnTitles in DisplayOptions) and (Row = 0) then Row := 1;
    inherited RowCount := NewRowCount;
  end;
end;

procedure TCustomPropertyEditor.Resize;
begin
  inherited;
  AdjustColWidths;
end;

procedure TCustomPropertyEditor.ColWidthsChanged;
begin
  AdjustColWidths;
  inherited;
end;

procedure TCustomPropertyEditor.Refresh;
begin
  if FEditUpdate = 0 then
  begin
    AdjustRowCount;
    Invalidate;
    InvalidateEditor;
  end;
end;

procedure TCustomPropertyEditor.StringsChanging;
begin
  HideEdit;
end;

{ Editing }

function TCustomPropertyEditor.CanEditModify: Boolean;
var
  ItemProp: TItemProp;
begin
  Result := inherited CanEditModify;
  if Result then
  begin
    ItemProp := FStrings.FindItemProp(Row-FixedRows);
    if Assigned(ItemProp) then
      Result := not ItemProp.ReadOnly;
  end;
end;

procedure TCustomPropertyEditor.DisableEditUpdate;
begin
  if FEditUpdate = 0 then
    FCountSave := Strings.Count;
  Inc(FEditUpdate);
end;

procedure TCustomPropertyEditor.EnableEditUpdate;
begin
  Dec(FEditUpdate);
  if (FEditUpdate = 0) and (FCountSave <> Strings.Count) then
      Refresh;
end;

function TCustomPropertyEditor.GetEditLimit: Integer;
var
  ItemProp: TItemProp;
begin
  ItemProp := FStrings.FindItemProp(Row-FixedRows);
  if Assigned(ItemProp) then
    Result := ItemProp.MaxLength else
    Result := inherited GetEditLimit;
end;

function TCustomPropertyEditor.GetEditMask(ACol, ARow: Integer): string;
var
  ItemProp: TItemProp;
begin
  ItemProp := FStrings.FindItemProp(Row-FixedRows);
  if Assigned(ItemProp) then
    Result := ItemProp.EditMask else
    Result := '';
  if Assigned(OnGetEditMask) then
    OnGetEditMask(Self, ACol, ARow, Result);
end;

function TCustomPropertyEditor.GetEditStyle(ACol, ARow: Integer): TEditStyle;
var
  ItemProp: TItemProp;
begin
  Result := esSimple;
  if (ACol <> 0) then
  begin
    ItemProp := FStrings.FindItemProp(Row-FixedRows);
    if Assigned(ItemProp) and (ItemProp.EditStyle <> esSimple) then
      Result := ItemProp.EditStyle
    else if GetPickList(EditList.PickList.Items) then
      Result := esPickList;
  end;
end;

function TCustomPropertyEditor.GetEditText(ACol, ARow: Longint): string;
begin
  Result := Cells[ACol, ARow];
  if Assigned(OnGetEditText) then OnGetEditText(Self, ACol, ARow, Result);
end;

procedure TCustomPropertyEditor.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  inherited SetEditText(ACol, ARow, Value);
  DisableEditUpdate;
  try
    if ((ARow - FixedRows) >= Strings.Count) and (Value <> '') then
    begin
      Strings.Append('');
      FCountSave := FStrings.Count;
    end;
    if (Value <> Cells[ACol, ARow]) then
    begin
      { If the Key is being updated, defer any error about duplicates until
        we try to we try to write it a second time (which will happen when
        the user tries to move to a different cell) }
      if (ACol = 0) and not FStrings.KeyIsValid(Value, False) and
         (FDupKeySave <> Value) then
        FDupKeySave := Value
      else
      begin
        FDupKeySave := '';
        Cells[ACol, ARow] := Value;

        Self.DoOnValidate();
      end;
    end;
  finally
    EnableEditUpdate;
  end;
end;

procedure TCustomPropertyEditor.EditListGetItems(ACol, ARow: Integer;
  Items: TStrings);
var
  ItemProp: TItemProp;
begin
  if (ACol <> 0) then
  begin
    ItemProp := FStrings.FindItemProp(Row-FixedRows);
    if Assigned(ItemProp) and ItemProp.HasPickList then
      Items.Assign(ItemProp.PickList)
    else
      Items.Clear;
    GetPickList(Items, False);
  end;
end;

function TCustomPropertyEditor.GetPickList(Values: TStrings;
  ClearFirst: Boolean = True): Boolean;
begin
  if Assigned(FOnGetPickList) and (Keys[Row] <> '') then
  begin
    Values.BeginUpdate;
    try
      if ClearFirst then
        Values.Clear;
      FOnGetPickList(Self, Keys[Row], Values);
      Result := Values.Count > 0;
      EditList.PickListLoaded := Result;
    finally
      Values.EndUpdate;
    end
  end
  else
    Result := False;
end;

function TCustomPropertyEditor.InsertRow(const KeyName, Value: string;
  Append: Boolean): Integer;
begin
  Result := Row;
  { If row is empty, use it, otherwise add a new row }
  if (Result > Strings.Count) or not IsEmptyRow then
  begin
    Strings.BeginUpdate;
    try
      if Append then
        Result := Strings.Add(FormatLine(KeyName, Value)) + FixedRows else
        Strings.Insert(Result - FixedRows, FormatLine(KeyName, Value));
    finally
      Strings.EndUpdate;
    end;
  end else
  begin
    Cells[0, Result] := KeyName;
    Cells[1, Result] := Value;
  end;

  If Length(_CellData)<=Strings.Count Then
    SetLength(_CellData, Succ(Strings.Count));
end;

procedure TCustomPropertyEditor.DeleteRow(ARow: Integer);
begin
  if FDeleting then Exit;
  FDeleting := True;
  try
    if (Row >= RowCount - 1) and (Strings.Count > 1) then
      Row := Row - 1;
    Strings.Delete(ARow - FixedRows);
  finally
    FDeleting := False;
  end;
end;

function TCustomPropertyEditor.RestoreCurrentRow: Boolean;

  function RestoreInplaceEditor: Boolean;
  var
    ChangedText: string;
  begin
    Result := False;
    if Assigned(EditList) and EditList.Modified then
    begin
      ChangedText := EditList.EditText;
      EditList.RestoreContents;
      Result := ChangedText <> EditList.EditText;
      if Result then
        EditList.SelectAll;
    end;
  end;

begin
  Result := RestoreInplaceEditor;
  if not Result and IsEmptyRow then
  begin
    DeleteRow(Row);
    Result := True;
  end
end;

procedure TCustomPropertyEditor.RowMoved(FromIndex, ToIndex: Longint);
begin
  Strings.Move(FromIndex, ToIndex);
  inherited RowMoved(FromIndex, ToIndex);
end;

procedure TCustomPropertyEditor.DoOnValidate;
Var
  KeyName, KeyValue:TERRAString;
  Obj:TERRAObject;
begin
  if (Assigned(InplaceEditor)) And (InplaceEditor.Modified) then
  begin
    KeyName := GetCell(0, Row);
    KeyValue := GetCell(1, Row);
    
    If Assigned(FOnValidate) Then
      FOnValidate(Self, Col, Row, KeyName, KeyValue); ///!!!

    If Assigned(_Target) Then
    Begin
      Obj := _Target.FindPropertyWithPath(KeyName);

      If Assigned(Obj) Then
        Obj.SetBlob(KeyValue);
    End;
  end;
end;

function TCustomPropertyEditor.SelectCell(ACol, ARow: Integer): Boolean;
Var
  Chk:TCheckBox;
begin
  { Delete any blank rows when moving to a new row }
  if (ARow <> Row) and (Strings.Count > 0) and IsEmptyRow and not FDeleting then
  begin
    Result := (ARow < Row);
    DeleteRow(Row);
    { When the selected cell is below, we need to adjust for the deletion }
    if not Result then
      FocusCell(ACol, ARow - 1, True);
  end else
  begin
    DoOnValidate;
    Result := inherited SelectCell(ACol, ARow) and
              ((goRowSelect in Options) or (keyEdit in KeyOptions) or (ACol > 0));

    If Length(_CellData)<=ARow Then
      SetLength(_CellData, Succ(ARow));
    If (ARow>0) And (ACol = 1) Then
    Begin
      If (_CellData[aRow].Editor = Nil) Then
      Begin
        Chk := TCheckBox.Create(Self);
        Chk.Parent := Self;
        Chk.Left := _CellData[aRow].Rects[1].Left;
        Chk.Top := _CellData[aRow].Rects[1].Top;
       // Chk.Anchors :=  [akLeft, akTop, akRight, akBottom];

        _CellData[aRow].Editor := Chk;
      End;

    End;

  end;
end;

procedure TCustomPropertyEditor.KeyDown(var Key: Word; Shift: TShiftState);

  function InsertOK: Boolean;
  begin
    Result := (Length(Cells[0, Row]) > 0) and (keyAdd in KeyOptions)
  end;

  procedure SetRow(NewRow: Integer);
  begin
    Row := NewRow;
    Key := 0;
  end;

begin
  case Key of
    VK_DOWN:
      if Shift = [ssCtrl] then
        SetRow(RowCount - 1)
      else if (Shift = []) and (Row = RowCount - 1) and InsertOK then
        SetRow(InsertRow('', '', True));
    VK_UP:
      if Shift = [ssCtrl] then SetRow(FixedRows);
    VK_INSERT:
      if InsertOK then SetRow(InsertRow('', '', False));
    VK_DELETE:
      if (Shift = [ssCtrl]) and (keyDelete in KeyOptions) then
      begin
        DeleteRow(Row);
        Key := 0;
      end;
    VK_ESCAPE:
      RestoreCurrentRow;
  end;
  inherited KeyDown(Key, Shift);
end;


function TCustomPropertyEditor.GetOptions: TGridOptions;
begin
  Result := inherited Options;
end;

procedure TCustomPropertyEditor.SetOptions(const Value: TGridOptions);
begin
  if goColMoving in Value then
    raise Exception.CreateRes(@SNoColumnMoving);
  inherited Options := Value;
end;

procedure TCustomPropertyEditor.CreateWnd;
begin
  inherited;
  { Clear the default vertical scrollbar since this will affect the client
    width of the control which will cause problems when calculating the
    column widths in the AdjustColWidths function }
  SetScrollRange(Handle, SB_VERT, 0, 0, False);
end;

procedure TCustomPropertyEditor.DoExit;
begin
  try
    DoOnValidate;
  except
    SetFocus;
    raise;
  end;
  inherited;
  HideEdit;
end;

procedure TCustomPropertyEditor.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
    AdjustColWidths;
end;

{ TValueListStrings }

constructor TValueListStrings.Create(AEditor: TCustomPropertyEditor);
begin
  FEditor := AEditor;
  inherited Create;
end;

procedure TValueListStrings.Assign(Source: TPersistent);
var
  I: Integer;
  ItemProp: TItemProp;
  SrcStrings: TStrings;
  ValStrings: TValueListStrings;
begin
  inherited;
  if Source is TValueListStrings then
  begin
    ValStrings := TValueListStrings(Source);
    for I := 0 to Count - 1 do
    begin
      ItemProp := ValStrings.FindItemProp(I);
      if Assigned(ItemProp) then
        ItemProps[I] := ItemProp;
    end;
  end
  else if Source is TStrings then
  begin
    SrcStrings := TStrings(Source);
    { See if the source strings have TItemProp clases stored in the data }
    for I := 0 to Count - 1 do
    begin
      if (SrcStrings.Objects[I] <> nil) and
          (SrcStrings.Objects[I] is TItemProp) then
        ItemProps[I] := TItemProp(SrcStrings.Objects[I]);
    end;
  end;
end;

procedure TValueListStrings.Changing;
begin
  inherited;
  if (UpdateCount = 0) and Assigned(FEditor) and (FEditor.FEditUpdate = 0) then
    FEditor.StringsChanging;
end;

procedure TValueListStrings.Changed;
begin
  inherited;
  if (UpdateCount = 0) and Assigned(FEditor) then
    FEditor.Refresh;
end;

function TValueListStrings.KeyIsValid(const Key: string; RaiseError: Boolean = True): Boolean;
var
  Index: Integer;
begin
  Result := True;
  if Pos('=', Key) <> 0 then
    raise Exception.CreateRes(@SNoEqualsInKey);
  if Assigned(FEditor) and (keyUnique in FEditor.KeyOptions) then
  begin
    if Key <> '' then
    begin
      Index := IndexOfName(Key);
      Result := (Index = -1);
      if not Result and RaiseError then
        raise Exception.CreateResFmt(@SKeyConflict, [Key]);
    end;
  end;
end;

procedure TValueListStrings.Clear;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Length(FItemProps) - 1 do
    FItemProps[I].Free;
  SetLength(FItemProps, 0);
end;

procedure TValueListStrings.CustomSort(Compare: TStringListSortCompare);
var
  I, OldIndex: Integer;
  OldOrder: TList;
  OldProps: TItemProps;
begin
  OldOrder := TList.Create;
  try
    { Preserve the existing order so we can re-associate the ItemProps }
    OldOrder.Count := Count;
    OldProps := Copy(FItemProps, 0, Count);
    for I := 0 to Count - 1 do
      OldOrder[I] := Pointer(Get(I));
    { Do the Sort }
    inherited;
    { Find and move the ItemProps }
    for I := 0 to Count - 1 do
    begin
      OldIndex := OldOrder.IndexOf(Pointer(Get(I)));
      FItemProps[I] := OldProps[OldIndex];
    end;
  finally
    OldOrder.Free;
  end;
  FEditor.InvalidateEditor;
end;

procedure TValueListStrings.Delete(Index: Integer);
begin
  Changing;
  inherited;
  FItemProps[Index].Free;
  if Index < Count then
    System.Move(FItemProps[Index + 1], FItemProps[Index],
      (Count - Index) * SizeOf(TItemProp));
  SetLength(FItemProps, Count);
  Changed;
end;

procedure TValueListStrings.Exchange(Index1, Index2: Integer);
var
  Item: TItemProp;
begin
  Changing;
  inherited;
  Item := FItemProps[Index1];
  FItemProps[Index1] := FItemProps[Index2];
  FItemProps[Index2] := Item;
  Changed;
end;

function TValueListStrings.FindItemProp(const KeyOrIndex: Variant;
  Create: Boolean = False): TItemProp;
var
  Index: Integer;
begin
  if (Count > 0) and Assigned(FItemProps) then
  begin
    if VarIsOrdinal(KeyOrIndex) then
      Index := KeyOrIndex
    else
    begin
      Index := IndexOfName(KeyOrIndex);
      if Create and (Index = -1) then
        raise Exception.CreateResFmt(@SKeyNotFound, [KeyOrIndex]);
    end;
    Result := FItemProps[Index];
    if Create and not Assigned(Result) then
    begin
      Result := TItemProp.Create(FEditor);
      FItemProps[Index] := Result;
    end;
  end
  else
    Result := nil;
end;

procedure TValueListStrings.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
var
  OldCount: Integer;
begin
  KeyIsValid(ExtractName(S));
  Changing;
  OldCount := Count;
  inherited;
  SetLength(FItemProps, Count);
  if Index < OldCount then
    System.Move(FItemProps[Index], FItemProps[Index + 1],
      (OldCount - Index) * SizeOf(TItemProp));
  FItemProps[Index] := nil;
  Changed;
end;

function TValueListStrings.GetItemProp(const KeyOrIndex: Variant): TItemProp;
begin
  Result := FindItemProp(KeyOrIndex, True);
end;

procedure TValueListStrings.Put(Index: Integer; const S: String);
var
  Name: string;
begin
  Name := ExtractName(S);
  KeyIsValid(Name, IndexOfName(Name) <> Index);
  inherited;
end;

procedure TValueListStrings.PutItemProp(const KeyOrIndex: Variant;
  const Value: TItemProp);
begin
  FindItemProp(KeyOrIndex, True).Assign(Value);
end;

{ TItemProp }

constructor TItemProp.Create(AEditor: TCustomPropertyEditor);
begin
  FEditor := AEditor;
end;

destructor TItemProp.Destroy;
begin
  inherited;
  FPickList.Free;
end;

procedure TItemProp.AssignTo(Dest: TPersistent);
begin
  if Dest is TItemProp then
    with Dest as TItemProp do
    begin
      EditMask := Self.EditMask;
      EditStyle := Self.EditStyle;
      PickList.Assign(Self.PickList);
      MaxLength := Self.MaxLength;
      ReadOnly := Self.ReadOnly;
      KeyDesc := Self.KeyDesc;
    end
  else
    inherited;
end;

procedure TItemProp.SetEditMask(const Value: string);
begin
  FEditMask := Value;
  UpdateEdit;
end;

procedure TItemProp.SetEditStyle(const Value: TEditStyle);
begin
  FEditStyle := Value;
  UpdateEdit;
end;

procedure TItemProp.SetKeyDesc(const Value: string);
begin
  FKeyDesc := Value;
end;

procedure TItemProp.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
  UpdateEdit;
end;

function TItemProp.HasPickList: Boolean;
begin
  Result := Assigned(FPickList) and (FPickList.Count > 0);
end;

function TItemProp.GetPickList: TStrings;
begin
  if not Assigned(FPickList) then
  begin
    FPickList := TStringList.Create;
    TStringList(FPickList).OnChange := PickListChange;
  end;
  Result := FPickList;
end;

procedure TItemProp.SetPickList(const Value: TStrings);
begin
  GetPickList.Assign(Value);
  UpdateEdit;
end;

procedure TItemProp.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  UpdateEdit;
end;

procedure TItemProp.UpdateEdit;
begin
  if Assigned(FEditor) and FEditor.EditorMode and
     (FEditor.FStrings.UpdateCount = 0) then
    FEditor.InvalidateEditor;
end;

procedure TItemProp.PickListChange(Sender: TObject);
begin
  if (EditStyle = esSimple) and (PickList.Count > 0) then
    EditStyle := esPickList
  else if (EditStyle = esPickList) and (PickList.Count = 0) then
    EditStyle := esSimple;
end;

Procedure TCustomPropertyEditor.AddPropertiesFromObject(Const Prev:TERRAString; Source:TERRAObject);
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
      Row := Self.InsertRow(Prev + Prop.ObjectName, S, True);
      _CellData[Row].Prop := Prop;
    End;

    If Not Prop.IsValueObject() Then
    Begin
      S := Prev + Prop.ObjectName + '.';
      AddPropertiesFromObject(S, Prop);
    End;

    Inc(Index);
  Until False;
End;

Procedure TCustomPropertyEditor.SetTarget(Target: TERRAObject);
Begin
  _Target := Target;
  If Target = Nil Then
    Self.Strings.Clear()
  Else
    AddPropertiesFromObject('', Target);
End;

end.
