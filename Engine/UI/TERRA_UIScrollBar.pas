Unit TERRA_UIScrollbar;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_UIIcon;

Type
  UIScrollbarType = (
    scrollVertical,
    scrollHorizontal
    );

  UIScrollBarHandle = Class(UIIcon)
    Public
      Procedure Render(); Override;
  End;

  UIScrollBar = Class(Widget)
    Protected
      _Value:Single;
      _Max:Single;

      _Changed:Boolean;
      _Kind:UIScrollbarType;

      _Handle:UIScrollbarHandle;
      _HandlePos:Single;
      _HandleOffset:Vector2D;
      _HandleMin:Vector2D;
      _HandleMax:Vector2D;


      Procedure SetValue(Const Value: Single);
    Public
      OnChange:WidgetEventHandler;

      //Function OnHandleRegion(X,Y:Integer):Boolean;

      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const ScrollWidth, ScrollHeight, HandleWidth, HandleHeight:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer=-1);

      Procedure Render; Override;

      Procedure Slide(Ammount:Single);

      Property Value:Single Read _Value Write SetValue;
      Property Max:Single Read _Max Write _Max;

      Property Kind:UIScrollbarType Read _Kind;
  End;

Implementation
Uses TERRA_Log;

{UIScrollBar}
Constructor UIScrollBar.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const ScrollWidth, ScrollHeight, HandleWidth, HandleHeight:UIDimension; Const ComponentName:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self.TabIndex := TabIndex;

  Self._Value := 0.0;
  Self._Max := 100;
  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;

  If (ScrollWidth.Value >ScrollHeight.Value) Then
    Self._Kind := scrollHorizontal
  Else
    Self._Kind := scrollVertical;

  Self.Width := ScrollWidth;
  Self.Height := ScrollHeight;

  _Handle := UIScrollBarHandle.Create(Name+'_handle', Self, 0, 0, 1, HandleWidth, HandleHeight, ComponentName);

(*  If (Length(_ComponentList)>0) And (Assigned(_ComponentList[0])) And (Assigned(Self._ComponentList[0].Buffer)) Then
  Begin
    _PickWidth := Self._ComponentList[0].Buffer.Width;
    _PickHeight := Self._ComponentList[0].Buffer.Height;
  End;

  If (Length(_ComponentList)>1) And (Assigned(_ComponentList[1])) And (Assigned(Self._ComponentList[1].Buffer)) Then
  Begin
    _BgWidth := Self._ComponentList[1].Buffer.Width;
    _BgHeight := Self._ComponentList[1].Buffer.Height;
  End;*)
End;


Procedure UIScrollBar.Render;
Var
  Size:Vector2D;
  I, Height:Integer;
  Count:Integer;
  HH:Single;
  LastValue, NewValue:Single;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();

  _Handle.Draggable := True;

  If (_Changed) Then
    _Changed := False;

//If  (UI.Instance.Dragger = Self)  Then  MyColor := ColorBlue;

  _HandleMin := VectorCreate2D(0, 0);
  _HandleMax := VectorCreate2D(Self.GetDimension(Self.Width, uiDimensionWidth) - _Handle.GetDimension(_Handle.Width, uiDimensionWidth), Self.GetDimension(Self.Height, uiDimensionHeight) - _Handle.GetDimension(_Handle.Height, uiDimensionHeight));

  If (_Kind = scrollHorizontal) Then
  Begin
    HH := Self.GetDimension(Self.Width, uiDimensionWidth) * 0.25;
    _HandleOffset.X := -(_Handle.GetDimension(_Handle.Width, uiDimensionWidth)  * 0.5);
    _HandleOffset.Y := (Self.GetDimension(Self.Height, uiDimensionHeight) - _Handle.GetDimension(_Handle.Height, uiDimensionHeight)) * 0.5;

    Self.DrawComponent(0, 0, 0, Self.Width, Self.Height, 1, Self.IsSelected);
  End Else
  Begin
    HH := Self.GetDimension(Self.Height, uiDimensionHeight) * 0.25;
    _HandleOffset.X := (Self.GetDimension(Self.Width, uiDimensionWidth) - _Handle.GetDimension(_Handle.Width, uiDimensionWidth)) * 0.5;
    _HandleOffset.Y := -(_Handle.GetDimension(_Handle.Height, uiDimensionHeight) * 0.5);

    Self.DrawComponent(0, 0, 0, Self.Width, Self.Height, 1, Self.IsSelected);
  End;

  Inherited;

  LastValue := _Value;

  If (_Kind = scrollHorizontal) Then
    NewValue := (_Handle.RelativePosition.X /_HandleMax.X) * _Max
  Else
    NewValue := (_Handle.RelativePosition.Y /_HandleMax.Y) * _Max;

  If (LastValue<>NewValue) Then
  Begin
    _Value := NewValue;
    If Assigned(OnChange) Then
      OnChange(Self);
  End;
End;

Procedure UIScrollBar.SetValue(const Value: Single);
Begin
  _Value := Value;
  If Assigned(OnChange) Then
    OnChange(Self);
End;

Procedure UIScrollBar.Slide(Ammount: Single);
Begin
  Value := Value + Ammount;
  If Value<0 Then
    Value := 0
  Else
  If (Value>Max) Then
    Value := Max;

  If Assigned(OnChange) Then
    OnChange(Self);
End;

{ UIScrollBarHandle }
Procedure UIScrollBarHandle.Render;
Var
  P:Vector2D;
Begin
  If (UIScrollBar(_Parent)._Kind = scrollHorizontal) Then
  Begin
    P.Y := UIScrollBar(_Parent)._HandleOffset.Y;

    If (RelativePosition.X < UIScrollBar(_Parent)._HandleMin.X) Then
      P.X := UIScrollBar(_Parent)._HandleMin.X
    Else
    If (RelativePosition.X > UIScrollBar(_Parent)._HandleMax.X) Then
      P.X := UIScrollBar(_Parent)._HandleMax.X;

    Self.SetRelativePosition(P);
  End Else
  Begin
    P.X := UIScrollBar(_Parent)._HandleOffset.X;

    If (RelativePosition.Y < UIScrollBar(_Parent)._HandleMin.Y) Then
      P.Y := UIScrollBar(_Parent)._HandleMin.Y
    Else
    If (RelativePosition.Y > UIScrollBar(_Parent)._HandleMax.Y) Then
      P.Y := UIScrollBar(_Parent)._HandleMax.Y;

    Self.SetRelativePosition(P);
  End;

  Inherited;

End;

End.