{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_UIGrid
 * Implements a grid widget, for displaying scrollable arrays of widgets
 ***********************************************************************************************************************
}

Unit TERRA_UIGrid;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_UI, TERRA_Tween, TERRA_Vector2D;

Type
  UIGrid = Class;

  UIGridElement = Class(TERRAObject)
    Protected
      _Grid:UIGrid;
      _OnScreen:Boolean;
      _Visible:Boolean;

    Public

      Procedure SetVisible(Const Value:Boolean);

      Procedure Update(W:Widget; X,Y:Integer); Virtual; Abstract;

      Property Visible:Boolean Read _Visible Write SetVisible;
  End;

  GridVisitor = Function (Element:UIGridElement; UserData:Pointer): Boolean; Cdecl;

  UIGrid = Class(Widget)
    Protected
      _GridX:Integer;
      _GridY:Integer;

      _CurrentColumn:Integer;
      _CurrentRow:Integer;
      _ColumnShift:Integer;
      _RowShift:Integer;
      _ColumnChange:Integer;
      _RowChange:Integer;

      _ShiftCol:Single;
      _OfsX:Single;
      _OfsY:Single;
      _SpaceX:Single;
      _SpaceY:Single;
      _CellWidth:Integer;
      _CellHeight:Integer;
      _ShiftLeft:Boolean;
      _ShiftRight:Boolean;

      _NeedsUpdate:Boolean;
      _WantHighlight:Integer;

      _VisibleColumns:Integer;

      _Temp:Widget;

      _Elements:Array Of UIGridElement;
      _ElementCount:Integer;

      Function CreateGridCell(UI:UI; Parent:Widget; Name:TERRAString):Widget; Virtual; Abstract;

      Procedure ResetPositions();

      Function GetWidgetOfs(I,J:Integer):Integer;


      Procedure OnHighlight(Prev:Widget); Override;

    Private

      Procedure UpdateGrid();

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; GridX, GridY:Integer; Const Width, Height:UIDimension; Const ComponentName:TERRAString; SpaceX:Single = 30; SpaceY:Single = 30);
      Procedure Release; Override;

      Procedure MoveRight(Columns:Integer);
      Procedure MoveLeft(Columns:Integer);

			Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;
			Function OnMouseUp(X,Y:Integer;Button:Word):Boolean; Override;
			Function OnMouseMove(X,Y:Integer):Boolean; Override;

      Procedure Render(); Override;

      Procedure ClearElements();

      Function GetRowCount():Integer;
      Function GetColumnCount():Integer;

      Function GetVisibleElementCount():Integer;
      Function GetPageCount():Integer;

      Procedure OnElementSelect(Element:UIGridElement); Virtual;
      Procedure OnElementHold(Element:UIGridElement); Virtual;

      Function GetColumnOf(W:Widget):Integer;
      Function GetRowOf(W:Widget):Integer;
      Procedure GetIndicesOf(W:Widget; Out X,Y:Integer);

      Function HasScroll():Boolean;

      Procedure Refresh();

      Procedure Filter(Visitor:GridVisitor; UserData:Pointer);

      Procedure AddElement(Element:UIGridElement);
      Function GetElement(X,Y:Integer):UIGridElement;

      Property GridColumns:Integer Read _GridX;
      Property GridRows:Integer Read _GridY;

      Property ElementCount:Integer Read _ElementCount;

      Property CurrentColumn:Integer Read _CurrentColumn Write _CurrentColumn;
  End;

Implementation

Function GridClickHandler(W:Widget):Boolean; Cdecl;
Var
  X,Y:Integer;
  Grid:UIGrid;
  Element:UIGridElement;
Begin
  Result := True;
  Grid := UIGrid(W.Parent);
  Grid.GetIndicesOf(W, X, Y);
  Element := Grid.GetElement(X, Y);
  Grid.OnElementSelect(Element);
End;

Function GridHoldHandler(W:Widget):Boolean; Cdecl;
Var
  X,Y:Integer;
  Grid:UIGrid;
  Element:UIGridElement;
Begin
  Result := True;
  Grid := UIGrid(W.Parent);
  Grid.GetIndicesOf(W, X, Y);
  Element := Grid.GetElement(X, Y);
  Grid.OnElementHold(Element);
End;

{ UIGrid }
Constructor UIGrid.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; GridX, GridY:Integer; Const Width, Height:UIDimension; Const ComponentName:TERRAString; SpaceX:Single = 30; SpaceY:Single = 30);
Var
  I,J:Integer;
  WndTotX, WndTotY:Integer;
  W:Widget;
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self._Position := VectorCreate2D(X,Y);

  Self._GridX := GridX;
  Self._GridY := GridY;

  Self._Layer := Z;
  Self._Width := Width;
  Self._Height := Height;

  Self.UpdateRects();

  _Temp := Self.CreateGridCell(UI, Self, 'temp');

  _WantHighlight := 0;

{  _MinX := -GridX;
  _MaxX := GridX + GridX;

  _MinY := -GridY;
  _MaxY := GridY + GridY;}

  _CellWidth := Trunc(_Temp.Size.X);
  _CellHeight := Trunc(_Temp.Size.Y);

  _SpaceX := SpaceX;
  _SpaceY := SpaceY;

  WndTotX := Trunc(_CellWidth * _GridX + _SpaceX * Pred(_GridX));
  WndTotY := Trunc(_CellHeight * _GridY + _SpaceY * Pred(_GridY));
  _OfsX :=(Self.GetDimension(_Width) - WndTotX) / 2;
  _OfsY := (Self.GetDimension(_Height) - WndTotY) / 2;
  _ShiftCol := (_CellWidth+_SpaceX);
  _Temp.Visible := False;

  _ChildrenCount := _GridX*3 * _GridY;
  SetLength(_ChildrenList, _ChildrenCount);

  For J:=0 To Pred(_GridY) Do
  For I:=0 To Pred(_GridX*3) Do
  Begin
    W := Self.CreateGridCell(UI, Self, '#'+IntToString(I)+'_'+IntToString(J));
    If (Assigned(W)) Then
    Begin
      W.Parent := Self;
      W.Visible := False;
      W.OnMouseClick := GridClickHandler;
      W.OnExtendedClick := GridHoldHandler;
      _ChildrenList[GetWidgetOfs(I,J)] := W;
    End;
  End;

  _NeedsUpdate := True;
  ResetPositions();
End;

Function UIGrid.GetWidgetOfs(I, J: Integer): Integer;
Begin
  Result := I + J  * (_GridX*3)
End;

Procedure UIGrid.OnElementSelect(Element: UIGridElement);
Begin
  // do nothing
End;

Procedure UIGrid.OnElementHold(Element: UIGridElement);
Begin
  // do nothing
End;

Function UIGrid.GetVisibleElementCount():Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_ElementCount) Do
  If (_Elements[I].Visible) Then
    Inc(Result);
End;

Function UIGrid.GetColumnCount: Integer;
Begin
  Result := Self.GetVisibleElementCount Div _GridY;
End;

Function UIGrid.GetRowCount: Integer;
Begin
  Result := _GridY;
End;

Function UIGrid.GetPageCount():Integer;
Begin
  Result := Self.GetVisibleElementCount();
  Result := Succ(Result Div (_GridX*_GridY));
End;


Procedure UIGrid.ResetPositions();
Var
  I,J:Integer;
  X,Y:Single;
  W:Widget;
Begin
  For J:=0 To Pred(_GridY) Do
  For I:=0 To Pred(_GridX*3) Do
  Begin
    W := _ChildrenList[GetWidgetOfs(I,J)];
    If (W<>Nil) Then
    Begin
      X := _OfsX + (_CellWidth+_SpaceX) * (I - _GridX);
      Y := _OfsY + (_CellHeight+_SpaceY) * J;
      TweenManager.Instance.RemoveTween(W);
      W.Position := VectorCreate2D(X,Y);
    End;
  End;
End;

Procedure ShiftColumnsLeft(P:Pointer); Cdecl;
Begin
  UIGrid(P)._ShiftLeft := True;
End;

Procedure ShiftColumnsRight(P:Pointer); Cdecl;
Begin
  UIGrid(P)._ShiftRight := True;
End;

Procedure UIGrid.MoveLeft(Columns:Integer);
Var
  I,J:Integer;
  T:Tween;
  W:Widget;
Begin
  _ColumnShift := Columns;
  _ColumnChange := _ColumnShift;

  T := Nil;
  
  For J:=0 To Pred(_GridY) Do
  For I:=0 To Pred(_GridX*3) Do
  Begin
    W := _ChildrenList[GetWidgetOfs(I,J)];
    If Assigned(W) Then
    Begin
      T := W.AddTween(wtPositionX, W.Position.X + _ShiftCol * _ColumnShift, 500);
    End;
  End;

  If Assigned(T) Then
  Begin
  	T.OnFinished := ShiftColumnsRight;
  	T.UserData := Self;
  End;
End;

Procedure UIGrid.MoveRight(Columns:Integer);
Var
  I,J:Integer;
  T:Tween;
  W:Widget;
Begin
  _ColumnShift := Columns;
  _ColumnChange := _ColumnShift;

  T := Nil;
	
  For J:=0 To Pred(_GridY) Do
  For I:=0 To Pred(_GridX*3) Do
  Begin
    W := _ChildrenList[GetWidgetOfs(I,J)];
    If Assigned(W) Then
    Begin
      T := W.AddTween(wtPositionX, W.Position.X - _ShiftCol * _ColumnShift, 500);
    End;
  End;

	If Assigned(T) Then
	Begin
  		T.OnFinished := ShiftColumnsLeft;
 	 	T.UserData := Self;
 	 End;
End;


Procedure UIGrid.AddElement(Element: UIGridElement);
Begin
  Inc(_ElementCount);
  SetLength(_Elements, _ElementCount);
  Element._Grid := Self;
  _Elements[Pred(_ElementCount)] := Element;
End;

Function UIGrid.GetElement(X,Y:Integer):UIGridElement;
Var
  CX, CY, Count:Integer;
  Max, Index, Off:Integer;
  Overflow:Boolean;
Begin
  If (X=0) And (Y=3) Then
    IntToString(2);

  Result := Nil;
  If (_ElementCount<=0) Then
    Exit;

  Max := Self.GetColumnCount();
  X := (_CurrentColumn+X);
  If (X < 0 ) Then
    X := X + Succ(Max);

  If (Y<0) Or (Y>=_GridY) Then
    Exit;

  Index := 0;
  If (X=0) And (Y=0) Then
  Begin
    While Not (_Elements[Index].Visible) Do
    Begin
      Inc(Index);
      If (Index>=_ElementCount) Then
        Exit;
    End;

    If (Index<_ElementCount) Then
      Result := _Elements[Index];
    Exit;
  End;

  CX := 0;
  CY := 0;
  Off := 0;
  Count := 0;
  Overflow := False;
  While (Not ((CX=X) And (CY=Y))) Do
  Begin
    Inc(Count);
    If (Count>_ElementCount*2) Then
      Exit;
      
    If (_Elements[Index].Visible) Then
    Begin
      If (CX=Max) And (Overflow) Then
        Inc(Off);

      Inc(CY);
      If (CY>=_GridY) Then
      Begin
        CY := 0;
        Inc(CX);
      End;
    End;

    Inc(Index);
    If (Index>=_ElementCount) Then
    Begin
      Index := 0;
      Overflow := True;
    End;

    If (Not ((CX=Max) And (Overflow))) Then
      If (Off>0) Then
      Begin
        While Off>0 Do
        Begin
          Dec(Index);
          If (Index<0) Then
            Exit;

          If (_Elements[Index].Visible) Then
            Dec(Off);
        End;
        Off := 0;
      End;

  End;

  If (X=Max) And (Overflow) Then
    Exit;

  While (Index<_ElementCount) And (Not _Elements[Index].Visible) Do
    Inc(Index);


  If (Index<_ElementCount) Then
    Result := _Elements[Index];
End;

Procedure UIGrid.Filter(Visitor: GridVisitor; UserData:Pointer);
Var
  I:Integer;
Begin
  For I :=0 To Pred(_ElementCount) Do
    _Elements[I].Visible := Visitor(_Elements[I], UserData);
End;

Function UIGrid.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Var
  I,J:Integer;
  W:Widget;
Begin
  Result := False;
  For J:=0 To Pred(_GridY) Do
    For I:=0 To Pred(_GridX*3) Do
    Begin
      W := _ChildrenList[GetWidgetOfs(I,J)];

      If Assigned(W) Then
      Begin
        Result := W.OnMouseDown(X,Y, Button);
        If Result Then
          Exit;
      End;
    End;
End;

Function UIGrid.OnMouseMove(X, Y: Integer): Boolean;
Var
  I,J:Integer;
  W:Widget;
Begin
  Result := False;
  For J:=0 To Pred(_GridY) Do
    For I:=0 To Pred(_GridX*3) Do
    Begin
      W := _ChildrenList[GetWidgetOfs(I,J)];
      If Assigned(W) Then
      Begin
        Result := W.OnMouseMove(X,Y);
        If Result Then
          Exit;
      End;
    End;
End;

Function UIGrid.OnMouseUp(X, Y: Integer; Button: Word): Boolean;
Var
  I,J:Integer;
  W:Widget;
Begin
  Result := False;
  For J:=0 To Pred(_GridY) Do
    For I:=0 To Pred(_GridX*3) Do
    Begin
      W := _ChildrenList[GetWidgetOfs(I,J)];
      If Assigned(W) Then
      Begin
        Result := W.OnMouseUp(X,Y, Button);
        If Result Then
          Exit;
      End;
    End;
End;

Procedure UIGrid.Render;
Begin
  Self.UpdateRects();
  Self.UpdateTransform();
  Self.UpdateHighlight();

  If (_ShiftLeft) Then
  Begin
    _ShiftLeft := False;
    ResetPositions();
    _CurrentColumn := _CurrentColumn + _ColumnChange;
    If (_CurrentColumn>Self.GetColumnCount()) Then
      _CurrentColumn := 0;
    _NeedsUpdate := True;
  End;

  If (_ShiftRight) Then
  Begin
    _ShiftRight := False;
    ResetPositions();
    _CurrentColumn := _CurrentColumn - _ColumnChange;
    If (_CurrentColumn<0) Then
      _CurrentColumn := _CurrentColumn + Succ(Self.GetColumnCount());
    _NeedsUpdate := True;
  End;

//  If (Self._NeedsUpdate) Then
    Self.UpdateGrid();

  Inherited;
End;

Procedure UIGrid.UpdateGrid();
Procedure DoColumn(I:Integer);
Var
  J, Count:Integer;
  P:UIGridElement;
  W:Widget;
Begin
  Count := 0;
  For J:=0 To Pred(_GridY) Do
    Begin
      W := _ChildrenList[GetWidgetOfs(I,J)];
      If Assigned(W) Then
      Begin
        P := GetElement(I-_GridX, J);

        If (P = Nil) Or (P._OnScreen) Then
          W.Visible := False
        Else
        Begin
{          If (I>_GridX) And (I<_GridX*2) Then
            W.LeftControl := _ChildrenList[GetWidgetOfs(Pred(I),J)]
          Else
            W.LeftControl := Self.LeftControl;

          If (I>=_GridX) And (I<Pred(_GridX*2)) Then
            W.RightControl := _ChildrenList[GetWidgetOfs(Succ(I),J)]
          Else
            W.RightControl := Self.RightControl;

          If (J>0) Then
            W.UpControl := _ChildrenList[GetWidgetOfs(I,Pred(J))]
          Else
            W.UpControl := Self.UpControl;

          If (J<Pred(_GridY)) Then
            W.DownControl := _ChildrenList[GetWidgetOfs(I,Succ(J))]
          Else
            W.DownControl := Self.DownControl;}

          W.Visible := True;
          P.Update(W, I, J);
          P._OnScreen := True;

          Inc(Count);

          W.Render();

          If (_WantHighlight<>0) Then
          Begin
            UI.Highlight := W;
            If (_WantHighlight<0) Then
              _WantHighlight := 0;
          End;
        End;
      End;
    End;

{  For J:=0 To Pred(_GridY) Do
  Begin
    W := _ChildrenList[GetWidgetOfs(I,J)];

    If (Assigned(W)) And (Assigned(W.DownControl)) And (Not W.DownControl.Visible) Then
    Begin
      W.DownControl := Self.DownControl;
    End;

    If (Assigned(W)) And (Assigned(W.UpControl)) And (Not W.UpControl.Visible) Then
    Begin
      W.UpControl := Self.UpControl;
    End;

    If (Assigned(W)) And (Assigned(W.LeftControl)) And (Not W.LeftControl.Visible) Then
    Begin
      W.LeftControl := Self.LeftControl;
    End;

    If (Assigned(W)) And (Assigned(W.RightControl)) And (Not W.RightControl.Visible) Then
    Begin
      W.RightControl := Self.RightControl;
    End;
  End;}

  _WantHighlight := 0;

  If (Count>0) Then
    Inc(_VisibleColumns);
End;

Var
  I:Integer;
Begin
  _NeedsUpdate := False;
  _VisibleColumns := 0;

  For I:=0 To Pred(_ElementCount) Do
    _Elements[I]._OnScreen := False;

  For I:=_GridX To Pred(_GridX*3) Do
    DoColumn(I);

  For I:=0 To Pred(_GridX) Do
    DoColumn(I);
End;

Procedure UIGrid.Refresh;
Begin
  Self._NeedsUpdate := True;
End;

Procedure UIGrid.OnHighlight(Prev: Widget);
Begin
  If (Prev=Nil) Or (Prev.Position.X<Self.Position.X) Then
    _WantHighlight := -1
  Else
    _WantHighlight := 1;

  UI.Highlight := GetChild(0);
End;

Function UIGrid.HasScroll: Boolean;
Begin
  Result := Self._VisibleColumns>Self._GridX;
End;

Procedure UIGrid.GetIndicesOf(W: Widget; Out X, Y: Integer);
Var
  S, S2:TERRAString;
Begin
  S := W.Name;
  S2 := StringGetNextSplit(S, Ord('#'));
  S2 := StringGetNextSplit(S, Ord('_'));
  X := StringToInt(S2);
  Y := StringToInt(S);
  Dec(X, _GridX);
End;

Function UIGrid.GetColumnOf(W: Widget):Integer;
Var
  A,B:Integer;
Begin
  Self.GetIndicesOf(W, A, B);
  Result := A;
End;

Function UIGrid.GetRowOf(W: Widget): Integer;
Var
  A,B:Integer;
Begin
  Self.GetIndicesOf(W, A, B);
  Result := B;
End;

Procedure UIGrid.ClearElements;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ElementCount) Do
    ReleaseObject(_Elements[I]);

  _ElementCount := 0;
End;

Procedure UIGrid.Release;
Begin
  Self.ClearElements();

  Inherited;
End;

{ UIGridElement }
Procedure UIGridElement.SetVisible(const Value: Boolean);
Begin
  If (Value = _Visible) Then
    Exit;

  _Visible := Value;
  _Grid._NeedsUpdate := True;
End;

End.