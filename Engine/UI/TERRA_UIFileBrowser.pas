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
 * TERRA_FileBrowser
 * Implements a file browser widget
 ***********************************************************************************************************************
}
Unit TERRA_UIFileBrowser;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_UI, TERRA_Widgets, TERRA_FileSearch, TERRA_Collections, 
  TERRA_Vector2D, TERRA_Vector3D, TERRA_Color, TERRA_SpriteManager;

Type
  UIFileBrowser = Class(UIWindow)
    Protected
      _InnerWnd:UIWindow;
      _Scroll:UIScrollbar;

      _ClipRect:ClipRect;

      _OfsY:Single;

      _BorderX:Single;
      _BorderY:Single;

      _TopBorder:Single;
      _BottomBorder:Single;

      _ExtraWndBorder:Single;
      _ExtraScrollBorder:Single;

      _IconFolder:Integer;
      _IconFile:Integer;

      _ListSubFolders:Boolean;
      _SourceChanged:Boolean;
      _Folder:TERRAString;
      _Filter:TERRAString;
      _FileList:List;
      _FolderList:List;

      _SelectedItem:Integer;

      Procedure UpdateWorkArea();
      Procedure UpdateContent(); Virtual;

      Procedure CreateButtons(); Virtual;

    Public

      Constructor Create(Name:TERRAString; UI:UI; X,Y,Z:Single; Width, Height:Integer; ComponentBG:TERRAString=''); Overload;

      Procedure Render; Override;

      Procedure SetBorders(TopBorder, BottomBorder:Integer);

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;
			Function OnMouseUp(X,Y:Integer;Button:Word):Boolean; Override;

      Procedure SetFilter(Filter:TERRAString);
      Procedure SetFolder(Folder:TERRAString);
  End;

Implementation

{ UIFileBrowser }
Constructor UIFileBrowser.Create(Name:TERRAString; UI:UI; X, Y, Z: Single; Width, Height: Integer; ComponentBG:TERRAString);
Begin
  Inherited Create(Name, UI, X, Y, Z, Width, Height, ComponentBG);

  _BorderX := 20;
  _BorderY := 20;

  _TopBorder := 0;
  _BottomBorder := 0;

  _IconFolder := Self.LoadComponent('ui_folder');
  _IconFile := Self.LoadComponent('ui_file');

  _ListSubFolders := True;

  _ClipRect := TERRA_SpriteManager.ClipRect.Create();

  _InnerWnd := UIWindow.Create(Name+'_inner', UI, Self, 0, 0 , 0.5, 1, 1, 'ui_list');

  _Scroll := UIScrollbar.Create(Name+'_scroll', UI, Self, 0, 0, 1.0, 5, False);

  Self.AddChild(_InnerWnd);

  Self.CreateButtons();

  Self.UpdateWorkArea();
End;

Procedure UIFileBrowser.CreateButtons;
Begin
End;

Function UIFileBrowser.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Var
  Pos:Vector2D;
  YY:Single;
Begin
  Result := _Scroll.OnMouseDown(X, Y, Button);
  If Result Then
    Exit;

  Result := _InnerWnd.OnMouseDown(X, Y, Button);
  If Result Then
  Begin
    Pos := _InnerWnd.GetAbsolutePosition();
    YY := Y - Pos.Y - _OfsY;
    _SelectedItem := Trunc(YY / _InnerWnd.Layout.GCSY(1));
    Exit;
  End;

  Result := False;
End;

Function UIFileBrowser.OnMouseUp(X, Y: Integer; Button: Word): Boolean;
Begin
  Result := Inherited OnMouseUp(X,Y, Button);
End;

Procedure UIFileBrowser.Render;
Var
  I,PY:Integer;
  It:ListObject;
  Info:FileInfo;
Begin
  If (_SourceChanged) Then
  Begin
    _SourceChanged := False;
    Self.UpdateContent();
  End;

  Inherited;

  _ClipRect.X := _InnerWnd.Position.X;
  _ClipRect.Y := _InnerWnd.Position.Y;
  _ClipRect.Width := _InnerWnd.Size.X;
  _ClipRect.Height := _InnerWnd.Size.Y;

  _OfsY := -_Scroll.Value * _InnerWnd.Layout.GCSY(1);

  PY := 0;
  If Assigned(_FileList) Then
  Begin
    It := _FileList.First;
    While Assigned(It) Do
    Begin
      Info := FileInfo(It);
      If (PY = Self._SelectedItem) Then
      Begin
        For I:=0 To _InnerWnd.Width Do
          _InnerWnd.DrawComponent(0, VectorCreate(_InnerWnd.Layout.GCSX(1)*Pred(I), _OfsY + _InnerWnd.Layout.GCSY(1)*Pred(PY), 0.75), _InnerWnd.Layout.X[1], _InnerWnd.Layout.Y[1], _InnerWnd.Layout.X[2], _InnerWnd.Layout.Y[2], ColorBlack);
      End;

      _InnerWnd.ClipRect := _ClipRect;
      _InnerWnd.DrawText(Info.Name, VectorCreate(5, _OfsY + _InnerWnd.Layout.GCSY(1) * PY, 1.0), ColorWhite, 1.0);
      _InnerWnd.ClipRect := Nil;

      Inc(PY);
      It := It.Next;
    End;
  End;

End;

Procedure UIFileBrowser.SetBorders(TopBorder, BottomBorder: Integer);
Begin
  _TopBorder := TopBorder;
  _BottomBorder := BottomBorder;
  Self.UpdateWorkArea();
End;

Procedure UIFileBrowser.SetFilter(Filter:TERRAString);
Begin
  If (Filter = _Filter) Then
    Exit;

  Self._Filter := Filter;
  Self._SourceChanged := True;
End;

Procedure UIFileBrowser.SetFolder(Folder:TERRAString);
Begin
  If (Folder = _Folder) Then
    Exit;

  Self._Folder := Folder;
  Self._SourceChanged := True;
End;

Procedure UIFileBrowser.UpdateWorkArea;
Var
  TargetWidth, TargetHeight:Single;
Begin
  TargetWidth := (Self.Size.X-_BorderX*2);
  _InnerWnd.Width := 0;
  Repeat
    _InnerWnd.Width := _InnerWnd.Width + 1;
  Until (_InnerWnd.Size.X > TargetWidth);
  _InnerWnd.Width := _InnerWnd.Width - 1;

  TargetHeight := (Self.Size.Y - (_BorderY*2 + _TopBorder + _BottomBorder));
  _InnerWnd.Height := 0;
  Repeat
    _InnerWnd.Height := _InnerWnd.Height + 1;
  Until (_InnerWnd.Size.Y>TargetHeight);
  _InnerWnd.Height := _InnerWnd.Height - 1;
  _ExtraWndBorder := TargetHeight - _InnerWnd.Size.Y;

  _Scroll.ScrollSize := 0;
  Repeat
    _Scroll.ScrollSize := _Scroll.ScrollSize + 1;
  Until (_Scroll.Size.Y>TargetHeight);
  _Scroll.ScrollSize := _Scroll.ScrollSize - 1;
  _ExtraScrollBorder := TargetHeight - _Scroll.Size.Y;

  _InnerWnd.Position := VectorCreate2D(0, _TopBorder + _BorderY + _ExtraWndBorder * 0.5);
  _InnerWnd.Align := waTopCenter;

  _Scroll.Position := VectorCreate2D(_BorderX, _TopBorder + _BorderY + _ExtraScrollBorder * 0.5);
  _Scroll.Align := waTopRight;
End;

Procedure UIFileBrowser.UpdateContent;
Var
  VisibleItems:Integer;
Begin
  If Assigned(_FileList) Then
  Begin
    _FileList.Destroy;
    _FileList := Nil;
  End;

  If Assigned(_FolderList) Then
  Begin
    _FolderList.Destroy;
    _FolderList := Nil;
  End;

  If (_Folder = '') Or (_Filter = '') Then
    Exit;

  _FileList := SearchFiles(_Folder, _Filter, False);
  _FolderList := SearchFolders(_Folder);

  VisibleItems := 1 + Trunc(_InnerWnd.Size.Y / _InnerWnd.Layout.GCSY(1));

  _Scroll.Max := _FileList.Count - VisibleItems;
  _Scroll.Value := 0;
  _Scroll.Visible := (_Scroll.Max>0);

  _SelectedItem := -1;
End;


End.