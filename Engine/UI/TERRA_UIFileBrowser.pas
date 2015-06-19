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
Uses TERRA_String, TERRA_Utils, TERRA_UI, TERRA_FileSearch, TERRA_Collections,
  TERRA_Vector2D, TERRA_Vector3D, TERRA_Color, TERRA_SpriteManager, TERRA_ClipRect,
  TERRA_UISkin, TERRA_UIWindow, TERRA_UIScrollbar;

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
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString);

      Procedure Render; Override;

      Procedure SetBorders(TopBorder, BottomBorder:Integer);

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;
			Function OnMouseUp(X,Y:Integer;Button:Word):Boolean; Override;

      Procedure SetFilter(Filter:TERRAString);
      Procedure SetFolder(Folder:TERRAString);
  End;

Implementation
Uses TERRA_Error;

{ UIFileBrowser }
Constructor UIFileBrowser.Create(Name:TERRAString; Parent:Widget; X, Y, Z: Single; Const Width, Height:UIDimension; Const ComponentName:TERRAString);
Begin
  Inherited Create(Name, Parent, X, Y, Z, Width, Height, ComponentName);

  _BorderX := 20;
  _BorderY := 20;

  _TopBorder := 0;
  _BottomBorder := 0;

{  _IconFolder := Self.LoadComponent('ui_folder');
  _IconFile := Self.LoadComponent('ui_file');}

  _ListSubFolders := True;

  _InnerWnd := UIWindow.Create(Name+'_inner', Self, 0, 0, 0.5, UIPixels(100), UIPixels(100), ComponentName);

  _Scroll := UIScrollbar.Create(Name+'_scroll', Self, 0, 0, 1.0, UIPixels(40), Height, UIPixels(20), UIPixels(20), ComponentName);

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
    _SelectedItem := 5; //Trunc(YY / _InnerWnd.Layout.GCSY(1));
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
  HH:Single;
  It:CollectionObject;
  Info:FileInfo;
  TextRect:Vector2D;
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

  _OfsY := -_Scroll.Value;

  HH := 20;

  PY := 0;
  If Assigned(_FileList) Then
  Begin
    It := _FileList.First;
    While Assigned(It) Do
    Begin
      Info := FileInfo(It);
      If (PY = Self._SelectedItem) Then
      Begin
      End;

      _InnerWnd.ClipRect := _ClipRect;
      TextRect := Self.FontRenderer.GetTextRect(Info.Name);
      _InnerWnd.DrawText(Info.Name, 5, _OfsY + HH * PY, 1.0, TextRect, ColorWhite, 1.0);
      _InnerWnd.ClipRect.Style := clipNothing;

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
  _InnerWnd.Width := UIPixels(TargetWidth);

  TargetHeight := (Self.Size.Y - (_BorderY*2 + _TopBorder + _BottomBorder));
  _InnerWnd.Height := UIPixels(TargetHeight);

  _Scroll.SetHeight(UIPixels(TargetHeight));

  _InnerWnd.Position := VectorCreate2D(0, _TopBorder + _BorderY);
  _InnerWnd.Align := waTopCenter;

  _Scroll.Position := VectorCreate2D(_BorderX, _TopBorder + _BorderY);
  _Scroll.Align := waTopRight;
End;

Procedure UIFileBrowser.UpdateContent;
Var
  VisibleItems:Integer;
Begin
  ReleaseObject(_FileList);
  ReleaseObject(_FolderList);

  If (_Folder = '') Or (_Filter = '') Then
    Exit;

  _FileList := SearchFiles(_Folder, _Filter, False);
  _FolderList := SearchFolders(_Folder);

  VisibleItems := 5; //1 + Trunc(_InnerWnd.Size.Y / _InnerWnd.Layout.GCSY(1));

  _Scroll.Max := _FileList.Count - VisibleItems;
  _Scroll.Value := 0;
  _Scroll.Visible := (_Scroll.Max>0);

  _SelectedItem := -1;
End;


End.