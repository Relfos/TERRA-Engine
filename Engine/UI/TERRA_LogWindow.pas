{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_LogWindow
 * Implements a Window widget that displays the current engine log
 ***********************************************************************************************************************
}
Unit TERRA_LogWindow;

Interface
Uses TERRA_Utils, TERRA_Log, TERRA_UI, TERRA_Widgets;

{$I terra.inc}

Procedure ActivateLogWindow;

Implementation
Const
  MaxLines = 16;

Var
  LogLines:Array[0..MaxLines] Of TERRAString;
  LogWnd:UIWindow;
  LogText:UILabel;
  Working:Boolean = False;

Procedure MyLogFilter(Module, Desc:TERRAString);
Var
  N,I:Integer;
  S:TERRAString;
Begin
  If (UI.Instance.DefaultFont=Nil) Or (Working) Then
    Exit;

  N := -1;
  For I:=0 To Pred(MaxLines) Do
  If (LogLines[I]='') Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    For I:=0 To Pred(MaxLines-1) Do
      LogLines[I] := LogLines[I+1];
    N := Pred(MaxLines);
  End;

  LogLines[N] := Module+':'+Desc;
  S := '';
  For I:=0 To Pred(MaxLines) Do
    S := S+LogLines[I]+'\n';

  If (LogWnd=Nil) Then
  Begin
    Working := True;
    LogWnd := UIWindow.Create('logwnd_', 0,0,99, 8, 10);
    LogWnd.AllowDragging := True;
    LogWnd.CenterOnScreen();
    LogText := UILabel.Create('logtext_', LogWnd, 20, 20, 0.1, '');
    Working := False;
  End;

  LogText.Caption := S;
End;

Procedure ActivateLogWindow;
Begin
  Log.Instance.SetFilter(logDebug, MyLogFilter);
End;


Initialization

End.