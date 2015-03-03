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
 * TERRA_Input
 * Implements input classes and support (keyboard, mouse, touch, gamepad, etc)
 ***********************************************************************************************************************
}
Unit TERRA_Input;

{$I terra.inc}

Interface

Const
  keyGamepadIndex   = 255;
	keyGamepadLeft    = keyGamepadIndex + 0;
	keyGamepadUp      = keyGamepadIndex + 1;
	keyGamepadRight   = keyGamepadIndex + 2;
	keyGamepadDown    = keyGamepadIndex + 3;
	keyGamepadA       = keyGamepadIndex + 4;
	keyGamepadB       = keyGamepadIndex + 5;
	keyGamepadC       = keyGamepadIndex + 6;
	keyGamepadD       = keyGamepadIndex + 7;
	keyGamepadX       = keyGamepadIndex + 8;
	keyGamepadY       = keyGamepadIndex + 9;
	keyGamepadZ       = keyGamepadIndex + 10;
	keyGamepadR       = keyGamepadIndex + 11;
	keyGamepadL       = keyGamepadIndex + 12;
  keyGamepadMenu    = keyGamepadIndex + 13;
  keyGamepadCount   = 32;

	keyGamepadLeft2    = keyGamepadLeft + keyGamepadCount * 1;
	keyGamepadUp2      = keyGamepadUp + keyGamepadCount * 1;
	keyGamepadRight2   = keyGamepadRight + keyGamepadCount * 1;
	keyGamepadDown2    = keyGamepadDown + keyGamepadCount * 1;
	keyGamepadA2       = keyGamepadA + keyGamepadCount * 1;
	keyGamepadB2       = keyGamepadB + keyGamepadCount * 1;
	keyGamepadC2       = keyGamepadC + keyGamepadCount * 1;
	keyGamepadD2       = keyGamepadD + keyGamepadCount * 1;
	keyGamepadX2       = keyGamepadX + keyGamepadCount * 1;
	keyGamepadY2       = keyGamepadY + keyGamepadCount * 1;
	keyGamepadZ2       = keyGamepadZ + keyGamepadCount * 1;
	keyGamepadR2       = keyGamepadR + keyGamepadCount * 1;
	keyGamepadL2       = keyGamepadL + keyGamepadCount * 1;
  keyGamepadMenu2    = keyGamepadMenu + keyGamepadCount * 1;

	keyGamepadLeft3    = keyGamepadLeft + keyGamepadCount * 2;
	keyGamepadUp3      = keyGamepadUp + keyGamepadCount * 2;
	keyGamepadRight3   = keyGamepadRight + keyGamepadCount * 2;
	keyGamepadDown3    = keyGamepadDown + keyGamepadCount * 2;
	keyGamepadA3       = keyGamepadA + keyGamepadCount * 2;
	keyGamepadB3       = keyGamepadB + keyGamepadCount * 2;
	keyGamepadC3       = keyGamepadC + keyGamepadCount * 2;
	keyGamepadD3       = keyGamepadD + keyGamepadCount * 2;
	keyGamepadX3       = keyGamepadX + keyGamepadCount * 2;
	keyGamepadY3       = keyGamepadY + keyGamepadCount * 2;
	keyGamepadZ3       = keyGamepadZ + keyGamepadCount * 2;
	keyGamepadR3       = keyGamepadR + keyGamepadCount * 2;
	keyGamepadL3       = keyGamepadL + keyGamepadCount * 2;
  keyGamepadMenu3    = keyGamepadMenu + keyGamepadCount * 2;

	keyGamepadLeft4    = keyGamepadLeft + keyGamepadCount * 3;
	keyGamepadUp4      = keyGamepadUp + keyGamepadCount * 3;
	keyGamepadRight4   = keyGamepadRight + keyGamepadCount * 3;
	keyGamepadDown4    = keyGamepadDown + keyGamepadCount * 3;
	keyGamepadA4       = keyGamepadA + keyGamepadCount * 3;
	keyGamepadB4       = keyGamepadB + keyGamepadCount * 3;
	keyGamepadC4       = keyGamepadC + keyGamepadCount * 3;
	keyGamepadD4       = keyGamepadD + keyGamepadCount * 3;
	keyGamepadX4       = keyGamepadX + keyGamepadCount * 3;
	keyGamepadY4       = keyGamepadY + keyGamepadCount * 3;
	keyGamepadZ4       = keyGamepadZ + keyGamepadCount * 3;
	keyGamepadR4       = keyGamepadR + keyGamepadCount * 3;
	keyGamepadL4       = keyGamepadL + keyGamepadCount * 3;
  keyGamepadMenu4    = keyGamepadMenu + keyGamepadCount * 3;

  MaxGamePads = 4;

  keyMouseLeft   = keyGamepadIndex + 1 + keyGamepadCount * MaxGamePads;
  keyMouseRight  = keyGamepadIndex + 2 + keyGamepadCount * MaxGamePads;
  keyMouseMiddle = keyGamepadIndex + 3 + keyGamepadCount * MaxGamePads;

Const
  MaxKeys = 512;
  MaximumFrameDelay = 10;

Type
  KeyState = Record
    Frame:Cardinal;
    State:Boolean;
    Pressed:Boolean;
    Released:Boolean;
  End;

	InputState = Class
    Protected
      _Keys:Array[0..Pred(MaxKeys)] Of KeyState;

    Public
      Function IsDown(Key:Word):Boolean;
      Function IsUp(Key:Word):Boolean;

      Function WasPressed(Key:Word):Boolean;
      Function WasReleased(Key:Word):Boolean;

      Function SetState(Key:Word; Value:Boolean):Boolean;

      Procedure Reset();
  End;

Implementation
Uses TERRA_GraphicsManager;

{ InputState }
Function InputState.SetState(Key: Word; Value: Boolean):Boolean;
Begin
  Result := False;

  If (Key>=MaxKeys) Then
    Exit;

  If (_Keys[Key].State = Value) Then
    Exit;

  _Keys[Key].State := Value;

  If Value Then
    _Keys[Key].Pressed := True
  Else
    _Keys[Key].Released := True;

  _Keys[Key].Frame := GraphicsManager.Instance.FrameID;

  Result := True;
End;

Function InputState.IsDown(Key: Word): Boolean;
Begin
  If (Key>=MaxKeys) Then
    Result := False
  Else
    Result := _Keys[Key].State;
End;

Function InputState.IsUp(Key: Word): Boolean;
Begin
  If (Key>=MaxKeys) Then
    Result := True
  Else
    Result := Not _Keys[Key].State;
End;

Function InputState.WasPressed(Key: Word): Boolean;
Var
  Delta:Integer;
Begin
  If (Key>=MaxKeys) Then
    Result := False
  Else
  Begin
    Result := (_Keys[Key].Pressed);

    If Result Then
    Begin
      Delta := GraphicsManager.Instance.FrameID - _Keys[Key].Frame;
      Result := (Delta<=MaximumFrameDelay);
      _Keys[Key].Pressed := False;
    End;
  End;
End;

Function InputState.WasReleased(Key: Word): Boolean;
Var
  Delta:Integer;
Begin
  If (Key>=MaxKeys) Then
    Result := False
  Else
  Begin
    Result := (_Keys[Key].Released);

    If Result Then
    Begin
      Delta := GraphicsManager.Instance.FrameID - _Keys[Key].Frame;
      Result := (Delta<=MaximumFrameDelay);
      _Keys[Key].Released := False;
    End;
  End;
End;

Procedure InputState.Reset;
Begin
  FillChar(_Keys[0], SizeOf(_Keys), 0);
End;

End.