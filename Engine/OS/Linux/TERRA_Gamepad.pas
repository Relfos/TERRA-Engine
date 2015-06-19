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
 ***********************************************************************************************************************
}

Unit TERRA_Gamepad;

//https://www.kernel.org/doc/Documentation/input/joystick-api.txt
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_InputManager;

Const
     JS_EVENT_BUTTON         = $01;    // button pressed/released
     JS_EVENT_AXIS           = $02;    // joystick moved
     JS_EVENT_INIT           = $80;    // initial state of device

Type
    JSEvent = Packed Record
      time:Cardinal;     // event timestamp in milliseconds
      value:Word;     //value
      kind:Byte;      // event type
      number:Byte;    // axis/button number
    End;

    LinuxGamepad = Class(Gamepad)
      Protected
         _Input:Integer;

         _LastConnect:Integer;

       Public
         Procedure Update(Keys:InputState); Override;
         Procedure Release; Override;
     End;

Implementation
Uses TERRA_Utils, TERRA_OS, baseunix;


{ LinuxGamepad }
Procedure LinuxGamepad.Release;
Begin
  Inherited;

  If (_Input>0) Then
  Begin
    FpClose(_Input);
    _Input := 0;
  End;
End;

Procedure LinuxGamepad.Update(Keys: InputState);
Var
  Event:JSEvent;
  ErrorCode:Integer;
  IsConnected:Boolean;
Begin
  If (_Input <= 0) Then
  Begin
    If (_LastConnect=0) Or (Application.GetTime()-_LastConnect>=1000) Then
    Begin
         _Input := FpOpen('/dev/input/js'+IntToString(_DeviceID), O_RDONLY Or O_NONBLOCK);
         _LastConnect := Application.GetTime();
    End;
  End;

  IsConnected := (_Input>0);

  If (Not IsConnected) Then
  Begin
    Self.Disconnnect();
    Exit;
  End;

  Self.Connnect();


  ErrorCode := FpRead(_Input, Event, SizeOf(Event));
  If ErrorCode<=0 Then
  Begin
    {FpClose(_Input);
    _Input := 0;}
    Exit;
  End;

  If ((Event.Kind And JS_EVENT_INIT)<>0) Then
  Begin
       Event.Kind := Event.Kind  Xor JS_EVENT_INIT;
  End;

  Case Event.Kind Of
  JS_EVENT_BUTTON:
    Begin
         Keys.SetState(GetGamePadKeyValue(LocalID, Event.Number), (Event.Value > 0));
    End;
  End;

  (*Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadUp_Offset), (XState.Buttons And XINPUT_GAMEPAD_DPAD_UP)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadDown_Offset), (XState.Buttons And XINPUT_GAMEPAD_DPAD_DOWN)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadLeft_Offset), (XState.Buttons And XINPUT_GAMEPAD_DPAD_LEFT)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadRight_Offset), (XState.Buttons And XINPUT_GAMEPAD_DPAD_RIGHT)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadMenu_Offset), (XState.Buttons And XINPUT_GAMEPAD_START)<>0);

  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadA_Offset), (XState.Buttons And XINPUT_GAMEPAD_A)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadB_Offset), (XState.Buttons And XINPUT_GAMEPAD_B)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadX_Offset), (XState.Buttons And XINPUT_GAMEPAD_X)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadY_Offset), (XState.Buttons And XINPUT_GAMEPAD_Y)<>0);

  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadL_Offset), (XState.Buttons And XINPUT_GAMEPAD_LEFT_SHOULDER)<>0);
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadR_Offset), (XState.Buttons And XINPUT_GAMEPAD_RIGHT_SHOULDER)<>0);*)
End;

End.
