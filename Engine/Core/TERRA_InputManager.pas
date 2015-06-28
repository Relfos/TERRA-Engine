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
Unit TERRA_InputManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector3D, TERRA_Application, TERRA_Collections;

Const
  keyGamepadIndex   = 255;
	keyGamepadLeft_Offset    = 0;
	keyGamepadUp_Offset      = 1;
	keyGamepadRight_Offset   = 2;
	keyGamepadDown_Offset    = 3;
	keyGamepadA_Offset       = 4;
	keyGamepadB_Offset       = 5;
	keyGamepadC_Offset       = 6;
	keyGamepadD_Offset       = 7;
	keyGamepadX_Offset       = 8;
	keyGamepadY_Offset       = 9;
	keyGamepadZ_Offset       = 10;
	keyGamepadR1_Offset       = 11;
	keyGamepadR2_Offset       = 12;
	keyGamepadR3_Offset       = 13;
	keyGamepadL1_Offset       = 14;
	keyGamepadL2_Offset       = 15;
	keyGamepadL3_Offset       = 16;
  keyGamepadMenu_Offset    = 17;
	keyGamepadDPadLeft_Offset    = 18;
	keyGamepadDPadUp_Offset      = 19;
	keyGamepadDPadRight_Offset   = 20;
	keyGamepadDPadDown_Offset    = 21;
  keyGamepadCount   = 32;

  MaxGamePads = 8;

  keyMouseLeft   = 250;
  keyMouseRight  = 251;
  keyMouseMiddle = 252;

  MaxKeys = 512;
  MaximumFrameDelay = 10;

Type
  GamePadKind = (
    gamepadUnknown = 0,
    gamepadGeneric = 1,
    gamepadXBox360 = 2,
    gamepadXBoxOne = 3,
    gamepadOUYA = 4,
    gamepadPS3 = 5,
    gamepadPS4 = 6,
    gamepadPSVita = 7,
    gamepadWii = 8,
    gamepadWiiU = 9
  );

  KeyState = Record
    Frame:Cardinal;
    State:Boolean;
    Pressed:Boolean;
    Released:Boolean;
  End;

	PCursor=^MouseCursor;
	MouseCursor=Record
		X:SmallInt;
		Y:SmallInt;
	End;

	InputState = Class(TERRAObject)
    Protected
      _Keys:Array[0..Pred(MaxKeys)] Of KeyState;

    Public
      Constructor Create();

      Function IsDown(Key:Word):Boolean;
      Function IsUp(Key:Word):Boolean;

      Function WasPressed(Key:Word):Boolean;
      Function WasReleased(Key:Word):Boolean;

      Function SetState(Key:Word; Value:Boolean):Boolean;

      Procedure Reset();
  End;

  GamePad = Class(TERRAObject)
    Private
      _Active:Boolean;

    Protected
      _Disabled:Boolean;

      _LocalID:Integer;
      _DeviceID:Integer;

      _Kind:GamePadKind;
      _Name:TERRAString;

      Procedure Connnect();
      Procedure Disconnnect();

    Public
      Constructor Create(DeviceID:Integer);
      Procedure Update(Keys:InputState); Virtual; Abstract;

      Property LocalID:Integer Read _LocalID;
      Property Active:Boolean Read _Active;

      Property Kind:GamePadKind Read _Kind;
      Property Name:TERRAString Read _Name;
  End;

  InputManager = Class(ApplicationComponent)
    Protected
  		_Keys:InputState; //Keyboard/mouse/gamepad state

      _GamePads:Array[0..Pred(MaxGamePads)] Of GamePad;

      _ConnectedControllerCount:Integer;


    Public
  		Mouse:MouseCursor;
      Accelerometer:Vector3D;
      Gyroscope:Vector3D;
      Compass:Vector3D; // (heading, pitch, roll)

      Procedure Update; Override;
      Procedure Init; Override;
      Procedure Release; Override;

      Procedure AddGamePad(Pad:GamePad);
      Function GetGamePad(PadID:Integer):GamePad;

      Class Function Instance:InputManager;

      Property Keys:InputState Read _Keys;


      Property ControllerCount:Integer Read _ConnectedControllerCount;
  End;

Function IsMouseInput(Key:Integer):Boolean;
Function IsKeyboardInput(Key:Integer):Boolean;
Function IsGamepadInput(Key:Integer):Boolean;

Function GetGamepadID(Key:Integer):Integer;

Function GetGamePadKeyValue(GamePadID, KeyOffset:Integer):Integer;

Function GetKeyByName(Const KeyName:TERRAString):Integer;
Function GetKeyName(Key:Integer):TERRAString;

Implementation
Uses TERRA_GraphicsManager, TERRA_OS;

Var
  _InputManager_Instance:ApplicationObject = Nil;

Function IsMouseInput(Key:Integer):Boolean;
Begin
  Result := (Key >= keyMouseLeft) And (Key<=keyMouseMiddle);
End;

Function IsKeyboardInput(Key:Integer):Boolean;
Begin
  Result := (Key <=255);
End;

Function IsGamepadInput(Key:Integer):Boolean;
Begin
  Result := GetGamepadID(Key)>=0;
End;

Function GetGamePadKeyValue(GamePadID, KeyOffset:Integer):Integer;
Begin
	Result := keyGamepadIndex + (keyGamepadCount * GamePadID) + KeyOffset;
End;

Function GetGamepadID(Key:Integer):Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(MaxGamePads) Do
  If (Key>=keyGamepadIndex + keyGamepadCount * I) And (Key<keyGamepadIndex + keyGamepadCount * Succ(I)) Then
  Begin
    Result := I;
    Exit;
  End;

  Result := -1;
End;

Function GetKeyName(Key:Integer):TERRAString;
Var
  GamePadID:Integer;
Begin
  If (Key <=0) Then
  Begin
    Result := '';
    Exit;
  End;

  GamePadID := GetGamepadID(Key);
  If GamepadID>=0 Then
  Begin
    Key := Key -  GetGamePadKeyValue(GamePadID, 0);
    Case Key Of
      keyGamepadLeft_Offset:     Result:='Left';
      keyGamepadUp_Offset:       Result:='Up';
      keyGamepadRight_Offset:    Result:='Right';
      keyGamepadDown_Offset:     Result:='Down';
      keyGamepadA_Offset:        Result:='A';
      keyGamepadB_Offset:        Result:='B';
      keyGamepadC_Offset:        Result:='C';
      keyGamepadD_Offset:        Result:='D';
      keyGamepadX_Offset:        Result:='X';
      keyGamepadY_Offset:        Result:='Y';
      keyGamepadZ_Offset:        Result:='Z';
      keyGamepadL1_Offset:        Result:='L1';
      keyGamepadL2_Offset:        Result:='L2';
      keyGamepadL3_Offset:        Result:='L3';
      keyGamepadR1_Offset:        Result:='R1';
      keyGamepadR2_Offset:        Result:='R2';
      keyGamepadR3_Offset:        Result:='R3';
      keyGamepadMenu_Offset:     Result:='Menu';
      keyGamepadDPadLeft_Offset:     Result:='DPadLeft';
      keyGamepadDPadUp_Offset:       Result:='DPadUp';
      keyGamepadDPadRight_Offset:    Result:='DPadRight';
      keyGamepadDPadDown_Offset:     Result:='DPadDown';

      Else
        Result := 'Button' +IntToString(Key);
    End;

    Result := 'Gamepad' + Result + IntToString(Succ(GamePadID));
    Exit;
  End;

  Case Key Of
    keyBackspace: Result:='Back';
    keyEnter:     Result:='Enter';
    keyLeft:      Result:='Left';
    keyUp:        Result:='Up';
    keyRight:     Result:='Right';
    keyDown:      Result:='Down';

    keyTab:       Result:='Tab';
    keyShift:     Result:='Shift';
    keyControl:   Result:='Ctrl';
    keyAlt:       Result:='Alt';
    keyPause:     Result:='Pause';
    keyEscape:    Result:='ESC';
    keySpace:     Result:='Space';
    keyPageUp:    Result:='PgUp';
    keyPageDown:  Result:='PgDn';
    keyEnd:       Result:='End';
    keyHome:      Result:='Home';
    keyInsert:    Result:='Ins';
    keyDelete:    Result:='Del';
    keyF1:        Result:='F1';
    keyF2:        Result:='F2';
    keyF3:        Result:='F3';
    keyF4:        Result:='F4';
    keyF5:        Result:='F5';
    keyF6:        Result:='F6';
    keyF7:        Result:='F7';
    keyF8:        Result:='F8';
    keyF9:        Result:='F9';
    keyF10:       Result:='F10';
    keyF11:       Result:='F11';
    keyF12:       Result:='F12';

    keyMouseLeft:       Result:='MouseLeft';
    keyMouseRight:      Result:='MouseRight';
    keyMouseMiddle:     Result:='MouseMiddle';

  keyPlus: Result := 'Plus';
  keyMinus: Result := 'Minus';
  keyPeriod: Result := 'Dot';

  keyA: Result := 'A';
  keyB: Result := 'B';
  keyC: Result := 'C';
  keyD: Result := 'D';
  keyE: Result := 'E';
  keyF: Result := 'F';
  keyG: Result := 'G';
  keyH: Result := 'H';
  keyI: Result := 'I';
  keyJ: Result := 'J';
  keyK: Result := 'K';
  keyL: Result := 'L';
  keyM: Result := 'M';
  keyN: Result := 'N';
  keyO: Result := 'O';
  keyP: Result := 'P';
  keyQ: Result := 'Q';
  keyR: Result := 'R';
  keyS: Result := 'S';
  keyT: Result := 'T';
  keyU: Result := 'U';
  keyV: Result := 'V';
  keyW: Result := 'W';
  keyX: Result := 'X';
  keyY: Result := 'Y';
  keyZ: Result := 'Z';

    Else
        Result:= 'Key #'+IntToString(Key);
  End;
End;

Function GetKeyByName(Const KeyName:TERRAString):Integer;
Var
  Max, I:Integer;
Begin
  Result:=0;
  If KeyName='' Then
    Exit;

  Max := keyGamepadIndex + MaxGamePads * keyGamepadCount;

  For I:=1 To Max Do
  If GetKeyName(I) = KeyName Then
  Begin
    Result := I;
    Exit;
  End;

  Result := StringToInt(KeyName, False);
End;


{ InputState }
Constructor InputState.Create();
Begin
End;

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

  If (Value) Then
    Application.Instance.OnKeyDown(Key)
  Else
    Application.Instance.OnKeyUp(Key);

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

{ InputManager }
Class Function InputManager.Instance: InputManager;
Begin
  If _InputManager_Instance = Nil Then
    _InputManager_Instance := InitializeApplicationComponent(InputManager, GraphicsManager);

  Result := InputManager(_InputManager_Instance.Instance);
End;

Procedure InputManager.Init;
Begin
  _Keys := InputState.Create();
End;

Procedure InputManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(MaxGamePads) Do
    ReleaseObject(_GamePads[I]);

  ReleaseObject(_Keys);
End;

Procedure InputManager.Update;
Var
  I:Integer;
Begin
  _ConnectedControllerCount := 0;
  For I:=0 To Pred(MaxGamePads) Do
  If (Assigned(_GamePads[I])) Then
  Begin
    _GamePads[I].Update(_Keys);

    If _GamePads[I].Active Then
      Inc(_ConnectedControllerCount);
  End;
End;

Procedure InputManager.AddGamePad(Pad: GamePad);
Var
  N, I:Integer;
Begin
  If Pad = Nil Then
    Exit;

  N := -1;
  For I:=0 To Pred(MaxGamePads) Do
  If (_GamePads[I] = Nil) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    ReleaseObject(Pad);
    Exit;
  End;

  _GamePads[N] := Pad;
  Pad._LocalID := -1;
End;

{ GamePad }
//http://blackhole12.blogspot.pt/2012/06/how-joysticks-ruined-my-graphics-engine.html
//https://github.com/adamdruppe/arsd/blob/master/joystick.d
Procedure GamePad.Connnect;
Var
  Input:InputManager;
  N, I:Integer;
Begin
  If _Active Then
    Exit;

  Input := InputManager.Instance();

  _Active := True;

  N := -1;
  For I:=0 To Pred(MaxGamePads) Do
  If (Assigned(Input._GamePads[I])) And (Input._GamePads[I]._LocalID>=N) Then
  Begin
    N := Input._GamePads[I]._LocalID;
  End;

  _LocalID := Succ(N);
  Application.Instance.OnGamepadConnect(_LocalID);
End;

Procedure GamePad.Disconnnect;
Begin
  If (_Active) Then
  Begin
    _Active := False;
    Application.Instance.OnGamepadDisconnect(_LocalID);
    _Active := False;
    _LocalID := -1;
    _Kind := gamepadUnknown;
  End;
End;


Constructor GamePad.Create(DeviceID: Integer);
Begin
  _Name := '';
  _DeviceID := DeviceID;
End;


Function InputManager.GetGamePad(PadID: Integer): GamePad;
Var
  I:Integer;
Begin
  For I:=0 To Pred(MaxGamePads) Do
  If (Assigned(_Gamepads[I])) And (_Gamepads[I]._LocalID = PadID) Then
  Begin
    Result := _Gamepads[I];
    Exit;
  End;

  Result := Nil;
End;

End.
