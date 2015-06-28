Unit TERRA_Gamepad;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_InputManager;

Type
  WindowsGamepad = Class(Gamepad)
    Protected
    Public
      Procedure Update(Keys:InputState); Override;
  End;


Const
  REGSTR_PATH_CURRENT_CONTROL_SET	= 'System\CurrentControlSet\Control';
  REGSTR_PATH_MEDIARESOURCES  = REGSTR_PATH_CURRENT_CONTROL_SET + '\MediaResources';
  REGSTR_PATH_MEDIAPROPERTIES  = REGSTR_PATH_CURRENT_CONTROL_SET + '\MediaProperties';
  REGSTR_PATH_PRIVATEPROPERTIES = REGSTR_PATH_MEDIAPROPERTIES +'\PrivateProperties';
  REGSTR_PATH_PUBLICPROPERTIES = REGSTR_PATH_MEDIAPROPERTIES +'\PublicProperties';

  REGSTR_PATH_JOYOEM           = REGSTR_PATH_PRIVATEPROPERTIES +'\Joystick\OEM';
  REGSTR_PATH_JOYCONFIG        = REGSTR_PATH_MEDIARESOURCES + '\Joystick';
  REGSTR_KEY_JOYCURR            = 'CurrentJoystickSettings';
  REGSTR_KEY_JOYSETTINGS       = 'JoystickSettings';

  REGSTR_VAL_JOYOEMNAME		= 'OEMName';

Implementation
Uses Windows, TERRA_Multimedia;

// Gets joystick name from the registry.
Function GetJoystickName(index:Integer):TERRAString;
Var
  RegKey, RegValue, Path:TERRASTring;
  szRegKey:TERRAString;
  RegName:TERRAString;
  RegSize:Integer;
  Root, Key:HKEY;
  ErrorCode:Cardinal;
  joycaps:GamepadCaps;
Begin
  Result := '';

  If (joyGetDevCaps(Index, joycaps, SizeOf(joycaps))<>JOYERR_NOERROR) Then
    Exit;

  Result := JoyCaps.szPname;
  szRegKey := JoyCaps.szRegKey;

  RegKey := REGSTR_PATH_JOYCONFIG+'\'+szRegKey+'\'+REGSTR_KEY_JOYCURR;

  Root := HKEY_CURRENT_USER;
  ErrorCode := RegOpenKeyExA(Root, PAnsiChar(regkey), 0, KEY_READ, Key);
  If (ErrorCode =  ERROR_SUCCESS) Then
  Begin
    RegValue := 'Joystick'+IntToString(Succ(Index))+ REGSTR_VAL_JOYOEMNAME;
    RegSize := 1024;
    SetLength(RegName, RegSize);
    ErrorCode := RegQueryValueExA(Key, PAnsiChar(Regvalue), Nil, Nil, @RegName[1], @RegSize);
    RegCloseKey(Key);

    RegName := StringTrimRight(RegName);

    If (ErrorCode = ERROR_SUCCESS) Then
    Begin
      RegKey := REGSTR_PATH_JOYOEM +'\'+ RegName;
      If RegOpenKeyExA(Root, PAnsiChar(regKey), 0, KEY_READ, Key) = ERROR_SUCCESS Then
      Begin
        Result := '';
        RegSize := 1024;
        SetLength(Result, RegSize);
        RegQueryValueExA(Key, REGSTR_VAL_JOYOEMNAME, Nil, Nil, @Result[1], @Regsize);
        Result := StringTrimRight(Result);
        RegCloseKey(Key);
      End;
    End;
  End;
End;


//https://github.com/adamdruppe/arsd/blob/master/joystick.d
{ WindowsGamepad }
Procedure WindowsGamepad.Update(Keys:InputState);
Var
  JoyInfo:GamepadInfoEx;
  dwResult:Cardinal;
Begin
  If (_Kind = gamepadUnknown) Then
  Begin
    _Name := GetJoystickName(Self._DeviceID);

    If (StringContains('360', _Name)) Then
    Begin
      _Kind := gamepadXBox360;
      _Disabled := True;
      Exit;
    End Else
    If (StringContains('PLAYSTATION', _Name)) Then
    Begin
      If (StringContains('4', _Name)) Then
        _Kind := gamepadPS4
      Else
        _Kind := gamepadPS3;
    End Else
    If (StringContains('OUYA', _Name)) Then
    Begin
      _Kind := gamepadOUYA;
    End Else
    Begin
      _Kind := gamepadGeneric;
    End;
  End;

  If _Disabled Then
    Exit;

  FillChar(JoyInfo, SizeOf(JoyInfo), 0);
  JoyInfo.dwSize := SizeOf(JoyInfo);
  JoyInfo.dwFlags := JOY_RETURNALL;
  dwResult := joyGetPosEx(_DeviceID, joyInfo);

  If (dwResult <> JOYERR_NOERROR) Then
  Begin
    Self.Disconnnect();
    Exit;
  End Else
    Self.Connnect();

  //Windows.SetWindowText(Handle, PAnsiChar(IntToString(JoyInfo.wXpos)));
  //Windows.SetWindowText(Handle, PAnsiChar(IntToString(JoyInfo.wButtons)));

  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadUp_Offset), (JoyInfo.wYpos=0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadDown_Offset), (JoyInfo.wYpos=65535));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadLeft_Offset), (JoyInfo.wXpos=0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadRight_Offset), (JoyInfo.wXpos=65535));

  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadDPadUp_Offset), (JoyInfo.dwPOV=0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadDPadDown_Offset), (JoyInfo.dwPOV=18000));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadDPadLeft_Offset), (JoyInfo.dwPOV=27000));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadDPadRight_Offset), (JoyInfo.dwPOV=9000));

  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadA_Offset), (JoyInfo.wButtons And $1<>0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadY_Offset), (JoyInfo.wButtons And $2<>0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadB_Offset), (JoyInfo.wButtons And $4<>0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadX_Offset), (JoyInfo.wButtons And $8<>0));

  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadL1_Offset), (JoyInfo.wButtons And $10<>0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadR1_Offset), (JoyInfo.wButtons And $20<>0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadL1_Offset), (JoyInfo.wButtons And $40<>0));
  Keys.SetState(GetGamePadKeyValue(LocalID, keyGamePadL2_Offset), (JoyInfo.wButtons And $80<>0));
End;

End.