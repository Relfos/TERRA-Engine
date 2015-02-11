Unit TERRA_iCade;

{$I terra.inc}

Interface

Procedure UpdateiCadeKeyDown(Var Key:Word);
Procedure UpdateiCadeKeyUp(Var Key:Word);

Implementation
Uses TERRA_Client, TERRA_Application, TERRA_Log, TERRA_Utils;

Function GetKeyUp(C:TERRAChar):Integer;
Begin
  Case C Of
    'E',#33: Result := keyGamepadUp;
    'Q',#45: Result := keyGamepadLeft;
    'C',#31: Result := keyGamepadRight;
    'Z',#54: Result := keyGamepadDown;
    'T',#41: Result := keyGamepadX;
    'U',#35: Result := keyGamepadY;
    'M',#48: Result := keyGamepadZ;
    'G',#46: Result := keyGamepadL;
    'R',#44: Result := keyGamepadA;
    'N',#50: Result := keyGamepadB;
    'P',#34: Result := keyGamepadC;
    'V',#42: Result := keyGamepadR;
    Else
      Result := 0;
  End;
End;

Function GetKeyDown(C:TERRAChar):Integer;
Begin
  Case C Of
    'W',#51: Result := keyGamepadUp;
    'A',#29: Result := keyGamepadLeft;
    'D',#32: Result := keyGamepadRight;
    'X',#52: Result := keyGamepadDown;
    'Y',#37: Result := keyGamepadX;
    'U',#43: Result := keyGamepadY;
    'I',#53: Result := keyGamepadZ;
    'O',#36: Result := keyGamepadL;
    'H',#39: Result := keyGamepadA;
    'J',#40: Result := keyGamepadB;
    'K',#49: Result := keyGamepadC;
    'L',#38: Result := keyGamepadR;
    Else
      Result := 0;
  End;
End;

Procedure UpdateiCadeKeyDown(Var Key:Word);
Var
  C:TERRAChar;
  Up,Down:Integer;
Begin
  If (Key>255) Then
    Exit;

  C := Char(Key);
  Up := GetKeyUp(C);
  Down := GetKeyDown(C);

  If (Down<>0) Then
  Begin
    Key := Down;
  End Else
  If (Up<>0) Then
  Begin
    Key := 0;
  End;
End;

Procedure UpdateiCadeKeyUp(Var Key:Word);
Var
  C:TERRAChar;
  Up,Down:Integer;
Begin
  If (Key>255) Then
    Exit;

  C := Char(Key);
  Up := GetKeyUp(C);
  Down := GetKeyDown(C);

  If (Down<>0) Then
  Begin
    Key := 0;
  End Else
  If (Up<>0) Then
  Begin
    Application.Instance.Input.Keys[Up] := False;
    Key := Up;
  End;
End;

End.
