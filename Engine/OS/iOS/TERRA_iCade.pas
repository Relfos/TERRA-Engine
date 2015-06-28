Unit TERRA_iCade;

{$I terra.inc}

Interface

Procedure UpdateiCadeKeyDown(Var Key:Word);
Procedure UpdateiCadeKeyUp(Var Key:Word);

Implementation
Uses TERRA_Application, TERRA_String, TERRA_OS, TERRA_Log, TERRA_InputManager, TERRA_Utils;

Function GetKeyUp(C:TERRAChar):Integer;
Begin
  Case C Of
    Ord('E'), 33: Result := keyUp;
    Ord('Q'), 45: Result := keyLeft;
    Ord('C'), 31: Result := keyRight;
    Ord('Z'), 54: Result := keyDown;
    Ord('T'), 41: Result := keyX;
    Ord('U'), 35: Result := keyY;
    Ord('M'), 48: Result := keyZ;
    Ord('G'), 46: Result := keyL;
    Ord('R'), 44: Result := keyA;
    Ord('N'), 50: Result := keyB;
    Ord('P'), 34: Result := keyC;
    Ord('V'), 42: Result := keyR;
    Else
      Result := 0;
  End;
End;

Function GetKeyDown(C:TERRAChar):Integer;
Begin
  Case C Of
    Ord('W'),51: Result := keyUp;
    Ord('A'),29: Result := keyLeft;
    Ord('D'),32: Result := keyRight;
    Ord('X'),52: Result := keyDown;
    Ord('Y'),37: Result := keyX;
    Ord('U'),43: Result := keyY;
    Ord('I'),53: Result := keyZ;
    Ord('O'),36: Result := keyL;
    Ord('H'),39: Result := keyA;
    Ord('J'),40: Result := keyB;
    Ord('K'),49: Result := keyC;
    Ord('L'),38: Result := keyR;
    Else
      Result := 0;
  End;
End;

Procedure UpdateiCadeKeyDown(Var Key:Word);
Var
  Up,Down:Integer;
Begin
  If (Key>255) Then
    Exit;

  Up := GetKeyUp(Key);
  Down := GetKeyDown(Key);

  If (Down<>0) Then
  Begin
    Key := Down;
    Application.Instance.AddValueEvent(eventKeyDown, Key);
  End Else
  If (Up<>0) Then
  Begin
    Key := 0;
  End;
End;

Procedure UpdateiCadeKeyUp(Var Key:Word);
Var
  Up,Down:Integer;
Begin
  If (Key>255) Then
    Exit;

  Up := GetKeyUp(Key);
  Down := GetKeyDown(Key);

  If (Down<>0) Then
  Begin
    Key := 0;
  End Else
  If (Up<>0) Then
  Begin
    Application.Instance.AddValueEvent(eventKeyUp, Key);
    Key := Up;
  End;
End;

End.
