Unit TERRA_UICursor;

{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_String, TERRA_Texture;

Type
  TERRACursorType = (
    cursor_Default,
    cursor_Busy,
    cursor_Precision,
    cursor_Text,
    cursor_Forbidden,
    cursor_ResizeVertical,
    cursor_ResizeHorizontal,
    cursor_ResizeDiagonal,
    cursor_ResizeDiagonal2,
    cursor_Move,
    cursor_Rotate,
    cursor_Link
  );

  TERRACursor = Class(TERRAObject)
    Texture:TERRATexture;
    OfsX:Integer;
    OfsY:Integer;
  End;


  CursorManager = Class(TERRAObject)
    Protected
      _Cursors:Array[TERRACursorType] Of TERRACursor;

    Public
      Constructor Create();
      Procedure Release(); Override;

      Function GetCursor(CursorType:TERRACursorType):TERRACursor;
      Function SetCursor(CursorType:TERRACursorType; Texture:TERRATexture; Const OfsX:Integer = 0; Const OfsY:Integer = 0):TERRACursor;
  End;

Implementation


{ CursorManager }
Constructor CursorManager.Create;
Var
  I:TERRACursorType;
Begin
  For I:=Low(TERRACursorType) To High(TERRACursorType) Do
  Begin
    _Cursors[I] := TERRACursor.Create();
  End;
End;

Procedure CursorManager.Release;
Var
  I:TERRACursorType;
Begin
  For I:=Low(TERRACursorType) To High(TERRACursorType) Do
    ReleaseObject(_Cursors[I]);
End;

Function CursorManager.GetCursor(CursorType: TERRACursorType): TERRACursor;
Begin
  If (Assigned(_Cursors[CursorType])) And (Assigned(_Cursors[CursorType].Texture)) Then
    Result := _Cursors[CursorType]
  Else
    Result := Nil;
End;


Function CursorManager.SetCursor(CursorType: TERRACursorType; Texture:TERRATexture; const OfsX, OfsY: Integer): TERRACursor;
Begin
  _Cursors[CursorType].Texture := Texture;
  _Cursors[CursorType].OfsX := OfsX;
  _Cursors[CursorType].OfsY := OfsY;
End;

End.