Unit TERRA_ConsoleApplication;

{$I terra.inc}

Interface
Uses {$IFDEF WINDOWS}Windows, {$ENDIF} TERRA_OS, TERRA_String;

Type
  ConsoleApplication = Class(Application)
    Protected
    {$IFDEF WINDOWS}
//      _StdHandle:THandle;
    {$ENDIF}

      Function InitWindow:Boolean; Override;
  		Procedure CloseWindow; Override;

      Function SetFullscreenMode(UseFullScreen:Boolean):Boolean; Override;


      {$IFDEF OSX}
      Procedure MoveToBundleFolder(); Override;
      {$ENDIF}

    Public
      Constructor Create();

      Procedure OnIdle; Override;

      Function SelectRenderer():Integer; Override;

      Procedure LogToConsole(Const Text:TERRAString); Override;

      Function GetWidth:Word; Override;
      Function GetHeight:Word; Override;
  End;


Implementation
Uses TERRA_Application, TERRA_InputManager;

{ ConsoleApplication }
Constructor ConsoleApplication.Create;
Begin
  _Managed := True;
  {$IFDEF WINDOWS}
  //get the console handle
  _Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  If _Handle = 0 Then
  Begin
    AllocConsole();
    _Handle := GetStdHandle(STD_INPUT_HANDLE);
  End;

  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}

  Inherited;
End;

Function ConsoleApplication.GetWidth: Word;
Begin
  Result := 0;
End;

Function ConsoleApplication.GetHeight: Word;
Begin
  Result := 0;
End;

Procedure ConsoleApplication.OnIdle;
{$IFDEF WINDOWS}
Var
  NumberOfEventsRead, EventCount:Cardinal;
  Buffer:TInputRecord;
{$ENDIF}
Begin
{$IFDEF WINDOWS}
  EventCount := 0;
  //get the number of events
  GetNumberOfConsoleInputEvents(_Handle, EventCount);
  If EventCount>0 Then
  Begin
    //retrieve the event
    PeekConsoleInput(_Handle, Buffer, 1, NumberOfEventsRead);
    If NumberOfEventsRead > 0 Then
    begin
      If Buffer.EventType = KEY_EVENT then //is a Keyboard event?
      Begin
        If Buffer.Event.KeyEvent.bKeyDown Then //the key was pressed?
        Begin
          Application.Instance.AddValueEvent(eventKeyDown, Buffer.Event.KeyEvent.wVirtualKeyCode);
        End Else
        Begin
          Application.Instance.AddValueEvent(eventKeyUp, Buffer.Event.KeyEvent.wVirtualKeyCode);
        End;
      End;

      FlushConsoleInputBuffer(_Handle);//flush the buffer
    End;
  End;

{$ENDIF}
End;

Function ConsoleApplication.InitWindow: Boolean;
Begin
  Result := True;
End;

Procedure ConsoleApplication.CloseWindow;
Begin

End;


Function ConsoleApplication.SetFullscreenMode( UseFullScreen: Boolean): Boolean;
Begin
  Result := False;
End;

Function ConsoleApplication.SelectRenderer: Integer;
Begin
  Result := 0; // select null renderer
End;

{$IFDEF OSX}
Procedure ConsoleApplication.MoveToBundleFolder();
Begin
 // do nothing
End;
{$ENDIF}

Procedure ConsoleApplication.LogToConsole(const Text: TERRAString);
Var
  S:WideString;
  Written:Cardinal;
Begin
  {$IFDEF WINDOWS}
  S := StringToWideString(Text) + #13#10;
  WriteConsoleW(_Handle, PWideChar(S), Length(S), Written, Nil);
  {$ELSE}
  WriteLn(Text);
  {$ENDIF}
End;

End.
