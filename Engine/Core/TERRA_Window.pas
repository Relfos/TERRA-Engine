Unit TERRA_Window;

Interface
Uses TERRA_Object, TERRA_String, TERRA_Vector2D;

Type
	// Window state
  TERRAWindowState = (
	  window_Normal,
    window_Background,
	  window_Minimized
  );

  TERRAWindow = Class(TERRAObject)
    Protected
      _Handle:Cardinal;
    
			_Title:TERRAString;
			_State:TERRAWindowState;

      _Position:Vector2D;

      _Width:Integer;
      _Height:Integer;
			_FullScreen:Boolean;

      _AspectRatio:Single;

      _AntialiasSamples:Integer;

      _LockCursor:Boolean;

      _ChangeToFullScreen:Boolean;

      Procedure SetTitle(Const Value:TERRAString); Virtual;
      Procedure SetPosition(Const Value:Vector2D); Virtual;
      Procedure SetSize(Width, Height:Integer); Virtual;
      Procedure SetState(const Value: TERRAWindowState); Virtual;

      Procedure SetFullScreen(const Value: Boolean);

      Procedure LockCursor(Value:Boolean); Virtual;

      Function SetFullscreenMode(UseFullScreen:Boolean):Boolean; Virtual;
      Procedure ToggleFullscreen;

    Public
      Constructor Create(Const Title:TERRAString;  Width, Height:Integer; Fullscreen:Boolean);

      Property Title:TERRAString Read _Title Write SetTitle;

      Procedure Resize(Width, Height:Integer);

      Procedure Update(); Virtual;

			Property Width:Integer Read _Width;
			Property Height:Integer Read _Height;
			Property FullScreen:Boolean Read _Fullscreen Write SetFullScreen;

      Property State:TERRAWindowState Read _State Write SetState;

      Property LockedCursor:Boolean Read _LockCursor Write LockCursor;

      Property Position:Vector2D Read _Position Write SetPosition;

      Property Handle:Cardinal Read _Handle;
  End;

Implementation
Uses TERRA_Log, TERRA_EngineManager;

{ TERRAWindow }
Constructor TERRAWindow.Create(const Title: TERRAString; Width, Height: Integer; Fullscreen: Boolean);
Begin
  Self._State := Window_Normal;
  Self._Title := Title;
  Self._Width := Width;
  Self._Height := Height;
  Self._Fullscreen := Fullscreen;
End;

Procedure TERRAWindow.LockCursor(Value: Boolean);
Begin
  Self._LockCursor := Value;
End;

Procedure TERRAWindow.SetFullScreen(const Value: Boolean);
Begin
  If (Value = Self._FullScreen) Or (_ChangeToFullScreen) Then
    Exit;

  _FullScreen := Value;
  _ChangeToFullScreen := True;
End;

Procedure TERRAWindow.SetPosition(const Value: Vector2D);
Begin
  // do nothing
End;

Procedure TERRAWindow.SetState(const Value: TERRAWindowState);
Begin
  // do nothing
End;

Procedure TERRAWindow.SetTitle(const Value: TERRAString);
Begin
  _Title := Value;
End;

Function TERRAWindow.SetFullscreenMode(UseFullScreen: Boolean): Boolean;
Begin
  Log(logError, 'App','ToggleFullscreen not implemented!');
  Result := False;
End;


Procedure TERRAWindow.ToggleFullscreen;
Var
   NewMode:Boolean;
Begin
  NewMode := Not Self._Fullscreen;
  If SetFullscreenMode(NewMode) Then
    Self._Fullscreen := NewMode;
End;

Procedure TERRAWindow.Resize(Width, Height: Integer);
Begin
  If (Width=0) Or (Height=0) Then
    Exit;

  _Width := Width;
  _Height := Height;

  Self.SetSize(_Width, _Height);

  Engine.Graphics.ResizeDevice(Width, Height);
End;

Procedure TERRAWindow.SetSize(Width, Height: Integer);
Begin
  // do nothing
End;

Procedure TERRAWindow.Update;
Begin
  If (_ChangeToFullScreen) Then
  Begin
	  _ChangeToFullScreen := False;
	  ToggleFullScreen();
  End;
End;

End.