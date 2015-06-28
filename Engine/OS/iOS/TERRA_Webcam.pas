Unit TERRA_Webcam;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Texture, TERRA_Color, TERRA_Image, TERRA_Application;

Type
  Webcam = Class(ApplicationComponent)
    Protected
      _Width:Integer;
      _Height:Integer;
      _NeedsRefresh:Boolean;
      _NeedsUpdate:Boolean;
      _Texture:Texture;
      _Image:Image;
      _DeviceCount:Integer;

      Procedure Release; Override;

      Procedure Init; Override;
      Procedure Update; Override;

      Procedure NewFrame(width, height:Integer; buffer:Pointer);

      Procedure Clear;


    Public
      Class Function Instance:Webcam;

      Procedure Stop;
      Procedure Pause;
      Procedure Resume;
      Function Start(DeviceID:Integer): integer;

      Function GetDeviceName(ID:Integer):TERRAString;

      Property Width: Integer Read _Width;
      Property Height: Integer Read _Height;
      Property DeviceCount:Integer Read _DeviceCount;
      Property  Texture:TERRA_Texture.Texture Read _Texture;
  End;

Procedure ApplicationOnCamera(width, height:Integer; buffer:Pointer); cdecl; export;

Implementation
Uses TERRA_Log, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_GraphicsManager, TERRA_OS;

Var
  _Webcam_Instance:ApplicationObject = Nil;
  _WebInit:Boolean = False;
  _CameraOn:Boolean = False;

Procedure Webcam.Init;
Begin
  _Image := Nil;
  _Texture := Nil;
  _NeedsRefresh := False;
  _NeedsUpdate := False;
  _Width := 0;
  _Height := 0;
  _CameraOn := False;
  _DeviceCount := 1;
End;

Procedure Webcam.Release;
Begin
  Self.Stop;
  Self.Clear;
  _Webcam_Instance := Nil;
End;

Class Function Webcam.Instance: Webcam;
Begin
  If Not Assigned(_Webcam_Instance) Then
    _Webcam_Instance := InitializeApplicationComponent(Webcam, GraphicsManager);


  Result := Webcam(_Webcam_Instance.Instance);
End;

Procedure Webcam.Clear;
Begin
  ReleaseObject(_Texture);
  ReleaseObject(_Image);
End;

Procedure ApplicationOnCamera(width, height:Integer; buffer:Pointer); cdecl; export;
Begin
  Log(logDebug, 'Webcam', 'Receiving frame: '+IntToString(Width)+'x'+IntToString(Height));

  If Not Assigned(_Webcam_Instance) Then
    Exit;

  Webcam(_Webcam_Instance.Instance).NewFrame(Width, Height, Buffer);
End;

Procedure Webcam.NewFrame(width, height:Integer; buffer:Pointer);
Begin

  If (Assigned(_Texture)) And ((_Texture.Width<>Width) Or (_Texture.Height<>Height) ) Then
    Self.Clear;

  If (_Texture = Nil) Then
  Begin
    _Width := Width;
    _Height := Height;
    Log(logDebug, 'Webcam', 'New texture: '+IntToString(Width)+'x'+IntToString(Height));

    _Texture := TERRA_Texture.Texture.New('webcam',Width, Height);
    _NeedsUpdate := True;
    Log(logDebug, 'Webcam', 'New image: '+IntToString(Width)+'x'+IntToString(Height));
    If (_Image = Nil) Then
      _Image := Image.Create(_Width, _Height);
  End;

  If Assigned(Buffer) Then
  Begin
    _NeedsRefresh := True;
    Move(Buffer^, _Image.Pixels^, _Image.Size);
  // _Image.FillRectangleByUV(0.0, 0.0, 1.0, 1.0, ColorGreen);
  End;
End;

Procedure Webcam.Update;
Begin
  If (_NeedsUpdate) Then
  Begin
    _Texture.BilinearFilter := False;
    _Texture.MipMapped := False;
    _Texture.Wrap := False;
    Log(logDebug, 'Webcam', 'UUpdating');
    _Texture.Update;
    _NeedsUpdate := False;
  End;

  If (_NeedsRefresh) Then
  Begin
    _NeedsRefresh := False;
    Log(logDebug, 'Webcam', 'Refresh!y');
    _Texture.UpdateRect(_Image);
  End;
End;

Procedure Webcam.Stop;
Begin
  If Not _CameraOn Then
    Exit;

  _CameraOn := False;
  Log(logDebug, 'Webcam', 'Stopping camera.');
  stopAVCapture();



End;

Procedure Webcam.Pause;
Begin
  Self.Stop;
End;

Procedure Webcam.Resume;
Begin
  Self.Start(0);
End;

Function Webcam.Start(DeviceID:Integer): integer;
Begin
  If Not _WebInit Then
  Begin
    _WebInit := True;
    enableAVCapture();
  End;

  If (_CameraOn) Then
    Exit;

  Log(logDebug, 'Webcam', 'Starting camera.');
  startAVCapture();
  Result := 0;
  _CameraOn := True;
End;

Function Webcam.GetDeviceName(ID:Integer):TERRAString;
Begin
  If (ID=0) Then
    Result := 'iOS Camera'
  Else
    Result := '';
End;

End.
