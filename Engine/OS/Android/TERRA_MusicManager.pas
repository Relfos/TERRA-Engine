Unit TERRA_MusicManager;
{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_String, TERRA_Error, TERRA_Utils, TERRA_FileUtils, TERRA_Application, TERRA_Java, TERRA_OS, TERRA_FileManager;

Const
  JavaMusicPlayerClassName = 'com.pascal.terra.TERRAMusicPlayer';

Type
  MusicManager = Class(ApplicationComponent)
    Protected
      _Volume:Integer;
      _CurrentTrack:TERRAString;

      _CrossFade:Integer;
      _CrossFadeTime:Cardinal;
      _CrossVolume:Integer;

      _Mute:Boolean;
      _OldVolume:Integer;

      _Player:JavaObject;

      _PreviousTrack:TERRAString;
      _Enabled:Boolean;

      Procedure Start;

      Procedure SetEnabled(const Value: Boolean);

    Public
      Class Function Instance:MusicManager;

      Procedure Init; Override;
      Procedure Update; Override;

      Procedure Play(SourceName:TERRAString);
      Procedure Release; Override;

      Procedure SetVolume(Volume:Single);
      Procedure SetMute(Value:Boolean);
      Procedure SetCrossFade(Value:Integer);

      // player methods
      Procedure Stop;

      Property CrossFade:Integer Read _CrossFade Write SetCrossFade;
      Property Mute:Boolean Read _Mute Write SetMute;

      Property Enabled:Boolean Read _Enabled Write SetEnabled;
      Property PreviousTrack:TERRAString Read _PreviousTrack;
  End;

Implementation
Uses TERRA_ResourceManager, TERRA_Log, TERRA_Stream,
  TERRA_FileStream, TERRA_Math, jni;

Var
  _MusicManager_Instance:ApplicationObject;

Class Function MusicManager.Instance:MusicManager;
Begin
  If Not Assigned(_MusicManager_Instance) Then
    _MusicManager_Instance := InitializeApplicationComponent(MusicManager, Nil);

  Result := MusicManager(_MusicManager_Instance.Instance);
End;

Procedure MusicManager.Release;
Begin
  Stop();

  _MusicManager_Instance := Nil;
End;

Procedure MusicManager.Init;
Begin
  // set initial values
  _Volume := 25;
  _Player := Nil;
End;

Procedure MusicManager.Play(SourceName:TERRAString);
Var
  S:TERRAString;
  Params:JavaArguments;
  Frame:JavaFrame;

  Procedure TryExtension(Ext:TERRAString);
  Begin
    S := FileManager.Instance.SearchResourceFile(SourceName+'.'+Ext);
  End;
Begin
  {$IFDEF DISABLEMUSIC}
  Exit;
  {$ENDIF}

  _PreviousTrack := SourceName;

  If (Not _Enabled) Then
    Exit;

  SourceName := GetFileName(SourceName, True);

  If (StringEquals(SourceName, _CurrentTrack)) Then
    Exit;

  _CurrentTrack := SourceName;

  Stop;
  S := '';
  If S='' Then TryExtension('ogg');
  If S='' Then TryExtension('mid');
  If S='' Then TryExtension('mp3');

  If (S='') Then
  Begin
    Log(logDebug, 'MusicManager', 'Track not found: '+_CurrentTrack);
    Exit;
  End;

  Log(logDebug, 'MusicManager', 'Playing track: '+_CurrentTrack);

  Java_Begin(Frame);
  If (_Player = Nil) Then
  Begin
    _Player := JavaObject.Create(JavaMusicPlayerClassName, Nil, Frame);
  End;

  Params := JavaArguments.Create(Frame);
  Params.AddString(S);
  _Player.CallVoidMethod(Frame, 'setTrack', Params);
  ReleaseObject(Params);
  Java_End(Frame);

  Self.SetVolume(_Volume);
  Self.Start;
End;


Procedure MusicManager.SetVolume(Volume:Single);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  If (_Player = Nil) Then
    Exit;

  _Volume := Trunc(Volume);
  If (_Volume<0) Then
    _Volume := 0
  Else
  If (_Volume>100) Then
    _Volume := 100;

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddInteger(Trunc(Volume));
  _Player.CallVoidMethod(Frame, 'setVolume', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;


Procedure MusicManager.Start;
Var
  Frame:JavaFrame;
Begin
  If (_Player = Nil) Then
    Exit;

  Java_Begin(Frame);
  _Player.CallVoidMethod(Frame, 'play', Nil);
  Java_End(Frame);
End;

Procedure MusicManager.Stop;
Var
  Frame:JavaFrame;
Begin
  If (_Player = Nil) Then
    Exit;

  Log(logDebug, 'MusicPlayer', 'Stopping music...');
  Java_Begin(Frame);
  _Player.CallVoidMethod(Frame, 'stop', Nil);
  _Player.CallVoidMethod(Frame, 'release', Nil);
  ReleaseObject(_Player);
  _Player := Nil;
  Java_End(Frame);
End;

Procedure MusicManager.Update;
Var
  Delta:Single;
Begin
  If (_Player = Nil) Then
    Exit;

  If (_CrossFade>0) Then
  Begin
    Delta := Application.GetTime - _CrossFadeTime;
    Delta := Delta / _CrossFade;
    If (Delta>1) Then
    Begin
      Delta := 1;
      _CrossFade := 0;
    End;

    Delta := Abs(Cos(Delta*180*RAD));
    Self.SetVolume(_CrossVolume * Delta);
  End;
End;

Procedure MusicManager.SetMute(Value: Boolean);
Begin
  If (_Mute = Value) Then
    Exit;

  _Mute := Value;
  If (_Mute) Then
  Begin
    _OldVolume := Self._Volume;
    Self.SetVolume(0.0);
  End Else
    Self.SetVolume(Self._OldVolume);
End;

Procedure MusicManager.SetCrossFade(Value:Integer);
Begin
  Value := Value * 2;

  If (_CrossFade = Value) Then
    Exit;

  If Value >0 Then
  Begin
    _CrossVolume := _Volume;
    _CrossFadeTime := Application.GetTime;
  End;
  _CrossFade := Value;
End;

Procedure MusicManager.SetEnabled(const Value: Boolean);
Begin
  If (Value = _Enabled) Then
    Exit;

  Log(logDebug, 'MusicPlayer', 'Set enabled= '+BoolToString(Value));

  Self._Enabled := Value;
  Self._CurrentTrack := '';
End;

End.
