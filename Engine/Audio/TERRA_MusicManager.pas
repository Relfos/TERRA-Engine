Unit TERRA_MusicManager;
{$I terra.inc}
Interface

Uses TERRA_Utils, TERRA_FileUtils, TERRA_Application, TERRA_MusicTrack;

Type
  MusicManager = Class(ApplicationComponent)
    Protected
      _Enabled:Boolean;

      _Mute:Boolean;
      _OldVolume:Single;
      _Volume:Single;

      _CurrentTrack:MusicTrack;
      _PreviousTrackName:AnsiString;

      _CrossFade:Integer;
      _CrossFadeTime:Cardinal;
      _CrossVolume:Single;

      Procedure SetEnabled(const Value: Boolean);

    Public
      Class Function Instance:MusicManager;

      Destructor Destroy; Override;


      Procedure Init; Override;
      Procedure Update; Override;
      
      Procedure Play(SourceName:AnsiString);
      Procedure Stop;

      Procedure SetVolume(Volume:Single);
      Procedure SetMute(Value:Boolean);
      Procedure SetCrossFade(Value:Integer);

      Property CrossFade:Integer Read _CrossFade Write SetCrossFade;
      Property Mute:Boolean Read _Mute Write SetMute;

      Property CurrentTrack:MusicTrack Read _CurrentTrack;
      Property Enabled:Boolean Read _Enabled Write SetEnabled;
      Property PreviousTrack:AnsiString Read _PreviousTrackName;
  End;

Implementation
Uses TERRA_FileManager, TERRA_SoundManager, TERRA_Log, TERRA_OS, TERRA_IO, TERRA_Math
{$IFNDEF OSX}, TERRA_AudioTrack{$ENDIF};

Var
  _MusicManager_Instance:ApplicationObject;

Class Function MusicManager.Instance:MusicManager;
Begin
  If Not Assigned(_MusicManager_Instance) Then
    _MusicManager_Instance := InitializeApplicationComponent(MusicManager, {$IFDEF USE_OPENAL}SoundManager{$ELSE}Nil{$ENDIF});

  Result := MusicManager(_MusicManager_Instance.Instance);
End;

Procedure MusicManager.Init;
Begin
  SoundManager.Instance(); // load open AL

  // set initial values
  _Volume := 0.5;
  _Enabled := True;
End;

Procedure MusicManager.Play(SourceName:AnsiString);
Var
  S, Ext:AnsiString;
  Source:Stream;

  Procedure TryExtension(Ext:AnsiString);
  Begin
    If S<>'' Then
      Exit;

    S := FileManager.Instance.SearchResourceFile(SourceName+'.'+Ext);
  End;

  Procedure TryClass(C:MusicTrackClass);
  Begin
    If (_CurrentTrack<>Nil) Then
      Exit;

    If (C.Supports(Ext)) Then
      _CurrentTrack := C.Create(S, Self._Volume);
  End;
Begin
  {$IFDEF DISABLEMUSIC}
  Log(logDebug, 'Music', 'Cannot play '+SourceName+', music is disabled');
  Exit;
  {$ENDIF}

  If (SourceName='') Then
  Begin
    Self.Stop();
    Exit;
  End;

  SourceName := LowStr(SourceName);

  If (Assigned(_CurrentTrack)) Then
  Begin
    If (SourceName = GetFileName(_CurrentTrack.FileName, True)) Then
      Exit;

    _PreviousTrackName := _CurrentTrack.FileName;
    Stop();
  End Else
    _PreviousTrackName := SourceName;

  If (Not _Enabled) Then
  Begin
    Log(logDebug, 'Music', 'Cannot play '+SourceName+', music is disabled');
    Exit;
  End;

  S := '';
  TryExtension('ogg');
  TryExtension('mid');
  TryExtension('mp3');
  TryExtension('mod');

  If (S='') Then
    Exit;

  Ext := GetFileExtension(S);

  _CurrentTrack := Nil;

{$IFNDEF OSX}
  TryClass(AudioMusicTrack);
{$ENDIF}
  TryClass(StreamingMusicTrack);

  If _CurrentTrack=Nil Then
  Begin
    Log(logWarning, 'MusicManager', 'Cannot play track: '+SourceName);
    Exit;
  End;

  _CurrentTrack.Init();
  _CurrentTrack.Play();
End;

Destructor MusicManager.Destroy;
Begin
  If (Assigned(_CurrentTrack)) Then
  Begin
    _CurrentTrack.Stop();
    _CurrentTrack.Destroy();
  End;

  _MusicManager_Instance := Nil;
End;

Procedure MusicManager.SetVolume(Volume:Single);
Begin
  _Volume := Trunc(Volume);
  If (_Volume<0) Then
    _Volume := 0
  Else
  If (_Volume>1) Then
    _Volume := 1;

  If Assigned(_CurrentTrack) Then
    _CurrentTrack.SetVolume(_Volume);
End;


Procedure MusicManager.Update;
Var
  Delta:Single;
Begin
  If (_CrossFade>0) Then
  Begin
    Delta := GetTime - _CrossFadeTime;
    Delta := Delta / _CrossFade;
    If (Delta>1) Then
    Begin
      Delta := 1;
      _CrossFade := 0;
    End;

    Delta := Abs(Cos(Delta*180*RAD));
    Self.SetVolume(_CrossVolume * Delta);
  End;

  If Assigned(_CurrentTrack) Then
    _CurrentTrack.Update();
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
    _CrossFadeTime := GetTime;
  End;
  _CrossFade := Value;
End;

Procedure MusicManager.SetEnabled(Const Value: Boolean);
Begin
  If (Value = _Enabled) Then
    Exit;
    
  Self._Enabled := Value;

  If Not Value Then
    Self.Stop();
End;

Procedure MusicManager.Stop();
Begin
  If _CurrentTrack = Nil Then
    Exit;

  _CurrentTrack.Stop();
  _CurrentTrack.Destroy();
  _CurrentTrack := Nil;
End;

End.
