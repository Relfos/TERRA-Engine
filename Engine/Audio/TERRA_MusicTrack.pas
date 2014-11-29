Unit TERRA_MusicTrack;
{$I terra.inc}
Interface


Uses TERRA_Utils, TERRA_AL, TERRA_OGG, TERRA_SoundStreamer;

Type
  MusicTrackClass = Class Of MusicTrack;
  
  MusicTrack = Class(TERRAObject)
    Protected
      _FileName:AnsiString;
      _Volume:Single;

    Public
      Constructor Create(FileName:AnsiString; Volume:Single);
      Destructor Destroy; Override;

      Procedure Init(); Virtual; Abstract;
      Procedure Play(); Virtual; Abstract;
      Procedure Update; Virtual;
      Procedure Stop; Virtual; Abstract;

      Procedure SetVolume(Volume:Single); Virtual;

      Class Function Supports(Const Extension:AnsiString):Boolean; Virtual;

      Property FileName:AnsiString Read _FileName;
      Property Volume:Single Read _Volume Write SetVolume;
  End;

  StreamingMusicTrack = Class(MusicTrack)
    Protected
      _Stream:SoundStream;

    Public
      Destructor Destroy; Override;

      Procedure Init(); Override;
      Procedure Play(); Override;
      Procedure Update; Override;
      Procedure Stop; Override;
      Procedure SetVolume(Volume:Single); Override;

      Class Function Supports(Const Extension:AnsiString):Boolean; Override;
  End;

Implementation
Uses TERRA_FileManager, TERRA_IO;

{ MusicTrack }
Constructor MusicTrack.Create(FileName: AnsiString; Volume:Single);
Begin
  _FileName := FileName;
  _Volume := Volume;
End;

Destructor MusicTrack.Destroy;
Begin
  // do nothing
End;

Procedure MusicTrack.SetVolume(Volume: Single);
Begin
  Self._Volume := Volume;

  If (_Volume<0) Then
    _Volume := 0
  Else
  If (_Volume>1) Then
    _Volume := 1;
End;

Class Function MusicTrack.Supports(const Extension: AnsiString): Boolean;
Begin
  Result := False;
End;

Procedure MusicTrack.Update;
Begin
  // do nothing
End;

{ StreamingMusicTrack }
Procedure StreamingMusicTrack.Init;
Var
  Source:Stream;
Begin
  If (OpenALHandle=0) Then
    Exit;

  Source := FileManager.Instance().OpenFileStream(FileName);
  _Stream := CreateSoundStream(Source);
End;

Destructor StreamingMusicTrack.Destroy;
Begin
  DestroyObject(@_Stream);
End;


Procedure StreamingMusicTrack.Play();
Begin
  If Assigned(_Stream) Then
  Begin
    SetVolume(_Volume);
    _Stream.Play;
  End;
End;

Procedure StreamingMusicTrack.Update;
Begin
  If Assigned(_Stream) Then
    _Stream.Update;
End;

Procedure StreamingMusicTrack.Stop;
Begin
  If Assigned(_Stream) Then
    _Stream.Stop;
End;

Procedure StreamingMusicTrack.SetVolume(Volume:Single);
Begin
  Inherited SetVolume(Volume);

  If Assigned(_Stream) Then
    _Stream.Volume := _Volume;
End;


Class Function StreamingMusicTrack.Supports(Const Extension: AnsiString):Boolean;
Begin
  Result := (Extension = 'ogg');
End;


End.
