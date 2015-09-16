Unit TERRA_AudioTrack;
{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_FileUtils, TERRA_Application, TERRA_MusicTrack, TERRA_MusicManager, TERRA_Java;

Type
  AudioMusicTrack = Class(MusicTrack)
    Protected
      _Player:JavaObject;

      Procedure ChangeVolume(Volume:Single); Override;

    Public
      Procedure Init(); Override;
      Procedure Play(); Override;
      Procedure Stop; Override;

      Class Function Supports(Const Extension:TERRAString):Boolean; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_FileManager, TERRA_Log, TERRA_OS, TERRA_Stream, TERRA_Math;

{ AudioMusicTrack }
Procedure AudioMusicTrack.ChangeVolume(Volume:Single);
Var
  Frame:JavaFrame;
  Params:JavaArguments;
Begin
  Inherited SetVolume(Volume);

  If _Player = Nil Then
    Exit;

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddInteger(Trunc(Volume));
  _Player.CallVoidMethod(Frame, 'setVolume', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Procedure AudioMusicTrack.Init();
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Java_Begin(Frame);
  _Player := JavaObject.Create(JavaMusicPlayerClassName, Nil, Frame);

  Params := JavaArguments.Create(Frame);
  Params.AddString(_FileName);
  _Player.CallVoidMethod(Frame, 'setTrack', Params);
  ReleaseObject(Params);

  Java_End(Frame);
End;

Procedure AudioMusicTrack.Play();
Var
  Frame:JavaFrame;
Begin
  If (_Player = Nil) Then
    Exit;

  Java_Begin(Frame);
  _Player.CallVoidMethod(Frame, 'play', Nil);
  Java_End(Frame);

  Self.SetVolume(_Volume);
End;

Procedure AudioMusicTrack.Stop;
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

Class Function AudioMusicTrack.Supports(Const Extension: TERRAString): Boolean;
Begin
  Result := (Extension = 'mid') Or (Extension = 'mp3') Or (Extension = 'ogg');
End;

End.
