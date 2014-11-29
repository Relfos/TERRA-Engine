Unit TERRA_AudioTrack;
{$I terra.inc}
Interface

Uses TERRA_Utils, TERRA_FileUtils, TERRA_Application, TERRA_MusicTrack, TERRA_Java;

Const
  JavaPlayerClassName = 'com.pascal.terra.TERRAMusicPlayer';

Type
  AudioMusicTrack = Class(MusicTrack)
    Protected
      _Player:JavaObject;

    Public
      Procedure Init(); Override;
      Procedure Play(); Override;
      Procedure Stop; Override;
      Procedure SetVolume(Volume:Single); Override;

      Class Function Supports(Const Extension:AnsiString):Boolean; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_FileManager, TERRA_Log, TERRA_OS, TERRA_IO, TERRA_Math;

{ AudioMusicTrack }
Procedure AudioMusicTrack.SetVolume(Volume:Single);
Var
  intError :integer;

Begin
  Inherited SetVolume(Volume);

  If _Player = Nil Then
    Exit;

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddInteger(Trunc(Volume));
  _Player.CallVoidMethod('setVolume', Params);
  Params.Destroy();
  Java_End(Frame);
End;

Procedure AudioMusicTrack.Init();
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Java_Begin(Frame);
  _Player := JavaObject.Create(JavaPlayerClassName, Nil, Frame);

  Params := JavaArguments.Create(Frame);
  Params.AddString(S);
  _Player.CallVoidMethod('setTrack', Params);
  Params.Destroy();
  
  Java_End(Frame);
End;

Procedure AudioMusicTrack.Play();
Var
  Frame:JavaFrame;
Begin
  If (_Player = Nil) Then
    Exit;

  Java_Begin(Frame);
  _Player.CallVoidMethod('play', Nil);
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
  _Player.CallVoidMethod('stop', Nil);
  _Player.CallVoidMethod('release', Nil);
  _Player.Destroy();
  _Player := Nil;
  Java_End(Frame);
End;

Class Function AudioMusicTrack.Supports(Const Extension: AnsiString): Boolean;
Begin
  Result := (Extension = 'mid') Or (Extension = 'mp3') Or (Extension = 'ogg');
End;

End.
