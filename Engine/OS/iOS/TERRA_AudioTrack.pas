Unit TERRA_AudioTrack;
{$I terra.inc}
Interface

Uses TERRA_Utils, TERRA_String, TERRA_FileUtils, TERRA_Application, TERRA_MusicTrack;

Type
  AudioMusicTrack = Class(MusicTrack)
    Protected
      _Player:Pointer;

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
Begin
  If Assigned(_Player) Then
    audioSetVolume(_Player, _Volume);
End;

Procedure AudioMusicTrack.Init();
Begin
  _Player := audioOpen(PAnsiChar(FileName));
End;

Procedure AudioMusicTrack.Play();
Begin
  If Assigned(_Player) Then
    audioPlay(_Player);

  Self.SetVolume(_Volume);
End;

Procedure AudioMusicTrack.Stop;
Begin
  If Assigned(_Player) Then
    audioStop(_player);
End;

Class Function AudioMusicTrack.Supports(Const Extension: TERRAString): Boolean;
Begin
  Result := (Extension = 'm4a') Or (Extension = 'mp3');
End;

End.
