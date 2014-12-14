{
@abstract(WAV Loader)
@author(Sergio Flores <relfos@gmail.com>)
@created(July 14, 2006)
@lastmod(July 14, 2006)
Allows loading of WAV sound files.

Version History
   14/07/06  • Implemented basic BMP loader
}

Unit TERRA_WAVE;
{$I terra.inc}
Interface
Uses TERRA_IO, TERRA_Sound;

Function ValidateWAV(Source:Stream):Boolean;
Function WAVLoad(Source:Stream; MySound:Sound):Boolean;

Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_FileIO, TERRA_FileUtils, TERRA_Log, TERRA_Application;

Const
  PCM_FORMAT =  1;
  // Chunks IDs
  RIFF_ID    =  $46464952;   // "RIFF"
  WAVE_ID    =  $45564157;   // "WAVE"
  FMT_ID     =  $20746D66;   // " fmt"
  DATA_ID    =  $61746164;   // "data"

Type
  TWaveHeader=Packed Record
                Format:Word;
                Channels:Word;
                Frequency:Cardinal;
                AvgBytes:Cardinal;
                BlockAlign:Word;
                Bits:Word;
              End;

  TWaveChunk=Packed Record
                ID:Cardinal;
                Size:Cardinal;
              End;

Function FindChunk(Source:Stream; ID:Cardinal; Var Chunk:TWaveChunk):Boolean;
Var
  Pos:Cardinal;
Begin
  Result:=False;
  Pos:=Source.Position;
  Repeat
    Source.Read(@Chunk, SizeOf(Chunk));
    If Chunk.ID<>ID Then
    Begin
       Inc(Pos,SizeOf(Chunk)+Chunk.Size);
       Source.Seek(Pos);
    End Else
      Result:=True;
  Until (Result) Or (Source.Position>=Source.Size);
End;

Function WAVLoad(Source:Stream; MySound:Sound):Boolean;
Var
  Chunk:TWaveChunk;
  Header:TWaveHeader;
  Pos,Id :Cardinal;
  Size:Cardinal;
Begin
  Result := False;
  Source.Read(@Chunk, SizeOf(Chunk));
  Source.Read(@Id,4);

  If (Chunk.ID<>RIFF_ID) Or (Id<> WAVE_ID) Then
  Begin
    RaiseError('Invalid wave file.');
    Exit;
  End;

  If Not FindChunk(Source, FMT_ID, Chunk ) Then
  Begin
    RaiseError('Cannot find header chunk.');
    Exit;
  End;

  Pos:=Source.Position;
  Source.Read(@Header, SizeOf(Header));

  If Header.Format<>PCM_FORMAT Then
  Begin
    RaiseError('Wave format not supported.');
    Exit;
  End;

  Source.Seek(Pos+Chunk.Size);

  If Not FindChunk(Source, DATA_ID, Chunk ) Then
  Begin
    RaiseError('Cannot find wave data chunk.');
    Exit;
  End;

  MySound.New(Chunk.Size, Header.Channels, Header.Bits, Header.Frequency);
  Source.Read(MySound.Data, Chunk.Size);
  Result := True;
End;

Function ValidateWAV(Source:Stream):Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID,4);
  Result := CompareFileHeader(ID, 'RIFF');
End;

Begin
  Log(logDebug, 'WAV', 'Initializing');

  RegisterSoundFormat('WAV',ValidateWAV,WAVLoad);

  Log(logDebug, 'WAV', 'WAVE sound format registered!');
End.
