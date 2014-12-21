{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_WAVE
 * Implements a WAV file loader/decoder
 ***********************************************************************************************************************
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
