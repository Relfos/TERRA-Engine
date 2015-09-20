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
Uses TERRA_Object, TERRA_Stream, TERRA_Sound, TERRA_FileFormat;

Type
  WAVFormat = Class(TERRAFileFormat)
    Protected
      Function Identify(Source:TERRAStream):Boolean; Override;

    Public
      Function LoadFromStream(Target:TERRAObject; Source:TERRAStream):Boolean; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_FileStream, TERRA_FileUtils, TERRA_Log, TERRA_Application, TERRA_Engine;

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

Function FindChunk(Source:TERRAStream; ID:Cardinal; Var Chunk:TWaveChunk):Boolean;
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

{ WAVFormat }
Function WAVFormat.Identify(Source: TERRAStream): Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID,4);
  Result := CompareFileHeader(ID, 'RIFF');
End;

Function WAVFormat.LoadFromStream(Target: TERRAObject; Source: TERRAStream): Boolean;
Var
  Chunk:TWaveChunk;
  Header:TWaveHeader;
  Pos,Id :Cardinal;
  Size, SampleCount:Cardinal;
  MySound:TERRASound;
Begin
  Result := False;
  Source.Read(@Chunk, SizeOf(Chunk));
  Source.Read(@Id,4);

  If (Chunk.ID<>RIFF_ID) Or (Id<> WAVE_ID) Then
  Begin
    Engine.Log.Write(logError, 'WAV', 'Invalid wave file.');
    Exit;
  End;

  If Not FindChunk(Source, FMT_ID, Chunk ) Then
  Begin
    Engine.Log.Write(logError, 'WAV', 'Cannot find header chunk.');
    Exit;
  End;

  Pos:=Source.Position;
  Source.Read(@Header, SizeOf(Header));

  If Header.Format<>PCM_FORMAT Then
  Begin
    Engine.Log.Write(logError, 'WAV', 'Wave format not supported.');
    Exit;
  End;

  Source.Seek(Pos+Chunk.Size);

  If Not FindChunk(Source, DATA_ID, Chunk ) Then
  Begin
    Engine.Log.Write(logError, 'WAV', 'Cannot find wave data chunk.');
    Exit;
  End;

  If (Header.Bits <> 16) Then
  Begin
    Engine.Log.Write(logError, 'WAV', 'Only 16-bit audio can be loaded.');
    Exit;
  End;


  SampleCount := (Chunk.Size Div (Header.Channels * 2));

  MySound := TERRASound(Target);
  MySound.SamplesFromStream(SampleCount, Header.Frequency, Header.Channels>1, Source);

  Result := True;
End;

Begin
  Engine.Formats.Add(WAVFormat.Create(TERRASound, 'wav'));
End.
