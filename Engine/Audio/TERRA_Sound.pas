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
 * TERRA_Sound
 * Implements a Sound resource
 ***********************************************************************************************************************
}

Unit TERRA_Sound;
{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Resource, TERRA_Collections, TERRA_AL;

Const
  DefaultSampleRate = 44100;

  sndStopped = 0;
  sndPlaying = 1;
  sndPaused   = 2;

  SOUND_FORMAT_16BIT = 1;
  SOUND_FORMAT_8BIT  = 0;

Type
  Sound = Class(TERRAResource)
    Protected
      _Buffer:Cardinal;
      _Data:Pointer;
      _BufferSize:Cardinal;
      _Frequency:Cardinal;
      _BitsPerSample:Cardinal;
      _Channels:Cardinal;

      _AttachList:Array Of Pointer;
      _AttachCount:Integer;

      Function GetBufferLength(Size,Channels,BitsPerSample,Frequency:Cardinal):Cardinal;
      Function GetBufferSize(Length,Channels,BitsPerSample,Frequency:Cardinal):Cardinal;
      Function GetSampleSize:Cardinal;
      Function GetLength:Cardinal;
      Function GetFormat:Integer;

      Function GetHandle:Cardinal;

    Public
      Function Load(Source:TERRAStream):Boolean; Override;
      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;

      Procedure AttachSource(Source:Pointer);
      Procedure RemoveSource(Source:Pointer);

      Class Function GetManager:Pointer; Override;

      Procedure New(Size,Channels,BitsPerSample,Frequency:Cardinal);

      Property Data:Pointer Read _Data;
      Property Size:Cardinal Read _BufferSize;
      Property BufferSize:Cardinal Read _BufferSize Write _BufferSize;
      Property Frequency:Cardinal Read _Frequency;
      Property BitsPerSample:Cardinal Read _BitsPerSample;
      Property SampleSize:Cardinal Read GetSampleSize;
      Property Channels:Cardinal Read _Channels;
      Property Format:Integer Read GetFormat;
      Property Buffer:Cardinal Read _Buffer;
  End;

  SoundStreamValidateFunction=Function(Source:TERRAStream):Boolean;
  SoundLoader=Function(Source:TERRAStream; Sound:Sound):Boolean;
  SoundSaver=Procedure(Source:TERRAStream; Sound:Sound; Const Options:TERRAString='');

  PSoundClassInfo = ^SoundClassInfo;
  SoundClassInfo = Record
    Name:TERRAString;
    Validate:SoundStreamValidateFunction;
    Loader:SoundLoader;
    Saver:SoundSaver;
  End;

Function GetSoundLoader(Source:TERRAStream):SoundLoader;
//Function GetSoundSaver(Format:TERRAString):SoundSaver;
Procedure RegisterSoundFormat(Name:TERRAString;
                              Validate:SoundStreamValidateFunction;
                              Loader:SoundLoader;
                              Saver:SoundSaver=Nil);

Var
  _SoundExtensions:Array Of SoundClassInfo;
  _SoundExtensionCount:Integer = 0;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Application, TERRA_Log, TERRA_SoundManager, TERRA_SoundSource;


Function Sound.Unload:Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_AttachCount) Do
    SoundManager.Instance().Delete(SoundSource(_AttachList[I]));
  _AttachCount := 0;

  If (_Buffer<>0) Then
  Begin
    alDeleteBuffers(1, @_Buffer); {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
    _Buffer := 0;
  End;

  If Assigned(_Data) Then
  Begin
    FreeMem(_Data);
    _Data:=Nil;
  End;

  Result := Inherited Unload();
End;

Procedure Sound.New(Size, Channels, BitsPerSample, Frequency: Cardinal);
Begin
  _Channels:=Channels;
  _Frequency:=Frequency;
  _BitsPerSample:=BitsPerSample;
  _BufferSize:=Size;

  _Buffer := 0;
  GetMem(_Data,_BufferSize);

  SetStatus(rsReady);
End;

Function Sound.Update:Boolean;
Begin
  Inherited Update();

  Result := False;

  If (_Buffer=0) Then
  Begin
    alGenBuffers(1, @_Buffer);  {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}
  End Else
    Exit;

  If Format=-1 Then
  Begin
    RaiseError('Invalid sound format.');
    Result := False;
    Exit;
  End;

  alBufferData(_Buffer, Format, Data, BufferSize, Frequency);    {$IFDEF FULLDEBUG}DebugOpenAL;{$ENDIF}

  SetStatus(rsReady);
End;

Function Sound.GetSampleSize:Cardinal;
Begin
  Result := BitsPerSample Div 8;
End;

Function Sound.GetHandle:Cardinal;
Begin
  Result := _Buffer;
End;

Function Sound.GetBufferLength(Size,Channels,BitsPerSample,Frequency:Cardinal):Cardinal;
Begin
  Result := Round((((Size/Self.Channels)/Self.SampleSize)*1000)/Frequency);
End;

Function Sound.GetBufferSize(Length,Channels,BitsPerSample,Frequency:Cardinal):Cardinal;
Begin
  Result := Round((Length/1000)*Frequency*Self.SampleSize*Self.Channels);
End;

Function Sound.GetLength:Cardinal;
Begin
  Result := GetBufferLength(BufferSize,Channels,SampleSize,Frequency);
End;

Function Sound.GetFormat:Integer;
Begin
  Result := -1;

  If Channels=2 Then
  Begin
    If BitsPerSample=8 Then
      Result := AL_FORMAT_STEREO8
    Else
    If BitsPerSample=16 Then
      Result := AL_FORMAT_STEREO16;
  End Else
  Begin
    If BitsPerSample=8 Then
      Result := AL_FORMAT_MONO8
    Else
    If BitsPerSample=16 Then
      Result := AL_FORMAT_MONO16;
  End;
End;

Procedure Sound.AttachSource(Source:Pointer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_AttachCount) Do
  If (_AttachList[I] = Source) Then
    Exit;

  Inc(_AttachCount);
  If Length(_AttachList)<_AttachCount Then
    SetLength(_AttachList, _AttachCount);
  _AttachList[Pred(_AttachCount)] := Source;
End;

Procedure Sound.RemoveSource(Source:Pointer);
Var
  N,I:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_AttachCount) Do
  If (_AttachList[I] = Source) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  _AttachList[I] := _AttachList[Pred(_AttachCount)];
  Dec(_AttachCount);
End;

Function GetSoundLoader(Source:TERRAStream):SoundLoader;
Var
  Pos:Cardinal;
  I:Integer;
Begin
  Log(logDebug, 'Sound', 'Getting sound loader for '+Source.Name);

  Result := Nil;
  If Not Assigned(Source) Then
    Exit;

  Pos := Source.Position;

  Log(logDebug, 'Sound',  IntegerProperty.Stringify(_SoundExtensionCount)+ ' sound extensions active');

  For I:=0 To Pred(_SoundExtensionCount) Do
  Begin
    Source.Seek(Pos);

    Log(logDebug, 'Sound', 'Testing sound extension: '+_SoundExtensions[I].Name);

    If _SoundExtensions[I].Validate(Source) Then
    Begin
      Log(logDebug, 'Sound', 'Sound extension  '+_SoundExtensions[I].Name+' matched!');

      Result := _SoundExtensions[I].Loader;

      Log(logDebug, 'Sound', 'Seeking... '+CardinalToString(Pos));
      Source.Seek(Pos);

      Log(logDebug, 'Sound', 'Returning...');
      Exit;
    End;
  End;


  Log(logWarning, 'Sound', 'No sound extensions matched!');
  Result := NIl;
End;

Function Sound.Load(Source:TERRAStream):Boolean;
Var
  I:Integer;
  Loader:SoundLoader;
Begin
  Loader := GetSoundLoader(Source);
  If Not Assigned(Loader) Then
  Begin
    Result := False;
    RaiseError('Unknown sound format. ['+Source.Name+']');

    {Log(logError, 'Sound', 'Unknown sound format. ['+Source.Name+']');
    _Status := rsInvalid;}

    Exit;
  End;

  Log(logDebug, 'Sound', 'Calling sound loader...');
  Result := Loader(Source, Self);
  SetStatus(rsReady);
End;


Procedure RegisterSoundFormat(Name:TERRAString;
                              Validate:SoundStreamValidateFunction;
                              Loader:SoundLoader;
                              Saver:SoundSaver=Nil);
Var
  I,N:Integer;
Begin
  Name := StringLower(Name);

  For I:=0 To Pred(_SoundExtensionCount) Do
  If (_SoundExtensions[I].Name = Name) Then
    Exit;

  N := _SoundExtensionCount;
  Inc(_SoundExtensionCount);
  SetLength(_SoundExtensions, _SoundExtensionCount);
  _SoundExtensions[N].Name := Name;
  _SoundExtensions[N].Validate :=Validate;
  _SoundExtensions[N].Loader := Loader;
  _SoundExtensions[N].Saver := Saver;
End;

Class Function Sound.GetManager: Pointer;
Begin
  Result := SoundManager.Instance;
End;

End. 
