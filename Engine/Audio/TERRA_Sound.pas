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
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Resource, TERRA_Collections, TERRA_AudioBuffer;

Const
  DefaultSampleRate = 44100;

Type
  Sound = Class(TERRAResource)
    Protected
      _Buffer:TERRAAudioBuffer;

      _AttachList:Array Of TERRAObject;
      _AttachCount:Integer;

    Public
      Function Load(Source:TERRAStream):Boolean; Override;
      Function Unload:Boolean; Override;

      Procedure AttachSource(Source:TERRAObject);
      Procedure RemoveSource(Source:TERRAObject);

      Class Function GetManager:TERRAObject; Override;

      Procedure New(Samples, Frequency:Cardinal; Stereo:Boolean; Data:Pointer);

      Property Buffer:TERRAAudioBuffer Read _Buffer;
  End;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Application, TERRA_Log, TERRA_SoundManager, TERRA_SoundSource, TERRA_AudioConverter, TERRA_AudioMixer;

Function Sound.Unload:Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_AttachCount) Do
    SoundManager.Instance().Delete(SoundSource(_AttachList[I]));
  _AttachCount := 0;

  ReleaseObject(_Buffer);

  Result := Inherited Unload();
End;

Procedure Sound.New(Samples, Frequency:Cardinal; Stereo:Boolean; Data:Pointer);
Var
  Temp:TERRAAudioBuffer;
  Converter:AudioRateConverter;
Begin
  ReleaseObject(_Buffer);

  _Buffer := TERRAAudioBuffer.Create(Samples, Frequency, Stereo);
  Move(Data^, _Buffer.Samples^, _Buffer.SizeInBytes);

  If (Frequency <> DefaultSampleFrequency) Then
  Begin
    Converter := AudioRateConverter.Create(_Buffer);
    Temp := _Buffer;
    _Buffer := Converter.Convert(DefaultSampleFrequency);
    ReleaseObject(Converter);
    ReleaseObject(Temp);
  End;

  SetStatus(rsReady);
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

Function GetSoundLoader(Source:Stream):SoundLoader;
Var
  Pos:Cardinal;
  I:Integer;
Begin
  Log(logDebug, 'Sound', 'Getting sound loader for '+Source.Name);

  Result := Nil;
  If Not Assigned(Source) Then
    Exit;

  Pos := Source.Position;

  Log(logDebug, 'Sound', IntToString(_SoundExtensionCount)+ ' sound extensions active');

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

Function Sound.Load(Source:Stream):Boolean;
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
