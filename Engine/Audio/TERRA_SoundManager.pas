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
 * TERRA_SoundManager
 * Implements the global sound manager
 ***********************************************************************************************************************
}
Unit TERRA_SoundManager;
{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Sound, TERRA_Application, TERRA_Collections, TERRA_Vector3D,
  TERRA_Log, TERRA_AudioMixer, TERRA_SoundSource, TERRA_SoundAmbience, TERRA_Resource, TERRA_ResourceManager;

Type
  SoundManager = Class(ResourceManager)
    Protected
      _Mixer:TERRAAudioMixer;

      _Enabled:Boolean;

      Procedure UpdatePosition(Position, Direction, Up:Vector3D);

      Procedure SetEnabled(Const Value: Boolean);

    Public
      Constructor Create();

      Function Play(Sound:TERRASound):SoundSource; Overload;
      Function Play(Const Name:TERRAString):SoundSource; Overload;

      Procedure Delete(Source:SoundSource);

      Function GetItem(Name:TERRAString):TERRASound;

      Property Mixer:TERRAAudioMixer Read _Mixer;

      Property Enabled:Boolean Read _Enabled Write SetEnabled;

      Property Sounds[Name:TERRAString]:TERRASound Read GetItem; Default;
  End;

Implementation
Uses TERRA_Error, TERRA_FileUtils, TERRA_FileManager, TERRA_Engine, TERRA_FileFormat;


{ SoundManager }
Constructor SoundManager.Create();
Var
  Attribs:Array[0..1] Of Integer;
Begin
  Inherited;

  Self.AutoUnload := True;

	Engine.Log.Write(logDebug, 'Audio','Initializing audio mixer');
  _Mixer := TERRAAudioMixer.Create(DefaultSampleFrequency, DefaultAudioSampleCount);

  _Enabled := True;

  AutoUnload := False;
End;

Function SoundManager.GetItem(Name:TERRAString):TERRASound;
Var
  Format:TERRAFileFormat;
  Location:TERRALocation;
Begin
  Result := Nil;

  Name := StringTrim(Name);
  Name := GetFileName(Name, True);
  If (Name='') Then
    Exit;

  Result := TERRASound(GetResource(Name));
  If Assigned(Result) Then
    Exit;

  Format := Engine.Formats.FindLocationFromName(Name, TERRASound, Location);

  If Assigned(Format) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Found '+S+'...');{$ENDIF}

    Result := TERRASound.Create(rtLoaded, Location);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture class instantiated sucessfully!');{$ENDIF}

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture loading priority set!');{$ENDIF}

    Self.AddResource(Result);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture added to manager!');{$ENDIF}
    Exit;
  End;

  //RaiseError('Could not find sound. ['+Name+']');
End;

Procedure SoundManager.Delete(Source:SoundSource);
Begin
  If Source = Nil Then
    Exit;
    
  _Mixer.RemoveSource(Source);
  ReleaseObject(Source);
End;

Function SoundManager.Play(Const Name:TERRAString): SoundSource;
Var
  Snd:TERRASound;
Begin
  Snd := Self.GetItem(Name);
  If Snd = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := Self.Play(Snd);
End;

Function SoundManager.Play(Sound:TERRASound): SoundSource;
Begin
  {$IFDEF DISABLESOUND}
  Result := Nil;
  Exit;
  {$ENDIF}

  If (Not Assigned(Sound)) Then
  Begin
    Result := Nil;
    Exit;
  End;

  If (Not _Enabled) Then
  Begin
    Result := Nil;
    Exit;
  End;

  Engine.Log.Write(logDebug, 'Sound', 'Playing '+Sound.Name);

  Sound.Prefetch();

  Result := ResourceSoundSource.Create(Sound);

  Engine.Log.Write(logDebug, 'Sound', 'Setting '+Sound.Name+' position');

  Result.Position := Vector3D_Zero;

  Engine.Log.Write(logDebug, 'Sound', 'Registering sound in mixer');
  _Mixer.AddSource(Result);
End;

Procedure SoundManager.UpdatePosition(Position, Direction, Up:Vector3D);
Begin
  //TODO
End;


Procedure SoundManager.SetEnabled(const Value: Boolean);
Var
  S:TERRAString;
Begin
  If (Self._Enabled = Value) Then
    Exit;

  _Enabled := Value;
  
  If (Value) Then
    S := 'Enabling audio...'
  Else
    S := 'Disabling audio...';

  Engine.Log.Write(logDebug, 'Audio', S);
End;

End.
