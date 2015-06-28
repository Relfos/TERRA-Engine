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
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Sound, TERRA_Application, TERRA_Collections, TERRA_Vector3D,
  TERRA_Log, TERRA_SoundSource, TERRA_SoundAmbience, TERRA_Resource, TERRA_ResourceManager, TERRA_AL;

Type
  SoundManager = Class(ResourceManager)
    Protected
      _Context:PALCcontext;
      _Device:PALCdevice;

      _SoundDeviceList:List;
      _SoundCaptureDeviceList:List;

      _Sources:Array Of SoundSource;
      _SourceCount:Integer;

      _Ambience:SoundAmbience;

      _Enabled:Boolean;

      Procedure LoadDeviceList(Var DeviceList:List; QueryID:Integer);

      Procedure Init; Override;
      Procedure Update; Override;

      Procedure UpdatePosition(Position, Direction, Up:Vector3D);

      Procedure SetEnabled(Const Value: Boolean);

    Public
      Procedure Release; Override;

      Function Play(MySound:Sound):SoundSource; Overload;
      Function Play(Const Name:TERRAString):SoundSource; Overload;

      Function Spawn(MySound:Sound; Position:Vector3D; Volume:Single=1.0):SoundSource;
      Procedure Delete(Source:SoundSource);

      Function GetSound(Name:TERRAString; ValidateError:Boolean = True):Sound;

      Procedure ValidateSource(Var Source:SoundSource);

      Class Function Instance:SoundManager;

      Property Ambience:SoundAmbience Read _Ambience;

      Property Enabled:Boolean Read _Enabled Write SetEnabled;
  End;

Implementation
Uses TERRA_Error, TERRA_CollectionObjects, TERRA_GraphicsManager, TERRA_Camera, TERRA_FileManager;

Var
  _SoundManager_Instance:ApplicationObject = Nil;

{ SoundSystem }
Procedure SoundManager.Init;
Var
  Attribs:Array[0..1] Of Integer;
Begin
  Inherited;

  Self.AutoUnload := True;

  LoadOpenAL();

  //Open default sound device
	Log(logDebug, 'Audio','Opening AL device');
  _Device := alcOpenDevice(Nil);

  Attribs[0] := ALC_FREQUENCY;
  {$IFDEF MOBILE}
  Attribs[1] := 22050;
  {$ELSE}
  Attribs[1] := 44100;
  {$ENDIF}

  //Create context
	Log(logDebug, 'Audio','Creating AL context');
  _Context := alcCreateContext(_Device, @Attribs[0]);

  //Set active context
	Log(logDebug, 'Audio','Making context current');
  alcMakeContextCurrent(_Context);

	Log(logDebug, 'Audio','Initializing AL functions');
  InitOpenAL;

  LoadDeviceList(_SoundDeviceList, ALC_DEVICE_SPECIFIER);
  LoadDeviceList(_SoundCaptureDeviceList, ALC_CAPTURE_DEVICE_SPECIFIER);

  alListenerfv(AL_Position, @VectorZero);
  alListenerfv(AL_VELOCITY, @VectorZero);

  //alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED);

  If (Assigned(_SoundDeviceList)) And (_SoundDeviceList.Count<=0) Then
	Log(logWarning, 'Audio','No sound devices found!');

  _Ambience := SoundAmbience.Create;

  _Enabled := True;

  AutoUnload := False;
End;

Procedure SoundManager.Release;
Var
  I:Integer;
Begin
  Inherited;

  For I := 0 To Pred(_SourceCount) Do
    ReleaseObject(_Sources[I]);
    
  SetLength(_Sources, 0);
  _SourceCount := 0;

  ReleaseObject(_Ambience);
  ReleaseObject(_SoundDeviceList);
  ReleaseObject(_SoundCaptureDeviceList);

  If Assigned(_Context) Then
  Begin
    alcMakeContextCurrent(Nil);
    alcDestroyContext(_Context);             //Release context
  End;

  If Assigned(_Device) Then
    alcCloseDevice(_Device);                 //Close device

  FreeOpenAL();

  _SoundManager_Instance := Nil;
End;

Function SoundManager.GetSound(Name:TERRAString; ValidateError:Boolean):Sound;
Var
  S:TERRAString;
Begin
  Result := Nil;
  Name := StringTrim(Name);
  If (Name='') Then
    Exit;

  Result := Sound(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    S := FileManager.Instance().SearchResourceFile(Name+'.wav');
    If (S='') Then
      S := FileManager.Instance().SearchResourceFile(Name+'.ogg');

    If S<>'' Then
    Begin
      Result := Sound.Create(rtLoaded, S);
      Self.AddResource(Result);
    End Else
    If ValidateError Then
      RaiseError('Could not find sound resource. ['+Name +']');
  End;
End;


Class function SoundManager.Instance: SoundManager;
Begin
  If Not Assigned(_SoundManager_Instance) Then
    _SoundManager_Instance := InitializeApplicationComponent(SoundManager, Nil);

  Result := SoundManager(_SoundManager_Instance.Instance);
End;

Procedure SoundManager.LoadDeviceList(var DeviceList:List; QueryID: Integer);
Var
  S:TERRAString;
  List:PAnsiChar;
Begin
  S := '';
  If Not Assigned(DeviceList) Then
  Begin
    DeviceList := TERRA_Collections.List.Create(collection_Unsorted);
    List := alcGetString(Nil,QueryID);
    If Assigned(List) Then
    While (List^<>#0) Do
    Begin
      S := S + List^;
      Inc(List);
      If (List^=#0) Then
      Begin
        DeviceList.Add(StringObject.Create(S));
	      Log(logDebug, 'Audio', 'Device: '+ S);
        S:='';
        Inc(List);
      End;
    End;
  End;
End;

Procedure SoundManager.Update;
Var
  I:Integer;
  Cam:Camera;
Begin
  Inherited;

(*  If (GraphicsManager.Instance().ActiveViewport = Nil) Then
    Exit;

  Cam := GraphicsManager.Instance().ActiveViewport.Camera;
  UpdatePosition(Cam.Position, Cam.View, Cam.Up);*)

  I := 0;
  While (I<_SourceCount) Do
  If (_Sources[I].Waiting) Then
  Begin
    If (_Sources[I].Sound.IsReady) Then
      _Sources[I].Start;
    Inc(I);
  End Else
  If (Not _Sources[I].Loop)  And (_Sources[I].Status = sndStopped) Then
  Begin
    _Sources[I].OnFinish();
    ReleaseObject(_Sources[I]);
    _Sources[I] := _Sources[Pred(_SourceCount)];
    Dec(_SourceCount);
  End Else
  Begin
    If Assigned(_Sources[I].Sound) Then
      _Sources[I].Sound.IsReady;
    Inc(I);
  End;
End;

Procedure SoundManager.Delete(Source:SoundSource);
Var
  N, I:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_SourceCount) Do
  If (_Sources[I] = Source) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  ReleaseObject(_Sources[N]);
  _Sources[N] := _Sources[Pred(_SourceCount)];
  _Sources[Pred(_SourceCount)] := Nil;
  Dec(_SourceCount);
End;

Function SoundManager.Play(MySound: Sound): SoundSource;
Begin
  {$IFDEF DISABLESOUND}
  Result := Nil;
  Exit;
  {$ENDIF}

  If (Not Assigned(MySound)) Then
  Begin
    Result := Nil;
    Exit;
  End;

  {$IFNDEF ANDROID}
  If (OpenALHandle=0) Then
  Begin
    Result := Nil;
    Exit;
  End;
  {$ENDIF}

  If (Not _Enabled) Then
  Begin
    Result := Nil;
    Exit;
  End;

  Log(logDebug, 'Sound', 'Playing '+MySound.Name);

  MySound.Prefetch();

  Result := SoundSource.Create();
  Result.Bind(MySound);

  Log(logDebug, 'Sound', 'Setting '+MySound.Name+' position');
  (*
  If Assigned(GraphicsManager.Instance().MainViewport) Then
    Result.Position := GraphicsManager.Instance().MainViewport.Camera.Position
  Else*)
    Result.Position := VectorZero;

  Log(logDebug, 'Sound', 'Registering sound in manager');
  Inc(_SourceCount);
  If Length(_Sources)<_SourceCount Then
    SetLength(_Sources, _SourceCount);
  _Sources[Pred(_SourceCount)] := Result;
End;

Procedure SoundManager.UpdatePosition(Position, Direction, Up:Vector3D);
Var
  Orientation:Record
                Direction:Vector3D;
                Up:Vector3D;
              End;

Begin
  Orientation.Direction := Direction;
  Orientation.Up := Up;

  alListenerfv(AL_POSITION, @Position);
  alListenerfv(AL_ORIENTATION, @Orientation);
End;


Function SoundManager.Spawn(MySound:Sound; Position:Vector3D; Volume:Single): SoundSource;
Begin
  Result := Self.Play(MySound);
  If Assigned(Result) Then
  Begin
    Result.Position := Position;
    Result.Volume := Volume;
  End;
End;

Procedure SoundManager.ValidateSource(var Source: SoundSource);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SourceCount) Do
    If (_Sources[I]=Source) Then
      Exit;

  Source := Nil;
End;

Function SoundManager.Play(Const Name:TERRAString): SoundSource;
Var
  Snd:Sound;
Begin
  Snd := Self.GetSound(Name, False);
  If Snd = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := Self.Play(Snd);
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

  Log(logDebug, 'Audio', S);
End;

End.
