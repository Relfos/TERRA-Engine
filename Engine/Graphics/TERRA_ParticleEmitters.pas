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
 * TERRA_ParticleEmitters
 * Implements particle emitters
 ***********************************************************************************************************************
}

Unit TERRA_ParticleEmitters;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Stream, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_ParticleRenderer;

Const
  particleEmitterSphere   = 0;
  particleEmitterCylinder = 1;

  // property generation flags for custom emitters
{  particleGenPosition   = 1;
  particleGenDirection  = 2;
  particleGenSpeed      = 4;
  particleGenLife       = 8;
  particleGenSize       = 16;
  particleGenColor      = 32;
  particleGenAngle      = 64;
  particleGenRotSpeed   = 128;}
  
Type
  ParticleSettingsEmitter = Class;

  PParticleSettingsEmitterGroup = ^ParticleSettingsEmitterGroup;
  ParticleSettingsEmitterGroup = Object
      SpawnRange:Single;
      SpawnPercent:Integer;
      EmissionRate:Integer;
      MaterialEmission:Single;
      MinSize,MaxSize:Single;
      MinRed,MaxRed:Single;
      MinGreen,MaxGreen:Single;
      MinBlue,MaxBlue:Single;
      MinAlpha,MaxAlpha:Single;
      MinDirection:Vector3D;
      MaxDirection:Vector3D;

      MinSpeed:Single;
      MaxSpeed:Single;

      MinRotationSpeed:Single;
      MaxRotationSpeed:Single;

      MinLife, MaxLife:Single;
      Texture:ParticleType;

      BlendMode:Integer;
      PhysicsMode:Integer;
      ColorMode:Integer;
      FadeMode:Integer;
      EmitterMode:Integer;
      AnimationFrames:Integer;
      AnimationRepeat:Integer;

      Owner:ParticleSettingsEmitter;

      Procedure Reset;
      Procedure Load(S:TERRAString);
      Function GetParticleCount:Integer;
  End;

  ParticleSettingsEmitter = Class(PositionalParticleEmitter)
    Protected
      _Name:TERRAString;
      _Groups:Array Of ParticleSettingsEmitterGroup;
      _GroupCount:Integer;
      _NeedsShuffle:Boolean;
      _Shuffle:Array[0..99] Of Byte;

      Function GetRandomGroup():Integer;

    Public
      Constructor Create(Const FXName:TERRAString; Position:Vector3D);

      Procedure Copy(Source:ParticleSettingsEmitter);

      Procedure SetGroupPercent(Const ID, Value:Integer);

      Procedure Load(Source:Stream);

      Procedure Emit(Target:Particle); Override;
      Function GetParticleCount: Integer; Override;

      Property Name:TERRAString Read _Name;
  End;

Implementation
Uses TERRA_OS, TERRA_Math, TERRA_GraphicsManager, TERRA_Renderer, TERRA_FileManager, TERRA_INI;

{ ParticleSettingsEmitter }
Procedure ParticleSettingsEmitter.Copy(Source: ParticleSettingsEmitter);
Var
  I:Integer;
Begin
  If (Source = Nil) Then
  Begin
    _GroupCount := 0;
    Exit;
  End;

  _GroupCount := Source._GroupCount;
  SetLength(_Groups, _GroupCount);
  For I:=0 To Pred(_GroupCount) Do
  Begin
    _Groups[I] := Source._Groups[I];
    _Groups[I].Owner := Self;
  End;
End;

Constructor ParticleSettingsEmitter.Create(Const FXName:TERRAString; Position:Vector3D);
Var
  S:TERRAString;
  Source:Stream;
Begin
  _Position := Position;
  _GroupCount := 0;
  _NeedsShuffle := True;

  S := FXName + '.fx';
  Source := FileManager.Instance.OpenStream(S);
  If Assigned(Source) Then
  Begin
    Self.Load(Source);
    ReleaseObject(Source);
  End;
End;

Function ParticleSettingsEmitter.GetParticleCount: Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_GroupCount) Do
    Inc(Result, _Groups[I].GetParticleCount);
End;

Procedure ParticleSettingsEmitter.Load(Source: Stream);
Var
  I:Integer;
  S, S2:TERRAString;
Begin
  S := '';
  S2 := '';
  While Not Source.EOF Do
  Begin
    Source.ReadLine(S2);
    S := S + crLf + S2;
  End;

  _GroupCount := 0;
  While S<>'' Do
  Begin
    I := StringCharPos(Ord('{'), S);
    If (I<=0) Then
      Break;

    S := StringCopy(S, I + 1, MaxInt);
    I := StringCharPos(Ord('}'), S);
    If (I<=0) Then
      I := Succ(StringLength(S));

    S2 := StringCopy(S, 1, Pred(I));
    S := StringCopy(S, I + 1, MaxInt);

    Inc(_GroupCount);
    SetLength(_Groups, _GroupCount);
    _Groups[Pred(_GroupCount)].Owner := Self;
    _Groups[Pred(_GroupCount)].Load(S2);
  End;

  _NeedsShuffle := True;
End;

Function ParticleSettingsEmitter.GetRandomGroup: Integer;
Var
  I, J, N, A, B:Integer;
Begin
  If _NeedsShuffle Then
  Begin
    FillChar(_Shuffle, SizeOf(_Shuffle), 0);
    N := 0;
    For I:=0 To Pred(_GroupCount) Do
    Begin
      J := 0;
      While (N<100) And (J<_Groups[I].SpawnPercent) Do
      Begin
        _Shuffle[N] := I;
        Inc(N);
        Inc(J);
      End;
    End;

    For I:=0 To 99 Do
    Begin
      A := Random(100);
      B := Random(100);
      N := _Shuffle[A];
      _Shuffle[A] := _Shuffle[B];
      _Shuffle[B] := N;
    End;
  End;

  N := Random(100);
  Result := _Shuffle[N];
End;

Procedure ParticleSettingsEmitter.SetGroupPercent(const ID, Value: Integer);
Begin
  If (ID<0) Or (ID>=_GroupCount) Then
    Exit;

  _Groups[ID].SpawnPercent := Value;
  _NeedsShuffle := True;
End;

{Function ParticleSettingsEmitter.GetGroupSettings(ID:Integer): PParticleSettingsEmitterGroup;
Begin
  If (ID<0) Or (ID>=_GroupCount) Then
    Result := Nil
  Else
    Result := @(_Groups[ID]);
End;
}

Procedure ParticleSettingsEmitter.Emit(Target:Particle);
Var
  I:Integer;
  N:Single;
  Group:PParticleSettingsEmitterGroup;
  Dir:Vector3D;
  Pos, DestPos:Vector3D;
  LinearSpeed:Single;
  C:Color;
Begin
  Group := @(Self._Groups[GetRandomGroup()]);

  Target.Life := RandomFloat(Group.MinLife, Group.MaxLife);

  If (Target.Life<=0) Then
    Target.Life := 0.001;

  Case Group.EmitterMode Of
    particleEmitterCylinder:
      Pos := VectorScale(VectorCreate(RandomFloat(-1.0, 1.0), 0, RandomFloat(-1.0, 1.0)), Group.SpawnRange);

    Else
      Pos := VectorScale(VectorCreate(RandomFloat(-1.0, 1.0), RandomFloat(-1.0, 1.0), RandomFloat(-1.0, 1.0)), Group.SpawnRange);
  End;

  Pos.Add(Self.Position);

  Dir.X := RandomFloat(Group.MinDirection.X, Group.MaxDirection.X);
  Dir.Y := RandomFloat(Group.MinDirection.Y, Group.MaxDirection.Y);
  Dir.Z := RandomFloat(Group.MinDirection.Z, Group.MaxDirection.Z);
  Dir.Normalize();

  LinearSpeed := RandomFloat(Group.MinSpeed, Group.MaxSpeed);

  {$IFDEF ALLOW_PARTICLE_DYNAMIC_MOVEMENT}
  Target.PosX.MakeConstant(Pos.X);
  Target.PosY.MakeConstant(Pos.Y);
  Target.PosZ.MakeConstant(Pos.Z);

  Target.DirX.MakeConstant(Dir.X);
  Target.DirY.MakeConstant(Dir.Y);
  Target.DirZ.MakeConstant(Dir.Z);

  Target.LinearSpeed.MakeConstant(LinearSpeed);
  {$ELSE}
  DestPos := VectorAdd(Pos, VectorScale(Dir, Target.Life * LinearSpeed * 0.5));

  Target.PosX.AddGraph(Pos.X, DestPos.X, 1.0);
  Target.PosY.AddGraph(Pos.Y, DestPos.Y, 1.0);
  Target.PosZ.AddGraph(Pos.Z, DestPos.Z, 1.0);
{  Target.PosX.MakeConstant(Pos.X);
  Target.PosY.MakeConstant(Pos.Y);
  Target.PosZ.MakeConstant(Pos.Z);}

  {$ENDIF}

  N := RandomFloat(Group.MinSize, Group.MaxSize);
  Target.Size.AddGraph(0, N, 0.15);
  Target.Size.AddGraph(N, N, 0.7);
  Target.Size.AddGraph(N, 0, 0.15);

  {Target.Size.AddGraph(N, N, 0.5);
  Target.Size.AddGraph(N, 0, 0.5);}
  //Target.Size.MakeConstant(N);

  Target.Angle.MakeConstant(RandomFloat(0.0, 360.0 * RAD));

  Target.RotationSpeed.MakeConstant(RandomFloat(Group.MinRotationSpeed, Group.MaxRotationSpeed));

  Case Group.ColorMode Of
    particleColorModeUniform:
      Begin
        C.R := Trunc(255 * RandomFloat(Group.MinRed, Group.MaxRed));
        C.G := C.R;
        C.B := C.R;
        C.A := Trunc(255 * RandomFloat(Group.MinAlpha, Group.MaxAlpha));
      End;

    particleColorModeScaled:
      Begin
        N := RandomFloat;
        C := ColorCreateFromFloat(
              Group.MinRed + (Group.MaxRed - Group.MinRed) * N,
              Group.MinGreen + (Group.MaxGreen - Group.MinGreen) * N,
              Group.MinBlue + (Group.MaxBlue - Group.MinBlue) * N,
              Group.MinAlpha + (Group.MaxAlpha - Group.MinAlpha) * N
              );
      End;
    Else
      Begin
        C := ColorCreateFromFloat(
          RandomFloat(Group.MinRed, Group.MaxRed),
          RandomFloat(Group.MinGreen, Group.MaxGreen),
          RandomFloat(Group.MinBlue, Group.MaxBlue),
          RandomFloat(Group.MinAlpha, Group.MaxAlpha));
      End;
  End;

  Target.Red.MakeConstant(C.R);
  Target.Green.MakeConstant(C.G);
  Target.Blue.MakeConstant(C.B);
  Target.Alpha.MakeConstant(C.A);

  Target.Texture := Group.Texture;
  Target.BlendMode := Group.BlendMode;
  Target.AnimationFrames := Group.AnimationFrames;
  Target.AnimationRepeat := Group.AnimationRepeat;
End;

{ ParticleSettingsEmitterGroup }
Procedure ParticleSettingsEmitterGroup.Reset;
Begin
  SpawnRange := 10.0;
  SpawnPercent := 100;
  MaterialEmission := 0.0;
  EmissionRate := 20;

  MinSize := 0.3;
  MaxSize := 2.0;

  BlendMode := blendBlend;

  ColorMode := particleColorModeDefault;
  PhysicsMode := particlePhysicsModeDefault;
  FadeMode := particleFadeModeDefault;
  EmitterMode := particleEmitterSphere;
  AnimationFrames := 0;
  AnimationRepeat := 1;

  Texture := Nil;

  MinRed := 0.0;
  MaxRed := 1.0;
  MinGreen := 0.0;                            
  MaxGreen := 1.0;
  MinBlue := 0.0;
  MaxBlue := 1.0;
  MinAlpha := 0.5;
  MaxAlpha := 1.0;

  MinDirection := VectorConstant(-1.0);
  MaxDirection := VectorConstant(1.0);

  MinSpeed := 0.8;
  MaxSpeed := 1.2;

  MinRotationSpeed := 0.1;
  MaxRotationSpeed := 0.5;

  MinLife := 3.0;
  MaxLife := 20.0;
End;

Procedure ParticleSettingsEmitterGroup.Load(S:TERRAString);
Var
  Tex:TERRAString;
  Parser:INIParser;
Begin
  Reset();
  StringReplaceText(#9, '', S);
  Parser := INIParser.Create;
  Parser.AddToken('spawnrange', tkFloat, @SpawnRange);
  Parser.AddToken('emissionrate', tkInteger, @EmissionRate);
  Parser.AddToken('materialemission', tkFloat, @MaterialEmission);
  Parser.AddToken('minsize', tkFloat, @MinSize);
  Parser.AddToken('maxsize', tkFloat, @MaxSize);
  Parser.AddToken('minred', tkFloat, @MinRed);
  Parser.AddToken('maxred', tkFloat, @MaxRed);
  Parser.AddToken('mingreen', tkFloat, @MinGreen);
  Parser.AddToken('maxgreen', tkFloat, @MaxGreen);
  Parser.AddToken('minblue', tkFloat, @MinBlue);
  Parser.AddToken('maxblue', tkFloat, @MaxBlue);
  Parser.AddToken('minalpha', tkFloat, @MinAlpha);
  Parser.AddToken('maxalpha', tkFloat, @MaxAlpha);

  Parser.AddToken('mindirx', tkFloat, @MinDirection.X);
  Parser.AddToken('maxdirx', tkFloat, @MaxDirection.X);

  Parser.AddToken('mindiry', tkFloat, @MinDirection.Y);
  Parser.AddToken('maxdiry', tkFloat, @MaxDirection.Y);

  Parser.AddToken('mindirz', tkFloat, @MinDirection.Z);
  Parser.AddToken('maxdirz', tkFloat, @MaxDirection.Z);

  Parser.AddToken('minvelocity', tkFloat, @MinSpeed);
  Parser.AddToken('maxvelocity', tkFloat, @MaxSpeed);

  Parser.AddToken('minrotspeed', tkFloat, @MinRotationSpeed);
  Parser.AddToken('maxrotspeed', tkFloat, @MaxRotationSpeed);

  Parser.AddToken('mindecay', tkFloat, @MinLife);
  Parser.AddToken('maxdecay', tkFloat, @MaxLife);

  Parser.AddToken('physicsmode', tkInteger, @PhysicsMode);
  Parser.AddToken('colormode', tkInteger, @ColorMode);
  Parser.AddToken('fademode', tkInteger, @FadeMode);
  Parser.AddToken('emittermode', tkInteger, @EmitterMode);
  Parser.AddToken('blendmode', tkInteger, @BlendMode);

  Parser.AddToken('percent', tkInteger, @SpawnPercent);
  Parser.AddToken('frames', tkInteger, @AnimationFrames);
  Parser.AddToken('repeat', tkInteger, @AnimationRepeat);
  Parser.AddToken('texture', tkString, @Tex);
  Parser.LoadFromString(S);
  ReleaseObject(Parser);

  Texture := ParticleManager.Instance.GetParticleType(Tex);

  If (MinRed>1.0) Then
    MinRed := MinRed / 255.0;
  If (MaxRed>1.0) Then
    MaxRed := MaxRed / 255.0;

  If (MinGreen>1.0) Then
    MinGreen := MinGreen / 255.0;
  If (MaxGreen>1.0) Then
    MaxGreen := MaxGreen / 255.0;

  If (MinBlue>1.0) Then
    MinBlue := MinBlue / 255.0;
  If (MaxBlue>1.0) Then
    MaxBlue := MaxBlue / 255.0;

  If (MinAlpha>1.0) Then
    MinAlpha := MinAlpha / 255.0;
  If (MaxAlpha>1.0) Then
    MaxAlpha := MaxAlpha / 255.0;
End;

Function ParticleSettingsEmitterGroup.GetParticleCount: Integer;
Begin
  Result := Trunc(EmissionRate * (SpawnPercent/100));
End;

End.