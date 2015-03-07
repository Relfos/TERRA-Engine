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
 * TERRA_ParticleRender
 * Implements an optimized particle renderer
 ***********************************************************************************************************************
}
Unit TERRA_ParticleRenderer;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Utils, TERRA_GraphicsManager, TERRA_Texture, TERRA_Application,
  TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_Stream, TERRA_Plane,
  TERRA_Matrix4x4, TERRA_Math, TERRA_TextureAtlas, TERRA_BoundingBox,
  TERRA_Shader, TERRA_UI, TERRA_Image,
  TERRA_FileManager;

Const
  particlePhysicsModeDefault  = 0;
  particlePhysicsModeGravity  = 1;

  particleColorModeDefault   = 0;
  particleColorModeUniform   = 1;
  particleColorModeScaled    = 2;

  particleFadeModeDefault    = 0;
  particleFadeModeScale      = 1;

  particleKindPoint = 0;
  particleKindLine  = 1;
  particleKindMesh  = 2;

  particleInterpolateLinear = 0;
  particleInterpolateCubic = 1;
  particleInterpolateCatmullRom = 2;
  particleInterpolateHermite = 3;

  ParticleQuadOffsets:Array[0..3] Of Vector2D = (
    (X:-1.0; Y:1.0),
    (X: 1.0; Y:1.0),
    (X: 1.0; Y:-1.0),
    (X:-1.0; Y:-1.0)
    );

  ParticleUVOffsets:Array[0..3] Of Vector2D = (
    (X:0.0; Y:0.0),
    (X:1.0; Y:0.0),
    (X:1.0; Y:1.0),
    (X:0.0; Y:1.0)
    );

Type
  ParticleCollection = Class;

  PParticleVertex = ^ParticleVertex;
  ParticleVertex = Packed Record
    Position:Vector3D;
    Ofs:Vector2D;
    Size:Vector2D;
    Angles:Vector2D; // cos / sin of rotation
    UV:Vector2D;
    Color:TERRA_Color.Color;
  End;

  ParticleType = Class(TERRAObject)
    Name:TERRAString;
    Item:TextureAtlasItem;
    U1,V1,U2,V2:Single;
  End;

  ParticlePropertyGraph = Record
    StartTime:Single;
    EndTime:Single;
    InvDuration:Single; // optimization, trading a bit of memory to avoid a division and a subtraction per particle

    StartValue:Single;
    EndValue:Single;
  End;

  ParticleProperty = Object
    Graphs:Array Of ParticlePropertyGraph;
    GraphCount:Integer;
    CurrentGraph:Integer;

    SamplingMethod:Integer;
    CurrentValue:Single;
    MinValue:Single;
    MaxValue:Single;

    Procedure Clear();
    Procedure AddGraph(StartValue, EndValue, Duration:Single); // duration in 0 to 1

    Procedure MakeConstant(Value:Single);

    Function Evaluate(T:Single):Single; // T in 0.0 to 1.0
  End;

  ParticleEmitter = Class;

  PParticle = ^Particle;
  Particle = Object
    Private
      Vertices:Array[0..3] Of PParticleVertex;

      //Group:PParticleSettingsGroup;

      CurrentTime:Single;
      CurrentFrame:Integer;

    Public
      Life:Single;    // duration, in miliseconds
      Kind:Integer;
      BlendMode:Integer;
      Texture:ParticleType;
      AnimationFrames:Integer;

      PosX:ParticleProperty;
      PosY:ParticleProperty;
      PosZ:ParticleProperty;

      {$IFDEF ALLOW_PARTICLE_DYNAMIC_MOVEMENT}
      DirX:ParticleProperty;
      DirY:ParticleProperty;
      DirZ:ParticleProperty;
      {$ENDIF}

      Size:ParticleProperty;
      Rotation:ParticleProperty;
      Angle:ParticleProperty;

      {$IFDEF ALLOW_PARTICLE_DYNAMIC_MOVEMENT}
      LinearSpeed:ParticleProperty;
      AngularSpeed:ParticleProperty;
      AngularDistance:ParticleProperty;
      {$ENDIF}
      RotationSpeed:ParticleProperty;

      Red:ParticleProperty;
      Green:ParticleProperty;
      Blue:ParticleProperty;
      Alpha:ParticleProperty;

      TrailLength:ParticleProperty;
      TrailRed:ParticleProperty;
      TrailGreen:ParticleProperty;
      TrailBlue:ParticleProperty;
      TrailAlpha:ParticleProperty;

      Procedure ClearGraphs();

      Procedure Reset(Emitter:ParticleEmitter);
      Function Update(Const Delta:Single; Const Landscape:Boolean):Boolean;
  End;

  ParticleEmitter = Class(TERRAObject)
    Protected
      _EmissionRate:Integer;
      _EmissionPerMinute:Single;
      _LastEmission:Cardinal;

      Procedure SetEmission(Value:Single);

    Public
      Procedure Emit(Target:PParticle); Virtual; Abstract;
      Function GetParticleCount: Integer; Virtual;

      Property EmissionPerMinute:Single Read _EmissionPerMinute Write SetEmission;
  End;

  PositionalParticleEmitter = Class(ParticleEmitter)
    Protected
      _Position:Vector3D;

    Public
      Property Position:Vector3D Read _Position Write _Position;
  End;


  ParticleCollection = Class(Renderable)
    Protected
      _Box:BoundingBox;
      _Init:Boolean;

      _Particles:Array Of Particle;
      _ParticleCount:Integer;

      _Vertices:Array Of ParticleVertex;
      _Temp:Array Of ParticleVertex;
      _VertexCount:Integer;

      _BlendModes:Array Of Integer;
      _BlendModeCount:Integer;
      _BlendModeHashes:Array[0..255] Of Byte;

      _Shader:Shader;

      //_Position:Vector3D;
      _LastTime:Integer;
      _StartTime:Cardinal;

      _Delta:Single;

      _ActiveCount:Integer;
      _Finished:Boolean;

      _Emitter:ParticleEmitter;
      //_GenFlags:Cardinal;

      Procedure AddBlendMode(Mode:Integer);
      Procedure UpdateBlendModes();

      // returns how many vertices processed
      Function UpdateBatch(BlendMode:Integer):Integer;

      Procedure Init;

    Public
      //Settings:ParticleSettings;
      Respawn:Boolean;

      Constructor Create(Emitter:ParticleEmitter{; Pos:Vector3D});
      Procedure Release; Override;

      Procedure Update; Override;

      Function IsOpaque():Boolean; Override;
      Function IsTranslucent():Boolean; Override;

      Procedure Reset;

      Procedure Render(TranslucentPass:Boolean); Override;
      Function GetBoundingBox:BoundingBox; Override;

      Property ActiveCount:Integer Read _ActiveCount;
      Property Finished:Boolean Read _Finished;

      Property Emitter:ParticleEmitter Read _Emitter;
      //Property Position:Vector3D Read _Position Write _Position;
  End;

  ParticleManager = Class(ApplicationComponent)
    Protected
      _Instance:ParticleManager;
      _TextureAtlas:TextureAtlas;

      _NormalTexture:Texture;
      _NormalImage:Image;

      _GlowTexture:Texture;
      _GlowImage:Image;

      _RefractionTexture:Texture;
      _RefractionImage:Image;

      _Types:Array Of ParticleType;
      _TypeCount:Integer;

      _NeedsRebuild:Boolean;

      _Shader:Shader;

      _ParticleCollections:Array Of ParticleCollection;
      _ParticleCollectionCount:Integer;

      Function GetShader:Shader;

    Public
      Class Function Instance:ParticleManager;

      Procedure Release; Override;

      Procedure Init; Override;
      //Procedure Update; Override;

      Procedure Clear;
      Procedure Render;

      Function GetParticleType(Name:TERRAString):ParticleType;

      Procedure AddParticleCollection(Particles:ParticleCollection);
      //Procedure Spawn(Name:TERRAString; Position:Vector3D);
      Procedure Spawn(Emitter:ParticleEmitter);

      Function GetTexture(Target:ParticleCollection):Texture;

      Property Shader:TERRA_Shader.Shader Read GetShader;
  End;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Log, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Camera, TERRA_Mesh,
  TERRA_INI, TERRA_FileStream, TERRA_FileUtils;

Var
  _ParticleManager_Instance:ApplicationObject = Nil;

Function GetShader_Particles():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 120 }');
  Line('vertex {');
  Line('	varying mediump vec4 texCoord;');
  Line('	varying lowp vec4 diffuse;');
  Line('	uniform highp vec3 cameraPosition;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec4 terra_UV0;');
  Line('  attribute mediump vec2 terra_ofs;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  attribute mediump vec2 terra_size;');
  Line('  attribute mediump vec2 terra_angle;');
  Line('	uniform mat4 cameraMatrix;');
  Line('	uniform mat4 projectionMatrix;');
  Line('  uniform mat4 reflectionMatrix;');
  Line('	uniform mediump vec3 cameraRight;');
  Line('	uniform mediump vec3 cameraUp;');
//  Line('	uniform highp float ratio;');
  Line('	void main()	{');
  Line('		texCoord = terra_UV0;');
  Line('		diffuse = terra_color;	');
  Line('		highp vec4 world_position = terra_position;');
  Line('    world_position = reflectionMatrix * world_position;');
  Line('    highp vec2 pp = terra_size * terra_ofs;');
  Line('    pp = vec2(pp.x * terra_angle.x - pp.y * terra_angle.y, pp.x * terra_angle.y + pp.y * terra_angle.x);');
  Line('		world_position.xyz += (pp.x * cameraRight + pp.y * cameraUp);');//  Line('		world_position.xyz += (ratio * pp.x * cameraRight + pp.y * cameraUp);');
  Line('		gl_Position = projectionMatrix * cameraMatrix * world_position;}');
  Line('}');
  Line('fragment {');
  Line('	uniform sampler2D texture0;');
  Line('	uniform highp vec3 cameraPosition;');
  Line('	uniform lowp vec4 sunColor;');
  Line('	varying mediump vec4 texCoord;');
  Line('	varying lowp vec4 diffuse;');
  Line('	void main()	{');
  Line('	  lowp vec4 color = texture2D(texture0, texCoord.st) * diffuse;');
  Line('    if (color.a<0.1) discard;');
  Line('    color *= sunColor;');
  Line('		gl_FragColor = color;}');
  Line('}');
  Result := S;
End;

{ ParticleCollection }
{Procedure ParticleCollection.ResetSettings;
Begin
  Settings.Copy(_SettingsTemplate);
End;}

Constructor ParticleCollection.Create(Emitter:ParticleEmitter);
Var
  Count:Integer;
Begin
  _Shader := ParticleManager.Instance.Shader;

  {_SettingsTemplate := ParticleManager.Instance.GetSettings(SettingName);
  Settings := ParticleSettings.Create;
  ResetSettings;}

  _Emitter := Emitter;
  Count := _Emitter.GetParticleCount;
  _VertexCount := Count * 6;
  _ParticleCount := Count;
  _LastTime := Application.Instance.GetElapsedTime();

  _Emitter._LastEmission := _LastTime;
  _Emitter._EmissionPerMinute := -1;

  _Init := True;
  //_Position := Pos;
  Respawn := True;
End;

Procedure ParticleCollection.Release;
Begin
  Inherited;

  ReleaseObject(_Emitter);
End;

Procedure ParticleCollection.Init;
Var
  I,J:Integer;
Begin
  SetLength(_Vertices, _VertexCount);
  SetLength(_Temp, _VertexCount);
  SetLength(_Particles, _ParticleCount);
  For I:=0 To Pred(_ParticleCount) Do
  Begin
    For J:=0 To 3 Do
      _Particles[I].Vertices[J] := @(_Vertices[I*4 + J]);

    If (Self.Emitter.EmissionPerMinute<0) Then
      _Particles[I].Reset(Self.Emitter);
  End;

  _StartTime := Application.Instance.GetElapsedTime();
  _Init := False;
End;


Procedure ParticleCollection.Update;
Var
  I, Count:Integer;
  CurrentTime:Integer;
  P:Vector3D;
  Landscape:Boolean;
Begin
  If _Init Then
  Begin
    Init;
    Exit;
  End;

  CurrentTime := Application.Instance.GetElapsedTime();
  _Finished := (_ActiveCount = 0) And (CurrentTime - _StartTime > 200);
  _Delta := (CurrentTime-_LastTime);
  If (_Delta<50) Then
    Exit;

  If (Self.Emitter.EmissionPerMinute>0) And (Self.Respawn) Then
  Begin
    Count := Trunc(((CurrentTime - Emitter._LastEmission)/1000) * Emitter.EmissionPerMinute);
    I := 0;
    If (Count>0) Then
    Begin
      Emitter._LastEmission := CurrentTime;
      While (Count>0) And (I<_ParticleCount) Do
      Begin
        If (_Particles[I].Life<=0.0) Then
        Begin
          _Particles[I].Reset(Emitter);
          Dec(Count);
        End;

        Inc(I);
      End;
    End;
  End Else
    Emitter._LastEmission := CurrentTime;

  _Delta := _Delta/1000.0;
  _LastTime := CurrentTime;
  _ActiveCount := 0;

  If (Application.Instance.Paused) Or (Not Assigned(_Particles)) Then
    Exit;

  _Box.Reset;
  Count := Length(_Particles);

{  If (Settings.Wrap) Then
  Begin
    For I:=0 To Pred(Count) Do
    Begin
        P := @(_Particles[I]);
        P.Update(Self, _Delta);

        While (P.Position.X<_WrapBox.StartVertex.X) Do
          P.Position.X := P.Position.X + ParticleWrapSize;
        While (P.Position.X>_WrapBox.EndVertex.X) Do
          P.Position.X := P.Position.X - ParticleWrapSize;

        While (P.Position.Y<_WrapBox.StartVertex.Y) Do
          P.Position.Y := P.Position.Y + ParticleWrapSize;
        While (P.Position.Y>_WrapBox.EndVertex.Y) Do
          P.Position.Y := P.Position.Y - ParticleWrapSize;

        While (P.Position.Z<_WrapBox.StartVertex.Z) Do
          P.Position.Z := P.Position.Z + ParticleWrapSize;
        While (P.Position.Z>_WrapBox.EndVertex.Z) Do
          P.Position.Z := P.Position.Z - ParticleWrapSize;

        _Box.Add(_Particles[I].Position);
    End;
  End Else}
  Begin
    Landscape := IsLandscapeOrientation(Application.Instance.Orientation);
    For I:=0 To Pred(Count) Do
    Begin
      If (Not _Particles[I].Update(_Delta, Landscape)) Then
      Begin
        If (Self.Respawn) Then
          _Particles[I].Reset(Emitter)
        Else
          Continue;
      End;

      Inc(_ActiveCount);

      P.X := _Particles[I].PosX.CurrentValue;
      P.Y := _Particles[I].PosY.CurrentValue;
      P.Z := _Particles[I].PosZ.CurrentValue;
      _Box.Add(P);
    End;
  End;
End;

Function ParticleCollection.UpdateBatch(BlendMode: Integer): Integer;
Var
  I, N:Integer;
  CC:Color;
  RenderStage:Integer;
  Angles:Vector2D;
Begin
  Result := 0;
  I := 0;
  N := 0;

  RenderStage := GraphicsManager.Instance.RenderStage;

  While N<_ParticleCount Do
  Begin
    If (_Particles[N].Life > 0.0) And ((RenderStage<>renderStageDiffuse) Or (_Particles[N].BlendMode = BlendMode)) Then
    Begin
      CC.R := Trunc(_Particles[N].Red.CurrentValue);
      CC.G := Trunc(_Particles[N].Green.CurrentValue);
      CC.B := Trunc(_Particles[N].Blue.CurrentValue);
      CC.A := Trunc(_Particles[N].Alpha.CurrentValue);

      Angles.X := Cos(_Particles[N].Rotation.CurrentValue);
      Angles.Y := Sin(_Particles[N].Rotation.CurrentValue);
      {Angles.X := 0;
      Angles.Y := 1;}

      _Temp[Result + 0] := _Vertices[I];
      _Temp[Result + 0].Color := CC;
      _Temp[Result + 0].Angles := Angles;
      _Temp[Result + 5] := _Temp[Result + 0];
      Inc(I);

      _Temp[Result + 1] := _Vertices[I];
      _Temp[Result + 1].Color := CC;
      _Temp[Result + 1].Angles := Angles;
      Inc(I);

      _Temp[Result + 2] := _Vertices[I];
      _Temp[Result + 2].Color := CC;
      _Temp[Result + 2].Angles := Angles;
      _Temp[Result + 3] := _Temp[Result + 2];
      Inc(I);

      _Temp[Result + 4] := _Vertices[I];
      _Temp[Result + 4].Color := CC;
      _Temp[Result + 4].Angles := Angles;
      Inc(I);

      Inc(Result, 6);
    End Else
      Inc(I, 4);

    Inc(N);
  End;
End;

Procedure ParticleCollection.Render(TranslucentPass:Boolean);
Var
  I, RenderCount:Integer;
//  Ratio:Single;
  Right, Up:Vector3D;
  PositionHandle, UVHandle, OfsHandle, SizeHandle, AnglesHandle, ColorHandle:Integer;
Begin
  If (Not TranslucentPass) Then
    Exit;

  {If (_Init) Then
    Self.Update;}

  {If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
  Begin
    Up := GraphicsManager.Instance.ActiveViewport.Camera.Right;
    Right := GraphicsManager.Instance.ActiveViewport.Camera.Up;
    Up.Scale(-1.0);
  End Else}
  Begin
    Right := GraphicsManager.Instance.ActiveViewport.Camera.Right;
    Up := GraphicsManager.Instance.ActiveViewport.Camera.Up;
  End;

  //Ratio := GraphicsManager.Instance.ActiveViewport.Height / GraphicsManager.Instance.ActiveViewport.Width;
//  Ratio := 1.0; //GraphicsManager.Instance.ActiveViewport.Height / GraphicsManager.Instance.ActiveViewport.Width;

  ShaderManager.Instance.Bind(_Shader);

  PositionHandle := _Shader.GetAttribute('terra_position');
  UVHandle := _Shader.GetAttribute('terra_UV0');
  OfsHandle := _Shader.GetAttribute('terra_ofs');
  SizeHandle := _Shader.GetAttribute('terra_size');
  AnglesHandle := _Shader.GetAttribute('terra_angle');
  ColorHandle := _Shader.GetAttribute('terra_color');

  If (PositionHandle<0) Then
    Exit;

  GraphicsManager.Instance.ActiveViewport.Camera.SetupUniforms;

  _Shader.SetUniform('sunColor', ColorWhite);
  _Shader.SetUniform('cameraUp', Up);
  _Shader.SetUniform('cameraRight', Right);
  _Shader.SetUniform('texture0', 0);
//  _Shader.SetUniform('ratio', Ratio);
  _Shader.SetUniform('reflectionMatrix', GraphicsManager.Instance.ReflectionMatrix);

  Self.UpdateBlendModes();


  For I:=0 To Pred(_BlendModeCount) Do
  Begin
    RenderCount := Self.UpdateBatch(_BlendModes[I]);

    If (I=0) Then
    Begin
      glDepthMask(False);
      ParticleManager.Instance.GetTexture(Self).Bind(0);
    End;

    If (RenderCount>0) Then
    Begin
      GraphicsManager.Instance.SetBlendMode(_BlendModes[I]);

      glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(ParticleVertex), @(_Temp[0].Position));
      glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, SizeOf(ParticleVertex), @(_Temp[0].UV));
      glVertexAttribPointer(OfsHandle, 2, GL_FLOAT, False, SizeOf(ParticleVertex), @(_Temp[0].Ofs));
      If SizeHandle>=0 Then
        glVertexAttribPointer(SizeHandle, 2, GL_FLOAT, False, SizeOf(ParticleVertex), @(_Temp[0].Size));
      If AnglesHandle>=0 Then
        glVertexAttribPointer(AnglesHandle, 2, GL_FLOAT, False, SizeOf(ParticleVertex), @(_Temp[0].Angles));
      glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(ParticleVertex), @(_Temp[0].Color));

      glDrawArrays(GL_TRIANGLES, 0, RenderCount);
      GraphicsManager.Instance.Internal(0, RenderCount Div 3);
    End;
  End;

  glDepthMask(True);

  GraphicsManager.Instance.SetBlendMode(blendNone);
End;

Procedure ParticleCollection.Reset;
Var
  I:Integer;
Begin
  _Init := True;
  For I:=0 To Pred(Length(_Particles)) Do
    _Particles[I].Life := 0.0;
End;

Function ParticleCollection.GetBoundingBox:BoundingBox;
Begin
  Result := _Box;
End;

Function ParticleCollection.IsOpaque: Boolean;
Begin
  Result := False;
End;

Function ParticleCollection.IsTranslucent: Boolean;
Begin
  Result := True;
End;


{Procedure ParticleCollection.SetCustomEmitter(Emitter: ParticleCustomEmitter; GenFlags:Cardinal; UserData: Pointer);
Begin
  Self._CustomEmitter := Emitter;
  Self._GenFlags := GenFlags;
  Self._UserData := UserData;
End;}

Procedure ParticleCollection.AddBlendMode(Mode:Integer);
Var
  I:Integer;
Begin
  If (Mode<0) Or (Mode>255) Then
    Exit;

  If (_BlendModeHashes[Mode]>0) Then
    Exit;

  _BlendModeHashes[Mode] := 1;

  Inc(_BlendModeCount);

  If (Length(_BlendModes)<_BlendModeCount) Then
    SetLength(_BlendModes, _BlendModeCount);

  _BlendModes[Pred(_BlendModeCount)] := Mode;
End;

Procedure ParticleCollection.UpdateBlendModes;
Var
  I:Integer;
Begin
  _BlendModeCount := 0;

  FillChar(_BlendModeHashes, SizeOf(_BlendModeHashes), 0);

  If (GraphicsManager.Instance.RenderStage <> renderStageDiffuse) Then
  Begin
    If (GraphicsManager.Instance.RenderStage = renderStageRefraction) Then
      AddBlendMode(blendBlend)
    Else
      AddBlendMode(blendNone);
    Exit;
  End;

  For I:=0 To Pred(_ParticleCount) Do
    AddBlendMode(_Particles[I].BlendMode);
End;

{ ParticleManager }

Procedure ParticleManager.AddParticleCollection(Particles: ParticleCollection);
Var
  I:Integer;
Begin
  If Particles = Nil Then
    Exit;

  For I:=0 To Pred(_ParticleCollectionCount) Do
  If (_ParticleCollections[I] = Particles) Then
    Exit;

  //Particles.Update();
  Particles.Respawn := False;    // make sure this stops emitting particles after some time

  Inc(_ParticleCollectionCount);
  SetLength(_ParticleCollections, _ParticleCollectionCount);
  _ParticleCollections[Pred(_ParticleCollectionCount)] := Particles;
End;

Procedure ParticleManager.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ParticleCollectionCount) Do
    _ParticleCollections[I].Release;

  _ParticleCollectionCount := 0;
End;

Procedure ParticleManager.Init;
Begin
  _NeedsRebuild := False;
  _TextureAtlas := TextureAtlas.Create('particle', 512, 512);
  _NormalImage := Image.Create(_TextureAtlas.Width, _TextureAtlas.Height);
  _GlowImage := Image.Create(_TextureAtlas.Width, _TextureAtlas.Height);
  _RefractionImage := Image.Create(_TextureAtlas.Width, _TextureAtlas.Height);
End;

Procedure ParticleManager.Release;
Var
  I:Integer;
Begin
  Clear();

  For I:=0 To Pred(_TypeCount) Do
    _Types[I].Release;
  _TypeCount := 0;

  If Assigned(_TextureAtlas) Then
    _TextureAtlas.Release;

  If Assigned(_NormalTexture) Then
    _NormalTexture.Release;

  If Assigned(_NormalImage) Then
    _NormalImage.Release;

  If Assigned(_GlowTexture) Then
    _GlowTexture.Release;

  If Assigned(_GlowImage) Then
    _GlowImage.Release;

  If Assigned(_RefractionTexture) Then
    _RefractionTexture.Release;

  If Assigned(_RefractionImage) Then
    _RefractionImage.Release;

  _ParticleManager_Instance := Nil;
End;

Function ParticleManager.GetParticleType(Name:TERRAString): ParticleType;
Var
  I:Integer;
  S:TERRAString;
  Source:Image;
Begin
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  Name := StringUpper(GetFileName(Name, True));
  For I:=0 To Pred(_TypeCount) Do
  If (_Types[I].Name = Name) Then
  Begin
    Result := (_Types[I]);
    Exit;
  End;

  // not loaded yet, search for isso
  If (Pos('.', Name)<=0) Then
    Name := Name + '.png';

  Inc(_TypeCount);
  SetLength(_Types, _TypeCount);
  Result := ParticleType.Create;
  Result.Name := StringUpper(GetFileName(Name,True));
  Result.Item := _TextureAtlas.Get(Name);
  _Types[Pred(_TypeCount)] := Result;

  If Not Assigned(Result.Item) Then
  Begin
    S := FileManager.Instance.SearchResourceFile(Name);
    If S<>'' Then
    Begin
      Source := Image.Create(S);
      Result.Item := _TextureAtlas.Add(Source, Name);
      _NeedsRebuild := True;
      Source.Release;
    End Else
    Begin
      Result := Nil;
      RaiseError('ParticleManager: Cannot find '+Name);
    End;
  End;
End;

{Function ParticleManager.GetSettings(Name:TERRAString): ParticleSettings;
Var
  S:TERRAString;
  I:Integer;
  Source:Stream;
Begin
  S := StringUpper(GetFileName(Name, True));
  For I:=0 To Pred(_SettingsCount) Do
  If (_Settings[I]._Name = S) Then
  Begin
    Result := _Settings[I];
    Exit;
  End;

  If (Pos('.',Name)<=0) Then
    Name := Name + '.fx';

  S := FileManager.Instance.SearchResourceFile(Name);
  If S<>'' Then
  Begin
    Result := ParticleSettings.Create;
    Result._Name := StringUpper(GetFileName(Name, True));
    Inc(_SettingsCount);
    SetLength(_Settings, _SettingsCount);
    _Settings[Pred(_SettingsCount)] := Result;

    Source := FileManager.Instance.OpenStream(S);
    If Assigned(Source) Then
    Begin
      Result.Load(Source);
      Source.Release;
    End;
  End Else
  Begin
    Result := Nil;
    Log(logError, 'Particles', 'Could not find particle file ' + Name);
  End;
End;}

Function ParticleManager.GetShader: Shader;
Begin
  If (_Shader = Nil) Then
  Begin
    _Shader := TERRA_Shader.Shader.CreateFromString(GetShader_Particles(), 'particles');
    ShaderManager.Instance.AddShader(_Shader);
  End;

  Result := _Shader;
End;

Function ParticleManager.GetTexture(Target:ParticleCollection): Texture;
Var
  I:Integer;
  Source:Image;
  Item:TextureAtlasItem;
  S:TERRAString;
Begin
  If Not Assigned(_TextureAtlas) Then
  Begin
    Result := Nil;
    Exit;
  End;

  If _NeedsRebuild Then
  Begin
    _NeedsRebuild := False;
    _TextureAtlas.Update;

    For I:=0 To Pred(_TextureAtlas.ItemCount) Do
    Begin
      Item := _TextureAtlas.Get(I);

      S := StringLower(GetFileName(Item.Name, True))+'_normal.png';
      S := FileManager.Instance.SearchResourceFile(S);
      If S<>'' Then
        Source := Image.Create(S)
      Else
      Begin
        Source := Image.Create(Item.Buffer.Width, Item.Buffer.Height);
        Source.FillRectangleByUV(0, 0, 1, 1, ColorNull);
      End;
      _NormalImage.Blit(Trunc(Item.X*_TextureAtlas.Width), Trunc(Item.Y*_TextureAtlas.Height), 0, 0, Pred(Source.Width), Pred(Source.Height), Source);
      Source.Release();

      S := StringLower(GetFileName(Item.Name, True))+'_glow.png';
      S := FileManager.Instance.SearchResourceFile(S);
      If S<>'' Then
        Source := Image.Create(S)
      Else
      Begin
        Source := Image.Create(Item.Buffer.Width, Item.Buffer.Height);
        Source.FillRectangleByUV(0, 0, 1, 1, ColorNull);
      End;
      _GlowImage.Blit(Trunc(Item.X*_TextureAtlas.Width), Trunc(Item.Y*_TextureAtlas.Height), 0, 0, Pred(Source.Width), Pred(Source.Height), Source);
      Source.Release;

      S := StringLower(GetFileName(Item.Name, True))+'_refraction.png';
      S := FileManager.Instance.SearchResourceFile(S);
      If S<>'' Then
        Source := Image.Create(S)
      Else
      Begin
        Source := Image.Create(Item.Buffer.Width, Item.Buffer.Height);
        Source.FillRectangleByUV(0, 0, 1, 1, ColorNull);
      End;
      _RefractionImage.Blit(Trunc(Item.X*_TextureAtlas.Width), Trunc(Item.Y*_TextureAtlas.Height), 0, 0, Pred(Source.Width), Pred(Source.Height), Source);
      Source.Release();
    End;

    If (_NormalTexture = Nil) Then
    Begin
      _NormalTexture := Texture.New('particles_normal', _TextureAtlas.Width, _TextureAtlas.Height);
      _NormalTexture.Update;
    End;
    _NormalTexture.UpdateRect(_NormalImage, 0, 0);

    If (_GlowTexture = Nil) Then
    Begin
      _GlowTexture := Texture.New('particles_glow', _TextureAtlas.Width, _TextureAtlas.Height);
      _GlowTexture.Update;
    End;
    _GlowTexture.UpdateRect(_GlowImage, 0, 0);

    If (_RefractionTexture = Nil) Then
    Begin
      _RefractionTexture := Texture.New('particles_refraction', _TextureAtlas.Width, _TextureAtlas.Height);
      _RefractionTexture.Update();
    End;
    _RefractionTexture.UpdateRect(_RefractionImage, 0, 0);

    //_RefractionImage.Save('reffi.png');

    {_TextureAtlas.GetTexture(0).GetImage.Save('TextureAtlas.png');
    _NormalImage.Save('normal.png');}

    For I:=0 To Pred(_TypeCount) Do
    Begin
      _Types[I].U1 := _Types[I].Item.X;
      _Types[I].V1 := _Types[I].Item.Y;
      _Types[I].U2 := _Types[I].Item.X + (Pred(_Types[I].Item.Buffer.Width) / _TextureAtlas.Width);
      _Types[I].V2 := _Types[I].Item.Y + (Pred(_Types[I].Item.Buffer.Height) / _TextureAtlas.Height);
    End;
  End;

  If (GraphicsManager.Instance.RenderStage = renderStageRefraction) Then
    Result := _RefractionTexture
  Else
  If (GraphicsManager.Instance.RenderStage = renderStageNormal) Then
    Result := _NormalTexture
  Else
  If (GraphicsManager.Instance.RenderStage = renderStageGlow) Then
    Result := _GlowTexture
  Else
    Result := _TextureAtlas.GetTexture(0);

  Result.BilinearFilter := False;
  Result.MipMapped := False;
End;

Class function ParticleManager.Instance: ParticleManager;
Begin
  If Not Assigned(_ParticleManager_Instance) Then
    _ParticleManager_Instance := InitializeApplicationComponent(ParticleManager, GraphicsManager);

  Result := ParticleManager(_ParticleManager_Instance.Instance);
End;


(*Procedure ParticleManager.Update;
Var
  I:Integer;
Begin
  {For I:=0 To Pred(_ParticleCollectionCount) Do
    _ParticleCollections[I].Update;}
End;*)

Procedure ParticleManager.Render;
Var
  I:Integer;
Begin
  I := 0;
  While (I<_ParticleCollectionCount) Do
  Begin
    If (_ParticleCollections[I].Finished) Then
    Begin
      _ParticleCollections[I].Release;
      _ParticleCollections[I] := _ParticleCollections[Pred(_ParticleCollectionCount)];
      Dec(_ParticleCollectionCount);
    End Else
    Begin
      GraphicsManager.Instance.AddRenderable(_ParticleCollections[I]);
      Inc(I);
    End;
  End;
End;

Procedure ParticleManager.Spawn(Emitter:ParticleEmitter);
Var
  Collection:ParticleCollection;
Begin
  Collection := ParticleCollection.Create(Emitter);
  Self.AddParticleCollection(Collection);
End;


{ Particle }
Procedure Particle.ClearGraphs;
Begin
  PosX.Clear();
  PosY.Clear();
  PosZ.Clear();

  {$IFDEF ALLOW_PARTICLE_DYNAMIC_MOVEMENT}
  DirX.Clear();
  DirY.Clear();
  DirZ.Clear();

  LinearSpeed.Clear();
  AngularSpeed.Clear();
  AngularDistance.Clear();

  Angle.Clear();
  {$ENDIF}

  Size.Clear();

  RotationSpeed.Clear();
  Rotation.Clear();

  Red.Clear();
  Green.Clear();
  Blue.Clear();
  Alpha.Clear();

  TrailLength.Clear();
  TrailRed.Clear();
  TrailGreen.Clear();
  TrailBlue.Clear();
  TrailAlpha.Clear();
End;

Function Particle.Update(Const Delta: Single; Const Landscape:Boolean):Boolean;
Var
  I:Integer;
  T:Single;
  V:Vector3D;
  U1,U2,V1,V2:Single;
  UD,UW:Single;
Begin
  {If Not Assigned(Particle.Group) Then
  Begin
    Particle.Life := 0.0;
    Exit;
  End;}

  Self.Life := Self.Life - Delta;
  If (Self.Life<=0.0) Then
  Begin
    Result := False;
    Exit;
  End;

  Self.CurrentTime := Self.CurrentTime + Delta;

  T := (Self.CurrentTime / Self.Life);

  If (Self.AnimationFrames>1) Then
    Self.CurrentFrame := Trunc(Pred(Self.AnimationFrames) * T)
  Else
    Self.CurrentFrame := 0;

  {$IFDEF ALLOW_PARTICLE_DYNAMIC_MOVEMENT}
  If (Self.DirX.GraphCount>0) Then
    Self.DirX.Evaluate(T)
  Else
    Self.PosX.Evaluate(T);

  If (Self.DirY.GraphCount>0) Then
    Self.DirY.Evaluate(T)
  Else
    Self.PosY.Evaluate(T);

  If (Self.DirZ.GraphCount>0) Then
    Self.DirZ.Evaluate(T)
  Else
    Self.PosZ.Evaluate(T);
  {$ENDIF}

  Self.Size.Evaluate(T);
  Self.Angle.Evaluate(T);

  {$IFDEF ALLOW_PARTICLE_DYNAMIC_MOVEMENT}
  Self.LinearSpeed.Evaluate(T);

  Self.AngularSpeed.Evaluate(T);
  Self.AngularDistance.Evaluate(T);
  {$ENDIF}

  If (Self.RotationSpeed.GraphCount>0) Then
    Self.RotationSpeed.Evaluate(T)
  Else
    Self.Rotation.Evaluate(T);

  Self.Red.Evaluate(T);
  Self.Green.Evaluate(T);
  Self.Blue.Evaluate(T);
  Self.Alpha.Evaluate(T);

  Self.TrailLength.Evaluate(T);

  If Self.TrailLength.CurrentValue>0 Then
  Begin
    Self.TrailRed.Evaluate(T);
    Self.TrailGreen.Evaluate(T);
    Self.TrailBlue.Evaluate(T);
    Self.TrailAlpha.Evaluate(T);
  End;

(*  If (Particle.Group.FadeMode = particleFadeModeScale) Then
  Begin
    Alpha := Abs(Sin(RAD*180*T));
    Particle.Size := Particle.InitSize * Alpha;

    Alpha := 1.0;
  End Else
  Begin
    Alpha := Abs(Sin(RAD*180*T));
  End;

  If (Particle.Group.PhysicsMode = particlePhysicsModeGravity) Then
  Begin
    Particle.Direction := VectorInterpolate(Particle.InitDir, VectorCreate(0, -1, 0), T);
    Particle.Direction.Normalize();
  End;

  If (Alpha>1.0) Then
    Alpha := 1.0
  Else
  If (Alpha<0.0) Then
    Alpha := 0.0;*)

  {$IFDEF ALLOW_PARTICLE_DYNAMIC_MOVEMENT}
  V.X := Self.DirX.CurrentValue;
  V.Y := Self.DirY.CurrentValue;
  V.Z := Self.DirZ.CurrentValue;
  V.Normalize();
  V.Scale(Self.LinearSpeed.CurrentValue * Delta);

  Self.PosX.CurrentValue := Self.PosX.CurrentValue + V.X;
  Self.PosY.CurrentValue := Self.PosY.CurrentValue + V.Y;
  Self.PosZ.CurrentValue := Self.PosZ.CurrentValue + V.Z;
  {$ELSE}
  Self.PosX.Evaluate(T);
  Self.PosY.Evaluate(T);
  Self.PosZ.Evaluate(T);
  {$ENDIF}

  For I:=0 To 3 Do
  Begin
    Vertices[I].Position.X := Self.PosX.CurrentValue;
    Vertices[I].Position.Y := Self.PosY.CurrentValue;
    Vertices[I].Position.Z := Self.PosZ.CurrentValue;

    Vertices[I].Size.X := Self.Size.CurrentValue;
    Vertices[I].Size.Y := Self.Size.CurrentValue;
  End;

  Self.Rotation.CurrentValue := Self.Rotation.CurrentValue + Self.RotationSpeed.CurrentValue * Delta;
  //Self.Angle := Self.InitRot  + 360 * RAD * T * Self.RotationSpeed;

  If Assigned(Self.Texture) Then
  Begin
    If AnimationFrames>1 Then
    Begin
      UW := Texture.U2 - Texture.U1;
      UD := (1/AnimationFrames) * UW;

      U1 := Texture.U1 + UD * CurrentFrame;
      U2 := U1 + UD;
    End Else
    Begin
      U1 := Texture.U1;
      U2 := Texture.U2;
    End;

    V1 := Texture.V1;
    V2 := Texture.V2;

    If (Landscape) Then
    Begin
      Vertices[1].UV.X := U1;
      Vertices[1].UV.Y := V2;

      Vertices[2].UV.X := U2;
      Vertices[2].UV.Y := V2;

      Vertices[3].UV.X := U2;
      Vertices[3].UV.Y := V1;

      Vertices[0].UV.X := U1;
      Vertices[0].UV.Y := V1;
    End Else
    Begin
      Vertices[0].UV.X := U1;
      Vertices[0].UV.Y := V1;

      Vertices[1].UV.X := U2;
      Vertices[1].UV.Y := V1;

      Vertices[2].UV.X := U2;
      Vertices[2].UV.Y := V2;

      Vertices[3].UV.X := U1;
      Vertices[3].UV.Y := V2;
    End;
  End;

  Result := True;
End;

Procedure Particle.Reset(Emitter:ParticleEmitter);
Var
  I:Integer;
Begin
  Self.ClearGraphs();

  Emitter.Emit(@Self);

  Self.CurrentTime := 0.0;

  For I:=0 To 3 Do
  Begin
    Self.Vertices[I].Ofs.X := ParticleQuadOffsets[I].X;
    Self.Vertices[I].Ofs.Y := ParticleQuadOffsets[I].Y;
  End;
End;


{ ParticleProperty }
Procedure ParticleProperty.AddGraph(StartValue, EndValue, Duration: Single);
Var
  N:Integer;
Begin
  N := GraphCount;
  Inc(GraphCount);
  If Length(Graphs)<GraphCount Then
    SetLength(Graphs, GraphCount);

  If (N>0) Then
    Graphs[N].StartTime := Graphs[Pred(N)].EndTime
  Else
    Graphs[N].StartTime := 0.0;

  If (Duration<=0.0) Then
    Duration := 0.01;

  Graphs[N].EndTime := Graphs[N].StartTime + Duration;
  Graphs[N].InvDuration := 1 / (Graphs[N].EndTime - Graphs[N].StartTime);

  Graphs[N].StartValue := StartValue;
  Graphs[N].EndValue := EndValue;
End;

Procedure ParticleProperty.Clear;
Begin
  GraphCount := 0;
  CurrentGraph := 0;
End;

Procedure ParticleProperty.MakeConstant(Value: Single);
Begin
  Self.Clear();
  CurrentValue := Value;
End;

Function ParticleProperty.Evaluate(T: Single): Single;
Begin
  If (GraphCount>0) Then
  Begin
    If (T>Graphs[CurrentGraph].EndTime) Then
    Begin
      If (CurrentGraph>=Pred(GraphCount)) Then
      Begin
        T := 1.0;
        {GraphCount := 0;
        Exit;}
      End Else
        Inc(CurrentGraph);
    End;

    // convert T from particle time to graph time
    //T := (T - Graphs[CurrentGraph].StartTime) / (Graphs[CurrentGraph].EndTime - Graphs[CurrentGraph].StartTime); // TODO OPTIMIZE USE 1/X instead
    T := (T - Graphs[CurrentGraph].StartTime) * Graphs[CurrentGraph].InvDuration; // OPTIZED

    CurrentValue := Graphs[CurrentGraph].EndValue * T + Graphs[CurrentGraph].StartValue * (1.0 - T);
  End;

  Result := CurrentValue;
End;

{ ParticleEmitter }
Function ParticleEmitter.GetParticleCount: Integer;
Begin
  Result := _EmissionRate;
End;

Procedure ParticleEmitter.SetEmission(Value: Single);
Begin
  If _EmissionPerMinute = Value Then
    Exit;

  _EmissionPerMinute := Value;
  _LastEmission := Application.Instance.GetElapsedTime();
End;

End.
