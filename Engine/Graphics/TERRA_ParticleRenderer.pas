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
  TERRA_String, TERRA_Utils, TERRA_Object, TERRA_GraphicsManager, TERRA_Texture, TERRA_Application,
  TERRA_Resource, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_Stream, TERRA_Plane, TERRA_Matrix4x4,
  TERRA_Math, TERRA_TextureAtlas, TERRA_BoundingBox,
  TERRA_Image, TERRA_Renderer, TERRA_FileManager, TERRA_VertexFormat, TERRA_Renderable, TERRA_Viewport;

Const
  vertexOfs  = vertexUV1;
  vertexSize = vertexUV2;
  vertexAngles = vertexUV3;

  vertexFormatOfs  = vertexFormatUV1;
  vertexFormatSize = vertexFormatUV2;
  vertexFormatAngles = vertexFormatUV3;

  ParticleVertexFormat = [vertexFormatPosition, vertexFormatColor, vertexFormatUV0, vertexFormatOfs, vertexFormatSize, vertexFormatAngles];

Type
  ParticleVertex = Class(Vertex)
    Protected
      Procedure Load(); Override;
      Procedure Save(); Override;

    Public
  		Position:Vector3D;
      Color:ColorRGBA;
      UV0:Vector2D;
	  	Ofs:Vector2D;
      Size:Vector2D;
      Angle:Vector2D;
  End;


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

{  PParticleVertex = ^ParticleVertex;
  ParticleVertex = Packed Record
    Position:Vector3D;
    Ofs:Vector2D;
    Size:Vector2D;
    Angles:Vector2D; // cos / sin of rotation
    UV:Vector2D;
    Color:TERRA_Color.Color;
  End;}

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

  Particle = Class(TERRAObject)
    Private
      _Parent:ParticleCollection;
      _Index:Integer;

      //Group:PParticleSettingsGroup;

      CurrentTime:Single;
      CurrentFrame:Integer;

      Procedure FillVertex(P:ParticleVertex; SubIndex:Integer; Const Landscape:Boolean);

    Public
      Life:Single;    // duration, in miliseconds
      Kind:Integer;
      BlendMode:Integer;
      Texture:ParticleType;
      AnimationFrames:Integer;
      AnimationRepeat:Integer;

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

      Constructor Create(Parent:ParticleCollection; Index:Integer);

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
      Procedure Emit(Target:Particle); Virtual; Abstract;
      Function GetParticleCount: Integer; Virtual;

      Property EmissionPerMinute:Single Read _EmissionPerMinute Write SetEmission;
  End;

  PositionalParticleEmitter = Class(ParticleEmitter)
    Protected
      _Position:Vector3D;

    Public
      Property Position:Vector3D Read _Position Write _Position;
  End;

  ParticleCollection = Class(TERRARenderable)
    Protected
      _Box:BoundingBox;
      _Init:Boolean;

      _Particles:Array Of Particle;
      _ParticleCount:Integer;

      _Vertices:VertexData;

      _BlendModes:Array Of Integer;
      _BlendModeCount:Integer;
      _BlendModeHashes:Array[0..255] Of Byte;

      _Shader:ShaderInterface;

      //_Position:Vector3D;
      _LastTime:Integer;
      _StartTime:Cardinal;

      _Delta:Single;

      _ActiveCount:Integer;
      _Finished:Boolean;

      _Emitter:ParticleEmitter;
      //_GenFlags:Cardinal;

      Procedure AddBlendMode(Mode:Integer);
      Procedure UpdateBlendModes(Const Stage:RendererStage);

      // returns how many vertices processed
      Function UpdateBatch(BlendMode:Integer; Const Stage:RendererStage; Const Landscape:Boolean):Integer;

      Procedure Init;

    Public
      //Settings:ParticleSettings;
      Respawn:Boolean;

      Constructor Create(Emitter:ParticleEmitter{; Pos:Vector3D});
      Procedure Release; Override;

      Procedure Update(View:TERRAViewport); Override;

      Function GetRenderBucket:Cardinal; Override;

      Procedure Reset;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal); Override;
      Function GetBoundingBox:BoundingBox; Override;

      Property ActiveCount:Integer Read _ActiveCount;
      Property Finished:Boolean Read _Finished;

      Property Emitter:ParticleEmitter Read _Emitter;
      //Property Position:Vector3D Read _Position Write _Position;
  End;

  ParticleManager = Class(TERRAObject)
    Protected
      _Instance:ParticleManager;
      _TextureAtlas:TextureAtlas;

      _NormalTexture:TERRATexture;
      _NormalImage:TERRAImage;

      _GlowTexture:TERRATexture;
      _GlowImage:TERRAImage;

      _RefractionTexture:TERRATexture;
      _RefractionImage:TERRAImage;

      _Types:Array Of ParticleType;
      _TypeCount:Integer;

      _NeedsRebuild:Boolean;

      _Shader:ShaderInterface;

      _ParticleCollections:Array Of ParticleCollection;
      _ParticleCollectionCount:Integer;

      Function GetShader:ShaderInterface;

    Public
      Constructor Create(); 
      Procedure Release; Override;

      Procedure Clear;
      Procedure Render(View:TERRAViewport);

      Function GetParticleType(Name:TERRAString):ParticleType;

      Procedure AddParticleCollection(Particles:ParticleCollection);
      //Procedure Spawn(Name:TERRAString; Position:Vector3D);
      Procedure Spawn(Emitter:ParticleEmitter);

      Function GetTexture(Const Stage:RendererStage; Target:ParticleCollection):TERRATexture;

      Property Shader:ShaderInterface Read GetShader;
  End;

Function CreateParticleVertexData(Count:Integer):VertexData;

Implementation
Uses TERRA_EngineManager, TERRA_Error, TERRA_OS, TERRA_Log, TERRA_Camera, TERRA_Mesh, TERRA_FileStream, TERRA_FileUtils, TERRA_ShaderManager;


Function CreateParticleVertexData(Count:Integer):VertexData;
Begin
  Result := VertexData.Create(ParticleVertexFormat, Count);
  Result.SetAttributeName(vertexOfs, 'terra_ofs');
  Result.SetAttributeName(vertexSize, 'terra_size');
  Result.SetAttributeName(vertexAngles, 'terra_angle');
End;

{ ParticleCollection }
Constructor ParticleCollection.Create(Emitter:ParticleEmitter);
Var
  Count:Integer;
Begin
  _Shader := Engine.Particles.Shader;

  {_SettingsTemplate := ParticleManager.Instance.GetSettings(SettingName);
  Settings := ParticleSettings.Create;
  ResetSettings;}

  _Emitter := Emitter;
  Count := _Emitter.GetParticleCount;
  _ParticleCount := Count;
  _LastTime := Application.Instance.GetElapsedTime();

  _Emitter._LastEmission := _LastTime;
  _Emitter._EmissionPerMinute := -1;

  _Init := True;
  //_Position := Pos;
  Respawn := True;
End;

Procedure ParticleCollection.Release;
Var
  I:Integer;
Begin
  Inherited;

  For I:=0 To Pred(_ParticleCount) Do
    ReleaseObject(_Particles[I]);

  ReleaseObject(_Emitter);
End;

Procedure ParticleCollection.Init;
Var
  I,J:Integer;
Begin
  _Vertices := CreateParticleVertexData(_ParticleCount * 6);

  SetLength(_Particles, _ParticleCount);
  For I:=0 To Pred(_ParticleCount) Do
  Begin
    _Particles[I] := Particle.Create(Self, I);
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

Function ParticleCollection.UpdateBatch(BlendMode:Integer; Const Stage:RendererStage; Const Landscape:Boolean): Integer;
Var
  It:VertexIterator;
  I, J, N:Integer;
  CC:ColorRGBA;
  Angles:Vector2D;
  P:ParticleVertex;
Begin
  Result := 0;
  N := 0;

  It := Self._Vertices.GetIteratorForClass(ParticleVertex);
  While N<_ParticleCount Do
  Begin
    If (_Particles[N].Life > 0.0) And ((Stage<>renderStageDiffuse) Or (_Particles[N].BlendMode = BlendMode)) Then
    Begin
      CC.R := Trunc(_Particles[N].Red.CurrentValue);
      CC.G := Trunc(_Particles[N].Green.CurrentValue);
      CC.B := Trunc(_Particles[N].Blue.CurrentValue);
      CC.A := Trunc(_Particles[N].Alpha.CurrentValue);

      Angles.X := Cos(_Particles[N].Rotation.CurrentValue);
      Angles.Y := Sin(_Particles[N].Rotation.CurrentValue);
      {Angles.X := 0;
      Angles.Y := 1;}


      For J:=0 To 5 Do
      Begin
        Case J Of
        3:  I := 2;
        4:  I := 3;
        5:  I := 0;
        Else
          I := J;
        End;

        If Not It.HasNext() Then
          Break;

        P := ParticleVertex(It.Value);
        P.Angle := Angles;
        P.Color := CC;
        _Particles[N].FillVertex(P, I, Landscape);
      End;
    End;

    Inc(N);
  End;

  Result := Succ(It.Position);
  ReleaseObject(It);
End;

Procedure ParticleCollection.Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal);
Var
  I, RenderCount:Integer;
//  Ratio:Single;
  Right, Up:Vector3D;
  Landscape:Boolean;
  Graphics:GraphicsManager;
Begin
  Graphics := Engine.Graphics;

  {If (_Init) Then
    Self.Update;}

  Landscape := IsLandscapeOrientation(Application.Instance.Orientation);

  {If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
  Begin
    Up := Graphics.ActiveViewport.Camera.Right;
    Right := Graphics.ActiveViewport.Camera.Up;
    Up.Scale(-1.0);
  End Else}
  Begin
    Right := View.Camera.Right;
    Up := View.Camera.Up;
  End;

  //Ratio := Graphics.ActiveViewport.Height / Graphics.ActiveViewport.Width;
//  Ratio := 1.0; //Graphics.ActiveViewport.Height / Graphics.ActiveViewport.Width;

  Graphics.Renderer.BindShader(_Shader);

  View.Camera.SetupUniforms;

  _Shader.SetColorUniform('sunColor', ColorWhite);
  _Shader.SetVec3Uniform('cameraUp', Up);
  _Shader.SetVec3Uniform('cameraRight', Right);
  _Shader.SetIntegerUniform('texture0', 0);
//  _Shader.SetFloatUniform('ratio', Ratio);
  _Shader.SetMat4Uniform('reflectionMatrix', Graphics.ReflectionMatrix);

  Self.UpdateBlendModes(Stage);


  For I:=0 To Pred(_BlendModeCount) Do
  Begin
    RenderCount := Self.UpdateBatch(_BlendModes[I], Stage, Landscape);

    If (I=0) Then
    Begin
      Graphics.Renderer.SetDepthMask(False);
      Engine.Particles.GetTexture(Stage, Self).Bind(0);
    End;

    If (RenderCount>0) Then
    Begin
      Graphics.Renderer.SetBlendMode(_BlendModes[I]);

      {Graphics.Renderer.SetSourceVertexSize(SizeOf(ParticleVertex));
      Graphics.Renderer.SetAttributeSource('terra_position', typeVector3D, @(_Temp[0].Position));
      Graphics.Renderer.SetAttributeSource('terra_UV0', typeVector2D, @(_Temp[0].UV));
      Graphics.Renderer.SetAttributeSource('terra_ofs', typeVector2D, @(_Temp[0].Ofs));
      Graphics.Renderer.SetAttributeSource('terra_size', typeVector2D, @(_Temp[0].Size));
      Graphics.Renderer.SetAttributeSource('terra_angle', typeVector2D, @(_Temp[0].Angles));
      Graphics.Renderer.SetAttributeSource('terra_color', typeColor, @(_Temp[0].Color));}

      Graphics.Renderer.SetVertexSource(_Vertices);
      Graphics.Renderer.DrawSource(renderTriangles, RenderCount);
    End;
  End;

  Graphics.Renderer.SetDepthMask(True);
  Graphics.Renderer.SetBlendMode(blendNone);
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

Function ParticleCollection.GetRenderBucket:Cardinal;
Begin
  Result := renderBucket_Translucent;
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

Procedure ParticleCollection.UpdateBlendModes(Const Stage:RendererStage);
Var
  I:Integer;
Begin
  _BlendModeCount := 0;

  FillChar(_BlendModeHashes, SizeOf(_BlendModeHashes), 0);

  If (Stage <> renderStageDiffuse) Then
  Begin
    If (Stage = renderStageRefraction) Then
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
    ReleaseObject(_ParticleCollections[I]);

  _ParticleCollectionCount := 0;
End;

Constructor ParticleManager.Create();
Begin
  _NeedsRebuild := False;
  _TextureAtlas := TextureAtlas.Create('particle', 512, 512);
  _NormalImage := TERRAImage.Create(_TextureAtlas.Width, _TextureAtlas.Height);
  _GlowImage := TERRAImage.Create(_TextureAtlas.Width, _TextureAtlas.Height);
  _RefractionImage := TERRAImage.Create(_TextureAtlas.Width, _TextureAtlas.Height);
End;

Procedure ParticleManager.Release;
Var
  I:Integer;
Begin
  Clear();

  For I:=0 To Pred(_TypeCount) Do
    ReleaseObject(_Types[I]);
  _TypeCount := 0;

  ReleaseObject(_TextureAtlas);
  ReleaseObject(_NormalTexture);
  ReleaseObject(_NormalImage);
  ReleaseObject(_GlowTexture);
  ReleaseObject(_GlowImage);
  ReleaseObject(_RefractionTexture);
  ReleaseObject(_RefractionImage);
End;

Function ParticleManager.GetParticleType(Name:TERRAString): ParticleType;
Var
  I:Integer;
  Location:TERRALocation;
  Source:TERRAImage;
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
    Location := Engine.Files.Search(Name);
    If Assigned(Location) Then
      Source := TERRAImage.Create(Location)
    Else
      Source := TERRAImage.Create(32, 32);

    Result.Item := _TextureAtlas.Add(Source, Name);
    _NeedsRebuild := True;
    ReleaseObject(Source);
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
      ReleaseObject(Source);
    End;
  End Else
  Begin
    Result := Nil;
    Log(logError, 'Particles', 'Could not find particle file ' + Name);
  End;
End;}

Function ParticleManager.GetShader: ShaderInterface;
Begin
  If (_Shader = Nil) Then
  Begin
    _Shader := Engine.Graphics.Renderer.CreateShader();
    _Shader.Generate('particles', GetShader_Particles());
  End;

  Result := _Shader;
End;

Function ParticleManager.GetTexture(Const Stage:RendererStage; Target:ParticleCollection): TERRATexture;
Var
  I:Integer;
  Source:TERRAImage;
  Item:TextureAtlasItem;
  Location:TERRALocation;
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

      Location := Engine.Files.Search(StringLower(GetFileName(Item.Name, True))+'_normal.png');
      If Assigned(Location) Then
        Source := TERRAImage.Create(Location)
      Else
      Begin
        Source := TERRAImage.Create(Item.Buffer.Width, Item.Buffer.Height);
        Source.ClearWithColor(ColorNull, maskRGBA);
      End;
      _NormalImage.Blit(Trunc(Item.U1*_TextureAtlas.Width), Trunc(Item.V1*_TextureAtlas.Height), 0, 0, Pred(Source.Width), Pred(Source.Height), Source);
      ReleaseObject(Source);

      Location := Engine.Files.Search(StringLower(GetFileName(Item.Name, True))+'_glow.png');
      If Assigned(Location) Then
        Source := TERRAImage.Create(Location)
      Else
      Begin
        Source := TERRAImage.Create(Item.Buffer.Width, Item.Buffer.Height);
        Source.ClearWithColor(ColorNull, maskRGBA);
      End;
      _GlowImage.Blit(Trunc(Item.U1*_TextureAtlas.Width), Trunc(Item.V1*_TextureAtlas.Height), 0, 0, Pred(Source.Width), Pred(Source.Height), Source);
      ReleaseObject(Source);

      Location := Engine.Files.Search(StringLower(GetFileName(Item.Name, True))+'_refraction.png');
      If Assigned(Location) Then
        Source := TERRAImage.Create(Location)
      Else
      Begin
        Source := TERRAImage.Create(Item.Buffer.Width, Item.Buffer.Height);
        Source.ClearWithColor(ColorNull, maskRGBA);
      End;
      _RefractionImage.Blit(Trunc(Item.U1*_TextureAtlas.Width), Trunc(Item.V1*_TextureAtlas.Height), 0, 0, Pred(Source.Width), Pred(Source.Height), Source);
      ReleaseObject(Source);
    End;

    If (_NormalTexture = Nil) Then
    Begin
      _NormalTexture := TERRATexture.Create(rtDynamic);
      _NormalTexture.InitFromSize(_TextureAtlas.Width, _TextureAtlas.Height, ColorCreate(0, 0, 255));
      _NormalTexture.Update;
    End;
    _NormalTexture.UpdateRect(_NormalImage, 0, 0);

    If (_GlowTexture = Nil) Then
    Begin
      _GlowTexture := TERRATexture.Create(rtDynamic);
      _GlowTexture.InitFromSize(_TextureAtlas.Width, _TextureAtlas.Height, ColorNull);
      _GlowTexture.Update;
    End;
    _GlowTexture.UpdateRect(_GlowImage, 0, 0);

    If (_RefractionTexture = Nil) Then
    Begin
      _RefractionTexture := TERRATexture.Create(rtDynamic);
      _RefractionTexture.InitFromSize(_TextureAtlas.Width, _TextureAtlas.Height, ColorNull);
      _RefractionTexture.Update();
    End;
    _RefractionTexture.UpdateRect(_RefractionImage, 0, 0);

    //_RefractionImage.Save('reffi.png');

    {_TextureAtlas.GetTexture(0).GetImage.Save('TextureAtlas.png');
    _NormalImage.Save('normal.png');}

    For I:=0 To Pred(_TypeCount) Do
    Begin
      _Types[I].U1 := _Types[I].Item.U1;
      _Types[I].V1 := _Types[I].Item.V1;
      _Types[I].U2 := _Types[I].Item.U2;
      _Types[I].V2 := _Types[I].Item.V2;
    End;
  End;

  If (Stage = renderStageRefraction) Then
    Result := _RefractionTexture
  Else
  If (Stage = renderStageNormal) Then
    Result := _NormalTexture
  Else
  If (Stage = renderStageGlow) Then
    Result := _GlowTexture
  Else
    Result := _TextureAtlas.GetTexture(0);

  If Assigned(Result) Then
  Begin
    Result.Filter := filterLinear;
    Result.MipMapped := False;
  End Else
    Result := Engine.Textures.WhiteTexture;
End;

Procedure ParticleManager.Render(View:TERRAViewport);
Var
  I:Integer;
Begin
  I := 0;
  While (I<_ParticleCollectionCount) Do
  Begin
    If (_ParticleCollections[I].Finished) Then
    Begin
      ReleaseObject(_ParticleCollections[I]);
      _ParticleCollections[I] := _ParticleCollections[Pred(_ParticleCollectionCount)];
      Dec(_ParticleCollectionCount);
    End Else
    Begin
      Engine.Graphics.AddRenderable(View, _ParticleCollections[I]);
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
    Self.CurrentFrame := Trunc(Pred(Self.AnimationFrames) * T * Self.AnimationRepeat) Mod Self.AnimationFrames
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

  Self.Rotation.CurrentValue := Self.Rotation.CurrentValue + Self.RotationSpeed.CurrentValue * Delta;
  //Self.Angle := Self.InitRot  + 360 * RAD * T * Self.RotationSpeed;

  Result := True;
End;

Procedure Particle.FillVertex(P:ParticleVertex; SubIndex:Integer; Const Landscape:Boolean);
Var
  UD,UW:Single;
  U1,U2,V1,V2:Single;
Begin
  P.Position := Vector3D_Create(Self.PosX.CurrentValue,  Self.PosY.CurrentValue, Self.PosZ.CurrentValue);
  P.Size := Vector2D_Create(Self.Size.CurrentValue, Self.Size.CurrentValue);
  P.Ofs := Vector2D_Create(ParticleQuadOffsets[SubIndex].X, ParticleQuadOffsets[SubIndex].Y);

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
      Case SubIndex Of
      0: P.UV0 := Vector2D_Create(U1, V2);
      1: P.UV0 := Vector2D_Create(U2, V2);
      2: P.UV0 := Vector2D_Create(U2, V1);
      3: P.UV0 := Vector2D_Create(U1, V1);
      End;
    End Else
    Begin
      Case SubIndex Of
      0: P.UV0 := Vector2D_Create(U1, V1);
      1: P.UV0 := Vector2D_Create(U2, V1);
      2: P.UV0 := Vector2D_Create(U2, V2);
      3: P.UV0 := Vector2D_Create(U1, V2);
      End;
    End;
  End;
End;

Procedure Particle.Reset(Emitter:ParticleEmitter);
Begin
  Self.ClearGraphs();
  Emitter.Emit(Self);
  Self.CurrentTime := 0.0;
End;


Constructor Particle.Create(Parent:ParticleCollection; Index:Integer);
Begin
  Self._Parent := Parent;
  Self._Index := Index;
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

{ ParticleVertex }
procedure ParticleVertex.Load;
Begin
  Self.GetVector3D(vertexPosition, Self.Position);
  Self.GetColor(vertexColor, Self.Color);
  Self.GetVector2D(vertexUV0, Self.UV0);
  Self.GetVector2D(vertexOfs, Self.Ofs);
  Self.GetVector2D(vertexSize, Self.Size);
  Self.GetVector2D(vertexAngles, Self.Angle);
End;

Procedure ParticleVertex.Save;
Begin
  Self.SetVector3D(vertexPosition, Self.Position);
  Self.SetColor(vertexColor, Self.Color);
  Self.SetVector2D(vertexUV0, Self.UV0);
  Self.SetVector2D(vertexOfs, Self.Ofs);
  Self.SetVector2D(vertexSize, Self.Size);
  Self.SetVector2D(vertexAngles, Self.Angle);
End;

End.
