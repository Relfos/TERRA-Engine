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
 * TERRA_Mesh
 * Implements the Mesh resource and Mesh Instance classes
 ***********************************************************************************************************************
}
Unit TERRA_Mesh;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Utils, TERRA_Object, TERRA_Texture, TERRA_Image, TERRA_Stream, TERRA_Resource,
  TERRA_MeshAnimation, TERRA_MeshAnimationNodes, TERRA_MeshSkeleton,
  TERRA_Renderer, TERRA_ResourceManager, TERRA_FileUtils, TERRA_Vector4D, TERRA_Quaternion,
  TERRA_Math, TERRA_Ray, TERRA_Collections, TERRA_ShadowVolumes, TERRA_GraphicsManager, TERRA_MeshFilter,
  TERRA_BoundingBox, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_PhysicsManager, TERRA_VertexFormat,
  TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ParticleRenderer, TERRA_ParticleEmitters, TERRA_Lights, TERRA_Renderable, TERRA_Viewport;

Const
  MaxBones    = 36;

  MaxTrailSize = 5;
  TrailDelay = 500;
  TrailDecay = 1000;

  customTransformNone   = 0;
  customTransformLocal  = 1;
  customTransformGlobal = 2;

  MinDelta = 0.01;

  tagMeshHeader         = 'MX3D';
  tagMeshEnd            = 'MEND';

  tagMeshGroup          = 'XGRP';
  tagMeshSkeleton       = 'XSKL';
  tagMeshEmitter        = 'XEMT';
  tagMeshLight          = 'XLIT';
  tagMeshMetadata       = 'XMET';
  tagMeshBoneMorph      = 'XBMR';

  tagGroupEnd           = 'GEND';
  tagVertexData         = 'VDAT';
  tagVertexMorph        = 'VMRP';
  tagTriangleIndices    = 'TIDX';
  tagTriangleNormals    = 'TNRM';
  tagTriangleEdges      = 'TEDG';
  tagMaterialDiffuse    = 'MDIF';
  tagMaterialTriplanar  = 'MTRP';
  tagMaterialSpecular   = 'MSPC';
  tagMaterialBump       = 'MNRM';
  tagMaterialDisplacement = 'MDIS';
  tagMaterialLightMap   = 'MLMP';
  tagMaterialRefraction = 'MRFR';
  tagMaterialReflective = 'MRFL';
  tagMaterialGlow       = 'MGLW';
  tagMaterialAlphaMap   = 'MAMP';
  tagMaterialEnvMap     = 'MENV';
  tagMaterialRamp       = 'MRMP';
  tagMaterialBlendMode  = 'MBLM';
  tagMaterialParticles  = 'MPFX';

  VertexCompressionLimit = 20000;

Type
  TERRAMesh = Class;

  PMeshAttach = ^MeshAttach;
  MeshAttach = Record
    AttachMesh:TERRAMesh;
    BoneIndex:Integer;
    Matrix:Matrix4x4;
    Color:TERRA_Color.Color;
    IsStencil:Boolean;
  End;

  MeshEmitter = Class(TERRAObject)
    Name:TERRAString;
    Content:TERRAString;
    Position:Vector3D;
    BoneIndex:Integer;
    ParentBone:TERRAString;
    Owner:TERRAMesh;

    Constructor Create(Owner:TERRAMesh);
    Procedure UpdateBone;
  End;

  MeshLight = Class(TERRAObject)
    Name:TERRAString;
    Owner:TERRAMesh;
    Position:Vector3D;
    BoneIndex:Integer;
    ParentBone:TERRAString;

    LightType:Byte;
    LightColor:Color;

    Param1:Vector3D;
    Param2:Vector3D;
    Param3:Vector3D;

    GroupIndex:Integer;

    Constructor Create(Owner:TERRAMesh);
    Procedure UpdateBone();
  End;

  MeshLightState = Record
    Light:PositionalLight;
    Enabled:Boolean;
    GroupID:Integer;
  End;

  MeshVertex = Class(Vertex)
    Protected
      Procedure Load(); Override;
      Procedure Save(); Override;

    Public
  		Position:Vector3D;
	  	Normal:Vector3D;
      Tangent:Vector4D;
      UV0:Vector2D;
      UV1:Vector2D;
      BaseColor:Color;
      HueShift:Single;
      BoneIndex:Integer;
  End;

  MeshInstance = Class;

  MeshFX = Class;
  MeshFXCallback = Procedure(FX:MeshFX; UserData:Pointer);  CDecl;

  MeshFX = Class(TERRAObject)
    Private
      _Target:TERRAMesh;
      _Callback:MeshFXCallback;
      _UserData:Pointer;

    Public
      Function Update():Boolean; Virtual; Abstract;

      Procedure SetCallback(Callback:MeshFXCallback; UserData:Pointer);

      Property Target:TERRAMesh Read _Target;
  End;

  MeshMaterial = Object
    BlendMode:Integer;

    AmbientColor:Color;
    DiffuseColor:Color;
    ShadowColor:Color;
    OutlineColor:Color;

    DiffuseMap:TERRATexture;
    TriplanarMap:TERRATexture;
    DecalMap:TERRATexture;
    NormalMap:TERRATexture;
    DisplacementMap:TERRATexture;
    SpecularMap:TERRATexture;
    GlowMap:TERRATexture;
    RefractionMap:TERRATexture;
    AlphaMap:TERRATexture;
    LightMap:TERRATexture;
    ReflectiveMap:TERRATexture;
    ReflectionMap:TERRATexture;
    FlowMap:TERRATexture;
    NoiseMap:TERRATexture;
    EnviromentMap:TERRATexture;

    FlowSpeed:Single;
    FlowBounds:Vector4D;

    ToonRamp:TERRATexture;
    ColorTable:TERRATexture;
    //ColorTableFactor:Single;

    DitherPatternMap:TERRATexture;

    VegetationBend:Single;
    Ghost:Boolean;

    HueShift:Single;

    Procedure Reset;

    Function Equals(Const Other:MeshMaterial):Boolean;

    Function Clone():MeshMaterial;
  End;

  MeshGroupInstance = Record
    Visibility:Boolean;
    Wireframe:Boolean;

    UseTextureMatrix:Boolean;
    TextureTransform:Matrix4x4;

    GeometryTransform:Matrix4x4;
    GeometryTransformType:Integer;

    TempAlpha:Byte;

    Material:MeshMaterial;
  End;

  MeshInstance = Class(Renderable)
    Protected
      _Mesh:TERRAMesh;
      _BoundingBox:BoundingBox;
      _Transform:Matrix4x4;

      _Body:PhysicsBody;

      _FX:Array Of MeshFX;
      _FXCount:Integer;
      _ClonedMesh:Boolean;

      _Groups:Array Of MeshGroupInstance;

      _Lights:Array Of MeshLightState;
      _LightCount:Integer;

      _AttachList:Array Of MeshAttach;
      _AttachCount:Integer;

      _Position:Vector3D;
      _Rotation:Vector3D;
      _Scale:Vector3D;

      _NeedsTransformUpdate:Boolean;
      _NeedsPositionUpdate:Boolean;
      _NeedsRotationUpdate:Boolean;
      _NeedsShadowUpdate:Boolean;

      _CastShadows:Boolean;
      _ShadowVolume:ShadowVolume;

      _AlphaLODValue:Single;

      _MotionBlur:Boolean;
      _RenderTrails:Boolean;
      _OldTransforms:Array[0..Pred(MaxTrailSize)] Of Matrix4x4;
      _LastTrailUpdate:Cardinal;
      _LastMotionBlur:Cardinal;

      _LastFrameID:Cardinal;
      _StencilID:Integer;

      _Animation:AnimationState;

      _ParticleSystems:Array Of ParticleCollection;
      _ParticleSystemCount:Integer;

      _Emitters:Array Of MeshEmitter;
      _EmitterCount:Integer;

      _ScratchID:Cardinal;

      Procedure SetGeometry(MyMesh:TERRAMesh);

      Procedure DrawMesh(View:TERRAViewport; Const MyTransform:Matrix4x4; TranslucentPass, StencilTest:Boolean);

      Procedure DrawParticles(View:TERRAViewport);

      Function IsGroupTranslucent(Index:Integer):Boolean;

      Function GetAnimation: AnimationState;

      Procedure UpdateBoundingBox();
      Procedure UpdateTransform();

      Function GetPosition():Vector3D;
      Function GetRotation():Vector3D;


      Function IsOpaque():Boolean;
      Function IsTranslucent():Boolean;

    Public
      CullGroups:Boolean;
      CustomShader:ShaderInterface;
      Diffuse:Color;
      AlwaysOnTop:Boolean;

      Procedure Update(View:TERRAViewport); Override;

      Function ActivatePhysics(Mass:Single):Boolean;

      Function IsReady():Boolean;

      Function GetRenderBucket:Cardinal; Override;

      Procedure RenderLights(View:TERRAViewport); Override;

      Function GetName():TERRAString; Override;

      Procedure SetDiffuseMap(GroupID:Integer; Map:TERRATexture);
      Function GetDiffuseMap(GroupID:Integer):TERRATexture;

      Procedure SetTriplanarMap(GroupID:Integer; Map:TERRATexture);
      Function GetTriplanarMap(GroupID:Integer):TERRATexture;

      Procedure SetDecalMap(GroupID:Integer; Map:TERRATexture);
      Function GetDecalMap(GroupID:Integer):TERRATexture;

      Procedure SetNormalMap(GroupID:Integer; Map:TERRATexture);
      Function GetNormalMap(GroupID:Integer):TERRATexture;

      Procedure SetDisplacementMap(GroupID:Integer; Map:TERRATexture);
      Function GetDisplacementMap(GroupID:Integer):TERRATexture;

      Procedure SetSpecularMap(GroupID:Integer; Map:TERRATexture);
      Function GetSpecularMap(GroupID:Integer):TERRATexture;

      Procedure SetGlowMap(GroupID:Integer; Map:TERRATexture);
      Function GetGlowMap(GroupID:Integer):TERRATexture;

      Procedure SetRefractionMap(GroupID:Integer; Map:TERRATexture);
      Function GetRefractionMap(GroupID:Integer):TERRATexture;

      Procedure SetReflectiveMap(GroupID:Integer; Map:TERRATexture);
      Function GetReflectiveMap(GroupID:Integer):TERRATexture;

      Procedure SetEnviromentMap(GroupID:Integer; Map:TERRATexture);
      Function GetEnviromentMap(GroupID:Integer):TERRATexture;

      Procedure SetFlowMap(GroupID:Integer; Map:TERRATexture);
      Function GetFlowMap(GroupID:Integer):TERRATexture;

      Procedure SetNoiseMap(GroupID:Integer; Map:TERRATexture);
      Function GetNoiseMap(GroupID:Integer):TERRATexture;

      Procedure SetToonRamp(GroupID:Integer; Map:TERRATexture);
      Function GetToonRamp(GroupID:Integer):TERRATexture;

      Procedure SetColorTable(Map:TERRATexture); Overload;
      Procedure SetColorTable(GroupID:Integer; Map:TERRATexture); Overload;
      Function GetColorTable(GroupID:Integer):TERRATexture;

      Procedure SetAlphaMap(GroupID:Integer; Map:TERRATexture);
      Function GetAlphaMap(GroupID:Integer):TERRATexture;

      Procedure SetLightMap(GroupID:Integer; Map:TERRATexture);
      Function GetLightMap(GroupID:Integer):TERRATexture;

      Procedure SetDitherPatternMap(GroupID:Integer; Map:TERRATexture);
      Function GetDitherPatternMap(GroupID:Integer):TERRATexture;

      Procedure SetVisibility(GroupID:Integer; Visible:Boolean);
      Function GetVisibility(GroupID:Integer):Boolean;

      Procedure SetWireframeMode(GroupID:Integer; Enabled:Boolean);
      Function GetWireframeMode(GroupID:Integer):Boolean;

      Function GetHueShift(GroupID:Integer): Single;
      Procedure SetHueShift(GroupID:Integer; Value:Single);

      Function AddEffect(FX:MeshFX):TERRAMesh;

      Function AddParticleEmitter(Const Name:TERRAString; Position: Vector3D; Const Content:TERRAString; Const ParentBone:TERRAString = ''):MeshEmitter;

      Procedure SetDiffuseColor(MyColor:Color); Overload;
      Procedure SetDiffuseColor(GroupID:Integer; MyColor:Color); Overload;
      Function GetDiffuseColor(GroupID:Integer):Color;

      Procedure SetShadowColor(GroupID:Integer; MyColor:Color);
      Function GetShadowColor(GroupID:Integer):Color;

 (*     Procedure SetAmbientColor(GroupID:Integer; MyColor:Color);
      Function GetAmbientColor(GroupID:Integer):Color;*)

      Procedure SetOutlineColor(MyColor:Color); Overload;
      Procedure SetOutlineColor(GroupID:Integer; MyColor:Color); Overload;
      Function GetOutlineColor(GroupID:Integer):Color;

      Procedure SetWaterFlowBounds(GroupID:Integer; Bounds:Vector4D); 
      Function GetWaterFlowBounds(GroupID:Integer):Vector4D;

      Procedure SetWaterFlowSpeed(GroupID:Integer; Speed:Single);
      Function GetWaterFlowSpeed(GroupID:Integer):Single;          

      Procedure SetTextureTransform(GroupID:Integer; Transform:Matrix4x4);
      Function GetTextureTransform(GroupID:Integer):Matrix4x4;

      Function GetVegetationBend(GroupID: Integer): Single;
      Procedure SetVegetationBend(GroupID: Integer; Bend:Single);

      Procedure SetGhostMode(Ghost:Boolean);

      Procedure SetUVOffset(GroupID:Integer; X,Y:Single);
      Function GetUVOffset(GroupID:Integer):Vector2D;

      Procedure SetUVScale(GroupID:Integer; X,Y:Single);

      Procedure SetBlendMode(GroupID:Integer; Mode:Integer);
      Function GetBlendMode(GroupID:Integer):Integer;

      Procedure SetGroupGlobalTransform(GroupID:Integer; Const Transform:Matrix4x4);
      Procedure SetGroupLocalTransform(GroupID:Integer; Const Transform:Matrix4x4);

      Procedure SetPosition(P:Vector3D);
      Procedure SetRotation(P:Vector3D);
      Procedure SetScale(P:Vector3D);

      Function GetTransform():Matrix4x4;

      Procedure SetMotionBlur(Enabled:Boolean);

      Procedure AddAttach(AttachMesh:TERRAMesh; BoneIndex:Integer; M:Matrix4x4; C:Color; IsStencil:Boolean = False);
      Procedure ClearAttachs;

      Constructor Create(MyMesh:TERRAMesh);
      Procedure Release(); Override;

      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render(View:TERRAViewport; Const Bucket:Cardinal); Override;

      Function GetAttach(Index:Integer):PMeshAttach;

      Procedure SetLightState(Index:Integer; Enabled:Boolean);

      Property Position:Vector3D Read GetPosition Write SetPosition;
      Property Rotation:Vector3D Read GetRotation Write SetRotation;
      Property Scale:Vector3D Read _Scale Write SetScale;
      Property Transform:Matrix4x4 Read GetTransform;

      Property CastShadows:Boolean Read _CastShadows Write _CastShadows;
      Property MotionBlur:Boolean Read _MotionBlur Write SetMotionBlur;

      Property Animation:AnimationState Read GetAnimation;
      Property Geometry:TERRAMesh Read _Mesh Write SetGeometry;

      Property AttachCount:Integer Read _AttachCount;

      Property ParticleSystemsCount:Integer Read _ParticleSystemCount;
      Property EmitterCount:Integer Read _EmitterCount;

      Property FXCount:Integer Read _FXCount;
      Property LightCount:Integer Read _LightCount;
  End;

  MeshGroup = Class;

  MeshVertexLink = Packed Record
    GroupIndex:SmallInt;
    VertexIndex:Word;
  End;

  MeshGroupMorph = Record
    ID:Integer;
    MorphType:Byte;
    Values:Array Of Vector3D;
  End;

	MeshGroup = Class(TERRAObject)
    Protected
      _ID:Integer;
      _Owner:TERRAMesh;
	  	_Name:TERRAString;

      _Buffer:VertexBufferInterface;

      _GPUSkinning:Boolean;

      _NeedsTangentSetup:Boolean;

      _Material:MeshMaterial;

      _Shader:ShaderInterface;
      _LightBatch:LightBatch;

      _BoundingBox:BoundingBox;
      _Unique:Boolean;

      _Pins:Array Of Word;
      _PinCount:Integer;

      {$IFDEF PCs}
      _Fur:Fur;
      _Cloth:VerletCloth;
      {$ENDIF}

      _EmitterFX:TERRAString;

      _Vertices:VertexData;
      _ScratchVertices:VertexData;

      _AlphaInspected:TERRATexture;

      _Triangles:Array Of Triangle;
      _Edges:Array Of TriangleEdgesState;
      _TriangleNormals:Array Of Vector3D;
		  _TriangleCount:Integer;

      _VisibleTriangles:Array Of Triangle;
      _VisibleTriangleCount:Integer;
      _CullGeometry:Boolean;

      _Morphs:Array Of MeshGroupMorph;
      _MorphCount:Integer;


      Procedure SetupUniforms(View:TERRAViewport; Transform:Matrix4x4; State:MeshInstance; Outline, TranslucentPass:Boolean; Const Material:MeshMaterial);

      //Procedure SetCombineWithColor(C:Color);
      Procedure BindMaterial(View:TERRAViewport; Var Slot:Integer; Const Material:MeshMaterial);

      Procedure Load(Source:Stream);
      Procedure Save(Dest:Stream);

      Procedure DrawGeometry(State:MeshInstance; ShowWireframe:Boolean);

      Procedure SetTriangleCount(Count:Integer);
      Procedure SetVertexCount(Count:Integer);
      Function GetTriangles:PTriangleArray;

      Function GetVertices():VertexData;
      Function GetVertexCount():Integer;

      Function GetHueShift: Single;
      Procedure SetHueShift(const Value: Single);

      //Procedure SetupSkeleton;

      Function CalculateDitherScale():Single;

      Procedure InheritMaterial(Const OtherMat:MeshMaterial; Var DestMaterial:MeshMaterial);

      Procedure InspectAlpha(Tex:TERRATexture);

    Public
      Userdata:Pointer;
      Flags:Cardinal;
      {$IFDEF PCs}
      FurSettings:TERRA_Fur.FurSettings;
      {$ENDIF}

      Constructor Create(ID:Integer; Parent:TERRAMesh; Format:VertexFormat; Name:TERRAString='');
      Procedure Release; Override;

		  Procedure Clean; Virtual;
      Procedure Init; Virtual;

      Procedure ReleaseBuffer();

      Procedure UpdateBoundingBox;

      Procedure Transform(Const TargetTransform:Matrix4x4);

(*      Function GetAmbientColor: Color;
      Procedure SetAmbientColor(const Value: Color);*)

      Procedure SetAlphaMap(Map:TERRATexture);
      Function GetAlphaMap:TERRATexture;

      Procedure SetLightmap(Map:TERRATexture);
      Function GetLightMap:TERRATexture;

      Procedure SetDitherPatternMap(Map:TERRATexture);
      Function GetDitherPatternMap:TERRATexture;

      Function GetToonRamp:TERRATexture;
      Procedure SetToonRamp(Const Map:TERRATexture);

      Function GetDecalMap:TERRATexture;
      Procedure SetDecalMap(const Value:TERRATexture);

      Function GetDiffuseColor: Color;
      Procedure SetDiffuseColor(const Value: Color);

      Function GetShadowColor: Color;
      Procedure SetShadowColor(const Value: Color);

      Function GetDiffuseMap:TERRATexture;
      Procedure SetDiffuseMap(const Value:TERRATexture);

      Function GetGlowMap:TERRATexture;
      Procedure SetGlowMap(const Value:TERRATexture);

      Function GetNormalMap:TERRATexture;
      Procedure SetNormalMap(const Value:TERRATexture);

      Function GetDisplacementMap:TERRATexture;
      Procedure SetDisplacementMap(const Value:TERRATexture);

      Function GetRefractionMap:TERRATexture;
      Procedure SetRefractionMap(const Value:TERRATexture);

      Function GetReflectiveMap:TERRATexture;
      Procedure SetReflectiveMap(const Value:TERRATexture);

      Function GetEnviromentMap:TERRATexture;
      Procedure SetEnviromentMap(const Value:TERRATexture);

      Function GetFlowMap:TERRATexture;
      Procedure SetFlowMap(const Value:TERRATexture);

      Function GetNoiseMap:TERRATexture;
      Procedure SetNoiseMap(const Value:TERRATexture);

      Function GetSpecularMap:TERRATexture;
      Procedure SetSpecularMap(const Value:TERRATexture);

      Procedure SetTriplanarMap(const Value:TERRATexture);
      Function GetTriplanarMap:TERRATexture;

      Procedure SetBlendMode(const Value: Integer);
      Function GetBlendMode: Integer;

      Procedure SetWireframe(Enabled:Boolean);

      Procedure SetEdge(TriangleIndex, EdgeIndex:Integer; Visible:Boolean);

      Procedure Optimize(VertexCacheSize:Integer);

      Function LockVertices():VertexData;
      Procedure UnlockVertices();

      Function AddVertexMorph(ID:Integer):Integer;
      Function HasVertexMorph(ID:Integer):Boolean;
      Function GetVertexMorph(ID, VertexIndex:Integer):Vector3D;
      Procedure SetVertexMorph(ID, VertexIndex:Integer; Value:Vector3D);

      Function SubDivideVertexFromTriangle(TriangleIndex, TargetVertex:Integer; UseA, UseB, UseC:Boolean):Integer;

      Property Name:TERRAString Read _Name Write _Name;

		  Property DiffuseMap:TERRATexture Read GetDiffuseMap Write SetDiffuseMap;
		  Property DecalMap:TERRATexture Read GetDecalMap Write SetDecalMap;
		  Property TriplanarMap:TERRATexture Read GetTriplanarMap Write SetTriplanarMap;
      Property NormalMap:TERRATexture Read GetNormalMap Write SetNormalMap;
      Property DisplacementMap:TERRATexture Read GetDisplacementMap Write SetDisplacementMap;
      Property AlphaMap:TERRATexture Read GetAlphaMap Write SetAlphaMap;
      Property SpecularMap:TERRATexture Read GetSpecularMap Write SetSpecularMap;
      Property RefractionMap:TERRATexture Read GetRefractionMap Write SetRefractionMap;
      Property ReflectiveMap:TERRATexture Read GetReflectiveMap Write SetReflectiveMap;
      Property EnviromentMap:TERRATexture Read GetEnviromentMap Write SetEnviromentMap;
      Property FlowMap:TERRATexture Read GetFlowMap Write SetFlowMap;
      Property NoiseMap:TERRATexture Read GetNoiseMap Write SetNoiseMap;
      Property GlowMap:TERRATexture Read GetGlowMap Write SetGlowMap;
      Property LightMap:TERRATexture Read GetLightMap Write SetLightmap;
      Property DitherPatternMap:TERRATexture Read GetDitherPatternMap Write SetDitherPatternMap;
      Property ToonRamp:TERRATexture Read GetToonRamp Write SetToonRamp;

      Property HueShift:Single Read GetHueShift Write SetHueShift;

      Property EmitterFX:TERRAString Read _EmitterFX Write _EmitterFX;

//      Property AmbientColor:Color Read GetAmbientColor Write SetAmbientColor;
      Property DiffuseColor:Color  Read GetDiffuseColor Write SetDiffuseColor;
      Property ShadowColor:Color Read GetShadowColor Write SetShadowColor;

      Procedure CalculateTangents();
      Procedure CalculateTriangleNormals();
      //Procedure BuildBillboards;

	  	Function AddVertex():Integer;
  		Procedure AddTriangle(Const A,B,C:Integer);
	  	Procedure AddQuad(Const A,B,C,D:Integer);
      Procedure AddVertexPin(ID:Word);

      Procedure CullTriangles(Box:BoundingBox; Transform:Matrix4x4);
      Procedure UncullTriangles();

      Function Render(View:TERRAViewport; Const Transform:Matrix4x4; TranslucentPass:Boolean; State:MeshInstance):Boolean;

      //Function DuplicateVertex(Index:Integer):Integer;

      Function Intersect(Const R:Ray; Var T:Single; Const Transform:Matrix4x4):Boolean;

      Procedure SetTriangle(Const T:Triangle; Index:Integer);
      Function GetTriangle(Index:Integer):Triangle;

      Function GetTriangleNormal(Index:Integer):Vector3D;
      Function GetTrianglePointer(Index:Integer):PTriangle;

      Property VertexCount:Integer Read GetVertexCount Write SetVertexCount;
      Property TriangleCount:Integer Read _TriangleCount Write SetTriangleCount;

      Property Triangles:PTriangleArray Read GetTriangles;
      Property Vertices:VertexData Read GetVertices;

      Property GetBoundingBox:TERRA_BoundingBox.BoundingBox Read _BoundingBox;

      Property BlendMode:Integer Read GetBlendMode Write SetBlendMode;

      Property ID:Integer Read _ID;
	End;

  MeshMetadata = Class(TERRAObject)
    Name:TERRAString;
    Position:Vector3D;
    Content:TERRAString;
  End;

  MeshBoneMorph = Record
    MorphID:Integer;
    MorphType:Byte;
    Values:Array Of Vector3D;
  End;

	TERRAMesh = Class(Resource)
		Protected
			_Groups:Array Of MeshGroup;
			_GroupCount:Integer;
			_BoundingBox:BoundingBox;

      _Filter:MeshFilter;

      _AlphaStage:Integer;

      _Skeleton:MeshSkeleton;

      _BoneMorphs:Array Of MeshBoneMorph;
      _BoneMorphCount:Integer;

      _Skinning:Boolean;
      _NormalMapping:Boolean;

      _Metadata:Array Of MeshMetadata;
      _MetadataCount:Integer;

      _Emitters:Array Of MeshEmitter;
      _EmitterCount:Integer;

      _Lights:Array Of MeshLight;
      _LightCount:Integer;

      _ScratchID:Cardinal;

      Function GetSkeleton:MeshSkeleton;
      Function GetGroupCount: Integer;

      Function GetMeshFilter:MeshFilter;

      Procedure RemoveGroups;

		Public
      Constructor CreateFromFilter(Source:MeshFilter);
      Procedure Release; Override;

      Function Load(Source:Stream):Boolean; Override;
	    Function Save(Dest:Stream):Boolean;

      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;

      Property Filter:MeshFilter Read GetMeshFilter;

      Class Function GetManager:Pointer; Override;

      Procedure CullTriangles(Box:BoundingBox; Transform:Matrix4x4);
      Procedure UncullTriangles();

//			Procedure AddMesh(MyMesh:Mesh; Const Transform:Matrix4x4);
			Procedure AddTriangle(Const A,B,C:Integer; Group:MeshGroup);
			Procedure AddQuad(Const A,B,C,D:Integer; Group:MeshGroup);

			Function AddGroup(Format:VertexFormat; Name:TERRAString=''):MeshGroup;
      Function DuplicateGroup(Group:MeshGroup; Name:TERRAString=''):MeshGroup;
			Function GetGroup(Name:TERRAString):MeshGroup; Overload;
      Function GetGroup(Index:Integer):MeshGroup; Overload;

      Function GetGroupIndex(Name:TERRAString):Integer;

      Procedure AddMetadata(Name:TERRAString; Position:Vector3D; Content:TERRAString='');
      Function GetMetadata(Const Name:TERRAString):MeshMetadata; Overload;
      Function GetMetadata(Index:Integer):MeshMetadata; Overload;

      Function AddEmitter(Name:TERRAString; Position:Vector3D; Content:TERRAString; ParentBone:TERRAString):MeshEmitter;
      Function GetEmitter(Index:Integer):MeshEmitter;

      Function AddLight(Name:TERRAString; Position:Vector3D; LightType:Integer; LightColor:Color; Param1, Param2, Param3:Vector3D; ParentBone:TERRAString):MeshLight; Overload;
      Function AddLight(OtherLight:MeshLight):MeshLight; Overload;
      Function GetLight(Index:Integer):MeshLight;

      Procedure Transform(Const TargetTransform:Matrix4x4);

      Function PolyCount:Integer;

			Procedure Clean();

      Procedure Optimize(VertexCacheSize:Integer);

      Function HasBoneMorph(MorphID:Integer):Boolean;
      Function GetBoneMorph(MorphID, BoneID:Integer):Vector3D;
      Function AddBoneMorph(MorphID:Integer):Integer;
      Procedure SetBoneMorph(MorphID, BoneID:Integer; Const Value:Vector3D);

      Procedure UpdateBoundingBox;

      Procedure Clone(Source:TERRAMesh);

      Function Intersect(Const R:Ray; Var T:Single; Const Transform:Matrix4x4):Boolean;

			Property BoundingBox:BoundingBox Read _BoundingBox;

      Property GroupCount:Integer Read GetGroupCount;

      Property MetadataCount:Integer Read _MetadataCount;
      Property EmitterCount:Integer Read _EmitterCount;
      Property LightCount:Integer Read _LightCount;

      Property Skeleton:MeshSkeleton Read GetSkeleton;

      Property AlphaStage:Integer Read _AlphaStage;
	End;

  MeshMerger = Class(TERRAObject)
    Protected
      Procedure ProcessVertex(SourceVertex, DestVertex:MeshVertex; SourceGroup, DestGroup:MeshGroup); Virtual;
      Procedure ProcessTriangle(T:PTriangle; Source, Dest:MeshGroup); Virtual;
      Procedure ProcessGroup(Group:MeshGroup); Virtual;

    Public
      Procedure Release; Override;

      Function Merge(Source, Dest:TERRAMesh; DestFormat:VertexFormat; IndividualGroup:Boolean = False; MaxVertsPerGroup:Integer = -1; UpdateBox:Boolean = True):IntegerArrayObject;
      Procedure MergeGroup(Source, Dest:MeshGroup; UpdateBox:Boolean = True);
  End;

  MeshParticleEmitter = Class(ParticleSettingsEmitter)
    Protected
      _TargetGroup:MeshGroup;

    Public
      Constructor Create(Const FXName:TERRAString; Target:MeshGroup);
      Procedure Emit(Target:Particle); Override;
  End;

  CustomMeshFilter = Class(MeshFilter)
    Protected
      _Mesh:TERRAMesh;
      _Animations:Array Of Animation;
      _AnimationCount:Integer;

    Public
      Procedure AddAnimation(Anim:Animation);

      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):TERRAString; Override;
      Function GetGroupFlags(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):VertexFormat; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexTangent(GroupID, Index:Integer):Vector4D; Override;
      Function GetVertexBone(GroupID, Index:Integer):Integer; Override;
      Function GetVertexColor(GroupID, Index:Integer):Color; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;
      Function GetVertexUV2(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;
      Function GetDiffuseMapName(GroupID:Integer):TERRAString; Override;

      Function GetBoneCount():Integer; Override;
      Function GetBoneName(BoneID:Integer):TERRAString; Override;
      Function GetBoneParent(BoneID:Integer):Integer; Override;
      Function GetBonePosition(BoneID:Integer):Vector3D; Override;
      Function GetBoneRotation(BoneID:Integer):Vector3D; Override;

      Function GetAnimationCount():Integer; Override;
      Function GetAnimationName(AnimationID:Integer):TERRAString; Override;
      Function GetAnimationFrameRate(AnimationID:Integer):Single; Override;
      Function GetAnimationLoop(AnimationID:Integer):Boolean; Override;

      Function GetPositionKeyCount(AnimationID, BoneID:Integer):Integer; Override;
      Function GetRotationKeyCount(AnimationID, BoneID:Integer):Integer; Override;

      Function GetPositionKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey; Override;
      Function GetRotationKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey; Override;
  End;

  MeshManager = Class(ResourceManager)
    Protected
      _CubeMesh:TERRAMesh;
      _SphereMesh:TERRAMesh;
      _CylinderMesh:TERRAMesh;
      _PlaneMesh:TERRAMesh;

      Function GetCubeMesh:TERRAMesh;
      Function GetPlaneMesh:TERRAMesh;
      Function GetSphereMesh:TERRAMesh;
      Function GetCylinderMesh:TERRAMesh;

    Public
      Procedure Init; Override;
      Procedure Release; Override;

      Class Function Instance:MeshManager;

      Function GetMesh(Name:TERRAString):TERRAMesh;

      Property CubeMesh:TERRAMesh Read GetCubeMesh;
      Property CylinderMesh:TERRAMesh Read GetCylinderMesh;
      Property SphereMesh:TERRAMesh Read GetSphereMesh;
      Property PlaneMesh:TERRAMesh Read GetPlaneMesh;

      Property Meshes[Name:TERRAString]:TERRAMesh Read GetMesh; Default;
   End;


  Function CreatePlaneMesh(Const Normal:Vector3D; SubDivisions:Cardinal):TERRAMesh;

  Function SelectMeshShader(View:TERRAViewport; Group:MeshGroup; Position:Vector3D; Outline, TranslucentPass:Boolean; Var DestMaterial:MeshMaterial; UseTextureMatrix:Boolean):ShaderInterface;

  Function MakeWaterFlowBounds(Const Box:BoundingBox):Vector4D;

Implementation
Uses TERRA_Error, TERRA_Application, TERRA_Log, TERRA_ShaderFactory, TERRA_OS,
  TERRA_FileManager, TERRA_CRC32, TERRA_ColorGrading, TERRA_Solids;

Type
  MeshDataBlockHandler = Function(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
  MeshDataBlockHandlerEntry = Record
    Tag:FileHeader;
    Handler:MeshDataBlockHandler;
  End;

  GroupDataBlockHandler = Function(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
  GroupDataBlockHandlerEntry = Record
    Tag:FileHeader;
    Handler:GroupDataBlockHandler;
  End;

Var
  _MeshManager:ApplicationObject = Nil;
  _MeshDataHandlers:Array Of MeshDataBlockHandlerEntry;
  _MeshDataHandlerCount:Integer = 0;
  _GroupDataHandlers:Array Of GroupDataBlockHandlerEntry;
  _GroupDataHandlerCount:Integer = 0;

Function IsImageTranslucent(Tex:TERRATexture):Boolean;
Begin
  If Assigned(Tex) Then
    Result := (Tex.TransparencyType = imageTranslucent)
  Else
    Result := False;
End;

Function DefaultMeshHandler(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
Begin
  Source.Skip(Size);
  Result := True;
End;

Procedure RegisterMeshDataHandler(Tag:FileHeader; Handler:MeshDataBlockHandler);
Begin
  Inc(_MeshDataHandlerCount);
  SetLength(_MeshDataHandlers, _MeshDataHandlerCount);
  _MeshDataHandlers[Pred(_MeshDataHandlerCount)].Tag := Tag;
  _MeshDataHandlers[Pred(_MeshDataHandlerCount)].Handler := Handler;
End;

Function GetMeshDataHandler(Tag:FileHeader):MeshDataBlockHandler;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MeshDataHandlerCount) Do
  If (_MeshDataHandlers[I].Tag = Tag) Then
  Begin
    Result := _MeshDataHandlers[I].Handler;
    Exit;
  End;

  Result := DefaultMeshHandler;
End;

Function DefaultGroupHandler(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Begin
  Source.Skip(Size);
  Result := True;
End;

Procedure RegisterMeshGroupHandler(Tag:FileHeader; Handler:GroupDataBlockHandler);
Begin
  Inc(_GroupDataHandlerCount);
  SetLength(_GroupDataHandlers, _GroupDataHandlerCount);
  _GroupDataHandlers[Pred(_GroupDataHandlerCount)].Tag := Tag;
  _GroupDataHandlers[Pred(_GroupDataHandlerCount)].Handler := Handler;
End;

Function GetMeshGroupHandler(Tag:FileHeader):GroupDataBlockHandler;
Var
  I:Integer;
Begin
  //Log(logDebug, 'Mesh', 'Tag: '+Tag);
  For I:=0 To Pred(_GroupDataHandlerCount) Do
  If (_GroupDataHandlers[I].Tag = Tag) Then
  Begin
    Result := _GroupDataHandlers[I].Handler;
    Exit;
  End;

  Result := DefaultGroupHandler;
End;

{ Mesh handlers}
Function MeshReadGroup(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
Var
  Group:MeshGroup;
  ID:Integer;
  Format:Cardinal;
  Name:TERRAString;
Begin
  ID := Target._GroupCount;
  Inc(Target._GroupCount);
  SetLength(Target._Groups, Target._GroupCount);

  Source.ReadCardinal(Format);
  Source.ReadString(Name);

  Group := MeshGroup.Create(ID, Target, VertexFormatFromFlags(Format), Name);
  Group.Load(Source);
  Target._Groups[ID] := Group;

  Result := True;
End;

Function MeshReadSkeleton(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
Begin
  Target.Skeleton.Read(Source);
  Result := True;
End;

Function MeshReadEmitter(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  I := Target._EmitterCount;
  Inc(Target._EmitterCount);
  SetLength(Target._Emitters, Target._EmitterCount);

  Target._Emitters[I] := MeshEmitter.Create(Target);
  Source.ReadString(Target._Emitters[I].Name);
  Source.ReadString(Target._Emitters[I].Content);
  Source.ReadString(Target._Emitters[I].ParentBone);
  Source.Read(@Target._Emitters[I].Position, SizeOf(Vector3D));
  Target._Emitters[I].UpdateBone();

  Result := True;
End;

Procedure AddLightGeometry(Index:Integer; Source:MeshLight; Target:TERRAMesh);
Var
  S:SolidMesh;
  Height, Width:Single;
  Merger:MeshMerger;
  Temp:TERRAMesh;
  Format:VertexFormat;
  Dir:Vector3D;
  TargetTransform:Matrix4x4;
  Group:MeshGroup;
  It:VertexIterator;
  V:MeshVertex;
  Alpha:Byte;
Begin
  If Source = Nil Then
    Exit;

  Temp := Nil;
  TargetTransform := Matrix4x4Identity;

  Case Source.LightType Of
  lightTypeSpot:
    Begin
      Height := 50;
      Width := Tan(Source.Param1.Y * RAD) * Height;
      Dir := Source.Param2;

      If (Abs(Dir.Y)>=0.999) Then
        TargetTransform := Matrix4x4Transform(Source.Position, VectorCreate(0.0, 0.0, -Dir.Y*180*RAD), VectorCreate(Width, Height, Width))
      Else
        TargetTransform := Matrix4x4Orientation(Source.Position, Dir, VectorCreate(0, 1.0, 0.0), VectorCreate(Width, Height, Width));

      S := ConeMesh.Create(1, 8, False, False);
      Temp := CreateMeshFromSolid(S);

      ReleaseObject(S);
    End;
  End;

  If Assigned(Temp) Then
  Begin
    Format := [vertexFormatPosition, vertexFormatColor];

    Merger := MeshMerger.Create();
    Merger.Merge(Temp, Target, Format);
    ReleaseObject(Merger);
    ReleaseObject(Temp);

    Group := Target.GetGroup(Pred(Target.GroupCount));
    Group.Flags := meshGroupDepthOff Or meshGroupLightOff Or meshGroupNormalsOff;
    Group.BlendMode := blendAdd;

    Source.GroupIndex := Group.ID;

    It := Group.Vertices.GetIteratorForClass(MeshVertex);
    While It.HasNext() Do
    Begin
      V := MeshVertex(It.Value);

      Alpha := Trunc((1.0 - V.Position.Y) * 64);
      V.BaseColor := ColorCreate(Source.LightColor.R, Source.LightColor.G, Source.LightColor.B, Alpha);
      V.HueShift := 0.0;
      V.Position := TargetTransform.Transform(V.Position);
    End;
    ReleaseObject(It);

  End;
End;

Function MeshReadLights(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
  TargetLight:MeshLight;
Begin
  I := Target._LightCount;
  Inc(Target._LightCount);
  SetLength(Target._Lights, Target._LightCount);

  TargetLight := MeshLight.Create(Target);
  Target._Lights[I] := TargetLight;
  Source.ReadString(TargetLight.Name);
  Source.ReadString(TargetLight.ParentBone);
  Source.Read(@TargetLight.LightType, 1);
  Source.Read(@TargetLight.LightColor, 4);
  Source.Read(@TargetLight.Position, SizeOf(Vector3D));
  Source.Read(@TargetLight.Param1, SizeOf(Vector3D));
  Source.Read(@TargetLight.Param2, SizeOf(Vector3D));
  Source.Read(@TargetLight.Param3, SizeOf(Vector3D));
  TargetLight.UpdateBone();

  AddLightGeometry(I, TargetLight, Target);
  Result := True;
End;

Function MeshReadBoneMorph(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
Var
  N, I, Count:Integer;
Begin
  N := Target._BoneMorphCount;
  Inc(Target._BoneMorphCount);
  SetLength(Target._BoneMorphs, Target._BoneMorphCount);

  Source.Read(@Target._BoneMorphs[N].MorphID, 4);
  Source.Read(@Target._BoneMorphs[N].MorphType, 1);

  Count := Target.Skeleton.BoneCount;
  SetLength(Target._BoneMorphs[N].Values, Count);
  For I:=0 To Pred(Count) Do
    Source.Read(@Target._BoneMorphs[N].Values[I], SizeOf(Vector3D));

  Result := True;
End;

Function MeshReadMeta(Target:TERRAMesh; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  Inc(Target._MetadataCount);
  SetLength(Target._Metadata, Target._MetadataCount);
  For I:=0 To Pred(Target._MetadataCount) Do
  Begin
    Target._Metadata[I] := MeshMetadata.Create;
    Source.ReadString(Target._Metadata[I].Name);
    Source.ReadString(Target._Metadata[I].Content);
    Source.Read(@Target._Metadata[I].Position, SizeOf(Vector3D));
  End;

  Result := True;
End;

{ Group handlers }
Function GroupReadVertexData(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I, Count:Integer;
  Format:Cardinal;
  PX,PY,PZ:SmallInt;
  P:Vector3D;
  Temp:VertexData;
  NewFormat:VertexFormat;
Begin
  Target.Vertices.Read(Source);

  If (GraphicsManager.Instance.Renderer.Settings.NormalMapping.Enabled)
  And (Not Target.Vertices.HasAttribute(vertexTangent)) Then
  Begin
    Target.Vertices.AddAttribute(vertexFormatTangent);
    Target._NeedsTangentSetup := True;
  End;

  Result := True;
End;

Function GroupReadVertexMorphs(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  N,I:Integer;
Begin
  N := Target._MorphCount;
  Inc(Target._MorphCount);
  SetLength(Target._Morphs, Target._MorphCount);

  Source.Read(@Target._Morphs[N].ID, 4);
  Source.Read(@Target._Morphs[N].MorphType, 1);

  SetLength(Target._Morphs[N].Values, Target.VertexCount);
  For I:=0 To Pred(Target.VertexCount) Do
  Begin
    Source.Read(@Target._Morphs[N].Values[I], SizeOf(Vector3D));
  End;

  Result := True;
End;

Function GroupReadTriangleIndices(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  Source.Read(@Target._TriangleCount, 4);
  SetLength(Target._Triangles, Target._TriangleCount);

  Target._VisibleTriangleCount := Target._TriangleCount;

  For I:=0 To Pred(Target._TriangleCount) Do
    Source.Read(@Target._Triangles[I], SizeOf(Triangle));

  Result := True;
End;

Function GroupReadTriangleNormals(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  SetLength(Target._TriangleNormals, Target._TriangleCount);

  For I:=0 To Pred(Target._TriangleCount) Do
    Source.Read(@Target._TriangleNormals[I], SizeOf(Vector3D));

  Result := True;
End;

Function GroupReadTriangleEdges(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  SetLength(Target._Edges, Target._TriangleCount);

  For I:=0 To Pred(Target._TriangleCount) Do
    Source.Read(@Target._Edges[I], SizeOf(TriangleEdgesState));

  Result := True;
End;

Function GroupReadMaterialDiffuse(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.Read(@Target._Material.DiffuseColor, SizeOf(Color));
  Source.ReadString(S);
  Target._Material.DiffuseMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialTriplanar(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.TriplanarMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialSpecular(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.SpecularMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialBump(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.NormalMap := TextureManager.Instance.GetTexture(S);

  If Assigned(Target._Material.NormalMap) Then
  Begin
    Target._Material.NormalMap.Uncompressed := True;
    Target._Material.NormalMap.PreserveQuality := True;
  End;

  Result := True;
End;

Function GroupReadMaterialDisplacement(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.DisplacementMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialLightmap(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.LightMap := TextureManager.Instance.GetTexture(S);

  If Assigned(Target._Material.LightMap) Then
  Begin
    Target._Material.LightMap.Uncompressed := True;
    Target._Material.LightMap.PreserveQuality := True;
    Target._Material.LightMap.WrapMode := wrapNothing;
    //Target._Material.LightMap.MipMapped := False;
  End;

  Result := True;
End;

Function GroupReadMaterialRefraction(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.RefractionMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;


Function GroupReadMaterialReflective(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.ReflectiveMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;


Function GroupReadMaterialEnvMap(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.EnviromentMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialGlow(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.GlowMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialAlphaMap(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.AlphaMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialRamp(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:TERRAString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.ToonRamp := TextureManager.Instance.GetTexture(S);

  If Assigned(Target._Material.ToonRamp) Then
    Target._Material.ToonRamp.Uncompressed := True;

  Result := True;
End;

Function GroupReadMaterialBlendMode(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  N:Byte;
Begin
  Source.Read(@N, 1);
  Target._Material.BlendMode := N;
  Result := True;
End;

Function GroupReadMaterialParticles(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Begin
  Source.ReadString(Target._EmitterFX);
  Result := True;
End;

{  If (ChunkHeader=mcFurData) Then
  Begin
    Source.ReadString(FurSettings.Pattern);
    Source.ReadSingle(FurSettings.Thickness);
    Source.ReadSingle(FurSettings.Waviness);
    Source.ReadSingle(FurSettings.Length);
    Source.ReadSingle(FurSettings.Density);
    _Fur := Fur.Create(FurSettings, (_Owner.Skeleton.BoneCount>0));
  End Else

  If (ChunkHeader=mcClothData) Then
  Begin
    _Cloth := VerletCloth.Create(Self);
    Source.Read(_PinCount, 4);
    SetLength(_Pins, _PinCount);
    Source.Read(_Pins[0], _PinCount*2);

    For I:=0 To Pred(_PinCount) Do
      _Cloth.PinPoint(_Pins[I]);
  End Else
  }

{ MeshManager }
Class Function MeshManager.Instance:MeshManager;
Begin
  If _MeshManager = Nil Then
  Begin
    _MeshManager := InitializeApplicationComponent(MeshManager, GraphicsManager);
    MeshManager(_MeshManager.Instance).AutoUnload := False;
  End;

  Result := MeshManager(_MeshManager.Instance);
End;


Procedure MeshManager.Release;
Begin
  Inherited;

  ReleaseObject(_CubeMesh);

  _MeshManager := Nil;
End;

Function MeshManager.GetMesh(Name:TERRAString):TERRAMesh;
Var
  I, N:Integer;
  S:TERRAString;
  Filter:MeshFilter;
Begin
  Result := Nil;
  Name := StringTrim(Name);
  If (Name='') Then
    Exit;

  Result := TERRAMesh(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    S := FileManager.Instance.SearchResourceFile(Name+'.mesh');
    If S<>'' Then
    Begin
      Result := TERRAMesh.Create(rtLoaded, S);
      Result.Priority := 60;
      Self.AddResource(Result);
    End Else
    Begin
      N := -1;
      For I:=0 To Pred(MeshFilterCount) Do
      Begin
        S := FileManager.Instance.SearchResourceFile(Name+'.'+MeshFilterList[I].Extension);
        If (S<>'') Then
        Begin
          N := I;
          Break;
        End;
      End;

      If (S<>'') Then
      Begin
        Filter := MeshFilterList[N].Filter.Create;
        Filter.Load(S);
        Result := TERRAMesh.CreateFromFilter(Filter);
        ReleaseObject(Filter);
      End;
    End;
  End;
End;

(*Function MeshManager.CloneMesh(Name:TERRAString):Mesh;
Var
  S:TERRAString;
Begin
  Log(logDebug, 'ResourceManager', 'Cloning mesh '+Name);
  Name := StringTrim(Name);
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  S := FileManager.Instance.SearchResourceFile(Name+'.mesh');
  If S<>'' Then
  Begin
    Result := TERRAMesh.Create(rtLoaded, S);
  End Else
  Begin
    Result := Nil;
  End;
End;*)

Function MeshManager.GetCubeMesh: TERRAMesh;
Var
  Cube:TERRA_Solids.CubeMesh;
Begin
  If _CubeMesh = Nil Then
  Begin
    Cube := TERRA_Solids.CubeMesh.Create(2);
    _CubeMesh := CreateMeshFromSolid(Cube);
    ReleaseObject(Cube);
  End;

  Result := _CubeMesh;
End;

Function MeshManager.GetPlaneMesh: TERRAMesh;
Var
  Plane:TERRA_Solids.PlaneMesh;
Begin
  If _PlaneMesh = Nil Then
  Begin
    Plane := TERRA_Solids.PlaneMesh.Create(VectorUp, 4, -0.5, -0.5);
    _PlaneMesh := CreateMeshFromSolid(Plane);
    ReleaseObject(Plane);
  End;

  Result := _PlaneMesh;
End;

Function MeshManager.GetCylinderMesh:TERRAMesh;
Var
  Cylinder:TERRA_Solids.CylinderMesh;
Begin
  If _CylinderMesh = Nil Then
  Begin
    Cylinder := TERRA_Solids.CylinderMesh.Create(8, 8);
    _CylinderMesh := CreateMeshFromSolid(Cylinder);
    ReleaseObject(Cylinder);
  End;

  Result := _CylinderMesh;
End;

Function MeshManager.GetSphereMesh: TERRAMesh;
Var
  Sphere:TERRA_Solids.SphereMesh;
Begin
  If _SphereMesh = Nil Then
  Begin
    Sphere := TERRA_Solids.SphereMesh.Create(8);
    _SphereMesh := CreateMeshFromSolid(Sphere);
    ReleaseObject(Sphere);
  End;

  Result := _SphereMesh;
End;

Function CreatePlaneMesh(Const Normal:Vector3D; SubDivisions:Cardinal):TERRAMesh;
Var
  Plane:TERRA_Solids.PlaneMesh;
Begin
  Plane := TERRA_Solids.PlaneMesh.Create(Normal, SubDivisions, -0.5, -0.5);
  Result := CreateMeshFromSolid(Plane);
  ReleaseObject(Plane);
End;

Procedure MeshManager.Init;
Begin
  Inherited;

//  Self.UseThreads := True;
End;

{ MeshInstance }
Procedure MeshInstance.AddAttach(AttachMesh:TERRAMesh; BoneIndex:Integer; M:Matrix4x4; C:Color; IsStencil:Boolean);
Var
  P:Vector3D;
Begin
  If (BoneIndex<0) Or (AttachMesh = Nil) Then
    Exit;

  (*P := Self._TERRAMesh.Skeleton.BindPose[Succ(BoneIndex)].Transform(VectorZero);
  M := Matrix4x4Multiply4x4(Matrix4x4Inverse(Self._TERRAMesh.Skeleton.BindPose[Succ(BoneIndex)]), Matrix4x4Multiply4x4(M, Matrix4x4Translation(P)));
  *)

  P := VectorZero;
  M := Matrix4x4Multiply4x4(M, Matrix4x4Translation(P));

  Inc(_AttachCount);
  SetLength(_AttachList, _AttachCount);
  _AttachList[Pred(_AttachCount)].AttachMesh := AttachMesh;
  _AttachList[Pred(_AttachCount)].IsStencil := IsStencil;
  _AttachList[Pred(_AttachCount)].BoneIndex := BoneIndex;
  _AttachList[Pred(_AttachCount)].Matrix := M;
  _AttachList[Pred(_AttachCount)].Color := C;
End;

Procedure MeshInstance.ClearAttachs;
Begin
  _AttachCount := 0;
End;

Procedure MeshInstance.SetMotionBlur(Enabled:Boolean);
Var
  I:Integer;
Begin
  If (Enabled = _MotionBlur) Then
    Exit;

  If (Enabled) Then
  Begin
    For I:=0 To Pred(MaxTrailSize) Do
      _OldTransforms[I] := _Transform;
    _RenderTrails := True;
  End;

  _LastMotionBlur := Application.Instance.GetElapsedTime();

  _MotionBlur := Enabled;
End;

Procedure MeshInstance.SetVisibility(GroupID:Integer; Visible:Boolean); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Or (_Groups[GroupID].Visibility = Visible) Then
    Exit;

  _Groups[GroupID].Visibility := Visible;
  
  If _CastShadows Then
    _NeedsShadowUpdate := True;
End;

Function MeshInstance.GetVisibility(GroupID:Integer):Boolean; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := False
  Else
    Result := _Groups[GroupID].Visibility;
End;

Procedure MeshInstance.SetWireframeMode(GroupID:Integer; Enabled:Boolean); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Or (_Groups[GroupID].Wireframe = Enabled) Then
    Exit;

  _Groups[GroupID].Wireframe := Enabled;
End;

Function MeshInstance.GetWireframeMode(GroupID:Integer):Boolean; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := False
  Else
    Result := _Groups[GroupID].Wireframe;
End;

Procedure MeshInstance.SetHueShift(GroupID:Integer; Value:Single); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.HueShift := Value;
End;

Function MeshInstance.GetHueShift(GroupID:Integer):Single; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := 0.0
  Else
    Result := _Groups[GroupID].Material.HueShift;
End;

Function MeshInstance.GetUVOffset(GroupID:Integer):Vector2D;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := VectorCreate2D(0, 0)
  Else
    Result := VectorCreate2D(_Groups[GroupID].TextureTransform.V[12], _Groups[GroupID].TextureTransform.V[13]);
End;

Procedure MeshInstance.SetUVOffset(GroupID:Integer; X,Y:Single);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].UseTextureMatrix := True;
  _Groups[GroupID].TextureTransform.V[12] := X;
  _Groups[GroupID].TextureTransform.V[13] := Y;
End;

Procedure MeshInstance.SetUVScale(GroupID:Integer; X,Y:Single);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].UseTextureMatrix := True;
  _Groups[GroupID].TextureTransform.V[0] := X;
  _Groups[GroupID].TextureTransform.V[5] := Y;
End;

Procedure MeshInstance.SetTextureTransform(GroupID:Integer; Transform:Matrix4x4);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].UseTextureMatrix := True;
  _Groups[GroupID].TextureTransform := Transform;
End;

Function MeshInstance.GetTextureTransform(GroupID:Integer):Matrix4x4;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Matrix4x4Identity
  Else
    Result := _Groups[GroupID].TextureTransform;
End;

Function MeshInstance.GetBlendMode(GroupID: Integer): Integer;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := blendNone
  Else
    Result := _Groups[GroupID].Material.BlendMode;
End;

Procedure MeshInstance.SetBlendMode(GroupID, Mode: Integer);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.BlendMode := Mode;
End;

Function MeshInstance.GetShadowColor(GroupID: Integer): Color;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := ColorBlack
  Else
    Result := _Groups[GroupID].Material.ShadowColor;
End;

Procedure MeshInstance.SetShadowColor(GroupID: Integer; MyColor: Color);
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.ShadowColor := MyColor;
End;

Procedure MeshInstance.SetDiffuseColor(MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Mesh._GroupCount) Do
    SetDiffuseColor(I, MyColor);
End;

Procedure MeshInstance.SetDiffuseColor(GroupID:Integer; MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.DiffuseColor := MyColor;
End;

Function MeshInstance.GetDiffuseColor(GroupID:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := ColorWhite
  Else
  Begin
    Result := _Groups[GroupID].Material.DiffuseColor;
    Result.A := Trunc(Result.A * _AlphaLODValue);
  End;
End;

(*Procedure MeshInstance.SetAmbientColor(GroupID:Integer; MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.AmbientColor := MyColor;
End;

Function MeshInstance.GetAmbientColor(GroupID:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := ColorWhite
  Else
    Result := _Groups[GroupID].Material.AmbientColor;
End;*)

Procedure MeshInstance.SetOutlineColor(MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Mesh.GroupCount) Do
    _Groups[I].Material.OutlineColor := MyColor;
End;

Procedure MeshInstance.SetOutlineColor(GroupID:Integer; MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.OutlineColor := MyColor;
End;

Procedure MeshInstance.SetWaterFlowBounds(GroupID:Integer; Bounds:Vector4D);
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.FlowBounds := Bounds;
End;

Function MeshInstance.GetWaterFlowBounds(GroupID:Integer):Vector4D;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Vector4DZero
  Else
    Result := _Groups[GroupID].Material.FlowBounds;
End;

Procedure MeshInstance.SetWaterFlowSpeed(GroupID:Integer; Speed:Single);
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.FlowSpeed := Speed;
End;

Function MeshInstance.GetWaterFlowSpeed(GroupID:Integer):Single;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := 0.0
  Else
    Result := _Groups[GroupID].Material.FlowSpeed;
End;

Function MeshInstance.GetOutlineColor(GroupID:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := ColorNull
  Else
    Result := _Groups[GroupID].Material.OutlineColor;
End;


Function MeshInstance.GetVegetationBend(GroupID: Integer): Single;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := 0
  Else
    Result := _Groups[GroupID].Material.VegetationBend;
End;

Procedure MeshInstance.SetVegetationBend(GroupID: Integer; Bend:Single);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.VegetationBend := Bend;
End;

Procedure MeshInstance.SetGhostMode(Ghost:Boolean);
Var
  I:Integer;
Begin
  If (_Mesh = Nil) Then
    Exit;

  For I:=0 To Pred(_Mesh._GroupCount) Do
    _Groups[I].Material.Ghost := Ghost;
End;

Procedure MeshInstance.SetPosition(P: Vector3D);
Var
  Ofs:Vector3D;
Begin
  {If (Self = Nil) Then
  Begin
    Log(logWarning, 'Mesh', 'Null mesh instance');
    Exit;
  End;}

  Ofs := VectorSubtract(P, _Position);

  If (Abs(Ofs.X)<MinDelta) And (Abs(Ofs.Y)<MinDelta) And (Abs(Ofs.Z)<MinDelta) Then
    Exit;

  If Assigned(_ShadowVolume) Then
    _ShadowVolume.Translate(Ofs);

  _BoundingBox.StartVertex.Add(Ofs);
  _BoundingBox.EndVertex.Add(Ofs);

  _Position := P;
  _NeedsPositionUpdate := False;
  _NeedsTransformUpdate := True;
End;

Procedure MeshInstance.SetRotation(P: Vector3D); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  {If (Self = Nil) Then
  Begin
    Log(logWarning, 'Mesh', 'Null mesh instance');
    Exit;
  End;}

  If (_Rotation.Distance(P)<MinDelta) Then
    Exit;

  _Rotation := P;

  _NeedsRotationUpdate := False;
  _NeedsTransformUpdate := True;

  _NeedsShadowUpdate := True;
End;

Procedure MeshInstance.SetScale(P: Vector3D); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (_Scale.Distance(P)<MinDelta) Then
    Exit;

  _Scale := P;
  _NeedsTransformUpdate := True;
End;

Procedure MeshInstance.SetGroupLocalTransform(GroupID:Integer; Const Transform: Matrix4x4);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].GeometryTransform := Transform;
  _Groups[GroupID].GeometryTransformType := customTransformLocal;
End;

Procedure MeshInstance.SetGroupGlobalTransform(GroupID:Integer; Const Transform: Matrix4x4);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].GeometryTransform := Transform;
  _Groups[GroupID].GeometryTransformType := customTransformGlobal;
End;

Procedure MeshInstance.UpdateBoundingBox;
Begin
  _BoundingBox := _Mesh.BoundingBox;
  {For I:=0 To Pred(_AttachCount) Do
    Box := BoundingBoxAdd(Box, _AttachList[I].AttachMesh.BoundingBox);}

  _BoundingBox.Transform(_Transform);

  If Assigned(_Mesh) Then
    _ScratchID := _Mesh._ScratchID;
End;

Procedure MeshInstance.UpdateTransform;
Begin
  If (_Mesh=Nil) Or (Not _Mesh.IsReady) Then
    Exit;

  _NeedsTransformUpdate := False;
//  _NeedsShadowUpdate := True;
  _Transform := Matrix4x4Transform(_Position, _Rotation, _Scale);
  UpdateBoundingBox();
End;

Constructor MeshInstance.Create(MyMesh: TERRAMesh);
Begin
  _ClonedMesh := False;
  _Position := VectorZero;
  _Rotation := VectorZero;
  _Scale := VectorOne;
  _NeedsTransformUpdate := True;
  _NeedsShadowUpdate := True;

  Self.Diffuse := ColorWhite;

  _LastFrameID := 0;
  _LastTrailUpdate := Application.Instance.GetElapsedTime();

  Self.SetGeometry(MyMesh);
End;

Procedure MeshInstance.SetGeometry(MyMesh:TERRAMesh);
Var
  N, I:Integer;
Begin
  If (MyMesh = Nil) Then
  Begin
    Log(logWarning, 'Mesh', 'Attemping to create instance from null mesh.');
    Exit;
  End;

  _Mesh := MyMesh;

  _Mesh.PreFetch();

  CullGroups := False;

  SetLength(_Groups, _Mesh.GroupCount);

  _ParticleSystemCount := _Mesh.EmitterCount;
  For I:=0 To Pred(_Mesh.GroupCount) Do
  If (_Mesh._Groups[I].EmitterFX<>'') Then
    Inc(_ParticleSystemCount);

  SetLength(_ParticleSystems, _ParticleSystemCount);
  For I:=0 To Pred(_ParticleSystemCount) Do
    _ParticleSystems[I] := Nil;

  _EmitterCount := 0;

  _LightCount := _Mesh.LightCount;
  SetLength(_Lights, _LightCount);
  For I:=0 To Pred(_LightCount) Do
    _Lights[I].Light := Nil;

  N := 0;
  For I:=0 To Pred(_Mesh.GroupCount) Do
  Begin
    _Groups[I].Visibility := (_Mesh._Groups[I].Flags And meshGroupHidden=0);
    _Groups[I].Material.Reset();
    _Groups[I].GeometryTransformType := customTransformNone;
    _Groups[I].TextureTransform := Matrix4x4Identity;

    If (_Mesh._Groups[I].Flags And meshGroupCastShadow<>0) Then
      Inc(N);
  End;

  CastShadows := (N>0);

  //ReleaseObject(_Animation);
End;

Procedure MeshInstance.Release;
Var
  I:Integer;
Begin
  Inherited;

  For I:=0 To Pred(Self._ParticleSystemCount) Do
    ReleaseObject(_ParticleSystems[I]);
  _ParticleSystemCount := 0;

  For I:=0 To Pred(Self._FXCount) Do
    ReleaseObject(_FX[I]);

  For I:=0 To Pred(Self._EmitterCount) Do
    ReleaseObject(_Emitters[I]);
  _EmitterCount := 0;

  For I:=0 To Pred(Self._LightCount) Do
    ReleaseObject(_Lights[I]);
  _LightCount := 0;

  If (_ClonedMesh) Then
    ReleaseObject(_Mesh);
  
  ReleaseObject(_ShadowVolume);
  ReleaseObject(_Animation);
End;

Function MeshInstance.GetBoundingBox:BoundingBox; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (_NeedsTransformUpdate) Then
    UpdateTransform()
  Else
  If (Assigned(_Mesh)) And (_ScratchID <> _Mesh._ScratchID) Then
    UpdateBoundingBox();

  Result := _BoundingBox;
  If GraphicsManager.Instance.ReflectionActive Then
    Result.Transform(GraphicsManager.Instance.ReflectionMatrix);
End;

Procedure MeshInstance.Update(View:TERRAViewport);
Var
  I,J, N:Integer;
  S:Single;
  M:Matrix4x4;
  B:MeshBone;
Begin
  If (_Mesh = Nil) Or (Not _Mesh.IsReady) Then
    Exit;

  If (Not _ClonedMesh) And (_Mesh.GroupCount <> Length(_Groups)) Then
  Begin
    SetGeometry(_Mesh);
  End;

  I:=0;
  While I<_FXCount Do
  Begin
    If _FX[I].Update() Then
    Begin
      Inc(I);
    End Else
    Begin
      If Assigned(_FX[I]._Callback) Then
        _FX[I]._Callback(_FX[I], _FX[I]._UserData);
      ReleaseObject(_FX[I]);
      _FX[I] := _FX[Pred(_FXCount)];
      Dec(_FXCount);
    End;
  End;

  If (Assigned(_Animation)) Then
    _Animation.Update;

  If (_RenderTrails) And (Application.Instance.GetElapsedTime()-_LastTrailUpdate>TrailDelay Div MaxTrailSize) Then
  Begin
    For I:=Pred(MaxTrailSize) DownTo 1 Do
    Begin
      _OldTransforms[I] := _OldTransforms[Pred(I)];
    End;

    _OldTransforms[0] := Transform;
    _LastTrailUpdate := Application.Instance.GetElapsedTime();
    If MotionBlur Then
      _LastMotionBlur := _LastTrailUpdate;
  End;
End;

Procedure MeshInstance.RenderLights(View:TERRAViewport);
Var
  I:Integer;
  M, Transform:Matrix4x4;
  Box:BoundingBox;
  GroupTransform:Boolean;
  MyLight:MeshLight;
  TargetLight:PositionalLight;
  N:Single;
  P:Vector3D;
Begin
  If (_Mesh = Nil ) Or (_Mesh.Status <> rsReady) Then
    Exit;

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Mesh', 'Rendering mesh lights');
  Log(logDebug, 'Mesh', 'Total '+IntToString(Self._LightCount)+' lights in '+_Mesh.Name);
  {$ENDIF}

  For I:=0 To Pred(Self._LightCount) Do
  Begin
    MyLight := _Mesh.GetLight(I);
    If (MyLight = Nil) Then
      Continue;

    If (_Lights[I].Light = Nil) Then
    Begin
      Case MyLight.LightType Of
      lightTypePoint:
        Begin
          _Lights[I].Light := PointLight.Create(VectorZero);
          {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Mesh', 'Creating point light...');{$ENDIF}
        End;

      lightTypeSpot:
        Begin
          _Lights[I].Light := SpotLight.Create(VectorZero, VectorUp {MyLight.Param2}, MyLight.Param1.X * RAD, MyLight.Param1.Y * RAD);
          {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Mesh', 'Creating point light...');{$ENDIF}
        End;

      Else
        Continue;
      End;

      _Lights[I].Enabled := True;
      _Lights[I].GroupID := MyLight.GroupIndex;
    End;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Mesh', 'Transforming light '+IntToString(I));{$ENDIF}

    P := MyLight.Position;
    If (MyLight.BoneIndex>=0) Then
    Begin
      M := Animation.Transforms[Succ(MyLight.BoneIndex)];
      M := Matrix4x4Multiply4x3(_Transform, M);
    End Else
      M := _Transform;

    P := M.Transform(P);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Mesh', 'Setting light '+IntToString(I)+' properties');{$ENDIF}

    TargetLight := _Lights[I].Light;
    TargetLight.Position := P;
    TargetLight.Color := MyLight.LightColor;

    Case MyLight.LightType Of
      lightTypeSpot:
        Begin
          (*N := MyLight.Param1.X;
          SpotLight(_Lights[I]).Radius := N;*)
        End;

      lightTypePoint:
        Begin
          N := MyLight.Param1.X;


          If (MyLight.Param3.X>0) Then
            N := N + N * 0.05 * Abs(Cos(RAD*(Trunc(Application.GetTime()/MyLight.Param3.X) Mod 180)));

            //Flicker Param3.y ??? {FIXME}

          PointLight(TargetLight).Radius := N;
        End;
    End;

    If _Lights[I].GroupID>=0 Then
      Self.SetVisibility(_Lights[I].GroupID, _Lights[I].Enabled);

    If _Lights[I].Enabled Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Mesh', 'Adding light to manager...');{$ENDIF}
      LightManager.Instance.AddLight(View, TargetLight);
    End;
  End;
End;

Procedure MeshInstance.DrawParticles(View:TERRAViewport);
Var
  I, J, ID,ID2:Integer;
  M, Transform:Matrix4x4;
  Box:BoundingBox;
  GroupTransform:Boolean;
  Emitter:MeshEmitter;
  N:Single;
  P:Vector3D;
Begin
  ID := 0;
  ID2 := 0;

  For I:=0 To Pred(Self._ParticleSystemCount) Do
  Begin
    If (I<_Mesh.EmitterCount) Then
    Begin
      Emitter := _Mesh.GetEmitter(I);
    End Else
    Begin
      While (ID<_Mesh._GroupCount) And (_Mesh._Groups[ID].EmitterFX='') Do
      Begin
        Inc(ID);
      End;

      If (ID>=_Mesh._GroupCount) Then
      Begin
        If (ID2>=_EmitterCount) Then
          Break;
          
        Emitter := _Emitters[ID2];
        Inc(ID2);
      End Else
      Begin
        If (_ParticleSystems[I]=Nil) Then
        Begin
          _ParticleSystems[I] := ParticleCollection.Create(MeshParticleEmitter.Create(_Mesh._Groups[ID].EmitterFX, _Mesh._Groups[ID]));
          _ParticleSystems[I].Respawn := True;
        End;

        P := Self._Position;
        Emitter := Nil;
      End;
    End;

    If Assigned(Emitter) Then
    Begin
      If (_ParticleSystems[I]=Nil) Then
      Begin
        _ParticleSystems[I] := ParticleCollection.Create(ParticleSettingsEmitter.Create(Emitter.Content, VectorAdd(Emitter.Position, Self.Position)));
        _ParticleSystems[I].Respawn := True;
      End;

      P := Emitter.Position;
      If (Emitter.BoneIndex>=0) Then
      Begin
        M := Animation.Transforms[Succ(Emitter.BoneIndex)];
        M := Matrix4x4Multiply4x3(_Transform, M);
      End Else
        M := _Transform;
    End Else
    Begin
    	M := Matrix4x4Identity;
    	P := VectorZero;
    End;

    If _ParticleSystems[I].Emitter Is PositionalParticleEmitter Then
    Begin
      P := M.Transform(P);
      PositionalParticleEmitter(_ParticleSystems[I].Emitter).Position := P;
    End;

    _ParticleSystems[I].Update(View);
    GraphicsManager.Instance.AddRenderable(View, _ParticleSystems[I]);
  End;
End;

Procedure MeshInstance.DrawMesh(View:TERRAViewport; Const MyTransform:Matrix4x4; TranslucentPass, StencilTest:Boolean);
Var
  I:Integer;
  M, Transform:Matrix4x4;
  Box:BoundingBox;
  GroupTransform:Boolean;
  Emitter:MeshEmitter;
  N:Single;
  P:Vector3D;
  Reflections:Boolean;
Begin
  If Not _Mesh.IsReady() Then
    Exit;

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Mesh', 'Drawing mesh '+Self.Geometry.Name);
  {$ENDIF}

  Reflections := GraphicsManager.Instance.ReflectionActive;

  For I:=0 To Pred(_Mesh._GroupCount) Do
  If (_StencilID=0) Or ((_Mesh._Groups[I].Flags And meshGroupStencilTest<>0) = (StencilTest Or Reflections)) Then
  Begin
    If (_Mesh._Groups[I].Flags And meshGroupStencilMask<>0) Then
      Continue;

    Case _Groups[I].GeometryTransformType Of
      customTransformLocal: Transform := Matrix4x4Multiply4x4(MyTransform, _Groups[I].GeometryTransform);
      customTransformGlobal: Transform := _Groups[I].GeometryTransform;
      Else
        Transform := MyTransform;
    End;

    If (CullGroups) Then
    Begin
      Box := _Mesh._Groups[I]._BoundingBox;
      Box.Transform(Transform);
      If Not GraphicsManager.Instance.IsBoxVisible(View, Box) Then
        Continue;
    End;

    If (IsGroupTranslucent(I) = TranslucentPass) Then
	    _Mesh._Groups[I].Render(View, Transform, TranslucentPass, Self);
  End;
End;

Procedure MeshInstance.Render(View:TERRAViewport; Const Bucket:Cardinal);
Var
  C:Color;
  Time:Cardinal;
  I, J:Integer;
  M:Matrix4x4;
  Temp:Matrix4x4;
  S:Single;
  Graphics:GraphicsManager;
  TranslucentPass:Boolean;
Begin
  If (_Mesh=Nil) Then
    Exit;

  TranslucentPass := (Bucket And renderBucket_Translucent<>0);

  Graphics := GraphicsManager.Instance;

  If Assigned(_Body) Then
  Begin
    Self._Transform := Matrix4x4Multiply4x4(_Body.Transform, Matrix4x4Scale(_Scale));
    Self.UpdateBoundingBox();

    _NeedsTransformUpdate := False;
    _NeedsRotationUpdate := True;
    _NeedsPositionUpdate := True;
  End;

  If (_NeedsTransformUpdate) Then
    UpdateTransform();

  (*If (Graphics.RenderStage=renderStageShadow) And (_CastShadows)
  And (Graphics.Renderer.Settings.DynamicShadows.Enabled) Then
  Begin
    If (Not Assigned(_ShadowVolume)) Then
    Begin
      _ShadowVolume := ShadowVolume.Create;
      _NeedsShadowUpdate := True;
    End;

    If (Assigned(_Animation)) And (Assigned(_Animation.Root)) Then
      _NeedsShadowUpdate := True;

    If (_NeedsShadowUpdate)  Then
    Begin
      _ShadowVolume.Rebuild(_Mesh, Self);
      _NeedsShadowUpdate := False;
    End;

    _ShadowVolume.Render;
    Exit;
  End; stencilshadows*)

  If (Graphics.Renderer.Settings.AlphaFade.Enabled) Then
  Begin
    If (_LOD>=2.0) Then
      _AlphaLODValue := 0.0
    Else
    If (_LOD>1.0) Then
      _AlphaLODValue := 1.0 - ((_LOD - 1.0) / 1.0)
    Else
      _AlphaLODValue := 1.0;

    If (_AlphaLODValue<=0.0) Then
      Exit;
  End Else
    _AlphaLODValue := 1.0;

  _StencilID := 0;
  If (Not Graphics.ReflectionActive) And (Graphics.RenderStage<>renderStageShadow) Then
  Begin
    For I:=0 To Pred(_AttachCount) Do
    If (_AttachList[I].IsStencil) Then
    Begin
      _StencilID := Graphics.GenerateStencilID();
      Break;
    End;

    If (_StencilID<=0) Then
    For I:=0 To Pred(_Mesh._GroupCount) Do
    If (_Mesh._Groups[I].Flags And meshGroupStencilMask<>0) Then
    Begin
      _StencilID := Graphics.GenerateStencilID();
      Break;
    End;
  End;

  If _StencilID>0 Then
  Begin
    //glClear(GL_STENCIL_BUFFER_BIT);
    Graphics.Renderer.SetStencilTest(True);
    Graphics.Renderer.SetStencilFunction(compareAlways, _StencilID, $FFFFFFFF);
    Graphics.Renderer.SetStencilOp(stencilReplace, stencilReplace, stencilReplace);
    Graphics.Renderer.SetColorMask(False, False, False, False);
    Graphics.Renderer.SetDepthMask(False);
    Graphics.Renderer.SetCullMode(cullBack);

    For I:=0 To Pred(_AttachCount) Do
    If (_AttachList[I].IsStencil) Then
    Begin
      M := Matrix4x4Multiply4x3(_Transform, Matrix4x4Multiply4x3(Animation.Transforms[Succ(_AttachList[I].BoneIndex)], _AttachList[I].Matrix));

      _AttachList[I].AttachMesh.Prefetch();

      For J:=0 To Pred(_AttachList[I].AttachMesh._GroupCount) Do
      Begin
        C := _AttachList[I].AttachMesh._Groups[J].DiffuseColor;
        _AttachList[I].AttachMesh._Groups[J].Flags := meshGroupColorOff;
  	    _AttachList[I].AttachMesh._Groups[J].Render(View, M, TranslucentPass, Nil);
      End;
    End;

    For I:=0 To Pred(_Mesh._GroupCount) Do
    If (_Mesh._Groups[I].Flags And meshGroupStencilMask<>0) Then
    Begin
      Self._Mesh._Groups[I].Render(View, _Transform, TranslucentPass, Self);
    End;

    Graphics.Renderer.SetDepthMask(True);
    Graphics.Renderer.SetColorMask(True, True, True, True);
    Graphics.Renderer.SetStencilFunction(compareDifferent, _StencilID, $FFFFFFFF);
    Graphics.Renderer.SetStencilOp(stencilKeep, stencilKeep, stencilKeep);
  End;

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Rendering main mesh');{$ENDIF}

  If (_StencilID>0) Then
  Begin
    DrawMesh(View, _Transform, TranslucentPass, True);
    Graphics.Renderer.SetStencilTest(False);
  End;

  DrawMesh(View, _Transform, TranslucentPass, False);

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Main mesh done');{$ENDIF}

  If (_RenderTrails) And (TranslucentPass) And (Graphics.RenderStage=renderStageDiffuse) Then
  Begin
    For I:=0 To Pred(_Mesh.GroupCount) Do
      _Groups[I].TempAlpha := _Groups[I].Material.DiffuseColor.A;
    Temp := _Transform;

    Time := Application.Instance.GetElapsedTime();
    _RenderTrails := False;

    For J:=Pred(MaxTrailSize) DownTo 0 Do
    Begin
      For I:=0 To Pred(_Mesh.GroupCount) Do
      Begin
        S := (Time - _LastMotionBlur)/ TrailDecay;
        If (S>1.0) Then
          _Groups[I].Material.DiffuseColor.A := 0
        Else
        Begin
          S := 1.0 - S;
          S := S * (1.0 - (Succ(J)/MaxTrailSize));
          _Groups[I].Material.DiffuseColor.A := Trunc(_Groups[I].TempAlpha * 0.75 * S);
          _RenderTrails := _Groups[I].Material.DiffuseColor.A>0;
        End;
      End;

      If _RenderTrails Then
      Begin
        S := 0.75 + 0.25 * (1.0 - (J/Pred(MaxTrailSize)));
        _Transform := Matrix4x4Multiply4x3(_OldTransforms[J], Matrix4x4Scale(S, S, S));
        DrawMesh( View, _Transform, TranslucentPass, False);
      End;
    End;

    _Transform := Temp;
    For I:=0 To Pred(_Mesh.GroupCount) Do
      _Groups[I].Material.DiffuseColor.A := _Groups[I].TempAlpha;
  End;

  For I:=0 To Pred(_AttachCount) Do
  If (Not _AttachList[I].IsStencil) Then
  Begin
{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Rendering attach '+IntToString(I));{$ENDIF}

    //M := MatrixMultiply4x3(_Transform, MatrixMultiply4x3(Animation.Transforms[Succ(_AttachList[I].BoneIndex)], _AttachList[I].Matrix));
    M := Matrix4x4Multiply4x3(_Transform, Matrix4x4Multiply4x3(Animation.Transforms[Succ(_AttachList[I].BoneIndex)], _AttachList[I].Matrix));

    If Not _AttachList[I].AttachMesh.IsReady() Then
      Continue;

    For J:=0 To Pred(_AttachList[I].AttachMesh._GroupCount) Do
    Begin
      C := _AttachList[I].AttachMesh._Groups[J].DiffuseColor;
      _AttachList[I].AttachMesh._Groups[J].DiffuseColor := ColorMultiply(_AttachList[I].AttachMesh._Groups[J].DiffuseColor, _AttachList[I].Color);
	    _AttachList[I].AttachMesh._Groups[J].Render(View, M, TranslucentPass, Nil);
      _AttachList[I].AttachMesh._Groups[J].DiffuseColor := C;
    End;
  End;

  If (TranslucentPass) Then
    Self.DrawParticles(View);

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Inherited mesh');{$ENDIF}
End;

Function MeshInstance.GetAttach(Index: Integer): PMeshAttach;
Begin
  If (Index<0) Or (Index>=_AttachCount) Then
    Result := Nil
  Else
    Result := @_AttachList[Index];
End;

Function MeshInstance.GetName:TERRAString;
Begin
  If Assigned(_Mesh) Then
    Result := _Mesh.Name + '(X:'+FloatToString(_Position.X)+ ', Y:'+FloatToString(_Position.Y)+ ', Z:'+FloatToString(_Position.Z)+')'
  Else
    Result := 'Undefined';
End;

Function MeshInstance.GetRenderBucket:Cardinal;
Begin
  If Self.IsOpaque Then
    Result := renderBucket_Opaque
  Else
    Result := 0;

  If Self.IsTranslucent Then
    Result := Result Or renderBucket_Translucent;
End;

Function MeshInstance.IsOpaque:Boolean;
Var
  I:Integer;
Begin
  If (_Mesh = Nil) Or (_RenderTrails) Or (Diffuse.A<255) Then
  Begin
    Result := False;
    Exit;
  End;

  For I:=0 To Pred(_Mesh.GroupCount) Do
  If (_Groups[I].Visibility) And (_Mesh._Groups[I].TriangleCount>0) And (Not IsGroupTranslucent(I)) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Function MeshInstance.IsTranslucent: Boolean;
Var
  I:Integer;
Begin
  If (_Mesh = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

  If (_RenderTrails) Or (Diffuse.A<255) Or (_ParticleSystemCount>0) Then
  Begin
    Result := True;
    Exit;
  End;

  For I:=0 To Pred(_Mesh.GroupCount) Do
  If (_Groups[I].Visibility) And (_Mesh._Groups[I].TriangleCount>0) And (IsGroupTranslucent(I)) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Function MeshInstance.IsGroupTranslucent(Index: Integer): Boolean;
Var
  Group:MeshGroup;
Begin
  Result := False;

  If (_Mesh = Nil) Then
    Exit;

  Group := _Mesh.GetGroup(Index);
  If (Group = Nil) Or (Group.Flags And meshGroupForceOpaque<>0) Or (Index>=Length(_Groups)) Then
    Exit;

  Result := (Self.Diffuse.A<255) Or (_Groups[Index].Material.DiffuseColor.A<255) Or (Group.DiffuseColor.A<255) Or (_Groups[Index].Material.Ghost)
  Or (IsImageTranslucent(_Groups[Index].Material.DiffuseMap)) Or (IsImageTranslucent(Group.DiffuseMap));
End;

Function MeshInstance.IsReady: Boolean;
Var
  I:Integer;
  Group:MeshGroup;
Begin
  Result := False;

  If (_Mesh = Nil) Or (_Mesh.Status<>rsReady) Then
    Exit;

  For I:=0 To Pred(_Mesh.GroupCount) Do
  Begin
    Group := _Mesh.GetGroup(I);

    If (Group.DiffuseMap<>Nil) And (Group.DiffuseMap.Status<>rsReady) Then
      Exit;
  End;

  Result := True;
End;

Function MeshInstance.AddParticleEmitter(Const Name:TERRAString; Position: Vector3D; Const Content:TERRAString; Const ParentBone:TERRAString):MeshEmitter;
Begin
  If (Name = '') Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := MeshEmitter.Create(Self._Mesh);
  Result.Name := Name;
  Result.Content := Content;
  Result.ParentBone := ParentBone;
  Result.Position := Position;
  Result.UpdateBone();

  Inc(_EmitterCount);
  SetLength(_Emitters, _EmitterCount);
  _Emitters[Pred(_EmitterCount)] := Result;

  Inc(_ParticleSystemCount);
  SetLength(_ParticleSystems, _ParticleSystemCount);
  _ParticleSystems[Pred(_ParticleSystemCount)] := Nil;
End;

Function MeshInstance.AddEffect(FX: MeshFX):TERRAMesh;
Var
  I:Integer;
  Old:TERRAMesh;
Begin
  If FX = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  If (Not _ClonedMesh) Then
  Begin
    Old := _Mesh;
    _Mesh := TERRAMesh.Create(rtDynamic, Old.Name);
    _Mesh.Clone(Old);// MeshManager.Instance.CloneMesh(_Mesh.Name);
    _Mesh.Prefetch();

    For I:=0 To Pred(_Mesh.GroupCount) Do
      _Mesh._Groups[I].Flags := _Mesh._Groups[I].Flags Or meshGroupDynamic;

    _ClonedMesh := True;
  End;

  Inc(_FXCount);
  SetLength(_FX, _FXCount);
  _FX[Pred(_FXCount)] := FX;

  For I:=0 To Pred(_FXCount) Do
    _FX[I]._Target := _Mesh;

  Result := _Mesh;
End;

Function MeshInstance.GetAlphaMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.AlphaMap;
End;

Function MeshInstance.GetToonRamp(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.ToonRamp;
End;

Function MeshInstance.GetDecalMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.DecalMap;
End;

Function MeshInstance.GetDiffuseMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.DiffuseMap;
End;

Function MeshInstance.GetGlowMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.GlowMap;
End;

Function MeshInstance.GetLightMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.LightMap;
End;


Function MeshInstance.GetDitherPatternMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.DitherPatternMap;
End;

Function MeshInstance.GetNormalMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.NormalMap;
End;

Function MeshInstance.GetDisplacementMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.DisplacementMap;
End;


Function MeshInstance.GetRefractionMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.RefractionMap;
End;

Function MeshInstance.GetReflectiveMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.ReflectiveMap;
End;

Function MeshInstance.GetEnviromentMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.EnviromentMap;
End;

Function MeshInstance.GetFlowMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.FlowMap;
End;

Function MeshInstance.GetNoiseMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.NoiseMap;
End;

Function MeshInstance.GetSpecularMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.SpecularMap;
End;

Function MeshInstance.GetColorTable(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.ColorTable;
End;

Function MeshInstance.GetTriplanarMap(GroupID: Integer):TERRATexture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Groups[GroupID].Material.TriplanarMap;
End;

Procedure MeshInstance.SetAlphaMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.AlphaMap := Map;
End;

Procedure MeshInstance.SetToonRamp(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.ToonRamp := Map;
End;

Procedure MeshInstance.SetDecalMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.DecalMap := Map;
End;

Procedure MeshInstance.SetDiffuseMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.DiffuseMap := Map;
End;

Procedure MeshInstance.SetGlowMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.GlowMap := Map;
End;

Procedure MeshInstance.SetLightMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.LightMap := Map;
End;

Procedure MeshInstance.SetDitherPatternMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.DitherPatternMap := Map;
End;

Procedure MeshInstance.SetNormalMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.NormalMap := Map;
End;


Procedure MeshInstance.SetDisplacementMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.DisplacementMap := Map;
End;

Procedure MeshInstance.SetRefractionMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.RefractionMap := Map;
End;

Procedure MeshInstance.SetReflectiveMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.ReflectiveMap := Map;
End;

Procedure MeshInstance.SetEnviromentMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.EnviromentMap := Map;
End;

Procedure MeshInstance.SetFlowMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.FlowMap := Map;
End;

Procedure MeshInstance.SetNoiseMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.NoiseMap := Map;
End;

Procedure MeshInstance.SetSpecularMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.SpecularMap := Map;
End;

Procedure MeshInstance.SetTriplanarMap(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.TriplanarMap := Map;
End;

Procedure MeshInstance.SetColorTable(Map:TERRATexture);
Var
  I:Integer;
Begin
  If (_Mesh = Nil) Then
    Exit;
    
  For I:=0 To Pred(_Mesh.GroupCount) Do
    SetColorTable(I, Map);
End;

Procedure MeshInstance.SetColorTable(GroupID: Integer; Map:TERRATexture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Groups[GroupID].Material.ColorTable := Map;
End;

Function MeshInstance.GetAnimation: AnimationState;
Begin
  If (_Animation = Nil) And (Assigned(_Mesh)) Then
    _Animation := AnimationState.Create(_Mesh.Skeleton);

  Result := _Animation;

  If Result = Nil Then
    RaiseError('lol');
End;

Function MeshInstance.GetTransform: Matrix4x4;
Begin
  If (_NeedsTransformUpdate) Then
    UpdateTransform();

  Result := _Transform;
End;

Function MeshInstance.ActivatePhysics(Mass: Single): Boolean;
Begin
  If Assigned(_Body) Then
  Begin
    Result := False;
    Exit;
  End;

  _Body := PhysicsManager.Instance.CreateSphereRigidBody(Self._Scale.X, _Position, _Rotation, Mass);
  Result := True;
End;

Function MeshInstance.GetPosition: Vector3D;
Begin
  If (_NeedsPositionUpdate) Then
  Begin
    _Position := _Transform.GetTranslation();
    _NeedsPositionUpdate := False;
  End;

  Result := _Position;
End;

Function MeshInstance.GetRotation: Vector3D;
Begin
  If (_NeedsRotationUpdate) Then
  Begin
    _Rotation := _Transform.GetEulerAngles();
    _NeedsRotationUpdate := False;
  End;

  Result := _Rotation;
End;

Procedure MeshInstance.SetLightState(Index:Integer; Enabled:Boolean);
Begin
  If (Index<0) Or (Index>=_LightCount) Then
    Exit;

  _Lights[Index].Enabled := Enabled;
End;

{ MeshGroup }
Procedure MeshGroup.Release;
Begin
  Clean;
End;

Procedure MeshGroup.ReleaseBuffer;
Begin
  ReleaseObject(_Buffer);
End;

(*Procedure MeshGroup.SetupSkeleton;
Var
  M:Matrix4x4;
  Skel:MeshSkeleton;
  It:VertexIterator;
  V:MeshVertex;
Begin
  _NeedsSkeletonSetup := False;

  Skel := _Owner.Skeleton;

  If (Skel.BoneCount=0) Or (Not Self.Vertices.HasAttribute(vertexBone)) Then
    Exit;

  It := Self.Vertices.GetIterator(MeshVertex);
  While It.HasNext() Do
  Begin
    V := MeshVertex(It.Value);

    If (V.BoneIndex>0) And (V.BoneIndex<=Skel.BoneCount) Then
    Begin
      M := Skel.GetBone(Pred(V.BoneIndex)).AbsoluteMatrix;

      V.Position := M.Transform(V.Position);
      V.Normal := M.TransformNormal(V.Normal);
    End Else
      Log(logWarning, 'Mesh', 'Invalid bone index '+IntToString(It.Position)+ ' in mesh '+Self.Name);
  End;
  ReleaseObject(It);
End;*)

Procedure MeshGroup.DrawGeometry(State:MeshInstance; ShowWireframe:Boolean);
Var
  M:Matrix4x4;
  PositionHandle, UVHandle, UVHandle2, ColorHandle, NormalHandle:Integer;
  Graphics:GraphicsManager;
  It:VertexIterator;
  V:MeshVertex;
  Target:VertexData;
Begin
  If (_CullGeometry) And (_VisibleTriangleCount>=_TriangleCount) Then
  Begin
    _CullGeometry := False;
  End;

  If (Assigned(_Buffer)) And (Not _Buffer.IsValid()) Then
  Begin
    ReleaseObject(_Buffer);
  End;

  Graphics := GraphicsManager.Instance;

  If (Self._GPUSkinning) Then
  Begin
    If Not Assigned(_Buffer) Then
    Begin
      _Buffer := Graphics.Renderer.CreateVertexBuffer();

      If Assigned(_Buffer) Then
        _Buffer.Generate(_Vertices, _Triangles,  _Edges, _TriangleCount, (Self.Flags And meshGroupDynamic<>0));
    End;

    If (_CullGeometry) And (Assigned(_VisibleTriangles)) Then
      _Buffer.SetIndexList(_VisibleTriangles, _VisibleTriangleCount)
    Else
      _Buffer.SetIndexList(_Triangles, _TriangleCount);

    _Buffer.Draw(ShowWireframe);

    Exit;
  End;

  Target := Self.LockVertices();
  If (_Owner.Skeleton.BoneCount > 0 ) And (Assigned(State)) And (Target.HasAttribute(vertexBone)) Then
  Begin
    It := Target.GetIteratorForClass(MeshVertex);
    While It.HasNext() Do
    Begin
      V := MeshVertex(It.Value);

      If V.BoneIndex>0 Then
      Begin
        If (State.Animation = Nil) Or (State.Animation.Root = Nil) Then
          //M := _Owner.Skeleton.BindPose[V.BoneIndex]
          M := Matrix4x4Identity
        Else
          M := State.Animation.Transforms[V.BoneIndex];
                    
        V.Position := M.Transform(V.Position);
        V.Normal := M.TransformNormal(V.Normal);
      End;
    End;
    ReleaseObject(It);
  End;
  Self.UnlockVertices();

    Graphics.Renderer.SetVertexSource(Target);
    If (_CullGeometry) And (_VisibleTriangles<>Nil)  Then
    Begin
      Graphics.Renderer.DrawIndexedSource(renderTriangles, _VisibleTriangleCount*3, @_VisibleTriangles[0]);
    End Else
    Begin
      Graphics.Renderer.DrawIndexedSource(renderTriangles, _TriangleCount*3, @_Triangles[0]);
    End;

{$IFDEF PC}
    {If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
    Begin
      If (LightManager.Instance.Sun.Enabled) Then
      Begin
        glEnable(GL_LIGHT0);
        glEnable(GL_LIGHTING);
      End Else
      Begin
        glDisable(GL_LIGHTING);
      End;
    End;}
{$ENDIF}

{    Graphics.Renderer.SetSourceVertexSize(SizeOf(MeshVertex));
    Graphics.Renderer.SetAttributeSource(TERRA_POSITION_ATTRIBUTE, typeVector3D, @(TV[0].Position));
    Graphics.Renderer.SetAttributeSource(TERRA_NORMAL_ATTRIBUTE, typeVector3D, @(TV[0].Normal));
    Graphics.Renderer.SetAttributeSource(TERRA_UV0_ATTRIBUTE, typeVector2D, @(TV[0].TextureCoords));
    Graphics.Renderer.SetAttributeSource(TERRA_UV1_ATTRIBUTE, typeVector2D, @(TV[0].TextureCoords2));
    Graphics.Renderer.SetAttributeSource(TERRA_COLOR_ATTRIBUTE, typeColor, @(TV[0].Color));
    Graphics.Renderer.SetAttributeSource(TERRA_NORMAL_ATTRIBUTE, typeVector3D, @(TV[0].Normal));
    Graphics.Renderer.SetAttributeSource(TERRA_TANGENT_ATTRIBUTE, typeVector4D, @(TV[0].Tangent));}
End;

{Function MeshGroup.DuplicateVertex(Index:Integer):Integer;
Begin
  Result := VertexCount;

  Self.SetVertexCount(Result + 1);
  Self.Vertices[
  _Vertices[Result] := _Vertices[Index];
End;}

Function MeshGroup.AddVertex():Integer;
{Const
	Epsilon = 0.001;
Var
	I:Integer;}
Begin
{  If Not FastInsert Then
	For I:=0 To Pred(_VertexCount) Do
	If (_Vertices[I].Position.Distance(A.Position)<Epsilon) And (_Vertices[I].TextureCoords.Distance(A.TextureCoords)<Epsilon)
	And (_Vertices[I].Normal.Distance(A.Normal)<Epsilon)	Then
	Begin
		Result := I;
		Exit;
	End;}

	Result := _Vertices.Count;
  _Vertices.Resize(_Vertices.Count + 1);
  //_Vertices.GetVector3D(Result, vertexPosition)^ := A;
End;

Procedure MeshGroup.AddTriangle(Const A,B,C:Integer);
Begin
	Inc(_TriangleCount);
  _VisibleTriangleCount := _TriangleCount;
	SetLength(_Triangles, _TriangleCount);
	_Triangles[Pred(_TriangleCount)].Indices[0] := A; //AddVertex(A, FastInsert);
	_Triangles[Pred(_TriangleCount)].Indices[1] := B; //AddVertex(B, FastInsert);
	_Triangles[Pred(_TriangleCount)].Indices[2] := C; //AddVertex(C, FastInsert);
End;

Procedure MeshGroup.AddQuad(Const A,B,C,D:Integer);
Begin
	AddTriangle(A, B, C);
	AddTriangle(B, C, D);
End;

Procedure MeshGroup.AddVertexPin(ID:Word);
Begin
  Inc(_PinCount);
  SetLength(_Pins, _PinCount);
  _Pins[Pred(_PinCount)] := ID;
End;

Procedure MeshGroup.SetTriangle(Const T:Triangle; Index:Integer);
Begin
  If (Index>=0) And (Index<_TriangleCount) Then
    _Triangles[Index] := T;
End;

Function MeshGroup.GetTriangle(Index:Integer):Triangle;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If (Index>=0) And (Index<_TriangleCount) Then
    Result := _Triangles[Index]
   Else
   	FillChar(Result, SizeOf(Result), 0);
End;

Function MeshGroup.GetTrianglePointer(Index: Integer): PTriangle;
Begin
  If (Index>=0) And (Index<_TriangleCount) And (Index<Length(_Triangles)) Then
    Result := @_Triangles[Index]
  Else
    Result := Nil;
End;

Function MeshGroup.GetTriangleNormal(Index:Integer):Vector3D;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := _TriangleNormals[Index];
End;

Procedure MeshGroup.Clean;
Begin
	SetLength(_Triangles, 0);
	SetLength(_TriangleNormals, 0);
  SetLength(_VisibleTriangles, 0);
  SetLength(_Morphs, 0);
  SetLength(_Edges, 0);
  SetLength(_Pins, 0);
	_TriangleCount := 0;
  _VisibleTriangleCount := 0;
  _MorphCount := 0;
  _PinCount := 0;

  _NeedsTangentSetup := False;
  _Shader := Nil;


  _EmitterFX := '';
  _AlphaInspected := Nil;
  _CullGeometry := False;

  ReleaseObject(_Buffer);
	ReleaseObject(_Vertices);
  ReleaseObject(_ScratchVertices);

{$IFDEF PCs}
  ReleaseObject(_Fur);
  ReleaseObject(_Cloth);
{$ENDIF}
End;

Function MeshGroup.GetTriangles:PTriangleArray;
Begin
  If (_TriangleCount>0) Then
    Result := @(_Triangles[0])
  Else
    Result := Nil;
End;

Procedure MeshGroup.SetVertexCount(Count:Integer);
Begin
  Self.Vertices.Resize(Count);
End;

Procedure MeshGroup.SetTriangleCount(Count:Integer);
Begin
  _TriangleCount := Count;
  _VisibleTriangleCount := Count;
  SetLength(_Triangles, _TriangleCount);
End;

Procedure MeshGroup.Load(Source:Stream);
Var
	K:Cardinal;
  I, Size:Integer;
  S:TERRAString;
  Tex:TERRATexture;
  Tag:FileHeader;
  Handler:GroupDataBlockHandler;
Begin
  Source.ReadCardinal(Flags);

  //_Material.AmbientColor := ColorWhite;
  _Material.DiffuseColor := ColorWhite;
  _Material.BlendMode := -1;

  Repeat
    Source.Read(@Tag, 4);
    If (Tag=tagGroupEnd) Then
      Break;

    Source.ReadInteger(Size);
    Handler := GetMeshGroupHandler(Tag);
    Handler(Self, Size, Source);
  Until (Source.EOF);
End;

Procedure MeshGroup.Save(Dest:Stream);
Var
  I, J, Size:Integer;
  Name:TERRAString;
  Tag:FileHeader;
  Index, Handness:Shortint;
  ShouldStore:Boolean;
  PX,PY,PZ:SmallInt;
  SX,SY,SZ:Shortint;
  PU,PV:Byte;
  PositionRange:Vector3D;
  P:PVector3D;
//  UVRange:Vector2D;

  It:VertexIterator;

  Procedure WriteTexture(Tag:FileHeader; Tex:TERRATexture);
  Begin
    If Tex = Nil Then
      Exit;

    Size := Succ(Length(Tex.Name));
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    Dest.WriteString(Tex.Name);
  End;
Begin
  Self.UpdateBoundingBox();

  Dest.WriteCardinal(VertexFormatToFlags(Vertices.Format));
  Dest.WriteString(_Name);
  Dest.WriteCardinal(Flags);

  {$IFDEF CONSOLEOUTPUT}
{  WriteLn('Vertices: ', _VertexCount);
  WriteLn('Triangles: ', _TriangleCount, ' [Ofs: ',Dest.Position,']');}
  {$ENDIF}

  Tag := tagVertexData;
  Size := 4 * 2 + Self.Vertices.Size * Self.Vertices.Count;

  Dest.Write(@Tag, 4);
  Dest.WriteInteger(Size);
  Self.Vertices.Write(Dest);

  Tag := tagTriangleIndices;
  Size := 4 + _TriangleCount * SizeOf(Triangle);
  Dest.Write(@Tag, 4);
  Dest.WriteInteger(Size);
  Dest.Write(@_TriangleCount, 4);
  For I:=0 To Pred(_TriangleCount) Do
    Dest.Write(@_Triangles[I], SizeOf(Triangle));

  ShouldStore := Length(_TriangleNormals) = _TriangleCount;
  If ShouldStore Then
  Begin
    Tag := tagTriangleNormals;
    Size := _TriangleCount * SizeOf(Vector3D);
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    For I:=0 To Pred(_TriangleCount) Do
      Dest.Write(@_TriangleNormals[I], SizeOf(Vector3D));
  End;

  ShouldStore := Length(_Edges) = _TriangleCount;
  If ShouldStore Then
  Begin
    Tag := tagTriangleEdges;
    Size := _TriangleCount * SizeOf(TriangleEdgesState);
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    For I:=0 To Pred(_TriangleCount) Do
      Dest.Write(@_Edges[I], SizeOf(TriangleEdgesState));
  End;
  
  If Assigned(_Material.DiffuseMap) Then
    Name := _Material.DiffuseMap.Name
  Else
    Name := '';
  Tag := tagMaterialDiffuse;
  Size := SizeOf(Color) + Succ(Length(Name));
  Dest.Write(@Tag, 4);
  Dest.WriteInteger(Size);
  Dest.Write(@_Material.DiffuseColor, SizeOf(Color));
  Dest.WriteString(Name);

  If (Self.BlendMode>=0) Then
  Begin
    Tag := tagMaterialBlendMode;
    Size := 1;
    PU := Byte(_Material.BlendMode);
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    Dest.Write(@PU, 1);
  End;

  If (Self.EmitterFX<>'') Then
  Begin
    Tag := tagMaterialParticles;
    Size := Succ(Length(_EmitterFX));
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    Dest.WriteString(EmitterFX);
  End;

  WriteTexture(tagMaterialTriplanar, _Material.TriplanarMap);
  WriteTexture(tagMaterialSpecular, _Material.SpecularMap);
  WriteTexture(tagMaterialBump, _Material.NormalMap);
  WriteTexture(tagMaterialDisplacement, _Material.DisplacementMap);
  WriteTexture(tagMaterialLightmap, _Material.LightMap);
  WriteTexture(tagMaterialRefraction, _Material.RefractionMap);
  WriteTexture(tagMaterialReflective, _Material.ReflectiveMap);
  WriteTexture(tagMaterialGlow, _Material.GlowMap);
  WriteTexture(tagMaterialAlphaMap, _Material.AlphaMap);
  WriteTexture(tagMaterialEnvMap, _Material.EnviromentMap);
  WriteTexture(tagMaterialRamp, _Material.ToonRamp);

  // morphs
  For J:=0 To Pred(_MorphCount) Do
  Begin
    Tag := tagVertexMorph;
    Size := Self.VertexCount * 4 + 5;
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);

    Dest.Write(@_Morphs[J].ID, 4);
    Dest.Write(@_Morphs[J].MorphType, 1);

    For I:=0 To Pred(Self.VertexCount) Do
      Dest.Write(@_Morphs[J].Values[I], SizeOf(Vector3D));
  End;

  {$IFDEF PC}
{  If (FurSettings.Pattern<>'') Then
  Begin
    Dest.BeginChunk(mcFurData);
    Dest.Stream.Write(GroupID, 2);
    Dest.Stream.WriteString(FurSettings.Pattern);
    Dest.Stream.Write(FurSettings.Thickness, 4);
    Dest.Stream.Write(FurSettings.Waviness, 4);
    Dest.Stream.Write(FurSettings.Length, 4);
    Dest.Stream.Write(FurSettings.Density, 4);
    Dest.EndChunk;
  End;

  If (_PinCount>0) Then
  Begin
    Dest.BeginChunk(mcClothData);
    Dest.Stream.Write(GroupID, 2);
    Dest.Stream.Write(_PinCount, 4);
    Dest.Stream.Write(_Pins[0], _PinCount*2);
    Dest.EndChunk;
  End;}
  {$ENDIF}

  Tag := tagGroupEnd;
  Dest.Write(@Tag, 4);
End;

Procedure MeshGroup.CalculateTriangleNormals;
Var
  I:Integer;
  A, B, C:Vector3D;
Begin
  SetLength(_TriangleNormals, _TriangleCount);
	For I:=0 To Pred(_TriangleCount) Do
  Begin
    _Vertices.GetVector3D(_Triangles[I].Indices[0], vertexPosition, A);
    _Vertices.GetVector3D(_Triangles[I].Indices[1], vertexPosition, B);
    _Vertices.GetVector3D(_Triangles[I].Indices[2], vertexPosition, C);

    _TriangleNormals[I] := TriangleNormal(A, B, C);
  End;
End;

Function MeshGroup.CalculateDitherScale():Single;
Var
  I:Integer;
  MinU, MinV, MaxU, MaxV:Single;
  UV:Vector2D;
Begin
  MinU := 9999;
  MinV := 9999;
  MaxU := -9999;
  MaxV := -9999;

  Result := 10;

	For I := 0 To Pred(Self.VertexCount) Do
	Begin
		Self.Vertices.GetVector2D(I, vertexUV0, UV);

    MinU := FloatMin(MinU, UV.X);
    MaxU := FloatMax(MaxU, UV.X);

    MinV := FloatMin(MinV, UV.Y);
    MaxV := FloatMax(MaxV, UV.Y);
	End;

  Result := FloatMax((MaxU - MinU), (MaxV - MinV));

  Result := 10.0 + (1.0 - Result) * 30;

//  Result := Result * 0.1;
End;


(*
Procedure MeshGroup.CalculateAdjacency();
Var
  I,J,K, N, A, B, AdjIndex:Integer;
  UnpairedCount:Integer;
  Tri:Triangle;
  UnpairedEdges:Array Of Integer;

  Function FindAdjancentEdge(TargetEdgeIndex:Integer):Integer;
  Var
    I, N, TargetVertexIndex:Integer;
  Begin
    TargetVertexIndex := _HalfEdges[TargetEdgeIndex].StartVertex;
    // search only in unpaired half edges
    For I:=0 To Pred(UnpairedCount) Do
    Begin
      N := UnpairedEdges[I];
      If (_HalfEdges[N].EndVertex = TargetVertexIndex) Then
      Begin
        Result := I;
        Exit;
      End;
    End;

    Result := -1;
  End;

Begin
  _HalfEdgeCount := _TriangleCount * 3;
  SetLength(_HalfEdges, _HalfEdgeCount);
  SetLength(UnpairedEdges, _HalfEdgeCount);

  I:=0;
  While I<_TriangleCount Do
  Begin
    Tri := Self.GetTriangle(I);

    For J:=0 To 2 Do
    Begin
      N := I*3 + J;
      _HalfEdges[N].FaceIndex := I;

      _HalfEdges[N].EndVertex := Tri.Indices[J];
      _HalfEdges[N].StartVertex := Tri.Indices[(J+2) Mod 3];

      _HalfEdges[N].NextEdge := I*3 + ((J+1) Mod 3);
      _HalfEdges[N].PairEdge := -1;
      UnpairedEdges[N] := N;
    End;

    Inc(I);
  End;

  // now we pair the edges
  UnpairedCount := _HalfEdgeCount;
  While UnpairedCount>0 Do
  Begin
    A := UnpairedEdges[0];
    AdjIndex := FindAdjancentEdge(A);

    If AdjIndex>=0 Then
    Begin
      B := UnpairedEdges[AdjIndex];

      _HalfEdges[A].PairEdge := B;
      _HalfEdges[B].PairEdge := A;

      // now remove B from paired edge list
      UnpairedEdges[AdjIndex] := UnpairedEdges[Pred(UnpairedCount)];
      Dec(UnpairedCount);
    End;
    //RaiseError(Self._Owner.Name+ ' has invalid topology in group '+IntToString(Self._ID));


    // now remove A from paired edge list
    UnpairedEdges[0] := UnpairedEdges[Pred(UnpairedCount)];
    Dec(UnpairedCount);
  End;

  SetLength(UnpairedEdges, 0);
End;*)

Procedure MeshGroup.CalculateTangents;
Var
  I:Integer;
  I1, I2, I3:Integer;
  V1, V2, V3: Vector3D;
  W1, W2, W3: Vector2D;
  Tan1, Tan2:Array Of Vector3D;
  X1, X2, Y1, Y2, Z1, Z2: Single;
  S1, S2, T1, T2, R: Single;
  SDir, TDir, N ,T:Vector3D;
  Handness:Single;
  UVSet:Integer;
Begin
  _NeedsTangentSetup := False;

  SetLength(Tan1, Self.VertexCount);
  SetLength(Tan2, Self.VertexCount);

	For I := 0 To Pred(Self.VertexCount) Do
	Begin
		tan1[i] := VectorZero;
		tan2[i] := VectorZero;
	End;

  If Self._Vertices.HasAttribute(vertexUV1) Then
    UVSet := vertexUV1
  Else
    UVSet := vertexUV0;

  For I := 0 To Pred(_TriangleCount) Do
  Begin
    i1 := _Triangles[i].Indices[0];
		i2 := _Triangles[i].Indices[1];
		i3 := _Triangles[i].Indices[2];

    _Vertices.GetVector3D(i1, vertexPosition, V1);
		_Vertices.GetVector3D(i2, vertexPosition, V2);
		_Vertices.GetVector3D(i3, vertexPosition, V3);

    _Vertices.GetVector2D(i1, UVSet, W1);
		_Vertices.GetVector2D(i2, UVSet, W2);
		_Vertices.GetVector2D(i3, UVSet, W3);

    x1 := v2.x - v1.x;
		x2 := v3.x - v1.x;
		y1 := v2.y - v1.y;
		y2 := v3.y - v1.y;
		z1 := v2.z - v1.z;
		z2 := v3.z - v1.z;

		s1 := w2.X - w1.X;
		s2 := w3.X - w1.X;
		t1 := w2.Y - w1.Y;
		t2 := w3.Y - w1.Y;

    R := (s1 * t2 - s2 * t1);
    If (R <= 0.0) Then
      R := 0.001;

    R := 1.0 / R;

		sdir := VectorCreate(	(t2 * x1 - t1 * x2) * r, (t2 * y1 - t1 * y2) * r, (t2 * z1 - t1 * z2) * r);
    tdir := VectorCreate(	(s1 * x2 - s2 * x1) * r, (s1 * y2 - s2 * y1) * r, (s1 * z2 - s2 * z1) * r);

    tan1[i1] := VectorAdd(tan1[i1], sdir);
		tan1[i2] := VectorAdd(tan1[i2], sdir);
		tan1[i3] := VectorAdd(tan1[i3], sdir);

    tan2[i1] := VectorAdd(tan2[i1], tdir);
		tan2[i2] := VectorAdd(tan2[i2], tdir);
		tan2[i3] := VectorAdd(tan2[i3], tdir);
  End;

  For I := 0 To Pred(Self.VertexCount) Do
  Begin
    _Vertices.GetVector3D(I, vertexNormal, N);
    t := tan1[i];
    T.Normalize();

    // Gram-Schmidt orthogonalize
		T := VectorSubtract(t, VectorScale(n, VectorDot(n, t)));
    T.Normalize();


{    If (IsNan(T.X)) Or (IsNan(T.Y)) Or (IsNan(T.Z)) Then
      FloatToString(T.X);}

    // Calculate handedness
    If (VectorDot( VectorCross(n, t), tan2[i]) < 0.0) Then
      Handness := 1.0
    Else
      Handness := -1.0;

    _Vertices.SetVector4D(I, vertexTangent, VectorCreate4D(T.X, T.Y, T.Z, Handness));
  End;
End;

(*Procedure MeshGroup.BuildBillboards;
Var
  I, N:Integer;
  Used:Array Of Boolean;
Begin
  If (Self._Flags And RENDER_BILLBOARD))
			continue;

	SetLength(Used, TriangleCount);
	For I := 0 To Pred(TriangleCount) Do
		Used[i] := False;

	for (unsigned int n=0; n<this->_groupList.size(); n++)
	{
		if (!(_groupList[n]._renderFlags & RENDER_BILLBOARD))
			continue;

		for (unsigned int i=0; i<_groupList[n]._triangleCount; i++)
		{
			if (_used[_groupList[n]._triangleIndices[i]])
				continue;

			Single minX, maxX;
			Single minY, maxY;
			Single minZ, maxZ;

			minX = 9999.0f;
			maxX = -9999.0f;
			minY = 9999.0f;
			maxY = -9999.0f;
			minZ = 9999.0f;
			maxZ = -9999.0f;

			_used[_groupList[n]._triangleIndices[i]] = true;
			int index = _groupList[n]._triangleIndices[i];
			for (unsigned int j=0; j<3; j++)
			{
				Vector v = _vertexList[_triangleList[index]._vertexIndices[j]].position;
				if (v.x<minX) minX = v.x;
				if (v.x>maxX) maxX = v.x;
				if (v.y<minY) minY = v.y;
				if (v.y>maxY) maxY = v.y;
				if (v.z<minZ) minZ = v.z;
				if (v.z>maxZ) maxZ = v.z;
			}
			for (unsigned int k=0; k<_triangleList[index]._adjacencyList.size(); k++)
			{
				for (int j=0; j<3; j++)
				{
					Vector v = _vertexList[_triangleList[_triangleList[index]._adjacencyList[k]]._vertexIndices[j]].position;
					if (v.x<minX) minX = v.x;
					if (v.x>maxX) maxX = v.x;
					if (v.y<minY) minY = v.y;
					if (v.y>maxY) maxY = v.y;
					if (v.z<minZ) minZ = v.z;
					if (v.z>maxZ) maxZ = v.z;
				}
			}
			Single sX = (maxX - minX);
			Single sY = (maxY - minY);
			Single sZ = (maxZ - minZ);
			Single cX = minX + sX * 0.5f;
			Single cY = minY + sY * 0.5f;
			Single cZ = minZ + sZ * 0.5f;
			sX = sX * 1.5f;
			sY = sY * 1.5f;

			if (sX<=0 || sY<=0)
				return;

			for (unsigned int j=0; j<3; j++)
			{
				Vertex *v = &(_vertexList[_triangleList[index]._vertexIndices[j]]);
				Single k;
				if (v->position.x<cX)
				{
					if (v->position.y<cY)
						k = 0.0;
					else
						k = 1.0;
				}
				else
				{
					if (v->position.y<=cY)
						k = 2.0;
					else
						k = 3.0;
				}
				v->tangent.x = sX;
				v->tangent.y = sY;
				v->tangent.z = k;
				v->position.x = cX;
				v->position.y = cY;
				v->position.z = cZ;
			}
			for (unsigned int k=0; k<_triangleList[index]._adjacencyList.size(); k++)
			{
				if (_used[_triangleList[index]._adjacencyList[k]])
					continue;
				_used[_triangleList[index]._adjacencyList[k]] = true;
				for (int j=0; j<3; j++)
				{
					Vertex *v = &(_vertexList[_triangleList[_triangleList[index]._adjacencyList[k]]._vertexIndices[j]]);
					Single k;
					if (v->position.x<cX)
					{
						if (v->position.y<cY)
							k = 0.0;
						else
							k = 1.0;
					}
					else
					{
						if (v->position.y<cY)
							k = 2.0;
						else
							k = 3.0;
					}
					v->tangent.x = sX;
					v->tangent.y = sY;
					v->tangent.z = k;
					v->position.x = cX;
					v->position.y = cY;
					v->position.z = cZ;
				}
			}
		}
	}
} *)

Procedure MeshGroup.Init;
Begin
  //UpdateBoundingBox;
End;

Procedure MeshGroup.UpdateBoundingBox;
Var
  I,N:Integer;
  P:Vector3D;
  It:VertexIterator;
  M:Matrix4x4;
  V:MeshVertex;
Begin
  If (_Vertices<>Nil) And (_Vertices.Count>0) Then
  Begin
    _BoundingBox.Reset();

    {For I:=0 To Pred(_VertexCount) Do
    Begin
      If (_Owner.Skeleton.BoneCount > 0 ) And (Length(_Owner.Skeleton.BindPose)>0) Then
      Begin
        N := Trunc(Vertices[I].BoneIndex);
        M := _Owner.Skeleton.BindPose[N];
        P := M.Transform(_Vertices[I].Position);
      End Else
        P := _Vertices[I].Position;}


    It := _Vertices.GetIteratorForClass(MeshVertex);
    While It.HasNext() Do
    Begin
      V := MeshVertex(It.Value);
      _BoundingBox.StartVertex.x := FloatMin(_BoundingBox.StartVertex.x, V.Position.X);
      _BoundingBox.StartVertex.y := FloatMin(_BoundingBox.StartVertex.y, V.Position.Y);
      _BoundingBox.StartVertex.z := FloatMin(_BoundingBox.StartVertex.z, V.Position.Z);
      _BoundingBox.EndVertex.x := FloatMax(_BoundingBox.EndVertex.x, V.Position.X);
      _BoundingBox.EndVertex.y := FloatMax(_BoundingBox.EndVertex.y, V.Position.Y);
      _BoundingBox.EndVertex.z := FloatMax(_BoundingBox.EndVertex.z, V.Position.Z);
    End;
    ReleaseObject(It);
  End;
End;

Procedure MeshGroup.Transform(const TargetTransform: Matrix4x4);
Var
  It:VertexIterator;
  V:MeshVertex;
Begin
  It := _Vertices.GetIteratorForClass(MeshVertex);
  While It.HasNext() Do
  Begin
    V := MeshVertex(It.Value);
    V.Position := TargetTransform.Transform(V.Position)
  End;
  ReleaseObject(It);
End;

Function MeshGroup.Intersect(const R: Ray; var T: Single; Const Transform:Matrix4x4): Boolean;
Var
  I:Integer;
  V0,V1,V2:Vector3D;
  U,V:Single;
Begin
  For I:=0 To Pred(_TriangleCount) Do
  Begin
    _Vertices.GetVector3D(_Triangles[I].Indices[0], vertexPosition, V0);
    _Vertices.GetVector3D(_Triangles[I].Indices[1], vertexPosition, V1);
    _Vertices.GetVector3D(_Triangles[I].Indices[2], vertexPosition, V2);

    V0 := Transform.Transform(V0);
    V1 := Transform.Transform(V1);
    V2 := Transform.Transform(V2);

    U := 0;
    V := 0;
    T := 999999;

    If (R.TriangleIntersect(V0, V1, V2, T, U,V)) Then
    Begin
      Result := True;
      Exit;
    End;
  End;
  Result := False;
End;


Procedure MeshGroup.SetupUniforms(View:TERRAViewport; Transform:Matrix4x4; State:MeshInstance; Outline, TranslucentPass:Boolean; Const Material:MeshMaterial);
Var
  I:Integer;
  TextureMatrix, M, M2:Matrix4x4;
  C:Color;
  BoneVectorLocation:Integer;
  BoneVectors:Array[0..(Succ(MaxBones)*3)] Of Vector4D;
  M2D:Matrix3x3;
  Bend,Delta:Single;
  Graphics:GraphicsManager;

  Procedure EncodeBoneMatrix(ID:Integer; Mat:Matrix4x4);
  Var
    B1,B2,B3:Vector4D;
  Begin
    //Mat := MatrixTranspose(Mat);
    B1.X := Mat.Get(0, 0);
    B1.Y := Mat.Get(0, 1);
    B1.Z := Mat.Get(0, 2);
    B1.W := Mat.Get(0, 3);

    B2.X := Mat.Get(1, 0);
    B2.Y := Mat.Get(1, 1);
    B2.Z := Mat.Get(1, 2);
    B2.W := Mat.Get(1, 3);

    B3.X := Mat.Get(2, 0);
    B3.Y := Mat.Get(2, 1);
    B3.Z := Mat.Get(2, 2);
    B3.W := Mat.Get(2, 3);

    BoneVectors[ID*3 + 0] := B1;
    BoneVectors[ID*3 + 1] := B2;
    BoneVectors[ID*3 + 2] := B3;
  End;
Begin
  Graphics := GraphicsManager.Instance;
                     
  If (Graphics.ReflectionActive) Then
    Transform := Matrix4x4Multiply4x4(GraphicsManager.Instance.ReflectionMatrix, Transform);

  If (Graphics.ReflectionActive) Then
    Transform := Matrix4x4Multiply4x4(Graphics.ReflectionMatrix, Transform);

  If Assigned(State) Then
    TextureMatrix := State._Groups[Self._ID].TextureTransform
  Else
    TextureMatrix := Matrix4x4Identity;

  {$IFDEF PC}
  (*If (Not GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable) Then
  Begin
    glMatrixMode(GL_TEXTURE);
    glLoadMatrixf(@TextureMatrix);
    M := GraphicsManager.Instance.ActiveViewport.Camera.Projection;
    M2 := GraphicsManager.Instance.ActiveViewport.Camera.Transform;
    M := Matrix4x4Multiply4x4(M, M2);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@M);
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@Transform);
    Exit;
  End; BIBI*)
  {$ENDIF}

  Graphics.Renderer.SetModelMatrix(Transform);
  Graphics.Renderer.SetTextureMatrix(TextureMatrix);

  View.Camera.SetupUniforms;

{$IFDEF ADVANCED_ALPHA_BLEND}
  If Not TranslucentPass Then
  Begin
    _Shader.SetColorUniform('targetColor', ColorNull);
  End Else
{$ENDIF}
  {$IFDEF REFLECTIONS_WITH_STENCIL}
  If (Graphics.ReflectionStencil) Then
  Begin
    _Shader.SetColorUniform('targetColor', ColorWhite);
  End Else
  {$ENDIF}
  If (Graphics.RenderStage = renderStageOutline) Then
  Begin
    _Shader.SetColorUniform('targetColor', Material.OutlineColor);
  End Else
  If (Outline) Then
  Begin
    _Shader.SetFloatUniform('outlineScale', 0.4);
    _Shader.SetColorUniform('outlineColor', Material.OutlineColor);
  End;

  If (Self.Flags And meshGroupVegetation<>0) And (Material.VegetationBend>0)  Then
  Begin
    Delta := Application.GetTime() + (Cardinal(Self) Mod 35) * 500;
    Delta := Delta/1000;

    _Shader.SetFloatUniform('vegetationBase', _BoundingBox.StartVertex.Y);
    _Shader.SetFloatUniform('vegetationSize', _BoundingBox.EndVertex.Y - _BoundingBox.StartVertex.Y);
    _Shader.SetFloatUniform('vegetationTime', Delta);
    _Shader.SetFloatUniform('vegetationBend', Material.VegetationBend);
  End;

  If (Material.AmbientColor.A>0) Then
  Begin
    _Shader.SetColorUniform('ambient_color', Material.AmbientColor);
  End;


  If (Graphics.RenderStage = renderStageDiffuse) And (Graphics.Renderer.Settings.CartoonHues.Enabled) Then
  Begin
    _Shader.SetColorUniform('hue_green', Graphics.Renderer.Settings.CartoonHueGreen);
    _Shader.SetColorUniform('hue_yellow', Graphics.Renderer.Settings.CartoonHueYellow);
    _Shader.SetColorUniform('hue_purple', Graphics.Renderer.Settings.CartoonHuePurple);
    _Shader.SetColorUniform('hue_black', Graphics.Renderer.Settings.CartoonHueBlack);

    (*
    _Shader.SetColorUniform('hue_low', ColorCreateFromFloat(304/360, 128, 128));
    _Shader.SetColorUniform('hue_high', ColorCreateFromFloat(48/360, 128, 128));
      *)
  End;

  If (_Owner.Skeleton.BoneCount > 0 ) And (Assigned(State)) And (Self.Vertices.HasAttribute(vertexBone)) Then
  Begin
    EncodeBoneMatrix(0, Matrix4x4Identity);

    If (_Owner.Skeleton.BoneCount>MaxBones) Then
    Begin
      Log(logWarning, 'Mesh', 'Bone limit reached, '+IntToString(_Owner.Skeleton.BoneCount)+' bones'
        + ', mesh name = "' + _Owner.Name + '"');
      Exit;
    End;

    For I:=1 To _Owner.Skeleton.BoneCount Do
    Begin
      If (State.Animation = Nil) Or (State.Animation.Root = Nil) Then
        //M := _Owner.Skeleton.BindPose[I]
        M := Matrix4x4Identity
      Else
        M := State.Animation.Transforms[I];

        EncodeBoneMatrix(I, M);
    End;

    Graphics.Renderer.ActiveShader.SetVec4ArrayUniform('boneVectors', Succ(_Owner.Skeleton.BoneCount)*3, @(BoneVectors[0]));
  End;
End;

Function MeshGroup.Render(View:TERRAViewport; Const Transform:Matrix4x4; TranslucentPass:Boolean; State:MeshInstance):Boolean;
Var
  UseOutline, ShowWireframe, UseTextureMatrix:Boolean;
  I,J,K, PassCount:Integer;
  Tex:TERRATexture;
  Transparency:Boolean;
  SM:Single;
  Slot, VolSlot:Integer;
  M:Matrix4x4;

  DestMaterial:MeshMaterial;

  Hidden:Boolean;
  AlwaysOnTop:Boolean;

  Graphics:GraphicsManager;
Begin
  Result := False;

  Graphics := GraphicsManager.Instance;

  If (Graphics.RenderStage = renderStageOutline) {$IFNDEF DISABLEOUTLINES} And (Not Graphics.Renderer.Settings.Outlines.Enabled) {$ENDIF} Then
  Begin
    IntToString(2);
    Exit;
  End;


  {If (Flags And meshGroupDepthOff<>0)  And (Graphics.ReflectionActive) Then
    Exit;}

  {$IFDEF REFLECTIONS_WITH_STENCIL}
  If (Graphics.ReflectionStencil) And (Self.Flags And meshGroupReflective=0) Then
    Exit;
  {$ENDIF}

  If (Graphics.RenderStage = renderStageRefraction) And (_Material.RefractionMap=Nil) Then
    Exit;

  {If (Self.Flags And meshGroupColorOff<>0) And (Graphics.RenderStage <> renderStageDiffuse) Then
    Exit;}

  If (Self.Flags And meshGroupShadowOnly<>0) And (Graphics.RenderStage <> renderStageShadow) Then
    Exit;

  If (Self.Flags And meshGroupNormalsOff<>0) And (Graphics.RenderStage = renderStageNormal)  Then
    Exit;

  If (Self.Flags And meshGroupOutlineOff<>0) And (Graphics.RenderStage = renderStageOutline) Then
    Exit;

  {If (Self.Name = 'underwater' ) Then
    IntToString(2);}

  {If (Graphics.RenderStage = renderStageGlow)
  And (Self._GlowMap=Nil) Then
    Exit;}

  If Assigned(State) Then
    Hidden := Not State.GetVisibility(Self._ID)
  Else
    Hidden := (Flags And meshGroupHidden<>0);

  ShowWireframe := (Flags And meshGroupWireframe<>0);
  If (Not ShowWireframe) And (Assigned(State)) Then
    ShowWireframe := State._Groups[Self.ID].Wireframe;

  If (_Vertices = Nil) Or (_Vertices.Count<=0) Or (Hidden) Then
    Exit;

  If Assigned(State) Then
  Begin
    Self.InheritMaterial(State._Groups[Self.ID].Material, DestMaterial);
    DestMaterial.DiffuseColor := ColorMultiply(DestMaterial.DiffuseColor, State.Diffuse);

    If (Self.Flags And meshGroupReflective<>0) Then
      DestMaterial.ReflectionMap := TextureManager.Instance.WhiteTexture;
  End Else
    DestMaterial := _Material;

  If (DestMaterial.DiffuseColor.A<=0) Then
    Exit;

  If (_NeedsTangentSetup) Then
    Self.CalculateTangents();

  AlwaysOnTop := False;

  If (Assigned(State)) Then
    AlwaysOnTop := State.AlwaysOnTop;

  If Graphics.ReflectionActive Then
    AlwaysOnTop := True;

  (*If (Self.Flags And meshGroupOverrideAmbient<>0) Then
    AmbientColor := Self.AmbientColor
  Else
    AmbientColor := LightManager.Instance.AmbientColor;*)

{$IFDEF PC}
{  If (Assigned(_Cloth)) Then
  Begin
 	  _Cloth.AddForce(VectorScale(VectorCreate(0, -0.005,0), TIME_STEPSIZE2)); // add gravity each frame, pointing down
	  _Cloth.WindForce(VectorScale(Graphics.WindVector,TIME_STEPSIZE2)); // generate some wind each frame

    If Assigned(Instance) Then
      _Cloth.Render(Instance);
    Result := True;
    Exit;
  End;
}
{$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'MeshGroup', 'Testing rendering passes');
  {$ENDIF}

{$IFNDEF DISABLEOUTLINES}
  If (Graphics.RenderStage = renderStageDiffuse)
  And (DestMaterial.OutlineColor.A>0) And (Self.Flags And meshGroupOutlineOff=0)
  And (Graphics.Renderer.Settings.Outlines.Enabled)
  And (Not Graphics.ReflectionActive)
  {$IFDEF POSTPROCESSING}
  And (Not View.IsRenderTargetEnabled(captureTargetNormal))
  {$ENDIF}
  Then
    PassCount := 2
  Else
{$ENDIF}
    PassCount := 1;
    
   UseOutline := False;

  For K:=1 To PassCount Do
  Begin
    UseOutline := K = 2;
    Slot := 0;

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'MeshGroup', 'Selecting shader');
  {$ENDIF}

  If (Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    If Assigned(State) And (State.CustomShader<>Nil) Then
      _Shader := State.CustomShader
    Else
    Begin
      UseTextureMatrix := (Assigned(State)) And (State._Groups[_ID].UseTextureMatrix);
      Self._Shader := SelectMeshShader(View, Self, Transform.GetTranslation(), UseOutline, TranslucentPass, DestMaterial, UseTextureMatrix);
    End;

    {If (Assigned(_Shader)) And (Not _Shader.IsReady) Or (_Shader = Nil) Then
      Exit; BIBI}

    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'MeshGroup', 'Binding shader');
    {$ENDIF}

    Graphics.Renderer.BindShader(_Shader);
  End Else
    _Shader := Nil;

  If (Graphics.RenderStage = renderStageGlow) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing glow');{$ENDIF}

      If Assigned(DestMaterial.GlowMap) Then
        DestMaterial.GlowMap.Bind(0)
      Else
        TextureManager.Instance.BlackTexture.Bind(0);

      If Assigned(_Shader) Then
        _Shader.SetIntegerUniform('glowMap', 0);
    End Else
    If (Graphics.RenderStage = renderStageReflection) Then
    Begin
      Slot := 0;

      If (DestMaterial.FlowMap<>Nil) Then
        IntToString(2);

      BindMaterial(View, Slot, DestMaterial);

      If Assigned(_Shader) Then
      Begin
        _Shader.SetFloatUniform('reflectionFactor', 0.25);
      End;
    End Else
    Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing skin');  {$ENDIF}

      Tex := DestMaterial.DiffuseMap;

      If Not Assigned(Tex) Then
        Exit;

      If (Assigned(Tex)) And (Tex.Status<>rsReady) Then
      Begin
        Tex.IsReady();
        Exit;
      End;

      If _AlphaInspected = Nil Then
      Begin
        Self.InspectAlpha(Tex);
      End;

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setting texture wrap mode');  {$ENDIF}
      Tex.WrapMode := wrapAll;

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Binding texture');  {$ENDIF}
      //glDisable(GL_TEXTURE_2D);
      Tex.Bind(0);

      If Assigned(_Shader) Then
      Begin
        _Shader.SetIntegerUniform('diffuseMap', 0);
        If Not UseOutline Then
          BindMaterial(View, Slot, DestMaterial);
      End{ Else
      Begin
        SetCombineWithColor(DestMaterial.DiffuseColor);
      End BIBI};

    End;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setuping uniforms'); {$ENDIF}
    LightManager.Instance.SetupUniforms(@_LightBatch, Slot);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setting uniform properties');  {$ENDIF}
    If Assigned(_Shader) Then
    Begin
      If (Graphics.RenderStage = renderStageShadow)  Then
        _Shader.SetColorUniform('diffuse_color', DestMaterial.ShadowColor)
      Else
        _Shader.SetColorUniform('diffuse_color', DestMaterial.DiffuseColor); // BIBI
     // _Shader.SetFloatUniform('specular_power', DestMaterial.SpecularPower);
      _Shader.SetColorUniform('specular_color', ColorWhite); // BIBI
    End;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setup mesh uniforms');  {$ENDIF}
    SetupUniforms(View, Transform, State, UseOutline, TranslucentPass, DestMaterial);

    {$IFDEF EDITOR}
    If (Flags And mgCullFace<>0) Then
      Flags := Flags Xor mgCullFace; // disable culling in edit mode
    {$ENDIF}

    If (UseOutline) Then
    Begin
      Graphics.Renderer.SetCullMode(cullFront);
      Graphics.Renderer.SetBlendMode(blendNone);
    End Else
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setting blending mode');  {$ENDIF}

      {$IFDEF ADVANCED_ALPHA_BLEND}
      Graphics.Renderer.SetBlendMode(blendNone);      
      {$ELSE}

      {$IFNDEF REFLECTIONS_WITH_STENCIL}
      If (Graphics.ReflectionActive) Then
        Graphics.Renderer.SetBlendMode(blendBlend)
      Else
      {$ENDIF}
      If (Graphics.RenderStage = renderStageDiffuse) {Or (Graphics.RenderStage = renderStageOutline)} Then
        Graphics.Renderer.SetBlendMode({DestMaterial.BlendMode}blendBlend)
      Else
      If (Graphics.RenderStage = renderStageShadow)  Then
        Graphics.Renderer.SetBlendMode(blendBlend)
      Else
        Graphics.Renderer.SetBlendMode(blendNone);
      {$ENDIF}

      {$IFDEF REFLECTIONS_WITH_STENCIL}
      If (Graphics.ReflectionStencil) Then
      Begin
        Graphics.SetBlendMode(blendNone);
      End Else
      If Graphics.ReflectionActive Then
      Begin
        Graphics.SetBlendMode(blendReflection);
      End;
      {$ENDIF}


      If (Graphics.RenderStage<>renderStageShadow) And (Flags And meshGroupDoubleSided<>0) And (Not Graphics.ReflectionActive) Then
        Graphics.Renderer.SetCullMode(cullNone)
      Else
        Graphics.Renderer.SetCullMode(cullBack);
    End;

    If (Flags And meshGroupDepthOff<>0) Or (UseOutline) Then
      Graphics.Renderer.SetDepthMask(False);

    {If (Flags And meshGroupSkybox<>0) Then
      glDepthRange(0.9999,1); BIBI}

    If AlwaysOnTop Then
      Graphics.Renderer.SetDepthTest(False);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Drawing geometry');  {$ENDIF}
    Self.DrawGeometry(State, ShowWireframe);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Undoing properties');  {$ENDIF}
    If (UseOutline) Or ((Graphics.RenderStage<>renderStageShadow) And (Flags And meshGroupDoubleSided<>0) And (Not Graphics.ReflectionActive)) Then
      Graphics.Renderer.SetCullMode(cullBack);

    If (Flags And meshGroupDepthOff<>0) Or (UseOutline) Then
      Graphics.Renderer.SetDepthMask(True);

    If AlwaysOnTop Then
      Graphics.Renderer.SetDepthTest(True);

    {If (Flags And meshGroupSkybox<>0) Then
      glDepthRange(0.0,1); BIBI}

    (*If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
    Begin
      glActiveTexture(GL_TEXTURE1);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      glDisable(GL_TEXTURE_2D);

      glActiveTexture(GL_TEXTURE0);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    End;
    BIBI
    *)

  Result := True;
End;

Procedure MeshGroup.SetTriplanarMap(const Value:TERRATexture);
Var
  N:Integer;
Begin
  _Material.TriplanarMap := Value;
  If ((Self.Flags And meshGroupTriplanar=0) = Assigned(Value)) Then
  Begin
    Self.Flags := Self.Flags Xor meshGroupTriplanar;
  End;
End;

Procedure MeshGroup.SetAlphaMap(Map:TERRATexture);
Var
  N:Cardinal;
Begin
  _Material.AlphaMap := Map;
  If ((Self.Flags And meshGroupAlphaMap=0) = Assigned(Map)) Then
  Begin
    N := Self.Flags;
    Self.Flags := Self.Flags Xor meshGroupAlphaMap;

    If (Self.Flags<>N) Then
      ReleaseObject(_Buffer);
  End;
End;

Procedure MeshGroup.SetLightMap(Map:TERRATexture);
Var
  N:Cardinal;
Begin
  _Material.LightMap := Map;
  If ((Self.Flags And meshGroupLightMap=0) = Assigned(Map)) Then
  Begin
    N := Self.Flags;
    Self.Flags := Self.Flags Xor meshGroupLightMap;

    If (Self.Flags <>N) Then
      ReleaseObject(_Buffer);
  End;
End;

Procedure MeshGroup.SetDitherPatternMap(Map:TERRATexture);
Begin
  _Material.DitherPatternMap := Map;
End;



Function MeshGroup.SubDivideVertexFromTriangle(TriangleIndex, TargetVertex:Integer; UseA, UseB, UseC:Boolean):Integer;
Var
  J, K, Count:Integer;
  Tri:Triangle;
  Op:Array[0..2] Of Boolean;
  TP, P, N:Vector3D;
  TUV, UV0, UV1:Vector2D;
  TC:Color;
  C:Vector4D;
  Bones:Array[0..2] Of Single;
  BestBoneIndex:Integer;
  Factor:Single;
Begin
  Result := TargetVertex;
  Tri := Self.GetTriangle(TriangleIndex);

  Op[0] := UseA;
  Op[1] := UseB;
  Op[2] := UseC;

  P := VectorZero;
  N := VectorZero;
  UV0 := VectorCreate2D(0, 0);
  UV1 := UV0;
  C := VectorCreate4D(0,0,0,0);
  Count := 0;

  For J:=0 To 2 Do
      Bones[J] := 0.0;

  For J:=0 To 2 Do
  If (OP[J]) Then
  Begin
    Inc(Count);
    K := Tri.Indices[J];

    _Vertices.GetFloat(K, vertexBone, Bones[J]);
    _Vertices.GetVector3D(K, vertexPosition, TP);
    P.Add(TP);

    _Vertices.GetVector3D(K, vertexNormal, TP);
    N.Add(TP);

    _Vertices.GetVector2D(K, vertexUV0, TUV);
    UV0.Add(TUV);

    _Vertices.GetVector2D(K, vertexUV1, TUV);
    UV1.Add(TUV);

    _Vertices.GetColor(K, vertexColor, TC);
    C.Add(VectorCreate4D(TC.R/255, TC.G/255, TC.B/255, TC.A/255));

    UV1.Add(TUV);
  End;

  Factor := 1/Count;
  P.Scale(Factor);
  N.Scale(Factor);
  UV0.Scale(Factor);
  UV1.Scale(Factor);
  C.Scale(Factor);

  If (Not UseA) Then
    BestBoneIndex := 1
  Else
  If (Not UseB) Or (Not UseC) Then
    BestBoneIndex := 0
  Else
  Begin
    If (Bones[1] = Bones[2]) Then
      BestBoneIndex := 1
    Else
      BestBoneIndex := 0;
  End;

  TC := ColorCreateFromFloat(C.X, C.Y, C.Z, C.W);

  _Vertices.SetVector3D(TargetVertex, vertexPosition, P);
  _Vertices.SetVector3D(TargetVertex, vertexNormal, N);
  _Vertices.SetVector2D(TargetVertex, vertexUV0, UV0);
  _Vertices.SetVector2D(TargetVertex, vertexUV1, UV1);
  _Vertices.SetFloat(TargetVertex, vertexBone, Bones[BestBoneIndex]);
  _Vertices.SetColor(TargetVertex, vertexColor, TC);
End;


{
  This is an implementation of Tom Forsyth's "Linear-Speed Vertex Cache Optimization" algorithm as described here:
  http://home.comcast.net/~tom_forsyth/papers/fast_vert_cache_opt.html
}

Type
  OptimizeVertexData = Record
    score:Single;
    activeFaceListStart:Integer;
    activeFaceListSize:Integer;
    cachePos0:Integer;
    cachePos1:Integer;
  End;

Procedure MeshGroup.Optimize(VertexCacheSize:Integer);
Begin
End;
(*Const
  kMaxVertexCacheSize = 64;
  kMaxPrecomputedVertexValenceScores = 64;
Var
  s_vertexCacheScores:Array[0..kMaxVertexCacheSize, 0.. Pred(kMaxVertexCacheSize)] Of Single;
  s_vertexValenceScores:Array[0..Pred(kMaxPrecomputedVertexValenceScores)] Of Single;

  // code for computing vertex score was taken, as much as possible
  // directly from the original publication.
  Function ComputeVertexCacheScore(cachePosition, vertexCacheSize:Integer):Single;
  Const
    FindVertexScore_CacheDecayPower = 1.5;
    FindVertexScore_LastTriScore = 0.75;
  Var
    scaler:Single;
  Begin
    Result := 0.0;
    If ( cachePosition < 0 ) Then // Vertex is not in FIFO cache - no score.
      Exit;


    If ( cachePosition < 3 ) Then
                    // This vertex was used in the last triangle,
                    // so it has a fixed score, whichever of the three
                    // it's in. Otherwise, you can get very different
                    // answers depending on whether you add
                    // the triangle 1,2,3 or 3,1,2 - which is silly.
      Result := FindVertexScore_LastTriScore
    Else
    Begin
      assert ( cachePosition < vertexCacheSize );
      // Points for being high in the cache.
      scaler := 1.0 / ( vertexCacheSize - 3 );
      Result := 1.0 - ( cachePosition - 3 ) * scaler;
      Result := Pow(Result, FindVertexScore_CacheDecayPower);
    End;
  End;

  Function ComputeVertexValenceScore(numActiveFaces:Integer):Single;
  Const
    FindVertexScore_ValenceBoostScale = 2.0;
    FindVertexScore_ValenceBoostPower = 0.5;
  Var
    valenceBoost:Single;
  Begin
    // Bonus points for having a low number of tris still to
    // use the vert, so we get rid of lone verts quickly.
    valenceBoost := Pow(numActiveFaces, -FindVertexScore_ValenceBoostPower);
    Result := FindVertexScore_ValenceBoostScale * valenceBoost;
  End;


  Function ComputeVertexScores():Boolean;
  Var
    cacheSize, cachePos, valence:Integer;
  Begin
    For cacheSize:=0 To Pred(kMaxVertexCacheSize) Do
      For cachePos:=0 To Pred(cacheSize) Do
      Begin
        s_vertexCacheScores[cacheSize, cachePos] := ComputeVertexCacheScore(cachePos, cacheSize);
      End;

      For valence:=0 To Pred(kMaxPrecomputedVertexValenceScores) Do
         s_vertexValenceScores[valence] := ComputeVertexValenceScore(valence);

      Result := True;
  End;

  Function FindVertexCacheScore(cachePosition, maxSizeVertexCache:Integer):Single; {$IFDEF FPC} Inline; {$ENDIF}
  Begin
    Result := s_vertexCacheScores[maxSizeVertexCache, cachePosition];
  End;

  Function FindVertexValenceScore(numActiveTris:Integer):Single; {$IFDEF FPC} Inline; {$ENDIF}
  Begin
    Result := s_vertexValenceScores[numActiveTris];
  End;

  Function FindVertexScore(numActiveFaces, cachePosition, vertexCacheSize:Integer):Single;
  Begin
    If ( numActiveFaces = 0 ) Then
    Begin
                // No tri needs this vertex!
      Result := -1.0;
      Exit;
    End;

    Result := 0.0;
    If (cachePosition < vertexCacheSize) Then
    Begin
      Result := s_vertexCacheScores[vertexCacheSize, cachePosition];
    End;

    If (numActiveFaces < kMaxPrecomputedVertexValenceScores) Then
    Begin
      Result := Result + s_vertexValenceScores[numActiveFaces];
    End Else
    Begin
      Result := Result + ComputeVertexValenceScore(numActiveFaces);
    End;
  End;

Const
  kEvictedCacheIndex  = 65535;
Var
  s_vertexScoresComputed:Boolean;
  vertexDataList:Array Of OptimizeVertexData;
  activeFaceList:Array Of Integer;
  processedFaceList:Array Of Boolean;
  vertexCacheBuffer:Array[0..Pred((kMaxVertexCacheSize+3)*2)] Of Integer;
  cache0, cache1, temp:PIntegerArray;
  curActiveFaceListPos:Integer;
  entriesInCache0, entriesInCache1:Integer;
  bestFace:Integer;
  bestScore:Single;
  maxValenceScore:Single;
  I,J,K, V, N, Index:Integer;
  faceScore:Single;
  TT:PTriangle;
  c0, c1, face:Integer;
  oldIndexList:Array Of Integer;
Begin
(*  s_vertexScoresComputed := ComputeVertexScores();

  SetLength(vertexDataList, _VertexCount);
  SetLength(oldIndexList, _TriangleCount * 3);

  // compute face count per vertex
  For I:=0 To Pred(_TriangleCount) Do
  Begin
    TT := Self.GetTrianglePointer(I);
    If TT = Nil Then
      Exit;

    For J:=0 To 2 Do
    Begin
      oldIndexList[I*3+J] := TT.Indices[J];
      Inc(vertexDataList[TT.Indices[J]].activeFaceListSize);
    End;
  End;


  // allocate face list per vertex
  curActiveFaceListPos := 0;
  For I:=0 To Pred(_vertexCount) Do
  Begin
    vertexDataList[i].cachePos0 := kEvictedCacheIndex;
    vertexDataList[i].cachePos1 := kEvictedCacheIndex;
    vertexDataList[i].activeFaceListStart := curActiveFaceListPos;
    Inc(curActiveFaceListPos, vertexDataList[i].activeFaceListSize);
    vertexDataList[i].score := FindVertexScore(vertexDataList[i].activeFaceListSize, vertexDataList[i].cachePos0, vertexCacheSize);
    vertexDataList[i].activeFaceListSize := 0;
  End;

  SetLength(activeFaceList, curActiveFaceListPos);

  // fill out face list per vertex
  For I:=0 To Pred(_TriangleCount) Do
  Begin
    TT := Self.GetTrianglePointer(I);
    If TT = Nil Then
      Exit;

    For J:=0 To 2 Do
    Begin
      Index := TT.Indices[J];
      activeFaceList[vertexDataList[index].activeFaceListStart + vertexDataList[index].activeFaceListSize] := I;
      Inc(vertexDataList[index].activeFaceListSize);
    End;
  End;

  SetLength(processedFaceList, _TriangleCount);

  cache0 := @(vertexCacheBuffer[0]);
  cache1 := @(vertexCacheBuffer[kMaxVertexCacheSize+3]);
  entriesInCache0 := 0;

  bestFace := 0;
  bestScore := -1.0;

  maxValenceScore := FindVertexScore(1, kEvictedCacheIndex, vertexCacheSize) * 3.0;

  For I:=0 To Pred(_TriangleCount) Do
  Begin
    If (bestScore < 0.0) Then
    Begin
      // no verts in the cache are used by any unprocessed faces so
      // search all unprocessed faces for a new starting point

      For J:=0 To Pred(_TriangleCount) Do
      Begin
        //T2 := @(oldTriangleList[J]);
        If (Not processedFaceList[J]) Then
        Begin
          faceScore := 0.0;
          For K:=0 To 2 Do
          Begin
            index := oldIndexList[J*3 + K];
              assert(vertexDataList[index].activeFaceListSize > 0);
            assert(vertexDataList[index].cachePos0 >= vertexCacheSize);
            faceScore := faceScore + vertexDataList[index].score;
          End;

          If (faceScore > bestScore) Then
          Begin
            bestScore := faceScore;
            bestFace := J;

            assert(bestScore <= maxValenceScore);
            If (bestScore >= maxValenceScore) Then
              Break;
          End;
        End;
      End;
      
      assert(bestScore >= 0.0);
    End;

      processedFaceList[bestFace] := True;
      entriesInCache1 := 0;

      // add bestFace to LRU cache and to newIndexList
      For V:=0 To 2 Do
      Begin
        Index := oldIndexList[BestFace*3+V];
        _Triangles[I].Indices[V] := index;

        If (vertexDataList[index].cachePos1 >= entriesInCache1) Then
        Begin
          vertexDataList[index].cachePos1 := entriesInCache1;
          cache1[entriesInCache1] := index;
          Inc(entriesInCache1);

          If (vertexDataList[index].activeFaceListSize = 1) Then
          Begin
            Dec(vertexDataList[index].activeFaceListSize);
            Continue;
          End;
        End;

        assert(vertexDataList[index].activeFaceListSize > 0);
        N := -1;
        For K:=vertexDataList[index].activeFaceListStart To vertexDataList[index].activeFaceListStart + Pred(vertexDataList[index].activeFaceListSize) Do
        If (activeFaceList[K] = bestFace) Then
        Begin
          N := K;
          Break;
        End;

        If (N<0) Then
          Exit;

        K := activeFaceList[N];
        activeFaceList[N] := activeFaceList[Pred(vertexDataList[index].activeFaceListSize)];
        activeFaceList[Pred(vertexDataList[index].activeFaceListSize)] := K;


        Dec(vertexDataList[index].activeFaceListSize);
        vertexDataList[index].score := FindVertexScore(vertexDataList[index].activeFaceListSize, vertexDataList[index].cachePos1, vertexCacheSize);
      End;

      // move the rest of the old verts in the cache down and compute their new scores
      For c0 := 0 To Pred(entriesInCache0) Do
      Begin
        index := cache0[c0];
        If (vertexDataList[index].cachePos1 >= entriesInCache1) Then
        Begin
          vertexDataList[index].cachePos1 := entriesInCache1;
          cache1[entriesInCache1] := index;
          Inc(entriesInCache1);
          vertexDataList[index].score := FindVertexScore(vertexDataList[index].activeFaceListSize, vertexDataList[index].cachePos1, vertexCacheSize);
        End;
      End;

      // find the best scoring triangle in the current cache (including up to 3 that were just evicted)
      bestScore := -1.0;
      For c1 := 0 To Pred(entriesInCache1) Do
      Begin
        index := cache1[c1];
        vertexDataList[index].cachePos0 := vertexDataList[index].cachePos1;
        vertexDataList[index].cachePos1 := kEvictedCacheIndex;
        For J:=0 To Pred(vertexDataList[index].activeFaceListSize) Do
        Begin
          faceScore := 0.0;
          face := activeFaceList[vertexDataList[index].activeFaceListStart+j];
          For V:=0 To 2 Do
              faceScore := faceScore + vertexDataList[oldindexList[face * 3 + v]].score;

          If (faceScore > bestScore) Then
          Begin
            bestScore := faceScore;
            bestFace := face;
          End;
        End;
      End;

      temp := cache0;
      cache0 := cache1;
      cache1 := temp;
      entriesInCache0 := IntMin(entriesInCache1, vertexCacheSize);
    End;
End;
  *)

Procedure MeshGroup.SetWireframe(Enabled: Boolean);
Begin
  If ((Self.Flags And meshGroupWireframe<>0) = Enabled) Then
    Exit;

  Self.Flags := Self.Flags Xor meshGroupWireframe;
  ReleaseObject(_Buffer);
End;

Procedure MeshGroup.SetEdge(TriangleIndex, EdgeIndex: Integer; Visible: Boolean);
Var
  I,J:Integer;
Begin
  If (Length(_Edges)<>_TriangleCount) Then
  Begin
    SetLength(_Edges, _TriangleCount);
    For I:=0 To Pred(_TriangleCount) Do
      For J:=0 To 2 Do
        _Edges[I].Visible[J] := True;
  End;

  If (TriangleIndex>=0) And (TriangleIndex<_TriangleCount) And(EdgeIndex>=0) And (EdgeIndex<=0) Then
    _Edges[TriangleIndex].Visible[EdgeIndex] := Visible;
End;

Procedure MeshGroup.BindMaterial(View:TERRAViewport; Var Slot: Integer; Const Material:MeshMaterial);
Var
  Tex:TERRATexture;
  FlowCycle:Vector3D;
  Graphics:GraphicsManager;
Begin
  Slot := 1;

  Graphics := GraphicsManager.Instance;

  If (Graphics.Renderer.Settings.DynamicShadows.Enabled) And (Graphics.RenderStage=renderStageDiffuse) Then
  Begin
    Tex := View.GetRenderTexture(captureTargetShadow);

    If Assigned(Tex) Then
    Begin
      Tex.Filter := filterBilinear;
//      Tex.WrapMode := wrapNothing;
      Tex.MipMapped := False;
    End Else
      Tex := TextureManager.Instance.WhiteTexture;

    Tex.Bind(Slot);
    _Shader.SetIntegerUniform(ShadowMapUniformName, Slot);
    Inc(Slot);
  End;

  If (Material.ColorTable<>Nil) Then
  Begin
    ColorTableBind(Material.ColorTable, Slot);
    Inc(Slot);
  End;

  If (Assigned(_Shader)) And (Assigned(Material.DecalMap)) And (_Shader.HasUniform(DecalMapUniformName)) Then
  Begin
    Material.DecalMap.Bind(Slot);
    _Shader.SetIntegerUniform(DecalMapUniformName, Slot);

    Inc(Slot);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing triplanar');  {$ENDIF}
  If (Self.Flags And meshGroupTriplanar<>0)  Then
  Begin
    Tex := Material.TriplanarMap;

    If Not Assigned(Tex) Then
      Tex := Material.DiffuseMap;

    Tex.WrapMode := wrapAll;
    Tex.Bind(Slot);
    If Assigned(_Shader) Then
      _Shader.SetIntegerUniform('diffuseMap2', Slot);

    Inc(Slot);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing normalmapping');  {$ENDIF}
  If (_Owner._NormalMapping) And (Assigned(_Shader)) And (_Shader.HasUniform(NormalMapUniformName)) Then
  Begin
    If Assigned(Material.NormalMap) Then
      Tex := Material.NormalMap
    Else
      Tex := TextureManager.Instance.DefaultNormalMap;

    Tex.Bind(Slot);
    _Shader.SetIntegerUniform(NormalMapUniformName, Slot);

    Inc(Slot);
  End;

  {$IFDEF PC}
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing DisplacementMapping');  {$ENDIF}
  If (_Owner._NormalMapping) And (Assigned(_Shader)) And (_Shader.HasUniform(DisplacementMapUniformName)) Then
  Begin
    If Assigned(Material.DisplacementMap) Then
      Tex := Material.DisplacementMap
    Else
      Tex := TextureManager.Instance.BlackTexture;

    Tex.Bind(Slot);
    _Shader.SetIntegerUniform(DisplacementMapUniformName, Slot);

    Inc(Slot);
  End;
  {$ENDIF}

	{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing specular map');  {$ENDIF}
	If (Assigned(Material.SpecularMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(SpecularMapUniformName)) Then
	Begin
    If (Assigned(Material.SpecularMap)) Then
  	  Material.SpecularMap.Bind(Slot)
    Else
      TextureManager.Instance.BlackTexture.Bind(Slot);

    _Shader.SetIntegerUniform(SpecularMapUniformName, Slot);

	  Inc(Slot);
	End;

	{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing color ramp');  {$ENDIF}
	If (Assigned(Material.ToonRamp)) And (Assigned(_Shader)) And (_Shader.HasUniform(ToonRampUniformName)) Then
	Begin
    Material.ToonRamp.WrapMode := wrapNothing;
    Material.ToonRamp.Filter := filterBilinear;
    Material.ToonRamp.MipMapped := True;
	  Material.ToonRamp.Bind(Slot);
    _Shader.SetIntegerUniform(ToonRampUniformName, Slot);
	  Inc(Slot);
	End;

	{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing lightmap');  {$ENDIF}
	If (Flags And meshGroupLightmap<>0) Then
	Begin
    If (Assigned(Material.Lightmap)) Then
  	  Material.Lightmap.Bind(Slot)
    Else
      TextureManager.Instance.WhiteTexture.Bind(Slot);

	  If Assigned(_Shader) Then
		_Shader.SetIntegerUniform('lightMap', Slot);
	  Inc(Slot);
	End;

	{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing alphamap');  {$ENDIF}
	If (Assigned(Material.Alphamap)) And (Assigned(_Shader)) And (_Shader.HasUniform(AlphaMapUniformName)) Then
	Begin
	  Material.Alphamap.Bind(Slot);
    _Shader.SetIntegerUniform(AlphaMapUniformName, Slot);

	  Inc(Slot);
	End;

  If (Assigned(Material.ReflectiveMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(ReflectiveMapUniformName)) Then
  Begin
	  Material.ReflectiveMap.Bind(Slot);
    _Shader.SetIntegerUniform(ReflectiveMapUniformName, Slot);
	  Inc(Slot);
  End;

  If (Assigned(Material.ReflectionMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(ReflectionMapUniformName)) Then
  Begin
    Material.ReflectionMap.Bind(Slot);
    _Shader.SetIntegerUniform(ReflectionMapUniformName, Slot);

    Inc(Slot);
  End;

  If (Material.HueShift<>0.0) Or (Self.Flags And meshGroupHueShift<>0) Then
  Begin
    _Shader.SetFloatUniform(HueShiftUniformName, Material.HueShift);
  End;

(*  If (Assigned(Material.DitherPatternMap)) And (Assigned(_Shader))  And (_Shader.HasUniform(DitherPatternMapUniformName)) Then
  Begin
    If (_DitherScale<=0) Then
    Begin
      _DitherScale := Self.CalculateDitherScale();
    End;

    _Shader.SetFloatUniform(DitherScaleUniformName, _DitherScale);

    Material.DitherPatternMap.Bind(Slot);
    _Shader.SetIntegerUniform(DitherPatternMapUniformName, Slot);

    Inc(Slot);
   End;*)

(*  If (Assigned(Material.DitherPaletteMap)) And (Assigned(_Shader))  And (_Shader.HasUniform(DitherPaletteMapUniformName)) Then
  Begin
    Material.DitherPaletteMap.Bind(Slot);
    _Shader.SetIntegerUniform(DitherPaletteMapUniformName, Slot);

    Inc(Slot);
  End;*)

  If (Assigned(Material.EnviromentMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(SphereMapUniformName)) Then
  Begin
	  Material.EnviromentMap.Bind(Slot);
    _Shader.SetIntegerUniform(SphereMapUniformName, Slot);

	  Inc(Slot);
  End;

  If (Assigned(Material.FlowMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(FlowMapUniformName)) Then
  Begin
    Material.FlowMap.WrapMode := wrapNothing;
    Material.FlowMap.Filter := filterBilinear;
    Material.FlowMap.MipMapped := False;
	  Material.FlowMap.Bind(Slot);

    FlowCycle.Z := 0; //FloatMod(GetTime, 1000);
    FlowCycle.X := FloatMod(Application.GetTime, 1000) / 2000;
    FlowCycle.Y := 2 * Abs(FlowCycle.X - 0.5);

		_Shader.SetIntegerUniform(FlowMapUniformName, Slot);
    _Shader.SetFloatUniform('flowSpeed', Material.FlowSpeed);
    _Shader.SetVec3Uniform('flowCycle', FlowCycle);
    //Log(logDebug, 'Lol', 'Settings bounds');
    _Shader.SetVec4Uniform('flowBounds', Material.FlowBounds);

	  Inc(Slot);
  End;

  If (Assigned(Material.NoiseMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(NoiseMapUniformName)) Then
  Begin
	  Material.NoiseMap.Bind(Slot);
    _Shader.SetIntegerUniform(NoiseMapUniformName, Slot);
	  Inc(Slot);
  End;

  If (Graphics.ReflectionActive) Then
  Begin
    If Assigned(Graphics.ReflectionMask) Then
      Graphics.ReflectionMask.Bind(Slot)
    Else
      TextureManager.Instance.BlackTexture.Bind(Slot);

    If Assigned(_Shader) Then
      _Shader.SetIntegerUniform('screenMask', Slot);
	  Inc(Slot);
  End;

    {    If (True) Then
        Begin
          CubeMapTexture.Bind(CubeMap, Slot);
          Shader.SetUniform('cubeMap', Slot);
          Inc(Slot);
        End;}
End;

Procedure MeshGroup.SetBlendMode(const Value: Integer);
Var
  NeedTransparency:Boolean;
Begin
  Self._Material.BlendMode := Value;

  NeedTransparency := (Value <> blendNone);
End;

Constructor MeshGroup.Create(ID:Integer; Parent:TERRAMesh; Format:VertexFormat; Name:TERRAString);
Begin
  If Name='' Then
    Name := 'group'+IntToString(ID);

  Self._Name := Name;
  Self._ID := ID;
  Self._Owner := Parent;

  Self._Material.Reset();
  Self._Unique := False;

  Self._Vertices := VertexData.Create(Format, 0);

  Self._GPUSkinning := GraphicsManager.Instance.Renderer.Settings.VertexBufferObject.Enabled;
//  Self._GPUSkinning := False;
End;

Function MeshGroup.LockVertices():VertexData;
Var
  I:Integer;
  P:VertexData;
Begin
  If (_Vertices = Nil) Or (_Vertices.Count<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;

  {If (Assigned(Self._Buffer)) Then
  Begin
    Result := Self._Buffer.Lock();
  End Else}

  If Assigned(_ScratchVertices) Then
  Begin
    If _Vertices.Count = _ScratchVertices.Count Then
      _Vertices.CopyBuffer(_ScratchVertices)
    Else
      ReleaseObject(_ScratchVertices);
  End;

  If (_ScratchVertices = Nil) Then
    _ScratchVertices := _Vertices.Clone();

  Result := _Vertices;
End;

Procedure MeshGroup.UnlockVertices;
Begin
  If (Assigned(Self._Buffer)) And (Assigned(_Vertices)) Then
  Begin
    //Self._Buffer.Unlock();
    Self._Buffer.Update(_Vertices.Buffer);
  End;

  If Assigned(_Owner) Then
    Inc(_Owner._ScratchID);
End;

Procedure MeshGroup.SetVertexMorph(ID, VertexIndex:Integer; Value:Vector3D);
Var
  N, I, Len:Integer;
Begin
  If (VertexIndex<0) Or (VertexIndex>= Self.VertexCount) Then
    Exit;

  N := -1;
  For I:=0 To Pred(_MorphCount) Do
  If (_Morphs[I].ID = ID) Then
  Begin
    N := I;
    Break;
  End;

  If N<0 Then
    Exit;

  Len := Length(_Morphs[N].Values);

  If (Len<=0) Then
  Begin
    SetLength(_Morphs[N].Values, 64);
  End Else
  If (Len<=VertexIndex) Then
  Begin
    SetLength(_Morphs[N].Values, Len * 2);
  End;

  _Morphs[N].Values[VertexIndex] := Value;
End;

Function MeshGroup.GetVertexMorph(ID, VertexIndex: Integer): Vector3D;
Var
  I:Integer;
Begin
  If (VertexIndex<0) Or (VertexIndex >= Self.VertexCount) Then
  Begin
    Result := VectorZero;
    Exit;
  End;

  For I:=0 To Pred(_MorphCount) Do
  If (_Morphs[I].ID = ID) Then
  Begin
    Result := _Morphs[I].Values[VertexIndex];
    Exit;
  End;

  Self._Vertices.GetVector3D(I, vertexPosition, Result);
End;

Function MeshGroup.AddVertexMorph(ID:Integer):Integer;
Begin
  Result := _MorphCount;
  Inc(_MorphCount);
  SetLength(_Morphs, _MorphCount);

  _Morphs[Result].ID := ID;
  _Morphs[Result].MorphType := 0;
  SetLength(_Morphs[Result].Values, Self.VertexCount);
End;

Function MeshGroup.HasVertexMorph(ID: Integer): Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MorphCount) Do
  If (_Morphs[I].ID = ID) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Procedure MeshGroup.CullTriangles(Box: BoundingBox; Transform:Matrix4x4);
Var
  Visibility:Array Of Boolean;
  I,J:Integer;
  P:Vector3D;
  Found:Boolean;
Begin
  If (_VisibleTriangles = Nil) Then
    SetLength(_VisibleTriangles, _TriangleCount);

  SetLength(Visibility, Self.VertexCount);

  _VisibleTriangleCount := 0;
  For I:=0 To Pred(Self.VertexCount) Do
  Begin
    _Vertices.GetVector3D(I, vertexPosition, P);
    P := Transform.Transform(P);
    Visibility[I] := Box.Contains(P);
  End;

  For I:=0 To Pred(Self._TriangleCount) Do
  Begin
    Found := False;
    For J:=0 To 2 Do
    If (Visibility[_Triangles[I].Indices[J]]) Then
    Begin
      Found := True;
      Break;
    End;

    If (Found) Then
    Begin
      _VisibleTriangles[_VisibleTriangleCount] := _Triangles[I];
      Inc(_VisibleTriangleCount);
    End;
  End;
End;

Procedure MeshGroup.UncullTriangles;
Begin
  _VisibleTriangleCount := _TriangleCount;
End;

Function MeshGroup.GetAlphaMap:TERRATexture;
Begin
  Result := _Material.AlphaMap;
End;

(*Function MeshGroup.GetAmbientColor: Color;
Begin
  Result := _Material.AmbientColor;
End;*)

Function MeshGroup.GetToonRamp:TERRATexture;
Begin
  Result := _Material.ToonRamp;
End;

Function MeshGroup.GetDecalMap:TERRATexture;
Begin
  Result := _Material.DecalMap;
End;

Function MeshGroup.GetDiffuseColor: Color;
Begin
  Result := _Material.DiffuseColor;
End;

Function MeshGroup.GetDiffuseMap:TERRATexture;
Begin
  Result := _Material.DiffuseMap;
End;

function MeshGroup.GetGlowMap:TERRATexture;
Begin
  Result := _Material.GlowMap;
End;

Function MeshGroup.GetLightMap:TERRATexture;
Begin
  Result := _Material.LightMap;
End;

Function MeshGroup.GetDitherPatternMap:TERRATexture;
Begin
  Result := _Material.DitherPatternMap;
End;

Function MeshGroup.GetNormalMap:TERRATexture;
Begin
  Result := _Material.NormalMap;
End;

Function MeshGroup.GetDisplacementMap:TERRATexture;
Begin
  Result := _Material.DisplacementMap;
End;

Function MeshGroup.GetRefractionMap:TERRATexture;
Begin
  Result := _Material.RefractionMap;
End;

Function MeshGroup.GetReflectiveMap:TERRATexture;
Begin
  Result := _Material.ReflectiveMap;
End;

Function MeshGroup.GetEnviromentMap:TERRATexture;
Begin
  Result := _Material.EnviromentMap;
End;

Function MeshGroup.GetFlowMap:TERRATexture;
Begin
  Result := _Material.FlowMap;
End;

Function MeshGroup.GetNoiseMap:TERRATexture;
Begin
  Result := _Material.NoiseMap;
End;

Function MeshGroup.GetSpecularMap:TERRATexture;
Begin
  Result := _Material.SpecularMap;
End;

Function MeshGroup.GetTriplanarMap:TERRATexture;
Begin
  Result := _Material.TriplanarMap;
End;

(*procedure MeshGroup.SetAmbientColor(const Value: Color);
Begin
  _Material.AmbientColor := Value;
End;*)

procedure MeshGroup.SetDecalMap(const Value:TERRATexture);
Begin
  _Material.DecalMap := Value;
End;

Procedure MeshGroup.SetDiffuseColor(const Value: Color);
Begin
  _Material.DiffuseColor := Value;
End;

procedure MeshGroup.SetDiffuseMap(const Value:TERRATexture);
Begin
  _Material.DiffuseMap := Value;
End;

procedure MeshGroup.SetGlowMap(const Value:TERRATexture);
Begin
  _Material.GlowMap := Value;
End;

Procedure MeshGroup.SetNormalMap(const Value:TERRATexture);
Begin
  _Material.NormalMap := Value;
End;

Procedure MeshGroup.SetDisplacementMap(const Value:TERRATexture);
Begin
  _Material.DisplacementMap := Value;
End;

procedure MeshGroup.SetRefractionMap(const Value:TERRATexture);
Begin
  _Material.RefractionMap := Value;
End;

procedure MeshGroup.SetReflectiveMap(const Value:TERRATexture);
Begin
  _Material.ReflectiveMap := Value;
End;

procedure MeshGroup.SetEnviromentMap(const Value:TERRATexture);
Begin
  _Material.EnviromentMap := Value;
End;

procedure MeshGroup.SetFlowMap(const Value:TERRATexture);
Begin
  _Material.FlowMap := Value;
End;

procedure MeshGroup.SetNoiseMap(const Value:TERRATexture);
Begin
  _Material.NoiseMap := Value;
End;

Procedure MeshGroup.SetSpecularMap(const Value:TERRATexture);
Begin
  _Material.SpecularMap := Value;
End;

Procedure MeshGroup.SetToonRamp(const Map:TERRATexture);
Begin
  _Material.ToonRamp := Map;
End;

function MeshGroup.GetBlendMode: Integer;
Begin
  Result := _Material.BlendMode;
End;

Procedure MeshGroup.InheritMaterial(Const OtherMat: MeshMaterial; Var DestMaterial: MeshMaterial);
  Function SelectTexture(A,B,C:TERRATexture):TERRATexture;
  Begin
    If A<>Nil Then
      Result := A
    Else
    If B<>Nil Then
      Result := B
    Else
      Result := C;
  End;
Var
  Transparency:Boolean;
Begin
  DestMaterial.DiffuseColor := ColorMultiply(_Material.DiffuseColor, OtherMat.DiffuseColor);
  //DestMaterial.AmbientColor := ColorMultiply(_Material.AmbientColor, OtherMat.AmbientColor);

  If (OtherMat.ShadowColor.A>0) Then
    DestMaterial.ShadowColor := OtherMat.ShadowColor
  Else
  If (_Material.ShadowColor.A>0) Then
    DestMaterial.ShadowColor := _Material.ShadowColor
  Else
    DestMaterial.ShadowColor := ColorGrey(240);

  If (OtherMat.OutlineColor.A>0) Then
    DestMaterial.OutlineColor := OtherMat.OutlineColor
  Else
    DestMaterial.OutlineColor := _Material.OutlineColor;

  If (OtherMat.FlowBounds.W>0.0) Then
    DestMaterial.FlowBounds := OtherMat.FlowBounds
  Else
  Begin
    If (_Material.FlowBounds.W<=0.0) Then
      _Material.FlowBounds := MakeWaterFlowBounds(_BoundingBox);

    DestMaterial.FlowBounds := _Material.FlowBounds;
  End;

  DestMaterial.FlowSpeed := {_Material.FlowSpeed * }OtherMat.FlowSpeed;

  DestMaterial.HueShift := _Material.HueShift + OtherMat.HueShift;

  If (Self.Flags And meshGroupIgnoreColorTable<>0) Then
    DestMaterial.ColorTable := Nil
  Else
    DestMaterial.ColorTable := SelectTexture(OtherMat.ColorTable, _Material.ColorTable, Nil);

  DestMaterial.DiffuseMap := SelectTexture(OtherMat.DiffuseMap, _Material.DiffuseMap, TextureManager.Instance.WhiteTexture);
  DestMaterial.DecalMap := SelectTexture(OtherMat.DecalMap, _Material.DecalMap, Nil);
  DestMaterial.TriplanarMap := SelectTexture(OtherMat.TriplanarMap, _Material.TriplanarMap, DestMaterial.DiffuseMap);
  DestMaterial.NormalMap := SelectTexture(OtherMat.NormalMap, _Material.NormalMap, Nil);
  DestMaterial.DisplacementMap := SelectTexture(OtherMat.DisplacementMap, _Material.DisplacementMap, Nil);
  DestMaterial.SpecularMap := SelectTexture(OtherMat.SpecularMap, _Material.SpecularMap, TextureManager.Instance.BlackTexture);
  DestMaterial.GlowMap := SelectTexture(OtherMat.GlowMap, _Material.GlowMap, TextureManager.Instance.BlackTexture);
  DestMaterial.RefractionMap := SelectTexture(OtherMat.RefractionMap, _Material.RefractionMap, TextureManager.Instance.BlackTexture);
  DestMaterial.ReflectiveMap := SelectTexture(OtherMat.ReflectiveMap, _Material.ReflectiveMap, Nil);
  DestMaterial.AlphaMap := SelectTexture(OtherMat.AlphaMap, _Material.AlphaMap, TextureManager.Instance.WhiteTexture);
  DestMaterial.LightMap := SelectTexture(OtherMat.LightMap, _Material.LightMap, Nil);
  DestMaterial.ToonRamp := SelectTexture(OtherMat.ToonRamp, _Material.ToonRamp, GraphicsManager.Instance.ToonRamp);
  DestMaterial.FlowMap := SelectTexture(OtherMat.FlowMap, _Material.FlowMap, Nil);
  DestMaterial.NoiseMap := SelectTexture(OtherMat.NoiseMap, _Material.NoiseMap, Nil);
  DestMaterial.ReflectionMap := TextureManager.Instance.BlackTexture;
  DestMaterial.DitherPatternMap := SelectTexture(OtherMat.DitherPatternMap, _Material.DitherPatternMap, Nil);

  If Assigned(DestMaterial.NormalMap) Then
    DestMaterial.NormalMap.WrapMode := wrapAll;

  If Assigned(DestMaterial.NoiseMap) Then
    DestMaterial.NoiseMap.WrapMode := wrapAll;

  DestMaterial.VegetationBend := OtherMat.VegetationBend;
  DestMaterial.Ghost := OtherMat.Ghost;

  DestMaterial.EnviromentMap := GraphicsManager.Instance.EnviromentMap;

  Transparency := (DestMaterial.DiffuseColor.A<255) Or (DestMaterial.DiffuseMap.TransparencyType<>imageOpaque);

  If (OtherMat.BlendMode>=0) Then
    DestMaterial.BlendMode := OtherMat.BlendMode
  Else
  If (_Material.BlendMode>=0) Then
    DestMaterial.BlendMode := _Material.BlendMode
  Else
  If (Transparency) Then
    DestMaterial.BlendMode := blendBlend
  Else
    DestMaterial.BlendMode := blendNone;
End;

Function MeshGroup.GetVertexCount: Integer;
Begin
  Result := Vertices.Count;
End;

Function MeshGroup.GetVertices:VertexData;
Begin
  If _Vertices = Nil Then
  Begin
    RaiseError('Vertex format not set!');
  End;

  Result := _Vertices;
End;

Function MeshGroup.GetShadowColor: Color;
Begin
  Result := _Material.ShadowColor;
End;

Procedure MeshGroup.SetShadowColor(const Value: Color);
Begin
  _Material.ShadowColor := Value;
End;

Procedure MeshGroup.InspectAlpha(Tex:TERRATexture);
Begin
  _AlphaInspected := Tex;
End;

Function MeshGroup.GetHueShift: Single;
Begin
  Result := _Material.HueShift;
End;

Procedure MeshGroup.SetHueShift(const Value: Single);
Begin
  _Material.HueShift := Value;
End;

{ Mesh }
Class Function TERRAMesh.GetManager: Pointer;
Begin
  Result := MeshManager.Instance;
End;

(*Procedure Mesh.OnContextLost;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
    _Groups[I].OnContextLost();
End;*)

Function TERRAMesh.GetGroup(Index:Integer):MeshGroup;
Begin
  If (Index>=0) And (Index<_GroupCount) Then
    Result := _Groups[Index]
  Else
    Result := Nil;
End;

Function TERRAMesh.GetGroup(Name:TERRAString):MeshGroup;
Var
	I:Integer;
Begin
  Self.Prefetch();

	For I:=0 To Pred(_GroupCount) Do
	If StringEquals(Name, _Groups[I]._Name) Then
	Begin
		Result := _Groups[I];
		Exit;
	End;

	Result := Nil;
End;

Function TERRAMesh.GetGroupIndex(Name:TERRAString):Integer;
Var
	I:Integer;
Begin
	For I:=0 To Pred(_GroupCount) Do
	If StringEquals(Name, _Groups[I]._Name) Then
	Begin
		Result := I;
		Exit;
	End;

	Result := -1;
End;


Function TERRAMesh.AddGroup(Format:VertexFormat; Name:TERRAString=''):MeshGroup;
Begin
	Inc(_GroupCount);
	SetLength(_Groups, _GroupCount);

	Result := MeshGroup.Create(Pred(_GroupCount), Self, Format, Name);
  _Groups[Pred(_GroupCount)] := Result;
End;

Function TERRAMesh.DuplicateGroup(Group:MeshGroup; Name:TERRAString=''):MeshGroup;
Var
  I:Integer;
Begin
  Result := Self.AddGroup(Group.Vertices.Format, Name);
  If Group = Nil Then
    Exit;

  Result.Flags := Group.Flags;
  Result._Material := Group._Material;

  Result._Vertices := Group._Vertices.Clone();

  Result._TriangleCount := Group._TriangleCount;
  Result._VisibleTriangleCount := Group._TriangleCount;
  SetLength(Result._Triangles, Group._TriangleCount);
  For I:=0 To Pred(Group._TriangleCount) Do
    Result._Triangles[I] := Group._Triangles[I];

  Result._BoundingBox := Group._BoundingBox;
End;

Procedure TERRAMesh.AddTriangle(Const A,B,C:Integer; Group:MeshGroup);
Begin
	Group.AddTriangle(A,B,C);
End;

Procedure TERRAMesh.AddQuad(Const A,B,C,D:Integer; Group:MeshGroup);
Begin
	Group.AddQuad(A,B,C,D);
End;

{Procedure Mesh.AddMesh(MyMesh:Mesh; Const Transform:Matrix4x4);
Var
	I,J:Integer;
	Group, NewGroup:MeshGroup;
	A,B,C:MeshVertex;
Begin
	For J:=0 To Pred(MyMesh._GroupCount) Do
	Begin
		Group := MyMesh._Groups[J];
    NewGroup := MyMesh.AddGroup;

		For I:=0 To Pred(Group._TriangleCount) Do
		Begin
			A := Group._Vertices[Group._Triangles[I].A];
			B := Group._Vertices[Group._Triangles[I].B];
			C := Group._Vertices[Group._Triangles[I].C];

			A.Position := Transform.Transform(A.Position);
			B.Position := Transform.Transform(B.Position);
			C.Position := Transform.Transform(C.Position);

			A.Normal := VectorTransformNormal(A.Normal, Transform);
			B.Normal := VectorTransformNormal(B.Normal, Transform);
			C.Normal := VectorTransformNormal(C.Normal, Transform);

			Self.AddTriangle(A,B,C, NewGroup);
		End;
	End;
End;
 }

Procedure TERRAMesh.RemoveGroups;
Var
	I:Integer;
Begin
    For I:=0 To Pred(_GroupCount) Do
    If Assigned(_Groups[I]) Then
    Begin
      _Groups[I].Clean();
      ReleaseObject(_Groups[I]);
    End;
	_GroupCount := 0;
	_Groups := Nil;
End;

Procedure TERRAMesh.Clean;
Var
	I:Integer;
Begin
  Self.RemoveGroups;

	For I:=0 To Pred(_MetadataCount) Do
        ReleaseObject(_Metadata[I]);

    _Metadata := Nil;
    _MetadataCount := 0;

	For I:=0 To Pred(_EmitterCount) Do
        ReleaseObject(_Emitters[I]);
  _Emitters := Nil;
  _EmitterCount := 0;

  ReleaseObject(_Skeleton);
End;

Function TERRAMesh.GetSkeleton:MeshSkeleton;
Begin
  If Not Assigned(_Skeleton) Then
  Begin
    _Skeleton := MeshSkeleton.Create;
    _Skeleton.Name := Self.Name;
  End;

  Result := _Skeleton;
End;

Function TERRAMesh.Load(Source:Stream):Boolean;
Var
  Size:Integer;
  Tag:FileHeader;
  Handler:MeshDataBlockHandler;
Begin
  _GroupCount := 0;

  Result := False;
  Log(logDebug, 'Mesh', 'Loading mesh: '+Self.Name);

  Source.Read(@Tag, 4);
  If (Tag<>tagMeshHeader) Then
  Begin
    Log(logError, 'Mesh', 'Invalid mesh file! ['+Source.Name+']');
    Exit;
  End;

  Repeat
    Source.Read(@Tag, 4);
    If (Tag = tagMeshEnd) Then
      Break;

    Source.Read(@Size, 4);

    Handler := GetMeshDataHandler(Tag);
    Handler(Self, Size, Source);
  Until Source.EOF;

  Result := True;
End;

Function TERRAMesh.Save(Dest:Stream):Boolean;
Var
  I,J, Size, Temp, Temp2, Count:Integer;
  Tag:FileHeader;
  S:TERRAString;
Begin
  Self.UpdateBoundingBox;

  Tag := tagMeshHeader;
  Dest.Write(@Tag, 4);

  For I:=0 To Pred(_GroupCount) Do
  Begin
    Tag := tagMeshGroup;
    Size := 0;
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    Temp := Dest.Position;
    _Groups[I].Save(Dest);
    Temp2 := Dest.Position;
    Size := Temp2-Temp;
    Dest.Seek(Temp - 4);
    Dest.WriteInteger(Size);
    Dest.Seek(Temp2);
  End;

  Tag := tagMeshSkeleton;
  Size := 0;
  Dest.Write(@Tag, 4);
  Dest.WriteInteger(Size);
  Temp := Dest.Position;
  Skeleton.Write(Dest);
  Temp2 := Dest.Position;
  Size := Temp2-Temp;
  Dest.Seek(Temp - 4);
  Dest.WriteInteger(Size);
  Dest.Seek(Temp2);

  For I:=0 To Pred(_MetadataCount) Do
  Begin
    Tag := tagMeshMetadata;
    Size := Succ(Length(_Metadata[I].Name)) + Succ(Length(_Metadata[I].Content)) + SizeOf(Vector3D);
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    Dest.WriteString(_Metadata[I].Name);
    Dest.WriteString(_Metadata[I].Content);
    Dest.Write(@_Metadata[I].Position, SizeOf(Vector3D));
  End;

  For I:=0 To Pred(_EmitterCount) Do
  Begin
    Tag := tagMeshEmitter;
    Size := Succ(Length(_Emitters[I].Name)) + Succ(Length(_Emitters[I].Content))+
           Succ(Length(_Emitters[I].ParentBone)) + SizeOf(Vector3D);
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    Dest.WriteString(_Emitters[I].Name);
    Dest.WriteString(_Emitters[I].Content);
    Dest.WriteString(_Emitters[I].ParentBone);
    Dest.Write(@_Emitters[I].Position, SizeOf(Vector3D));
  End;

  For I:=0 To Pred(_LightCount) Do
  Begin
    Tag := tagMeshLight;
    Size := Succ(Length(_Lights[I].Name)) + 4 + SizeOf(Color)+
           Succ(Length(_Lights[I].ParentBone)) + SizeOf(Vector3D)*4;
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);
    Dest.WriteString(_Lights[I].Name);
    Dest.WriteString(_Lights[I].ParentBone);
    Dest.Write(@_Lights[I].LightType, 1);
    Dest.Write(@_Lights[I].LightColor, 4);
    Dest.Write(@_Lights[I].Position, SizeOf(Vector3D));
    Dest.Write(@_Lights[I].Param1, SizeOf(Vector3D));
    Dest.Write(@_Lights[I].Param2, SizeOf(Vector3D));
    Dest.Write(@_Lights[I].Param3, SizeOf(Vector3D));
  End;

  For I:=0 To Pred(_BoneMorphCount) Do
  Begin
    Count := Skeleton.BoneCount;
    Tag := tagMeshBoneMorph;
    Size := 5 + SizeOf(Vector3D) * Count;
    Dest.Write(@Tag, 4);
    Dest.WriteInteger(Size);

    Dest.Write(@_BoneMorphs[I].MorphID, 4);
    Dest.Write(@_BoneMorphs[I].MorphType, 1);
    For J:=0 To Pred(Count) Do
      Dest.Write(@_BoneMorphs[I].Values[J], SizeOf(Vector3D));
  End;

  Tag := tagMeshEnd;
  Dest.Write(@Tag, 4);

  Result := True;
End;

Function TERRAMesh.Intersect(const R: Ray; var T:Single; Const Transform:Matrix4x4): Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
  If (_Groups[I].Intersect(R,T, Transform)) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Function TERRAMesh.Unload:Boolean;
Begin
  Clean;
  Result := True;
End;


Procedure TERRAMesh.UpdateBoundingBox;
Var
	I:Integer;
Begin
  Log(logDebug, 'Mesh', 'Begining updating bounding box for '+Self.Name);

	For I:=0 To Pred(_GroupCount) Do
		_Groups[I].UpdateBoundingBox;

  _BoundingBox.Reset;
  For I:=0 To Pred(_GroupCount) Do
  Begin
    _BoundingBox.StartVertex.x:=FloatMin(_BoundingBox.StartVertex.x, _Groups[I]._BoundingBox.StartVertex.x);
    _BoundingBox.StartVertex.y:=FloatMin(_BoundingBox.StartVertex.y, _Groups[I]._BoundingBox.StartVertex.y);
    _BoundingBox.StartVertex.z:=FloatMin(_BoundingBox.StartVertex.z, _Groups[I]._BoundingBox.StartVertex.z);
    _BoundingBox.EndVertex.x:=FloatMax(_BoundingBox.EndVertex.x, _Groups[I]._BoundingBox.EndVertex.x);
    _BoundingBox.EndVertex.y:=FloatMax(_BoundingBox.EndVertex.y, _Groups[I]._BoundingBox.EndVertex.y);
    _BoundingBox.EndVertex.z:=FloatMax(_BoundingBox.EndVertex.z, _Groups[I]._BoundingBox.EndVertex.z);
  End;

  Log(logDebug, 'Mesh', 'Finished updating bounding box for '+Self.Name);
End;

Function TERRAMesh.Update:Boolean;
Var
	I:Integer;
Begin
  Inherited Update();

  Log(logDebug, 'Mesh', 'Initializng groups');
	For I:=0 To Pred(_GroupCount) Do
		_Groups[I].Init;

  Log(logDebug, 'Mesh', 'Initializng bround box');
  UpdateBoundingBox();
  Result := True;
End;


Function TERRAMesh.PolyCount:Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_GroupCount) Do
    Inc(Result, _Groups[I].TriangleCount);
End;

Constructor TERRAMesh.CreateFromFilter(Source:MeshFilter);
Var
  I, J, N:Integer;
  Format:VertexFormat;
  S:TERRAString;
  B:MeshBone;
  P:Vector2D;
Begin
  Self.Clean();
  If Source = Nil Then
    Exit;

  _GroupCount := Source.GetGroupCount;
  SetLength(_Groups, _GroupCount);
  For N:=0 To Pred(_GroupCount) Do
  Begin
    Format := Source.GetVertexFormat(N);

    _Groups[N] := MeshGroup.Create(N, Self, Format, Source.GetGroupName(N));
    _Groups[N].Flags := Source.GetGroupFlags(N);
    _Groups[N]._Vertices.Resize(Source.GetVertexCount(N));
    _Groups[N]._TriangleCount := Source.GetTriangleCount(N);
    _Groups[N]._VisibleTriangleCount := _Groups[N]._TriangleCount;

    For I:=0 To Pred(_Groups[N].VertexCount) Do
    Begin
{      _Groups[N]._Vertices.SetVector3D(I, vertexPosition, Source.GetVertexPosition(N, I);

      If (Format And vertexBone<>0) Then
        _Groups[N].GetSingle(I, vertexBone)^ := Source.GetVertexBone(N, I)
      Else
        _Groups[N].GetSingle(I, vertexBone)^ := -1;

      If (Format And vertexNormal<>0) Then
        _Groups[N]._Vertices[I].Normal := Source.GetVertexNormal(N, I);

      If (Format And vertexUV0<>0) Then
        _Groups[N]._Vertices[I].TextureCoords := Source.GetVertexUV(N, I);

      If (Format And vertexUV1<>0) Then
        _Groups[N]._Vertices[I].TextureCoords2 := Source.GetVertexUV2(N, I);

      If (Format And vertexColor<>0) Then
        _Groups[N]._Vertices[I].Color := Source.GetVertexColor(N, I)
      Else
        _Groups[N]._Vertices[I].Color := ColorWhite;

      If (Format And vertexTangent<>0) Then
      Begin
        _Groups[N]._Vertices[I].Tangent := Source.GetVertexTangent(N, I);
        _Groups[N]._Vertices[I].Handness := Source.GetVertexHandness(N, I);
      End;}

      RaiseError('unfinished mesh');
    End;
    SetLength(_Groups[N]._Triangles, _Groups[N]._TriangleCount);
    For I:=0 To Pred(_Groups[N]._TriangleCount) Do
      _Groups[N]._Triangles[I] := Source.GetTriangle(N,I);

    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));
    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));
    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));
    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));
    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));
    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));
    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));
    _Groups[N].DiffuseMap := TextureManager.Instance.GetTexture(Source.GetDiffuseMapName(N));

    _Groups[N]._Material.DiffuseColor := Source.GetDiffuseColor(N);

    If (vertexFormatTangent In Format) Then
      _Groups[N].CalculateTangents;
  End;

  If (Source.GetBoneCount>0) Then
  Begin
    For I:=0 To Pred(Source.GetBoneCount) Do
    Begin
      B := Skeleton.AddBone();
      B.Name := Source.GetBoneName(I);
      B.StartPosition := Source.GetBonePosition(I);
      {$IFNDEF NO_ROTS}
      B.StartRotation := Source.GetBoneRotation(I);
      {$ENDIF}
    End;

    For I:=0 To Pred(Source.GetBoneCount) Do
    Begin
      B := Skeleton.GetBone(I);
      B.Parent := Skeleton.GetBone(Source.GetBoneParent(I));
    End;
  End;

  Self.Update;
End;

Function TERRAMesh.GetEmitter(Index:Integer):MeshEmitter;
Begin
  If (Index<0) Or (Index>=_EmitterCount) Then
    Result := Nil
  Else
    Result := (_Emitters[Index]);
End;

Function TERRAMesh.AddEmitter(Name:TERRAString; Position: Vector3D; Content:TERRAString; ParentBone:TERRAString):MeshEmitter;
Begin
  Result := MeshEmitter.Create(Self);
  Result.Name := Name;
  Result.Content := Content;
  Result.Position := Position;
  Result.ParentBone := ParentBone;
  Result.UpdateBone();

  Inc(_EmitterCount);
  SetLength(_Emitters, _EmitterCount);
  _Emitters[Pred(_EmitterCount)] := Result;
End;

Function TERRAMesh.GetLight(Index:Integer):MeshLight;
Begin
  If (Index<0) Or (Index>=_LightCount) Then
    Result := Nil
  Else
    Result := (_Lights[Index]);
End;

Function TERRAMesh.AddLight(Name:TERRAString; Position:Vector3D; LightType:Integer; LightColor:Color; Param1, Param2, Param3:Vector3D; ParentBone:TERRAString):MeshLight;
Begin
  Result := MeshLight.Create(Self);
  Result.Name := Name;
  Result.LightType := LightType;
  Result.LightColor := LightColor;
  Result.Param1 := Param1;
  Result.Param2 := Param2;
  Result.Param3 := Param3;
  Result.Position := Position;
  Result.ParentBone := ParentBone;
  Result.UpdateBone();

  Inc(_LightCount);
  SetLength(_Lights, _LightCount);
  _Lights[Pred(_LightCount)] := Result;
End;

Function TERRAMesh.AddLight(OtherLight:MeshLight):MeshLight;
Begin
  Result := Self.AddLight(OtherLight.Name, OtherLight.Position, OtherLight.LightType, OtherLight.LightColor, OtherLight.Param1, OtherLight.Param2, OtherLight.Param3, OtherLight.ParentBone);
End;

Procedure TERRAMesh.AddMetadata(Name:TERRAString; Position: Vector3D; Content:TERRAString);
Begin
  Inc(_MetaDataCount);
  SetLength(_Metadata, _MetaDataCount);
  _Metadata[Pred(_MetaDataCount)] := MeshMetadata.Create();
  _Metadata[Pred(_MetaDataCount)].Name := Name;
  _Metadata[Pred(_MetaDataCount)].Position := Position;
  _Metadata[Pred(_MetaDataCount)].Content := Content;
End;

Function TERRAMesh.GetMetadata(Const Name:TERRAString): MeshMetadata;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MetadataCount) Do
  If (StringEquals(_Metadata[I].Name, Name)) Then
  Begin
    Result := _Metadata[I];
    Exit;
  End;

  Result := Nil;
End;

Function TERRAMesh.GetMetadata(Index: Integer): MeshMetadata;
Begin
  If (Index<0) Or (Index>=_MetadataCount) Then
    Result := Nil
  Else
    Result := _Metadata[Index];
End;

Procedure TERRAMesh.Optimize(VertexCacheSize:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
  If (_Groups[I]._TriangleCount>0) Then
    _Groups[I].Optimize(VertexCacheSize);
End;

Procedure TERRAMesh.Transform(const TargetTransform: Matrix4x4);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
    _Groups[I].Transform(TargetTransform);
End;

Function TERRAMesh.GetGroupCount: Integer;
Begin
  If (Self._GroupCount<=0) Then
    Self.Prefetch();

  Result := Self._GroupCount;
End;

Procedure TERRAMesh.Clone(Source:TERRAMesh);
Var
  I,J:Integer;
  T:Triangle;
  Group, OtherGroup:MeshGroup;
Begin
  Source.Prefetch();

  Self.Clean();

  Self._GroupCount := 0;
  For I:=0 To Pred(Source._GroupCount) Do
  Begin
    OtherGroup := Source.GetGroup(I);
    Group := Self.AddGroup(OtherGroup._Vertices.Format, OtherGroup._Name);
    Group._Vertices.Resize(OtherGroup.VertexCount);
    Group._Vertices.CopyBuffer(OtherGroup._Vertices);
    Group._GPUSkinning := OtherGroup._GPUSkinning;
    Group._NeedsTangentSetup := False;
    Group._Material := OtherGroup._Material.Clone();
    Group._Shader := Nil;
    Group._BoundingBox := OtherGroup._BoundingBox;

    Group._TriangleCount := OtherGroup._TriangleCount;
    SetLength(Group._Triangles, OtherGroup._TriangleCount);
    For J:=0 To Pred(OtherGroup._TriangleCount) Do
      Group._Triangles[J] := OtherGroup.GetTriangle(J);

    Group._MorphCount := OtherGroup._MorphCount;
    SetLength(Group._Morphs, Group._MorphCount);
    For J:=0 To Pred(Group._MorphCount) Do
      Group._Morphs[J] := OtherGroup._Morphs[J];

    Group._VisibleTriangleCount := OtherGroup._VisibleTriangleCount;
  End;

  Self._BoundingBox := Source._BoundingBox;

  Self._Skeleton := MeshSkeleton.Create();
  Self._Skeleton.Clone(Source.Skeleton);

  Self._AlphaStage := Source._AlphaStage;
  Self._BoneMorphCount := Source._BoneMorphCount;
  SetLength(Self._BoneMorphs, Source._BoneMorphCount);
  For J:=0 To Pred(Source._BoneMorphCount) Do
    Self._BoneMorphs[J] := Source._BoneMorphs[J];

  Self._Skinning := Source._Skinning;
  Self._NormalMapping := Source._NormalMapping;

  Self._MetadataCount := 0;
  For J:=0 To Pred(Source._MetadataCount) Do
    Self.AddMetadata(Source._Metadata[J].Name, Source._Metadata[J].Position, Source._Metadata[J].Content);

  Self._EmitterCount := 0;
  For J :=0 To Pred(Source._EmitterCount) Do
    Self.AddEmitter(Source._Emitters[J].Name, Source._Emitters[J].Position, Source._Emitters[J].Content, Source._Emitters[J].ParentBone);

  Self._LightCount := 0;
  For J:=0 To Pred(Source._LightCount) Do
    Self.AddLight(Source._Lights[J].Name, Source._Lights[J].Position, Source._Lights[J].LightType, Source._Lights[J].LightColor, Source._Lights[J].Param1, Source._Lights[J].Param2, Source._Lights[J].Param3, Source._Lights[J].ParentBone);
                                      
  Self._Time := Application.GetTime();
  Self.SetStatus(rsReady);
End;

Function TERRAMesh.AddBoneMorph(MorphID: Integer):Integer;
Begin
  Result := Self._BoneMorphCount;
  Inc(_BoneMorphCount);
  SetLength(_BoneMorphs, _BoneMorphCount);
  _BoneMorphs[Result].MorphID := MorphID;
  _BoneMorphs[Result].MorphType := 0;
  SetLength(_BoneMorphs[Result].Values, Self.Skeleton.BoneCount);
End;

Procedure TERRAMesh.SetBoneMorph(MorphID, BoneID:Integer; Const Value:Vector3D);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BoneMorphCount) Do
  If (_BoneMorphs[I].MorphID = MorphID) Then
  Begin
  	If (BoneID>=0) And (BoneID<Length(_BoneMorphs[I].Values)) Then
	    _BoneMorphs[I].Values[BoneID] := Value;
    Exit;
  End;
End;

Function TERRAMesh.GetBoneMorph(MorphID, BoneID: Integer): Vector3D;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BoneMorphCount) Do
  If (_BoneMorphs[I].MorphID = MorphID) Then
  Begin
    Result := _BoneMorphs[I].Values[BoneID];
    Exit;
  End;

  Result := VectorZero;
End;

Function TERRAMesh.HasBoneMorph(MorphID: Integer): Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BoneMorphCount) Do
  If (_BoneMorphs[I].MorphID = MorphID) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Procedure TERRAMesh.CullTriangles(Box: BoundingBox; Transform:Matrix4x4);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
  If (_Groups[I]._TriangleCount>0) Then
    _Groups[I].CullTriangles(Box, Transform);
End;

Procedure TERRAMesh.UncullTriangles;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
    _Groups[I].UncullTriangles();
End;

Procedure TERRAMesh.Release;
Begin
  ReleaseObject(_Filter);

  Inherited;
End;

Function TERRAMesh.GetMeshFilter: MeshFilter;
Begin
  If (_Filter = Nil) Then
  Begin
    _Filter := CustomMeshFilter.Create;
    CustomMeshFilter(_Filter)._Mesh := Self;
  End;

  Result := _Filter;
End;


{ CustomMeshFilter }
Procedure CustomMeshFilter.AddAnimation(Anim: Animation);
Begin
  Inc(_AnimationCount);
  SetLength(_Animations, _AnimationCount);
  _Animations[Pred(_AnimationCount)] := Anim;
End;

Function CustomMeshFilter.GetAnimationCount:Integer;
Begin
  Result := _AnimationCount;
End;

Function CustomMeshFilter.GetAnimationFrameRate(AnimationID: Integer): Single;
Begin
  If (AnimationID>=0) And (AnimationID<_AnimationCount) Then
    Result := _Animations[AnimationID].FPS
  Else
    Result := 0.0;
End;

Function CustomMeshFilter.GetAnimationLoop(AnimationID: Integer): Boolean;
Begin
  If (AnimationID>=0) And (AnimationID<_AnimationCount) Then
    Result := _Animations[AnimationID].Loop
  Else
    Result := False;
End;

Function CustomMeshFilter.GetAnimationName(AnimationID: Integer):TERRAString;
Begin
  If (AnimationID>=0) And (AnimationID<_AnimationCount) Then
  Begin
    Result := _Animations[AnimationID].Name;
    Result := Copy(Result, Pos('_', Result)+1, MaxInt);
  End Else
    Result := '';
End;

Function CustomMeshFilter.GetBoneCount: Integer;
Begin
  Result := _Mesh.Skeleton.BoneCount;
End;

Function CustomMeshFilter.GetBoneName(BoneID: Integer):TERRAString;
Begin
  Result := _Mesh.Skeleton.GetBone(BoneID).Name;
End;

Function CustomMeshFilter.GetBoneParent(BoneID: Integer): Integer;
Begin
  If Assigned(_Mesh.Skeleton.GetBone(BoneID).Parent) Then
    Result := _Mesh.Skeleton.GetBone(BoneID).Parent.Index
  Else
    Result := -1;
End;

Function CustomMeshFilter.GetBonePosition(BoneID: Integer): Vector3D;
Begin
  Result := _Mesh.Skeleton.GetBone(BoneID).StartPosition;
End;

Function CustomMeshFilter.GetBoneRotation(BoneID: Integer): Vector3D;
Begin
{$IFNDEF NO_ROTS}
  Result := _Mesh.Skeleton.GetBone(BoneID).StartRotation;
{$ELSE}
  Result := VectorZero;
{$ENDIF}
End;

Function CustomMeshFilter.GetDiffuseColor(GroupID: Integer): Color;
Begin
  Result := _Mesh._Groups[GroupID]._Material.DiffuseColor;
End;

Function CustomMeshFilter.GetDiffuseMapName(GroupID: Integer):TERRAString;
Begin
  If Assigned(_Mesh._Groups[GroupID]._Material.DiffuseMap) Then
  Begin
    Result := _Mesh._Groups[GroupID]._Material.DiffuseMap.Name;
    Result := Copy(Result, Succ(Pos('.', Result)), MaxInt) + '.png';
  End Else
    Result := '';
End;

Function CustomMeshFilter.GetGroupCount: Integer;
Begin
  Result := _Mesh._GroupCount;
End;

Function CustomMeshFilter.GetGroupFlags(GroupID: Integer): Cardinal;
Begin
  Result := _Mesh._Groups[GroupID].Flags;
End;

Function CustomMeshFilter.GetGroupName(GroupID: Integer):TERRAString;
Begin
  Result := _Mesh._Groups[GroupID].Name;
End;

Function CustomMeshFilter.GetPositionKey(AnimationID, BoneID, KeyID: Integer): MeshVectorKey;
Var
  Bone:BoneAnimation;
Begin
  If (AnimationID>=0) And (AnimationID<_AnimationCount) Then
  Begin
    Bone := _Animations[AnimationID].GetBone(BoneID);
    Result.Value := Bone.Positions.Keyframes[KeyID].Value;
    Result.Time := Bone.Positions.Keyframes[KeyID].Time;
  End Else
  	FillChar(Result, SizeOf(Result), 0);
End;

Function CustomMeshFilter.GetPositionKeyCount(AnimationID, BoneID: Integer): Integer;
Var
  Bone:BoneAnimation;
Begin
  If (AnimationID>=0) And (AnimationID<_AnimationCount) Then
  Begin
    Bone := _Animations[AnimationID].GetBone(BoneID);
    Result := Bone.Positions.Count
  End Else
    Result := 0;
End;

Function CustomMeshFilter.GetRotationKey(AnimationID, BoneID, KeyID: Integer): MeshVectorKey;
Var
  Bone:BoneAnimation;
Begin
  If (AnimationID>=0) And (AnimationID<_AnimationCount) Then
  Begin
    Bone := _Animations[AnimationID].GetBone(BoneID);
    Result.Value := Bone.Rotations.Keyframes[KeyID].Value;
    Result.Time := Bone.Rotations.Keyframes[KeyID].Time;
  End Else
  	FillChar(Result, SizeOf(Result), 0);
End;

Function CustomMeshFilter.GetRotationKeyCount(AnimationID, BoneID: Integer): Integer;
Var
  Bone:BoneAnimation;
Begin
  If (AnimationID>=0) And (AnimationID<_AnimationCount) Then
  Begin
    Bone := _Animations[AnimationID].GetBone(BoneID);
    Result := Bone.Rotations.Count;
  End Else
    Result := 0;
End;

Function CustomMeshFilter.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _Mesh._Groups[GroupID].GetTriangle(Index);
End;

Function CustomMeshFilter.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := _Mesh._Groups[GroupID].TriangleCount;
End;

Function CustomMeshFilter.GetVertexBone(GroupID, Index: Integer): Integer;
Var
  BoneIndex:Single;
Begin
  _Mesh._Groups[GroupID].Vertices.GetFloat(Index, vertexBone, BoneIndex);
  Result := Trunc(BoneIndex);
End;

Function CustomMeshFilter.GetVertexColor(GroupID, Index: Integer): Color;
Begin
  _Mesh._Groups[GroupID].Vertices.GetColor(Index, vertexColor, Result);
End;

Function CustomMeshFilter.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _Mesh._Groups[GroupID].VertexCount;
End;

Function CustomMeshFilter.GetVertexFormat(GroupID: Integer):VertexFormat;
Begin
  Result := _Mesh._Groups[GroupID].Vertices.Format;
End;

Function CustomMeshFilter.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  _Mesh._Groups[GroupID].Vertices.GetVector3D(Index, vertexNormal, Result);
End;

Function CustomMeshFilter.GetVertexPosition(GroupID,Index: Integer): Vector3D;
Begin
  _Mesh._Groups[GroupID].Vertices.GetVector3D(Index, vertexPosition, Result);
End;

Function CustomMeshFilter.GetVertexTangent(GroupID, Index: Integer): Vector4D;
Begin
  _Mesh._Groups[GroupID].Vertices.GetVector4D(Index, vertexTangent, Result);
End;

Function CustomMeshFilter.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  _Mesh._Groups[GroupID].Vertices.GetVector2D(Index, vertexUV0, Result);
  Result.Y := 1 - Result.Y;
End;

Function CustomMeshFilter.GetVertexUV2(GroupID, Index: Integer): Vector2D;
Begin
  _Mesh._Groups[GroupID].Vertices.GetVector2D(Index, vertexUV1, Result);
End;

{ MeshMerger }
Procedure MeshMerger.Release;
Begin
  // do nothing
End;

Function MeshMerger.Merge(Source, Dest:TERRAMesh; DestFormat:VertexFormat; IndividualGroup: Boolean; MaxVertsPerGroup: Integer; UpdateBox:Boolean): IntegerArrayObject;
Var
  I, J, Init:Integer;
  Target, SourceGroup:MeshGroup;
Begin
  Result.Items := Nil;
  Result.Count := 0;

  If (Source = Nil) Or (Dest =Nil) Then
  Begin
    Exit;
  End;

  Log(logDebug, 'Mesh', 'Beginning merging '+Source.Name+' into '+Dest.Name);

  Source.Prefetch();

  Dest.Status := rsReady;

  Init := Dest.PolyCount;

  If (MaxVertsPerGroup<=0) Then
    MaxVertsPerGroup := 32000;

  For J:=0 To Pred(Source.GroupCount) Do
  Begin
    Target := Nil;
    SourceGroup := Source.GetGroup(J);
    If (SourceGroup=Nil) Then
    Begin
      StringToInt(Source.Name);
      Continue;
    End;
    
    If Not IndividualGroup Then
    For I:=0 To Pred(Dest.GroupCount) Do
    If (Not Dest._Groups[I]._Unique)
    And (Dest._Groups[I].Flags = SourceGroup.Flags)
    And (Dest._Groups[I].Userdata = SourceGroup.Userdata)
    And (Dest._Groups[I]._Material.Equals(SourceGroup._Material))
    And (Dest._Groups[I].VertexCount + SourceGroup.VertexCount<MaxVertsPerGroup) Then
    Begin
      Target := Dest._Groups[I];
      Break;
    End;

    If Not Assigned(Target) Then
    Begin
      Target := Dest.AddGroup(DestFormat, SourceGroup._Name);
      Target.Flags := SourceGroup.Flags;
      Target.UserData := SourceGroup.UserData;
      Target._Unique := IndividualGroup;
      Target._Material := SourceGroup._Material;

      Self.ProcessGroup(Target);
    End;

    MergeGroup(Source._Groups[J], Target, UpdateBox);
    Result.Add(Target._ID);
  End;

  Log(logDebug, 'Mesh', 'Finished merging '+Source.Name+' into '+Dest.Name);
  Log(logDebug, 'Mesh', IntToString(Dest.PolyCount-Init)+' triangles added!');
End;

Procedure MeshMerger.MergeGroup(Source, Dest:MeshGroup; UpdateBox:Boolean = True);
Var
  VOfs, TOfs:Cardinal;
  DestVertex:VertexData;
  SourceTriangle, DestTriangle:PTriangle;
  I, N:Integer;
  SrcIt, DestIt:VertexIterator;
Begin
  If (Source = Nil) Or (Dest =  Nil) Then
  Begin
    IntToString(2);
    Exit;
  End;

  If Assigned(Dest._Owner) Then
    Dest._Owner.Status := rsReady;

  Log(logDebug, 'Mesh', 'Beginning group '+Source._Owner.Name+'.'+ Source.Name+' into '+Dest._Owner.Name+'.'+ Dest.Name);

  VOfs := Dest.VertexCount;
  TOfs := Dest._TriangleCount;
  Dest.Vertices.Resize(Dest.VertexCount + Source.VertexCount);
  Inc(Dest._TriangleCount, Source._TriangleCount);
  Dest._VisibleTriangleCount := Dest._TriangleCount;

  If (Length(Dest._Triangles)<Dest._TriangleCount) Then
  Begin
    If (Length(Dest._Triangles)<=0) Then
    Begin
      If (Dest._TriangleCount>=256) Then
        N := Dest._TriangleCount
      Else
        N := 256;
    End Else
      N := Length(Dest._Triangles) * 2;

    SetLength(Dest._Triangles, N);
    SetLength(Dest._TriangleNormals, N);
  End;

  DestIt := Dest.Vertices.GetIteratorForClass(MeshVertex);
  DestIt.Seek(VOfs);

  SrcIt := Source.Vertices.GetIteratorForClass(MeshVertex);
  While (SrcIt.HasNext()) And (DestIt.HasNext()) Do
  Begin
    Self.ProcessVertex(MeshVertex(SrcIt.Value), MeshVertex(DestIt.Value), Source, Dest);
  End;
  ReleaseObject(DestIt);
  ReleaseObject(SrcIt);

  For I:=0 To Pred(Source._TriangleCount) Do
  Begin
    SourceTriangle := Source.GetTrianglePointer(I);
    DestTriangle := Dest.GetTrianglePointer(I + TOfs);

    //_TriangleNormals[TOfs + I] := Source._TriangleNormals[I];
    If (Assigned(SourceTriangle)) And (Assigned(DestTriangle)) Then
    Begin
      DestTriangle.Indices[0] := SourceTriangle.Indices[0] + VOfs;
      DestTriangle.Indices[1] := SourceTriangle.Indices[1] + VOfs;
      DestTriangle.Indices[2] := SourceTriangle.Indices[2] + VOfs;
      Self.ProcessTriangle(DestTriangle, Source, Dest);
    End;
  End;

  If UpdateBox Then
    Dest.UpdateBoundingBox();
End;

Procedure MeshMerger.ProcessGroup(Group: MeshGroup); Begin End;
Procedure MeshMerger.ProcessTriangle(T: PTriangle; Source, Dest:MeshGroup); Begin End;

Procedure MeshMerger.ProcessVertex(SourceVertex, DestVertex:MeshVertex; SourceGroup, DestGroup:MeshGroup);
Begin
  DestVertex.Position := SourceVertex.Position;
  DestVertex.Normal := SourceVertex.Normal;
  DestVertex.Tangent := SourceVertex.Tangent;
  DestVertex.UV0 := SourceVertex.UV0;
  DestVertex.UV1 := SourceVertex.UV1;
  DestVertex.BaseColor := SourceVertex.BaseColor;
  DestVertex.BoneIndex := SourceVertex.BoneIndex;
  DestVertex.HueShift := SourceVertex.HueShift;
End;

{ MeshEmitter }
Constructor MeshEmitter.Create(Owner: TERRAMesh);
Begin
  Self.Owner := Owner;
  Self.BoneIndex := -1;
End;

Procedure MeshEmitter.UpdateBone;
Var
  Bone:MeshBone;
Begin
  Bone := Owner.Skeleton.GetBone(Self.ParentBone);
  If Assigned(Bone) Then
    Self.BoneIndex := Bone.Index
  Else
    Self.BoneIndex := -1;
End;

{ MeshLight }
Constructor MeshLight.Create(Owner: TERRAMesh);
Begin
  Self.GroupIndex := -1;
  Self.Owner := Owner;
End;

Procedure MeshLight.UpdateBone;
Var
  Bone:MeshBone;
Begin
  Bone := Owner.Skeleton.GetBone(Self.ParentBone);
  If Assigned(Bone) Then
    Self.BoneIndex := Bone.Index
  Else
    Self.BoneIndex := -1;
End;

{ MeshMaterial }
Function MeshMaterial.Clone: MeshMaterial;
Begin
  Result := Self;
End;

Function MeshMaterial.Equals(const Other: MeshMaterial): Boolean;
Begin
  Result :=
       (Self.DiffuseMap = Other.DiffuseMap)
    And (Self.TriplanarMap = Other.TriplanarMap)
    And (Self.DecalMap = Other.DecalMap)
    And (Self.NormalMap = Other.NormalMap)
    And (Self.DisplacementMap = Other.DisplacementMap)
    And (Self.SpecularMap = Other.SpecularMap)
    And (Self.GlowMap = Other.GlowMap)
    And (Self.RefractionMap = Other.RefractionMap)
    And (Self.AlphaMap = Other.AlphaMap)
    And (Self.LightMap = Other.LightMap)
    And (Self.DitherPatternMap = Other.DitherPatternMap)
    And (Self.ToonRamp = Other.ToonRamp)
    And (Self.ColorTable = Other.ColorTable)
    And (Self.BlendMode = Other.BlendMode)
    And (Cardinal(Self.DiffuseColor) = Cardinal(Other.DiffuseColor))
    //And (Cardinal(Self.AmbientColor) = Cardinal(Other.AmbientColor))
    And (Cardinal(Self.OutlineColor) = Cardinal(Other.OutlineColor))
    ;
End;

Procedure MeshMaterial.Reset;
Begin
  BlendMode := -1;
  HueShift := 0;

//  AmbientColor := ColorWhite;
  DiffuseColor := ColorWhite;
  OutlineColor := ColorNull;
  ShadowColor := ColorNull;

  DiffuseMap := Nil;
  TriplanarMap := Nil;
  DecalMap := Nil;
  NormalMap := Nil;
  DisplacementMap := Nil;
  SpecularMap := Nil;
  GlowMap := Nil;
  RefractionMap := Nil;
  AlphaMap := Nil;
  LightMap := Nil;
  DitherPatternMap := Nil;

  ToonRamp := Nil;
  ColorTable := Nil;

  VegetationBend := 0.5;
End;

{ MeshParticleEmitter }
Constructor MeshParticleEmitter.Create(Const FXName:TERRAString; Target: MeshGroup);
Begin
  Self._TargetGroup := Target;
  Inherited Create(FXName, VectorZero);
End;

Procedure MeshParticleEmitter.Emit(Target:Particle);
Var
  N:Integer;
  P:Vector3D;
Begin
  N := Random(_TargetGroup.VertexCount);
  //N := GetTime Mod Group._VertexCount;
  _TargetGroup.Vertices.GetVector3D(N, vertexPosition, P);
  Self.Position := P;
  //Target.SpawnWithDirection(Group._Vertices[N].Normal);

  Inherited Emit(Target);
End;

Function MakeWaterFlowBounds(Const Box:BoundingBox):Vector4D;
Begin
  Result.X := Box.StartVertex.X;
  Result.Y := Box.StartVertex.Z;

  Result.Z := SafeDiv(1.0, (Box.EndVertex.X - Box.StartVertex.X), 1.0);
  Result.W := SafeDiv(1.0, (Box.EndVertex.Z - Box.StartVertex.Z), 1.0);
End;

{ SelectMeshShader }
Function SelectMeshShader(View:TERRAViewport; Group:MeshGroup; Position:Vector3D; Outline, TranslucentPass:Boolean; Var DestMaterial:MeshMaterial; UseTextureMatrix:Boolean):ShaderInterface;
Var
  DisableLights:Boolean;
  LightPivot:Vector3D;
  RenderStage:Integer;
  FxFlags, OutFlags:Cardinal;
  Graphics:GraphicsManager;
Begin
  FxFlags := shaderVertexColor;
  OutFlags := 0;
  Group._LightBatch.Reset();
  DisableLights := False;

  Graphics := GraphicsManager.Instance;

  If (Group.Flags And meshGroupVegetation<>0) And (DestMaterial.VegetationBend>0) Then
    FxFlags := FxFlags Or shaderVegetation;

  Group._Owner._Skinning := (Assigned(Group._Owner.Skeleton)) And (Group._Owner.Skeleton.BoneCount>0) And (Group.Vertices.HasAttribute(vertexBone));
  Group._Owner._NormalMapping := (Assigned(DestMaterial.NormalMap)) And (Graphics.Renderer.Settings.NormalMapping.Enabled);

(*  If (Group.AmbientColor.R = 0) And (Group.AmbientColor.G = 0) And (Group.AmbientColor.B=0) Then
    FxFlags := FxFlags Or shaderSkipAmbient;*)

  RenderStage := Graphics.RenderStage;

  If (View.Camera.UseClipPlane) Then
    FxFlags := FxFlags Or shaderClipPlane;

  If (Group.Flags And meshGroupStencilMask<>0) {$IFDEF REFLECTIONS_WITH_STENCIL} Or (Graphics.ReflectionStencil) {$ENDIF}  Then
  Begin
    DisableLights := True;
    OutFlags := shader_OutputFixedColor;
  End Else
  If RenderStage = renderStageOutline Then
  Begin
    DisableLights := True;
    OutFlags := OutFlags Or shader_OutputFixedColor;
  End Else
  If RenderStage = renderStageNormal Then
  Begin
    DisableLights := True;
    OutFlags := OutFlags Or shader_OutputNormal;
  End Else
  If RenderStage = renderStageRefraction Then
  Begin
    DisableLights := True;
    OutFlags := OutFlags Or shader_OutputRefraction
  End Else
  If RenderStage = renderStageGlow Then
  Begin
    DisableLights := True;
    OutFlags := OutFlags Or shader_OutputGlow;
  End Else
  If RenderStage = renderStageShadow Then
  Begin
    DisableLights := True;

    OutFlags := OutFlags Or shader_OutputShadow;

    If (Group.Flags And meshGroupLightmap<>0) Then
    Begin
      //FxFlags := FxFlags Or shaderAddSigned;

      If Assigned(DestMaterial.LightMap) Then
        FxFlags := FxFlags Or shaderLightmap;
    End;
  End Else
  If RenderStage = renderStageReflection Then
  Begin
    DisableLights := True;
    OutFlags := OutFlags Or shader_OutputReflection;

    If (Group.Flags And meshGroupWaterMap<>0) Then
    Begin
      {$IFDEF HIGH_QUALITY_WATER}
      FxFlags := FxFlags Or shaderFresnelTerm;
      {$ENDIF}

      If (DestMaterial.NormalMap <> Nil) Then
      Begin
        FxFlags := FxFlags Or shaderNormalMap;
      End;
    End;
  End Else
  If RenderStage = renderStageDiffuse Then
  Begin
    OutFlags := OutFlags Or shader_OutputDiffuse;

    If Graphics.Renderer.Settings.CartoonHues.Enabled Then
      FxFlags := FxFlags Or shaderCartoonHue;

    If (Graphics.Renderer.Settings.Specular.Enabled) Then
      FxFlags := FxFlags Or shaderSpecular;

    If (Assigned(DestMaterial.ToonRamp)) Then
      FxFlags := FxFlags Or shaderToonRamp;

    If Assigned(DestMaterial.DecalMap) Then
      FxFlags := FxFlags Or shaderDecalMap;

    (*If (Assigned(DestMaterial.DitherPatternMap)) Then
    Begin
      FxFlags := FxFlags Or shaderDitherColor;
    End;*)

    If (Graphics.Renderer.Settings.DynamicShadows.Enabled) Then
      FxFlags := FxFlags Or shaderShadowMap;

    If (Group.Flags And meshGroupAlphaMap<>0) Then
    Begin
      FxFlags := FxFlags Or shaderAlphamap;
    End;

    If (Group.Flags And meshGroupLightmap<>0) Then
    Begin
      FxFlags := FxFlags Or shaderSelfIllumn;
      DisableLights := True;
    End Else
    Begin
      If (Not DisableLights) And (Group.Flags And meshGroupLightOff=0) Then
      Begin
        (*If (Group._BoundingBox.Radius>50) Then
          LightPivot := Graphics.ActiveViewport.Camera.Position
        Else
          LightPivot := Position;    *)
        LightPivot := View.Camera.FocusPoint;

        LightManager.Instance.SortLights(LightPivot, Group._BoundingBox, Group._LightBatch);
      End;
    End;

    {$IFNDEF DISABLECOLORGRADING}
    If Assigned(DestMaterial.ColorTable) Then
      FxFlags := FxFlags Or shaderColorTable;
    {$ENDIF}

{$IFDEF ADVANCED_ALPHA_BLEND}
    If Not TranslucentPass Then
      OutFlags := OutFlags Or shader_OutputFixedColor;
{$ENDIF}

    If (Group.Flags And meshGroupSphereMap<>0) Then
      FxFlags := FxFlags Or shaderSphereMap;

    If (Group.Flags And meshGroupTriplanar<>0) Then
      FxFlags := FxFlags Or shaderTriplanar;

    If (Group.Flags And meshGroupWireframe<>0) Then
      FxFlags := FxFlags Or shaderWireframe;

    //DestMaterial.HueShift := 0.0;
    If ((DestMaterial.HueShift<>0.0) Or (Group.Flags And meshGroupHueShift<>0)) Then
    Begin
      Group.Vertices.AddAttribute(vertexFormatHue);
      Group.ReleaseBuffer();
      FxFlags := FxFlags Or shaderHueChange;
    End;

    If (DestMaterial.ReflectiveMap <> Nil) Then
      FxFlags := FxFlags Or shaderSphereMap Or shaderReflectiveMap;

    If (Group.Flags And meshGroupWaterMap<>0) Then
    Begin
      {$IFDEF HIGH_QUALITY_WATER}
      FxFlags := FxFlags Or shaderFresnelTerm;
      FxFlags := FxFlags Or shaderSphereMap;
      {$ENDIF}

      If (DestMaterial.NormalMap <> Nil) Then
       FxFlags := FxFlags Or shaderNormalMap;
    End;

    If (Not DisableLights) And (Group._LightBatch.DirectionalLightCount<=0) Then
      FxFlags := FxFlags Or shaderSelfIllumn;

    If (DestMaterial.Ghost) Then
      FxFlags := FxFlags Or shaderGhost;
  End;

  If (Group.Flags And meshGroupLightmap<>0) And (FxFlags And shaderVertexColor<>0) And (Assigned(DestMaterial.LightMap)) Then
  Begin
    FxFlags := FxFlags Xor shaderVertexColor;
  End;

  If (Group._Owner._NormalMapping) And ((RenderStage = renderStageNormal) Or (RenderStage = renderStageDiffuse)) Then
  Begin
    FxFlags := FxFlags Or shaderNormalMap;
    {$IFDEF PC}
    If Assigned(DestMaterial.DisplacementMap) Then
      FxFlags := FxFlags Or shaderParallaxBump;
    {$ENDIF};
  End;

  If (Group._Owner._Skinning) And (Group._GPUSkinning) Then
    FxFlags := FxFlags Or shaderSkinning;

  If (Group.Flags And meshGroupColorOff<>0) Then
  Begin
    FxFlags := shaderColorOff;
  End;

  If RenderStage = renderStageDiffuse Then
  Begin
    //FxFlags := FxFlags Or shaderAddSigned;

    If (Group._LightBatch.AmbientColor.A>0) Then
      FxFlags := FxFlags Or shaderAmbientColor;
  End;

  If (Group.Flags And meshGroupWaterMap<>0) And (DestMaterial.FlowMap <> Nil) Then
  Begin
    FxFlags := FxFlags Or shaderFlowMap;

    If (DestMaterial.NoiseMap <> Nil) Then
    Begin
      FxFlags := FxFlags Or shaderNoiseMap;
    End;
  End;

  If (Outline) Then
  Begin
    DisableLights := True;
    FxFlags := FxFlags Or shaderAlphaTest;
    OutFlags := OutFlags Or shader_OutputOutline;
  End;

(*  If (StringContains('monster', DestMaterial.DiffuseMap.Name)) Then
    IntToString(2);*)

  If (Graphics.Renderer.Settings.AlphaTesting.Enabled) And (IsImageTranslucent(DestMaterial.DiffuseMap))
  And ((Group.Flags And meshGroupShadowOnly)=0)  Then
    FxFlags := FxFlags Or shaderAlphaTest;

  If (Graphics.ReflectionActive) Then
    FxFlags := FxFlags Or shaderScreenMask;

  If (UseTextureMatrix) And (FxFlags And shaderFlowMap = 0) Then
    FxFlags := FxFlags Or shaderTextureMatrix;

//  Flags := shaderOutputNormal;

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Getting shader with flags '+CardinalToString(FXFlags));{$ENDIF}

  DestMaterial.AmbientColor := Group._LightBatch.AmbientColor;
  Result := ShaderFactory.Instance.GetShader(FxFlags, OutFlags, Graphics.Renderer.Settings.FogMode, Group._LightBatch);
End;

{ MeshFX }
Procedure MeshFX.SetCallback(Callback: MeshFXCallback; UserData: Pointer);
Begin
  Self._Callback := Callback;
  Self._UserData := UserData;
End;

{ MeshVertex }
Procedure MeshVertex.Load;
Var
  N:Single;
Begin
  Self.GetVector3D(vertexPosition, Self.Position);
  Self.GetVector3D(vertexNormal, Self.Normal);
  Self.GetVector4D(vertexTangent, Self.Tangent);
  Self.GetVector2D(vertexUV0, Self.UV0);
  Self.GetVector2D(vertexUV1, Self.UV1);
  Self.GetFloat(vertexHue, Self.HueShift);

  Self.GetColor(vertexColor, Self.BaseColor);

  Self.GetFloat(vertexBone, N);
  If (N<0) Or (N>MaxBones) Then
    N := 0;

  Self.BoneIndex := Trunc(N);
End;

Procedure MeshVertex.Save;
Begin
  Self.SetVector3D(vertexPosition, Self.Position);
  Self.SetVector3D(vertexNormal, Self.Normal);
  Self.SetVector4D(vertexTangent, Self.Tangent);
  Self.SetVector2D(vertexUV0, Self.UV0);
  Self.SetVector2D(vertexUV1, Self.UV1);
  Self.SetFloat(vertexHue, Self.HueShift);
  Self.SetColor(vertexColor, Self.BaseColor);
  Self.SetFloat(vertexBone, Self.BoneIndex);
End;

Initialization
  Log(logDebug, 'Mesh', 'Initializing');

  RegisterMeshDataHandler(tagMeshGroup, MeshReadGroup);
  RegisterMeshDataHandler(tagMeshSkeleton, MeshReadSkeleton);
  RegisterMeshDataHandler(tagMeshEmitter, MeshReadEmitter);
  RegisterMeshDataHandler(tagMeshLight, MeshReadLights);
  RegisterMeshDataHandler(tagMeshMetadata, MeshReadMeta);
  RegisterMeshDataHandler(tagMeshBoneMorph, MeshReadBoneMorph);

  RegisterMeshGroupHandler(tagVertexData, GroupReadVertexData);
  RegisterMeshGroupHandler(tagVertexMorph, GroupReadVertexMorphs);
//  RegisterMeshGroupHandler(tagVertexBoneWeights, GroupReadVertexBoneWeights);
  RegisterMeshGroupHandler(tagTriangleIndices, GroupReadTriangleIndices);
  RegisterMeshGroupHandler(tagTriangleNormals, GroupReadTriangleNormals);
  RegisterMeshGroupHandler(tagTriangleEdges, GroupReadTriangleEdges);
  RegisterMeshGroupHandler(tagMaterialDiffuse, GroupReadMaterialDiffuse);
  RegisterMeshGroupHandler(tagMaterialTriplanar, GroupReadMaterialTriplanar);
  RegisterMeshGroupHandler(tagMaterialSpecular, GroupReadMaterialSpecular);
  RegisterMeshGroupHandler(tagMaterialBump, GroupReadMaterialBump);
  RegisterMeshGroupHandler(tagMaterialDisplacement, GroupReadMaterialDisplacement);
  RegisterMeshGroupHandler(tagMaterialLightMap, GroupReadMaterialLightmap);
  RegisterMeshGroupHandler(tagMaterialRefraction, GroupReadMaterialRefraction);
  RegisterMeshGroupHandler(tagMaterialReflective, GroupReadMaterialReflective);
  RegisterMeshGroupHandler(tagMaterialEnvMap, GroupReadMaterialEnvMap);
  RegisterMeshGroupHandler(tagMaterialGlow, GroupReadMaterialGlow);
  RegisterMeshGroupHandler(tagMaterialAlphaMap, GroupReadMaterialAlphaMap);
  RegisterMeshGroupHandler(tagMaterialRamp, GroupReadMaterialRamp);
  RegisterMeshGroupHandler(tagMaterialBlendMode, GroupReadMaterialBlendMode);
  RegisterMeshGroupHandler(tagMaterialParticles, GroupReadMaterialParticles);
Finalization
  ReleaseObject(_MeshManager);
End.


