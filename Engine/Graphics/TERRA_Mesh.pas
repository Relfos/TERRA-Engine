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
  TERRA_Utils, TERRA_Texture, TERRA_IO, TERRA_Resource, TERRA_MeshAnimation,
  TERRA_VertexBufferObject, TERRA_ResourceManager, TERRA_FileUtils, TERRA_Vector4D,
  TERRA_Math, TERRA_Ray, TERRA_Collections, TERRA_ShadowVolumes, TERRA_GraphicsManager, TERRA_MeshFilter,
  TERRA_BoundingBox, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_RenderTarget,
  TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ParticleRenderer, TERRA_ParticleEmitters, TERRA_Lights, TERRA_Shader
//  {$IFDEF PC}, TERRA_Fur, TERRA_Cloth{$ENDIF}
;

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
  tagVertexPositions    = 'VPOS';
  tagVertexNormals      = 'VNRM';
  tagVertexUVs0         = 'VUV0';
  tagVertexUVs1         = 'VUV1';
  tagVertexTangents     = 'VTAN';
  tagVertexColors       = 'VCLR';
  tagVertexBoneIndices  = 'VBIX';
  tagVertexBoneWeights  = 'VBWE';
  tagVertexMorph        = 'VMRP';
  tagVertexLinks        = 'VLNK';
  tagTriangleIndices    = 'TIDX';
  tagTriangleNormals    = 'TNRM';
  tagTriangleEdges      = 'TEDG';
  tagMaterialDiffuse    = 'MDIF';
  tagMaterialTriplanar  = 'MTRP';
  tagMaterialSpecular   = 'MSPC';
  tagMaterialBump       = 'MNRM';
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
  Mesh = Class;

  PMeshAttach = ^MeshAttach;
  MeshAttach = Record
    AttachMesh:Mesh;
    BoneIndex:Integer;
    Matrix:Matrix4x4;
    Color:TERRA_Color.Color;
    IsStencil:Boolean;
  End;

  MeshEmitter = Class(TERRAObject)
    Name:AnsiString;
    Content:AnsiString;
    Position:Vector3D;
    BoneIndex:Integer;
    ParentBone:AnsiString;
    Owner:Mesh;

    Constructor Create(Owner:Mesh);
    Procedure UpdateBone;
  End;

  MeshLight = Class(TERRAObject)
    Name:AnsiString;
    Owner:Mesh;
    Position:Vector3D;
    BoneIndex:Integer;
    ParentBone:AnsiString;

    LightType:Byte;
    LightColor:Color;

    Param1:Vector3D;
    Param2:Vector3D;
    Param3:Vector3D;

    Constructor Create(Owner:Mesh);
    Procedure UpdateBone();
  End;

  MeshFX = Class(TERRAObject)
    Procedure Update; Virtual; Abstract;
  End;

  MeshMaterial = {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    BlendMode:Integer;

    AmbientColor:Color;
    DiffuseColor:Color;
    OutlineColor:Color;

    DiffuseMap:Texture;
    TriplanarMap:Texture;
    DecalMap:Texture;
    NormalMap:Texture;
    SpecularMap:Texture;
    GlowMap:Texture;
    RefractionMap:Texture;
    AlphaMap:Texture;
    LightMap:Texture;
    ReflectiveMap:Texture;
    ReflectionMap:Texture;
    FlowMap:Texture;
    NoiseMap:Texture;
    EnviromentMap:Texture;

    FlowSpeed:Single;
    FlowBounds:Vector4D;

    ColorRamp:Texture;
    ColorTable:Texture;
    //ColorTableFactor:Single;

    VegetationBend:Single;
    Ghost:Boolean;

    Procedure Reset;

    Function Equals(Const Other:MeshMaterial):Boolean;
  End;

  MeshInstance = Class(Renderable)
    Protected
      _Mesh:Mesh;
      _BoundingBox:BoundingBox;
      _Transform:Matrix4x4;

      _FX:Array Of MeshFX;
      _FXCount:Integer;
      _ClonedMesh:Boolean;

      _Visibility:Array Of Boolean;
      _Wireframe:Array Of Boolean;

      _Materials:Array Of MeshMaterial;
      _TempAlpha:Array Of Byte;

      _UVTransforms:Array Of Matrix4x4;

      _Transforms:Array Of Matrix4x4;
      _CustomTransform:Array Of Integer;

      _AttachList:Array Of MeshAttach;
      _AttachCount:Integer;

      _Position:Vector3D;
      _Rotation:Vector3D;
      _Scale:Vector3D;

      _NeedsUpdate:Boolean;
      _NeedsRebuild:Boolean;

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

      _Lights:Array Of PositionalLight;
      _LightCount:Integer;

      Procedure DrawMesh(Const MyTransform:Matrix4x4; TranslucentPass, StencilTest:Boolean);

      Procedure DrawParticles();

      Function IsGroupTranslucent(Index:Integer):Boolean;

      Function GetAnimation: AnimationState;

    Public
      CullGroups:Boolean;
      CustomShader:Shader;
      Diffuse:Color;
      AlwaysOnTop:Boolean;

      Procedure UpdateBoundingBox;
      Procedure UpdateTransform;
      Procedure Update; Override;

      Function IsReady():Boolean;

      Function IsOpaque():Boolean; Override;
      Function IsTranslucent():Boolean; Override;

      Procedure RenderLights; Override;

      Function GetName():AnsiString; Override;

      Procedure SetDiffuseMap(GroupID:Integer; Map:Texture);
      Function GetDiffuseMap(GroupID:Integer):Texture;

      Procedure SetTriplanarMap(GroupID:Integer; Map:Texture);
      Function GetTriplanarMap(GroupID:Integer):Texture;

      Procedure SetDecalMap(GroupID:Integer; Map:Texture);
      Function GetDecalMap(GroupID:Integer):Texture;

      Procedure SetNormalMap(GroupID:Integer; Map:Texture);
      Function GetNormalMap(GroupID:Integer):Texture;

      Procedure SetSpecularMap(GroupID:Integer; Map:Texture);
      Function GetSpecularMap(GroupID:Integer):Texture;

      Procedure SetGlowMap(GroupID:Integer; Map:Texture);
      Function GetGlowMap(GroupID:Integer):Texture;

      Procedure SetRefractionMap(GroupID:Integer; Map:Texture);
      Function GetRefractionMap(GroupID:Integer):Texture;

      Procedure SetReflectiveMap(GroupID:Integer; Map:Texture);
      Function GetReflectiveMap(GroupID:Integer):Texture;

      Procedure SetEnviromentMap(GroupID:Integer; Map:Texture);
      Function GetEnviromentMap(GroupID:Integer):Texture;

      Procedure SetFlowMap(GroupID:Integer; Map:Texture);
      Function GetFlowMap(GroupID:Integer):Texture;

      Procedure SetNoiseMap(GroupID:Integer; Map:Texture);
      Function GetNoiseMap(GroupID:Integer):Texture;

      Procedure SetColorRamp(GroupID:Integer; Map:Texture);
      Function GetColorRamp(GroupID:Integer):Texture;

      Procedure SetColorTable(Map:Texture); Overload;
      Procedure SetColorTable(GroupID:Integer; Map:Texture); Overload;
      Function GetColorTable(GroupID:Integer):Texture;

      Procedure SetAlphaMap(GroupID:Integer; Map:Texture);
      Function GetAlphaMap(GroupID:Integer):Texture;

      Procedure SetLightMap(GroupID:Integer; Map:Texture);
      Function GetLightMap(GroupID:Integer):Texture;

      Procedure SetVisibility(GroupID:Integer; Visible:Boolean);
      Function GetVisibility(GroupID:Integer):Boolean;

      Procedure SetWireframeMode(GroupID:Integer; Enabled:Boolean);
      Function GetWireframeMode(GroupID:Integer):Boolean;

      Function AddEffect(FX:MeshFX):Mesh;

      Function AddParticleEmitter(Const Name:AnsiString; Position: Vector3D; Const Content:AnsiString; Const ParentBone:AnsiString = ''):MeshEmitter;

      Procedure SetDiffuseColor(MyColor:Color); Overload;
      Procedure SetDiffuseColor(GroupID:Integer; MyColor:Color); Overload;
      Function GetDiffuseColor(GroupID:Integer):Color;

      Procedure SetAmbientColor(GroupID:Integer; MyColor:Color);
      Function GetAmbientColor(GroupID:Integer):Color;

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
      Procedure SetTransform(M:Matrix4x4);

      Procedure SetMotionBlur(Enabled:Boolean);

      Procedure AddAttach(AttachMesh:Mesh; BoneIndex:Integer; M:Matrix4x4; C:Color; IsStencil:Boolean = False);
      Procedure ClearAttachs;

      Constructor Create(MyMesh:Mesh);
      Destructor Destroy; Override;

      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render(TranslucentPass:Boolean); Override;

      Function GetAttach(Index:Integer):PMeshAttach;

      Property Position:Vector3D Read _Position Write SetPosition;
      Property Rotation:Vector3D Read _Rotation Write SetRotation;
      Property Scale:Vector3D Read _Scale Write SetScale;
      Property Transform:Matrix4x4 Read _Transform Write SetTransform;

      Property CastShadows:Boolean Read _CastShadows Write _CastShadows;
      Property MotionBlur:Boolean Read _MotionBlur Write SetMotionBlur;

      Property Animation:AnimationState Read GetAnimation;
      Property Geometry:TERRA_Mesh.Mesh Read _Mesh;

      Property AttachCount:Integer Read _AttachCount;

      Property ParticleSystemsCount:Integer Read _ParticleSystemCount;
      Property EmitterCount:Integer Read _EmitterCount;
  End;

  MeshGroup = Class;

  MeshVertexLink = Packed Record
    GroupIndex:SmallInt;
    VertexIndex:Word;
  End;

  PMeshVertexArray = ^MeshVertexArray;
  MeshVertexArray=Array[0..65536]Of MeshVertex;

  MeshGroupMorph = Record
    ID:Integer;
    MorphType:Byte;
    Values:Array Of Vector3D;
  End;

	MeshGroup = Class
    Protected
      _ID:Integer;
      _Owner:Mesh;
	  	_Name:AnsiString;

      _Buffer:VBO;

      _NeedsSkeletonSetup:Boolean;

      _Material:MeshMaterial;

      _Shader:Shader;
      _LightBatch:LightBatch;

      _BoundingBox:BoundingBox;
      _Unique:Boolean;

      _Pins:Array Of Word;
      _PinCount:Integer;

      {$IFDEF PCs}
      _Fur:Fur;
      _Cloth:VerletCloth;
      {$ENDIF}

      _EmitterFX:AnsiString;

      _Vertices:Array Of MeshVertex;
    	_VertexCount:Integer;
      _ScratchVertices:Array Of MeshVertex;

      _Triangles:Array Of Triangle;
      _Edges:Array Of TriangleEdgesState;
      _TriangleNormals:Array Of Vector3D;
		  _TriangleCount:Integer;

      _VisibleTriangles:Array Of Triangle;
      _VisibleTriangleCount:Integer;
      _CullGeometry:Boolean;

      _Morphs:Array Of MeshGroupMorph;
      _MorphCount:Integer;

      _Links:Array Of MeshVertexLink;

      Procedure SetupUniforms(Transform:Matrix4x4; State:MeshInstance; Outline:Boolean; Const Material:MeshMaterial);

      Procedure SetCombineWithColor(C:Color);
      Procedure BindMaterial(Var Slot:Integer; Const Material:MeshMaterial);

      Procedure Load(Source:Stream);
      Procedure Save(Dest:Stream);

      Procedure DrawGeometry(State:MeshInstance; ShowWireframe:Boolean);

      Procedure SetTriangleCount(Count:Integer);
      Procedure SetVertexCount(Count:Integer);
      Function GetTriangles:PTriangleArray;
      Function GetVertices:PMeshVertexArray;

      Procedure SetupSkeleton;

      Procedure InheritMaterial(Const OtherMat:MeshMaterial; Var DestMaterial:MeshMaterial);

    Public
      Userdata:Pointer;
      Flags:Cardinal;
      {$IFDEF PCs}
      FurSettings:TERRA_Fur.FurSettings;
      {$ENDIF}

      Constructor Create(ID:Integer; Parent:Mesh; Name:AnsiString='');
      Destructor Destroy; Reintroduce;

		  Procedure Clean; Virtual;
      Procedure Init; Virtual;

      Procedure UpdateBoundingBox;

      Function GetAmbientColor: Color;
      Procedure SetAmbientColor(const Value: Color);

      Procedure SetAlphaMap(Map:Texture);
      Function GetAlphaMap: Texture;

      Procedure SetLightmap(Map:Texture);
      Function GetLightMap: Texture;

      Function GetColorRamp: Texture;
      Procedure SetColorRamp(Const Map:Texture);

      Function GetDecalMap: Texture;
      Procedure SetDecalMap(const Value: Texture);

      Function GetDiffuseColor: Color;
      Procedure SetDiffuseColor(const Value: Color);

      Function GetDiffuseMap: Texture;
      Procedure SetDiffuseMap(const Value: Texture);

      Function GetGlowMap: Texture;
      Procedure SetGlowMap(const Value: Texture);

      Function GetNormalMap: Texture;
      Procedure SetNormalMap(const Value: Texture);

      Function GetRefractionMap: Texture;
      Procedure SetRefractionMap(const Value: Texture);

      Function GetReflectiveMap: Texture;
      Procedure SetReflectiveMap(const Value: Texture);

      Function GetEnviromentMap: Texture;
      Procedure SetEnviromentMap(const Value: Texture);

      Function GetFlowMap: Texture;
      Procedure SetFlowMap(const Value: Texture);

      Function GetNoiseMap: Texture;
      Procedure SetNoiseMap(const Value: Texture);

      Function GetSpecularMap: Texture;
      Procedure SetSpecularMap(const Value: Texture);

      Procedure SetTriplanarMap(const Value: Texture);
      Function GetTriplanarMap: Texture;

      Procedure SetBlendMode(const Value: Integer);
      Function GetBlendMode: Integer;

      Procedure SetWireframe(Enabled:Boolean);

      Procedure SetEdge(TriangleIndex, EdgeIndex:Integer; Visible:Boolean);

      Procedure Optimize(VertexCacheSize:Integer);

      Function LockVertices(Static:Boolean):PMeshVertex;
      Procedure UnlockVertices();

      Procedure ResolveLinks();
      Procedure SetVertexLink(VertexIndex, TargetGroup, TargetVertex:Integer);

      Function AddVertexMorph(ID:Integer):Integer;
      Function HasVertexMorph(ID:Integer):Boolean;
      Function GetVertexMorph(ID, VertexIndex:Integer):Vector3D;
      Procedure SetVertexMorph(ID, VertexIndex:Integer; Value:Vector3D);

      Property Name:AnsiString Read _Name Write _Name;

		  Property DiffuseMap:Texture Read GetDiffuseMap Write SetDiffuseMap;
		  Property DecalMap:Texture Read GetDecalMap Write SetDecalMap;
		  Property TriplanarMap:Texture Read GetTriplanarMap Write SetTriplanarMap;
      Property NormalMap:Texture Read GetNormalMap Write SetNormalMap;
      Property AlphaMap:Texture Read GetAlphaMap Write SetAlphaMap;
      Property SpecularMap:Texture Read GetSpecularMap Write SetSpecularMap;
      Property RefractionMap:Texture Read GetRefractionMap Write SetRefractionMap;
      Property ReflectiveMap:Texture Read GetReflectiveMap Write SetReflectiveMap;
      Property EnviromentMap:Texture Read GetEnviromentMap Write SetEnviromentMap;
      Property FlowMap:Texture Read GetFlowMap Write SetFlowMap;
      Property NoiseMap:Texture Read GetNoiseMap Write SetNoiseMap;
      Property GlowMap:Texture Read GetGlowMap Write SetGlowMap;
      Property LightMap:Texture Read GetLightMap Write SetLightmap;
      Property ColorRamp:Texture Read GetColorRamp Write SetColorRamp;

      Property EmitterFX:AnsiString Read _EmitterFX Write _EmitterFX;

      Property AmbientColor:Color Read GetAmbientColor Write SetAmbientColor;
      Property DiffuseColor:Color  Read GetDiffuseColor Write SetDiffuseColor;

      Procedure CalculateTangents;
      Procedure CalculateTriangleNormals;
      //Procedure BuildBillboards;

	  	Function AddVertex(A:MeshVertex; FastInsert:Boolean = False):Integer;
  		Procedure AddTriangle(A,B,C:MeshVertex; FastInsert:Boolean = False);
	  	Procedure AddQuad(A,B,C,D:MeshVertex; FastInsert:Boolean = False);
      Procedure AddVertexPin(ID:Word);

      Procedure CullTriangles(Box:BoundingBox; Transform:Matrix4x4);
      Procedure UncullTriangles();

      Function Render(Const Transform:Matrix4x4; State:MeshInstance):Boolean;

      Function DuplicateVertex(Index:Integer):Integer;

      Procedure OnContextLost();

      Function Intersect(Const R:Ray; Var T:Single; Const Transform:Matrix4x4):Boolean;

      Function GetVertex(Index:Integer):MeshVertex;
      Function GetTriangle(Index:Integer):Triangle;
      Function GetTriangleNormal(Index:Integer):Vector3D;

      Function GetVertexPointer(Index:Integer):PMeshVertex;
      Function GetTrianglePointer(Index:Integer):PTriangle;

      Property VertexCount:Integer Read _VertexCount Write SetVertexCount;
      Property TriangleCount:Integer Read _TriangleCount Write SetTriangleCount;

      Property Triangles:PTriangleArray Read GetTriangles;
      Property Vertices:PMeshVertexArray Read GetVertices;

      Property GetBoundingBox:TERRA_BoundingBox.BoundingBox Read _BoundingBox;

      Property BlendMode:Integer Read GetBlendMode Write SetBlendMode;

      Property ID:Integer Read _ID;
	End;

  MeshMetadata = Class
    Name:AnsiString;
    Position:Vector3D;
    Content:AnsiString;
  End;

  MeshBoneMorph = Record
    MorphID:Integer;
    MorphType:Byte;
    Values:Array Of Vector3D;
  End;

	Mesh = Class(Resource)
		Protected
			_Groups:Array Of MeshGroup;
			_GroupCount:Integer;
			_BoundingBox:BoundingBox;

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

      _Instances:Array Of MeshInstance;
      _InstanceCount:Integer;

      Function GetSkeleton:MeshSkeleton;
      Function GetGroupCount: Integer;

      Procedure RegisterInstance(Instance:MeshInstance);
      Procedure UnregisterInstance(Instance:MeshInstance);

		Public
      Constructor CreateFromFilter(Source:MeshFilter);
      Destructor Destroy; Override;

      Function Load(Source:Stream):Boolean; Override;
	    Function Save(Dest:Stream):Boolean;

      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;

      Procedure OnContextLost(); Override;

      Procedure ResolveLinks();

      Class Function GetManager:Pointer; Override;

      Procedure CullTriangles(Box:BoundingBox; Transform:Matrix4x4);
      Procedure UncullTriangles();

//			Procedure AddMesh(MyMesh:Mesh; Const Transform:Matrix4x4);
			Procedure AddTriangle(A,B,C:MeshVertex; Group:MeshGroup);
			Procedure AddQuad(A,B,C,D:MeshVertex; Group:MeshGroup);

			Function AddGroup(Name:AnsiString=''):MeshGroup;
      Function DuplicateGroup(Group:MeshGroup; Name:AnsiString=''):MeshGroup;
			Function GetGroup(Name:AnsiString):MeshGroup; Overload;
      Function GetGroup(Index:Integer):MeshGroup; Overload;

      Function GetGroupIndex(Name:AnsiString):Integer;

      Procedure AddMetadata(Name:AnsiString; Position:Vector3D; Content:AnsiString='');
      Function GetMetadata(Name:AnsiString):MeshMetadata; Overload;
      Function GetMetadata(Index:Integer):MeshMetadata; Overload;

      Function AddEmitter(Name:AnsiString; Position:Vector3D; Content:AnsiString; ParentBone:AnsiString):MeshEmitter;
      Function GetEmitter(Index:Integer):MeshEmitter;

      Function AddLight(Name:AnsiString; Position:Vector3D; LightType:Integer; LightColor:Color; Param1, Param2, Param3:Vector3D; ParentBone:AnsiString):MeshLight; Overload;
      Function AddLight(OtherLight:MeshLight):MeshLight; Overload;
      Function GetLight(Index:Integer):MeshLight;

      Function PolyCount:Integer;

			Procedure Clean();

      Procedure Optimize(VertexCacheSize:Integer);

      Function HasBoneMorph(MorphID:Integer):Boolean;
      Function GetBoneMorph(MorphID, BoneID:Integer):Vector3D;
      Function AddBoneMorph(MorphID:Integer):Integer;
      Procedure SetBoneMorph(MorphID, BoneID:Integer; Const Value:Vector3D);

      Procedure UpdateBoundingBox;

      Function Clone():Mesh;

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
      Procedure ProcessVertex(Vertex:PMeshVertex; Source, Dest:MeshGroup); Virtual;
      Procedure ProcessTriangle(T:PTriangle; Source, Dest:MeshGroup); Virtual;
      Procedure ProcessGroup(Group:MeshGroup); Virtual;

    Public
      Destructor Destroy; Override;
      
      Function Merge(Source, Dest:Mesh; IndividualGroup:Boolean = False; MaxVertsPerGroup:Integer = -1; UpdateBox:Boolean = True):IntegerArray;
      Procedure MergeGroup(Source, Dest:MeshGroup; UpdateBox:Boolean = True);
  End;

  MeshParticleEmitter = Class(ParticleSettingsEmitter)
    Protected
      _TargetGroup:MeshGroup;

    Public
      Constructor Create(Const FXName:AnsiString; Target:MeshGroup);
      Procedure Emit(Target:PParticle); Override;
  End;

  CustomMeshFilter = Class(MeshFilter)
    Protected
      _Mesh:Mesh;
      _Animations:Array Of Animation;
      _AnimationCount:Integer;

    Public
      Procedure AddAnimation(Anim:Animation);

      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):AnsiString; Override;
      Function GetGroupFlags(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):Cardinal; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexTangent(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexBone(GroupID, Index:Integer):Integer; Override;
      Function GetVertexColor(GroupID, Index:Integer):Color; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;
      Function GetVertexUV2(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;
      Function GetDiffuseMapName(GroupID:Integer):AnsiString; Override;

      Function GetBoneCount():Integer; Override;
      Function GetBoneName(BoneID:Integer):AnsiString; Override;
      Function GetBoneParent(BoneID:Integer):Integer; Override;
      Function GetBonePosition(BoneID:Integer):Vector3D; Override;
      Function GetBoneRotation(BoneID:Integer):Vector3D; Override;

      Function GetAnimationCount():Integer; Override;
      Function GetAnimationName(AnimationID:Integer):AnsiString; Override;
      Function GetAnimationFrameRate(AnimationID:Integer):Single; Override;
      Function GetAnimationLoop(AnimationID:Integer):Boolean; Override;

      Function GetPositionKeyCount(AnimationID, BoneID:Integer):Integer; Override;
      Function GetRotationKeyCount(AnimationID, BoneID:Integer):Integer; Override;

      Function GetPositionKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey; Override;
      Function GetRotationKey(AnimationID, BoneID:Integer; KeyID:Integer):MeshVectorKey; Override;
  End;

  MeshManager = Class(ResourceManager)
    Protected
      _CubeMesh:Mesh;

      Function GetCubeMesh:Mesh;

    Public
      Destructor Destroy; Override;
      Class Function Instance:MeshManager;

      Function GetMesh(Name:AnsiString; ValidateError:Boolean = True):Mesh;
      Function CloneMesh(Name:AnsiString; ValidateError:Boolean = True):Mesh;

      Property CubeMesh:Mesh Read GetCubeMesh;
   End;


  Function CreateFilterFromMesh(MyMesh:Mesh):MeshFilter;

  Function SelectMeshShader(Group:MeshGroup; Position:Vector3D; Outline:Boolean; Const DestMaterial:MeshMaterial):Shader;

  Function MakeWaterFlowBounds(Const Box:BoundingBox):Vector4D;

Implementation
Uses TERRA_Error, TERRA_Application, TERRA_Log, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, 
  TERRA_CubeMap, TERRA_ShaderFactory, TERRA_OS,
  TERRA_FileManager, TERRA_ColorGrading, TERRA_Solids;

Type
  MeshDataBlockHandler = Function(Target:Mesh; Size:Integer; Source:Stream):Boolean;
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


Function DefaultMeshHandler(Target:Mesh; Size:Integer; Source:Stream):Boolean;
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
Function MeshReadGroup(Target:Mesh; Size:Integer; Source:Stream):Boolean;
Var
  Group:MeshGroup;
  ID:Integer;
Begin
  ID := Target._GroupCount;
  Inc(Target._GroupCount);
  SetLength(Target._Groups, Target._GroupCount);
  Group := MeshGroup.Create(ID, Target);
  Group.Load(Source);
  Target._Groups[ID] := Group;

  Result := True;
End;

Function MeshReadSkeleton(Target:Mesh; Size:Integer; Source:Stream):Boolean;
Begin
  Target.Skeleton.Read(Source);
  Result := True;
End;

Function MeshReadEmitter(Target:Mesh; Size:Integer; Source:Stream):Boolean;
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

Function MeshReadLights(Target:Mesh; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  I := Target._LightCount;
  Inc(Target._LightCount);
  SetLength(Target._Lights, Target._LightCount);

  Target._Lights[I] := MeshLight.Create(Target);
  Source.ReadString(Target._Lights[I].Name);
  Source.ReadString(Target._Lights[I].ParentBone);
  Source.Read(@Target._Lights[I].LightType, 1);
  Source.Read(@Target._Lights[I].LightColor, 4);
  Source.Read(@Target._Lights[I].Position, SizeOf(Vector3D));
  Source.Read(@Target._Lights[I].Param1, SizeOf(Vector3D));
  Source.Read(@Target._Lights[I].Param2, SizeOf(Vector3D));
  Source.Read(@Target._Lights[I].Param3, SizeOf(Vector3D));
  Target._Lights[I].UpdateBone();

  Result := True;
End;

Function MeshReadBoneMorph(Target:Mesh; Size:Integer; Source:Stream):Boolean;
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

Function MeshReadMeta(Target:Mesh; Size:Integer; Source:Stream):Boolean;
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
Function GroupReadVertexPositions(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
  PX,PY,PZ:SmallInt;
  CompressionRange:Vector3D;
Begin
  Source.Read(@Target._VertexCount, 4);
  Source.Read(@CompressionRange, SizeOf(Vector3D));

  SetLength(Target._Vertices, Target._VertexCount);

  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@PX, 2);
    Source.Read(@PY, 2);
    Source.Read(@PZ, 2);

    Target._Vertices[I].Position.X := (PX/VertexCompressionLimit)* CompressionRange.X;
    Target._Vertices[I].Position.Y := (PY/VertexCompressionLimit)* CompressionRange.Y;
    Target._Vertices[I].Position.Z := (PZ/VertexCompressionLimit)* CompressionRange.Z;
    Target._Vertices[I].Color := ColorWhite;
    Target._Vertices[I].BoneIndex := 0;
  End;

  Result := True;
End;

Function GroupReadVertexNormals(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
  PX,PY,PZ:Shortint;
Begin
  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@PX, 1);
    Source.Read(@PY, 1);
    Source.Read(@PZ, 1);

    Target._Vertices[I].Normal := VectorCreate(PX/127, PY/127, PZ/127);
  End;
  Result := True;
End;

Function GroupReadVertexUV0(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
  U,V:Byte;
  CompressionRange:Vector2D;
Begin
  Source.Read(@CompressionRange, SizeOf(Vector2D));

  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@U, 1);
    Source.Read(@V, 1);

    Target._Vertices[I].TextureCoords := VectorCreate2D((U/255)* CompressionRange.X, (V/255)* CompressionRange.Y);
  End;

  Result := True;
End;

Function GroupReadVertexUV1(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
  U,V:Byte;
  CompressionRange:Vector2D;
Begin
  Source.Read(@CompressionRange, SizeOf(Vector2D));

  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@U, 1);
    Source.Read(@V, 1);

    Target._Vertices[I].TextureCoords2 := VectorCreate2D((U/255)* CompressionRange.X, (V/255)* CompressionRange.Y);
  End;

  Result := True;
End;

Function GroupReadVertexTangents(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
  Handness:Shortint;
  PX,PY,PZ:Shortint;
Begin
  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@PX, 1);
    Source.Read(@PY, 1);
    Source.Read(@PZ, 1);

    Target._Vertices[I].Tangent := VectorCreate(PX/127, PY/127, PZ/127);

    Source.Read(@Handness, 1);
    Target._Vertices[I].Handness := Handness;
  End;

  Result := True;
End;

Function GroupReadVertexColors(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Target._VertexCount) Do
    Source.Read(@Target._Vertices[I].Color, SizeOf(Color));

  Result := True;
End;

Function GroupReadVertexBoneIndices(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
  N:Shortint;
Begin
  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@N, 1);
    If (N>MaxBones) Then
      N := 0;

    Target._Vertices[I].BoneIndex := N;
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

  SetLength(Target._Morphs[N].Values, Target._VertexCount);
  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@Target._Morphs[N].Values[I], SizeOf(Vector3D));
  End;

  Result := True;
End;

Function GroupReadVertexLinks(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  I:Integer;
Begin
  SetLength(Target._Links, Target._VertexCount);

  For I:=0 To Pred(Target._VertexCount) Do
  Begin
    Source.Read(@Target._Links[I], 4);
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
  S:AnsiString;
Begin
  S := '';

  Source.Read(@Target._Material.DiffuseColor, SizeOf(Color));
  Source.ReadString(S);
  Target._Material.DiffuseMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialTriplanar(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.TriplanarMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialSpecular(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.SpecularMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialBump(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
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

Function GroupReadMaterialLightmap(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.LightMap := TextureManager.Instance.GetTexture(S);

  If Assigned(Target._Material.LightMap) Then
  Begin
    Target._Material.LightMap.Uncompressed := True;
    Target._Material.LightMap.PreserveQuality := True;
  End;

  Result := True;
End;

Function GroupReadMaterialRefraction(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.RefractionMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;


Function GroupReadMaterialReflective(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.ReflectiveMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;


Function GroupReadMaterialEnvMap(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.EnviromentMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialGlow(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.GlowMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialAlphaMap(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.AlphaMap := TextureManager.Instance.GetTexture(S);

  Result := True;
End;

Function GroupReadMaterialRamp(Target:MeshGroup; Size:Integer; Source:Stream):Boolean;
Var
  S:AnsiString;
Begin
  S := '';

  Source.ReadString(S);
  Target._Material.ColorRamp := TextureManager.Instance.GetTexture(S);

  If Assigned(Target._Material.ColorRamp) Then
    Target._Material.ColorRamp.Uncompressed := True;

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


Destructor MeshManager.Destroy;
Begin
  Inherited;

  If Assigned(_CubeMesh) Then
  Begin
    _CubeMesh.Destroy();
    _CubeMesh := Nil;
  End;

  _MeshManager := Nil;
End;

Function MeshManager.GetMesh(Name:AnsiString; ValidateError:Boolean):Mesh;
Var
  I, N:Integer;
  S:AnsiString;
  Filter:MeshFilter;
Begin
  Result := Nil;
  Name := TrimLeft(TrimRight(Name));
  If (Name='') Then
    Exit;

  Result := Mesh(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    S := FileManager.Instance.SearchResourceFile(Name+'.mesh');
    If S<>'' Then
    Begin
      Result := Mesh.Create(S);
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
        Result := Mesh.CreateFromFilter(Filter);
        Filter.Destroy;
      End Else
      If ValidateError Then
        RaiseError('Could not find mesh. ['+Name +']');
    End;
  End;
End;

Function MeshManager.CloneMesh(Name:AnsiString; ValidateError:Boolean):Mesh;
Var
  S:AnsiString;
Begin
  Log(logDebug, 'ResourceManager', 'Cloning mesh '+Name);
  Name := TrimLeft(TrimRight(Name));
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  S := FileManager.Instance.SearchResourceFile(Name+'.mesh');
  If S<>'' Then
  Begin
    Result := Mesh.Create(S);
  End Else
  Begin
    If ValidateError Then
      RaiseError('Could not clone mesh. ['+Name +']');
    Result := Nil;
  End;
End;

Function MeshManager.GetCubeMesh: Mesh;
Var
  Cube:TERRA_Solids.CubeMesh;
Begin
  If _CubeMesh = Nil Then
  Begin
    Cube := TERRA_Solids.CubeMesh.Create(2);
    _CubeMesh := CreateMeshFromSolid(Cube);
    Cube.Destroy();
  End;

  Result := _CubeMesh;
End;

{ MeshInstance }
Procedure MeshInstance.AddAttach(AttachMesh:Mesh; BoneIndex:Integer; M:Matrix4x4; C:Color; IsStencil:Boolean);
Var
  P:Vector3D;
Begin
  If (BoneIndex<0) Or (AttachMesh = Nil) Then
    Exit;

  P := Self._Mesh.Skeleton.BindPose[Succ(BoneIndex)].Transform(VectorZero);
  M := Matrix4x4Multiply4x4(Matrix4x4Inverse(Self._Mesh.Skeleton.BindPose[Succ(BoneIndex)]), Matrix4x4Multiply4x4(M, Matrix4x4Translation(P)));

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
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Or (_Visibility[GroupID] = Visible) Then
    Exit;

  _Visibility[GroupID] := Visible;
  If _CastShadows Then
    _NeedsRebuild := True;
End;

Function MeshInstance.GetVisibility(GroupID:Integer):Boolean; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := False
  Else
    Result := _Visibility[GroupID];
End;

Procedure MeshInstance.SetWireframeMode(GroupID:Integer; Enabled:Boolean); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Or (_Wireframe[GroupID] = Enabled) Then
    Exit;

  _Wireframe[GroupID] := Enabled;
End;

Function MeshInstance.GetWireframeMode(GroupID:Integer):Boolean; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := False
  Else
    Result := _Wireframe[GroupID];
End;

Function MeshInstance.GetUVOffset(GroupID:Integer):Vector2D;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := VectorCreate2D(0, 0)
  Else
    Result := VectorCreate2D(_UVTransforms[GroupID].V[12], _UVTransforms[GroupID].V[13]);
End;

Procedure MeshInstance.SetUVOffset(GroupID:Integer; X,Y:Single);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  If Assigned(_Mesh) Then
    _Mesh._Groups[GroupID].Flags := _Mesh._Groups[GroupID].Flags Or meshGroupTextureMatrix;

  _UVTransforms[GroupID].V[12] := X;
  _UVTransforms[GroupID].V[13] := Y;
End;

Procedure MeshInstance.SetUVScale(GroupID:Integer; X,Y:Single);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  If Assigned(_Mesh) Then
    _Mesh._Groups[GroupID].Flags := _Mesh._Groups[GroupID].Flags Or meshGroupTextureMatrix;

  _UVTransforms[GroupID].V[0] := X;
  _UVTransforms[GroupID].V[5] := Y;
End;

Procedure MeshInstance.SetTextureTransform(GroupID:Integer; Transform:Matrix4x4);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _UVTransforms[GroupID] := Transform;
End;

Function MeshInstance.GetTextureTransform(GroupID:Integer):Matrix4x4;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Matrix4x4Identity
  Else
    Result := _UVTransforms[GroupID];
End;

Function MeshInstance.GetBlendMode(GroupID: Integer): Integer;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := blendBlend
  Else
    Result := _Materials[GroupID].BlendMode;
End;

Procedure MeshInstance.SetBlendMode(GroupID, Mode: Integer);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].BlendMode := Mode;
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

  _Materials[GroupID].DiffuseColor := MyColor;
End;

Function MeshInstance.GetDiffuseColor(GroupID:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := ColorWhite
  Else
  Begin
    Result := _Materials[GroupID].DiffuseColor;
    Result.A := Trunc(Result.A * _AlphaLODValue);
  End;
End;

Procedure MeshInstance.SetAmbientColor(GroupID:Integer; MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].AmbientColor := MyColor;
End;

Function MeshInstance.GetAmbientColor(GroupID:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := ColorWhite
  Else
    Result := _Materials[GroupID].AmbientColor;
End;

Procedure MeshInstance.SetOutlineColor(MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Var
  I:Integer;
Begin
  For I:=0 To Pred(_Mesh.GroupCount) Do
    _Materials[I].OutlineColor := MyColor;
End;

Procedure MeshInstance.SetOutlineColor(GroupID:Integer; MyColor:Color); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].OutlineColor := MyColor;
End;

Procedure MeshInstance.SetWaterFlowBounds(GroupID:Integer; Bounds:Vector4D);
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].FlowBounds := Bounds;
End;

Function MeshInstance.GetWaterFlowBounds(GroupID:Integer):Vector4D;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Vector4DZero
  Else
    Result := _Materials[GroupID].FlowBounds;
End;

Procedure MeshInstance.SetWaterFlowSpeed(GroupID:Integer; Speed:Single);
Begin
  If (GroupID<0) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].FlowSpeed := Speed;
End;

Function MeshInstance.GetWaterFlowSpeed(GroupID:Integer):Single;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := 0.0
  Else
    Result := _Materials[GroupID].FlowSpeed;
End;

Function MeshInstance.GetOutlineColor(GroupID:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Result := ColorNull
  Else
    Result := _Materials[GroupID].OutlineColor;
End;


Function MeshInstance.GetVegetationBend(GroupID: Integer): Single;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := 0
  Else
    Result := _Materials[GroupID].VegetationBend;
End;

Procedure MeshInstance.SetVegetationBend(GroupID: Integer; Bend:Single);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].VegetationBend := Bend;
End;

Procedure MeshInstance.SetGhostMode(Ghost:Boolean);
Var
  I:Integer;
Begin
  If (_Mesh = Nil) Then
    Exit;

  For I:=0 To Pred(_Mesh._GroupCount) Do
    _Materials[I].Ghost := Ghost;
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
  _NeedsUpdate := True;
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
  _NeedsUpdate := True;
  _NeedsRebuild := True;
End;

Procedure MeshInstance.SetScale(P: Vector3D); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (_Scale.Distance(P)<MinDelta) Then
    Exit;

  _Scale := P;
  _NeedsUpdate := True;
End;

Procedure MeshInstance.SetTransform(M: Matrix4x4);
Begin
  _NeedsUpdate := False;
  _NeedsRebuild := True;
  _Transform := M;
  _Position.X := M.V[12];
  _Position.Y := M.V[13];
  _Position.Z := M.V[14];
  UpdateBoundingBox;
End;

{Procedure MeshInstance.SetGroupTransform(GroupID:Integer; Transform: Matrix4x4);
Var
  Center:Vector3D;
  A,B:Matrix4x4;
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  Center := _Mesh.GetGroup(GroupID).GetBoundingBox.Center;
  A := MatrixTranslation(Center);
  Center.Scale(-1);
  B := MatrixTranslation(Center);
  _Transforms[GroupID] := MatrixMultiply4x4(A, MatrixMultiply4x4(Transform, B));
  _CustomTransform[GroupID] := True;
End;}

Procedure MeshInstance.SetGroupLocalTransform(GroupID:Integer; Const Transform: Matrix4x4);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Transforms[GroupID] := Transform;
  _CustomTransform[GroupID] := customTransformLocal;
End;

Procedure MeshInstance.SetGroupGlobalTransform(GroupID:Integer; Const Transform: Matrix4x4);
Begin
  If (GroupID<0) Or (GroupID>=_Mesh._GroupCount) Then
    Exit;

  _Transforms[GroupID] := Transform;
  _CustomTransform[GroupID] := customTransformGlobal;
End;

Procedure MeshInstance.UpdateBoundingBox;
Begin
  _BoundingBox := _Mesh.BoundingBox;
  {For I:=0 To Pred(_AttachCount) Do
    Box := BoundingBoxAdd(Box, _AttachList[I].AttachMesh.BoundingBox);}

  _BoundingBox.Transform(_Transform);
End;

Procedure MeshInstance.UpdateTransform;
Begin
  If (_Mesh=Nil) Or (Not _Mesh.IsReady) Then
    Exit;

  _NeedsUpdate := False;
  _Transform := Matrix4x4Transform(_Position, _Rotation, _Scale);
  UpdateBoundingBox;
End;

Constructor MeshInstance.Create(MyMesh: Mesh);
Var
  N, I:Integer;
Begin
  If (MyMesh = Nil) Then
  Begin
    Log(logWarning, 'Mesh', 'Attemping to create instance from null mesh.');
    Exit;
  End;

  _Mesh := MyMesh;

  _Mesh.RegisterInstance(Self);

  _Mesh.PreFetch();

  _ClonedMesh := False;

  _Position := VectorZero;
  _Rotation := VectorZero;
  _Scale := VectorOne;
  _NeedsUpdate := True;
  _NeedsRebuild := True;

  CullGroups := False;

  Self.Diffuse := ColorWhite;

  _LastFrameID := 0;
  _LastTrailUpdate := Application.Instance.GetElapsedTime();

  SetLength(_Visibility, _Mesh.GroupCount);
  SetLength(_Wireframe, _Mesh.GroupCount);
  SetLength(_Materials, _Mesh.GroupCount);
  SetLength(_Transforms, _Mesh.GroupCount);
  SetLength(_UVTransforms, _Mesh.GroupCount);
  SetLength(_CustomTransform, _Mesh.GroupCount);
  SetLength(_TempAlpha, _Mesh.GroupCount);

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
    _Lights[I] := Nil;

  N := 0;
  For I:=0 To Pred(_Mesh.GroupCount) Do
  Begin
    _Visibility[I] := (_Mesh._Groups[I].Flags And meshGroupHidden=0);
    _Materials[I].Reset();
    _CustomTransform[I] := customTransformNone;
    _UVTransforms[I] := Matrix4x4Identity;

    If (_Mesh._Groups[I].Flags And meshGroupCastShadow<>0) Then
      Inc(N);
  End;

  CastShadows := (N>0);

  _Animation := Nil;
End;

Destructor MeshInstance.Destroy;
Var
  I:Integer;
Begin
  Inherited;

  For I:=0 To Pred(Self._ParticleSystemCount) Do
    DestroyObject(@_ParticleSystems[I]);
  _ParticleSystemCount := 0;

  For I:=0 To Pred(Self._EmitterCount) Do
    DestroyObject(@_Emitters[I]);
  _EmitterCount := 0;

  For I:=0 To Pred(Self._LightCount) Do
    DestroyObject(@_Lights[I]);
  _LightCount := 0;

  DestroyObject(@_ShadowVolume);
  DestroyObject(@_Animation);

  If Assigned(_Mesh) Then
  Begin
    _Mesh.UnregisterInstance(Self);

    If (_ClonedMesh) Then
      _Mesh.Destroy;
  End;
End;

Function MeshInstance.GetBoundingBox:BoundingBox; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (_NeedsUpdate) Then
    UpdateTransform;

  Result := _BoundingBox;
  If GraphicsManager.Instance.ReflectionActive Then
    Result.Transform(GraphicsManager.Instance.ReflectionMatrix);
End;

Procedure MeshInstance.Update();
Var
  I,J, N:Integer;
  S:Single;
  B:MeshBone;
  M:Matrix4x4;
Begin
  If (_Mesh = Nil) Or (Not _Mesh.IsReady) Then
    Exit;

  For I:=0 To Pred(_FXCount) Do
    _FX[I].Update();

  If (Assigned(_Animation)) And (Assigned(_Animation.Root)) Then
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

Procedure MeshInstance.RenderLights;
Var
  I:Integer;
  M, Transform:Matrix4x4;
  Box:BoundingBox;
  GroupTransform:Boolean;
  MyLight:MeshLight;
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

    If (_Lights[I]=Nil) Then
    Begin
      Case MyLight.LightType Of
      lightTypePoint:
        Begin
          _Lights[I] := PointLight.Create(VectorZero);
          {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Mesh', 'Creating point light...');{$ENDIF}
        End;
      Else
        Continue;
      End;
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

    _Lights[I].Position := P;
    _Lights[I].Color := MyLight.LightColor;

    Case MyLight.LightType Of
      lightTypePoint:
        Begin
          N := MyLight.Param1.X;
          

          If (MyLight.Param3.X>0) Then
            N := N + N * 0.05 * Abs(Cos(RAD*(Trunc(GetTime()/MyLight.Param3.X) Mod 180)));

            //Flicker Param3.y ??? {FIXME}

          PointLight(_Lights[I]).Radius := N;
        End;
    End;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Mesh', 'Adding light to manager...');{$ENDIF}
    LightManager.Instance.AddLight(_Lights[I]);
  End;
End;

Procedure MeshInstance.DrawParticles();
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

    _ParticleSystems[I].Update();
    GraphicsManager.Instance.AddRenderable(_ParticleSystems[I]);
  End;
End;

Procedure MeshInstance.DrawMesh(Const MyTransform:Matrix4x4; TranslucentPass, StencilTest:Boolean);
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

    If (_CustomTransform = Nil) Then
      _CustomTransform[I] := customTransformNone;

    Case _CustomTransform[I] Of
      customTransformLocal: Transform := Matrix4x4Multiply4x4(MyTransform, _Transforms[I]);
      customTransformGlobal: Transform := _Transforms[I];
      Else
        Transform := MyTransform;
    End;

    If (CullGroups) Then
    Begin
      Box := _Mesh._Groups[I]._BoundingBox;
      Box.Transform(Transform);
      If Not GraphicsManager.Instance.IsBoxVisible(Box) Then
        Continue;
    End;

    If (IsGroupTranslucent(I) = TranslucentPass) Then
	    _Mesh._Groups[I].Render(Transform, Self);
  End;
End;

Procedure MeshInstance.Render(TranslucentPass:Boolean);
Var
  C:Color;
  Time:Cardinal;
  I, J:Integer;
  M:Matrix4x4;
  Temp:Matrix4x4;
  S:Single;
Begin
  If (_Mesh=Nil) Then
    Exit;

  If (GraphicsManager.Instance.RenderStage=renderStageShadow) And (Not _CastShadows) Then
    Exit;

  If (_NeedsUpdate) Then
    UpdateTransform;

  If (GraphicsManager.Instance.RenderStage=renderStageShadow) And (_CastShadows)
  And (GraphicsManager.Instance.Settings.DynamicShadows.Enabled) Then
  Begin
    If (Not Assigned(_ShadowVolume)) Then
    Begin
      _ShadowVolume := ShadowVolume.Create;
      _NeedsRebuild := True;
    End;

    If (Assigned(_Animation)) And (Assigned(_Animation.Root)) Then
      _NeedsRebuild := True;

    If (_NeedsRebuild)  Then
    Begin
      _ShadowVolume.Rebuild(_Mesh, Self);
      _NeedsRebuild := False;
    End;

    _ShadowVolume.Render;
    Exit;
  End;

  If (GraphicsManager.Instance.Settings.AlphaFade.Enabled) Then
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
  If (Not GraphicsManager.Instance.ReflectionActive) And (GraphicsManager.Instance.RenderStage<>renderStageShadow) Then
  Begin
    For I:=0 To Pred(_AttachCount) Do
    If (_AttachList[I].IsStencil) Then
    Begin
      _StencilID := GraphicsManager.Instance.GenerateStencilID();
      Break;
    End;

    If (_StencilID<=0) Then
    For I:=0 To Pred(_Mesh._GroupCount) Do
    If (_Mesh._Groups[I].Flags And meshGroupStencilMask<>0) Then
    Begin
      _StencilID := GraphicsManager.Instance.GenerateStencilID();
      Break;
    End;
  End;

  If _StencilID>0 Then
  Begin
    //glClear(GL_STENCIL_BUFFER_BIT);
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS, _StencilID, $FFFFFFFF);
    glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
    glColorMask(False, False, False, False);
    glDepthMask(False);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    For I:=0 To Pred(_AttachCount) Do
    If (_AttachList[I].IsStencil) Then
    Begin
      M := Matrix4x4Multiply4x3(_Transform, Matrix4x4Multiply4x3(Animation.Transforms[Succ(_AttachList[I].BoneIndex)], _AttachList[I].Matrix));

      _AttachList[I].AttachMesh.Prefetch();

      For J:=0 To Pred(_AttachList[I].AttachMesh._GroupCount) Do
      Begin
        C := _AttachList[I].AttachMesh._Groups[J].DiffuseColor;
        _AttachList[I].AttachMesh._Groups[J].Flags := meshGroupColorOff;
  	    _AttachList[I].AttachMesh._Groups[J].Render(M, Nil);
      End;
    End;

    For I:=0 To Pred(_Mesh._GroupCount) Do
    If (_Mesh._Groups[I].Flags And meshGroupStencilMask<>0) Then
    Begin
      Self._Mesh._Groups[I].Render(_Transform, Self);
    End;                       

    glDepthMask(True);
    glColorMask(True, True, True, True);
    glStencilFunc(GL_NOTEQUAL, _StencilID, $FFFFFFFF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  End;

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Rendering main mesh');{$ENDIF}

  If (_StencilID>0) Then
  Begin
    DrawMesh(_Transform, TranslucentPass, True);
    glDisable(GL_STENCIL_TEST);
  End;

  DrawMesh(_Transform, TranslucentPass, False);

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Main mesh done');{$ENDIF}

  If (_RenderTrails) And (TranslucentPass) And (GraphicsManager.Instance.RenderStage=renderStageDiffuse) Then
  Begin
    For I:=0 To Pred(_Mesh.GroupCount) Do
      _TempAlpha[I] := _Materials[I].DiffuseColor.A;
    Temp := _Transform;

    Time := Application.Instance.GetElapsedTime();
    _RenderTrails := False;

    For J:=Pred(MaxTrailSize) DownTo 0 Do
    Begin
      For I:=0 To Pred(_Mesh.GroupCount) Do
      Begin
        S := (Time - _LastMotionBlur)/ TrailDecay;
        If (S>1.0) Then
          _Materials[I].DiffuseColor.A := 0
        Else
        Begin
          S := 1.0 - S;
          S := S * (1.0 - (Succ(J)/MaxTrailSize));
          _Materials[I].DiffuseColor.A := Trunc(_TempAlpha[I] * 0.75 * S);
          _RenderTrails := _Materials[I].DiffuseColor.A>0;
        End;
      End;

      If _RenderTrails Then
      Begin
        S := 0.75 + 0.25 * (1.0 - (J/Pred(MaxTrailSize)));
        _Transform := Matrix4x4Multiply4x3(_OldTransforms[J], Matrix4x4Scale(S, S, S));
        DrawMesh( _Transform, TranslucentPass, False);
      End;
    End;

    _Transform := Temp;
    For I:=0 To Pred(_Mesh.GroupCount) Do
      _Materials[I].DiffuseColor.A := _TempAlpha[I];
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
      _AttachList[I].AttachMesh._Groups[J].DiffuseColor := ColorScale(_AttachList[I].AttachMesh._Groups[J].DiffuseColor, _AttachList[I].Color);
	    _AttachList[I].AttachMesh._Groups[J].Render(M, Nil);
      _AttachList[I].AttachMesh._Groups[J].DiffuseColor := C;
    End;
  End;

  If (TranslucentPass) Then
    Self.DrawParticles();

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Inherited mesh');{$ENDIF}
End;

Function MeshInstance.GetAttach(Index: Integer): PMeshAttach;
Begin
  If (Index<0) Or (Index>=_AttachCount) Then
    Result := Nil
  Else
    Result := @_AttachList[Index];
End;

Function MeshInstance.GetName:AnsiString;
Begin
  If Assigned(_Mesh) Then
    Result := _Mesh.Name + '(X:'+FloatToString(_Position.X)+ ', Y:'+FloatToString(_Position.Y)+ ', Z:'+FloatToString(_Position.Z)+')'
  Else
    Result := 'Undefined';
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
  If (_Visibility[I]) And (_Mesh._Groups[I].TriangleCount>0) And (Not IsGroupTranslucent(I)) Then
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
  If (_Visibility[I]) And (_Mesh._Groups[I].TriangleCount>0) And (IsGroupTranslucent(I)) Then
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
  If (Group.Flags And meshGroupForceOpaque<>0) Then
    Exit;

  If (Self.Diffuse.A<255) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := (_Materials[Index].DiffuseColor.A<255) Or (Group.DiffuseColor.A<255) Or (Group.Flags And meshGroupTransparency<>0) Or (_Materials[Index].Ghost);
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

Function MeshInstance.AddParticleEmitter(Const Name:AnsiString; Position: Vector3D; Const Content:AnsiString; Const ParentBone:AnsiString):MeshEmitter;
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

Function MeshInstance.AddEffect(FX: MeshFX):Mesh;
Var
  I:Integer;
Begin
  If _FXCount = 0 Then
  Begin
    _Mesh := MeshManager.Instance.CloneMesh(_Mesh.Name);
    _Mesh.Prefetch();

    For I:=0 To Pred(_Mesh.GroupCount) Do
      _Mesh._Groups[I].Flags := _Mesh._Groups[I].Flags Or meshGroupDynamic;
      
    _ClonedMesh := True;
  End;

  Inc(_FXCount);
  SetLength(_FX, _FXCount);
  _FX[Pred(_FXCount)] := FX;

  Result := _Mesh;
End;

Function MeshInstance.GetAlphaMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].AlphaMap;
End;

Function MeshInstance.GetColorRamp(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].ColorRamp;
End;

Function MeshInstance.GetDecalMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].DecalMap;
End;

Function MeshInstance.GetDiffuseMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].DiffuseMap;
End;

Function MeshInstance.GetGlowMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].GlowMap;
End;

Function MeshInstance.GetLightMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].LightMap;
End;

Function MeshInstance.GetNormalMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].NormalMap;
End;

Function MeshInstance.GetRefractionMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].RefractionMap;
End;

Function MeshInstance.GetReflectiveMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].ReflectiveMap;
End;

Function MeshInstance.GetEnviromentMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].EnviromentMap;
End;

Function MeshInstance.GetFlowMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].FlowMap;
End;

Function MeshInstance.GetNoiseMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].NoiseMap;
End;

Function MeshInstance.GetSpecularMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].SpecularMap;
End;

Function MeshInstance.GetColorTable(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].ColorTable;
End;

Function MeshInstance.GetTriplanarMap(GroupID: Integer): Texture;
Begin
  If (GroupID<0) Or (_Mesh = Nil)Or (GroupID>=_Mesh._GroupCount) Then
    Result := Nil
  Else
    Result := _Materials[GroupID].TriplanarMap;
End;

Procedure MeshInstance.SetAlphaMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].AlphaMap := Map;
End;

Procedure MeshInstance.SetColorRamp(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].ColorRamp := Map;
End;

Procedure MeshInstance.SetDecalMap(GroupID: Integer; Map:Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].DecalMap := Map;
End;

Procedure MeshInstance.SetDiffuseMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].DiffuseMap := Map;
End;

Procedure MeshInstance.SetGlowMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].GlowMap := Map;
End;

Procedure MeshInstance.SetLightMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].LightMap := Map;
End;

Procedure MeshInstance.SetNormalMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].NormalMap := Map;
End;

Procedure MeshInstance.SetRefractionMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].RefractionMap := Map;
End;

Procedure MeshInstance.SetReflectiveMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].ReflectiveMap := Map;
End;

Procedure MeshInstance.SetEnviromentMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].EnviromentMap := Map;
End;

Procedure MeshInstance.SetFlowMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].FlowMap := Map;
End;

Procedure MeshInstance.SetNoiseMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].NoiseMap := Map;
End;

Procedure MeshInstance.SetSpecularMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].SpecularMap := Map;
End;

Procedure MeshInstance.SetTriplanarMap(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].TriplanarMap := Map;
End;

Procedure MeshInstance.SetColorTable(Map: Texture);
Var
  I:Integer;
Begin
  If (_Mesh = Nil) Then
    Exit;
    
  For I:=0 To Pred(_Mesh.GroupCount) Do
    SetColorTable(I, Map);
End;

Procedure MeshInstance.SetColorTable(GroupID: Integer; Map: Texture);
Begin
  If (GroupID<0) Or (_Mesh = Nil) Or (GroupID >= _Mesh._GroupCount) Then
    Exit;

  _Materials[GroupID].ColorTable := Map;
End;

Function MeshInstance.GetAnimation: AnimationState;
Begin
  If (_Animation = Nil) And (Assigned(_Mesh)) Then
    _Animation := AnimationState.Create(_Mesh.Skeleton.Name, _Mesh.Skeleton);

  Result := _Animation;
End;

{ MeshGroup }
Destructor MeshGroup.Destroy;
Begin
  Clean;
End;

Procedure MeshGroup.SetupSkeleton;
Var
  Index, J:Integer;
  M:Matrix4x4;
  Skel:MeshSkeleton;
Begin
  _NeedsSkeletonSetup := False;

  Skel := _Owner.Skeleton;

  If (Skel.BoneCount=0) Then
    Exit;


  For J:=0 To Pred(_VertexCount) Do
  Begin
    Index := Trunc(_Vertices[J].BoneIndex);
	  If (Index>0) Then
    Begin
      M := Skel.GetBone(Pred(Index)).AbsoluteMatrix;
      _Vertices[J].Position := M.Transform(_Vertices[J].Position);
      _Vertices[J].Normal := M.TransformNormal(_Vertices[J].Normal);
    End;
  End;
End;

Var
  TV:Array[0..1024*64] Of MeshVertex;

Procedure MeshGroup.DrawGeometry(State:MeshInstance; ShowWireframe:Boolean);
Var
  I, N:Integer;
  M:Matrix4x4;
  PositionHandle, UVHandle, UVHandle2, ColorHandle, NormalHandle:Integer;
Begin
  If (_CullGeometry) And (_VisibleTriangleCount>=_TriangleCount) Then
  Begin
    _CullGeometry := False;
  End;

  If (Not GraphicsManager.Instance.Settings.VertexBufferObject.Enabled) Then
  Begin
    If (_Owner.Skeleton.BoneCount > 0 ) And (Assigned(State)) Then
    Begin
      For I:=0 To Pred(_VertexCount) Do
      Begin
        TV[I] := _Vertices[I];
        N := Trunc(TV[I].BoneIndex);
        If (State.Animation = Nil) Or (State.Animation.Root = Nil) Then
          M := _Owner.Skeleton.BindPose[N]
        Else
          M := State.Animation.Transforms[N];

        TV[I].Position := M.Transform(TV[I].Position);
        TV[I].Normal := M.TransformNormal(TV[I].Normal);
      End;
    End Else
    Begin
      For I:=0 To Pred(_VertexCount) Do
        TV[I] := _Vertices[I];
    End;

    If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
    Begin
{$IFDEF PC}
      {If (LightManager.Instance.Sun.Enabled) Then
      Begin
        glEnable(GL_LIGHT0);
        glEnable(GL_LIGHTING);
      End Else}
      Begin
        glDisable(GL_LIGHTING);
      End;
{$ENDIF}
    End;

  {$IFDEF PC}
    If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
    Begin
      glEnableClientState(GL_VERTEX_ARRAY);
      glEnableClientState(GL_COLOR_ARRAY);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);

      glVertexPointer(3, GL_FLOAT, SizeOf(MeshVertex), @TV[0].Position);
      glTexCoordPointer(2, GL_FLOAT, SizeOf(MeshVertex), @TV[0].TextureCoords);
      glColorPointer(4, GL_UNSIGNED_BYTE, SizeOf(MeshVertex), @TV[0].Color);
    End Else
  {$ENDIF}
    Begin
      PositionHandle := _Shader.GetAttribute('terra_position');
      UVHandle := _Shader.GetAttribute('terra_UV0');
      UVHandle2 := _Shader.GetAttribute('terra_UV1');
      ColorHandle := _Shader.GetAttribute('terra_color');
      NormalHandle := _Shader.GetAttribute('terra_normal');

      glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(MeshVertex), @(TV[0].Position));

      If (UVHandle>=0) Then
        glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, SizeOf(MeshVertex), @(TV[0].TextureCoords));

      If (UVHandle2>=0) Then
        glVertexAttribPointer(UVHandle2, 2, GL_FLOAT, False, SizeOf(MeshVertex), @(TV[0].TextureCoords2));

      If (NormalHandle>=0) Then
        glVertexAttribPointer(NormalHandle, 3, GL_FLOAT, False, SizeOf(MeshVertex), @(TV[0].Normal));

      If (ColorHandle>=0) Then
        glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(MeshVertex), @(TV[0].Color));
    End;

    If (_CullGeometry) And (_VisibleTriangles<>Nil)  Then
    Begin
      If _VisibleTriangleCount>0 Then
      Begin
        glDrawElements(GL_TRIANGLES, _VisibleTriangleCount*3, GL_UNSIGNED_SHORT, @_VisibleTriangles[0]);
        GraphicsManager.Instance.Internal(0 , _VisibleTriangleCount);
      End;
    End Else
    Begin
      glDrawElements(GL_TRIANGLES, _TriangleCount*3, GL_UNSIGNED_SHORT, @_Triangles[0]);
      GraphicsManager.Instance.Internal(0 , _TriangleCount);
    End;

    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'Mesh', 'glDrawElements: '+IntToString(_TriangleCount*3));
    {$ENDIF}

  {$IFDEF PC}
    If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
    Begin
      glDisableClientState(GL_VERTEX_ARRAY);
      glDisableClientState(GL_COLOR_ARRAY);
      glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    End;
  {$ENDIF}
  End Else
  Begin
    If Not Assigned(_Buffer) Then
    Begin
      _Buffer := VBO.Create(_Vertices, _Triangles,  _Edges, _VertexCount, _TriangleCount, SizeOf(MeshVertex), (Self.Flags And meshGroupDynamic<>0));
      _Buffer.AddAttribute('terra_position', 3, GL_FLOAT, False);
      _Buffer.AddAttribute('terra_UV0', 2, GL_FLOAT, False);
      _Buffer.AddAttribute('terra_UV1', 2, GL_FLOAT, False);
      _Buffer.AddAttribute('terra_color', 4, GL_UNSIGNED_BYTE, True);
      _Buffer.AddAttribute('terra_normal', 3, GL_FLOAT, False);
      _Buffer.AddAttribute('terra_boneIndex', 1, GL_FLOAT, False, Not _Owner._Skinning);
      _Buffer.AddAttribute('terra_tangent', 4, GL_FLOAT, False, Not _Owner._NormalMapping);
    End;

    If (_CullGeometry) And (Assigned(_VisibleTriangles)) Then
      _Buffer.SetIndexList(_VisibleTriangles, _VisibleTriangleCount)
    Else
      _Buffer.SetIndexList(_Triangles, _TriangleCount);
      
    _Buffer.Draw(ShowWireframe);
  End;
End;

Function MeshGroup.DuplicateVertex(Index:Integer):Integer;
Begin
  Result := VertexCount;
  Inc(_VertexCount);
  SetLength(_Vertices, _VertexCount);
  _Vertices[Result] := _Vertices[Index];
End;

Function MeshGroup.AddVertex(A:MeshVertex; FastInsert:Boolean):Integer;
Const
	Epsilon = 0.001;
Var
	I:Integer;
Begin
  If Not FastInsert Then
	For I:=0 To Pred(_VertexCount) Do
	If (_Vertices[I].Position.Distance(A.Position)<Epsilon) And (_Vertices[I].TextureCoords.Distance(A.TextureCoords)<Epsilon)
	And (_Vertices[I].Normal.Distance(A.Normal)<Epsilon)	Then
	Begin
		Result := I;
		Exit;
	End;

	Inc(_VertexCount);
	SetLength(_Vertices, _VertexCount);
	Result := Pred(_VertexCount);
	_Vertices[Result] := A;
End;

Procedure MeshGroup.AddTriangle(A,B,C:MeshVertex; FastInsert:Boolean);
Begin
	Inc(_TriangleCount);
  _VisibleTriangleCount := _TriangleCount;
	SetLength(_Triangles, _TriangleCount);
	_Triangles[Pred(_TriangleCount)].Indices[0] := AddVertex(A, FastInsert);
	_Triangles[Pred(_TriangleCount)].Indices[1] := AddVertex(B, FastInsert);
	_Triangles[Pred(_TriangleCount)].Indices[2] := AddVertex(C, FastInsert);
End;

Procedure MeshGroup.AddQuad(A,B,C,D:MeshVertex; FastInsert:Boolean);
Begin
	AddTriangle(A, B, C, FastInsert);
	AddTriangle(B, C, D, FastInsert);
End;

Procedure MeshGroup.AddVertexPin(ID:Word);
Begin
  Inc(_PinCount);
  SetLength(_Pins, _PinCount);
  _Pins[Pred(_PinCount)] := ID;
End;

Function MeshGroup.GetVertex(Index:Integer):MeshVertex;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If (Index>=0) And (Index<_VertexCount) Then
    Result := _Vertices[Index]
   Else
   	FillChar(Result, SizeOf(Result), 0);
End;

Function MeshGroup.GetTriangle(Index:Integer):Triangle;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  If (Index>=0) And (Index<_TriangleCount) Then
    Result := _Triangles[Index]
   Else
   	FillChar(Result, SizeOf(Result), 0);
End;

Function MeshGroup.GetVertexPointer(Index: Integer): PMeshVertex;
Begin
  If (Index>=0) And (Index<_VertexCount) And (Index<Length(_Vertices)) Then
    Result :=  @_Vertices[Index]
  Else
    Result := Nil;
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
	SetLength(_Vertices, 0);
	SetLength(_Triangles, 0);
  SetLength(_Morphs, 0);
  SetLength(_Links, 0);
	_VertexCount := 0;
	_TriangleCount := 0;
  _VisibleTriangleCount := 0;
  _MorphCount := 0;

  If Assigned(_Buffer) Then
  Begin
    _Buffer.Destroy;
    _Buffer := Nil;
  End;

{$IFDEF PCs}
  If Assigned(_Fur) Then
  Begin
    _Fur.Destroy;
    _Fur := Nil;
  End;

  If Assigned(_Cloth) Then
  Begin
    _Cloth.Destroy;
    _Cloth := Nil;
  End;
{$ENDIF}
End;

Function MeshGroup.GetVertices:PMeshVertexArray;
Begin
  If (_VertexCount>0) Then
    Result := @(_Vertices[0])
  Else
    Result := Nil;
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
  _VertexCount := Count;
  SetLength(_Vertices, _VertexCount);
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
  S:AnsiString;
  Tex:Texture;
  Tag:FileHeader;
  Handler:GroupDataBlockHandler;
Begin
  Source.ReadString(_Name);
  Source.Read(@Flags, 4);

  _Material.AmbientColor := ColorWhite;
  _Material.DiffuseColor := ColorWhite;
  _Material.BlendMode := -1;

  If (Self.Flags And meshGroupAlphaTest<>0) Then
    Self.Flags := Self.Flags Or meshGroupTransparency;

  Repeat
    Source.Read(@Tag, 4);
    If (Tag=tagGroupEnd) Then
      Break;

    Source.Read(@Size, 4);
    Handler := GetMeshGroupHandler(Tag);
    Handler(Self, Size, Source);
  Until (Source.EOF);
End;

Procedure MeshGroup.Save(Dest:Stream);
Var
  I, J, Size:Integer;
  Name:AnsiString;
  Tag:FileHeader;
  Index, Handness:Shortint;
  ShouldStore:Boolean;
  PX,PY,PZ:SmallInt;
  SX,SY,SZ:Shortint;
  PU,PV:Byte;
  PositionRange:Vector3D;
  UVRange:Vector2D;

  Procedure WriteTexture(Tag:FileHeader; Tex:Texture);
  Begin
    If Tex = Nil Then
      Exit;

    Size := Succ(Length(Tex.Name));
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    Dest.WriteString(Tex.Name);
  End;
Begin
  Self.UpdateBoundingBox();

  Dest.WriteString(_Name);
  Dest.Write(@Flags, 4);

  {$IFDEF CONSOLEOUTPUT}
  WriteLn('Vertices: ', _VertexCount);
  WriteLn('Triangles: ', _TriangleCount, ' [Ofs: ',Dest.Position,']');
  {$ENDIF}

  Tag := tagVertexPositions;
  Size := 4 + _VertexCount * 2 * 3;

  PositionRange := VectorZero;
  For I:=0 To Pred(_VertexCount) Do
  Begin
    PositionRange.X := FloatMax(PositionRange.X, Abs(_Vertices[I].Position.X));
    PositionRange.Y := FloatMax(PositionRange.Y, Abs(_Vertices[I].Position.Y));
    PositionRange.Z := FloatMax(PositionRange.Z, Abs(_Vertices[I].Position.Z));
  End;

  Dest.Write(@Tag, 4);
  Dest.Write(@Size, 4);
  Dest.Write(@_VertexCount, 4);
  Dest.Write(@PositionRange, SizeOf(Vector3D));

  For I:=0 To Pred(_VertexCount) Do
  Begin
    PX := Trunc(SafeDiv(_Vertices[I].Position.X, PositionRange.X)*VertexCompressionLimit);
    PY := Trunc(SafeDiv(_Vertices[I].Position.Y, PositionRange.Y)*VertexCompressionLimit);
    PZ := Trunc(SafeDiv(_Vertices[I].Position.Z, PositionRange.Z)*VertexCompressionLimit);
    Dest.Write(@PX, 2);
    Dest.Write(@PY, 2);
    Dest.Write(@PZ, 2);
  End;

  Tag := tagVertexNormals;
  Size := _VertexCount * 3;
  Dest.Write(@Tag, 4);
  Dest.Write(@Size, 4);
  For I:=0 To Pred(_VertexCount) Do
  Begin
    SX := Trunc(_Vertices[I].Normal.X*127);
    SY := Trunc(_Vertices[I].Normal.Y*127);
    SZ := Trunc(_Vertices[I].Normal.Z*127);
    Dest.Write(@SX, 1);
    Dest.Write(@SY, 1);
    Dest.Write(@SZ, 1);
  End;

  Tag := tagVertexUVs0;
  Size := _VertexCount * 2;

  UVRange := VectorCreate2D(0, 0);
  For I:=0 To Pred(_VertexCount) Do
  Begin
    UVRange.X := FloatMax(UVRange.X, Abs(_Vertices[I].TextureCoords.X));
    UVRange.Y := FloatMax(UVRange.Y, Abs(_Vertices[I].TextureCoords.Y));
  End;

  Dest.Write(@Tag, 4);
  Dest.Write(@Size, 4);
  Dest.Write(@UVRange, SizeOf(Vector2D));

  For I:=0 To Pred(_VertexCount) Do
  Begin
    If (_Vertices[I].TextureCoords.X<0) Or (_Vertices[I].TextureCoords.Y<0) Then
    Begin
      IntTOString(2);
    End;

    If (_Vertices[I].TextureCoords.X<0) Then 
      _Vertices[I].TextureCoords.X := 0;

    If (_Vertices[I].TextureCoords.Y<0) Then
      _Vertices[I].TextureCoords.Y := 0;

    PU := Trunc(SafeDiv(_Vertices[I].TextureCoords.X,UVRange.X)*255);
    PV := Trunc(SafeDiv(_Vertices[I].TextureCoords.Y,UVRange.Y)*255);
    Dest.Write(@PU, 1);
    Dest.Write(@PV, 1);
  End;

  If (Flags And meshGroupLightmap<>0) Or (Flags And meshGroupAlphaMap<>0) Then
  Begin
    Tag := tagVertexUVs1;
    Size := _VertexCount * 2;

    UVRange := VectorCreate2D(0, 0);
    For I:=0 To Pred(_VertexCount) Do
    Begin
      UVRange.X := FloatMax(UVRange.X, Abs(_Vertices[I].TextureCoords2.X));
      UVRange.Y := FloatMax(UVRange.Y, Abs(_Vertices[I].TextureCoords2.Y));
    End;

    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    Dest.Write(@UVRange, SizeOf(Vector2D));

    For I:=0 To Pred(_VertexCount) Do
    Begin
      PU := Trunc(SafeDiv(_Vertices[I].TextureCoords2.X,UVRange.X)*255);
      PV := Trunc(SafeDiv(_Vertices[I].TextureCoords2.Y,UVRange.Y)*255);
      Dest.Write(@PU, 1);
      Dest.Write(@PV, 1);
    End;
  End;

  Tag := tagVertexTangents;
  Size := _VertexCount * ( 3 * 2 + 1);
  Dest.Write(@Tag, 4);
  Dest.Write(@Size, 4);
  For I:=0 To Pred(_VertexCount) Do
  Begin
    SX := Trunc(_Vertices[I].Tangent.X*127);
    SY := Trunc(_Vertices[I].Tangent.Y*127);
    SZ := Trunc(_Vertices[I].Tangent.Z*127);
    Dest.Write(@SX, 1);
    Dest.Write(@SY, 1);
    Dest.Write(@SZ, 1);

    Handness := Trunc(_Vertices[I].Handness);
    Dest.Write(@Handness, 1);
  End;

  ShouldStore := False;
  For I:=0 To Pred(_VertexCount) Do
  If (Cardinal(_Vertices[I].Color) <> Cardinal(ColorWhite)) Then
  Begin
    ShouldStore := True;
    Break;
  End;

  If ShouldStore Then
  Begin
    Tag := tagVertexColors;
    Size := _VertexCount * SizeOf(Color);
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    For I:=0 To Pred(_VertexCount) Do
      Dest.Write(@_Vertices[I].Color, SizeOf(Color));
  End;

  ShouldStore := False;
  For I:=0 To Pred(_VertexCount) Do
  If (_Vertices[I].BoneIndex>0) Then
  Begin
    ShouldStore := True;
    Break;
  End;

  If ShouldStore Then
  Begin
    Tag := tagVertexBoneIndices;
    Size := _VertexCount;
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    For I:=0 To Pred(_VertexCount) Do
    Begin
      Index := Trunc(_Vertices[I].BoneIndex);
      Dest.Write(@Index, 1);
    End;
  End;

  //tagVertexBoneWeights  = 'VBWE';

  Tag := tagTriangleIndices;
  Size := 4 + _TriangleCount * SizeOf(Triangle);
  Dest.Write(@Tag, 4);
  Dest.Write(@Size, 4);
  Dest.Write(@_TriangleCount, 4);
  For I:=0 To Pred(_TriangleCount) Do
    Dest.Write(@_Triangles[I], SizeOf(Triangle));

  ShouldStore := Length(_TriangleNormals) = _TriangleCount;
  If ShouldStore Then
  Begin
    Tag := tagTriangleNormals;
    Size := _TriangleCount * SizeOf(Vector3D);
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    For I:=0 To Pred(_TriangleCount) Do
      Dest.Write(@_TriangleNormals[I], SizeOf(Vector3D));
  End;

  ShouldStore := Length(_Edges) = _TriangleCount;
  If ShouldStore Then
  Begin
    Tag := tagTriangleEdges;
    Size := _TriangleCount * SizeOf(TriangleEdgesState);
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
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
  Dest.Write(@Size, 4);
  Dest.Write(@_Material.DiffuseColor, SizeOf(Color));
  Dest.WriteString(Name);

  If (Self.BlendMode>=0) Then
  Begin
    Tag := tagMaterialBlendMode;
    Size := 1;
    PU := Byte(_Material.BlendMode);
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    Dest.Write(@PU, 1);
  End;

  If (Self.EmitterFX<>'') Then
  Begin
    Tag := tagMaterialParticles;
    Size := Succ(Length(_EmitterFX));
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    Dest.WriteString(EmitterFX);
  End;

  WriteTexture(tagMaterialTriplanar, _Material.TriplanarMap);
  WriteTexture(tagMaterialSpecular, _Material.SpecularMap);
  WriteTexture(tagMaterialBump, _Material.NormalMap);
  WriteTexture(tagMaterialLightmap, _Material.LightMap);
  WriteTexture(tagMaterialRefraction, _Material.RefractionMap);
  WriteTexture(tagMaterialReflective, _Material.ReflectiveMap);
  WriteTexture(tagMaterialGlow, _Material.GlowMap);
  WriteTexture(tagMaterialAlphaMap, _Material.AlphaMap);
  WriteTexture(tagMaterialEnvMap, _Material.EnviromentMap);
  WriteTexture(tagMaterialRamp, _Material.ColorRamp);

  // morphs
  For J:=0 To Pred(_MorphCount) Do
  Begin
    Tag := tagVertexMorph;
    Size := _VertexCount * 4 + 5;
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);

    Dest.Write(@_Morphs[J].ID, 4);
    Dest.Write(@_Morphs[J].MorphType, 1);

    For I:=0 To Pred(_VertexCount) Do
      Dest.Write(@_Morphs[J].Values[I], SizeOf(Vector3D));
  End;

  ShouldStore := (Flags And meshGroupLinked<>0) And (Length(_Links)>0);
  If ShouldStore Then
  Begin
    Tag := tagVertexLinks;
    Size := _VertexCount * 4;
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    For I:=0 To Pred(_VertexCount) Do
      Dest.Write(@_Links[I], 4);
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
Begin
  SetLength(_TriangleNormals, _TriangleCount);
	For I:=0 To Pred(_TriangleCount) Do
  Begin
    _TriangleNormals[I] := TriangleNormal(_Vertices[_Triangles[I].Indices[0]].Position, _Vertices[_Triangles[I].Indices[1]].Position, _Vertices[_Triangles[I].Indices[2]].Position);
  End;
End;

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
Begin
  SetLength(Tan1, _VertexCount);
  SetLength(Tan2, _VertexCount);

	For I := 0 To Pred(_VertexCount) Do
	Begin
		tan1[i] := VectorZero;
		tan2[i] := VectorZero;
	End;

  For I := 0 To Pred(_TriangleCount) Do
  Begin
    i1 := _Triangles[i].Indices[0];
		i2 := _Triangles[i].Indices[1];
		i3 := _Triangles[i].Indices[2];

    v1 := _Vertices[i1].Position;
		v2 := _Vertices[i2].Position;
		v3 := _Vertices[i3].Position;

    w1 := _Vertices[i1].TextureCoords;
		w2 := _Vertices[i2].TextureCoords;
		w3 := _Vertices[i3].TextureCoords;

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

  For I := 0 To Pred(_VertexCount) Do
  Begin
    n := _Vertices[i].Normal;
    t := tan1[i];
    T.Normalize;

    // Gram-Schmidt orthogonalize
		_Vertices[i].tangent := VectorSubtract(t, VectorScale(n, VectorDot(n, t)));
    _Vertices[i].tangent.Normalize;

    // Calculate handedness
    If (VectorDot( VectorCross(n, t), tan2[i]) < 0.0) Then
      _Vertices[i].Handness := 1.0
    Else
      _Vertices[i].Handness := -1.0;
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
  M:Matrix4x4;
Begin
  If (_VertexCount>0) And (_Vertices<>Nil) Then
  Begin
    If (Length(_Vertices)< _VertexCount) Then
    Begin
      IntToString(2);
      Exit;
    End;
    
    _BoundingBox.StartVertex := _Vertices[0].Position;
    _BoundingBox.EndVertex := _Vertices[0].Position;

    {For I:=0 To Pred(_VertexCount) Do
    Begin
      If (_Owner.Skeleton.BoneCount > 0 ) And (Length(_Owner.Skeleton.BindPose)>0) Then
      Begin
        N := Trunc(Vertices[I].BoneIndex);
        M := _Owner.Skeleton.BindPose[N];
        P := M.Transform(_Vertices[I].Position);
      End Else
        P := _Vertices[I].Position;}


    For I:=1 To Pred(_VertexCount) Do
    Begin
      P := _Vertices[I].Position;
      _BoundingBox.StartVertex.x := FloatMin(_BoundingBox.StartVertex.x, P.X);
      _BoundingBox.StartVertex.y := FloatMin(_BoundingBox.StartVertex.y, P.Y);
      _BoundingBox.StartVertex.z := FloatMin(_BoundingBox.StartVertex.z, P.Z);
      _BoundingBox.EndVertex.x := FloatMax(_BoundingBox.EndVertex.x, P.X);
      _BoundingBox.EndVertex.y := FloatMax(_BoundingBox.EndVertex.y, P.Y);
      _BoundingBox.EndVertex.z := FloatMax(_BoundingBox.EndVertex.z, P.Z);
    End;
  End;
End;

Function MeshGroup.Intersect(const R: Ray; var T: Single; Const Transform:Matrix4x4): Boolean;
Var
  I:Integer;
  V0,V1,V2:Vector3D;
  U,V:Single;
Begin
  For I:=0 To Pred(_TriangleCount) Do
  Begin
    V0 := _Vertices[_Triangles[I].Indices[0]].Position;
    V1 := _Vertices[_Triangles[I].Indices[1]].Position;
    V2 := _Vertices[_Triangles[I].Indices[2]].Position;

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


Procedure MeshGroup.SetupUniforms(Transform:Matrix4x4; State:MeshInstance; Outline:Boolean; Const Material:MeshMaterial);
Var
  I:Integer;
  TextureMatrix, M, M2:Matrix4x4;
  C:Color;
  BoneVectorLocation:Integer;
  BoneVectors:Array[0..(Succ(MaxBones)*3)] Of Vector4D;
  M2D:Matrix3x3;
  Bend,Delta:Single;

  Procedure UploadBoneMatrix(ID:Integer; Mat:Matrix4x4);
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
  If (GraphicsManager.Instance.ReflectionActive) Then
    Transform := Matrix4x4Multiply4x4(GraphicsManager.Instance.ReflectionMatrix, Transform);

  If Assigned(State) Then
    TextureMatrix := State._UVTransforms[Self._ID]
  Else
    TextureMatrix := Matrix4x4Identity;

  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
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
  End;
  {$ENDIF}

  _Shader.SetUniform('modelMatrix', Transform);
  _Shader.SetUniform('textureMatrix', TextureMatrix);

  GraphicsManager.Instance.ActiveViewport.Camera.SetupUniforms;

  {$IFDEF REFLECTIONS_WITH_STENCIL}
  If (GraphicsManager.Instance.ReflectionStencil) Then
  Begin
    _Shader.SetUniform('targetColor', ColorWhite);
  End Else
  {$ENDIF}
  If (GraphicsManager.Instance.RenderStage = renderStageOutline) Then
  Begin
    _Shader.SetUniform('targetColor', Material.OutlineColor);
  End Else
  If (Outline) Then
  Begin
    _Shader.SetUniform('outlineScale', 0.4);
    _Shader.SetUniform('outlineColor', Material.OutlineColor);
  End;

  If (Self.Flags And meshGroupVegetation<>0) And (Material.VegetationBend>0)  Then
  Begin
    Delta := GetTime() + (Cardinal(Self) Mod 35) * 500;
    Delta := Delta/1000;

    _Shader.SetUniform('vegetationBase', _BoundingBox.StartVertex.Y);
    _Shader.SetUniform('vegetationSize', _BoundingBox.EndVertex.Y - _BoundingBox.StartVertex.Y);
    _Shader.SetUniform('vegetationTime', Delta);
    _Shader.SetUniform('vegetationBend', Material.VegetationBend);
  End;

  If (AmbientColor.R > 0) Or (AmbientColor.G > 0) Or (AmbientColor.B>0) Then
  Begin
    _Shader.SetUniform('ambient_color', AmbientColor);
  End;

  If (_Owner.Skeleton.BoneCount > 0 ) And (Assigned(State)) Then
  Begin
    BoneVectorLocation := _Shader.GetUniform('boneVectors');
    If (BoneVectorLocation<0) Then
      Exit;

    UploadBoneMatrix(0, Matrix4x4Identity);

    If (_Owner.Skeleton.BoneCount>MaxBones) Then
    Begin
      Log(logWarning, 'Mesh', 'Bone limit reached, '+IntToString(_Owner.Skeleton.BoneCount)+' bones'
        + ', mesh name = "' + _Owner.Name + '"');
      Exit;
    End;

    For I:=1 To _Owner.Skeleton.BoneCount Do
    If (State.Animation = Nil) Or (State.Animation.Root = Nil) Then
      UploadBoneMatrix(I, _Owner.Skeleton.BindPose[I])
    Else
      UploadBoneMatrix(I, State.Animation.Transforms[I]);

    glUniform4fv(BoneVectorLocation, Succ(_Owner.Skeleton.BoneCount)*3, @(BoneVectors[0]));
  End;
End;


Procedure MeshGroup.SetCombineWithColor(C:Color);
Var
  CC:Array[0..3] Of Single;
Begin
  {$IFDEF PC}
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_TEXTURE_2D);
  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE );

  glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PRIMARY_COLOR);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_TEXTURE0);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR );
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);

  glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_ALPHA, GL_MODULATE);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA, GL_PRIMARY_COLOR);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA, GL_TEXTURE0);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND0_ALPHA, GL_SRC_ALPHA);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND1_ALPHA, GL_SRC_ALPHA);

  TextureManager.Instance.WhiteTexture.Bind(1);
  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE );

  CC[0] := C.R/255;
  CC[1] := C.G/255;
  CC[2] := C.B/255;
  CC[3] := C.A/255;
  glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @CC);

  glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PREVIOUS);
  glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_CONSTANT );
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_CONSTANT);

  glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_ALPHA, GL_MODULATE);
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA, GL_PREVIOUS);
  glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE1_ALPHA, GL_CONSTANT );
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND0_ALPHA, GL_SRC_ALPHA);
  glTexEnvi( GL_TEXTURE_ENV, GL_OPERAND1_ALPHA, GL_CONSTANT);

   {$ENDIF}
End;

Function MeshGroup.Render(Const Transform:Matrix4x4; State:MeshInstance):Boolean;
Var
  UseOutline, ShowWireframe:Boolean;
  I,J,K, PassCount:Integer;
  Tex:Texture;
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

  If (Graphics.RenderStage = renderStageOutline) {$IFNDEF DISABLEOUTLINES} And (Not Graphics.Settings.Outlines.Enabled) {$ENDIF} Then
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
    ShowWireframe := State._Wireframe[Self.ID];

  If (_VertexCount<=0) Or (Hidden) Then
    Exit;

  If Assigned(State) Then
  Begin
    Self.InheritMaterial(State._Materials[Self.ID], DestMaterial);
    DestMaterial.DiffuseColor := ColorScale(DestMaterial.DiffuseColor, State.Diffuse);

    If (Self.Flags And meshGroupReflective<>0) Then
      DestMaterial.ReflectionMap := TextureManager.Instance.WhiteTexture;
  End Else
    DestMaterial := _Material;

  If (DestMaterial.DiffuseColor.A<=0) Then
    Exit;

  If (_NeedsSkeletonSetup) Then
    Self.SetupSkeleton();

  Self.ResolveLinks();

  AlwaysOnTop := False;

  If (Assigned(State)) Then
    AlwaysOnTop := State.AlwaysOnTop;

  If Graphics.ReflectionActive Then
    AlwaysOnTop := True;

  If (Self.Flags And meshGroupOverrideAmbient<>0) Then
    AmbientColor := Self.AmbientColor
  Else
    AmbientColor := LightManager.Instance.AmbientColor;

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
  And (GraphicsManager.Instance.Settings.Outlines.Enabled)
  And (Not Graphics.ReflectionActive)
  {$IFDEF POSTPROCESSING}
  And (Not Graphics.ActiveViewport.IsRenderTargetEnabled(captureTargetNormal))
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

  If (Graphics.Settings.Shaders.Avaliable) Then
  Begin
    If Assigned(State) And (State.CustomShader<>Nil) Then
      _Shader := State.CustomShader
    Else
    Begin
      Self._Shader := SelectMeshShader(Self, Transform.GetTranslation(), UseOutline, DestMaterial);
    End;
    
    If (Assigned(_Shader)) And (Not _Shader.IsReady) Or (_Shader = Nil) Then
      Exit;

    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'MeshGroup', 'Binding shader');
    {$ENDIF}
    ShaderManager.Instance.Bind(_Shader);
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
        _Shader.SetUniform('glowMap', 0);
    End Else
    If (Graphics.RenderStage = renderStageReflection) Then
    Begin
      Slot := 0;

      If (DestMaterial.FlowMap<>Nil) Then
        IntToString(2);

      BindMaterial(Slot, DestMaterial);

      If Assigned(_Shader) Then
      Begin
        _Shader.SetUniform('reflectionFactor', 0.25);
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

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setting texture wrap mode');  {$ENDIF}
      Tex.Wrap := True;

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Binding texture');  {$ENDIF}
      //glDisable(GL_TEXTURE_2D);
      Tex.Bind(0);

      If Assigned(_Shader) Then
      Begin
        _Shader.SetUniform('diffuseMap', 0);
        If Not UseOutline Then
          BindMaterial(Slot, DestMaterial);
      End Else
      Begin
        SetCombineWithColor(DestMaterial.DiffuseColor);
      End;

    End;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setuping uniforms'); {$ENDIF}
    LightManager.Instance.SetupUniforms(@_LightBatch, Slot);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setting uniform properties');  {$ENDIF}
    If Assigned(_Shader) Then
    Begin
      _Shader.SetUniform('diffuse_color', DestMaterial.DiffuseColor);
     // _Shader.SetUniform('specular_power', DestMaterial.SpecularPower);
      _Shader.SetUniform('specular_color', ColorWhite);
    End;

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setup mesh uniforms');  {$ENDIF}
    SetupUniforms(Transform, State, UseOutline, DestMaterial);
   
    {$IFDEF EDITOR}
    If (Flags And mgCullFace<>0) Then
      Flags := Flags Xor mgCullFace; // disable culling in edit mode
    {$ENDIF}

    If (UseOutline) Then
    Begin
      glCullFace(GL_FRONT);
      glEnable(GL_CULL_FACE);

      Graphics.SetBlendMode(blendNone);
    End Else
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Setting blending mode');  {$ENDIF}


      {$IFNDEF REFLECTIONS_WITH_STENCIL}
      If (Graphics.ReflectionActive) Then
        Graphics.SetBlendMode(blendBlend)
      Else
      {$ENDIF}
      If (Graphics.RenderStage = renderStageDiffuse) {Or (Graphics.RenderStage = renderStageOutline)} Then
        Graphics.SetBlendMode(DestMaterial.BlendMode)
      Else
        Graphics.SetBlendMode(blendNone);

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
        glDisable(GL_CULL_FACE);
    End;

    If (Flags And meshGroupDepthOff<>0) Or (UseOutline) Then
      glDepthMask(False);

    If (Flags And meshGroupSkybox<>0) Then
      glDepthRange(0.9999,1);

    If AlwaysOnTop Then
      glDisable(GL_DEPTH_TEST);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Drawing geometry');  {$ENDIF}
    Self.DrawGeometry(State, ShowWireframe);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Undoing properties');  {$ENDIF}
    If (UseOutline) Then
      glCullFace(GL_BACK);

    If (Graphics.RenderStage<>renderStageShadow) And (Flags And meshGroupDoubleSided<>0) And (Not Graphics.ReflectionActive) Then
      glEnable(GL_CULL_FACE);

    If (Flags And meshGroupDepthOff<>0) Or (UseOutline) Then
      glDepthMask(True);

    If AlwaysOnTop Then
      glEnable(GL_DEPTH_TEST);

    If (Flags And meshGroupSkybox<>0) Then
      glDepthRange(0.0,1);

    {$IFDEF PC}
    If (Not Graphics.Settings.Shaders.Avaliable) Then
    Begin
      glActiveTexture(GL_TEXTURE1);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      glDisable(GL_TEXTURE_2D);

      glActiveTexture(GL_TEXTURE0);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    End;
    {$ENDIF}

{  If (MeshInstance(Instance).OutlineWidth>0.0) Then
  Begin
    glCullFace(GL_FRONT);
    glColor4ub(0, 0, 0, 255);
    SM := MeshInstance(Instance).OutlineWidth;
    glScalef(SM, SM, SM);
    Self.DrawGeometry(Transform, Instance);
    SM := 1/SM;
    glScalef(SM, SM, SM);
    glCullFace(GL_BACK);
  End;}

  Result := True;
End;

Procedure MeshGroup.SetTriplanarMap(const Value: Texture);
Var
  N:Integer;
Begin
  _Material.TriplanarMap := Value;
  If ((Self.Flags And meshGroupTriplanar=0) = Assigned(Value)) Then
  Begin
    Self.Flags := Self.Flags Xor meshGroupTriplanar;
  End;
End;

Procedure MeshGroup.SetAlphaMap(Map:Texture);
Var
  N:Cardinal;
Begin
  _Material.AlphaMap := Map;
  If ((Self.Flags And meshGroupAlphaMap=0) = Assigned(Map)) Then
  Begin
    N := Self.Flags;
    Self.Flags := Self.Flags Xor meshGroupAlphaMap;

    If Assigned(Map) Then
      Self.Flags := Self.Flags Or meshGroupTransparency;

    If (Self.Flags<>N) Then
    Begin
      If Assigned(_Buffer) Then
      Begin
        _Buffer.Destroy();
        _Buffer := Nil;
      End;
    End;
  End;
End;

Procedure MeshGroup.SetLightMap(Map:Texture);
Var
  N:Cardinal;
Begin
  _Material.LightMap := Map;
  If ((Self.Flags And meshGroupLightMap=0) = Assigned(Map)) Then
  Begin
    N := Self.Flags;
    Self.Flags := Self.Flags Xor meshGroupLightMap;

    If (Self.Flags <>N) Then
    Begin
      If Assigned(_Buffer) Then
      Begin
        _Buffer.Destroy();
        _Buffer := Nil;
      End;
    End;
  End;
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

  If Assigned(_Buffer) Then
  Begin
    _Buffer.Destroy();
    _Buffer := Nil;
  End;
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

Procedure MeshGroup.BindMaterial(Var Slot: Integer; Const Material:MeshMaterial);
Var
  Tex:Texture;
  FlowCycle:Vector3D;
Begin
  Slot := 1;

  If (Material.ColorTable<>Nil) Then
  Begin
    ColorTableBind(Material.ColorTable, Slot);
    Inc(Slot);
  End;

  If (Assigned(_Shader)) And (Assigned(Material.DecalMap)) And (_Shader.HasUniform(DecalMapUniformName)) Then
  Begin
    Material.DecalMap.Bind(Slot);
    _Shader.SetUniform(DecalMapUniformName, Slot);

    Inc(Slot);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing triplanar');  {$ENDIF}
  If (Self.Flags And meshGroupTriplanar<>0)  Then
  Begin
    Tex := Material.TriplanarMap;

    If Not Assigned(Tex) Then
      Tex := Material.DiffuseMap;

    Tex.Wrap := True;
    Tex.Bind(Slot);
    If Assigned(_Shader) Then
      _Shader.SetUniform('diffuseMap2', Slot);

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
    _Shader.SetUniform(NormalMapUniformName, Slot);

    Inc(Slot);
  End;

	{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing specular map');  {$ENDIF}
	If (Assigned(Material.SpecularMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(SpecularMapUniformName)) Then
	Begin
    If (Assigned(Material.SpecularMap)) Then
  	  Material.SpecularMap.Bind(Slot)
    Else
      TextureManager.Instance.BlackTexture.Bind(Slot);

    _Shader.SetUniform(SpecularMapUniformName, Slot);

	  Inc(Slot);
	End;

	{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing color ramp');  {$ENDIF}
	If (Assigned(Material.ColorRamp)) And (Assigned(_Shader)) And (_Shader.HasUniform(ColorRampUniformName)) Then
	Begin
    Material.ColorRamp.Wrap := False;
	  Material.ColorRamp.Bind(Slot);
    _Shader.SetUniform(ColorRampUniformName, Slot);
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
		_Shader.SetUniform('lightMap', Slot);
	  Inc(Slot);
	End;

	{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Testing alphamap');  {$ENDIF}
	If (Assigned(Material.Alphamap)) And (Assigned(_Shader)) And (_Shader.HasUniform(AlphaMapUniformName)) Then
	Begin
	  Material.Alphamap.Bind(Slot);
    _Shader.SetUniform(AlphaMapUniformName, Slot);

	  Inc(Slot);
	End;

  If (Assigned(Material.ReflectiveMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(ReflectiveMapUniformName)) Then
  Begin
	  Material.ReflectiveMap.Bind(Slot);
    _Shader.SetUniform(ReflectiveMapUniformName, Slot);
	  Inc(Slot);
  End;

  If (Assigned(Material.ReflectionMap)) And (Assigned(_Shader))  And (_Shader.HasUniform(ReflectionMapUniformName)) Then
  Begin
    Material.ReflectionMap.Bind(Slot);
    _Shader.SetUniform(ReflectionMapUniformName, Slot);

    Inc(Slot);
  End;

  If (Assigned(Material.EnviromentMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(SphereMapUniformName)) Then
  Begin
	  Material.EnviromentMap.Bind(Slot);
    _Shader.SetUniform(SphereMapUniformName, Slot);

	  Inc(Slot);
  End;

  If (Assigned(Material.FlowMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(FlowMapUniformName)) Then
  Begin
    Material.FlowMap.Wrap := False;
    Material.FlowMap.BilinearFilter := True;
    Material.FlowMap.MipMapped := False;
	  Material.FlowMap.Bind(Slot);

    FlowCycle.Z := 0; //FloatMod(GetTime, 1000);
    FlowCycle.X := FloatMod(GetTime, 1000) / 2000;
    FlowCycle.Y := 2 * Abs(FlowCycle.X - 0.5);

		_Shader.SetUniform(FlowMapUniformName, Slot);
    _Shader.SetUniform('flowSpeed', Material.FlowSpeed);
    _Shader.SetUniform('flowCycle', FlowCycle);
    _Shader.SetUniform('flowBounds', Material.FlowBounds);

	  Inc(Slot);
  End;

  If (Assigned(Material.NoiseMap)) And (Assigned(_Shader)) And (_Shader.HasUniform(NoiseMapUniformName)) Then
  Begin
	  Material.NoiseMap.Bind(Slot);
    _Shader.SetUniform(NoiseMapUniformName, Slot);
	  Inc(Slot);
  End;

  If (GraphicsManager.Instance.ReflectionActive) Then
  Begin
    If Assigned(GraphicsManager.Instance.ReflectionMask) Then
      GraphicsManager.Instance.ReflectionMask.Bind(Slot)
    Else
      TextureManager.Instance.BlackTexture.Bind(Slot);

    If Assigned(_Shader) Then
      _Shader.SetUniform('screenMask', Slot);
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

  SetFlag(Self.Flags, meshGroupTransparency, NeedTransparency);
End;

Constructor MeshGroup.Create(ID: Integer; Parent: Mesh; Name:AnsiString);
Begin
  If Name='' Then
    Name := 'group'+IntToString(ID);

  Self._Name := Name;
  Self._ID := ID;
  Self._Owner := Parent;

  Self._Material.Reset();
  Self._Unique := False;

  Self._NeedsSkeletonSetup := True;
End;

Function MeshGroup.LockVertices(Static:Boolean): PMeshVertex;
Var
  I:Integer;
  P:PMeshVertex;
Begin
  If (_VertexCount<=0) Or (_Vertices = Nil) Then
  Begin
    Result := Nil;
    Exit;
  End;

  {If (Assigned(Self._Buffer)) Then
  Begin
    Result := Self._Buffer.Lock();
  End Else}
  Begin
    Result := @(_Vertices[0]);
  End;

  If (_ScratchVertices = Nil) Then
  Begin
    SetLength(_ScratchVertices, _VertexCount);
    For I:=0 To Pred(_VertexCount) Do
      _ScratchVertices[I] := _Vertices[I];
  End Else
  Begin
    P := Result;
    If (Static) Then
    Begin
      I:=0;
      While I<_VertexCount Do
      Begin
        P^ := _ScratchVertices[I];
        Inc(I);
        Inc(P);
      End;
    End;
  End;
End;

Procedure MeshGroup.UnlockVertices;
Begin
  If (Assigned(Self._Buffer)) Then
  Begin
    //Self._Buffer.Unlock();
    Self._Buffer.Update(@_Vertices[0]);
  End;
End;

Procedure MeshGroup.SetVertexLink(VertexIndex, TargetGroup, TargetVertex: Integer);
Begin
  If (Length(_Links)<_VertexCount) Then
    SetLength(_Links, _VertexCount);

  If (TargetVertex<0) Then
    TargetVertex := 0;

  _Links[VertexIndex].GroupIndex := TargetGroup;
  _Links[VertexIndex].VertexIndex := TargetVertex;
End;

Procedure MeshGroup.ResolveLinks;
Var
  I,ID, N:Integer;
  P:Vector3D;
Begin
  If (Self.Flags And meshGroupLinked = 0) Then
    Exit;

  Self.Flags := Self.Flags Xor meshGroupLinked;

  If (Length(_Links)<_VertexCount) Then
    Exit;

  For I:=0 To Pred(_VertexCount) Do
  Begin
    N := _Links[I].GroupIndex;
    If N<0 Then
      Continue;

    ID := _Links[I].VertexIndex;
    If (ID>=_Owner._Groups[N]._VertexCount) Then
      Continue;

    P := _Owner._Groups[N]._Vertices[ID].Position;
    _Vertices[I].Position.Add(P);
  End;
End;

Procedure MeshGroup.SetVertexMorph(ID, VertexIndex:Integer; Value:Vector3D);
Var
  N, I, Len:Integer;
Begin
  If (VertexIndex<0) Or (VertexIndex>=_VertexCount) Then
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
  If (VertexIndex<0) Or (VertexIndex>=_VertexCount) Then
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

  Result := _Vertices[I].Position;
End;

Function MeshGroup.AddVertexMorph(ID:Integer):Integer;
Begin
  Result := _MorphCount;
  Inc(_MorphCount);
  SetLength(_Morphs, _MorphCount);

  _Morphs[Result].ID := ID;
  _Morphs[Result].MorphType := 0;
  SetLength(_Morphs[Result].Values, _VertexCount);
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

  SetLength(Visibility, _VertexCount);

  _VisibleTriangleCount := 0;
  For I:=0 To Pred(Self._VertexCount) Do
  Begin
    P := Transform.Transform(_Vertices[I].Position);
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

Function MeshGroup.GetAlphaMap:Texture;
Begin
  Result := _Material.AlphaMap;
End;

Function MeshGroup.GetAmbientColor: Color;
Begin
  Result := _Material.AmbientColor;
End;

Function MeshGroup.GetColorRamp: Texture;
Begin
  Result := _Material.ColorRamp;
End;

Function MeshGroup.GetDecalMap: Texture;
Begin
  Result := _Material.DecalMap;
End;

Function MeshGroup.GetDiffuseColor: Color;
Begin
  Result := _Material.DiffuseColor;
End;

Function MeshGroup.GetDiffuseMap: Texture;
Begin
  Result := _Material.DiffuseMap;
End;

function MeshGroup.GetGlowMap: Texture;
Begin
  Result := _Material.GlowMap;
End;

Function MeshGroup.GetLightMap: Texture;
Begin
  Result := _Material.LightMap;
End;

Function MeshGroup.GetNormalMap: Texture;
Begin
  Result := _Material.NormalMap;
End;

Function MeshGroup.GetRefractionMap: Texture;
Begin
  Result := _Material.RefractionMap;
End;

Function MeshGroup.GetReflectiveMap: Texture;
Begin
  Result := _Material.ReflectiveMap;
End;

Function MeshGroup.GetEnviromentMap: Texture;
Begin
  Result := _Material.EnviromentMap;
End;

Function MeshGroup.GetFlowMap: Texture;
Begin
  Result := _Material.FlowMap;
End;

Function MeshGroup.GetNoiseMap: Texture;
Begin
  Result := _Material.NoiseMap;
End;

Function MeshGroup.GetSpecularMap: Texture;
Begin
  Result := _Material.SpecularMap;
End;

Function MeshGroup.GetTriplanarMap:Texture;
Begin
  Result := _Material.TriplanarMap;
End;

procedure MeshGroup.SetAmbientColor(const Value: Color);
Begin
  _Material.AmbientColor := Value;
End;

procedure MeshGroup.SetDecalMap(const Value: Texture);
Begin
  _Material.DecalMap := Value;
End;

Procedure MeshGroup.SetDiffuseColor(const Value: Color);
Begin
  _Material.DiffuseColor := Value;
End;

procedure MeshGroup.SetDiffuseMap(const Value: Texture);
Begin
  _Material.DiffuseMap := Value;
End;

procedure MeshGroup.SetGlowMap(const Value: Texture);
Begin
  _Material.GlowMap := Value;
End;

Procedure MeshGroup.SetNormalMap(const Value: Texture);
Begin
  _Material.NormalMap := Value;
End;

procedure MeshGroup.SetRefractionMap(const Value: Texture);
Begin
  _Material.RefractionMap := Value;
End;

procedure MeshGroup.SetReflectiveMap(const Value: Texture);
Begin
  _Material.ReflectiveMap := Value;
End;

procedure MeshGroup.SetEnviromentMap(const Value: Texture);
Begin
  _Material.EnviromentMap := Value;
End;

procedure MeshGroup.SetFlowMap(const Value: Texture);
Begin
  _Material.FlowMap := Value;
End;

procedure MeshGroup.SetNoiseMap(const Value: Texture);
Begin
  _Material.NoiseMap := Value;
End;

Procedure MeshGroup.SetSpecularMap(const Value: Texture);
Begin
  _Material.SpecularMap := Value;
End;

Procedure MeshGroup.SetColorRamp(const Map: Texture);
Begin
  _Material.ColorRamp := Map;
End;

function MeshGroup.GetBlendMode: Integer;
Begin
  Result := _Material.BlendMode;
End;

Procedure MeshGroup.InheritMaterial(Const OtherMat: MeshMaterial; Var DestMaterial: MeshMaterial);
  Function SelectTexture(A,B,C:Texture):Texture;
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
  Transparency := (Flags And meshGroupTransparency<>0) Or (DestMaterial.DiffuseColor.A<255);

  DestMaterial.DiffuseColor := ColorScale(_Material.DiffuseColor, OtherMat.DiffuseColor);
  DestMaterial.AmbientColor := ColorScale(_Material.AmbientColor, OtherMat.AmbientColor);

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

  If (Self.Flags And meshGroupIgnoreColorTable<>0) Then
    DestMaterial.ColorTable := Nil
  Else
    DestMaterial.ColorTable := SelectTexture(OtherMat.ColorTable, _Material.ColorTable, Nil);

  DestMaterial.DiffuseMap := SelectTexture(OtherMat.DiffuseMap, _Material.DiffuseMap, TextureManager.Instance.WhiteTexture);
  DestMaterial.DecalMap := SelectTexture(OtherMat.DecalMap, _Material.DecalMap, Nil);
  DestMaterial.TriplanarMap := SelectTexture(OtherMat.TriplanarMap, _Material.TriplanarMap, DestMaterial.DiffuseMap);
  DestMaterial.NormalMap := SelectTexture(OtherMat.NormalMap, _Material.NormalMap, Nil);
  DestMaterial.SpecularMap := SelectTexture(OtherMat.SpecularMap, _Material.SpecularMap, TextureManager.Instance.BlackTexture);
  DestMaterial.GlowMap := SelectTexture(OtherMat.GlowMap, _Material.GlowMap, TextureManager.Instance.BlackTexture);
  DestMaterial.RefractionMap := SelectTexture(OtherMat.RefractionMap, _Material.RefractionMap, TextureManager.Instance.BlackTexture);
  DestMaterial.ReflectiveMap := SelectTexture(OtherMat.ReflectiveMap, _Material.ReflectiveMap, Nil);
  DestMaterial.AlphaMap := SelectTexture(OtherMat.AlphaMap, _Material.AlphaMap, TextureManager.Instance.WhiteTexture);
  DestMaterial.LightMap := SelectTexture(OtherMat.LightMap, _Material.LightMap, TextureManager.Instance.WhiteTexture);
  DestMaterial.ColorRamp := SelectTexture(OtherMat.ColorRamp, _Material.ColorRamp, GraphicsManager.Instance.ColorRamp);
  DestMaterial.FlowMap := SelectTexture(OtherMat.FlowMap, _Material.FlowMap, Nil);
  DestMaterial.NoiseMap := SelectTexture(OtherMat.NoiseMap, _Material.NoiseMap, Nil);
  DestMaterial.ReflectionMap := TextureManager.Instance.BlackTexture;

  If Assigned(DestMaterial.NormalMap) Then
    DestMaterial.NormalMap.Wrap := True;

  If Assigned(DestMaterial.NoiseMap) Then
    DestMaterial.NoiseMap.Wrap := True;

  DestMaterial.VegetationBend := OtherMat.VegetationBend;
  DestMaterial.Ghost := OtherMat.Ghost;

  DestMaterial.EnviromentMap := GraphicsManager.Instance.EnviromentMap;

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

{ Mesh }
Class Function Mesh.GetManager: Pointer;
Begin
  Result := MeshManager.Instance;
End;

Procedure Mesh.OnContextLost;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
    _Groups[I].OnContextLost();

  Self._ContextID := Application.Instance.ContextID;
End;

Function Mesh.GetGroup(Index:Integer):MeshGroup;
Begin
  If (Index>=0) And (Index<_GroupCount) Then
    Result := _Groups[Index]
  Else
    Result := Nil;
End;

Function Mesh.GetGroup(Name:AnsiString):MeshGroup;
Var
	I:Integer;
Begin
  Self.Prefetch();

	For I:=0 To Pred(_GroupCount) Do
	If UpStr(Name)=UpStr(_Groups[I]._Name) Then
	Begin
		Result := _Groups[I];
		Exit;
	End;

	Result := Nil;
End;

Function Mesh.GetGroupIndex(Name:AnsiString):Integer;
Var
	I:Integer;
Begin
	For I:=0 To Pred(_GroupCount) Do
	If UpStr(Name)=UpStr(_Groups[I]._Name) Then
	Begin
		Result := I;
		Exit;
	End;

	Result := -1;
End;


Function Mesh.AddGroup(Name:AnsiString=''):MeshGroup;
Begin
	Inc(_GroupCount);
	SetLength(_Groups, _GroupCount);

	Result := MeshGroup.Create(Pred(_GroupCount), Self, Name);
  _Groups[Pred(_GroupCount)] := Result;
End;

Function Mesh.DuplicateGroup(Group:MeshGroup; Name:AnsiString=''):MeshGroup;
Var
  I:Integer;
Begin
  Result := Self.AddGroup(Name);
  If Group = Nil Then
    Exit;

  Result.Flags := Group.Flags;
  Result._Material := Group._Material;

  Result._VertexCount := Group._VertexCount;
  SetLength(Result._Vertices, Group._VertexCount);
  For I:=0 To Pred(Group._VertexCount) Do
    Result._Vertices[I] := Group._Vertices[I];

  Result._TriangleCount := Group._TriangleCount;
  Result._VisibleTriangleCount := Group._TriangleCount;
  SetLength(Result._Triangles, Group._TriangleCount);
  For I:=0 To Pred(Group._TriangleCount) Do
    Result._Triangles[I] := Group._Triangles[I];

  Result._BoundingBox := Group._BoundingBox;
End;

Procedure Mesh.AddTriangle(A,B,C:MeshVertex; Group:MeshGroup);
Begin
	Group.AddTriangle(A,B,C);
End;

Procedure Mesh.AddQuad(A,B,C,D:MeshVertex; Group:MeshGroup);
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
Procedure Mesh.Clean;
Var
	I:Integer;
Begin
	For I:=0 To Pred(_GroupCount) Do
  Begin
		_Groups[I].Clean();
    _Groups[I].Destroy();
    _Groups[I] := Nil;
  End;
	_GroupCount := 0;
	_Groups := Nil;

	For I:=0 To Pred(_MetadataCount) Do
  Begin
		_Metadata[I].Destroy();
    _Metadata[I] := Nil;
  End;
  _Metadata := Nil;
  _MetadataCount := 0;

	For I:=0 To Pred(_EmitterCount) Do
  Begin
		_Emitters[I].Destroy();
    _Emitters[I] := Nil;
  End;
  _Emitters := Nil;
  _EmitterCount := 0;

  If Assigned(_Skeleton) Then
  Begin
    _Skeleton.Destroy;
    _Skeleton := Nil;
  End;
End;

Function Mesh.GetSkeleton:MeshSkeleton;
Begin
  If Not Assigned(_Skeleton) Then
  Begin
    _Skeleton := MeshSkeleton.Create;
    _Skeleton.Name := Self.Name;
  End;

  Result := _Skeleton;
End;

Function Mesh.Load(Source:Stream):Boolean;
Var
  Size:Integer;
  Tag:FileHeader;
  Handler:MeshDataBlockHandler;
Begin
  _GroupCount := 0;

  Result := False;
  Log(logDebug, 'Mesh', 'Loading mesh: '+Self._Name);

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

Function Mesh.Save(Dest:Stream):Boolean;
Var
  I,J, Size, Temp, Temp2, Count:Integer;
  Tag:FileHeader;
  S:AnsiString;
Begin
  Self.UpdateBoundingBox;

  Tag := tagMeshHeader;
  Dest.Write(@Tag, 4);

  For I:=0 To Pred(_GroupCount) Do
  Begin
    Tag := tagMeshGroup;
    Size := 0;
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
    Temp := Dest.Position;
    _Groups[I].Save(Dest);
    Temp2 := Dest.Position;
    Size := Temp2-Temp;
    Dest.Seek(Temp - 4);
    Dest.Write(@Size, 4);
    Dest.Seek(Temp2);
  End;

  Tag := tagMeshSkeleton;
  Size := 0;
  Dest.Write(@Tag, 4);
  Dest.Write(@Size, 4);
  Temp := Dest.Position;
  Skeleton.Write(Dest);
  Temp2 := Dest.Position;
  Size := Temp2-Temp;
  Dest.Seek(Temp - 4);
  Dest.Write(@Size, 4);
  Dest.Seek(Temp2);

  For I:=0 To Pred(_MetadataCount) Do
  Begin
    Tag := tagMeshMetadata;
    Size := Succ(Length(_Metadata[I].Name)) + Succ(Length(_Metadata[I].Content)) + SizeOf(Vector3D);
    Dest.Write(@Tag, 4);
    Dest.Write(@Size, 4);
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
    Dest.Write(@Size, 4);
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
    Dest.Write(@Size, 4);
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
    Dest.Write(@Size, 4);

    Dest.Write(@_BoneMorphs[I].MorphID, 4);
    Dest.Write(@_BoneMorphs[I].MorphType, 1);
    For J:=0 To Pred(Count) Do
      Dest.Write(@_BoneMorphs[I].Values[J], SizeOf(Vector3D));
  End;

  Tag := tagMeshEnd;
  Dest.Write(@Tag, 4);

  Result := True;
End;

Function Mesh.Intersect(const R: Ray; var T:Single; Const Transform:Matrix4x4): Boolean;
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

Function Mesh.Unload:Boolean;
Begin
  Clean;
  Result := True;
End;


Procedure Mesh.UpdateBoundingBox;
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

Function Mesh.Update:Boolean;
Var
	I:Integer;
Begin
  Inherited Update();

	For I:=0 To Pred(_GroupCount) Do
		_Groups[I].Init;

  UpdateBoundingBox;
  Result := True;
End;


Function Mesh.PolyCount:Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_GroupCount) Do
    Inc(Result, _Groups[I].TriangleCount);
End;

Constructor Mesh.CreateFromFilter(Source:MeshFilter);
Var
  I, J, N:Integer;
  Format:Integer;
  S:AnsiString;
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
    _Groups[N] := MeshGroup.Create(N, Self, Source.GetGroupName(N));
    _Groups[N].Flags := Source.GetGroupFlags(N);
    _Groups[N]._VertexCount := Source.GetVertexCount(N);
    _Groups[N]._TriangleCount := Source.GetTriangleCount(N);
    _Groups[N]._VisibleTriangleCount := _Groups[N]._TriangleCount;
    SetLength(_Groups[N]._Vertices, _Groups[N]._VertexCount);

    Format := Source.GetVertexFormat(N);

    For I:=0 To Pred(_Groups[N]._VertexCount) Do
    Begin
      _Groups[N]._Vertices[I].Position := Source.GetVertexPosition(N, I);

      If (Format And meshFormatBone<>0) Then
        _Groups[N]._Vertices[I].BoneIndex := Source.GetVertexBone(N, I)
      Else
        _Groups[N]._Vertices[I].BoneIndex := -1;

      If (Format And meshFormatNormal<>0) Then
        _Groups[N]._Vertices[I].Normal := Source.GetVertexNormal(N, I);

      If (Format And meshFormatUV1<>0) Then
        _Groups[N]._Vertices[I].TextureCoords := Source.GetVertexUV(N, I);

      If (Format And meshFormatUV2<>0) Then
        _Groups[N]._Vertices[I].TextureCoords2 := Source.GetVertexUV2(N, I);

      If (Format And meshFormatColor<>0) Then
        _Groups[N]._Vertices[I].Color := Source.GetVertexColor(N, I)
      Else
        _Groups[N]._Vertices[I].Color := ColorWhite;

      If (Format And meshFormatTangent<>0) Then
      Begin
        _Groups[N]._Vertices[I].Tangent := Source.GetVertexTangent(N, I);
        _Groups[N]._Vertices[I].Handness := Source.GetVertexHandness(N, I);
      End;
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

    If ((Format And meshFormatTangent)=0) Then
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

Function Mesh.GetEmitter(Index:Integer):MeshEmitter;
Begin
  If (Index<0) Or (Index>=_EmitterCount) Then
    Result := Nil
  Else
    Result := (_Emitters[Index]);
End;

Function Mesh.AddEmitter(Name:AnsiString; Position: Vector3D; Content:AnsiString; ParentBone:AnsiString):MeshEmitter;
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

Function Mesh.GetLight(Index:Integer):MeshLight;
Begin
  If (Index<0) Or (Index>=_LightCount) Then
    Result := Nil
  Else
    Result := (_Lights[Index]);
End;

Function Mesh.AddLight(Name:AnsiString; Position:Vector3D; LightType:Integer; LightColor:Color; Param1, Param2, Param3:Vector3D; ParentBone:AnsiString):MeshLight;
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

Function Mesh.AddLight(OtherLight:MeshLight):MeshLight;
Begin
  Result := Self.AddLight(OtherLight.Name, OtherLight.Position, OtherLight.LightType, OtherLight.LightColor, OtherLight.Param1, OtherLight.Param2, OtherLight.Param3, OtherLight.ParentBone);
End;

Procedure Mesh.AddMetadata(Name:AnsiString; Position: Vector3D; Content:AnsiString);
Begin
  Inc(_MetaDataCount);
  SetLength(_Metadata, _MetaDataCount);
  _Metadata[Pred(_MetaDataCount)] := MeshMetadata.Create();
  _Metadata[Pred(_MetaDataCount)].Name := Name;
  _Metadata[Pred(_MetaDataCount)].Position := Position;
  _Metadata[Pred(_MetaDataCount)].Content := Content;
End;

Function Mesh.GetMetadata(Name:AnsiString): MeshMetadata;
Var
  I:Integer;
Begin
  Name := UpStr(Name);
  For I:=0 To Pred(_MetadataCount) Do
  If (UpStr(_Metadata[I].Name) = Name) Then
  Begin
    Result := _Metadata[I];
    Exit;
  End;

  Result := Nil;
End;

Function Mesh.GetMetadata(Index: Integer): MeshMetadata;
Begin
  If (Index<0) Or (Index>=_MetadataCount) Then
    Result := Nil
  Else
    Result := _Metadata[Index];
End;

Procedure MeshGroup.OnContextLost;
Begin
  If Assigned(_Buffer) Then
  Begin
    _Buffer.Destroy;
    _Buffer := Nil;
  End;
End;

Procedure Mesh.Optimize(VertexCacheSize:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
  If (_Groups[I]._TriangleCount>0) Then
    _Groups[I].Optimize(VertexCacheSize);
End;

Function Mesh.GetGroupCount: Integer;
Begin
  If (Self._GroupCount<=0) Then
    Self.Prefetch();

  Result := Self._GroupCount;
End;

Function Mesh.Clone: Mesh;
Var
  Merger:MeshMerger;
Begin
  Result := Mesh.Create('@'+Self.Name);
  Merger := MeshMerger.Create;
  Merger.Merge(Self, Result, True);
  Merger.Destroy;
End;

Procedure Mesh.ResolveLinks;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
    _Groups[I].ResolveLinks();
End;

Function Mesh.AddBoneMorph(MorphID: Integer):Integer;
Begin
  Result := Self._BoneMorphCount;
  Inc(_BoneMorphCount);
  SetLength(_BoneMorphs, _BoneMorphCount);
  _BoneMorphs[Result].MorphID := MorphID;
  _BoneMorphs[Result].MorphType := 0;
  SetLength(_BoneMorphs[Result].Values, Self.Skeleton.BoneCount);
End;

Procedure Mesh.SetBoneMorph(MorphID, BoneID:Integer; Const Value:Vector3D);
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

Function Mesh.GetBoneMorph(MorphID, BoneID: Integer): Vector3D;
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

Function Mesh.HasBoneMorph(MorphID: Integer): Boolean;
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

Procedure Mesh.CullTriangles(Box: BoundingBox; Transform:Matrix4x4);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
  If (_Groups[I]._TriangleCount>0) Then
    _Groups[I].CullTriangles(Box, Transform);
End;

Procedure Mesh.UncullTriangles;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_GroupCount) Do
    _Groups[I].UncullTriangles();
End;

Procedure Mesh.RegisterInstance(Instance: MeshInstance);
Begin
  Inc(_InstanceCount);
  SetLength(_Instances, _InstanceCount);
  _Instances[Pred(_InstanceCount)] := Instance;
End;

Procedure Mesh.UnregisterInstance(Instance: MeshInstance);
Var
  I:Integer;
Begin
  I:=0;
  While I<_InstanceCount Do
  If (_Instances[I] = Instance) Then
  Begin
    _Instances[I] := _Instances[Pred(_InstanceCount)];
    Dec(_InstanceCount);
    Exit;
  End Else
    Inc(I);
End;

Destructor Mesh.Destroy;
Var
  I:Integer;
Begin
  I:=0;
  For I:=0 To Pred(_InstanceCount) Do
    _Instances[I]._Mesh := Nil;
    
  Inherited;
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

Function CustomMeshFilter.GetAnimationName(AnimationID: Integer):AnsiString;
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

Function CustomMeshFilter.GetBoneName(BoneID: Integer):AnsiString;
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

Function CustomMeshFilter.GetDiffuseMapName(GroupID: Integer):AnsiString;
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

Function CustomMeshFilter.GetGroupName(GroupID: Integer):AnsiString;
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
Begin
  Result := Trunc(_Mesh._Groups[GroupID].GetVertex(Index).BoneIndex);
End;

Function CustomMeshFilter.GetVertexColor(GroupID, Index: Integer): Color;
Begin
  Result := _Mesh._Groups[GroupID].GetVertex(Index).Color;
End;

Function CustomMeshFilter.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _Mesh._Groups[GroupID]._VertexCount;
End;

Function CustomMeshFilter.GetVertexFormat(GroupID: Integer): Cardinal;
Begin
  Result := meshFormatNormal Or meshFormatUV1 or meshFormatBone;
End;

Function CustomMeshFilter.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Mesh._Groups[GroupID].GetVertex(Index).Normal;
End;

Function CustomMeshFilter.GetVertexPosition(GroupID,Index: Integer): Vector3D;
Begin
  Result := _Mesh._Groups[GroupID].GetVertex(Index).Position;
End;

Function CustomMeshFilter.GetVertexTangent(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Mesh._Groups[GroupID].GetVertex(Index).Tangent;
End;

Function CustomMeshFilter.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result := _Mesh._Groups[GroupID].GetVertex(Index).TextureCoords;
  Result.Y := 1 - Result.Y;
End;

Function CustomMeshFilter.GetVertexUV2(GroupID, Index: Integer): Vector2D;
Begin
  Result := _Mesh._Groups[GroupID].GetVertex(Index).TextureCoords2;
End;

Function CreateFilterFromMesh(MyMesh:Mesh):MeshFilter;
Begin
  Result := CustomMeshFilter.Create;
  CustomMeshFilter(Result)._Mesh := MyMesh;
End;

{ MeshMerger }
Destructor MeshMerger.Destroy;
Begin
  // do nothing
End;

Function MeshMerger.Merge(Source, Dest:Mesh; IndividualGroup: Boolean; MaxVertsPerGroup: Integer; UpdateBox:Boolean): IntegerArray;
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

  Source.Prefetch();
  Dest.Prefetch();

  Log(logDebug, 'Mesh', 'Beginning merging '+Source.Name+' into '+Dest.Name);

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
    And (Dest._Groups[I]._VertexCount + SourceGroup._VertexCount<MaxVertsPerGroup) Then
    Begin
      Target := Dest._Groups[I];
      Break;
    End;

    If Not Assigned(Target) Then
    Begin
      Target := Dest.AddGroup();
      Target._Name := SourceGroup._Name;
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
  SourceVertex, DestVertex:PMeshVertex;
  SourceTriangle, DestTriangle:PTriangle;
  I, N:Integer;
Begin
  If (Source = Nil) Or (Dest =  Nil) Then
  Begin
    IntToString(2);
    Exit;
  End;

  Log(logDebug, 'Mesh', 'Beginning group '+Source._Owner.Name+'.'+ Source.Name+' into '+Dest._Owner.Name+'.'+ Dest.Name);

  VOfs := Dest._VertexCount;
  TOfs := Dest._TriangleCount;
  Inc(Dest._VertexCount, Source._VertexCount);
  Inc(Dest._TriangleCount, Source._TriangleCount);  
  Dest._VisibleTriangleCount := Dest._TriangleCount;

  If (Length(Dest._Vertices) < Dest._VertexCount) Then
  Begin
    If (Length(Dest._Vertices)<=0) Then
    Begin
      If (Dest._VertexCount>=256) Then
        N := Dest._VertexCount
      Else
        N := 256;
    End Else
      N := Length(Dest._Vertices) * 2;

    SetLength(Dest._Vertices, N);
  End;

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

  For I:=0 To Pred(Source._VertexCount) Do
  Begin
    SourceVertex := Source.GetVertexPointer(I);
    DestVertex := Dest.GetVertexPointer(VOfs + I);
    If (Assigned(SourceVertex)) And (Assigned(DestVertex)) Then
    Begin
      DestVertex^ := SourceVertex^;
      Self.ProcessVertex(DestVertex, Source, Dest);
    End;
  End;

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
Procedure MeshMerger.ProcessVertex(Vertex: PMeshVertex; Source, Dest:MeshGroup); Begin End;

{ MeshEmitter }
Constructor MeshEmitter.Create(Owner: Mesh);
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
Constructor MeshLight.Create(Owner: Mesh);
Begin
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
Function MeshMaterial.Equals(const Other: MeshMaterial): Boolean;
Begin
  Result :=
       (Self.DiffuseMap = Other.DiffuseMap)
    And (Self.TriplanarMap = Other.TriplanarMap)
    And (Self.DecalMap = Other.DecalMap)
    And (Self.NormalMap = Other.NormalMap)
    And (Self.SpecularMap = Other.SpecularMap)
    And (Self.GlowMap = Other.GlowMap)
    And (Self.RefractionMap = Other.RefractionMap)
    And (Self.AlphaMap = Other.AlphaMap)
    And (Self.LightMap = Other.LightMap)
    And (Self.ColorRamp = Other.ColorRamp)
    And (Self.ColorTable = Other.ColorTable)
    And (Self.BlendMode = Other.BlendMode)
    And (Cardinal(Self.DiffuseColor) = Cardinal(Other.DiffuseColor))
    And (Cardinal(Self.AmbientColor) = Cardinal(Other.AmbientColor))
    And (Cardinal(Self.OutlineColor) = Cardinal(Other.OutlineColor))
    ;
End;

Procedure MeshMaterial.Reset;
Begin
  BlendMode := -1;

  AmbientColor := ColorWhite;
  DiffuseColor := ColorWhite;
  OutlineColor := ColorNull;

  DiffuseMap := Nil;
  TriplanarMap := Nil;
  DecalMap := Nil;
  NormalMap := Nil;
  SpecularMap := Nil;
  GlowMap := Nil;
  RefractionMap := Nil;
  AlphaMap := Nil;
  LightMap := Nil;

  ColorRamp := Nil;
  ColorTable := Nil;

  VegetationBend := 0.5;
End;

{ MeshParticleEmitter }
Constructor MeshParticleEmitter.Create(Const FXName:AnsiString; Target: MeshGroup);
Begin
  Self._TargetGroup := Target;
  Inherited Create(FXName, VectorZero);
End;

Procedure MeshParticleEmitter.Emit(Target: PParticle);
Var
  N:Integer;
Begin
  N := Random(_TargetGroup._VertexCount);
  //N := GetTime Mod Group._VertexCount;
  Self.Position := _TargetGroup._Vertices[N].Position;
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
Function SelectMeshShader(Group:MeshGroup; Position:Vector3D; Outline:Boolean; Const DestMaterial:MeshMaterial):Shader;
Var
  DisableLights:Boolean;
  LightPivot:Vector3D;
  RenderStage:Integer;
  FxFlags, OutFlags:Cardinal;
Begin
  FxFlags := 0;
  OutFlags := 0;
  Group._LightBatch.DirectionalLightCount := 0;
  Group._LightBatch.PointLightCount := 0;
  Group._LightBatch.SpotLightCount := 0;
  DisableLights := False;

  If (Group.Flags And meshGroupVegetation<>0) And (DestMaterial.VegetationBend>0) Then
    FxFlags := FxFlags Or shaderVegetation;

  Group._Owner._Skinning := (Assigned(Group._Owner.Skeleton)) And (Group._Owner.Skeleton.BoneCount>0);
  Group._Owner._NormalMapping := (Assigned(DestMaterial.NormalMap)) And (GraphicsManager.Instance.Settings.NormalMapping.Enabled);

  If (Group.AmbientColor.R = 0) And (Group.AmbientColor.G = 0) And (Group.AmbientColor.B=0) Then
    FxFlags := FxFlags Or shaderSkipAmbient;

  RenderStage := GraphicsManager.Instance.RenderStage;

  If (GraphicsManager.Instance.ActiveViewport.Camera.UseClipPlane) Then
    FxFlags := FxFlags Or shaderClipPlane;

  If (Group.Flags And meshGroupStencilMask<>0) {$IFDEF REFLECTIONS_WITH_STENCIL} Or (GraphicsManager.Instance.ReflectionStencil) {$ENDIF}  Then
  Begin
    DisableLights := True;
    OutFlags := shader_OutputColor;
  End Else
  If RenderStage = renderStageOutline Then
  Begin
    DisableLights := True;
    OutFlags := OutFlags Or shader_OutputColor;
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
        FxFlags := FxFlags Or shaderNormalMap;
    End;
  End Else
  If RenderStage = renderStageDiffuse Then
  Begin
    If (GraphicsManager.Instance.Settings.Specular.Enabled) Then
      FxFlags := FxFlags Or shaderSpecular;

    If (Assigned(DestMaterial.ColorRamp)) Then
      FxFlags := FxFlags Or shaderColorRamp;

    If Assigned(DestMaterial.DecalMap) Then
      FxFlags := FxFlags Or shaderDecalMap;

    If (Group.Flags And meshGroupLightmap<>0) Then
    Begin
      FxFlags := FxFlags Or shaderLightmap Or shaderAddSigned;
    End Else
    Begin
      If (Group.Flags And meshGroupAlphaMap<>0) Then
      Begin
        FxFlags := FxFlags Or shaderAlphamap;
      End;

      If (Not DisableLights) And (Group.Flags And meshGroupLightOff=0) Then
      Begin
        If (Group._BoundingBox.Radius>50) Then
          LightPivot := GraphicsManager.Instance.ActiveViewport.Camera.Position
        Else
          LightPivot := Position;

        Group._LightBatch := LightManager.Instance.SortLights(LightPivot, Group._BoundingBox);
      End;
    End;

    {$IFNDEF DISABLECOLORGRADING}
    If Assigned(DestMaterial.ColorTable) Then
      FxFlags := FxFlags Or shaderColorTable;
    {$ENDIF}

    If (Group.Flags And meshGroupSphereMap<>0) Then
      FxFlags := FxFlags Or shaderSphereMap;

    If (Group.Flags And meshGroupTriplanar<>0) Then
      FxFlags := FxFlags Or shaderTriplanar;

    If (Group.Flags And meshGroupWireframe<>0) Then
      FxFlags := FxFlags Or shaderWireframe;

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

    If (DestMaterial.Ghost) Then
      FxFlags := FxFlags Or shaderGhost;
  End;

  If (Group._Owner._NormalMapping) And ((RenderStage = renderStageNormal) Or (RenderStage = renderStageDiffuse)) Then
  Begin
    FxFlags := FxFlags Or shaderNormalMap;
  End;

  If (Group._Owner._Skinning) And (GraphicsManager.Instance.Settings.VertexBufferObject.Enabled) Then
    FxFlags := FxFlags Or shaderSkinning;

  If (Group.Flags And meshGroupColorOff<>0) Then
  Begin
    FxFlags := shaderColorOff;
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

  If (Group.Flags And meshGroupAlphaTest<>0) And (GraphicsManager.Instance.Settings.AlphaTesting.Enabled) Then
    FxFlags := FxFlags Or shaderAlphaTest;

  If (GraphicsManager.Instance.ReflectionActive) Then
    FxFlags := FxFlags Or shaderScreenMask;

  If (Group.Flags And meshGroupTextureMatrix<>0) And (FxFlags And shaderFlowMap = 0) Then
    FxFlags := FxFlags Or shaderTextureMatrix;

//  Flags := shaderOutputNormal;

{$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'MeshGroup', 'Getting shader with flags '+CardinalToString(Flags));{$ENDIF}

  Result := ShaderFactory.Instance.GetShader(FxFlags, OutFlags, GraphicsManager.Instance.Settings.FogMode, GraphicsManager.Instance.LightModel, Group._LightBatch);
End;

Initialization
  Log(logDebug, 'Mesh', 'Initializing');
  RegisterResourceClass(Mesh);

  RegisterMeshDataHandler(tagMeshGroup, MeshReadGroup);
  RegisterMeshDataHandler(tagMeshSkeleton, MeshReadSkeleton);
  RegisterMeshDataHandler(tagMeshEmitter, MeshReadEmitter);
  RegisterMeshDataHandler(tagMeshLight, MeshReadLights);
  RegisterMeshDataHandler(tagMeshMetadata, MeshReadMeta);
  RegisterMeshDataHandler(tagMeshBoneMorph, MeshReadBoneMorph);

  RegisterMeshGroupHandler(tagVertexPositions, GroupReadVertexPositions);
  RegisterMeshGroupHandler(tagVertexNormals, GroupReadVertexNormals);
  RegisterMeshGroupHandler(tagVertexUVs0, GroupReadVertexUV0);
  RegisterMeshGroupHandler(tagVertexUVs1, GroupReadVertexUV1);
  RegisterMeshGroupHandler(tagVertexTangents, GroupReadVertexTangents);
  RegisterMeshGroupHandler(tagVertexColors, GroupReadVertexColors);
  RegisterMeshGroupHandler(tagVertexBoneIndices, GroupReadVertexBoneIndices);
  RegisterMeshGroupHandler(tagVertexMorph, GroupReadVertexMorphs);
  RegisterMeshGroupHandler(tagVertexLinks, GroupReadVertexLinks);
//  RegisterMeshGroupHandler(tagVertexBoneWeights, GroupReadVertexBoneWeights);
  RegisterMeshGroupHandler(tagTriangleIndices, GroupReadTriangleIndices);
  RegisterMeshGroupHandler(tagTriangleNormals, GroupReadTriangleNormals);
  RegisterMeshGroupHandler(tagTriangleEdges, GroupReadTriangleEdges);
  RegisterMeshGroupHandler(tagMaterialDiffuse, GroupReadMaterialDiffuse);
  RegisterMeshGroupHandler(tagMaterialTriplanar, GroupReadMaterialTriplanar);
  RegisterMeshGroupHandler(tagMaterialSpecular, GroupReadMaterialSpecular);
  RegisterMeshGroupHandler(tagMaterialBump, GroupReadMaterialBump);
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
  If (Assigned(_MeshManager)) Then
    _MeshManager.Destroy;
End.

