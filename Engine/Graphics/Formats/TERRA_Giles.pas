{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_Giles
 * Implements [g]iles lightmapper loader/writer/mesh filter
 ***********************************************************************************************************************
}

Unit TERRA_Giles;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Stream, TERRA_INI, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Color, TERRA_FileStream, TERRA_FileUtils, TERRA_Vector4D, TERRA_MeshFilter;

Const
  gilesLight_Directional = 1;
  gilesLight_Omni = 2;
  gilesLight_Spot = 3;
  gilesLight_Ambient = 4;

  gilesMatFX_FullBright = 1;
  gilesMatFX_VertexColor = 2;
  gilesMatFX_FlatShade = 4;
  gilesMatFX_NoFog = 8;
  gilesMatFX_TwoSided = 16;
  gilesMatFX_VertexAlpha = 32;

  gilesMatBlendAlpha = 1;
  gilesMatBlendMultiply = 2;
  gilesMatBlendAdditive = 3;

  gilesLight_Unlit = 0;
  gilesLight_Dynamic = 1;
  gilesLight_Lightmap = 2;

  gilesTexture_Color = 1;
  gilesTexture_Alpha = 2;
  gilesTexture_Masked = 4;
  gilesTexture_MipMap = 8;
  gilesTexture_ClampU = 16;
  gilesTexture_ClampV = 32;
  gilesTexture_SphereMap = 64;
  gilesTexture_CubeMap = 128;
  gilesTexture_VRam = 256;
  gilesTexture_HiColor = 512;

  gilesTexBlendAlpha = 1;
  gilesTexBlendMultiply = 2;
  gilesTexBlendAdditive = 3;
  gilesTexBlendDot3 = 4;

Type
  GilesObject = Class(TERRAObject)
    Name:TERRAString;
    Position:Vector3D;
    Rotation:Vector3D;
    Scale:Vector3D;
    Props:TERRAString;
    FileName:TERRAString;
    Hidden:Byte;

    Parent:GilesObject;
    Children:Array Of GilesObject;
    ChildrenCount:Integer;
  End;

  GilesObjectClass = Class Of GilesObject;

  GilesMesh = Class;

  GilesSurface = Class(TERRAObject)
    Owner:GilesMesh;
    VertexCount:Word;
    TriangleCount:Word;
    MaterialIndex:Integer;
    VertexFormat:Cardinal;
    Vertices:Array Of MeshVertex;
    Triangles:Array Of Triangle;
  End;

  GilesMesh = Class(GilesObject)
    OverrideLighting:Byte;
    BackLight:Byte;
    ReceiveShadow:Byte;
    CastShadow:Byte;
    ReceiveGI:Byte;
    AffectGI:Byte;

    Surfaces:Array Of GilesSurface;
    SurfaceCount:Integer;
  End;

  GilesPivot = Class(GilesObject)
  End;

  GilesLight = Class(GilesObject)
    LightType:Byte;
    LightActive:Boolean;
    CastShadows:Boolean;
    Infinite:Boolean;
    Overshoot:Boolean;
    Radius:Single;
    ColorRed:Single;
    ColorGreen:Single;
    ColorBlue:Single;
    Intensity:Single;
    NearValue:Single;
    FarValue:Single;
    InnerCone:Single;
    OuterCone:Single;
    ToonShading:Boolean;
    ToonLevels:Byte; //???
  End;

  GilesMaterial = Class(TERRAObject)
    Name:TERRAString;
    ColorRed:Single;
    ColorGreen:Single;
    ColorBlue:Single;
    ColorAlpha:Single;
    SelfIllum:Single;
    Shininess:Single;
    Effects:Cardinal;
    BlendMode:Cardinal;
    LighMode:Byte;
    LightMapIndex:SmallInt;
    ReceiveBackLight:Boolean;
    ReceiveShadow:Boolean;
    CastShadow:Boolean;
    ReceiveGI:Boolean;
    AffectGI:Boolean;
    TextureLayer:Byte;
    TextureIndex:SmallInt;

    DiffuseColor:Color;
  End;

  GilesTexture = Class(TERRAObject)
    FileName:TERRAString;
    ScaleU:Single;
    ScaleV:Single;
    OfsU:Single;
    OfsV:Single;
    Angle:Single;
    Flags:Cardinal;
    BlendMode:Cardinal;
    CoordSet:Byte;
  End;

  GilesLightmap = Class(TERRAObject)
    Name:TERRAString;
    FileName:TERRAString; // to export
    Width:Word;
    Height:Word;
    NoUniform:Boolean;
    UseCustomTexel:Boolean;
    CustomTexel:Single;
    Repack:Boolean;
    Data:Array Of Byte;
  End;

  GilesSettings = Record
    ClearBeforeRender:Boolean;
    DirectIllum:Boolean;
    GlobalIllum:Boolean;
    RayBias:Single;
    DirectMultiply:Single;
    CastShadowBackFace:Boolean;
    DirectShadows:Boolean;
    SoftShadows:Boolean;
    SoftShadowSamples:Byte;
    GI_IgnoreTextures:Boolean;
    GI_Iterations:Byte;
    GI_Density:Single; // 1.0 to 100.0
    GI_Samples:Integer;
    GI_Multiply:Single;
    GI_SkyEnable:Boolean;
    GI_SkyRed:Single;
    GI_SkyGreen:Single;
    GI_SkyBlue:Single;
    GI_SkyMultiply:Single; // not used
    AutoBlur:Boolean;
    AutoExpand:Boolean;
    AutoBlurRadius:Byte;
  End;

  GilesModel = Class(MeshFilter)
    Protected
      _Objects:Array Of GilesObject;
      _ObjectCount:Integer;

      _Groups:Array Of GilesSurface;
      _GroupCount:Integer;

      _Materials:Array Of GilesMaterial;
      _MaterialCount:Integer;

      _Textures:Array Of GilesTexture;
      _TextureCount:Integer;

      _Lightmaps:Array Of GilesLightmap;
      _LightmapCount:Integer;

      _Settings:GilesSettings;

      Function ReadObject(Source:Stream; ChunkEnd:Integer; Parent:GilesObject):Boolean;
      Function ReadMesh(Source:Stream; ChunkEnd:Integer; Obj:GilesMesh):Boolean;
      Function ReadPivot(Source:Stream; ChunkEnd:Integer; Obj:GilesPivot):Boolean;
      Function ReadLight(Source:Stream; ChunkEnd:Integer; Obj:GilesLight):Boolean;

      Function ReadSurface(Source:Stream; ChunkEnd:Integer; Surface:GilesSurface):Boolean;

      Function ReadMaterial(Source:Stream; ChunkEnd:Integer):Boolean;
      Function ReadTexture(Source:Stream; ChunkEnd:Integer):Boolean;
      Function ReadLightmap(Source:Stream; ChunkEnd:Integer):Boolean;

      Function ReadRenderSettings(Source:Stream; ChunkEnd:Integer):Boolean;

      Function FindChunk(Source:Stream; ID:Cardinal; Var ChunkSize:Cardinal):Boolean;

    Public
      Function Load(Source:Stream):Boolean; Override;
      Class Function Save(Dest:Stream; MyMesh:MeshFilter):Boolean; Override;

      Function GetGroupCount:Integer; Override;
      Function GetGroupFlags(GroupID:Integer):Cardinal; Override;
      Function GetGroupBlendMode(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):Cardinal; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;

      Function GetDiffuseMapName(GroupID:Integer):TERRAString; Override;
  End;


Implementation
Uses TERRA_Log, TERRA_Error, TERRA_TextureAtlas, TERRA_Image, TERRA_Application,
  TERRA_GraphicsManager, TERRA_ResourceManager;

Const
  GLS_HEADER = $FFFF;
  GLS_AUTHOR = $F000;
  GLS_MODELS = $1000;

  GLS_MODEL  = $1001;
  GLS_MODEL_NAME      = $1002;
  GLS_MODEL_POSITION  = $1003;
  GLS_MODEL_ROTATION  = $1004;
  GLS_MODEL_SCALE     = $1005;
  GLS_MODEL_CUSTOMPROPS = $1006;
  GLS_MODEL_FILE  = $1007;
  GLS_MODEL_HIDDEN = $1008;

  GLS_MESH = $2000;
  GLS_MESH_OVERRIDE = $2001;
  GLS_MESH_BACKLIGHT = $2002;
  GLS_MESH_RECEIVESHADOW = $2003;
  GLS_MESH_CASTSHADOW = $2004;
  GLS_MESH_RECEIVEGI = $2005;
  GLS_MESH_AFFECTGI = $2006;

  GLS_MESH_SURFACES = $2100;
  GLS_MESH_SURF = $2101;
  GLS_MESH_SURFVERTS = $2102;
  GLS_MESH_SURFPOLYS = $2103;
  GLS_MESH_SURFMATERIAL = $2104;
  GLS_MESH_SURFVERTFORMAT = $2105;
  GLS_MESH_SURFVERTDATA = $2106;
  GLS_MESH_SURFPOLYDATA = $2107;

  GLS_PIVOT = $3000;

  GLS_LIGHT = $4000;
  GLS_LIGHT_TYPE = $4001;
  GLS_LIGHT_ACTIVE = $4002;
  GLS_LIGHT_CASTSHADOWS = $4003;
  GLS_LIGHT_INFINITE = $4004;
  GLS_LIGHT_OVERSHOOT = $4005;
  GLS_LIGHT_RADIUS = $4006;
  GLS_LIGHT_RED = $4007;
  GLS_LIGHT_GREEN = $4008;
  GLS_LIGHT_BLUE = $4009;
  GLS_LIGHT_INTENSITY = $400A;
  GLS_LIGHT_NEAR = $400B;
  GLS_LIGHT_FAR = $400C;
  GLS_LIGHT_INNER = $400D;
  GLS_LIGHT_OUTER = $400E;
  GLS_LIGHT_TOON =	$400F;
  GLS_LIGHT_TOONLEVELS = $4010;

  GLS_MATERIALS = $5000;
  GLS_MAT = $5001;
  GLS_MAT_NAME = $5002;
  GLS_MAT_RED = $5003;
  GLS_MAT_GREEN = $5004;
  GLS_MAT_BLUE = $5005;
  GLS_MAT_ALPHA = $5006;
  GLS_MAT_SELFILLUMINATION = $5007;
  GLS_MAT_SHININESS = $5008;
  GLS_MAT_FX = $5009;
  GLS_MAT_BLEND = $500A;
  GLS_MAT_LIGHTMETHOD = $500B;
  GLS_MAT_LIGHTMAP = $500C;
  GLS_MAT_RECEIVEBACK = $500D;
  GLS_MAT_RECEIVESHADOW = $500E;
  GLS_MAT_CASTSHADOW = $500F;
  GLS_MAT_RECEIVEGI = $5010;
  GLS_MAT_AFFECTGI = $5011;
  GLS_MAT_TEXLAYER = $5012;

  GLS_TEXTURES = $6000;
  GLS_TEX = $6001;
  GLS_TEX_FILE = $6002;
  GLS_TEX_SCALEU = $6003;
  GLS_TEX_SCALEV = $6004;
  GLS_TEX_OFFSETU = $6005;
  GLS_TEX_OFFSETV = $6006;
  GLS_TEX_ANGLE = $6007;
  GLS_TEX_FLAGS = $6008;
  GLS_TEX_BLEND = $6009;
  GLS_TEX_COORDSET = $600A;

  GLS_LIGHTMAPS = $7000;
  GLS_LMAP = $7001;
  GLS_LMAP_NAME = $7002;
  GLS_LMAP_FILE = $7003;
  GLS_LMAP_WIDTH = $7004;
  GLS_LMAP_HEIGHT = $7005;
  GLS_LMAP_NONUNIFORM = $7006;
  GLS_LMAP_USECUSTOMTEXEL = $7007;
  GLS_LMAP_CUSTOMTEXEL = $7008;
  GLS_LMAP_REPACK = $7009;
  GLS_LMAP_DATA = $700A;

  GLS_RENDER = $8000;
  GLS_RENDER_CLEARBEFORERENDER = $8001;
  GLS_RENDER_DIRENABLE = $8002;
  GLS_RENDER_GIENABLE = $8003;
  GLS_RENDER_RAYBIAS = $8004;
  GLS_RENDER_DIRMULTIPLY = $8005;
  GLS_RENDER_DIRBACKSHAD = $8006;
  GLS_RENDER_DIRSHADOWS = $8007;
  GLS_RENDER_DIRSOFT = $8008;
  GLS_RENDER_DIRSOFTSAMPLES = $8009;
  GLS_RENDER_GIIGNORETEX = $800A;
  GLS_RENDER_GIITERATIONS = $800B;
  GLS_RENDER_GIDENSITY = $800C;
  GLS_RENDER_GISAMPLES = $800D;
  GLS_RENDER_GIMULTIPLY = $800E;
  GLS_RENDER_SKYENABLE = $800F;
  GLS_RENDER_SKYRED = $8010;
  GLS_RENDER_SKYGREEN = $8011;
  GLS_RENDER_SKYBLUE = $8012;
  GLS_RENDER_SKYMULTIPLY = $8013;
  GLS_RENDER_AUTOBLUR = $8014;
  GLS_RENDER_AUTOEXPAND = $8015;
  GLS_RENDER_AUTOBLURRADIUS = $8016;

  vfPosition    = 0;
  vfNormals     = 1;
  vfColors      = 2;
  vfLight       = 4;
  vfUV1         = 8;
  vfUV0         = 16;

Function GilesModel.ReadSurface(Source:Stream; ChunkEnd:Integer; Surface:GilesSurface):Boolean;
Var
  I:Integer;
  ChunkID:Cardinal;
  ChunkSize:Cardinal;
Begin
  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_MESH_SURFVERTS:
      Begin
        Source.Read(@Surface.VertexCount, 4);
        SetLength(Surface.Vertices, Surface.VertexCount);
      End;

    GLS_MESH_SURFPOLYS:
      Begin
        Source.Read(@Surface.TriangleCount, 4);
        SetLength(Surface.Triangles, Surface.TriangleCount);
      End;

    GLS_MESH_SURFMATERIAL:
      Begin
        Source.Read(@Surface.MaterialIndex, 4);
      End;

    GLS_MESH_SURFVERTFORMAT:
      Begin
        Source.Read(@Surface.VertexFormat, 4);
      End;

    GLS_MESH_SURFVERTDATA:
      For I:=0 To Pred(Surface.VertexCount) Do
      Begin
        Source.Read(@Surface.Vertices[I].Position, SizeOf(Vector3D));

        If (Surface.VertexFormat And vfNormals<>0) Then
          Source.Read(@Surface.Vertices[I].Normal, SizeOf(Vector3D));

        If (Surface.VertexFormat And vfColors<>0) Then
          Source.Read(@Surface.Vertices[I].Color, 4);

        If (Surface.VertexFormat And vfLight<>0) Then
          Source.Skip(3);

        If (Surface.VertexFormat And vfUV1<>0) Then
          Source.Read(@Surface.Vertices[I].TextureCoords2, SizeOf(Vector2D));

        If (Surface.VertexFormat And vfUV0<>0) Then
          Source.Read(@Surface.Vertices[I].TextureCoords, SizeOf(Vector2D));
        End;

    GLS_MESH_SURFPOLYDATA:
      For I:=0 To Pred(Surface.TriangleCount) Do
      Begin
        Source.Read(@Surface.Triangles[I], SizeOf(Triangle));
      End;


    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Result := True;

End;

Function GilesModel.ReadMesh(Source:Stream; ChunkEnd:Integer; Obj:GilesMesh):Boolean;
Var
  SurfaceEnd:Integer;
  Surface:GilesSurface;
  ChunkID:Cardinal;
  ChunkSize:Cardinal;
Begin
  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_MESH_SURFACES:
      Begin
        SurfaceEnd := Source.Position + ChunkSize;
        Repeat
          If Not FindChunk(Source, GLS_MESH_SURF, ChunkSize) Then
            Exit;

          Surface := GilesSurface.Create();
          Surface.Owner := Obj;
          Inc(Obj.SurfaceCount);
          SetLength(Obj.Surfaces, Obj.SurfaceCount);
          Obj.Surfaces[Pred(Obj.SurfaceCount)] := Surface;

          ReadSurface(Source, Source.Position + ChunkSize, Surface);

          Inc(_GroupCount);
          SetLength(_Groups, _GroupCount);
          _Groups[Pred(_GroupCount)] := Surface;

        Until (Source.Position>=SurfaceEnd);
      End;

    GLS_MESH_OVERRIDE:
      Begin
        Source.Read(@Obj.OverrideLighting, 1);
      End;

    GLS_MESH_BACKLIGHT:
      Begin
        Source.Read(@Obj.BackLight, 1);
      End;

    GLS_MESH_RECEIVESHADOW:
      Begin
        Source.Read(@Obj.ReceiveShadow, 1);
      End;

    GLS_MESH_CASTSHADOW:
      Begin
        Source.Read(@Obj.CastShadow, 1);
      End;

    GLS_MESH_RECEIVEGI:
      Begin
        Source.Read(@Obj.ReceiveGI, 1);
      End;

    GLS_MESH_AFFECTGI:
      Begin
        Source.Read(@Obj.AffectGI, 1);
      End;

    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Result := True;
End;

Function GilesModel.ReadPivot(Source:Stream; ChunkEnd:Integer; Obj:GilesPivot):Boolean;
Begin
  Source.Seek(ChunkEnd);
  Result := True;
End;

Function GilesModel.ReadLight(Source:Stream; ChunkEnd:Integer; Obj:GilesLight):Boolean;
Var
  ChunkID:Cardinal;
  ChunkSize:Cardinal;
Begin
  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
      GLS_LIGHT_TYPE:
      Begin
        Source.Read(@Obj.LightType, 1);
      End;

      GLS_LIGHT_ACTIVE:
      Begin
        Source.Read(@Obj.LightActive, 1);
      End;

      GLS_LIGHT_CASTSHADOWS:
      Begin
        Source.Read(@Obj.CastShadows, 1);
      End;

      GLS_LIGHT_INFINITE:
      Begin
        Source.Read(@Obj.Infinite, 1);
      End;

      GLS_LIGHT_OVERSHOOT:
      Begin
        Source.Read(@Obj.Overshoot, 1);
      End;

      GLS_LIGHT_RADIUS:
      Begin
        Source.Read(@Obj.Radius, 4);
      End;

      GLS_LIGHT_RED:
      Begin
        Source.Read(@Obj.ColorRed, 4);
      End;

      GLS_LIGHT_GREEN:
      Begin
        Source.Read(@Obj.ColorGreen, 4);
      End;

      GLS_LIGHT_BLUE:
      Begin
        Source.Read(@Obj.ColorBlue, 4);
      End;

      GLS_LIGHT_INTENSITY:
      Begin
        Source.Read(@Obj.Intensity, 4);
      End;

      GLS_LIGHT_NEAR:
      Begin
        Source.Read(@Obj.NearValue, 4);
      End;

      GLS_LIGHT_FAR:
      Begin
        Source.Read(@Obj.FarValue, 4);
      End;

      GLS_LIGHT_INNER:
      Begin
        Source.Read(@Obj.InnerCone, 4);
      End;

      GLS_LIGHT_OUTER:
      Begin
        Source.Read(@Obj.OuterCone, 4);
      End;

      GLS_LIGHT_TOON:
      Begin
        Source.Read(@Obj.ToonShading, 1);
      End;

      GLS_LIGHT_TOONLEVELS:
      Begin
        Source.Read(@Obj.ToonLevels, 4);
      End;

    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Result := True;
End;

Function GilesModel.ReadObject(Source:Stream; ChunkEnd:Integer; Parent:GilesObject):Boolean;
Var
  ChunkID:Cardinal;
  ChunkSize:Cardinal;
  Obj:GilesObject;

  Procedure CreateNewObject(ClassType:GilesObjectClass; Parent:GilesObject);
  Begin
    Obj := GilesObject.Create();

    Inc(_ObjectCount);
    SetLength(_Objects, _ObjectCount);
    _Objects[Pred(_ObjectCount)] := Obj;

    If Assigned(Parent) Then
    Begin
      Inc(Parent.ChildrenCount);
      SetLength(Parent.Children, Parent.ChildrenCount);
      Parent.Children[Pred(Parent.ChildrenCount)] := Obj;
      Obj.Parent := Parent;
    End Else
      Obj.Parent := Nil;
  End;

Begin
  Obj := Nil;

  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_MESH:
      Begin
        CreateNewObject(GilesMesh, Parent);
        Self.ReadMesh(Source, Source.Position + ChunkSize, GilesMesh(Obj));
      End;

    GLS_PIVOT:
      Begin
        CreateNewObject(GilesPivot, Parent);
        Self.ReadPivot(Source, Source.Position + ChunkSize, GilesPivot(Obj));
      End;

    GLS_LIGHT:
      Begin
        CreateNewObject(GilesLight, Parent);
        Self.ReadLight(Source, Source.Position + ChunkSize, GilesLight(Obj));
      End;

    GLS_MODEL:
      Begin
        Self.ReadObject(Source, Source.Position + ChunkSize, Obj);
      End;

    GLS_MODEL_NAME:
      Begin
        Source.ReadString(Obj.Name, True);
      End;

    GLS_MODEL_POSITION:
      Begin
        Source.Read(@Obj.Position, SizeOf(Vector3D));
      End;

    GLS_MODEL_ROTATION:
      Begin
        Source.Read(@Obj.Rotation, SizeOf(Vector3D));
      End;

    GLS_MODEL_SCALE:
      Begin
        Source.Read(@Obj.Scale, SizeOf(Vector3D));
      End;


    GLS_MODEL_CUSTOMPROPS:
      Begin
        Source.ReadString(Obj.Props, True);
      End;

    GLS_MODEL_FILE:
      Begin
        Source.ReadString(Obj.FileName, True);
      End;

    GLS_MODEL_HIDDEN:
      Begin
        Source.Read(@Obj.Hidden, 1);
      End;

    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Result := True;
End;

Function GilesModel.ReadMaterial(Source:Stream; ChunkEnd:Integer): Boolean;
Var
  ChunkID, ChunkSize:Cardinal;
  Mat:GilesMaterial;
Begin
  Mat := GilesMaterial.Create();
  Inc(_MaterialCount);
  SetLength(_Materials, _MaterialCount);
  _Materials[Pred(_MaterialCount)] := Mat;

  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_MAT_NAME:
      Begin
        Source.ReadString(Mat.Name, True);
      End;

    GLS_MAT_RED:
      Begin
        Source.Read(@Mat.ColorRed, 4);
      End;

    GLS_MAT_GREEN:
      Begin
        Source.Read(@Mat.ColorGreen, 4);
      End;

    GLS_MAT_BLUE:
      Begin
        Source.Read(@Mat.ColorBlue, 4);
      End;

    GLS_MAT_ALPHA:
      Begin
        Source.Read(@Mat.ColorAlpha, 4);
      End;

    GLS_MAT_SELFILLUMINATION:
      Begin
        Source.Read(@Mat.SelfIllum, 4);
      End;

    GLS_MAT_SHININESS:
      Begin
        Source.Read(@Mat.Shininess, 4);
      End;

    GLS_MAT_FX:
      Begin
        Source.Read(@Mat.Effects, 4);
      End;

    GLS_MAT_BLEND:
      Begin
        Source.Read(@Mat.BlendMode, 4); //??
      End;

    GLS_MAT_LIGHTMETHOD:
      Begin
        Source.Read(@Mat.LighMode, 1);
      End;

    GLS_MAT_LIGHTMAP:
      Begin
        Source.Read(@Mat.LightMapIndex, 2);
      End;

    GLS_MAT_RECEIVEBACK:
      Begin
        Source.Read(@Mat.ReceiveBackLight, 1);
      End;

    GLS_MAT_RECEIVESHADOW:
      Begin
        Source.Read(@Mat.ReceiveShadow, 1);
      End;

    GLS_MAT_CASTSHADOW:
      Begin
        Source.Read(@Mat.CastShadow, 1);
      End;

    GLS_MAT_RECEIVEGI:
      Begin
        Source.Read(@Mat.ReceiveGI, 1);
      End;

    GLS_MAT_AFFECTGI:
      Begin
        Source.Read(@Mat.AffectGI, 1);
      End;

    GLS_MAT_TEXLAYER:
      Begin
        Source.Read(@Mat.TextureLayer, 1);
        Source.Read(@Mat.TextureIndex, 2);
      End;

    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Mat.DiffuseColor := ColorCreate(Mat.ColorRed, Mat.ColorGreen, Mat.ColorBlue, Mat.ColorAlpha);

  Result := True;
End;

Function GilesModel.ReadTexture(Source:Stream; ChunkEnd:Integer):Boolean;
Var
  ChunkID, ChunkSize:Cardinal;
  Tex:GilesTexture;
Begin
  Tex := GilesTexture.Create();
  Inc(_TextureCount);
  SetLength(_Textures, _TextureCount);
  _Textures[Pred(_TextureCount)] := Tex;

  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_TEX_FILE:
      Begin
        Source.ReadString(Tex.FileName, True);
      End;

    GLS_TEX_SCALEU:
      Begin
        Source.Read(@Tex.ScaleU, 4);
      End;

    GLS_TEX_SCALEV:
      Begin
        Source.Read(@Tex.ScaleV, 4);
      End;

    GLS_TEX_OFFSETU:
      Begin
        Source.Read(@Tex.OfsU, 4);
      End;

    GLS_TEX_OFFSETV:
      Begin
        Source.Read(@Tex.OfsV, 4);
      End;

    GLS_TEX_ANGLE:
      Begin
        Source.Read(@Tex.Angle, 4);
      End;

    GLS_TEX_FLAGS:
      Begin
        Source.Read(@Tex.Flags, 4);
      End;

    GLS_TEX_BLEND:
      Begin
        Source.Read(@Tex.BlendMode, 4);
      End;

    GLS_TEX_COORDSET:
      Begin
        Source.Read(@Tex.CoordSet, 1);
      End;

    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Result := True;
End;

Function GilesModel.ReadLightmap(Source:Stream; ChunkEnd:Integer):Boolean;
Var
  ChunkID, ChunkSize:Cardinal;
  LMap:GilesLightmap;
  LSize:Integer;
Begin
  LMap := GilesLightmap.Create();
  Inc(_LightmapCount);
  SetLength(_Lightmaps, _LightmapCount);
  _Lightmaps[Pred(_LightmapCount)] := LMap;

  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_LMAP_NAME:
      Begin
        Source.ReadString(LMap.Name, True);
      End;

    GLS_LMAP_FILE:
      Begin
        Source.ReadString(LMap.FileName, True);
      End;

    GLS_LMAP_WIDTH:
      Begin
        Source.Read(@LMap.Width, 2);
      End;

    GLS_LMAP_HEIGHT:
      Begin
        Source.Read(@LMap.Height, 2);
      End;

    GLS_LMAP_NONUNIFORM:
      Begin
        Source.Read(@LMap.NoUniform, 1);
      End;

    GLS_LMAP_USECUSTOMTEXEL:
      Begin
        Source.Read(@LMap.UseCustomTexel, 1);
      End;

    GLS_LMAP_CUSTOMTEXEL:
      Begin
        Source.Read(@LMap.CustomTexel, 4);
      End;

    GLS_LMAP_REPACK:
      Begin
        Source.Read(@LMap.Repack, 1);
      End;

    GLS_LMAP_DATA:
      Begin
        LSize := LMap.Width * LMap.Height * 3;
        SetLength(LMap.Data, LSize);
        Source.Read(@LMap.Data[0], LSize);
      End;

    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Result := True;
End;


Function GilesModel.ReadRenderSettings(Source:Stream; ChunkEnd:Integer):Boolean;
Var
  ChunkID, ChunkSize:Cardinal;
Begin
  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_RENDER_CLEARBEFORERENDER:
      Begin
        Source.Read(@_Settings.ClearBeforeRender, 1);
      End;

    GLS_RENDER_DIRENABLE:
      Begin
        Source.Read(@_Settings.DirectIllum, 1);
      End;

    GLS_RENDER_GIENABLE:
      Begin
        Source.Read(@_Settings.GlobalIllum, 1);
      End;

    GLS_RENDER_RAYBIAS:
      Begin
        Source.Read(@_Settings.RayBias, 4);
      End;

    GLS_RENDER_DIRMULTIPLY:
      Begin
        Source.Read(@_Settings.DirectMultiply, 4);
      End;

    GLS_RENDER_DIRBACKSHAD:
      Begin
        Source.Read(@_Settings.CastShadowBackFace, 1);
      End;

    GLS_RENDER_DIRSHADOWS:
      Begin
        Source.Read(@_Settings.DirectShadows, 1);
      End;

    GLS_RENDER_DIRSOFT:
      Begin
        Source.Read(@_Settings.SoftShadows, 1);
      End;

    GLS_RENDER_DIRSOFTSAMPLES:
      Begin
        Source.Read(@_Settings.SoftShadowSamples, 1);
      End;

    GLS_RENDER_GIIGNORETEX:
      Begin
        Source.Read(@_Settings.GI_IgnoreTextures, 1);
      End;

    GLS_RENDER_GIITERATIONS:
      Begin
        Source.Read(@_Settings.GI_Iterations, 1);
      End;

    GLS_RENDER_GIDENSITY:
      Begin
        Source.Read(@_Settings.GI_Density, 1);
      End;

    GLS_RENDER_GISAMPLES:
      Begin
        Source.Read(@_Settings.GI_Samples, 1);
      End;

    GLS_RENDER_GIMULTIPLY:
      Begin
        Source.Read(@_Settings.GI_Multiply, 1);
      End;

    GLS_RENDER_SKYENABLE:
      Begin
        Source.Read(@_Settings.GI_SkyEnable, 1);
      End;

    GLS_RENDER_SKYRED:
      Begin
        Source.Read(@_Settings.GI_SkyRed, 1);
      End;

    GLS_RENDER_SKYGREEN:
      Begin
        Source.Read(@_Settings.GI_SkyGreen, 1);
      End;

    GLS_RENDER_SKYBLUE:
      Begin
        Source.Read(@_Settings.GI_SkyBlue, 1);
      End;

    GLS_RENDER_SKYMULTIPLY:
      Begin
        Source.Read(@_Settings.GI_SkyMultiply, 1);
      End;

    GLS_RENDER_AUTOBLUR:
      Begin
        Source.Read(@_Settings.AutoBlur, 1);
      End;

    GLS_RENDER_AUTOEXPAND:
      Begin
        Source.Read(@_Settings.AutoExpand, 1);
      End;

    GLS_RENDER_AUTOBLURRADIUS:
      Begin
        Source.Read(@_Settings.AutoBlurRadius, 1);
      End;
      
    Else
      Source.Skip(ChunkSize);
    End;

  Until (Source.Position>=ChunkEnd);

  Result := True;
End;

Function GilesModel.FindChunk(Source:Stream; ID:Cardinal; Var ChunkSize:Cardinal):Boolean;
Var
  ChunkID:Cardinal;
Begin
  Source.Read(@ChunkID, 4);
  Source.Read(@ChunkSize, 4);

  If (ChunkID = ID) Then
  Begin
    Result := True;
  End Else
  Begin
    Result := False;
    RaiseError('Unexpected chunk, expected '+HexStr(ID)+' but got '+HexStr(ChunkID));
  End;
End;

Function GilesModel.Load(Source:Stream):Boolean;
Var
  ChunkID:Cardinal;
  ChunkSize, ChunkEnd:Cardinal;
Begin
  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
    GLS_MODELS:
      Begin
        ChunkEnd := Source.Position + ChunkSize;
        Repeat
          If Not FindChunk(Source, GLS_MODEL, ChunkSize) Then
            Exit;

          ReadObject(Source, Source.Position + ChunkSize, Nil);
        Until (Source.Position>=ChunkEnd);
      End;

    GLS_MATERIALS:
      Begin
        ChunkEnd := Source.Position + ChunkSize;
        Repeat
          If Not FindChunk(Source, GLS_MAT, ChunkSize) Then
            Exit;

          ReadMaterial(Source, Source.Position + ChunkSize);
        Until (Source.Position>=ChunkEnd);
      End;

    GLS_TEXTURES:
      Begin
        ChunkEnd := Source.Position + ChunkSize;
        Repeat
          If Not FindChunk(Source, GLS_TEX, ChunkSize) Then
            Exit;

          ReadTexture(Source, Source.Position + ChunkSize);
        Until (Source.Position>=ChunkEnd);
      End;

    GLS_LIGHTMAPS:
      Begin
        ChunkEnd := Source.Position + ChunkSize;
        Repeat
          If Not FindChunk(Source, GLS_LMAP, ChunkSize) Then
            Exit;

          ReadLightmap(Source, Source.Position + ChunkSize);
        Until (Source.Position>=ChunkEnd);
      End;

    GLS_RENDER:
      Begin
        ReadRenderSettings(Source, Source.Position + ChunkSize);
      End;

    Else
      Source.Skip(ChunkSize);
    End;

  Until Source.EOF;
End;

Function GilesModel.GetGroupCount: Integer;
Begin
  Result := Self._GroupCount;
End;

Function GilesModel.GetDiffuseColor(GroupID: Integer): Color;
Begin
  Result := _Materials[_Groups[GroupID].MaterialIndex].DiffuseColor;
End;

Function GilesModel.GetDiffuseMapName(GroupID: Integer): TERRAString;
Begin
  Result := _Textures[_Materials[_Groups[GroupID].MaterialIndex].TextureIndex].FileName;
End;

Function GilesModel.GetGroupBlendMode(GroupID: Integer): Cardinal;
Var
  Mat:GilesMaterial;
Begin
  Mat := _Materials[_Groups[GroupID].MaterialIndex];
  Case Mat.BlendMode Of
  gilesMatBlendAlpha:     Result := blendBlend;
  gilesMatBlendMultiply:  Result := blendModulate;
  gilesMatBlendAdditive:  Result := blendAdd;
  Else
    Result := blendNone;
  End;
End;

Function GilesModel.GetGroupFlags(GroupID: Integer): Cardinal;
Var
  Mat:GilesMaterial;
Begin
  Result := 0;

  Mat := _Materials[_Groups[GroupID].MaterialIndex];

  If ((Mat.Effects And gilesMatFX_TwoSided)<>0) Then
  Begin
    Result := Result Or meshGroupDoubleSided;
  End;

  {gilesMatFX_FullBright = 1;
  gilesMatFX_VertexColor = 2;
  gilesMatFX_FlatShade = 4;
  gilesMatFX_NoFog = 8;
  gilesMatFX_TwoSided = 16;
  gilesMatFX_VertexAlpha = 32;}
End;

Function GilesModel.GetTriangleCount(GroupID: Integer): Integer;
Begin
  Result := _Groups[GroupID].TriangleCount;
End;

Function GilesModel.GetTriangle(GroupID, Index: Integer): Triangle;
Begin
  Result := _Groups[GroupID].Triangles[Index];
End;

Function GilesModel.GetVertexCount(GroupID: Integer): Integer;
Begin
  Result := _Groups[GroupID].VertexCount;
End;

Function GilesModel.GetVertexFormat(GroupID: Integer): Cardinal;
Begin
  Result := meshFormatNormal Or meshFormatColor Or meshFormatUV1; //  meshFormatUV2
End;

Function GilesModel.GetVertexPosition(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Groups[GroupID].Vertices[Index].Position;
End;

Function GilesModel.GetVertexNormal(GroupID, Index: Integer): Vector3D;
Begin
  Result := _Groups[GroupID].Vertices[Index].Normal;
End;

Function GilesModel.GetVertexUV(GroupID, Index: Integer): Vector2D;
Begin
  Result := _Groups[GroupID].Vertices[Index].TextureCoords;
End;

Class function GilesModel.Save(Dest: Stream; MyMesh: MeshFilter): Boolean;
Begin

End;

Initialization
  RegisterMeshFilter(GilesModel, 'GLS');
End.