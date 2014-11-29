{
  Giles mesh loader/saver
}

Unit TERRA_Giles;

{$I terra.inc}
Interface
Uses TERRA_Utils, TERRA_Math, TERRA_IO, TERRA_INI, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Color, TERRA_FileIO, TERRA_FileUtils, TERRA_Quaternion, TERRA_MeshFilter;

Type
  GilesObject = Class
    Name:AnsiString;
    Position:Vector3D;
    Rotation:Vector3D;
    Scale:Vector3D;
    Props:AnsiString;
    FileName:AnsiString;
    Hidden:Byte;

    Parent:GilesObject;
    Children:Array Of GilesObject;
    ChildrenCount:Integer;
  End;

  GilesObjectClass = Class Of GilesObject;

  GilesMesh = Class;

  GilesSurface = Class
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
  End;

  GilesModel = Class(MeshFilter)
    Protected
      _Objects:Array Of GilesObject;
      _ObjectCount:Integer;

      Function ReadObject(Source:Stream; ChunkEnd:Integer; Parent:GilesObject):Boolean;
      Function ReadMesh(Source:Stream; ChunkEnd:Integer; Obj:GilesMesh):Boolean;
      Function ReadPivot(Source:Stream; ChunkEnd:Integer; Obj:GilesPivot):Boolean;
      Function ReadLight(Source:Stream; ChunkEnd:Integer; Obj:GilesLight):Boolean;

      Function ReadSurface(Source:Stream; ChunkEnd:Integer; Surface:GilesSurface):Boolean;

      Function FindChunk(Source:Stream; ID:Cardinal; Var ChunkSize:Cardinal):Boolean;

    Public
      Function Load(Source:Stream):Boolean; Override;
      Class Function Save(Dest:Stream; MyMesh:MeshFilter):Boolean; Override;

      Function GetGroupCount:Integer; Override;
      Function GetGroupName(GroupID:Integer):AnsiString; Override;
      Function GetGroupFlags(GroupID:Integer):Cardinal; Override;
      Function GetGroupBlendMode(GroupID:Integer):Cardinal; Override;

      Function GetTriangleCount(GroupID:Integer):Integer; Override;
      Function GetTriangle(GroupID, Index:Integer):Triangle; Override;

      Function GetVertexCount(GroupID:Integer):Integer; Override;
      Function GetVertexFormat(GroupID:Integer):Cardinal; Override;
      Function GetVertexPosition(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexNormal(GroupID, Index:Integer):Vector3D; Override;
      Function GetVertexBone(GroupID, Index:Integer):Integer; Override;
      Function GetVertexUV(GroupID, Index:Integer):Vector2D; Override;

      Function GetDiffuseColor(GroupID:Integer):Color; Override;

      Function GetDiffuseMapName(GroupID:Integer):AnsiString; Override;
      Function GetSpecularMapName(GroupID:Integer):AnsiString; Override;
      Function GetEmissiveMapName(GroupID:Integer):AnsiString; Override;
  End;


Implementation
Uses TERRA_Log, TERRA_Canvas, TERRA_Image, TERRA_Application, TERRA_ResourceManager,
  Math;

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
sds
  Repeat
    Source.Read(@ChunkID, 4);
    Source.Read(@ChunkSize, 4);

    Case ChunkID Of
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
    Else
      Source.Skip(ChunkSize);
    End;

  Until Source.EOF;
End;


Initialization
  RegisterMeshFilter(GilesModel, 'GLS');
End.