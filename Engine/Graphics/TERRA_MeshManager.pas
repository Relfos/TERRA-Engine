Unit TERRA_MeshManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Application, TERRA_GraphicsManager,
  TERRA_Resource, TERRA_ResourceManager, TERRA_Mesh, TERRA_Vector3D;

Type
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
      Constructor Create();
      Procedure Release; Override;

      Function GetMesh(Name:TERRAString):TERRAMesh;

      Property CubeMesh:TERRAMesh Read GetCubeMesh;
      Property CylinderMesh:TERRAMesh Read GetCylinderMesh;
      Property SphereMesh:TERRAMesh Read GetSphereMesh;
      Property PlaneMesh:TERRAMesh Read GetPlaneMesh;

      Property Meshes[Name:TERRAString]:TERRAMesh Read GetMesh; Default;
   End;

Function CreatePlaneMesh(Const Normal:Vector3D; SubDivisions:Cardinal):TERRAMesh;

Implementation
Uses TERRA_Solids, TERRA_MeshFilter, TERRA_EngineManager, TERRA_FileManager;

{ MeshManager }
Constructor MeshManager.Create;
Begin
  Inherited;

//  Self.UseThreads := True;
End;


Procedure MeshManager.Release;
Begin
  Inherited;

  ReleaseObject(_CubeMesh);
  ReleaseObject(_SphereMesh);
  ReleaseObject(_CylinderMesh);
  ReleaseObject(_PlaneMesh);
End;

Function MeshManager.GetMesh(Name:TERRAString):TERRAMesh;
Var
  I, N:Integer;
  Location:TERRALocation;
  Filter:MeshFilter;
Begin
  Result := Nil;
  Name := StringTrim(Name);
  If (Name='') Then
    Exit;

  Result := TERRAMesh(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    Location := Engine.Files.Search(Name+'.mesh');
    If Assigned(Location) Then
    Begin
      Result := TERRAMesh.Create(rtLoaded, Location);
      Result.Priority := 60;
      Self.AddResource(Result);
    End Else
    Begin
      (*N := -1;
      For I:=0 To Pred(MeshFilterCount) Do
      Begin
        S := Engine.Files.SearchResourceFile(Name+'.'+MeshFilterList[I].Extension);
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
      End;*)
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
    Plane := TERRA_Solids.PlaneMesh.Create(Vector3D_Up, 4);
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
  Plane := TERRA_Solids.PlaneMesh.Create(Normal, SubDivisions);
  Result := CreateMeshFromSolid(Plane);
  ReleaseObject(Plane);
End;

End.
