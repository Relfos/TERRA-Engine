Unit TERRA_MeshOctree;

{$I terra.inc}
Interface
Uses TERRA_Mesh, TERRA_Octree, TERRA_Vector3D, TERRA_Ray, TERRA_Utils,
  TERRA_Color, TERRA_DebugDraw;

Type
  MeshOctreeElement = Class(OctreeElement)
    Public
      Index, GroupID:Integer;
      P1,P2,P3:Vector3D;

      Constructor Create(Index, GroupID:Integer; P1,P2,P3:Vector3D);

      Function Intersect(Const R:Ray; Var T:Single):Boolean; Override;
      Procedure Render; Override;
  End;

Function CreateOctreeFromMesh(MyMesh:Mesh):Octree;

Implementation
Uses TERRA_ResourceManager;

{ MeshOctreeElement }
Constructor MeshOctreeElement.Create(Index, GroupID: Integer; P1, P2,P3: Vector3D);
Begin
  Self.Index := Index;
  Self.GroupID := GroupID;
  Self.P1 := P1;
  Self.P2 := P2;
  Self.P3 := P3;
  Self.Box.Reset;
  Self.Box.Add(P1);
  Self.Box.Add(P2);
  Self.Box.Add(P3);
End;

Function MeshOctreeElement.Intersect(const R: Ray; var T: Single): Boolean;
Var
  U,V:Single;
Begin
  Result := R.TriangleIntersect(P1,P2,P3, T, U, V);
End;

Function CreateOctreeFromMesh(MyMesh:Mesh):Octree;
Var
  G:MeshGroup;
  I,J :Integer;
  T:Triangle;
  TT:MeshOctreeElement;
Begin
  MeshManager.Instance.PreFetch(MyMesh);
  Result := Octree.Create(MyMesh.BoundingBox);
  For I:=0 To Pred(MyMesh.GroupCount) Do
  Begin
    G := MyMesh.GetGroup(I);
    For J:=0 To Pred(G.TriangleCount) Do
    Begin
      T := G.Triangles[J];
      TT := MeshOctreeElement.Create(J, I, G.Vertices[T.A].Position, G.Vertices[T.B].Position, G.Vertices[T.C].Position);
      Result.AddElement(TT);
    End;
  End;
End;


Procedure MeshOctreeElement.Render;
Begin
  DrawBoundingBox(Box, ColorWhite);
End;

End.