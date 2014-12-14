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
 * TERRA_Lightmapper
 * Implements a lightmapper
 ***********************************************************************************************************************
}

Unit TERRA_LightMapper;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Mesh, TERRA_Vector3D, TERRA_Matrix, TERRA_Color, TERRA_Ray, TERRA_Math, TERRA_Image;

{$DEFINE OCCLUSION}
{$DEFINE SHADOWS}

Const
  ShadowRays = 4;
  OcclusionRays = 64;
  LightMapSize = 128;

Type
  LightPixel = Record
    Position:Vector3D;
    Normal:Vector3D;
    Color:TERRA_Color.Color;
    Working:Boolean;
    GID:Integer;
    TID:Integer;
  End;

  Light = Record
    Position:Vector3D;
    R,G,B:Single;
  End;

  Lightmapper = Class
    Protected
      _Target:Mesh;
      _Pixels:Array Of Array Of LightPixel;
      _Dest:Image;

      _GID:Integer;
      _TID:Integer;

      _Lights:Array Of Light;
      _LightCount:Integer;

      Function GetPixelsLeft:Integer;

    Public

      Procedure AddLight(P:Vector3D; C:Color);

      Constructor Create(Target:Mesh);

      Procedure UnwrapMesh(Callback:ProgressCallback=Nil);
      Procedure GenerateMap(Callback:ProgressCallback=Nil);
  End;

Implementation
Uses TERRA_Packer, TERRA_Unwrap, TERRA_Rasterizer, TERRA_Octree, TERRA_BoundingBox,
  TERRA_ThreadPool, TERRA_Application;

Type
  LightRasterizer = Class(Rasterizer)
    Protected
      Procedure SetColor(Values:PSingleArray; Dest:PColor); Override;
    Public
      MyOctree:Octree;
      Mapper:Lightmapper;
  End;

  OctreeTriangle = Class(OctreeElement)
    A,B,C:Vector3D;

    Function Intersect(Const R:Ray; Var T:Single):Boolean; Override;
  End;

{ Lightmapper }
Procedure Lightmapper.AddLight(P: Vector3D; C: Color);
Begin
  Inc(_LightCount);
  SetLength(_Lights, _LightCount);
  _Lights[Pred(_LightCount)].Position := P;
  _Lights[Pred(_LightCount)].R := C.R /255;
  _Lights[Pred(_LightCount)].G := C.G /255;
  _Lights[Pred(_LightCount)].B := C.B /255;
End;

Constructor Lightmapper.Create(Target: Mesh);
Begin
  _Target := Target;
  _Lightcount := 0;

  AddLight(VectorCreate(50, 200, 50), ColorWhite);
End;

Type
  LightRect = Class
    X1,Y1, X2,Y2:Integer;
    Mapper:LightMapper;

    Constructor Create(X1,Y1, X2,Y2:Integer; Mapper:LightMapper);
  End;

Constructor LightRect.Create(X1,Y1, X2,Y2:Integer; Mapper:LightMapper);
Begin
  Self.X1 := X1;
  Self.Y1 := Y1;
  Self.X2 := X2;
  Self.Y2 := Y2;
  Self.Mapper := Mapper;
End;

Function TestRay(Const R:Ray; Mapper:LightMapper):Boolean;
Var
  T,U,V:Single;
  I,J:Integer;
  Group:MeshGroup;
  A,B,C:Vector3D;
Begin
  Result := False;

  For I:=0 To Pred(Mapper._Target.GroupCount) Do
  Begin
    Group := Mapper._Target.GetGroup(I);
    For J:=0 To Pred(Group.TriangleCount) Do
    Begin
      A := Group.Vertices[Group.Triangles[J].A].Position;
      B := Group.Vertices[Group.Triangles[J].B].Position;
      C := Group.Vertices[Group.Triangles[J].C].Position;

      Result := R.TriangleIntersect(A,B,C, T, U, V);
      If (Result) And (T>0) Then
        Exit;
    End;
  End;
End;

Procedure LightMapSimple(P:Pointer);
Var
  Rect:LightRect;
  I,J,K,N:Integer;
  Normal:Vector3D;
  Result:Color;
  Ray:TERRA_Ray.Ray;
  Count:Integer;
  L:Vector3D;
  R,G,B,S:Single;
  Att:Single;
Begin
  Rect := LightRect(P);
  For J:=Rect.Y1 To Rect.Y2 Do
    For I:=Rect.X1 To Rect.X2 Do
    If (Rect.Mapper._Pixels[I,J].Working) Then
    Begin
      R := 0.0;
      G := 0.0;
      B := 0.0;

      {Normal := Rect.Mapper._Pixels[I,J].Normal;
      Normal.Scale(0.5);
      Normal.Add(VectorUniform(0.5));
      Result := ColorCreate(Normal.X, Normal.Y, Normal.Z, 1.0);
      }
      For K:=0 To Pred(Rect.Mapper._LightCount) Do
      Begin
        {$IFDEF SHADOWS}
        Count := 0;
        For N:=1 To ShadowRays Do
        Begin
          Ray.Origin := Rect.Mapper._Pixels[I,J].Position;

          L := Rect.Mapper._Lights[K].Position;
          If (N>1) Then
          Begin
            Normal := VectorCreate(RandomFloat(-1, 1), RandomFloat(-1, 1), RandomFloat(-1, 1));
            //Normal.Normalize;
            Normal.Scale(4);
            L.Add(Normal);
          End;

          Ray.Direction := VectorSubtract(L, Ray.Origin);

          Ray.Direction.Normalize;
          Ray.Origin.Add(VectorScale(Ray.Direction, 10));

          //If (MyOctree.Intersect(R, T)) And (T>0) Then
          If TestRay(Ray, Rect.Mapper) Then
            Inc(Count);
        End;

        Att := Ray.Origin.Distance(Rect.Mapper._Lights[K].Position);
        Att := 1.0 - (Att/300);
        If (Att>0) Then
        Begin
          S := (1.0-(Count/ShadowRays)) * Att;
          R := R + S * Rect.Mapper._Lights[K].R;
          G := G + S * Rect.Mapper._Lights[K].G;
          B := B + S * Rect.Mapper._Lights[K].B;
        End;
        {$ENDIF}
      End;

      {$IFDEF OCCLUSION}
        Count := 0;
        For N:=1 To OcclusionRays Do
        Begin
          Ray.Origin := Rect.Mapper._Pixels[I,J].Position;
          Ray.Direction := VectorCreate(RandomFloat(-1, 1), RandomFloat(-1, 1), RandomFloat(-1, 1));
          Ray.Direction.Normalize;
          Ray.Origin.Add(VectorScale(Ray.Direction, 0.005));

          If TestRay(Ray, Rect.Mapper) Then
            Inc(Count);
        End;

        S := 1.0-(Count/OcclusionRays);
        R := R * S;
        G := G * S;
        B := B * S;
      {$ENDIF}

      R := R * 255;
      G := G * 255;
      B := B * 255;
      If (R>255) Then R := 255;
      If (G>255) Then G := 255;
      If (B>255) Then B := 255;

      Result.R := Trunc(R);
      Result.G := Trunc(G);
      Result.B := Trunc(B);
      Result.A := 255;
      Rect.Mapper._Dest.SetPixel(I,J, Result);
      Rect.Mapper._Pixels[I,J].Working := False;
    End;
  Rect.Destroy;
End;

Procedure Lightmapper.GenerateMap(Callback: ProgressCallback);
Var
  Rasterizer:LightRasterizer;
  I,J,K:Integer;
  A,B,C:Integer;
  VA,VB,VC:PMeshVertex;
  Group:MeshGroup;
  Tri:OctreeTriangle;
  Gutter:GutterFiller;
  Last,N:Integer;
Begin
  //Dest := Image.Create(2048, 2048);
  _Dest := Image.Create(LightMapSize, LightMapSize);
  Rasterizer := LightRasterizer.Create;
  Rasterizer.Target := _Dest;
  Rasterizer.MyOctree := Octree.Create(_Target.BoundingBox);
  Rasterizer.Mapper := Self;

  SetLength(_Pixels, _Dest.Width, _Dest.Height);

  For I:=0 To Pred(_Target.GroupCount) Do
  Begin
    Group := _Target.GetGroup(I);
    For J:=0 To Pred(Group.TriangleCount) Do
    Begin
      A := Group.Triangles[J].A;
      B := Group.Triangles[J].B;
      C := Group.Triangles[J].C;

      VA := @(Group.Vertices[A]);
      VB := @(Group.Vertices[B]);
      VC := @(Group.Vertices[C]);

      Tri := OctreeTriangle.Create;
      Tri.Box.Reset;
      Tri.Box.Add(VA.Position);
      Tri.Box.Add(VB.Position);
      Tri.Box.Add(VC.Position);
      Tri.A := VA.Position;
      Tri.B := VB.Position;
      Tri.C := VC.Position;

      Rasterizer.MyOctree.AddElement(Tri);
    End;
  End;

  For I:=0 To Pred(_Target.GroupCount) Do
  Begin
    For J:=0 To Pred(_Dest.Height) Do
      For K:=0 To Pred(_Dest.Width) Do
        _Pixels[I,K].Working := False;
    _Dest.FillRectangleByUV(0, 0, 1, 1, ColorNull);

    _GID := I;
    Group := _Target.GetGroup(I);
    For J:=0 To Pred(Group.TriangleCount) Do
    Begin
      If Assigned(CallBack) Then
        Callback(notification_LightmapUnwrap, Trunc((J/Group.TriangleCount)*100));

      _TID := J;

      A := Group.Triangles[J].A;
      B := Group.Triangles[J].B;
      C := Group.Triangles[J].C;

      VA := @(Group.Vertices[A]);
      VB := @(Group.Vertices[B]);
      VC := @(Group.Vertices[C]);

      Rasterizer.SetInterpolatorValues(rasterX, VA.TextureCoords.X, VB.TextureCoords.X, VC.TextureCoords.X);
      Rasterizer.SetInterpolatorValues(rasterY, VA.TextureCoords.Y, VB.TextureCoords.Y, VC.TextureCoords.Y);

      Rasterizer.SetInterpolatorValues(2, VA.Normal.X, VB.Normal.X, VC.Normal.X);
      Rasterizer.SetInterpolatorValues(3, VA.Normal.Y, VB.Normal.Y, VC.Normal.Y);
      Rasterizer.SetInterpolatorValues(4, VA.Normal.Z, VB.Normal.Z, VC.Normal.Z);

      Rasterizer.SetInterpolatorValues(5, VA.Position.X, VB.Position.X, VC.Position.X);
      Rasterizer.SetInterpolatorValues(6, VA.Position.Y, VB.Position.Y, VC.Position.Y);
      Rasterizer.SetInterpolatorValues(7, VA.Position.Z, VB.Position.Z, VC.Position.Z);

      Rasterizer.Rasterize;
    End;

    //ThreadPool.Instance.RunTask(LightMapSimple, LightRect.Create(0, 0, Pred(_Dest.Width), Pred(_Dest.Height), Self), 50);
    ThreadPool.Instance.RunTask(LightMapSimple, LightRect.Create(0, 0, _Dest.Width Div 2, _Dest.Height Div 2, Self), 50);
    ThreadPool.Instance.RunTask(LightMapSimple, LightRect.Create(0, Succ(_Dest.Height Div 2), _Dest.Width Div 2, Pred(_Dest.Height), Self), 50);

    ThreadPool.Instance.RunTask(LightMapSimple, LightRect.Create(Succ(_Dest.Width Div 2), 0, Pred(_Dest.Width), _Dest.Height Div 2, Self), 50);
    ThreadPool.Instance.RunTask(LightMapSimple, LightRect.Create(Succ(_Dest.Width Div 2), Succ(_Dest.Height Div 2), Pred(_Dest.Width), Pred(_Dest.Height), Self), 50);

    Last := 0;
    Repeat
      N := GetPixelsLeft;
      Application.Instance.Yeld;

      If (Assigned(CallBack)) And (N<>Last) Then
        Callback(notification_LightmapRendering, 100 - Trunc(N/(_Dest.Width*_Dest.Height) *100));

      Last := N;
    Until (N<=0);

    Gutter := GutterFiller.Create(_Dest, 5);
    Gutter.Apply(_Dest);
    Gutter.Destroy;

    _Dest.Save('lightmap'+IntToString(I)+'.png');
  End;

  Rasterizer.MyOctree.Destroy;
  Rasterizer.Destroy;
  _Dest.Destroy;
End;

Function Lightmapper.GetPixelsLeft: Integer;
Var
  I,J:Integer;
Begin
  Result :=0 ;

  For J:=0 To Pred(_Dest.Height) Do
    For I:=0 To Pred(_Dest.Width) Do
    If (_Pixels[I,J].Working) Then
      Inc(Result);

End;

Procedure Lightmapper.UnwrapMesh(Callback:ProgressCallback=Nil);
Var
  I, J:Integer;
  A,B,C:Integer;
  V0,V1,V2:MeshVertex;
  Group:MeshGroup;
  Unwrapper:TERRA_Unwrap.Unwrapper;
  T:PUnwrapTriangle;
  Img:Image;
Begin
  For I:=0 To Pred(_Target.GroupCount) Do
  Begin
    Group := _Target.GetGroup(I);
    Unwrapper := TERRA_Unwrap.Unwrapper.Create;
    Unwrapper.SetTriangleCount(Group.TriangleCount);
    For J:=0 To Pred(Group.TriangleCount) Do
    Begin
      A := Group.Triangles[J].A;
      B := Group.Triangles[J].B;
      C := Group.Triangles[J].C;
      Unwrapper.AddTriangle(J, Group.Vertices[A].Position, Group.Vertices[B].Position, Group.Vertices[C].Position,
                            Group.Vertices[A].Normal, Group.Vertices[B].Normal, Group.Vertices[C].Normal,
                            TriangleNormal(Group.Vertices[A].Position, Group.Vertices[B].Position, Group.Vertices[C].Position));
    End;

    Unwrapper.Unwrap(Callback);

    Img := Unwrapper.GetTemplate;
    //Img.Save('unwrap'+IntToString(I)+'.png');
    Img.Destroy;

    Group.TriangleCount := 0;
    For J:=0 To Pred(Unwrapper.TriangleCount) Do
    Begin
      T := Unwrapper.GetTriangle(J);
      V0.Position := T.Vertex[0].Position;
      V0.Normal := T.Vertex[0].Normal;
      V0.TextureCoords := T.Vertex[0].UV;

      V1.Position := T.Vertex[1].Position;
      V1.Normal := T.Vertex[1].Normal;
      V1.TextureCoords := T.Vertex[1].UV;

      V2.Position := T.Vertex[2].Position;
      V2.Normal := T.Vertex[2].Normal;
      V2.TextureCoords := T.Vertex[2].UV;

      Group.AddTriangle(V0, V1, V2);
    End;

    Unwrapper.Destroy;
  End;
End;

{ LightRasterizer }
Procedure LightRasterizer.SetColor(Values: PSingleArray; Dest: PColor);
Var
  Position, Normal:Vector3D;
  R:Ray;
  T:Single;
  I,J,K,Count:Integer;
  X,Y:Integer;
  Hit:Boolean;
  Group:MeshGroup;
  A,B,C:Vector3D;
  U,V:Single;
  P:^LightPixel;
Begin
  Normal.X := Values[2];
  Normal.Y := Values[3];
  Normal.Z := Values[4];
  Normal.Normalize;

  Position.X := Values[5];
  Position.Y := Values[6];
  Position.Z := Values[7];

  X := Trunc(Values[rasterX]);
  Y := Trunc(Values[rasterY]);
  If (X<0) Or (X>=Self.Mapper._Dest.Width) Or (Y<0) Or (Y>=Self.Mapper._Dest.Height) Then
    Exit;

  P := @(Self.Mapper._Pixels[X, Y]);
  P.Position := Position;
  P.Normal := Normal;
  P.TID := Self.Mapper._TID;
  P.GID := Self.Mapper._GID;
  P.Working := True;
End;

{ OctreeTriangle }
Function OctreeTriangle.Intersect(const R: Ray; var T: Single): Boolean;
Var
  U,V:Single;
Begin
  Result := R.TriangleIntersect(A, B, C, T, U, V);
End;

End.