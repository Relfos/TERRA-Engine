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
 * TERRA_BoundingBox
 * Implements an axis aligned bounding box
 ***********************************************************************************************************************
}
Unit TERRA_BoundingBox;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Math, TERRA_Vector3D, TERRA_Matrix4x4;

Type
  BoundingBoxVertices=Array[1..8] Of Vector3D;

  PBoundingBox = ^ BoundingBox;
  BoundingBox = Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    Public
      StartVertex:Vector3D;
      EndVertex:Vector3D;

      Constructor Create(Const A,B:Vector3D);
      Procedure Reset;

      Procedure Add(Const P:Vector3D); Overload;
      Procedure Add(Const B:BoundingBox); Overload;

      Procedure Scale(Const S:Single); Overload;
      Procedure Scale(Const V:Vector3D); Overload;

      Function Intersect(Const P:Vector3D):Boolean; Overload;
      Function Intersect(Const B:BoundingBox):Boolean; Overload;

      Procedure GetVertices(Out Vertices:BoundingBoxVertices);

      Procedure Transform(Const M:Matrix4x4);

      Function Contains(Const P:Vector3D):Boolean;

      Function Center:Vector3D;
      Function Size:Vector3D;
      Function Radius:Single;

      // Test if is inside B
      Function Inside(Const B:BoundingBox):Boolean;
  End;

  BoundingSphere = Record
    Center:Vector3D;
    Radius:Single;
  End;


Function BoundingSphereCreate(Const A:BoundingBox):BoundingSphere; Overload;
Function BoundingSphereCreate(Const Center:Vector3D; Const Radius:Single):BoundingSphere; Overload;

Implementation

Procedure BoundingBox.Reset;
Begin
  StartVertex := Vector3D_Create(99999,99999,99999);
  EndVertex := Vector3D_Create(-99999,-99999,-99999);
End;

Procedure BoundingBox.Add(Const P:Vector3D); {$IFDEF FPC} Inline; {$ENDIF}
Begin
  If (P.X < StartVertex.X) Then
    StartVertex.X := P.X;
  If (P.X > EndVertex.X) Then
    EndVertex.X := P.X;

  If (P.Y < StartVertex.Y) Then
    StartVertex.Y := P.Y;
  If (P.Y > EndVertex.Y) Then
    EndVertex.Y := P.Y;

  If (P.Z < StartVertex.Z) Then
    StartVertex.Z := P.Z;
  If (P.Z > EndVertex.Z) Then
    EndVertex.Z := P.Z;
End;

Function BoundingBox.Center:Vector3D;
Begin
  Result.X := StartVertex.X + (EndVertex.X - StartVertex.X) * 0.5;
  Result.Y := StartVertex.Y + (EndVertex.Y - StartVertex.Y) * 0.5;
  Result.Z := StartVertex.Z + (EndVertex.Z - StartVertex.Z) * 0.5;
End;

Function BoundingBox.Size:Vector3D;
Begin
  Result.X := EndVertex.X - StartVertex.X;
  Result.Y := EndVertex.Y - StartVertex.Y;
  Result.Z := EndVertex.Z - StartVertex.Z;
End;

Function BoundingBox.Radius:Single;
Var
  N:Vector3D;
Begin
  N := Self.Size;
  Result := FloatMax(N.X, FloatMax(N.Y, N.Z)) * 0.5;
End;

Procedure BoundingBox.Scale(Const S:Single);
Var
  C:Vector3D;
Begin
  C := Self.Center;
  StartVertex := Vector3D_Add(C, Vector3D_Scale(Vector3D_Subtract(StartVertex, C),S));
  EndVertex := Vector3D_Add(C, Vector3D_Scale(Vector3D_Subtract(EndVertex, C) ,S));
End;

Procedure BoundingBox.Scale(Const V:Vector3D);
Var
  C:Vector3D;
Begin
  C := Self.Center;
  StartVertex := Vector3D_Add(C, Vector3D_Multiply(Vector3D_Subtract(StartVertex, C), V));
  EndVertex := Vector3D_Add(C, Vector3D_Multiply(Vector3D_Subtract(EndVertex, C) , V));
End;

Constructor BoundingBox.Create(Const A,B:Vector3D);
Begin
  StartVertex := Vector3D_Min(A,B);
  EndVertex := Vector3D_Max(A,B);
End;

Procedure BoundingBox.Add(Const B:BoundingBox);
Begin
  Add(B.StartVertex);
  Add(B.EndVertex);
End;

Procedure BoundingBox.GetVertices(Out Vertices:BoundingBoxVertices);
Begin
  Vertices[1].X:= StartVertex.X;
  Vertices[1].Y:= StartVertex.Y;
  Vertices[1].Z:= StartVertex.Z;

  Vertices[2].X := EndVertex.X;
  Vertices[2].Y := StartVertex.Y;
  Vertices[2].Z := StartVertex.Z;

  Vertices[3].X := StartVertex.X;
  Vertices[3].Y := StartVertex.Y;
  Vertices[3].Z := EndVertex.Z;

  Vertices[4].X := EndVertex.X;
  Vertices[4].Y := StartVertex.Y;
  Vertices[4].Z := EndVertex.Z;

  Vertices[5].X:= StartVertex.X;
  Vertices[5].Y:= EndVertex.Y;
  Vertices[5].Z:= StartVertex.Z;

  Vertices[6].X:= EndVertex.X;
  Vertices[6].Y:= EndVertex.Y;
  Vertices[6].Z:= StartVertex.Z;

  Vertices[7].X:= StartVertex.X;
  Vertices[7].Y:= EndVertex.Y;
  Vertices[7].Z:= EndVertex.Z;

  Vertices[8].X:= EndVertex.X;
  Vertices[8].Y:= EndVertex.Y;
  Vertices[8].Z:= EndVertex.Z;
End;

Procedure BoundingBox.Transform(Const M:Matrix4x4);
Var
  I:Integer;
  Vertices:BoundingBoxVertices;
Begin
  GetVertices(Vertices);

  Self.Reset;
  For I:=1 To 8 Do
  Begin
    Vertices[I] := M.Transform(Vertices[I]);
    Self.Add(Vertices[I]);
  End;
End;

Function BoundingBox.Intersect(Const P:Vector3D):Boolean;
Begin
 Result:=(P.X >= StartVertex.X) And
         (P.Y >= StartVertex.Y) And
         (P.Z >= StartVertex.Z) And
         (P.X <= EndVertex.X)   And
         (P.Y <= EndVertex.Y)   And
         (P.Z <= EndVertex.Z);
End;

Function BoundingBox.Intersect(Const B:BoundingBox):Boolean;
Begin
 Result:=((StartVertex.X<=B.EndVertex.X) And
          (StartVertex.Y<=B.EndVertex.Y) And
          (StartVertex.Z<=B.EndVertex.Z) And
          (EndVertex.X>=B.StartVertex.X) And
          (EndVertex.Y>=B.StartVertex.Y) And
          (EndVertex.Z>=B.StartVertex.Z));
End;

Function BoundingBox.Inside(Const B:BoundingBox):Boolean;
Begin
 Result:=((StartVertex.X<B.EndVertex.X) And
          (StartVertex.Y<B.EndVertex.Y) And
          (StartVertex.Z<B.EndVertex.Z) And
          (EndVertex.X>B.StartVertex.X) And
          (EndVertex.Y>B.StartVertex.Y) And
          (EndVertex.Z>B.StartVertex.Z) And
          (StartVertex.X>B.StartVertex.X) And
          (StartVertex.Y>B.StartVertex.Y) And
          (StartVertex.Z>B.StartVertex.Z) And
          (EndVertex.X<B.EndVertex.X) And
          (EndVertex.Y<B.EndVertex.Y) And
          (EndVertex.Z<B.EndVertex.Z));
End;

Function BoundingSphereCreate(Const A:BoundingBox):BoundingSphere;
Begin
  Result.Radius := FloatMax(A.EndVertex.X-A.StartVertex.X, FloatMax(A.EndVertex.Y-A.StartVertex.Y, A.EndVertex.Z-A.StartVertex.Z));
  Result.Radius := Result.Radius * 0.5;
  With Result.Center Do
  Begin
    X := A.StartVertex.X + (A.EndVertex.X - A.StartVertex.X) * 0.5;
    Y := A.StartVertex.Y + (A.EndVertex.Y - A.StartVertex.Y) * 0.5;
    Z := A.StartVertex.Z + (A.EndVertex.Z - A.StartVertex.Z) * 0.5;
  End;
End;

Function BoundingSphereCreate(Const Center:Vector3D; Const Radius:Single):BoundingSphere;
Begin
  Result.Center := Center;
  Result.Radius := Radius;
End;


Function BoundingBox.Contains(const P: Vector3D): Boolean;
Begin
  Result := (P.X>=StartVertex.X) And (P.X<=EndVertex.X) And (P.Y>=StartVertex.Y) And (P.Y<=EndVertex.Y)
    And (P.Z>=StartVertex.Z) And (P.Z<=EndVertex.Z);
End;

End.