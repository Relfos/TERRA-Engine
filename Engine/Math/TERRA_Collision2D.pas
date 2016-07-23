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
 * TERRA_Collision2D
 * Implements collision detection in 2D 
 ***********************************************************************************************************************
}
Unit TERRA_Collision2D;

{$I terra.inc}
Interface
Uses TERRA_Utils, TERRA_Math, TERRA_Vector2D, TERRA_Color;

Type
  Circle = Object
    Center:Vector2D;
    Radius:Single;

    // test if point P is inside this circle
    Function PointInside(P:Vector2D):Boolean;

    Function Intersect(C:Circle):Boolean;
  End;

  Polygon2D = Object
      Vertices:Array Of Vector2D;
      VertexCount:Integer;

      Procedure AddVertex(X, Y:Single); Overload;
      Procedure AddVertex(P:Vector2D); Overload;

      Procedure RemoveVertex(Index:Integer);

      // test if point P is inside this poly
      Function PointInside(P:Vector2D):Boolean;
      // test if poly P is inside this poly
      Function PolygonInside(P:Polygon2D):Boolean;

      Function Intersect(C:Circle; Hit:PVector2D = Nil):Boolean;
  End;

  Line2D = Object
    P1:Vector2D;
    P2:Vector2D;

    Function Intersect(C:Circle; Hit:PVector2D = Nil; Distance:PSingle = Nil):Boolean; Overload;
    Function Intersect(B:Line2D; Hit:PVector2D = Nil):Boolean; Overload;
    Function Intersect(P:Polygon2D; Hit:PVector2D = Nil):Boolean; Overload;

    Function PointInside(P:Vector2D):Boolean;
  End;

  PointCloud2D = Object
    Points:Array Of Vector2D;
    PointCount:Integer;

    Procedure AddPoint(P:Vector2D);
    Function GetConvexHull:Polygon2D;
    Function GetRect:Line2D;
  End;

  Function CircleCreate(Origin:Vector2D; Radius:Single):Circle; Overload;
  Function CircleCreate(X,Y:Single; Radius:Single):Circle; Overload;

  Function LineCreate2D(X1,Y1,X2,Y2:Single):Line2D; Overload;
  Function LineCreate2D(P1,P2:Vector2D):Line2D; Overload;

  Function TriangleArea2D(A,B,C:Vector2D):Single;

Implementation

Function TriangleArea2D(A,B,C:Vector2D):Single;
Begin
  Result := (A.X*B.Y + B.X * C.Y + C.X * A.Y - A.X*C.Y - B.X * A.Y - C.x* B.Y) * 0.5;
End;


Function CircleCreate(Origin:Vector2D; Radius:Single):Circle;
Begin
  Result.Center := Origin;
  Result.Radius := Radius;
End;

Function CircleCreate(X,Y:Single; Radius:Single):Circle;
Begin
  Result := CircleCreate(Vector2D_Create(X,Y), Radius);
End;

Function LineCreate2D(P1,P2:Vector2D):Line2D;
Begin
  Result.P1 := P1;
  Result.P2 := P2;
End;

Function LineCreate2D(X1,Y1,X2,Y2:Single):Line2D;
Begin
  Result.P1 := Vector2D_Create(X1,Y1);
  Result.P2 := Vector2D_Create(X2,Y2);
End;


Function Line2D.PointInside(P: Vector2D): Boolean;
Var
  MinX, MinY, MaxX, MaxY:Single;
Begin
  MinX := FloatMin(P1.X, P2.X);
  MinY := FloatMin(P1.Y, P2.Y);
  MaxX := FloatMax(P1.X, P2.X);
  MaxY := FloatMax(P1.Y, P2.Y);
  Result := (P.X>=MinX) And (P.X<=MaxX) And (P.Y>=MinY) And (P.Y<=MaxY);
End;

Function Line2D.Intersect(C:Circle; Hit:PVector2D; Distance:PSingle):Boolean;
{    const Vector2& c,        // center
    float r,                            // radius
    const Vector2& p1,     // segment start
    const Vector2& p2)     // segment end}
Var
  Dir, Diff:Vector2D;
  T, Distsqr:Single;
  Closest, D:Vector2D;
Begin
  Dir := P2;
  Dir.Subtract(P1);
  Diff := C.Center;
  Diff.Subtract(P1);

  T := Vector2D_Dot(Diff, Dir) / Vector2D_Dot(Dir, Dir);

  If (T < 0.0) Then
    T := 0.0;

  If (T > 1.0) Then
    T := 1.0;

  Closest := Dir;
  Closest.Scale(T);
  Closest.Add(P1);

  D := C.Center;
  D.Subtract(Closest);

  Distsqr := Vector2D_Dot(D, D);
  If Assigned(Distance) Then
    Distance^ := Sqrt(Distsqr);

  Result := (distsqr <= Sqr(C.Radius));
  If Assigned(Hit) Then
    Hit^ := Closest;
End;


Function Line2D.Intersect(B:Line2D; Hit:PVector2D = Nil):Boolean;
Var
  Denom, nume_a, nume_b:Single;
  Ua,Ub:Single;
Begin
  denom := (B.P2.y - B.P1.y) * (P2.x - P1.x) - (B.P2.x - B.P1.x) * (P2.y - P1.y);
  nume_a := (B.P2.x - B.P1.x) * (P1.y - B.P1.y) - (B.P2.y - B.P1.y) * (P1.x - B.P1.x);
  nume_b := (P2.x - P1.x) * (P1.y - B.P1.y) - (P2.y - P1.y) * (P1.x - B.P1.x);

  If (denom = 0.0) Then
  Begin
    If(nume_a = 0.0) And (nume_b = 0.0) Then
      Result := True // COINCIDENT
    Else
      Result := False; //PARALLEL;
    Exit;
  End;

  ua := nume_a / denom;
  ub := nume_b / denom;

  If (ua >= 0.0) And (ua <= 1.0) And (ub >= 0.0) And (ub <= 1.0) Then
  Begin
    // Get the intersection point.
    If (Assigned(Hit)) Then
    Begin
      Hit.X := P1.x + ua*(P2.x - P1.x);
      Hit.Y := P1.y + ua*(P2.y - P1.y);
    End;

    Result := True;
  End Else
    Result := False;
End;

Function Line2D.Intersect(P:Polygon2D; Hit:PVector2D):Boolean;
Var
  I,J:Integer;
Begin
  J := Pred(P.VertexCount);
  For I:=0 To Pred(P.VertexCount) Do
  Begin
    If (Self.Intersect(LineCreate2D(P.Vertices[I], P.Vertices[J]), Hit)) Then
    Begin
      Result := True;
      Exit;
    End;
    J := I;
  End;

  Result := False;
End;

{ Polygon2D }
Procedure Polygon2D.AddVertex(X, Y:Single);
Begin
  Self.AddVertex(Vector2D_Create(X,Y));
End;

Procedure Polygon2D.AddVertex(P:Vector2D);
Begin
  Inc(VertexCount);
  SetLength(Vertices, VertexCount);
  Vertices[Pred(VertexCount)] := P;
End;

Procedure Polygon2D.RemoveVertex(Index: Integer);
Var
  I:Integer;
Begin
  For I:=Index To Pred(VertexCount-1) Do
    Vertices[I] := Vertices[I+1];
  Dec(VertexCount);
End;

Function Polygon2D.Intersect(C:Circle; Hit:PVector2D = Nil):Boolean;
Var
  I,J:Integer;
Begin
  J := Pred(VertexCount);
  For I:=0 To Pred(VertexCount) Do
  Begin
    If (LineCreate2D(Vertices[I], Vertices[J]).Intersect(C, Hit)) Then
    Begin
      Result := True;
      Exit;
    End;
    J := I;
  End;

  Result := PointInside(C.Center);
End;

Function Polygon2D.PolygonInside(P:Polygon2D):Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(P.VertexCount) Do
  If (Not PointInside(P.Vertices[I])) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := True;
End;

Function Polygon2D.PointInside(P: Vector2D):Boolean;
Var
  I,Counter:Integer;
  P1,P2:Vector2D;
  xinters:Single;
Begin
  Counter := 0;
  If (VertexCount<=0) Then
  Begin
    Result := False;
    Exit;
  End;

  P1 := Vertices[0];
  For I:=1 To VertexCount Do
  Begin
    P2 := Vertices[I Mod VertexCount];
    If (P.y > FloatMin(p1.y,p2.y)) Then
      If (p.y <= FloatMax(p1.y,p2.y)) Then
        if (p.x <= FloatMax(p1.x,p2.x)) Then
          if (p1.y <>  p2.y) Then
          Begin
            xinters := ((p.y - p1.y) * (p2.x-p1.x)) / (p2.y-p1.y) + p1.x;
            if (p1.x = p2.x) Or (p.x <= xinters) Then
              Inc(counter);
          End;
    p1 := p2;
  End;

  Result := Odd(Counter);
End;

{ ConvexHull2D }

Procedure PointCloud2D.AddPoint(P: Vector2D);
Begin
  Inc(PointCount);
  SetLength(Points, PointCount);
  Points[Pred(PointCount)] := P;
End;

Function PointCloud2D.GetRect:Line2D;
Var
  I:Integer;
Begin
  Result.P1 := Points[0];
  Result.P2 := Points[0];
  For I:=1 To Pred(PointCount) Do
  Begin
    Result.P1.X := FloatMin(Result.P1.X, Points[I].X);
    Result.P1.Y := FloatMin(Result.P1.Y, Points[I].Y);
    Result.P2.X := FloatMax(Result.P2.X, Points[I].X);
    Result.P2.Y := FloatMax(Result.P2.Y, Points[I].Y);
  End;
End;

Function PointCloud2D.GetConvexHull: Polygon2D;
Var
  I,J, pointOnHull, firstPoint:Integer;
  AllOnLeft, Found:Boolean;
  Min:Single;
Begin
  Result.VertexCount := 0;

  // remove duplicate vertices
  I := 0;
  While (I<PointCount) Do
  Begin
    Found := False;
    For J:=0 To Pred(PointCount) Do
    If (I<>J) And (Points[J].Distance(Points[I])<=0.1) Then
    Begin
      Found := True;
      Break;
    End;

    If (Found) Then
    Begin
      Points[I] := Points[Pred(PointCount)];
      Dec(PointCount);
    End Else
      Inc(I);
  End;

  If (PointCount<3) Then
    Exit;

  If (PointCount=3) Then
  Begin
    For I:=0 To 2 Do
      Result.AddVertex(Points[I]);
    Exit;
  End;

  //    pointOnHull = leftmost point in S
  pointOnHull := 0;
  Min := Points[0].X;
  For I:=1 To Pred(PointCount) Do
  If (Points[I].X<Min) Then
  Begin
    Min := Points[I].X;
    pointOnHull := I;
  End;

  firstPoint := pointOnHull;
  Repeat
    Result.AddVertex(Points[pointOnHull]); //   P[i] = pointOnHull
    Found := False;
    For I:=0 To Pred(PointCount) Do
    If (I<>PointOnHull) Then
    Begin
      AllOnLeft := True;
      For J:=0 To Pred(PointCount) Do
      If (J<>I) And (J<>pointOnHull) Then
      Begin
        If (TriangleArea2D(Points[pointOnHull], Points[I], Points[J])<0) Then
        Begin
          AllOnLeft := False;
          Break;
        End;
      End;

      If (AllOnLeft) Then
      Begin
        pointOnHull := I;
        Found := True;
        Break;
      End;
    End;

    If Not Found Then
    Begin
    End;

  Until (pointOnHull = firstPoint) {Or (Not Found)};

{  If Not Found Then
  Begin
    Save('hullpoints.dat');
    Halt;
  End;}
End;

{Procedure PointCloud2D.Load(FileName:TERRAString);
Var
  Source:FileStream;
Begin
  Source := FileStream.Open(FileName);
  Source.Read(PointCount, 4);
  SetLength(Points, PointCount);
  Source.Read(Points[0], SizeOf(Vector2D) * PointCount);
  ReleaseObject(Source);
End;

Procedure PointCloud2D.Save(FileName:TERRAString);
Var
  Dest:FileStream;
Begin
  Dest := FileStream.Create(FileName);
  Dest.Write(PointCount, 4);
  Dest.Write(Points[0], SizeOf(Vector2D) * PointCount);
  ReleaseObject(Dest);
End;}

Function Circle.Intersect(C: Circle): Boolean;
Var
  Dx,Dy,R:Single;
Begin
  //compare the distance to combined radii
  Dx := C.Center.X - Self.Center.X;
  Dy := C.Center.Y - Self.Center.Y;
  R := C.Radius + Self.Radius;
  Result := (Sqr(Dx) + Sqr(Dy) < Sqr(R));
End;

{Procedure Polygon2D.Load(FileName:TERRAString);
Var
  Source:FileStream;
Begin
  Source := FileStream.Open(FileName);
  Source.Read(VertexCount, 4);
  SetLength(Vertices, VertexCount);
  If (VertexCount>0) Then
    Source.Read(Vertices[0], SizeOf(Vector2D) * VertexCount);
  ReleaseObject(Source);
End;

Procedure Polygon2D.Save(FileName:TERRAString);
Var
  Dest:FileStream;
Begin
  Dest := FileStream.Create(FileName);
  Dest.Write(VertexCount, 4);
  If (VertexCount>0) Then
    Dest.Write(Vertices[0], SizeOf(Vector2D) * VertexCount);
  ReleaseObject(Dest);
End;}

Function Circle.PointInside(P: Vector2D): Boolean;
Begin
  Result := (P.Distance(Center)<=Radius);
End;


End.