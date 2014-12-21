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
 ***********************************************************************************************************************

 TERRA_Clipping
 Implements Cohen–Sutherland clipping.
 ***********************************************************************************************************************
}

Unit TERRA_Clipping;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Classes, TERRA_Vector2D;

Type
  ClipperVertex = Record
    Position:Vector2D;
    UV:Vector2D;
  End;

  ClipperInputTriangle = Record
    Vertices:Array[0..2] Of ClipperVertex;
  End;

  ClipperOutputPolygon = Record
    Vertices:Array Of ClipperVertex;
    VertexCount:Integer;

    Triangles:Array Of Triangle;
    TriangleCount:Integer;
  End;

  RectangleClipper = Class
    Protected
      _Min:Vector2D;
      _Max:Vector2D;

      Function ComputeOutCode(P:Vector2D):Integer;
      Function CohenSutherlandLineClip(Var P0, P1:ClipperVertex):Boolean;

    Public
      Constructor Create(MinX,MinY,MaxX,MaxY:Single);
      Procedure SetClippingRect(MinX,MinY,MaxX,MaxY:Single);
      Function ClipTriangle(T:ClipperInputTriangle; Var P:ClipperOutputPolygon; NormalizePosition:Boolean):Boolean;

  End;

Implementation

Const
  INSIDE = 0; // 0000
  LEFT = 1;   // 0001
  RIGHT = 2;  // 0010
  BOTTOM = 4; // 0100
  TOP = 8;    // 1000

// Compute the bit code for a point (x, y) using the clip rectangle
// bounded diagonally by (xmin, ymin), and (xmax, ymax)
Function RectangleClipper.ComputeOutCode(P:Vector2D):Integer;
Begin
  Result := INSIDE;          // initialized as being inside of clip window

  If (P.x < _min.X) Then          // to the left of clip window
		Result := Result Or LEFT
	else
  If (P.x > _max.X) Then      // to the right of clip window
		Result := Result Or RIGHT;

	if (P.y < _min.Y) Then           // below the clip window
		Result := Result Or BOTTOM
  Else
	If (P.y > _max.Y) Then     // above the clip window
		Result := Result Or TOP;
End;

Function Interpolate(A,B,C:Single; U1,U2:Single):Single;
Var
  Temp, Alpha:Single;
Begin
  If (A>B) Then
  Begin
    Temp := A;
    A := B;
    B := Temp;

    Temp := U1;
    U1 := U2;
    U2 := Temp;
  End;

  Alpha := (C-A)/(B-A);
  Result := (1.0 - Alpha) * U1 + Alpha * U2;
End;

// Cohen–Sutherland clipping algorithm clips a line from
// P0 = (x0, y0) to P1 = (x1, y1) against a rectangle with
// diagonal from (xmin, ymin) to (xmax, ymax).
Function RectangleClipper.CohenSutherlandLineClip(Var P0, P1:ClipperVertex):Boolean;
Var
  outCode0, outCode1, outcodeOut:Integer;
  P:ClipperVertex;
Begin
	// compute outcodes for P0, P1, and whatever point lies outside the clip rectangle
	outcode0 := ComputeOutCode(P0.Position);
	outcode1 := ComputeOutCode(P1.Position);
	Result := False;

	While (true) Do
  Begin
		If (Not ((outcode0 Or outcode1)<>0)) Then
    Begin      //logical or is 0. Trivially accept and get out of loop
			Result := True;
			Break;
		End Else
		If ((outcode0 And outcode1) <>0) Then
    Begin      //Both are outside, and in the same clipping plane
      Exit;
		End Else
    Begin
			// failed both tests, so calculate the line segment to clip
			// from an outside point to an intersection with clip edge

			// At least one endpoint is outside the clip rectangle; pick it.
      If (outCode0<>0) Then
        outcodeOut := outCode0
      Else
        outcodeOut := outCode1;
      
			// Now find the intersection point;
			// use formulas y = y0 + slope * (x - x0), x = x0 + (1 / slope) * (y - y0)
			if (outcodeOut And TOP<>0) Then
      Begin           // point is above the clip rectangle
				P.Position.x := P0.Position.X + (P1.Position.X - P0.Position.X) * (_max.Y - P0.Position.Y) / (P1.Position.y - P0.Position.Y);
				P.Position.y := _max.Y;
			End Else
      if (outcodeOut And BOTTOM<>0) Then // point is below the clip rectangle
			Begin
      	P.Position.x := P0.Position.X + (P1.Position.X - P0.Position.X) * (_min.Y - P0.Position.Y) / (P1.Position.Y - P0.Position.Y);
				P.Position.y := _min.Y;
			End Else
      If (outcodeOut And RIGHT<>0) Then
      Begin  // point is to the right of clip rectangle
				P.Position.y := P0.Position.Y + (P1.Position.Y - P0.Position.Y) * (_max.X - P0.Position.X) / (P1.Position.X - P0.Position.X);
				P.Position.x := _max.X;
			End Else
      If (outcodeOut And LEFT<>0) Then
      Begin   // point is to the left of clip rectangle
				P.Position.y := P0.Position.Y + (P1.Position.Y - P0.Position.Y) * (_min.X - P0.Position.X) / (P1.Position.X - P0.Position.X);
				P.Position.x := _min.X;
			End;

      // interpolate UVs
      P.UV.X := Interpolate(P0.Position.X, P1.Position.X, P.Position.X, P0.UV.X, P1.UV.X);
      P.UV.Y := Interpolate(P0.Position.Y, P1.Position.Y, P.Position.Y, P0.UV.Y, P1.UV.Y);

			// Now we move outside point to intersection point to clip
			// and get ready for next pass.
			if (outcodeOut = outcode0) Then
      Begin
        P0 := P;
				outcode0 := ComputeOutCode(P0.Position);
			End Else
      Begin
        P1 := P;
				outcode1 := ComputeOutCode(P1.Position);
      End;
		End;
	End;
End;

Constructor RectangleClipper.Create(MinX,MinY,MaxX,MaxY:Single);
Begin
  SetClippingRect(MinX,MinY,MaxX,MaxY);
End;

Procedure RectangleClipper.SetClippingRect(MinX,MinY,MaxX,MaxY:Single);
Begin
  _Min := VectorCreate2D(MinX, MinY);
  _Max := VectorCreate2D(MaxX, MaxY);
End;

Function RectangleClipper.ClipTriangle(T:ClipperInputTriangle; Var P:ClipperOutputPolygon; NormalizePosition:Boolean):Boolean;
  Function ClipEdge(A,B:ClipperVertex):Boolean;
  Var
    I,J,N:Integer;
    V:Array[0..1] Of ClipperVertex;
  Begin
    Result := Self.CohenSutherlandLineClip(A, B);
    If (Result) Then
    Begin
      V[0] := A;
      V[1] := B;

      For J:=0 To 1 Do
      Begin
        N := -1;
        For I:=0 To Pred(P.VertexCount) Do
        If (P.Vertices[I].Position.Distance( V[J].Position)<=0.01)
        And (P.Vertices[I].UV.Distance(V[J].UV)<=0.01)  Then
        Begin
          N := I;
          Break;
        End;

        If (N<0) Then
        Begin
          Inc(P.VertexCount);
          SetLength(P.Vertices, P.VertexCount);
          P.Vertices[Pred(P.VertexCount)] := V[J];
        End;
      End;
    End;
  End;

Var
  I:Integer;
Begin
  P.VertexCount := 0;
  ClipEdge(T.Vertices[0], T.Vertices[1]);
  ClipEdge(T.Vertices[1], T.Vertices[2]);
  ClipEdge(T.Vertices[2], T.Vertices[0]);
  Result := P.VertexCount>=3;
  If (Result) Then
  Begin
    If (NormalizePosition) Then
    Begin
      For I:=0 To Pred(P.VertexCount) Do
      Begin
        P.Vertices[I].Position.X := (P.Vertices[I].Position.X - _Min.X) / (_Max.X - _Min.X);
        P.Vertices[I].Position.Y := (P.Vertices[I].Position.Y - _Min.Y) / (_Max.Y - _Min.Y);
      End;
    End;

    P.TriangleCount := P.VertexCount-2;
    SetLength(P.Triangles, P.TriangleCount);
    For I:=0 To Pred(P.TriangleCount) Do
    Begin
      P.Triangles[I].A := 0;
      P.Triangles[I].B := I+1;
      P.Triangles[I].C := I+2;
    End;
  End;

End;

End.

