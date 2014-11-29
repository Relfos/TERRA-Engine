Unit TERRA_Plane;
{$I terra.inc}

Interface
Uses TERRA_Math, TERRA_Vector3D;

Type
  PPlane =^ Plane;
  Plane = Packed Object
    A,B,C,D:Single;

    Function Distance(X,Y,Z:Single):Single; Overload;
    Function Distance(V:Vector3D):Single; Overload;

    Function Normal:Vector3D;

    Procedure Normalize();
  End;

Function PlaneCreate(PA,PB,PC,PD:Single):Plane;Overload;
Function PlaneCreate(V1,V2,V3:Vector3D):Plane;Overload;
Function PlaneCreate(Source, Normal: Vector3D):Plane;Overload;

Function GetQuadHeight(A,B,C,D, Position:Vector3D; Normal:PVector3D=Nil):Single;

Implementation

Function Plane.Distance(X,Y,Z:Single):Single; {$IFDEF FPC} Inline; {$ENDIF}
Begin
  // This function will simply perform a dot product of the point and this plane.
  Result := A*X + B*Y + C*Z + D;
End;

Function Plane.Distance(V:Vector3D):Single;  {$IFDEF FPC} Inline; {$ENDIF}
Begin
  Result := A*V.X + B*V.Y + C*V.Z + D;
End;

Function PlaneCreate(PA,PB,PC,PD:Single):Plane;
Begin
  Result.A := PA;
  Result.B := PB;
  Result.C := PC;
  Result.D := PD;
End;

Function PlaneCreate(V1,V2,V3:Vector3D):Plane;
Var
  N:Vector3D;
Begin
  N := TriangleNormal(V1,V2,V3);
  Result := PlaneCreate(V1,N);
End;

Function PlaneCreate(Source, Normal: Vector3D):Plane;
Begin
  Result.A := Normal.X;
  Result.B := Normal.Y;
  Result.C := Normal.Z;
  Result.D := -VectorDot(Source, Normal);
End;

{
Function PlaneDotCoord(P:Plane; Vector:Vector3D):Single;
Begin
  With P, Vector Do
    Result := A*X + B*Y + C*Z + D*1;
End;

Function PlaneDotNormal(P:Plane; Normal:Vector3D):Single;
Begin
  With P, Normal Do
    Result := A*X + B*Y + C*Z + D*0;
End;


Function GetMirrorPlane(Const StartVertex,EndVertex,Normal:Vector3D):Plane;
Var
  Vertex:Array[0..2]Of Vector3D;
Begin
  Vertex[0]:=StartVertex;
  Vertex[1]:=StartVertex;
  Vertex[1].Y:=EndVertex.Y;
  Vertex[2]:=EndVertex;

   //determining the coefficients in plane's equation
  Result.A:= ( (vertex[1].y - vertex[0].y)*(vertex[2].z - vertex[0].z) ) - ( (vertex[1].z - vertex[0].z)*(vertex[2].y - vertex[0].y) );
  Result.B:= ( (vertex[1].z - vertex[0].z)*(vertex[2].x - vertex[0].x) ) - ( (vertex[2].z - vertex[0].z)*(vertex[1].x - vertex[0].x) );
  Result.C:= ( (vertex[1].x - vertex[0].x)*(vertex[2].y - vertex[0].y) ) - ( (vertex[1].y - vertex[0].y)*(vertex[2].x - vertex[0].x) );
  Result.D:= - ( vertex[0].x* Result.A + vertex[0].y*Result.B + vertex[0].z*Result.C );
End;
 }

{
A----B
|    |
|    |
C----D
}
Function GetQuadHeight(A,B,C,D, Position:Vector3D; Normal:PVector3D=Nil):Single;
Var
  P:Plane;
  coefX, coefZ:Single;
Begin
  coefX := B.x - position.x;
  coefZ := B.z - position.z;

//1. Decide which triangle it's in:
  if (coefX >= coefZ) Then // you are in ABD
    P := PlaneCreate(A,B,D)
  Else //we're in ACD
    P := PlaneCreate(A,C,D);

  Result := (-position.x * P.a - position.z * P.c - P.d) / P.b;

  If Assigned(Normal) Then
  Begin
    Normal^ := VectorCreate(-P.A, -P.B, -P.C);
    Normal.Normalize();
  End;
End;

Function Plane.Normal: Vector3D;
Begin
  Result.X := A;
  Result.Y := B;
  Result.Z := C;
  Result.Normalize;
End;

Procedure Plane.Normalize;
Var
  DD:Single;
Begin
  DD := Sqrt(a * a + b * b + c * c);
  If (DD>0) Then
  Begin
    Self.a := a / DD;
    Self.b := b / DD;
    Self.c := c / DD;
    Self.d := d / DD;
  End Else
  Begin
    Self.a := 0;
    Self.b := 0;
    Self.c := 0;
    Self.d := 0;
  End;
End;

End.