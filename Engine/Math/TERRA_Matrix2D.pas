Unit TERRA_Matrix2D;
{$I terra.inc}

Interface
Uses TERRA_Math, TERRA_Vector2D, TERRA_Vector3D, TERRA_Matrix;

Type
  PMatrix2D = ^Matrix2D;
  Matrix2D=Packed Object
    V:Array [0..8] Of Single;

    Function Transform(Const P:Vector2D):Vector2D; Overload;
    Function Transform(Const P:Vector3D):Vector3D; Overload;

    Procedure Init(Const M:Matrix);
  End;

Const
 MatrixIdentity2D:Matrix2D= (V:(1.0, 0.0, 0.0,
                              0.0, 1.0, 0.0,
                              0.0, 0.0, 1.0));

// Returns a rotation matrix
Function MatrixRotation2D(Const Angle:Single):Matrix2D; {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixRotationAndScale2D(Const Angle, ScaleX, ScaleY:Single):Matrix2D; {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixTransformAroundPoint2D(Const Center:Vector2D; Const Mat:Matrix2D):Matrix2D;

// Returns a translation matrix
Function MatrixTranslation2D(Const Translation:Vector2D):Matrix2D;Overload; {$IFDEF FPC}Inline;{$ENDIF}
Function MatrixTranslation2D(Const X,Y:Single):Matrix2D;Overload; {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixScale2D(Const Scale:Vector2D):Matrix2D;Overload; {$IFDEF FPC}Inline;{$ENDIF}
Function MatrixScale2D(Const X,Y:Single):Matrix2D;Overload;   {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixInverse2D(Const Mat:Matrix2D):Matrix2D;

Function MatrixSkew2D(TX, TY:Single):Matrix2D;

// Multiplys two matrices
Function MatrixMultiply3x3(Const A,B:Matrix2D):Matrix2D;

Implementation
Uses Math{$IFDEF NEON_FPU},TERRA_NEON{$ENDIF};

{
  0 1 2
  3 4 5
  6 7 8
}

Function Matrix2D.Transform(Const P:Vector2D):Vector2D;
Begin
  Result.X := P.X * V[0] + P.Y * V[1] + V[2];
  Result.Y := P.X * V[3] + P.Y * V[4] + V[5];
End;

Procedure Matrix2D.Init(Const M: Matrix);
Begin
  V[0] := M.V[0];
  V[1] := M.V[1];
  V[2] := M.V[2];

  V[3] := M.V[4];
  V[4] := M.V[5];
  V[5] := M.V[6];

  V[6] := M.V[8];
  V[7] := M.V[9];
  V[8] := M.V[10];
End;

Function Matrix2D.Transform(Const P:Vector3D):Vector3D;
Begin
  Result.X := P.X * V[0] + P.Y * V[1] + V[2];
  Result.Y := P.X * V[3] + P.Y * V[4] + V[5];
  Result.Z := P.Z;
End;

Function MatrixInverse2D(Const Mat:Matrix2D):Matrix2D;
Var
  Det:Double;
Begin
  Det := Mat.V[0]*Mat.V[3] - Mat.V[1]*Mat.V[2];
  Result.V[0] := Mat.V[3]/det;
  Result.V[1] := -Mat.V[1]/det;
  Result.V[2] := -Mat.V[2]/det;
  Result.V[3] := Mat.V[0]/det;
End;

Function MatrixRotation2D(Const Angle:Single):Matrix2D;  {$IFDEF FPC}Inline;{$ENDIF}
Var
  S,C:Single;
Begin
  C := Cos(Angle);
	S := Sin(Angle);
  Result.V[0] := C;
  Result.V[1] := S;
  Result.V[2] := 0.0;
  Result.V[3] := -S;
  Result.V[4] := C;
  Result.V[5] := 0.0;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 1.0;
End;

Function MatrixRotationAndScale2D(Const Angle, ScaleX, ScaleY:Single):Matrix2D; {$IFDEF FPC}Inline;{$ENDIF}
Var
  S,C:Single;
Begin
  C := Cos(Angle);
	S := Sin(Angle);
  Result.V[0] := C * ScaleX;
  Result.V[1] := S * ScaleX;
  Result.V[2] := 0.0;
  Result.V[3] := -S * ScaleY;
  Result.V[4] := C * ScaleY;
  Result.V[5] := 0.0;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 1.0;
End;

Function MatrixTransformAroundPoint2D(Const Center:Vector2D; Const Mat:Matrix2D):Matrix2D;
Var
  A,B:Matrix2D;
Begin
  A := MatrixTranslation2D(-Center.X, -Center.Y);
  B := MatrixTranslation2D(Center);

  Result :=  MatrixMultiply3x3(A, MatrixMultiply3x3(Mat, B));
End;

Function MatrixTranslation2D(Const Translation:Vector2D):Matrix2D;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := MatrixTranslation2D(Translation.X,Translation.Y);
End;

Function MatrixTranslation2D(Const X,Y:Single):Matrix2D;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result.V[0] := 1.0;
  Result.V[1] := 0.0;
  Result.V[2] := X;
  Result.V[3] := 0.0;
  Result.V[4] := 1.0;
  Result.V[5] := Y;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 1.0;
End;

Function MatrixScale2D(Const Scale:Vector2D):Matrix2D;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := MatrixScale2D(Scale.X, Scale.Y);
End;

Function MatrixScale2D(Const X,Y:Single):Matrix2D;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result.V[0] := X;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;
  Result.V[3] := 0.0;
  Result.V[4] := Y;
  Result.V[5] := 0.0;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 1.0;
End;

{
  0 1 2
  3 4 5
  6 7 8
}

Function MatrixMultiply3x3(Const A,B:Matrix2D):Matrix2D;
Begin
{$IFDEF NEON_FPU}
  matmul3_neon(@A,@B,@Result);
{$ELSE}
	Result.V[0] := A.V[0]*B.V[0] + A.V[3]*B.V[1] + A.V[6]*B.V[2];
	Result.V[1] := A.V[1]*B.V[0] + A.V[4]*B.V[1] + A.V[7]*B.V[2];
	Result.V[2] := A.V[2]*B.V[0] + A.V[5]*B.V[1] + A.V[8]*B.V[2];

	Result.V[3] := A.V[0]*B.V[3] + A.V[3]*B.V[4] + A.V[6]*B.V[5];
	Result.V[4] := A.V[1]*B.V[3] + A.V[4]*B.V[4] + A.V[7]*B.V[5];
	Result.V[5] := A.V[2]*B.V[3] + A.V[5]*B.V[4] + A.V[8]*B.V[5];

	Result.V[6] := A.V[0]*B.V[6] + A.V[3]*B.V[7] + A.V[6]*B.V[8];
	Result.V[7] := A.V[1]*B.V[6] + A.V[4]*B.V[7] + A.V[7]*B.V[8];
	Result.V[8] := A.V[2]*B.V[6] + A.V[5]*B.V[7] + A.V[8]*B.V[8];
{$ENDIF}
End;

Function MatrixSkew2D(TX, TY:Single):Matrix2D;
Begin
  If (TX<>0) Then
    TX := Tan(TX);

  If (TY<>0) Then
    TY := Tan(TY);

  Result.V[0] := 1.0;
  Result.V[1] := TX;
  Result.V[2] := 0.0;
  Result.V[3] := TY;
  Result.V[4] := 1.0;
  Result.V[5] := 0.0;
  Result.V[6] := 0.0;
  Result.V[7] := 0.0;
  Result.V[8] := 1.0;
End;

End.
