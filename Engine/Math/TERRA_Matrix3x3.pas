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
 * TERRA_Matrix3x3
 * Implements a 3x3 matrix
 ***********************************************************************************************************************
}
Unit TERRA_Matrix3x3;
{$I terra.inc}

Interface
Uses TERRA_Vector2D, TERRA_Vector3D, TERRA_Matrix4x4;

Type
  PMatrix3x3 = ^Matrix3x3;
  Matrix3x3=Packed {$IFDEF USE_OLD_OBJECTS}Object{$ELSE}Record{$ENDIF}
    V:Array [0..8] Of Single;

    Function Transform(Const P:Vector2D):Vector2D; Overload;
    Function Transform(Const P:Vector3D):Vector3D; Overload;

    Procedure Init(Const M:Matrix4x4);

    Procedure SetTranslation(Const P:Vector2D);
    Function GetTranslation():Vector2D;
  End;

Const
 MatrixIdentity3x3:Matrix3x3= (V:(1.0, 0.0, 0.0,
                              0.0, 1.0, 0.0,
                              0.0, 0.0, 1.0));

// Returns a rotation matrix
Function MatrixRotation2D(Const Angle:Single):Matrix3x3; {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixRotationAndScale2D(Const Angle, ScaleX, ScaleY:Single):Matrix3x3; {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixTransformAroundPoint2D(Const Center:Vector2D; Const Mat:Matrix3x3):Matrix3x3;

// Returns a translation matrix
Function MatrixTranslation2D(Const Translation:Vector2D):Matrix3x3;Overload; {$IFDEF FPC}Inline;{$ENDIF}
Function MatrixTranslation2D(Const X,Y:Single):Matrix3x3;Overload; {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixScale2D(Const Scale:Vector2D):Matrix3x3;Overload; {$IFDEF FPC}Inline;{$ENDIF}
Function MatrixScale2D(Const X,Y:Single):Matrix3x3;Overload;   {$IFDEF FPC}Inline;{$ENDIF}
Function MatrixScale2D(Const Scale:Single):Matrix3x3;Overload;   {$IFDEF FPC}Inline;{$ENDIF}

Function MatrixInverse2D(Const Mat:Matrix3x3):Matrix3x3;

Function MatrixTranspose2D(Const Mat:Matrix3x3):Matrix3x3;

Function MatrixSkew2D(TX, TY:Single):Matrix3x3;

// Multiplys two matrices
Function MatrixMultiply3x3(Const A,B:Matrix3x3):Matrix3x3;

Implementation
Uses TERRA_Math{$IFDEF NEON_FPU},TERRA_NEON{$ENDIF};

Procedure Matrix3x3.Init(Const M: Matrix4x4);
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


Function MatrixTranslation2D(Const Translation:Vector2D):Matrix3x3;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := MatrixTranslation2D(Translation.X,Translation.Y);
End;

Function MatrixTranslation2D(Const X,Y:Single):Matrix3x3;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result.V[0] := 1.0;
  Result.V[1] := 0.0;
  Result.V[2] := 0.0;

  Result.V[3] := 0.0;
  Result.V[4] := 1.0;
  Result.V[5] := 0.0;

  Result.V[6] := X;
  Result.V[7] := Y;
  Result.V[8] := 1.0;
End;

Function Matrix3x3.GetTranslation():Vector2D;
Begin
  Result.X := Self.V[6];
  Result.Y := Self.V[7];
End;

Procedure Matrix3x3.SetTranslation(Const P:Vector2D);
Begin
  Self.V[6] := P.X;
  Self.V[7] := P.Y;
End;

Function Matrix3x3.Transform(Const P:Vector2D):Vector2D;
Begin
  Result.X := P.X*V[0] + P.Y*V[3] + V[6];
  Result.Y := P.X*V[1] + P.Y*V[4] + V[7];
End;


Function Matrix3x3.Transform(Const P:Vector3D):Vector3D;
Begin
  Result.X := P.X*V[0] + P.Y*V[3] + V[6];
  Result.Y := P.X*V[1] + P.Y*V[4] + V[7];
  Result.Z := P.Z;
End;

Function MatrixTranspose2D(Const Mat:Matrix3x3):Matrix3x3;
Begin
  Result.V[0] := Mat.V[0];
  Result.V[1] := Mat.V[3];
  Result.V[2] := Mat.V[6];

  Result.V[3] := Mat.V[1];
  Result.V[4] := Mat.V[4];
  Result.V[5] := Mat.V[7];

  Result.V[6] := Mat.V[2];
  Result.V[7] := Mat.V[5];
  Result.V[8] := Mat.V[8];
End;

{
  0 1 2
  3 4 5
  6 7 8
}

//http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html
Function MatrixInverse2D(Const Mat:Matrix3x3):Matrix3x3;
Var
  InvDet, Det:Double;
Begin
  Det :=  Mat.V[0] * Mat.V[4] * Mat.V[8]+
          Mat.V[3] * Mat.V[7] * Mat.V[2] +
          Mat.V[6] * Mat.V[1] * Mat.V[5] -
          Mat.V[0] * Mat.V[7] * Mat.V[5] -
          Mat.V[6] * Mat.V[4] * Mat.V[2] -
          Mat.V[3] * Mat.V[1] * Mat.V[8];

  InvDet := 1.0 / Det;

  Result.V[0] := ((Mat.V[4] * Mat.V[8]) - (Mat.V[5] * Mat.V[7])) * InvDet;
  Result.V[1] := ((Mat.V[2] * Mat.V[7]) - (Mat.V[1] * Mat.V[8]))  * InvDet;
  Result.V[2] := ((Mat.V[1] * Mat.V[5]) - (Mat.V[2] * Mat.V[4]))  * InvDet;


  Result.V[3] := ((Mat.V[5] * Mat.V[6]) - (Mat.V[3] * Mat.V[8])) * InvDet;
  Result.V[4] := ((Mat.V[0] * Mat.V[8]) - (Mat.V[2] * Mat.V[6]))  * InvDet;
  Result.V[5] := ((Mat.V[2] * Mat.V[3]) - (Mat.V[0] * Mat.V[5]))  * InvDet;

  Result.V[6] := ((Mat.V[3] * Mat.V[7]) - (Mat.V[4] * Mat.V[6])) * InvDet;
  Result.V[7] := ((Mat.V[1] * Mat.V[6]) - (Mat.V[0] * Mat.V[7]))  * InvDet;
  Result.V[8] := ((Mat.V[0] * Mat.V[4]) - (Mat.V[1] * Mat.V[3]))  * InvDet;
End;

Function MatrixRotation2D(Const Angle:Single):Matrix3x3;  {$IFDEF FPC}Inline;{$ENDIF}
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

Function MatrixRotationAndScale2D(Const Angle, ScaleX, ScaleY:Single):Matrix3x3; {$IFDEF FPC}Inline;{$ENDIF}
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

Function MatrixTransformAroundPoint2D(Const Center:Vector2D; Const Mat:Matrix3x3):Matrix3x3;
Var
  ToOrigin, FromOrigin:Matrix3x3;
Begin
  ToOrigin := MatrixTranslation2D(-Center.X, -Center.Y);
  FromOrigin := MatrixTranslation2D(Center);

  Result := MatrixMultiply3x3(FromOrigin, MatrixMultiply3x3(Mat, ToOrigin));
End;

Function MatrixScale2D(Const Scale:Vector2D):Matrix3x3;  {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := MatrixScale2D(Scale.X, Scale.Y);
End;

Function MatrixScale2D(Const Scale:Single):Matrix3x3;Overload;   {$IFDEF FPC}Inline;{$ENDIF}
Begin
  Result := MatrixScale2D(Scale, Scale);
End;

Function MatrixScale2D(Const X,Y:Single):Matrix3x3;  {$IFDEF FPC}Inline;{$ENDIF}
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

Function MatrixMultiply3x3(Const A,B:Matrix3x3):Matrix3x3;
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
	Result.V[8] := 1.0; //A.V[2]*B.V[6] + A.V[5]*B.V[7] + A.V[8]*B.V[8];
{$ENDIF}
End;

Function MatrixSkew2D(TX, TY:Single):Matrix3x3;
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
