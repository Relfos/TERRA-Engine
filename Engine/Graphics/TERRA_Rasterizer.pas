{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_Rasterizer
 * Implements a triangle software rasterizer
 ***********************************************************************************************************************
}
Unit TERRA_Rasterizer;
{$I terra.inc}

{
  How to use:

  1 - Attach a target image to a rasterizer.Target property
  2 - Add the X and Y of each of the 3 corners with
      Rasterizer.SetInterpolatorValues(rasterX, X1, X2, X3);
      Rasterizer.SetInterpolatorValues(rasterY, Y1, Y2, Y3);
  3 - Add any other values for interpolation (optional)
  4 - Call Rasterizer.Rasterize to output a triangle to the target image

}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Image, TERRA_Color, TERRA_Vector3D;

Const
  rasterX   = 0;
  rasterY   = 1;
  rasterZ   = 2;
  rasterU   = 3;
  rasterV   = 4;
  rasterW   = 5;
  rasterNX  = 6;
  rasterNY  = 7;
  rasterNZ  = 8;

Type
  InterpolatorValues = Array[0..2] Of Single;

  RasterizerTriangle = Class(TERRAObject)
    Protected
      _Interpolators:Array Of InterpolatorValues;
      _InterpolatorCount:Integer;

    Public
      // all coordinates are in normalized space (0.0 to 1.0)
      Procedure SetInterpolatorValues(Index:Integer; A,B,C:Single); Overload;
  End;

  TERRARasterizer = Class(ImageIterator)
    Protected
      _MinY, _MaxY:Integer;
      _IL, _IR:Array Of Array Of Single;

      _Value, _Add:Array Of Single;

      _Triangles:Array Of RasterizerTriangle;
      _TriangleCount:Integer;

      _CurrentTriangle:RasterizerTriangle;
      _TriangleIndex:Integer;

      _CurrentY:Integer;
      _BufferInitialized:Boolean;
      _PixelCount:Integer;

      Function DoPixel():Boolean;
      Procedure DoSide(A,B:Integer);

      Procedure InitBuffers;
      Function InitLine():Boolean;

      Function ObtainNext():Boolean; Override;

    Public
      Procedure AddTriangle(Tri:RasterizerTriangle);

      Function GetRasterValue(Index:Integer):Single;

  End;

Procedure FillGutter(Target, GutterMask:TERRAImage; Levels:Integer);

Implementation
Uses TERRA_Log;

Function TERRARasterizer.DoPixel:Boolean;
Var
  I:Integer;
Begin
  If (_PixelCount<=0) Then
  Begin
    Result := InitLine();
    If Result Then
    Begin
      Result := Self.InitLine();
    End;

    If Not Result Then
      Exit;
  End;

  _X := Trunc(_Value[rasterX]);
  _Y := Trunc(_Value[rasterY]);

  For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
    _Value[I] := _Value[I] + _Add[I];

  Dec(_PixelCount);

  If (_PixelCount<=0) Then
    Inc(_CurrentY);

  Result := True;
End;

Function TERRARasterizer.InitLine():Boolean;
Var
  I,D:Integer;
  O:Word;
  U,V,Ux,Vx:Single;
  X1,X2:Integer;
  Y:Integer;
Begin
  If (_CurrentY>_MaxY) Then
  Begin
    Result := False;
    Exit;
  End;

  Y := _CurrentY;

  For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
    _Value[I] := _IL[Y, I];

  X1 := Trunc(_IL[Y, rasterX]);
  X2 := Trunc(_IR[Y, rasterX]);

  _PixelCount := (X2 - X1);

  If _PixelCount<=0 Then
  Begin
    For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
      _Add[I] := 0;

    Inc(_CurrentY);
    Result := Self.InitLine();
    Exit;
  End;

  For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
    _Add[I] := (_IR[Y,I] - _IL[Y,I]) / _PixelCount;

  Result := True;
End;

Procedure TERRARasterizer.DoSide(A,B:Integer);
Var
  I:Integer;
  Y1,Y2:Integer;
  Temp,Y:Integer;
Begin
  Y1 := Trunc(_CurrentTriangle._Interpolators[rasterY, A]);
  Y2 := Trunc(_CurrentTriangle._Interpolators[rasterY, B]);

  If (Y1=Y2) Then
    Exit;

  If (Y1>Y2) Then
  Begin
    DoSide(B, A);
    Exit;
 End;

  For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
  Begin
    _Value[I] := _CurrentTriangle._Interpolators[I, A];
    _Add[I] := (_CurrentTriangle._Interpolators[I, B] - _CurrentTriangle._Interpolators[I, A]) /(Y2-Y1);
  End;

  For Y:=Y1 To Y2 Do
  Begin
    If (Y>=_Target.Height) Then
      Exit;

    If Y>_MaxY Then
      _MaxY := Y;
    If Y<_MinY Then
      _MinY := Y;

    If Y>=0 Then
    Begin
      If _Value[rasterX]<_IL[Y, rasterX] Then
      Begin
        For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
          _IL[Y, I] := _Value[I];
      End;

      If _Value[rasterX]>_IR[Y, rasterX] Then
      Begin
        For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
          _IR[Y, I] := _Value[I];
      End;
    End;

    For I:=0 To Pred(_CurrentTriangle._InterpolatorCount) Do
      _Value[I] := _Value[I] + _Add[I];
 End;
End;

Procedure TERRARasterizer.InitBuffers;
Var
  I:Integer;
Begin
  _CurrentTriangle := _Triangles[_TriangleIndex];

  SetLength(_IL, _Target.Height, _CurrentTriangle._InterpolatorCount);
  SetLength(_IR, _Target.Height, _CurrentTriangle._InterpolatorCount);

  SetLength(_Value, _CurrentTriangle._InterpolatorCount);
  SetLength(_Add, _CurrentTriangle._InterpolatorCount);

  For I:=0 To Pred(_Target.Height) Do
  Begin
    _IL[I, rasterX] := _Target.Width;
    _IR[I, rasterX] := 0;
  End;

  _MinY := _Target.Height;
  _MaxY := 0;

  _CurrentY := 0;

  DoSide(0, 1);
  DoSide(1, 2);
  DoSide(2, 0);
End;

Function TERRARasterizer.ObtainNext: Boolean;
Begin
  If (_TriangleCount<=0) Then
  Begin
    Result := False;
    Exit;
  End;

  If (Not _BufferInitialized) Then
  Begin
    Self.InitBuffers();
    _BufferInitialized := True;
  End;

  Result := DoPixel();

  If (Not Result) And (_TriangleIndex<Pred(_TriangleCount)) Then
  Begin
    ReleaseObject(_Triangles[_TriangleIndex]);
    Inc(_TriangleIndex);

    _BufferInitialized := False;

    Result := Self.ObtainNext();
  End;
End;

Procedure TERRARasterizer.AddTriangle(Tri: RasterizerTriangle);
Begin
  Inc(_TriangleCount);
  SetLength(_Triangles, _TriangleCount);

  _Triangles[Pred(_TriangleCount)] := Tri;
End;

{ RasterizerTriangle }
Procedure RasterizerTriangle.SetInterpolatorValues(Index:Integer; A,B,C:Single);
Begin
  If (Index>Pred(_InterpolatorCount)) Then
  Begin
    _InterpolatorCount := Succ(Index);
    SetLength(_Interpolators, _InterpolatorCount);
  End;

  (*If (Index = rasterX) And (Normalized) Then
  Begin
    A := A * _Target.Width;
    B := B * _Target.Width;
    C := C * _Target.Width;
  End;

  If (Index = rasterY) And (Normalized) Then
  Begin
    A := A * _Target.Height;
    B := B * _Target.Height;
    C := C * _Target.Height;
  End;*)

  _Interpolators[Index, 0] := A;
  _Interpolators[Index, 1] := B;
  _Interpolators[Index, 2] := C;
End;


Function TERRARasterizer.GetRasterValue(Index: Integer): Single;
Begin
  Result := _Value[Index];
End;

Type
  GutterTexel = Record
    X,Y:Integer;
    PX,PY:Integer;
  End;

Procedure FillGutter(Target, GutterMask:TERRAImage; Levels:Integer);
Var
  _GutterTexels:Array Of GutterTexel;
  _GutterCount:Integer;
  P:ColorRGBA;
  I,J,K:Integer;
  Mask:TERRAImage;

Function FindGutter(Var Texel:GutterTexel):Boolean;
Const
  GX:Array[0..7] Of Integer = (-1,1,-1,0,1,-1,0,1);
  GY:Array[0..7] Of Integer = (0, 0, -1,-1,-1, 1,1 ,1);
Var
  P:ColorRGBA;
  I,J:Integer;
  PX,PY:Integer;
Begin
  Result := True;
  For I:=0 To 7 Do
  Begin
    PX := Texel.X + GX[I];
    PY := Texel.Y + GY[I];

    If (PX<0) Or (PY<0) Or (PX>=Mask.Width) Or (PY>=Mask.Height) Then
      Continue;

    P := Mask.GetPixel(PX,PY);
    If (P.A<>0) Then
    Begin
      Texel.PX := PX;
      Texel.PY := PY;
      Exit;
    End;
  End;

  Result := False;
End;

Begin
  _GutterCount := 0;
  If Assigned(GutterMask) Then
    Mask := TERRAImage.Create(GutterMask)
  Else
    Mask := TERRAImage.Create(Target);

  For K:=1 To Levels Do
  Begin
    For J:=0 To Pred(Mask.Height) Do
      For I:=0 To Pred(Mask.Width) Do
      Begin
        (*If (I=18) And (J=0) Then
          IntToString(2);*)

        P := Mask.GetPixel(I,J);
        If (P.A=0) Then
        Begin
          Inc(_GutterCount);
          SetLength(_GutterTexels, _GutterCount);
          _GutterTexels[Pred(_GutterCount)].X := I;
          _GutterTexels[Pred(_GutterCount)].Y := J;
          If Not FindGutter(_GutterTexels[Pred(_GutterCount)]) Then
            Dec(_GutterCount);
        End;
      End;

    For I:=0 To Pred(_GutterCount) Do
      Mask.SetPixel(_GutterTexels[I].X, _GutterTexels[I].Y, ColorWhite);
  End;

  ReleaseObject(Mask);
  Mask := Nil;

  For I:=0 To Pred(_GutterCount) Do
  Begin
    P := Target.GetPixel(_GutterTexels[I].PX, _GutterTexels[I].PY);
    Target.SetPixel(_GutterTexels[I].X, _GutterTexels[I].Y, P);
  End;
End;



End.

