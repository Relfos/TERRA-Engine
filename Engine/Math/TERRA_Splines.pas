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
 * TERRA_Splines
 * Implements a 3D spline class
 ***********************************************************************************************************************
}
Unit TERRA_Splines;

{$I terra.inc}
Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Vector3D, TERRA_Color,
  TERRA_Quaternion, TERRA_Stream, TERRA_BoundingBox;

Type
  SplineControlPoint = Record
    Time:Single;
    Position:Vector3D;
  End;

  Spline = Class(TERRAObject)
    Protected
      _Points:Array Of SplineControlPoint;
      _PointCount:Integer;
      _Speed:Single;
      _Closed:Boolean;
      _Active:Boolean;
      _Geometric:Boolean;
      _NeedsBuild:Boolean;

      _BoundingBox:BoundingBox;

      _Ready:Boolean;
      _StartTime:Cardinal;
      _TotalLength:Single;

      Procedure Build;
      Function IsReady:Boolean;

      Function GetLength:Single;

      Function GetTime:Single;
      Procedure SetTime(Value:Single);

      Procedure GetParams(T:Single; Var Delta:Single; Var Index, Y0,Y1,Y2,Y3:Integer);
      Function GetTangent(Index:Integer):Vector3D;

    Public
      Constructor Create;
      Procedure Release; Override;

      Function GetPosition(T:Single):Vector3D;
      Function GetOrientation(T:Single):Quaternion;

	    Procedure SetPoint(Index:Integer; P:Vector3D);
	    Procedure AddPoint(P:Vector3D);

      Function GetPointByIndex(Index:Integer):Vector3D;

      Procedure Update;

      Procedure Synchronize(OtherSpline:Spline);

      Procedure Load(Source:Stream); Overload;
      Procedure Load(Const FileName:TERRAString); Overload;
      Procedure Save(Dest:Stream); Overload;
      Procedure Save(FileName:TERRAString); Overload;

      Property Speed:Single Read _Speed Write _Speed;
      Property Time:Single Read GetTime Write SetTime;
      Property Length:Single Read GetLength;
      Property Closed:Boolean Read _Closed Write _Closed;
      Property Geometric:Boolean Read _Geometric Write _Geometric;
      Property Ready:Boolean Read IsReady;
      Property PointCount:Integer Read _PointCount;
    End;

Implementation
Uses TERRA_Error, TERRA_Application, TERRA_FileStream, TERRA_OS;

{ Spline }
Constructor Spline.Create;
Begin
  _Speed := 200.0;
  _StartTime := Application.Instance.GetElapsedTime();
  _Closed := True;
  _NeedsBuild := False;
End;

Procedure Spline.AddPoint(P: Vector3D);
Begin
  Inc(_PointCount);
  SetLength(_Points, _PointCount);
  _Points[Pred(_PointCount)].Position := P;
  _NeedsBuild := True;
End;

Procedure Spline.GetParams(T:Single; Var Delta:Single; Var Index, Y0,Y1,Y2,Y3:Integer);
Var
  I:Integer;
Function GetOfs(N:Integer):Integer;
Begin
  If (N<0) Then
    Result := _PointCount + N
  Else
  If (N>=_PointCount) Then
    Result := N - _PointCount
  Else
    Result := N;
End;
Begin
  If (T<0.0) Then
    T := 0.0;

  If (T>1.0) Then
    T := 1.0;

  Index := -1;
  For I:=0 To Pred(_PointCount) Do
  If (_Points[I].Time>=T) Then
  Begin
    Index := I;
    Break;
  End;

  If (Index<0) Then
  Begin
    Index := 0;
    Delta := (T - _Points[GetOfs(Index-1)].Time) / (1.0 - _Points[GetOfs(Index-1)].Time);
  End Else
  Begin
    Delta := (T - _Points[GetOfs(Index-1)].Time) / (_Points[GetOfs(Index)].Time - _Points[GetOfs(Index-1)].Time);
  End;

  Y0 := GetOfs(Index - 2);
  Y1 := GetOfs(Index - 1);
  Y2 := GetOfs(Index + 0);
  Y3 := GetOfs(Index + 1);
End;

Function Spline.GetPosition(T: Single):Vector3D;
Var
  N:Integer;
  Delta:Single;
  Y0,Y1,Y2,Y3:Integer;
Begin
  If (_NeedsBuild) Then
    Build;

  If (_PointCount<2) Then
  Begin
    Result := VectorZero;
    Exit;
  End;

  GetParams(T, Delta, N, Y0,Y1,Y2,Y3);

  Result.X := CatmullRomInterpolate(_Points[Y0].Position.X, _Points[Y1].Position.X, _Points[Y2].Position.X, _Points[Y3].Position.X, Delta);
  Result.Y := CatmullRomInterpolate(_Points[Y0].Position.Y, _Points[Y1].Position.Y, _Points[Y2].Position.Y, _Points[Y3].Position.Y, Delta);
  Result.Z := CatmullRomInterpolate(_Points[Y0].Position.Z, _Points[Y1].Position.Z, _Points[Y2].Position.Z, _Points[Y3].Position.Z, Delta);

  {Result.X := LinearInterpolate(_Points[Y1].Position.X, _Points[Y2].Position.X, Delta);
  Result.Y := LinearInterpolate(_Points[Y1].Position.Y, _Points[Y2].Position.Y, Delta);
  Result.Z := LinearInterpolate(_Points[Y1].Position.Z, _Points[Y2].Position.Z, Delta);}
End;

Procedure Spline.Synchronize(OtherSpline:Spline);
Var
  I:Integer;
Begin
  If (OtherSpline._PointCount <> Self._PointCount) Then
  Begin
    RaiseError('Spline.Synchronize: Failed');
    Exit;
  End;

  OtherSpline.Build;

  For I:=0 To Pred(_PointCount) Do
    _Points[I].Time := OtherSpline._Points[I].Time;

  _NeedsBuild := False;
End;

Function Spline.IsReady:Boolean;
Begin
  Result := (_PointCount>0) And (Not _NeedsBuild);
End;

Procedure Spline.Build;
Var
  I:Integer;
  D:Single;
Begin
  _NeedsBuild := False;

  _TotalLength := 0.0;
  For I:=1 To Pred(_PointCount) Do
    _TotalLength := _TotalLength + _Points[I].Position.Distance(_Points[Pred(I)].Position);

  If (_Closed) Then
    _TotalLength := _TotalLength + _Points[Pred(_PointCount)].Position.Distance(_Points[0].Position);

  // build time frames based on distance
  If (_Geometric) Then
  Begin
    _Points[0].Time := 0.0;

    For I:=1 To Pred(_PointCount) Do
    Begin
      D := _Points[I].Position.Distance(_Points[Pred(I)].Position);
      D := D / _TotalLength;
      _Points[I].Time := _Points[Pred(I)].Time + D;
    End;
  End Else // constant time frames
  Begin
    For I:=0 To Pred(_PointCount) Do
      _Points[I].Time := I / Pred(_PointCount);
  End;

  _BoundingBox.Reset();
  For I:=0 To Pred(_PointCount) Do
    _BoundingBox.Add(_Points[I].Position);
  _NeedsBuild := False;
End;

Procedure Spline.Update;
Var
  Delta:Single;
Begin
  Delta := Self.GetTime;
  If (Delta>1.0) And (_Closed) Then
    _StartTime := Application.Instance.GetElapsedTime();

  If (_NeedsBuild) Then
    Build;
End;

Procedure Spline.Load(Source:Stream);
Var
  I:Integer;
Begin
  Source.Read(@_PointCount, 4);
  SetLength(_Points, _PointCount);
  For I:=0 To Pred(_PointCount) Do
    Source.Read(@_Points[I].Position, SizeOf(Vector3D));
  _NeedsBuild := True;
End;

Procedure Spline.Save(Dest:Stream);
Var
  I:Integer;
Begin
  Dest.Write(@_PointCount, 4);
  For I:=0 To Pred(_PointCount) Do
    Dest.Write(@_Points[I].Position, SizeOf(Vector3D));
End;

Procedure Spline.Save(FileName:TERRAString);
Var
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  Save(Dest);
  ReleaseObject(Dest);
End;

Procedure Spline.Load(Const FileName:TERRAString);
Var
  Source:Stream;
Begin
  Source := FileStream.Open(FileName);
  Load(Source);
  ReleaseObject(Source);
End;

Function Spline.GetLength: Single;
Begin
  If (_NeedsBuild) Then
    Build;

  Result := _TotalLength;
End;

Function Spline.GetTime: Single; {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result := Application.Instance.GetElapsedTime() - _StartTime;
  If (Result<=0) Then
    Result := 0.0
  Else
  Begin
    Result := Result / _Speed;
    If (Result>1) Then
      Result := 1;
  End;
End;

Procedure Spline.SetTime(Value: Single);
Begin
  If (Value>1) Then
    Value := 1
  Else
  If (Value<0) Then
    Value := 0;

  _StartTime := Trunc(Application.Instance.GetElapsedTime() - (Value * _Speed));
End;

Function Spline.GetPointByIndex(Index: Integer): Vector3D;
Begin
  If (Index<0) Or (Index>=_PointCount) Then
    Result := VectorZero
  Else
    Result := _Points[Index].Position;
End;

Procedure Spline.SetPoint(Index: Integer; P: Vector3D);
Begin
  Self._Points[Index].Position := P;
End;

Function Spline.GetTangent(Index:Integer):Vector3D;
Var
  PrevIndex:Integer;
  A,B, V:Vector3D;
Begin
  If (Index>0) Then
    PrevIndex := Index - 1
  Else
    PrevIndex := Pred(_PointCount);

  A := _Points[PrevIndex].Position;
  B := _Points[Index].Position;

  V := VectorSubtract(B, A);
  V.Normalize();

  //V := VectorCross(V, VectorUp);

  Result := V;
End;

Function Spline.GetOrientation(T: Single):Quaternion;
Var
  N:Integer;
  Delta:Single;
  Y0,Y1,Y2,Y3:Integer;
  T0,T1,T2,T3:Vector3D;
  V:Vector3D;
Begin
  GetParams(T, Delta, N, Y0,Y1,Y2,Y3);

  T0 := GetTangent(Y0);
  T1 := GetTangent(Y1);
  T2 := GetTangent(Y2);
  T3 := GetTangent(Y3);

  V.X := CatmullRomInterpolate(T0.X, T1.X, T2.X, T3.X, Delta);
  V.Y := CatmullRomInterpolate(T0.Y, T1.Y, T2.Y, T3.Y, Delta);
  V.Z := CatmullRomInterpolate(T0.Z, T1.Z, T2.Z, T3.Z, Delta);

  Result := QuaternionLookRotation(V, VectorUp);
End;


Procedure Spline.Release;
Begin
  SetLength(_Points, 0);
End;

End.

