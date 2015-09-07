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
 * TERRA_Camera
 * Implements a generic camera class
 ***********************************************************************************************************************
}
Unit TERRA_Camera;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Frustum, TERRA_BoundingBox, TERRA_Vector3D, TERRA_Matrix4x4, TERRA_Math, TERRA_Plane;

Const
  MoveSpeed = 0.05;
  RotSpeed  = 2;
  ZoomSpeed = 1;

  MinPitch  = -90;
  MaxPitch  = 90;

  MinZoom = 60;
  MaxZoom = 120;

Type
  TERRACameraMovement = (
    CameraMove_Forward,
    CameraMove_Backward,
    CameraMove_Left,
    CameraMove_Right,
    CameraMove_Up,
    CameraMove_Down
  );

  TERRACamera = Class;

  TERRACameraController = Class(TERRAObject)
    Protected
      _Target:TERRACamera;

    Public
      Procedure Update(); Virtual; Abstract;

      Property Target:TERRACamera Read _Target;
  End;

  { TERRACamera }
  TERRACamera = Class(TERRAObject)
    Protected
      _Position:Vector3DProperty;
      _Offset:Vector3D;

      _Transform:Matrix4x4;

			_ProjectionMatrix4x4:Matrix4x4;

      _NeedsUpdate:Boolean;
      _LastOrientation:Single;

      _Width, _Height:Integer;

      _Near:Single;
      _Far:Single;
      _FOV:Single;

      _CurrentEye:Integer;

      _ClipPlane:Plane;
      _UseClipPlane:Boolean;

      _FrustumCorners:Array[0..7] Of Vector3D;
			_Frustum:Frustum;

      _Up:Vector3D;
      _Right:Vector3D;

      _Focus:Vector3D;

      _Controller:TERRACameraController;

      _Shaking:Boolean;
      _ShakeTime:Cardinal;

      Procedure UpdateMatrix4x4(Eye:Integer);

      Procedure CalculateProjection(Const Eye:Integer; Out Result:Matrix4x4); Virtual; Abstract;
      Procedure CalculateTransform(Out Result:Matrix4x4); Virtual; Abstract;

      Function ConvertPlaneWorldToCameraSpace(Point, Normal:Vector3D):Plane;

      Procedure SetController(const Value: TERRACameraController);

      Function GetPosition: Vector3D;

    Public
      Constructor Create(Const Name:TERRAString);
      Procedure Release; Override;

      Procedure Update(Width, Height, Eye:Integer);

      Procedure Refresh();

      Procedure SetupUniforms; Virtual; Abstract;

      Procedure SetFocusPoint(Const P:Vector3D);

      Procedure SetPosition(Const Value:Vector3D);

      Procedure SetNear(Value:Single);
      Procedure SetFar(Value:Single);
      Procedure SetFOV(Value:Single);

      Procedure Move(Const Dir:TERRACameraMovement; Speed:Single); Virtual; Abstract;

      Procedure Shake(Const Duration:Cardinal);

      Procedure SetClipPlane(Point, Normal:Vector3D);
      Procedure RemoveClipPlane();

      Property Position:Vector3D Read GetPosition Write SetPosition;

      Property Transform:Matrix4x4 Read _Transform;
      Property Projection:Matrix4x4 Read _ProjectionMatrix4x4;

      Property NearDistance:Single Read _Near Write SetNear;
      Property FarDistance:Single Read _Far Write SetFar;
      Property FOV:Single  Read _FOV Write SetFOV;

      Property Frustum:TERRA_Frustum.Frustum Read _Frustum;

      Property Up:Vector3D Read _Up;
      Property Right:Vector3D Read _Right;

      Property UseClipPlane:Boolean Read _UseClipPlane;

      Property Controller:TERRACameraController Read _Controller Write SetController;

      Property FocusPoint:Vector3D Read _Focus Write SetFocusPoint;
  End;

  PerspectiveCamera = Class(TERRACamera)
    Protected
      _View:Vector3DProperty;
      _Roll:Vector3DProperty;

      _Ratio:Single;

      Procedure CalculateProjection(Const Eye:Integer; Out Result:Matrix4x4); Override;
      Procedure CalculateTransform(Out Result:Matrix4x4); Override;

      Function GetView: Vector3D;
      Function GetRoll: Vector3D;

    Public
      Constructor Create(Const Name:TERRAString);
      Procedure Move(Const Dir:TERRACameraMovement; Speed:Single); Override;

      Procedure AdjustToFit(Const Box:BoundingBox);

      Procedure SetView(Const Value:Vector3D);
      Procedure SetRoll(Const Value:Vector3D);

      Procedure LookAt(Const Target:Vector3D);
      Procedure Rotate(RotX, RotY:Single);

      Procedure SetupUniforms; Override;

      Property View:Vector3D Read GetView Write SetView;
      Property Roll:Vector3D Read GetRoll Write SetRoll;

      Property Ratio:Single Read _Ratio;
  End;

  OrthoCamera = Class(TERRACamera)
    Protected
      _OrthoX1:Single;
      _OrthoX2:Single;
      _OrthoY1:Single;
      _OrthoY2:Single;
      _OrthoScale:Single;

      Procedure CalculateProjection(Const Eye:Integer; Out Result:Matrix4x4); Override;
      Procedure CalculateTransform(Out Result:Matrix4x4); Override;

    Public
      Constructor Create(Const Name:TERRAString);

      Procedure Move(Const Dir:TERRACameraMovement; Speed:Single); Override;

      Procedure SetArea(X1,Y1,X2,Y2:Single);
      Procedure SetScale(Value:Single);
  End;

Implementation
Uses TERRA_OS, TERRA_Application, TERRA_Lights, TERRA_GraphicsManager, TERRA_Renderer,  TERRA_InputManager, TERRA_Log,
  TERRA_Engine, TERRA_Renderable;

{ TERRACamera}
Constructor TERRACamera.Create(Const Name:TERRAString);
Begin
  Self._ObjectName := Name;
  _FOV := 45.0;
  _LastOrientation := -1;

  _Near := 1.0;
  _Far := 300.0;

  _Position := Vector3DProperty.Create('position', Vector3D_Create(0.0, 0.0, 1.0));
  _Offset := Vector3D_Zero;

  _NeedsUpdate := True;
End;

Procedure TERRACamera.Release();
Begin
  ReleaseObject(_Position);
End;

Procedure TERRACamera.SetPosition(Const Value:Vector3D);
Begin
  If (_Position.Value.X = Value.X) And (_Position.Value.Y = Value.Y) And (_Position.Value.Z = Value.Z) Then
    Exit;

  _Position.Value := Value;
  _NeedsUpdate := True;
End;

procedure TERRACamera.UpdateMatrix4x4(Eye: Integer);
Begin
  _NeedsUpdate := False;

  CalculateProjection(Eye, _ProjectionMatrix4x4);
  CalculateTransform(_Transform);

  {If Self._UseClipPlane Then
    CalculateObliqueMatrix4x4ClipPlane(_ProjectionMatrix4x4, _Transform, _ClipPlane);}

	_Frustum.Update(_ProjectionMatrix4x4, Self._Transform);
End;

Procedure TERRACamera.Update(Width, Height, Eye: Integer);
Begin
  _Width := Width;
  _Height := Height;

  If (_Shaking) Then
  Begin
    _Offset := Vector3D_Create(RandomFloat(-1, 1), RandomFloat(-1, 1), RandomFloat(-1, 1));
    _NeedsUpdate := True;

    If (Application.GetTime() > _ShakeTime) Then
      _Shaking := False;
  End;

  If Assigned(_Controller) Then
    _Controller.Update();

  If (Eye <> _CurrentEye) Then
  Begin
    _CurrentEye := Eye;
    _NeedsUpdate := True;
  End;

  If (_NeedsUpdate) Then
    UpdateMatrix4x4(Eye);
End;

Procedure TERRACamera.SetFocusPoint(const P: Vector3D);
Begin
  _Focus := P;
End;


procedure TERRACamera.SetFOV(Value: Single);
Begin
  If (Value = _FOV) Then
    Exit;

  _FOV := Value;
  _NeedsUpdate := True;
End;

procedure TERRACamera.SetFar(Value: Single);
Begin
  If (Value = _Far) Then
    Exit;

  _Far := Value;
  _NeedsUpdate := True;
End;

procedure TERRACamera.SetNear(Value: Single);
Begin
  If (Value = _Near) Then
    Exit;

  _Near := Value;
  _NeedsUpdate := True;
End;

function TERRACamera.ConvertPlaneWorldToCameraSpace(Point, Normal: Vector3D): Plane;
Var
  A,B,C,N:Single;
  pX, pY, pZ, pW:Single; //transformed point
  nX, nY, nZ, nW, inverse_normal_length:Single; //transformed normal
  Pos:Vector3D;
Begin
  A := Point.X;
  B := Point.Y;
  C := Point.Z;

  Pos := Self.Position;

  Result := PlaneCreate(Point, Normal);
  N := (Pos.X * Result.A) + (Pos.Y * Result.B) + (Pos.Z * Result.C) + (1 * Result.D);

  pX := A*_Transform.V[0] +  B*_Transform.V[4] +  C*_Transform.V[8] +  _Transform.V[12];
  pY := A*_Transform.V[1] +  B*_Transform.V[5] +  C*_Transform.V[9] +  _Transform.V[13];
  pZ := A*_Transform.V[2] +  B*_Transform.V[6] +  C*_Transform.V[10] + _Transform.V[14];
  pW := 1;

  //transforming normal
  A := Normal.X;
  B := Normal.Y;
  C := Normal.Z;
  nX := A*_Transform.V[0] + B*_Transform.V[4] + C*_Transform.V[8];
 	nY := A*_Transform.V[1] + B*_Transform.V[5] + C*_Transform.V[9];
  nZ := A*_Transform.V[2] + B*_Transform.V[6] + C*_Transform.V[10];
 	nW := 0;

  //normalize
  inverse_normal_length := 1.0 / sqrt( nX*nX + nY*nY + nZ*nZ );

  nX := nX * inverse_normal_length;
  nY := nY * inverse_normal_length;
  nZ := nZ * inverse_normal_length;

  //clip plane values
  Result.A := nX;
  Result.B := nY;
  Result.C := nZ;
  //dot between normal and point
  Result.D := -(pX*nX + pY*nY + pZ*nZ);

  If (N>0) Then
  Begin
    Result.A := -Result.A;
    Result.B := -Result.B;
    Result.C := -Result.C;
    Result.D := -Result.D;
  End;
End;

procedure TERRACamera.SetClipPlane(Point, Normal: Vector3D);
{$IFDEF PC}
Var
  Clip:Array[0..4] Of Double;
{$ENDIF}
Begin
  {$IFDEF MOBILE}
  Exit;
  {$ENDIF}
  _UseClipPlane := True;
  //_ClipPlane := ConvertPlaneWorldToCameraSpace(Point, Normal);
  _ClipPlane := PlaneCreate(Point, Normal);

(*  If Not GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable Then
  Begin
    {$IFDEF PC}
    Clip[0] := _ClipPlane.A;
    Clip[1] := _ClipPlane.B;
    Clip[2] := _ClipPlane.C;
    Clip[3] := _ClipPlane.D;
    glClipPlane(GL_CLIP_PLANE0, @Clip);
    glEnable(GL_CLIP_PLANE0);
    {$ENDIF}
    Exit;
  End; BIBI
  *)

  UpdateMatrix4x4(0);
End;

procedure TERRACamera.RemoveClipPlane;
Begin
  If Not _UseClipPlane Then
    Exit;

  _UseClipPlane := False;

(*  If Not GraphicsManager.Instance.Settings.Shaders.Avaliable Then
  Begin
    {$IFDEF PC}
    glDisable(GL_CLIP_PLANE0);
    {$ENDIF}
    Exit;
  End; BIBI
  *)

  UpdateMatrix4x4(0);
End;

procedure TERRACamera.Refresh;
Begin
  _NeedsUpdate := True;
End;

Function TERRACamera.GetPosition: Vector3D;
Begin
  Result := _Position.Value;
End;

Procedure TERRACamera.SetController(const Value: TERRACameraController);
Begin
  If (Value = _Controller) Then
    Exit;

  ReleaseObject(_Controller);

  _Controller := Value;

  If Assigned(_Controller) Then
  Begin
    If (Assigned(_Controller.Target)) And (_Controller.Target<> Self) Then
      _Controller.Target.SetController(Nil);

    _Controller._Target := Self;
  End;
End;

Procedure TERRACamera.Shake(Const Duration:Cardinal);
Begin
  _ShakeTime := Application.GetTime() + Duration;
  _Shaking := True;
  _NeedsUpdate := True;
End;

{ PerspectiveCamera }
Constructor PerspectiveCamera.Create(const Name: TERRAString);
Begin
  Inherited Create(Name);

  _View := Vector3DProperty.Create('view', Vector3D_Create(0.0, 0.0, -1.0));
  _Roll := Vector3DProperty.Create('roll', Vector3D_Up);
End;

Procedure PerspectiveCamera.SetView(Const Value:Vector3D);
Begin
  If (_View.Value.X = Value.X) And (_View.Value.Y = Value.Y) And (_View.Value.Z = Value.Z) Then
    Exit;

  _View.Value := Value;
  _NeedsUpdate := True;
End;

Procedure PerspectiveCamera.SetRoll(Const Value:Vector3D);
Begin
  If (_Roll.Value.X = Value.X) And (_Roll.Value.Y = Value.Y) And (_Roll.Value.Z = Value.Z) Then
    Exit;

  _Roll.Value := Value;
  _NeedsUpdate := True;
End;


Procedure PerspectiveCamera.CalculateProjection(Const Eye:Integer; Out Result:Matrix4x4);
Begin
{$IFDEF EMULATED_LANDSCAPE}
 // If (Application.Instance.IsLandscape()) And (_Target<>Nil) Then
    _Ratio := SafeDiv(_Height, _Width, 1.0) * SafeDiv(GraphicsManager.Instance.Height, GraphicsManager.Instance.Width, 1.0);
{$ELSE}
    _Ratio := SafeDiv(_Width, _Height, 1.0);
{$ENDIF}

  {$IFDEF DISABLEVR}
  Result := Matrix4x4_Perspective(FOV, Ratio, _Near, _Far);
  {$ELSE}
  Result := Application.Instance.GetVRProjectionMatrix(Eye, FOV, Ratio, _Near, _Far);
  {$ENDIF}
End;

Procedure PerspectiveCamera.CalculateTransform(out Result: Matrix4x4);
Var
  P, V, R:Vector3D;
Begin
  P := Self.Position;
  V := Self.View;
  R := Self.Roll;

  P.Add(Vector3D_Scale(_Offset, 0.25));

  _Transform := Matrix4x4_LookAt(P, Vector3D_Add(P, V), R);

  _Up := R;
  _Right := Vector3D_Cross(_Up, V);
  _Up := Vector3D_Cross(V, _Right);

  //_Right.Normalize;
  //_Up.Normalize;
End;

Procedure PerspectiveCamera.Move(Const Dir:TERRACameraMovement; Speed: Single);
Begin
  Speed := Speed * Engine.Graphics.ElapsedTime;

  Case Dir Of
    CameraMove_Forward:   SetPosition(Vector3D_Add(Self.Position, Vector3D_Scale(Self.View, Speed)));
    CameraMove_Backward:  SetPosition(Vector3D_Add(Self.Position, Vector3D_Scale(Self.View, -Speed)));

    CameraMove_Up:    SetPosition(Vector3D_Add(Self.Position, Vector3D_Scale(Self.Up, Speed)));
    CameraMove_Down:  SetPosition(Vector3D_Add(Self.Position, Vector3D_Scale(Self.Up, -Speed)));

    CameraMove_Left:  SetPosition(Vector3D_Add(Self.Position, Vector3D_Scale(Self.Right, Speed)));
    CameraMove_Right: SetPosition(Vector3D_Add(Self.Position, Vector3D_Scale(Self.Right, -Speed)));
  End;
End;

Procedure PerspectiveCamera.AdjustToFit(Const Box:BoundingBox);
Var
  oneOverSine:Single;
  distanceToCenter:Single;
  offset, Center:Vector3D;
  p,N:Vector3D;
Begin
  Center := Box.Center;
  offset := Vector3D_Subtract(Box.EndVertex, Center); // Radius of bounding sphere

  P := center;
  oneOverSine := 1.0 / Tan(_FOV *RAD / 4.0); // 1 / sin = adjacent / opposite
  distanceToCenter := Offset.Length * oneOverSine; // (adjacent / opposite) * opposite = adjacent

  N := Vector3D_Constant(1.0);
  N.Normalize;
  P.Add( Vector3D_Scale(N, distanceToCenter));
  SetPosition(P);
  P.Subtract(Center);
  P.Normalize;
  P.Scale(-1);
  SetView(P);
End;

Procedure PerspectiveCamera.LookAt(Const Target:Vector3D);
Var
  P:Vector3D;
Begin
  P := Vector3D_Subtract(Target, Self.Position);
  P.Normalize;
  SetView(P);
End;

Procedure PerspectiveCamera.Rotate(RotX, RotY: Single);
Var
  V, Axis:Vector3D;
Begin
  rotX := rotX * Engine.Graphics.ElapsedTime * 2.0;
  rotY := rotY * Engine.Graphics.ElapsedTime * 2.0;

	V := _view.Value;
  V.Rotate(Vector3D_Up, rotX);
	Axis := Vector3D_Create(-V.Z, 0.0, V.X);
  Axis.Normalize();
	V.Rotate(Axis, rotY);
  V.Normalize();
  _View.Value := V;

  _NeedsUpdate := True;
End;


Function PerspectiveCamera.GetView: Vector3D;
Begin
  Result := _View.Value;
End;

Function PerspectiveCamera.GetRoll: Vector3D;
Begin
  Result := _Roll.Value;
End;

Procedure PerspectiveCamera.SetupUniforms;
Var
  _Shader:ShaderInterface;
  P:Vector3D;
  A,B,C, Delta:Single;
Begin
  _Shader := Engine.Graphics.Renderer.ActiveShader;
  If (_Shader=Nil) Then
    Exit;

  _Shader.SetVec3Uniform('cameraPosition', _Position.Value);
  _Shader.SetVec3Uniform('cameraView', _View.Value);
  _Shader.SetMat4Uniform('cameraMatrix', _Transform);
  _Shader.SetMat4Uniform('projectionMatrix', _ProjectionMatrix4x4);
  _Shader.SetFloatUniform('zNear', _Near);
  _Shader.SetFloatUniform('zFar', _Far);

  If (_UseClipPlane) Then
    _Shader.SetPlaneUniform('clipPlane', _ClipPlane);

  If (Engine.Graphics.Renderer.Settings.FogMode<>0) Then
  Begin
    _Shader.SetColorUniform('fogColor', Engine.Graphics.Renderer.Settings.FogColor);

    If (Engine.Graphics.Renderer.Settings.FogMode And fogDistance<>0) Then
    Begin
      A := Engine.Graphics.Renderer.Settings.FogDistanceStart;
      B := Engine.Graphics.Renderer.Settings.FogDistanceEnd;
      C := (B-A) * 0.5;
      _Shader.SetFloatUniform('fogDistanceCenter', C + A);
      _Shader.SetFloatUniform('fogDistanceSize', C);
    End;

    If (Engine.Graphics.Renderer.Settings.FogMode And fogHeight<>0) Then
    Begin
      _Shader.SetFloatUniform('fogHeightStart', Engine.Graphics.Renderer.Settings.FogHeightStart);
      _Shader.SetFloatUniform('fogHeightEnd', Engine.Graphics.Renderer.Settings.FogHeightEnd);
    End;

    If (Engine.Graphics.Renderer.Settings.FogMode And fogBox<>0) Then
    Begin
      _Shader.SetVec3Uniform('fogBoxAreaStart', Engine.Graphics.Renderer.Settings.FogBoxArea.StartVertex);
      _Shader.SetVec3Uniform('fogBoxAreaEnd', Engine.Graphics.Renderer.Settings.FogBoxArea.EndVertex);
      _Shader.SetFloatUniform('fogBoxSize', Engine.Graphics.Renderer.Settings.FogBoxSize);
    End;

  End;
End;

{ OrthoCamera }
Constructor OrthoCamera.Create(Const Name:TERRAString);
Begin
  Inherited Create(Name);

  _OrthoScale := 1.0;
  _Near := 0.0;
  _Far := 100;

  Self.SetArea(-100, -100, 100, 100);
End;

Procedure OrthoCamera.CalculateProjection(Const Eye:Integer; Out Result:Matrix4x4);
Begin
  Result := Matrix4x4_Ortho(_OrthoX1 * _OrthoScale, _OrthoX2 *_OrthoScale, _OrthoY2 * _OrthoScale, _OrthoY1 *_OrthoScale, _Near, _Far);
  Result := Matrix4x4_Multiply4x4(Result, Matrix4x4_Translation(0.375, 0.375, 0.0)); // apply "pixel-perfect" correction
End;

Procedure OrthoCamera.CalculateTransform(out Result: Matrix4x4);
Var
  P:Vector3D;
Begin
  P := Self.Position;
  P.Add(Vector3D_Scale(_Offset, 5.0));
  P.Z := 0.0;
  
  Result := Matrix4x4_Translation(P);
End;

Procedure OrthoCamera.SetArea(X1, Y1, X2, Y2: Single);
Begin
  _NeedsUpdate := True;

  _OrthoX1 := X1;
  _OrthoY1 := Y1;

  _OrthoX2 := X2;
  _OrthoY2 := Y2;
End;

Procedure OrthoCamera.SetScale(Value: Single);
Begin
  If (_OrthoScale = Value) Then
    Exit;

  _OrthoScale := Value;
  _NeedsUpdate := True;
End;

Procedure OrthoCamera.Move(const Dir: TERRACameraMovement; Speed: Single);
Begin
  Speed := Speed * Engine.Graphics.ElapsedTime;

  Case Dir Of
    CameraMove_Left:  SetPosition(Vector3D_Create(Self.Position.X + Speed, Self.Position.Y, Self.Position.Z));
    CameraMove_Right: SetPosition(Vector3D_Create(Self.Position.X - Speed, Self.Position.Y, Self.Position.Z));

    CameraMove_Up:    SetPosition(Vector3D_Create(Self.Position.X, Self.Position.Y + Speed, Self.Position.Z));
    CameraMove_Down:  SetPosition(Vector3D_Create(Self.Position.X, Position.Y - Speed, Self.Position.Z));
    Else
      Exit;
  End;
End;

End.

