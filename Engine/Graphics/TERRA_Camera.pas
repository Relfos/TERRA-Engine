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

  camDirForward   = 0;
  camDirBackward  = 1;
  camDirLeft      = 2;
  camDirRight     = 3;

Type

  { Camera }

  Camera = Class(TERRAObject)
    Protected
      _Name:TERRAString;

      _View:Vector3D;
      _Position:Vector3D;
      _Roll:Vector3D;
      _Speed:Single;

      _Transform:Matrix4x4;
      _ScreenMatrix4x4:Matrix4x4;

			_ProjectionMatrix4x4:Matrix4x4;
      _Ortho:Boolean;

      _OrthoX1:Single;
      _OrthoX2:Single;
      _OrthoY1:Single;
      _OrthoY2:Single;
      _OrthoScale:Single;

      _NeedsUpdate:Boolean;
      _LastOrientation:Single;

      _Width, _Height:Integer;

      _Near:Single;
      _Far:Single;
      _FOV:Single;

      _Ratio:Single;

      _CurrentEye:Integer;

      _ClipPlane:Plane;
      _UseClipPlane:Boolean;

      _FrustumCorners:Array[0..7] Of Vector3D;
			_Frustum:Frustum;

      _Up:Vector3D;
      _Right:Vector3D;

      _Focus:Vector3D;

      Procedure UpdateMatrix4x4(Eye:Integer);

      Function ConvertPlaneWorldToCameraSpace(Point, Normal:Vector3D):Plane;

    Public
      Constructor Create(Name:TERRAString);
      Procedure Release; Override;

      Procedure Update(Width, Height, Eye:Integer);

      Procedure Refresh();

      Procedure SetupUniforms;

      Procedure FreeCam;
      Procedure Rotate(rotX, rotY:Single);

      Procedure SetFocusPoint(Const P:Vector3D);

      Procedure SetView(NewView:Vector3D);
      Procedure SetPosition(NewPos:Vector3D);
      Procedure SetRoll(NewRoll:Vector3D);

      Procedure SetOrthoMode(Enabled:Boolean; X1,Y1,X2,Y2:Single);
      Procedure SetOrthoScale(Value:Single);

      Procedure SetNear(Value:Single);
      Procedure SetFar(Value:Single);
      Procedure SetFOV(Value:Single);

      Procedure SetRatio(Value:Single);

      Procedure LookAt(P:Vector3D);

      Procedure Move(Dir:Integer; Speed:Single);

      Procedure AdjustToFit(Box:BoundingBox);

      Procedure SetClipPlane(Point, Normal:Vector3D);
      Procedure RemoveClipPlane();

      Property Position:Vector3D Read _Position Write SetPosition;
      Property View:Vector3D Read _View  Write SetView;
      Property Roll:Vector3D Read _Roll Write _Roll;

      Property Ortho:Boolean Read _Ortho;
      Property OrthoScale:Single Read _OrthoScale Write SetOrthoScale;

      Property Transform:Matrix4x4 Read _Transform;
      Property Projection:Matrix4x4 Read _ProjectionMatrix4x4;

      Property ScreenMatrix4x4:Matrix4x4 Read _ScreenMatrix4x4;
      Property Speed:Single Read _Speed Write _Speed;

      Property Near:Single Read _Near Write SetNear;
      Property Far:Single Read _Far Write SetFar;
      Property FOV:Single  Read _FOV Write SetFOV;

      Property Frustum:TERRA_Frustum.Frustum Read _Frustum;

      Property Up:Vector3D Read _Up;
      Property Right:Vector3D Read _Right;

      Property Name:TERRAString Read _Name Write _Name;

      Property Ratio:Single Read _Ratio Write SetRatio;

      Property UseClipPlane:Boolean Read _UseClipPlane;

      Property FocusPoint:Vector3D Read _Focus Write SetFocusPoint;
  End;

Implementation
Uses TERRA_OS, TERRA_Application, TERRA_Lights, TERRA_GraphicsManager, TERRA_Renderer,  TERRA_InputManager, TERRA_Log, Math;

// Camera

constructor Camera.Create(Name: TERRAString);
Begin
  _Name := Name;
  _Roll := VectorUp;
  _FOV := 45.0;
  _LastOrientation := -1;
  _Position := VectorCreate(0.0, 0.0, 1.0);
  _View := VectorCreate(0.0, 0.0, -1.0);
  _Near := 1.0;
  _Far := 300.0;
  _Speed := 4.0;// * GL_WORLD_SCALE;

  _Ortho := False;
  _OrthoScale := 1.0;

  _NeedsUpdate := True;
End;

procedure Camera.SetPosition(NewPos: Vector3D);
Begin
  _Position:= NewPos;
  _NeedsUpdate := True;
End;

procedure Camera.SetView(NewView: Vector3D);
Begin
//  NewView := VectorCreate(0,-1,0);
  _View := NewView;
  _NeedsUpdate := True;
End;

procedure Camera.SetRoll(NewRoll: Vector3D);
Begin
  _Roll:= NewRoll;
  _NeedsUpdate := True;
End;

procedure Camera.UpdateMatrix4x4(Eye: Integer);
Const
  ZoomFactor = 1;
Var
  P:Vector3D;
  Zoom:Single;
  Proj:Matrix4x4;
Begin
  _NeedsUpdate := False;

{$IFDEF EMULATED_LANDSCAPE}
 // If (Application.Instance.IsLandscape()) And (_Target<>Nil) Then
    _Ratio := SafeDiv(_Height, _Width, 1.0) * SafeDiv(GraphicsManager.Instance.Height, GraphicsManager.Instance.Width, 1.0);
{$ELSE}
    _Ratio := SafeDiv(_Width, _Height, 1.0);
{$ENDIF}


  If (_Ortho) Then
    _ProjectionMatrix4x4 := Matrix4x4Ortho(Ratio*_OrthoX1*_OrthoScale, Ratio*_OrthoX2*_OrthoScale,
                                       _OrthoY1*_OrthoScale, _OrthoY2*_OrthoScale, _Near, _Far)
  Else
  {$IFDEF DISABLEVR}
    _ProjectionMatrix4x4 := Matrix4x4Perspective(FOV, Ratio, _Near, _Far);
  {$ELSE}
    _ProjectionMatrix4x4 := Application.Instance.GetVRProjectionMatrix(Eye, FOV, Ratio, _Near, _Far);
  {$ENDIF}

//Log(logDebug, 'Viewport', 'X:'+IntToString(Trunc(_X)) +' Y:'+IntToString(Trunc(_Y)));
//  Log(logDebug, 'Viewport', 'W:'+IntToString(Trunc(_Width)) +' W:'+IntToString(Trunc(_Height)));

  P := _Position;
{  If GraphicsManager.Instance.LandscapeOrientation Then
    _Up := VectorCreate(_Roll.Y, _Roll.X, _Roll.Z)
  Else}

  _Transform := Matrix4x4LookAt(P, VectorAdd(_Position, _View), _Roll);
  _ScreenMatrix4x4 := _Transform;

  _Up := _Roll;
{  If (Abs(_Up.Dot(_View))>=0.9) Then
  Begin
    _Up.Y := _Roll.Z;
    _Up.Z := _Roll.Y;
    _Right := VectorCross(_Up, _View);
    _Up := VectorCross(_View, _Right);
  End Else}
  Begin
    _Right := VectorCross(_Up, _View);
    _Up := VectorCross(_View, _Right);
  End;

  //_Right.Normalize;
  //_Up.Normalize;

  {If Self._UseClipPlane Then
    CalculateObliqueMatrix4x4ClipPlane(_ProjectionMatrix4x4, _Transform, _ClipPlane);}

	_Frustum.Update(_ProjectionMatrix4x4, Self._Transform);
End;

procedure Camera.SetOrthoScale(Value: Single);
Begin
  If (_OrthoScale = Value) Then
    Exit;

  _OrthoScale := Value;
  _NeedsUpdate := True;
End;

procedure Camera.SetOrthoMode(Enabled: Boolean; X1, Y1, X2, Y2: Single);
Begin
  If (Ortho = Enabled) Then
    Exit;

  _Ortho := Enabled;
  _NeedsUpdate := True;

  _OrthoX1 := X1;
  _OrthoX2 := X2;
  _OrthoY1 := Y1;
  _OrthoY2 := Y2;
End;

procedure Camera.Update(Width, Height, Eye: Integer);
Begin
  _Width := Width;
  _Height := Height;

  If (Eye <> _CurrentEye) Then
  Begin
    _CurrentEye := Eye;
    _NeedsUpdate := True;
  End;

  If (_NeedsUpdate) Then
    UpdateMatrix4x4(Eye);
End;

procedure Camera.Rotate(rotX, rotY: Single);
Var
  rot_axis:Vector3D;
Begin
  rotX := rotX * (Speed * GraphicsManager.Instance.ElapsedTime) * 0.5;
  rotY := rotY * (Speed * GraphicsManager.Instance.ElapsedTime) * 0.5;

	_view.Rotate(VectorUp, rotX);
	rot_axis := VectorCreate(-_view.z, 0.0, _view.x);
  rot_axis.Normalize;
	_view.Rotate(rot_axis, rotY);
  _view.Normalize;

  _NeedsUpdate := True;
End;

Procedure Camera.SetFocusPoint(const P: Vector3D);
Begin
  _Focus := P;
End;

procedure Camera.SetupUniforms;
Var
  _Shader:ShaderInterface;
  P:Vector3D;
  A,B,C, Delta:Single;
Begin
  _Shader := GraphicsManager.Instance.Renderer.ActiveShader;
  If (_Shader=Nil) Then
    Exit;

  _Shader.SetVec3Uniform('cameraPosition', _Position);
  _Shader.SetVec3Uniform('cameraView', _View);
  _Shader.SetMat4Uniform('cameraMatrix', _Transform);
  _Shader.SetMat4Uniform('projectionMatrix', _ProjectionMatrix4x4);
  _Shader.SetFloatUniform('zNear', _Near);
  _Shader.SetFloatUniform('zFar', _Far);

  If (_UseClipPlane) Then
    _Shader.SetPlaneUniform('clipPlane', _ClipPlane);

  If (GraphicsManager.Instance.Renderer.Settings.FogMode<>0) Then
  Begin
    _Shader.SetColorUniform('fogColor', GraphicsManager.Instance.Renderer.Settings.FogColor);

    If (GraphicsManager.Instance.Renderer.Settings.FogMode And fogDistance<>0) Then
    Begin
      A := GraphicsManager.Instance.Renderer.Settings.FogDistanceStart;
      B := GraphicsManager.Instance.Renderer.Settings.FogDistanceEnd;
      C := (B-A) * 0.5;
      _Shader.SetFloatUniform('fogDistanceCenter', C + A);
      _Shader.SetFloatUniform('fogDistanceSize', C);
    End;

    If (GraphicsManager.Instance.Renderer.Settings.FogMode And fogHeight<>0) Then
    Begin
      _Shader.SetFloatUniform('fogHeightStart', GraphicsManager.Instance.Renderer.Settings.FogHeightStart);
      _Shader.SetFloatUniform('fogHeightEnd', GraphicsManager.Instance.Renderer.Settings.FogHeightEnd);
    End;

    If (GraphicsManager.Instance.Renderer.Settings.FogMode And fogBox<>0) Then
    Begin
      _Shader.SetVec3Uniform('fogBoxAreaStart', GraphicsManager.Instance.Renderer.Settings.FogBoxArea.StartVertex);
      _Shader.SetVec3Uniform('fogBoxAreaEnd', GraphicsManager.Instance.Renderer.Settings.FogBoxArea.EndVertex);
      _Shader.SetFloatUniform('fogBoxSize', GraphicsManager.Instance.Renderer.Settings.FogBoxSize);
    End;

  End;
End;

procedure Camera.SetFOV(Value: Single);
Begin
  If (Value = _FOV) Then
    Exit;

  _FOV := Value;
  _NeedsUpdate := True;
End;

procedure Camera.SetFar(Value: Single);
Begin
  If (Value = _Far) Then
    Exit;

  _Far := Value;
  _NeedsUpdate := True;
End;

procedure Camera.SetNear(Value: Single);
Begin
  If (Value = _Near) Then
    Exit;

  _Near := Value;
  _NeedsUpdate := True;
End;

procedure Camera.AdjustToFit(Box: BoundingBox);
Var
  oneOverSine:Single;
  distanceToCenter:Single;
  offset, Center:Vector3D;
  p,N:Vector3D;
Begin
  Center := Box.Center;
  offset := VectorSubtract(Box.EndVertex, Center); // Radius of bounding sphere

  P := center;
  oneOverSine := 1.0 / Tan(_FOV *RAD / 4.0); // 1 / sin = adjacent / opposite
  distanceToCenter := Offset.Length * oneOverSine; // (adjacent / opposite) * opposite = adjacent

  N := VectorConstant(1);
  N.Normalize;
  P.Add( VectorScale(N, distanceToCenter));
  SetPosition(P);
  P.Subtract(Center);
  P.Normalize;
  P.Scale(-1);
  SetView(P);
End;

procedure Camera.LookAt(P: Vector3D);
Begin
  P := VectorSubtract(P, _Position);
  P.Normalize;
  SetView(P);
End;

procedure Camera.FreeCam;
Var
  Walk_speed:Single;
  Rot:Single;
  Input:InputManager;
Begin
  Walk_Speed := GraphicsManager.Instance.ElapsedTime * Speed * 3.0;

  Input := InputManager.Instance;

	If (Input.Keys.IsDown(keyShift)) Then
    Walk_speed := Walk_speed * 8;

	If (Input.Keys.IsDown(keyW)) Then
    Move(camDirForward, Walk_Speed);

	If (Input.Keys.IsDown(keyS)) Then
    Move(camDirBackward, Walk_Speed);

	If (Input.Keys.IsDown(keyA)) Then
    Move(camDirLeft, Walk_Speed);

	If (Input.Keys.IsDown(keyD)) Then
    Move(camDirRight, Walk_Speed);

	If (Input.Keys.IsDown(keyQ)) Then
		_position.y := _position.y -walk_speed;

	If (Input.Keys.IsDown(keyE)) Then
		_position.y := _position.y + walk_speed;

  {$IFDEF MOBILE}
  Rot := 0.125;
  {$ELSE}
  Rot := 0.5;
  {$ENDIF}

  If (Input.Keys.IsDown(keyLEFT)) Then
    Self.Rotate(Rot, 0.0);

  If (Input.Keys.IsDown(keyRight)) Then
    Self.Rotate(-Rot, 0.0);

  If (Input.Keys.IsDown(keyUp)) Then
    Self.Rotate(0.0, Rot);

  If (Input.Keys.IsDown(keyDown)) Then
    Self.Rotate(0.0, -Rot);

  _NeedsUpdate := True;
End;

procedure Camera.Move(Dir: Integer; Speed: Single);
Begin
  Case Dir Of
    camdirForward:   _position := VectorAdd(_position, VectorScale(_view, Speed));
    camdirBackward:  _position := VectorAdd(_position, VectorScale(_view, -Speed));
    camdirLeft:
      Begin
    		_position.x := _position.x + (_view.z * Speed);
    		_position.z := _position.z - (_view.x * Speed);
    	End;

    camdirRight:
    Begin
	  	_position.x := _position.x -( _view.z * Speed);
  		_position.z := _position.z + (_view.x * Speed);
  	End;
  End;

  _NeedsUpdate := True;
End;


function Camera.ConvertPlaneWorldToCameraSpace(Point, Normal: Vector3D): Plane;
Var
  A,B,C,N:Single;
  pX, pY, pZ, pW:Single; //transformed point
  nX, nY, nZ, nW, inverse_normal_length:Single; //transformed normal
Begin
  A := Point.X;
  B := Point.Y;
  C := Point.Z;

  Result := PlaneCreate(Point, Normal);
  N := (_Position.X * Result.A) + (_Position.Y * Result.B) + (_Position.Z * Result.C) + (1 * Result.D);

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

procedure Camera.SetClipPlane(Point, Normal: Vector3D);
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

procedure Camera.RemoveClipPlane;
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

procedure Camera.SetRatio(Value: Single);
Begin
  If (_Ratio = Value) Then
    Exit;

  _Ratio := Value;
  _NeedsUpdate := True;
End;

procedure Camera.Refresh;
Begin
  _NeedsUpdate := True;
End;

procedure Camera.Release;
Begin
  // do nothing
End;

End.

