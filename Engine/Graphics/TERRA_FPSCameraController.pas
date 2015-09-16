Unit TERRA_FPSCameraController;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Camera, TERRA_Vector2D, TERRA_Vector3D, TERRA_BoundingBox, TERRA_Math;

Const
  MouseSpeed = 0.005;

  Max_Tilt = 60*RAD;
  Min_Tilt = -Max_Tilt;

Type
  FPSCameraController = Class(TERRACameraController)
    Protected
      _Heading:Single;
      _Tilt:Single;

    Public
      Constructor Create();
      Procedure Release(); Override;
      Procedure Update(); Override;
  End;

Implementation
Uses TERRA_InputManager, TERRA_Engine, TERRA_OS;


{ FPSCameraController }
Constructor FPSCameraController.Create();
Begin
  _Heading := -45.0 * RAD;
  _Tilt := -30.0 * RAD;

  Application.Instance.Window.LockedCursor := True;
End;

Procedure FPSCameraController.Release;
Begin
  Application.Instance.Window.LockedCursor := False;
End;

Procedure FPSCameraController.Update;
Var
  Walk_Speed:Single;
  Strafe_Speed:Single;
  Rot:Single;
  Input:InputManager;
  TargetCam:PerspectiveCamera;
  TargetView:Vector3D;
Begin
  If _Target = Nil Then
    Exit;

  TargetCam := Nil;
  If (_Target Is PerspectiveCamera) Then
    TargetCam := PerspectiveCamera(_Target)
  Else
    Exit;


  Input := Engine.Input;

  Walk_Speed := 20.0;
	If (Input.Keys.IsDown(keyShift)) Then
    Walk_speed := Walk_speed * 8;

  // strafing is 50% slower
  Strafe_Speed := Walk_Speed * 0.5;

	If (Input.Keys.IsDown(keyW)) Then
    TargetCam.Move(CameraMove_Forward, Walk_Speed);

	If (Input.Keys.IsDown(keyS)) Then
    TargetCam.Move(CameraMove_Backward, Walk_Speed);

	If (Input.Keys.IsDown(keyA)) Then
    TargetCam.Move(CameraMove_Left, Strafe_Speed);

	If (Input.Keys.IsDown(keyD)) Then
    TargetCam.Move(CameraMove_Right, Strafe_Speed);

	If (Input.Keys.IsDown(keyQ)) Then
    TargetCam.Move(CameraMove_Down, Walk_Speed);

	If (Input.Keys.IsDown(keyE)) Then
    TargetCam.Move(CameraMove_Up, Walk_Speed);

  If (Application.Instance.Window.LockedCursor) Then
  Begin
    _Heading := _Heading + Input.Mouse.X * MouseSpeed;
    _Tilt := _Tilt + Input.Mouse.Y * MouseSpeed;

    If _Tilt > Max_Tilt Then _Tilt := Max_Tilt;
    If _Tilt < Min_Tilt Then _Tilt := Min_Tilt;

    TargetView := Vector3D_Create(Cos(_Heading), Sin(_Tilt), -Sin(_Heading));
    TargetCam.View := TargetView;
  End;

  {$IFDEF MOBILE}
  Rot := 0.125;
  {$ELSE}
  Rot := 0.5;
  {$ENDIF}

(*  If (Input.Keys.IsDown(keyLEFT)) Then
    TargetCam.Rotate(Rot, 0.0);

  If (Input.Keys.IsDown(keyRight)) Then
    TargetCam.Rotate(-Rot, 0.0);

  If (Input.Keys.IsDown(keyUp)) Then
    TargetCam.Rotate(0.0, Rot);

  If (Input.Keys.IsDown(keyDown)) Then
    TargetCam.Rotate(0.0, -Rot);*)

  If Input.Keys.IsDown(keySpace) Then
    TargetCam.Shake(500);
End;

End.
