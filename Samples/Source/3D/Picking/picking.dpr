{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  TERRA_MemoryManager,
  TERRA_DemoApplication,
  TERRA_OS,
  TERRA_Object,
  TERRA_Utils,
  TERRA_Viewport,
  TERRA_Vector3D,
  TERRA_Texture,
  TERRA_ScreenFX,
  TERRA_Mesh,
  TERRA_Engine,
  TERRA_Math,
  TERRA_Ray,
  TERRA_Color,
  TERRA_Camera,
  TERRA_InputManager;

Type
  MyDemo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;

      Procedure OnMouseMove(Const X,Y:Single); Override;

      Procedure OnRender3D(V:TERRAViewport); Override;
  End;

Const
  SolidCount = 6;

Var
  Solids:Array[1..SolidCount] Of MeshInstance;

  DiffuseTex:TERRATexture;

{ Game }
Procedure MyDemo.OnCreate;
Var
  I:Integer;
  Mesh:TERRAMesh;
  Angle:Single;
Begin
  Inherited;

  Self.ShowFPS := True;

  Self.GUI.Viewport.Visible := True;
  Self.MainViewport.Visible := True;

  Self.MainViewport.Camera.Controller := Nil;

  Self.MainViewport.Camera.SetPosition(Vector3D_Create(0, 8, -8));
  PerspectiveCamera(Self.MainViewport.Camera).SetView(Vector3D_Create(0, -0.5, 0.5));

  DiffuseTex := Engine.Textures.GetItem('cobble');

  For I:=1 To SolidCount Do
  Begin
    Angle := ((I-1) / SolidCount) * 360 * RAD;

    Case I Mod 3 Of
      0: Mesh := Engine.Meshes.CubeMesh;
      1: Mesh := Engine.Meshes.CylinderMesh;
      2: Mesh := Engine.Meshes.SphereMesh;
    End;

    Solids[I] := MeshInstance.Create(Mesh);
    Solids[I].SetDiffuseMap(0, DiffuseTex);
    Solids[I].SetPosition(Vector3D_Create(Cos(Angle) * 4,  1, Sin(Angle) * 4));
    Solids[I].SetScale(Vector3D_Constant(2.0));
  End;

  Self.Floor.SetPosition(Vector3D_Zero);
End;

Procedure MyDemo.OnDestroy;
Var
  I:Integer;
Begin
  For I:=1 To SolidCount Do
    ReleaseObject(Solids[I]);

  Inherited;
End;

Procedure MyDemo.OnMouseMove(Const X, Y: Single);
Var
  R:TERRARay;
  I:Integer;
  T:Single;
Begin
  inherited;

  R := Self.MainViewport.GetPickRay(X, Y);

  T := 9999;
  For I:=1 To SolidCount Do
  If (Solids[I].Intersect(R, T)) Then
    Solids[I].SetDiffuseColor(0, ColorRed)
  Else
    Solids[I].SetDiffuseColor(0, ColorWhite);
End;

Procedure MyDemo.OnRender3D(V: TERRAViewport);
Var
  I:Integer;
Begin
  For I:=1 To SolidCount Do
    Engine.Graphics.AddRenderable(V, Solids[I]);

  Inherited;
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}
End.



