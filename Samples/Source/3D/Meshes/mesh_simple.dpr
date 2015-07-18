{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_DemoApplication, TERRA_Utils, TERRA_Object, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_UI, TERRA_Lights, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_Mesh,
  TERRA_FileManager, TERRA_Color,
  TERRA_ScreenFX, TERRA_InputManager;

Type
  MyDemo = Class(DemoApplication)
    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;

      Procedure OnRender(V:TERRAViewport); Override;
  End;


Var
  CarInstance:MeshInstance;
  DwarfInstance:MeshInstance;

{ MyDemo }
Procedure MyDemo.OnCreate;
Var
  MyMesh:TERRAMesh;
Begin
  Inherited;
  
  MyMesh := MeshManager.Instance.GetMesh('jeep');
  If Assigned(MyMesh) Then
  Begin
    CarInstance :=MeshInstance.Create(MyMesh);
  End Else
    CarInstance := Nil;

  MyMesh := MeshManager.Instance.GetMesh('dwarf');
  If Assigned(MyMesh) Then
  Begin
    DwarfInstance :=MeshInstance.Create(MyMesh);
    DwarfInstance.SetPosition(VectorCreate(10, 0, 0));
    DwarfInstance.Animation.Play('walk');
  End Else
    DwarfInstance := Nil;
End;

Procedure MyDemo.OnDestroy;
Begin
  Inherited;
  ReleaseObject(DwarfInstance);
  ReleaseObject(CarInstance);
End;

Procedure MyDemo.OnRender(V:TERRAViewport);
Begin
  GraphicsManager.Instance.AddRenderable(V, DwarfInstance);
  GraphicsManager.Instance.AddRenderable(V, CarInstance);
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

