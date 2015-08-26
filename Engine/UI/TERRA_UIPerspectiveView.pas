Unit TERRA_UIPerspectiveView;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_UIView, TERRA_Renderer, TERRA_Renderable, TERRA_Resource, TERRA_Mesh, TERRA_BoundingBox,
  TERRA_InputManager, TERRA_EngineManager, TERRA_Viewport, TERRA_Matrix4x4;

Type
  UIPerspectiveView = Class(MeshInstance)
    Protected
      _Target:UIView;
      _Mesh:TERRAMesh;
      _Width:Single;
      _Height:Single;

    Public
      Constructor Create(Const Name:TERRAString; View:UIView);
      Procedure Release(); Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage; Const Bucket:Cardinal); Override;

      Property Target:UIView Read _Target;
  End;

Implementation
Uses TERRA_OS;

{ UIPerspective }
Constructor UIPerspectiveView.Create(Const Name:TERRAString; View:UIView);
Var
  ScaleMat:Matrix4x4;
Begin
  Self._ObjectName := Name;
  _Target := View;

  _Mesh := TERRAMesh.Create(rtDynamic);
  _Mesh.Clone(Engine.Meshes.PlaneMesh);

  ScaleMat := Matrix4x4Scale(960/640, 1, 1);
  _Mesh.Transform(ScaleMat);

  Inherited Create(_Mesh);
End;

Procedure UIPerspectiveView.Release;
Begin
  inherited;

  ReleaseObject(_Mesh);
End;

Procedure UIPerspectiveView.Render(View: TERRAViewport; Const Stage:RendererStage; const Bucket:Cardinal);
Begin
  Self.SetDiffuseMap(0, Self.Target.Viewport.ResolveTexture);

(*  If Engine.Input.Keys.WasPressed(keyK) Then
    Self.Target.Viewport.ResolveTexture.Save('frake.png');*)

  Inherited Render(View, Stage, Bucket);
End;

End.
