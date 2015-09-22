Unit TERRA_UIPerspectiveView;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_UIView, TERRA_Renderer, TERRA_Renderable, TERRA_Resource, TERRA_Mesh, TERRA_BoundingBox,
  TERRA_InputManager, TERRA_Engine, TERRA_Viewport, TERRA_Matrix4x4, TERRA_Ray;

Type
  UIPerspectiveView = Class(MeshInstance)
    Protected
      _Target:UIView;
      _Mesh:TERRAMesh;
      _Width:Single;
      _Height:Single;

      Procedure RenderMenuToTexture(V: TERRAViewport);

      Procedure UpdateMouseInput(View: TERRAViewport);


    Public
      Constructor Create(Const Name:TERRAString; View:UIView);
      Procedure Release(); Override;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage); Override;

      Property Target:UIView Read _Target;
  End;

Implementation
Uses TERRA_OS;

{ UIPerspective }
Constructor UIPerspectiveView.Create(Const Name:TERRAString; View:UIView);
Var
  ScaleMat:Matrix4x4;
Begin
  View.Viewport.AutoResolve := True;
  View.Viewport.OnRender := RenderMenuToTexture;

  Self._ObjectName := Name;
  _Target := View;

  _Mesh := TERRAMesh.Create(rtDynamic);
  _Mesh.Clone(Engine.Meshes.PlaneMesh);

  ScaleMat := Matrix4x4_Scale(960/640, 1, 1);
  _Mesh.Transform(ScaleMat);

  Inherited Create(_Mesh);
End;

Procedure UIPerspectiveView.Release;
Begin
  inherited;

  ReleaseObject(_Mesh);
End;

Procedure UIPerspectiveView.Render(View: TERRAViewport; Const Stage:RendererStage);
Begin
  Self.SetDiffuseMap(0, Self.Target.Viewport.ResolveTexture);

(*  If Engine.Input.Keys.WasPressed(keyK) Then
    Self.Target.Viewport.ResolveTexture.Save('frake.png');*)

  Self.UpdateMouseInput(View);

  Inherited Render(View, Stage);
End;

Procedure UIPerspectiveView.RenderMenuToTexture(V: TERRAViewport);
Begin
  Engine.Graphics.AddRenderable(V, Self._Target);
End;

Procedure UIPerspectiveView.UpdateMouseInput(View: TERRAViewport);
Var
  R:TERRARay;
Begin
  R := View.GetPickRay(Engine.Input.Mouse.X, Engine.Input.Mouse.Y);
End;

End.
