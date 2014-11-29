Unit TERRA_Scene;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Matrix, TERRA_Vector3D, TERRA_Viewport;

Type
  Scene = Class(TERRAObject)
    Public
      Procedure IncludeShadowCasters(V:Viewport; Var MinZ,MaxZ:Single; Const ShadowMatrix:Matrix); Virtual;
      Procedure RenderShadowCasters(V:Viewport); Virtual;
      {Procedure RenderReflections(V:Viewport); Virtual;
      Procedure RenderReflectiveSurfaces(V:Viewport); Virtual;}
      Procedure RenderViewport(V:Viewport); Virtual;
      Procedure RenderSky(V:Viewport); Virtual;
      Procedure RenderSkyEmission(V:Viewport); Virtual;
      Procedure RenderSprites(V:Viewport); Virtual;
      Procedure OnMouseDown(X,Y, Button:Integer); Virtual;
  End;

Implementation

{ Scene }

Procedure Scene.IncludeShadowCasters(V:Viewport; Var MinZ, MaxZ: Single; Const ShadowMatrix:Matrix);
Begin

End;

Procedure Scene.OnMouseDown(X, Y, Button: Integer);
Begin

End;

Procedure Scene.RenderSprites(V:Viewport);
Begin
  // do nothing
End;

Procedure Scene.RenderViewport(V:Viewport);
Begin

End;

{Procedure Scene.RenderReflections;
Begin

End;

procedure Scene.RenderReflectiveSurfaces;
begin

end;}

Procedure Scene.RenderShadowCasters;
Begin

End;

Procedure Scene.RenderSky;
Begin

End;

Procedure Scene.RenderSkyEmission;
Begin

End;

End.
