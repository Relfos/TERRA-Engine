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
 * TERRA_Scene
 * Implements a generic Scene
 ***********************************************************************************************************************
}
Unit TERRA_Scene;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Matrix4x4, TERRA_Vector3D, TERRA_Viewport;

{$HINTS OFF}

Type
  TERRAScene = Class(TERRAObject)
    Public
      Procedure IncludeShadowCasters(V:TERRAViewport; Var MinZ,MaxZ:Single; Const ShadowMatrix4x4:Matrix4x4); Virtual;
      Procedure RenderShadowCasters(V:TERRAViewport); Virtual;
      {Procedure RenderReflections(V:Viewport); Virtual;
      Procedure RenderReflectiveSurfaces(V:Viewport); Virtual;}
      Procedure RenderViewport(V:TERRAViewport); Virtual;

      Procedure RenderSprites(V:TERRAViewport); Virtual;
  End;

Implementation

{ Scene }

Procedure TERRAScene.IncludeShadowCasters(V:TERRAViewport; Var MinZ, MaxZ: Single; Const ShadowMatrix4x4:Matrix4x4);
Begin

End;

Procedure TERRAScene.RenderSprites(V:TERRAViewport);
Begin
  // do nothing
End;

Procedure TERRAScene.RenderViewport(V:TERRAViewport);
Begin

End;

{Procedure TERRAScene.RenderReflections;
Begin

End;

procedure TERRAScene.RenderReflectiveSurfaces;
begin

end;}

Procedure TERRAScene.RenderShadowCasters;
Begin

End;


End.
