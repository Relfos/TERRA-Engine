{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_Skydome
 * Implements a Skydome
 ***********************************************************************************************************************
}

Unit TERRA_Skydome;
{$i terra.inc}

Interface
Uses TERRA_Utils, TERRA_Texture, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Lights, TERRA_Shader;

Type
  // Skydome object
  Skydome = Class(TERRAObject)
    Protected
      _Shader:Shader;

      _LastUpdate:Cardinal;

      Procedure RenderLayer(Textures:Array Of Texture);

    Public
      Constructor Create;
      Procedure Release;

      Procedure Render; // Renders the Skydome
  End;

Implementation
Uses TERRA_OS, TERRA_GraphicsManager, TERRA_ResourceManager, TERRA_Image, TERRA_Stream,
  TERRA_BoundingBox, TERRA_Vector3D, TERRA_Camera;

// Creates a Skydome
Constructor Skydome.Create;
Begin
  _LastUpdate := GetTime;

  // Create shader
  _Shader := ResourceManager.Instance.GetShader('sky_color');
End;

Procedure Skydome.Release;
Begin
End;

Procedure Skydome.RenderLayer(Textures:Array Of Texture);
Const
  Scale:Single = 2.0;
  R:Single = 1.005; // If you have border issues change this to 1.005f
Begin
	// Begin DrawSkybox
  GraphicsManager.Instance.SetLighting(False);
  GraphicsManager.Instance.SetFog(False);
	glDepthMask(False);
	glDepthRange(0.99, 1.0);

	// Save Current Matrix
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();

	// First apply scale matrix
	glScalef(scale, scale, scale);

	// Common Axis Z - FRONT Side
	Texture.Bind(Textures[4], 0);
	glBegin(GL_QUADS);
		glTexCoord2f(0.0, 0.0); glVertex3f( r ,1.0, -r);
		glTexCoord2f(1.0, 0.0); glVertex3f( r, 1.0, r);
		glTexCoord2f(1.0, 1.0); glVertex3f(-r, 1.0, r);
		glTexCoord2f(0.0, 1.0); glVertex3f(-r ,1.0, -r);
	glEnd();

	// Common Axis X - Left side
	Texture.Bind(Textures[0], 0);
	glBegin(GL_QUADS);
		glTexCoord2f(0.0, 1.0); glVertex3f(-1.0, -r,-r);
		glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  r,-r);
		glTexCoord2f(1.0, 0.0); glVertex3f(-1.0,  r, r);
		glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -r, r);
	glEnd();

	// Common Axis X - Right side
	Texture.Bind(Textures[1], 0);
	glBegin(GL_QUADS);
		glTexCoord2f(0.0, 1.0); glVertex3f(1.0, -r, r);
		glTexCoord2f(0.0, 0.0); glVertex3f(1.0,  r, r);
		glTexCoord2f(1.0, 0.0); glVertex3f(1.0,  r,-r);
		glTexCoord2f(1.0, 1.0); glVertex3f(1.0, -r,-r);
	glEnd();

	// Common Axis Y - Draw Up side
	Texture.Bind(Textures[3], 0);
	glBegin(GL_QUADS);
		glTexCoord2f(0.0, 1.0); glVertex3f(-r, -r, 1.0);
		glTexCoord2f(0.0, 0.0); glVertex3f(-r,  r, 1.0);
		glTexCoord2f(1.0, 0.0); glVertex3f( r,  r, 1.0);
		glTexCoord2f(1.0, 1.0); glVertex3f( r, -r, 1.0);
	glEnd();

	// Common Axis Y - Draw Up side
	Texture.Bind(Textures[2], 0);
	glBegin(GL_QUADS);
		glTexCoord2f(0.0, 1.0); glVertex3f( r, -r, -1.0);
		glTexCoord2f(0.0, 0.0); glVertex3f( r,  r, -1.0);
		glTexCoord2f(1.0, 0.0); glVertex3f(-r,  r, -1.0);
		glTexCoord2f(1.0, 1.0); glVertex3f(-r, -r, -1.0);
	glEnd();

	// Load Saved Matrix
	glPopMatrix();

	glDepthMask(True);
	glDepthRange(0.0, 1.0);
End;

Procedure Skydome.Render;
Var
  Sun:DirectionalLight;
  Delta:Cardinal;
  I:Integer;
  Speed:PSingle;
  Vertices:BoundingBoxVertices;
  Camera:TERRA_Camera.Camera;
  Normal:Vector3D;

  Function GetNormal(V:Vector3D):Vector3D;
  Begin
    Result := VectorSubtract(V, Camera.Position);
    Result.Normalize;
  End;

Begin
  Sun := DirectionalLight(LightManager.Instance.GetLight(0));
  Vertices := GraphicsManager.Instance.ActiveViewport.Camera.Frustum.Vertices;

  // Disable ligthing, because we are using lit vertices
  GraphicsManager.Instance.SetLighting(False);
  GraphicsManager.Instance.SetFog(False);

  // We need to disable zbuffer, otherwise other geometry wont be GraphicsManager correctly
  glDepthMask(False);
  glDepthRange(0.9999, 1.0);

  Camera := GraphicsManager.Instance.ActiveViewport.Camera;

  If (GraphicsManager.Instance.Settings.DeferredLighting.Enabled) Then
    GraphicsManager.Instance.FrameBuffer.Bind(0,0);

  Shader.Bind(_Shader);
  Shader.SetUniform('exposure', 0.5);
  Shader.SetUniform('texture', 0);
  Shader.SetUniform('sunDirection', Sun.Direction);
  Shader.SetUniform('cameraPosition', Camera.Position);
  Shader.SetUniform('cameraDirection', Camera.View);
  Shader.SetUniform('skyPosition', VectorAdd(Camera.Position, VectorScale(Camera.View, Camera.Far)));

  //  glEnable(GL_CULL_FACE);
//  glPolygonMode(GL_FRONT, GL_LINE);

	glMatrixMode(GL_PROJECTION);
	glPushMatrix;
  glLoadIdentity;
	glOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);
	glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
	glLoadIdentity();

	glBegin(GL_QUADS);
    Normal := GetNormal(Vertices[5]);
		glMultiTexCoord2f(GL_TEXTURE0, 1.0, 0.0);
		glMultiTexCoord3fv(GL_TEXTURE1, @Normal);
    glVertex2f(1.0, 0.0);

    Normal := GetNormal(Vertices[7]);
		glMultiTexCoord2f(GL_TEXTURE0, 1.0, 1.0);
    glMultiTexCoord3fv(GL_TEXTURE1, @Normal);
    glVertex2f(1.0, 1.0);

    Normal := GetNormal(Vertices[8]);
		glMultiTexCoord2f(GL_TEXTURE0, 0.0, 1.0);
    glMultiTexCoord3fv(GL_TEXTURE1, @Normal);
    glVertex2f(0.0, 1.0);

    Normal := GetNormal(Vertices[6]);
		glMultiTexCoord2f(GL_TEXTURE0, 0.0, 0.0);
    glMultiTexCoord3fv(GL_TEXTURE1, @Normal);
    glVertex2f(0.0, 0.0);
	glEnd();

	glMatrixMode(GL_PROJECTION);
	glPopMatrix;
	glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  // Restore zwriting
  glDepthRange(0.0, 1.0);
  glDepthMask(True);

 // Texture.Bind(Nil);
End;

End.

