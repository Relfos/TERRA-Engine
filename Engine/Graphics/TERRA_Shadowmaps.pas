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
 * TERRA_ShadowMaps
 * Implements shadow maps
 ***********************************************************************************************************************
}
Unit TERRA_Shadowmaps;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Frustum, TERRA_FrameBufferObject, 
  TERRA_BoundingBox, TERRA_Matrix, TERRA_Quaternion, TERRA_Math
  {$IFDEF SHADERS},TERRA_Shader{$ENDIF};
             
Type
  ShadowMapObject = Class(TERRAObject)
    Protected
      _Light:Pointer;
      _ShadowStaticShader:Shader;
      _ShadowAnimatedShader:Shader;
	    _LookAtMatrix:Matrix;
      _LastSlot:Integer;

    Public
      Constructor Create;

      Procedure SetShader(Animated:Boolean);

      Procedure Render; Virtual; Abstract;

      Procedure Bind(Slot:Integer); Virtual; Abstract;
      Procedure Unbind; Virtual; Abstract;
  End;

  CascadedShadowmap = Class(ShadowMapObject)
    Protected
      _SplitCount:Integer;
      _Offset,_Scale:Scalar;
      _CurrentIndex:Integer;
      _Near:Array Of Scalar;
      _Far:Array Of Scalar;
      _SplitDistances:Array Of Scalar;
      _fbo:Array Of FrameBufferObject;
      _ShadowMatrix:Array Of Matrix;
      _depthRanges:Vector4D;

      Function ApplyCropMatrix(Corners:BoundingBoxVertices):Scalar;
      Procedure CalculateSplitDistances;

    Public
      Constructor Create(Light:Pointer; Splits:Integer);
      Procedure Release; Override;

      Procedure Render; Override;

      Procedure Bind(Slot:Integer); Override;
      Procedure Unbind; Override;
    End;

  SimpleShadowMap = Class(ShadowMapObject)
    Protected
      _fbo:FrameBufferObject;
      _ShadowMatrix:Matrix;
      _offset:Scalar;

    Public
      Constructor Create(Light:Pointer);
      Procedure Release; Override;

      Procedure Render; Override;

      Procedure Bind(Slot:Integer); Override;
      Procedure Unbind; Override;
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_GraphicsManager, TERRA_Texture, TERRA_Camera, TERRA_ResourceManager, TERRA_Application,
  TERRA_Lights, TERRA_Color, TERRA_Vector3D{$IFNDEF FIXEDPOINT}, Math{$ENDIF};

Constructor ShadowMapObject.Create;
Begin
  _ShadowStaticShader := ResourceManager.Instance.GetShader('static_shadow');
  _ShadowAnimatedShader := ResourceManager.Instance.GetShader('animated_shadow');
End;

Procedure ShadowMapObject.SetShader(Animated:Boolean);
Begin
  If Animated Then
    Shader.Bind(_ShadowAnimatedShader)
  Else
    Shader.Bind(_ShadowStaticShader);
  Shader.SetUniform('cameraMatrix', _LookAtMatrix);
End;

Constructor CascadedShadowMap.Create(Light:Pointer; Splits:Integer);
Var
  I:Integer;
  ShadowMapSize:Integer;
Begin
  Inherited Create;

  _Light := Light;
  _SplitCount := Splits;
  _Offset := GraphicsManager.Instance.Settings.ShadowBias;
  _Scale := 8.0;

  SetLength(_Near, _SplitCount);
  SetLength(_Far, _SplitCount);
  SetLength(_SplitDistances, _SplitCount);
  SetLength(_fbo, _SplitCount);
  SetLength(_ShadowMatrix, _SplitCount);

  ShadowMapSize := GraphicsManager.Instance.Settings.ShadowMapSize;
  For I:=0 To Pred(_SplitCount) Do
  Begin
    _fbo[I] := FrameBufferObject.Create(ShadowMapSize, ShadowMapSize, FBO_COLOR8, True, False, False, 1);
    _fbo[I].ClearColor := ColorCreate(0,0,0,0);
    _fbo[I].Bind(0,0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);    
    Texture.Bind(Nil);
  End;
End;

Procedure CascadedShadowMap.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SplitCount) Do
    ReleaseObject(_fbo[I]);
End;

Procedure CascadedShadowMap.Bind(Slot:Integer);
Var
  I:Integer;
  ShadowMatrixLocation:Integer;
Begin
  _LastSlot := Slot;
  Shader.SetUniform('splitDistance', _DepthRanges);

  For I:=0 To Pred(_SplitCount) Do
  Begin
    _fbo[I].Bind(0, Slot + I);
    Shader.SetUniform('shadowmap'+IntToString(I), Slot + I);
  End;

  ShadowMatrixLocation := Shader.GetUniform('shadowMatrix');  
  If (ShadowMatrixLocation<0) Then
    Exit;

  glUniformMatrix4fv(ShadowMatrixLocation, _SplitCount, False, @(_ShadowMatrix[0]));  
End;

Procedure CascadedShadowMap.Unbind;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SplitCount) Do
    Texture.Bind(Nil, _LastSlot + I);
End;

// Use the practical split scheme for PSSM. This is exactly the same as in the paper.
// I've had no special troubles implementing it, so I won't go into detail about this function.
Procedure CascadedShadowMap.CalculateSplitDistances;
Var
  I:Integer;
  Lambda, Ratio, si:Scalar;
  fNear, fFar:Scalar;
Begin
  fNear := GraphicsManager.Instance.ActiveViewport.Camera.Near;
  fFar := GraphicsManager.Instance.ActiveViewport.Camera.Far;
	lambda := GraphicsManager.Instance.Settings.ShadowSplitWeight;
	ratio := fNear/fFar;
	_Near[0] := fNear;

	For I:=1 To Pred(_SplitCount) Do
	Begin
		si := Succ(i) / _SplitCount;

		_Near[i] := lambda*(fNear*power(ratio, si)) + (1-lambda)*(fNear + (fFar - fNear)*si);
		_Far[i-1] := _Near[i]  * 1.005;
	End;
	_Far[Pred(_SplitCount)] := fFar;
End;

Function CascadedShadowMap.ApplyCropMatrix(Corners:BoundingBoxVertices):Scalar;
Var
  I:Integer;
	shad_modelview,
	shad_proj,
	shad_crop,
	shad_mvp:Matrix;

  maxX, maxY, maxZ, minX, minY, minZ:Scalar;
  scaleX, scaleY, offsetX, offsetY:Scalar;
  transf:Vector4D;
  nv_mvp:Matrix;
  Sphere:BoundingSphere;
Begin
	maxX := -1000.0;
  maxY := -1000.0;
  minX :=  1000.0;
  minY :=  1000.0;

	// find the z-range of the current frustum as seen from the light
	// in order to increase precision
	glGetFloatv(GL_MODELVIEW_MATRIX, @shad_modelview);  
	nv_mvp := shad_modelview;

	// note that only the z-component is need and thus
	// the multiplication can be simplified
	// transf.z = shad_modelview[2] * f.point[0].x + shad_modelview[6] * f.point[0].y + shad_modelview[10] * f.point[0].z + shad_modelview[14];
  Transf := QuaternionCreate(Corners[1]);
	transf.Transform(nv_mvp);
	minZ := transf.z;
	maxZ := transf.z;
	For I:=2 To 7 Do
	Begin
    Transf := QuaternionCreate(Corners[I]);
  	transf.Transform(nv_mvp);

		if(transf.z > maxZ) Then maxZ := transf.z;
		if(transf.z < minZ) Then minZ := transf.z;
	End;

	// make sure all relevant shadow casters are included
	// note that these here are dummy objects at the edges of our scene
  If Assigned(GraphicsManager.Instance.Scene) Then
    GraphicsManager.Instance.Scene.IncludeShadowCasters(MinZ, MaxZ, nv_mvp);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();	

  // set the projection matrix with the new z-bounds
	// note the inversion because the light looks at the neg. z axis	
	glOrtho(-1.0, 1.0, -1.0, 1.0, -maxZ, -minZ); 

	glGetFloatv(GL_PROJECTION_MATRIX, @shad_proj);	
  glPushMatrix();                                 
	glMultMatrixf(@shad_modelview);                 
	glGetFloatv(GL_PROJECTION_MATRIX, @shad_mvp);   
	glPopMatrix();                                  

	// find the extends of the frustum slice as projected in light's homogeneous coordinates
	nv_mvp := shad_mvp;
	For I:=1 To 8 Do
	Begin
    Transf := QuaternionCreate(Corners[I]);
    Transf.Transform(nv_mvp);

		transf.x := transf.x / transf.w;
		transf.y := transf.y / transf.w;

		if(transf.x > maxX) Then maxX := transf.x;
		if(transf.x < minX) Then minX := transf.x;
		if(transf.y > maxY) Then maxY := transf.y;
		if(transf.y < minY) Then minY := transf.y;
	End;

	scaleX := ScalarTwo / (maxX - minX);
	scaleY := ScalarTwo / (maxY - minY);
	offsetX := - ScalarHalf * (maxX + minX) *scaleX;
	offsetY := - ScalarHalf *(maxY + minY) *scaleY;

	// apply a crop matrix to modify the projection matrix we got from glOrtho.
	nv_mvp := MatrixIdentity;
	nv_mvp.V[0] := scaleX;
	nv_mvp.V[5] := scaleY;
	nv_mvp.V[12] := offsetX;
	nv_mvp.V[13] := offsetY;
	shad_crop := nv_mvp;
	glLoadMatrixf(@shad_crop);                      
	glMultMatrixf(@shad_proj);                      

	Result := minZ;
End;

Procedure CascadedShadowMap.Render;
Var
  shad_modelview:Matrix;
  Projection:Matrix;
  shad_cpm:Matrix;
  Bias:Matrix;
  Light:DirectionalLight;
  I:Integer;
  FrustumCorners:BoundingBoxVertices;
  minZ:Scalar;
  PP:PScalarArray;
  ShadowMapSize:Integer;
Begin
  Light := DirectionalLight(_Light);

	glMatrixMode(GL_PROJECTION);                    
	glPushMatrix();                                 

	glMatrixMode(GL_MODELVIEW);                     
	glPushMatrix();                                 
  _LookAtMatrix := MatrixLookAt(VectorZero, VectorScale(Light.Direction, -1.0), VectorCreate(-1.0, 0.0, 0.0));
  glLoadMatrixf(@(_LookAtMatrix.V[0]));  
  shad_modelview := _LookAtMatrix;

  ShadowMapSize := GraphicsManager.Instance.Settings.ShadowMapSize;

	// store the screen viewport
	glPushAttrib(GL_VIEWPORT_BIT);       

	// and render only to the shadowmap
	GraphicsManager.Instance.SetViewArea(0, 0, ShadowMapSize, ShadowMapSize);  

	// offset the geometry slightly to prevent z-fighting
	// note that this introduces some light-leakage artifacts
	//glPolygonOffset( 1, 2);  	
  //glEnable(GL_POLYGON_OFFSET_FILL);     

	// draw all faces since our terrain is not closed.
	glDisable(GL_CULL_FACE);              

	// compute the z-distances for each split as seen in camera space
	CalculateSplitDistances;

  Bias := MatrixTransform(VectorUniform(ScalarHalf), VectorZero, VectorUniform(ScalarHalf));

  PP := @_DepthRanges;
	// for all shadow maps:
	For I:=0 To Pred(_SplitCount) Do
	Begin
	  glMatrixMode(GL_PROJECTION);          

    glPushMatrix;                         
    Projection := MatrixPerspective(45.0, GraphicsManager.Instance.Width/GraphicsManager.Instance.Height, _Near[I], _Far[I]);
    glLoadMatrixf(@(Projection.V[0]));    
    glPopMatrix;                          

		// Calculate the frustum corners for the current split
    FrustumCorners := GraphicsManager.Instance.ActiveViewport.Camera.Frustum.GetVertices(GraphicsManager.Instance.ActiveViewport.Camera.Matrix, Projection);

		// adjust the view frustum of the light, so that it encloses the camera frustum slice fully.
		// note that this function sets the projection matrix as it sees best fit
		// minZ is just for optimization to cull trees that do not affect the shadows
		minZ := applyCropMatrix( FrustumCorners);

		// make the current depth map a rendering target
       View.SetViewArea(0, 0, curRT.Width, curRT.Height);
    _FBO[I].BeginCapture;

		// draw the scene
		GraphicsManager.Instance.RenderShadowmap;

    _FBO[I].EndCapture;

    If (Application.Instance.Keys[Ord('U')]) Then
      _fbo[I].Save(0, 'shadowmap'+IntToString(I)+'.png');

		glMatrixMode(GL_PROJECTION);              
		// store the product of all shadow matries for later
		glMultMatrixf(@shad_modelview);           
		glGetFloatv(GL_PROJECTION_MATRIX, @shad_cpm);    
    glMatrixMode(GL_PROJECTION);                     
    glLoadMatrixf(@Bias);                            
		glMultMatrixf(@shad_cpm);                        
		// multiply the light's (bias*crop*proj*modelview) by the inverse camera modelview
    glGetFloatv(GL_PROJECTION_MATRIX, @_ShadowMatrix[I]);    

    PP[i] := 0.5*(-_Far[i]* GraphicsManager.Instance.ActiveViewport.Projection.V[10] + GraphicsManager.Instance.ActiveViewport.Projection.V[14])/_Far[i] + 0.5;
	End;

    If (Application.Instance.Keys[Ord('U')]) Then
      Application.Instance.Keys[Ord('U')] := False;

      {
    If (Application.Instance.Keys[Ord('H')]) Then
    Begin
      _Offset := _Offset - 0.025;
      Application.Instance.Keys[Ord('H')] := False;
    End;
    If (Application.Instance.Keys[Ord('J')]) Then
    Begin
      _Offset := _Offset + 0.025;
      Application.Instance.Keys[Ord('J')] := False;
    End;}

	// revert to normal back face culling as used for rendering
	glEnable(GL_CULL_FACE);             

	glDisable(GL_POLYGON_OFFSET_FILL);  
	glPopAttrib();                      

	glMatrixMode(GL_PROJECTION);        
	glPopMatrix();                      
	glMatrixMode(GL_MODELVIEW);         
	glPopMatrix();                      
End;


Constructor SimpleShadowMap.Create(Light:Pointer);
Var
  ShadowMapSize:Integer;
Begin
  Inherited Create;

  ShadowMapSize := GraphicsManager.Instance.Settings.ShadowMapSize;

  _Light := Light;
  _fbo := FrameBufferObject.Create(ShadowMapSize, ShadowMapSize, FBO_COLOR8, True, False, False, 1);
  _fbo.ClearColor := ColorCreate(0,0,0,0);
  _fbo.Bind(0,0);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);  
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
  Texture.Bind(Nil);
End;

Procedure SimpleShadowMap.Release;
Begin
  ReleaseObject(_FBO);
End;

Procedure SimpleShadowMap.Render;
Var
  Light:SpotLight;
  Size:Integer;
  Dest:Vector3D;
  VM,PM:Matrix;
  Bias, Perspective:Matrix;
Begin
  Light := SpotLight(_Light);
  Size := Round(Tan(Light.OuterCone) * Light.Intensity);
  Dest := VectorAdd(Light.Position, VectorScale(Light.Direction, Light.Intensity));

	glMatrixMode(GL_PROJECTION);    	
  glPushMatrix();                   
  Perspective := MatrixPerspective(Light.OuterCone * Deg,1.0, 0.1, Light.Intensity);
  glLoadMatrixf(@(Perspective.V[0]));                 

	glMatrixMode(GL_MODELVIEW);           
	glPushMatrix();                       
  _LookAtMatrix := MatrixLookAt(Light.Position, Dest, VectorCreate(0.0, 0.0, 1.0));
	glLoadMatrixf(@(_LookAtMatrix.V[0]));  

	glGetFloatv(GL_PROJECTION_MATRIX,@PM);    
	glGetFloatv(GL_MODELVIEW_MATRIX,@VM);     

  glCullFace(GL_FRONT);                     

	// make the current depth map a rendering target
     View.SetViewArea(0, 0, curRT.Width, curRT.Height);
  _fbo.BeginCapture;

	// draw the scene
	GraphicsManager.Instance.RenderShadowmap;//(minZ);

  _fbo.EndCapture;

  If (Application.Instance.Keys[Ord('I')]) Then
  Begin
    Application.Instance.Keys[Ord('I')] := False;
    _fbo.Save(0, 'spotshadowmap.png');
  End;

  glCullFace(GL_BACK);                      
  Bias := MatrixTransform(VectorUniform(ScalarHalf), VectorZero, VectorUniform(ScalarHalf));

	glMatrixMode(GL_PROJECTION);              
  glLoadMatrixf(@Bias);                     
	glMultMatrixf(@PM);                       
  glMultMatrixf(@VM);                       
  glGetFloatv(GL_PROJECTION_MATRIX, @_ShadowMatrix);  
	glPopMatrix();                                      

	glMatrixMode(GL_MODELVIEW);                         
	glPopMatrix();                                      

    If (Application.Instance.Keys[Ord('H')]) Then
    Begin
      _Offset := _Offset - 3;
      Application.Instance.Keys[Ord('H')] := False;
    End;
    If (Application.Instance.Keys[Ord('J')]) Then
    Begin
      _Offset := _Offset + 3;
      Application.Instance.Keys[Ord('J')] := False;
    End;
End;

Procedure SimpleShadowMap.Bind(Slot:Integer);
Begin
  _LastSlot := Slot;
  _fbo.Bind(0, Slot);
  Shader.SetUniform('shadowmap', Slot);
  Shader.SetUniform('shadowMatrix', _ShadowMatrix);
End;

Procedure SimpleShadowMap.Unbind;
Begin
  Texture.Bind(Nil, _LastSlot);
End;

End.
