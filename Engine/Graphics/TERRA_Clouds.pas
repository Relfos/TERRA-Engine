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
 * TERRA_Clouds
 * Implements cloud rendering
 ***********************************************************************************************************************
}
Unit TERRA_Clouds;

{$I terra.inc}
Interface
Uses TERRA_Utils, TERRA_Vector3D, TERRA_Color, TERRA_BoundingBox, TERRA_Camera, TERRA_Math,
  TERRA_Matrix, TERRA_Viewport, TERRA_Lights, TERRA_Texture, TERRA_UI, TERRA_Application,
  TERRA_Image, TERRA_GraphicsManager, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Particles, TERRA_Shader;

Type
  CloudParticle = Record
    _Position:Vector3D;
    _Radius:Single;
    _SortDistance:Single;
    _Color:Color;
    _Vertices:Array[0..3] Of ParticleVertex;
  End;

  Cloud = Class(Renderable)
    Protected
      _Position:Vector3D;
      _Particles:Array Of CloudParticle;
      _ParticleCount:Integer;
      _Box:BoundingBox;

      _Vertices:Array Of ParticleVertex;
      _VertexCount:Integer;

      _iShadeResolution:Integer;
      _rAlbedo:Single;
      _rExtinction:Single;
      _rTransparency:Single;
      _rScatterFactor:Single;
      _rSortAngleErrorTolerance:Single;
      _rSortSquareDistanceTolerance:Single;

      _vecLastSortViewDir:Vector3D;
      _vecLastSortCamPos:Vector3D;

      _UsePhaseFunction:Boolean;

      _Shader:Shader;

      _Initialized:Boolean;
      _NeedsShading:Boolean;
      _PositionHandle, _UVHandle, _OfsHandle, _SizeHandle, _ColorHandle:Integer;
      _Up, _Right:Vector3D;
      _Ratio:Single;
      _Strength:Single;

      Procedure SetupShader(View:Viewport);

      Procedure SortParticles(vecViewDir, vecSortPoint:Vector3D; dir:Integer);
      Procedure QuickSort(iLo,iHi, dir:Integer);

      Function PhaseFunction(vecLightDir, vecViewDir:Vector3D):Single;

      Procedure Illuminate(pLight:Light; bReset:Boolean);

      Procedure CreateSplatTexture(N:Integer);

      Procedure SetPosition(P:Vector3D);

      Procedure AddSphere(P:Vector3D; Radius:Single);

    Public
      Constructor Create(Radius:Single);

      Procedure Render; Override;
      Procedure PreProcess; Override;

      Function GetBoundingBox:BoundingBox; Override;

      Property Position:Vector3D Read _Position Write SetPosition;
      Property Strength:Single Read _Strength Write _Strength;
  End;

Implementation

Const
  SKY_CLOUD_SORT_TOWARD = 0;
  SKY_CLOUD_SORT_AWAY = 1;

Var
  _SplatTexture:Texture;
  _CloudView:Viewport;

{ Cloud }
Constructor Cloud.Create(Radius:Single);
Const
  X = 0.2;
Var
  I:Integer;
  Cam:Camera;
Begin
  _AlphaStage := renderAlpha2;
  _iShadeResolution  := 32;
  _rAlbedo           := 0.9;
  _rExtinction       := 80.0;
  _rTransparency     := exp(-_rExtinction);
  _rScatterFactor    := _rAlbedo * _rExtinction * (1.0 / (4.0 * Pi));
  _rSortAngleErrorTolerance := 0.8;
  _rSortSquareDistanceTolerance := 100;
  _UsePhaseFunction := True;
  _NeedsShading := True;
  _Strength := 0.9;

  If (_SplatTexture=Nil) Then
    Self.CreateSplatTexture(32);

  If (_CloudView=Nil) Then
  Begin
    Cam := Camera.Create;
    _CloudView := Viewport.Create(0.0, 0.0, 1.0, 1.0);
    _CloudView.Camera := Cam;
  End;

  _Box.Reset();
  For I:=1 To 12+Random(5) Do
    Self.AddSphere(VectorCreate(RandomFloat(-Radius*X, Radius*X), RandomFloat(-Radius*X, Radius*X), RandomFloat(-Radius*X, Radius*X)), Radius*(0.35+Random(4)*0.1));

  _Initialized := False;
  _Shader := ParticleManager.Instance.Shader;
End;

Procedure Cloud.AddSphere(P:Vector3D; Radius:Single);
Var
  R,Teta,Ro:Single;
  I, N:Integer;
Begin
  N := _ParticleCount;
  Inc(_ParticleCount, Random(400)+100);
  SetLength(_Particles, _ParticleCount);
  For I:=N To Pred(_ParticleCount) Do
  Begin
    R := RandomFloat(0, Radius*0.25);
    Teta := RandomFloat(0, 360)*RAD;
    Ro := RandomFloat(0, 360)*RAD;
    _Particles[I]._Position.X := R * Sin(Teta) * Cos(Ro);
    _Particles[I]._Position.Y := R * Sin(Teta) * Sin(Ro);
    _Particles[I]._Position.Z := R * Cos(Teta);

    _Particles[I]._Position.Add(P);
    _Particles[I]._Radius := RandomFloat(Radius * 0.05, Radius * 0.1);
    _Particles[I]._Color := ColorWhite;
    _Box.Add(_Particles[I]._Position);
  End;
  _VertexCount := _ParticleCount * 6;
  SetLength(_Vertices, _VertexCount);
End;

Procedure Cloud.QuickSort(iLo,iHi, dir:Integer);
Var
  Lo, Hi: Integer;
  Mid, Temp:CloudParticle;
Begin
    If iHi<iLo Then
      Exit;
    Lo := iLo;
    Hi := iHi;
    Mid := _Particles[(Lo + Hi) Shr 1];
    Repeat
      If (Dir = SKY_CLOUD_SORT_TOWARD) Then
      Begin
        While Mid._SortDistance<_Particles[Lo]._SortDistance Do Inc(Lo);
        While _Particles[Hi]._SortDistance<Mid._SortDistance Do Dec(Hi);
      End Else
      Begin
        While Mid._SortDistance>_Particles[Lo]._SortDistance Do Inc(Lo);
        While _Particles[Hi]._SortDistance>Mid._SortDistance Do Dec(Hi);
      End;

      If Lo <= Hi Then
      Begin
        Temp := _Particles[Lo];
        _Particles[Lo] := _Particles[Hi];
        _Particles[Hi] := Temp;
        Inc(Lo);
        Dec(Hi);
      End;
    Until Lo > Hi;
    If Hi > iLo Then
      QuickSort(iLo, Hi, Dir);
    If Lo < iHi Then
      QuickSort(Lo, iHi, Dir);
End;

Procedure Cloud.SortParticles(vecViewDir, vecSortPoint:Vector3D; dir:Integer);
Var
  partPos:Vector3D;
  I:Integer;
Begin
	For I:=0 To Pred(_ParticleCount) Do
	Begin
		partPos := _Particles[i]._Position;
		partPos.Subtract(vecSortPoint);
		_Particles[I]._SortDistance := VectorDot(partPos, vecViewDir);
	End;

  Self.QuickSort(0, Pred(_ParticleCount), Dir);
End;

Function Cloud.GetBoundingBox: BoundingBox;
Begin
  Result := _Box;
End;

(* A phase function is a transfer function that determines, for any angle between incident
 * and outgoing directions, how much of the incident light intensity will be
 * scattered in the outgoing direction.  For example, scattering by very small
 * particles such as those found in clear air, can be approximated using <i>Rayleigh
 * scattering</i>.  The phase function for Rayleigh scattering is
 * p(q) = 0.75*(1 + cos<sup>2</sup>(q)), where q  is the angle between incident
 * and scattered directions.  Scattering by larger particles is more complicated.
 * It is described by Mie scattering theory.  Cloud particles are more in the regime
 * of Mie scattering than Rayleigh scattering.  However, we obtain good visual
 * results by using the simpler Rayleigh scattering phase function as an approximation.
 *)
Function Cloud.PhaseFunction(vecLightDir, vecViewDir:Vector3D):Single;
Var
  rCosAlpha:Single;
Begin
  rCosAlpha := VectorDot(vecLightDir, vecViewDir);
  Result := 0.75 * (1 + rCosAlpha * rCosAlpha); // rayleigh scattering = (3/4) * (1+cos^2(alpha))
End;

Procedure Cloud.Render;
Var
  rCosAngleSinceLastSort:Single;
  rSquareDistanceSinceLastSort:Single;
  vecSortPos:Vector3D;
  Cam:Camera;
  cloudColor, lightColor:Color;
  eyeDir:Vector3D;
  I,J,K,Count:Integer;
  pf:Single;
Begin
  Cam := GraphicsManager.Instance.ActiveViewport.Camera;

  // This cosine computation, along with the if() below, are an optimization.  The goal
  // is to avoid sorting when it will make no visual difference.  This will be true when the
  // cloud particles are almost sorted for the current viewpoint.  This is the case most of the
  // time, since the viewpoint does not move very far in a single frame.  Each time we sort,
  // we cache the current view direction.  Then, each time the cloud is displayed, if the
  // current view direction is very close to the current view direction (dot product is nearly 1)
  // then we do not resort the particles.
  rCosAngleSinceLastSort := VectorDot(_vecLastSortViewDir, Cam.View); // dot product

  rSquareDistanceSinceLastSort := Cam.Position.Distance(_vecLastSortCamPos);

  If (rCosAngleSinceLastSort < _rSortAngleErrorTolerance) Or (rSquareDistanceSinceLastSort > _rSortSquareDistanceTolerance) Then
  Begin
    // compute the sort position for particles.
    // don't just use the camera position -- if it is too far away from the cloud, then
    // precision limitations may cause the STL sort to hang.  Instead, put the sort position
    // just outside the bounding sphere of the cloud in the direction of the camera.
    vecSortPos := VectorScale(Cam.View, -1);
    vecSortPos.Scale(1.1 * _Box.Radius());

    // sort the particles from back to front wrt the camera position.
    SortParticles(cam.View, vecSortPos, SKY_CLOUD_SORT_TOWARD);

    _vecLastSortViewDir := Cam.View;
    _vecLastSortCamPos := Cam.Position;
  End;

  // set the material state / properties that clouds use for rendering:
  // Enables blending, with blend func (ONE, ONE_MINUS_SRC_ALPHA).
  // Enables alpha test to discard completely transparent fragments.
  // Disables depth test.
  // Enables texturing, with modulation, and the texture set to the shared splat texture.
  //s_pMaterial->Activate();
  Self.SetupShader(GraphicsManager.Instance.ActiveViewport);
  If (_PositionHandle<0) Then
    Exit;

  Texture.Bind(_SplatTexture);

  Count := 0;
  For I:=0 To Pred(_ParticleCount) Do
  Begin
    // Start with ambient light
    cloudColor := _particles[I]._Color;

    if (_UsePhaseFunction) Then // use the phase function for anisotropic scattering.
    Begin
      eyeDir := cam.Position;
      eyeDir.Subtract(_Particles[I]._Position);
      eyeDir.Normalize();

      // add the color contribution to this particle from each light source, modulated by
      // the phase function.  See _PhaseFunction() documentation for details.
      //For J:int i = 0; i < p->GetNumLitColors(); i++)
      pf := PhaseFunction(LightManager.Instance.Sun.Direction, eyeDir);
      // expand this to avoid temporary vector creation in the inner loop
      lightColor := ColorScale(LightManager.Instance.Sun.Color, _Strength);
      cloudColor := ColorAdd(CloudColor, ColorScale(lightColor, pf));
    End Else
    // just use isotropic scattering instead.
    Begin
      lightColor := LightManager.Instance.Sun.Color;
      cloudColor := ColorAdd(CloudColor, lightColor);
    End;

    // Set the transparency independently of the colors
    cloudColor.A := Trunc((1.0 - _rTransparency) * 255);

    // draw the particle as a textured billboard.
    //DrawQuad(_Particles[I]._Position, VectorScale(Cam.Right, _Particles[I]._Radius), VectorScale(Cam.Up, _Particles[I]._Radius), cloudColor);

    For K:=0 To 3 Do
    Begin
      _Particles[I]._Vertices[K].Position := _Particles[I]._Position;
      _Particles[I]._Vertices[K].Ofs.X := ParticleQuadOffsets[K].X;
      _Particles[I]._Vertices[K].Ofs.Y := ParticleQuadOffsets[K].Y;
      _Particles[I]._Vertices[K].Size.X := _Particles[I]._Radius;
      _Particles[I]._Vertices[K].Size.Y := _Particles[I]._Radius;
      _Particles[I]._Vertices[K].Color := cloudColor;
      _Particles[I]._Vertices[K].UV := ParticleUVOffsets[K];
    End;

    _Vertices[Count + 0] := _Particles[I]._Vertices[0];
    _Vertices[Count + 5] := _Vertices[Count + 0];

    _Vertices[Count + 1] := _Particles[I]._Vertices[1];

    _Vertices[Count + 2] := _Particles[I]._Vertices[2];
    _Vertices[Count + 3] := _Particles[I]._Vertices[2];

    _Vertices[Count + 4] := _Particles[I]._Vertices[3];

    Inc(Count, 6);
  End;

  If (Count>0) Then
  Begin
    _Initialized := True;
    GraphicsManager.Instance.SetBlendMode(blendFilter);
    glDepthMask(False);                                                               
    Texture.Bind(_SplatTexture);

    glVertexAttribPointer(_PositionHandle, 3, GL_FLOAT, False, 40, @(_Vertices[0].Position));    
    glVertexAttribPointer(_UVHandle, 2, GL_FLOAT, False, 40, @(_Vertices[0].UV));    
    glVertexAttribPointer(_OfsHandle, 2, GL_FLOAT, False, 40, @(_Vertices[0].Ofs));    
    glVertexAttribPointer(_SizeHandle, 2, GL_FLOAT, False, 40, @(_Vertices[0].Size));    
    glVertexAttribPointer(_ColorHandle, 4, GL_UNSIGNED_BYTE, True, 40, @(_Vertices[0].Color));    

    glDrawArrays(GL_TRIANGLES, 0, Count);
    GraphicsManager.Instance.Internal(0, Count Div 3);

    glDepthMask(True);                                            
    GraphicsManager.Instance.SetBlendMode(blendNone);
  End;
End;


(* This method uses graphics hardware to compute multiple forward scattering at each cloud
 * in the cloud of light from the directional light source @a pLight.  The algorithm works
 * by successively subtracting "light" from an initially white (fully lit) frame buffer by
 * using hardware blending and read back.  The method stores the illumination from each light
 * source passed to it separately at each particle, unless @a bReset is true, in which case
 * the lists of illumination in the particles are reset before the lighting is computed.
 *
 *)
Procedure Cloud.Illuminate(pLight:Light; bReset:Boolean);
Var
  iOldVP:Array[0..3] Of Integer;
  vecUp, vecDir, vecLightPos:Vector3D;
  Cam:Camera;
  DistToCntr:Single;
  rNearDist,rFarDist:Single;
  rPixelsPerLength:Single;
  M:Matrix;
  rSolidAngle:Single;
  I, J, K:Integer;
  vecParticlePos, vecOffset:Vector3D;
  rArea, rDistance:Single;
  iPixelDim, iNumPixels, iSum:Integer;
  rColorScaleFactor:Single;
  vecWinPos:Vector3D;
  c:Array Of Color;
  vecColor, vecScatter, vecScatteredAmount:Color;
Begin
  vecDir := pLight.GetDirection(VectorZero);

  // compute the light/sort position for particles from the light direction.
  // don't just use the camera position -- if it is too far away from the cloud, then
  // precision limitations may cause the STL sort to hang.  Instead, put the sort position
  // just outside the bounding sphere of the cloud in the direction of the camera.
  vecLightPos := vecDir;
  vecLightPos.Scale(1.1*_Box.Radius());
  vecLightPos.Add(_Box.Center());

  // Set up a camera to look at the cloud from the light position.  Since the sun is an infinite
  // light source, this camera will use an orthographic projection tightly fit to the bounding
  // sphere of the cloud.

  // Avoid degenerate camera bases.
  vecUp := VectorUp;
  If (Abs(VectorDot(vecDir,vecUp)) - 1 < 0.001) Then // check that the view and up directions are not parallel.
    vecUp := VectorCreate(1, 0, 0);

  Cam := _CloudView.Camera;
  Cam.Position := vecLightPos;
  Cam.LookAt(_box.Center());

  // sort the particles away from the light source.
  SortParticles(cam.View, vecLightPos, SKY_CLOUD_SORT_AWAY);

  // projected dist to cntr along viewdir
  DistToCntr := VectorDot(VectorSubtract(_Box.Center(), vecLightPos), cam.View);

  // calc tight-fitting near and far distances for the orthographic frustum
  rNearDist := DistToCntr - _Box.Radius();
  rFarDist  := DistToCntr + _Box.Radius();

  _CloudView.SetSize(_iShadeResolution, _iShadeResolution);
  _CloudView.Camera.SetNear(rNearDist);
  _CloudView.Camera.SetFar(rFarDist);
  // switch to parallel projection                                                    f
  _CloudView.EnableOrtho(-_Box.Radius(), _Box.Radius(), -_Box.Radius(), _Box.Radius());

  _CloudView.Camera.Update();
  _CloudView.Bind();

  // set the material state / properties that clouds use for shading:
  // Enables blending, with blend func (ONE, ONE_MINUS_SRC_ALPHA).
  // Enables alpha test to discard completely transparent fragments.
  // Disables depth test.
  // Enables texturing, with modulation, and the texture set to the shared splat texture.
  //_pShadeMaterial->Activate();
  GraphicsManager.Instance.SetBlendMode(blendFilter);

  // initialize back buffer to all white -- modulation darkens areas where cloud particles
  // absorb light, and lightens it where they scatter light in the forward direction.
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);

  rPixelsPerLength := _iShadeResolution / (2.0 * _Box.Radius());

  // setup material
  GraphicsManager.Instance.SetBlendMode(blendFilter);
  glDepthMask(False);                                                               
  Texture.Bind(_SplatTexture);

  Self.SetupShader(_CloudView);

  // the solid angle over which we will sample forward-scattered light.
  rSolidAngle := 0.09;
  i := 0;
  For I:=0 To Pred(_ParticleCount) Do
  Begin
    vecParticlePos := _Particles[I]._Position;

    vecOffset := vecLightPos;
    vecOffset.Subtract(vecParticlePos);

    // compute the pixel area to read back in order to integrate the illumination of the particle
    // over a constant solid angle.
    rDistance  := Abs(VectorDot(cam.View, vecOffset)) - rNearDist;

    rArea := rSolidAngle * rDistance * rDistance;
    iPixelDim  := Trunc(sqrt(rArea) * rPixelsPerLength);
    iNumPixels := iPixelDim * iPixelDim;
    if (iNumPixels < 1) Then
    Begin
      iNumPixels := 1;
      iPixelDim := 1;
    End;

    // the scale factor to convert the read back pixel colors to an average illumination of the area.
    rColorScaleFactor := rSolidAngle / (iNumPixels * 255.0);

    SetLength(c, iNumPixels);

    // find the position in the buffer to which the particle position projects.
    vecWinPos := _CloudView.ProjectPoint(vecParticlePos);

    // offset the projected window position by half the size of the readback region.
    vecWinPos.x := vecWinPos.x - 0.5 * iPixelDim;
    if (vecWinPos.x < 0) Then vecWinPos.x := 0;
    vecWinPos.y := vecWinPos.y - 0.5 * iPixelDim;
    if (vecWinPos.y < 0) Then vecWinPos.y := 0;

    // read back illumination of this particle from the buffer.
    glReadBuffer(GL_BACK);
    glReadPixels(Trunc(vecWinPos.x), Trunc(vecWinPos.y), iPixelDim, iPixelDim, GL_RGBA, GL_UNSIGNED_BYTE, c);

    // scattering coefficient vector.
    vecScatter := ColorGrey(Trunc(255*_rScatterFactor), 255);

    // add up the read back pixels (only need one component -- its grayscale)
    iSum := 0;
    For K:=0 To iNumPixels-1 Do
      Inc(iSum, c[k].R);

    // compute the amount of light scattered to this particle by particles closer to the light.
    // this is the illumination over the solid angle that we measured (using glReadPixels) times
    // the scattering coefficient (vecScatter);
    vecScatteredAmount := ColorGrey(Trunc(iSum * rColorScaleFactor*255), Trunc((1 - _rTransparency)*255));
    vecScatteredAmount := ColorScale(vecScatteredAmount, vecScatter);

    // the color of th particle (iter) contributed by this light source (pLight) is the
    // scattered light from the part of the cloud closer to the light, times the diffuse color
    // of the light source.  The alpha is 1 - the uniform transparency of all particles (modulated
    // by the splat texture).
    vecColor := ColorScale(vecScatteredAmount, pLight.Color);
    vecColor.a := Trunc((1 - _rTransparency)*255);

    // add this color to the list of lit colors for the particle.  The contribution from each light
    // is kept separate because the phase function we apply at runtime depends on the light vector
    // for each light source separately.  This view-dependent effect is impossible without knowing
    // the amount of light contributed for each light.  This, of course, assumes the clouds will
    // be lit by a reasonably small number of lights (The sun plus some simulation of light reflected
    // from the sky and / or ground.) This technique works very well for simulating anisotropic
    // illumination by skylight.
    _Particles[I]._Color := vecColor;

    // the following computation (scaling of the scattered amount by the phase function) is done
    // after the lit color is stored so we don't add the scattering to this particle twice.
    vecScatteredAmount := ColorScale(vecScatteredAmount, 1.5); // rayleigh scattering phase function for angle of zero or 180 = 1.5!

    // clamp the color
    vecScatteredAmount.a := Trunc((1 - _rTransparency)*255);

    K := I * 6;
    For J:=0 To 5 Do
      _Vertices[K+J].Color := _Particles[I]._Color;

    // Draw the particle as a texture billboard.  Use the scattered light amount as the color to
    // simulate forward scattering of light by this particle.
    glVertexAttribPointer(_PositionHandle, 3, GL_FLOAT, False, 40, @(_Vertices[K].Position));    
    glVertexAttribPointer(_UVHandle, 2, GL_FLOAT, False, 40, @(_Vertices[K].UV));    
    glVertexAttribPointer(_OfsHandle, 2, GL_FLOAT, False, 40, @(_Vertices[K].Ofs));    
    glVertexAttribPointer(_SizeHandle, 2, GL_FLOAT, False, 40, @(_Vertices[K].Size));    
    glVertexAttribPointer(_ColorHandle, 4, GL_UNSIGNED_BYTE, True, 40, @(_Vertices[K].Color));    

    glDrawArrays(GL_TRIANGLES, 0, 6);
    GraphicsManager.Instance.Internal(0, 2);

  // Application.Instance.SwapBuffers();
  // Sleep(100);
  End;

  glDepthMask(True);                                            
  GraphicsManager.Instance.SetBlendMode(blendNone);
End;

Procedure Cloud.CreateSplatTexture(N:Integer);
Var
  M:Array Of Single;
  X,Y,Y2,Dist:Single;
  Incr:Single;
  I,J,K,W:Integer;
  Img:Image;
Begin
  SetLength(M, 2*N*N);
  Img := Image.Create(N, N);
  Incr := 2.0/N;
  i := 0;
  j := 0;
  Y := -1.0;
  For W:=0 To N-1 Do
  Begin
    Y2 := Y*Y;
    X := -1.0;
    For K:=0 To N-1 Do
    Begin
      Dist := sqrt(X*X+Y2);
      if (Dist>1) Then Dist := 1;
      M[i] := HermiteInterpolate(0.4,0,0,0,Dist);// * (1 - noise);
      M[i+1] := M[I];
      Img.SetPixelByOffset(J, ColorGrey(Trunc(M[i] * 255), Trunc(M[i] * 255)));

      X := X + Incr;
      Inc(I,2);
      Inc(J);
      IntToString(K);
    End;

    Y := Y + Incr;
  End;

  //Img.Save('test.png');

  _SplatTexture := Texture.New(N, N);
  _SplatTexture.Update();
  _SplatTexture.UpdateRect(Img, 0, 0);
  ReleaseObject(Img);
End;

Procedure Cloud.SetPosition(P: Vector3D);
Begin
  _Position := P;
End;

Procedure Cloud.SetupShader(View:Viewport);
Begin
  ShaderManager.Instance.Bind(_Shader);

  If GraphicsManager.Instance.LandscapeOrientation Then
  Begin
    _Up := View.Camera.Right;
    _Right := View.Camera.Up;
    _Up.Scale(-1.0);
  End Else
  Begin
    _Right := View.Camera.Right;
    _Up := View.Camera.Up;
  End;

  _Ratio := UI.Instance.Height/UI.Instance.Width;

  _PositionHandle := _Shader.GetAttribute('terra_position');
  _UVHandle := _Shader.GetAttribute('terra_UV0');
  _OfsHandle := _Shader.GetAttribute('terra_ofs');
  _SizeHandle := _Shader.GetAttribute('terra_size');
  _ColorHandle := _Shader.GetAttribute('terra_color');

  View.Camera.SetupUniforms;

  _Shader.SetUniform('cameraUp', _Up);
  _Shader.SetUniform('cameraRight', _Right);
  _Shader.SetUniform('texture0', 0);
  _Shader.SetUniform('ratio', _Ratio);
  _Shader.SetUniform('reflectionMatrix', GraphicsManager.Instance.ReflectionMatrix);
End;

Procedure Cloud.PreProcess;
Begin
  If (_NeedsShading) And (_Initialized) Then
  Begin
    Self.Illuminate(LightManager.Instance.Sun, True);
    _NeedsShading := False;
  End;
End;

End.