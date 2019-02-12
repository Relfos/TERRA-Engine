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
 * TERRA_LensFalre
 * Implements lens flare rendering via occlusion
 ***********************************************************************************************************************
}

Unit TERRA_LensFlare;

Interface
{$I terra.inc}
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
    TERRA_Utils, TERRA_Texture, TERRA_Lights, TERRA_Shader, TERRA_Vector3D;

Const
  LensFlareTextureSize = 128;

Type
  FlareVisibility = Record
    Position:Vector3D;
    Alpha:Single;
  End;

  LensFlare = Class(TERRAObject)
    Protected
      _FlareTexture:Texture;
      _SparkTexture:Texture;
      _Query:Cardinal;
      _Shader:Shader;

      Function GetOcclusion(X,Y:Single):Single;

      Procedure DrawFlare(X,Y, Radius:Single; ID:Integer);
      Procedure DrawSpark(X,Y, Radius:Single);

      Constructor Create;
    Public
      Procedure Release;

      Class Function Instance:LensFlare;

      Function GetVisibility(Source:Light):FlareVisibility;
      Procedure Render(Visibility:FlareVisibility);
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Image, TERRA_GraphicsManager, TERRA_ResourceManager, TERRA_Color;

Const
  RaySize = Trunc(LensFlareTextureSize * 0.45);
  RayParticleRadius = 1;

Var
  LensFlare_Instance:LensFlare = Nil;

Class Function LensFlare.Instance:LensFlare;
Begin
  If Not Assigned(LensFlare_Instance) Then
    LensFlare_Instance := LensFlare.Create;

  Result := LensFlare_Instance;
End;

Constructor LensFlare.Create;
Var
  Temp, Target:Image;
  X,Y,K:Integer;
  Radius, R, Dx, Dy, C, Angle:Single;
  Max, Min:Single;
  Fx, Fy:Single;
  Buffer:Array Of Array Of Single;
  Dest:PColor;

  Procedure DrawParticle;
  Var
    R2:Single;
    X,Y:Integer;
    Px, Py:Integer;
  Begin
    For y := -RayParticleRadius To RayParticleRadius Do
      For x := -RayParticleRadius To RayParticleRadius Do
      Begin
        r2 := Sqr(x) + Sqr(y);
        c := 1.0 - r2/Sqr(RayParticleRadius);
        c := c*c;
        c := c*c;
        Px := Trunc(x+fx);
        Py := Trunc(y+fy);
        C := Buffer[Px, Py] + C;
        Buffer[Px, Py] := C;

        If (C>Max) Then Max := C;
        If (C<Min) Then Min := C;
      End;
  End;

Begin
  _Shader := ResourceManager.Instance.GetShader('sky_flares');
  Temp := Image.Create(LensFlareTextureSize, LensFlareTextureSize);

  _FlareTexture := Texture.New(LensFlareTextureSize * 2, LensFlareTextureSize * 2);
  Target := Image.Create(_FlareTexture.Width, _FlareTexture.Height);

  Radius := (LensFlareTextureSize - 4) * 0.5;
  For K := 1 To 4 Do
  Begin
    Dest := Temp.Pixels;
    For Y:=0 To Pred(Temp.Height) Do
      For X:=0 To Pred(Temp.Width) Do
      Begin
        dx := Radius - x;
        dy := Radius - y;
        r := Sqrt( dx*dx + dy*dy ) / Radius;
        Case K Of
        1:  Begin
              C := 1.0 - R;
              C := C * C;
              C := C * (1.0-smoothstep(1-0.01, 1+0.01, R));
            End;
        2:  Begin
              C := r*r;
              C := c*c;
              C := c*c*c;
              C := C * (1.0-smoothstep(1-0.01, 1+0.01, R));
            End;
        3:  Begin
              c := r;
              C := C * (1.0-smoothstep(1-0.01, 1+0.01, R));
              C := C * 0.5;
            End;
        4:  Begin
              c := 1- Abs(r - 0.9)/ 0.1;
              If (c < 0) Then C := 0.0;
              C := c*c;
              C := c*c;
            End;
        End;

        Dest^ := ColorCreate(C, C, C, C);
        Inc(Dest);
      End;

    Target.BlitByUV(0.5 * (Pred(K) Mod 2), 0.5 * (Pred(K) Div 2), 0.0, 0.0, 1.0, 1.0, Temp);
    //mage.Save('lensflares'+IntToString(K)+'.png');
  End;
  //_FlareTexture.Image.Save('flare.png');
  _FlareTexture.Refresh(Target);
  _FlareTexture.Update;
  ReleaseObject(Temp);
  ReleaseObject(Target);

  _SparkTexture := Texture.New(LensFlareTextureSize, LensFlareTextureSize);
  Target := Image.Create(LensFlareTextureSize, LensFlareTextureSize);
  SetLength(Buffer, Target.Width, Target.Height);
  For K:=0 To 500 Do
  Begin
    //pick a random direction
    Angle := RandomFloat * 2 * Pi;
    dx := System.Cos(angle);
    dy := System.Sin(angle);

    // push particle along this path
    fx := Radius;
    fy := Radius;
    Y := (RaySize Div 2) + Random(RaySize Div 2);
    For X := 0 To Y Do
    Begin
      Fx := Fx + dx;
      Fy := Fy + dy;
      DrawParticle;
    End;
  End;

  Dest := Target.Pixels;
  For Y:=0 To Pred(Target.Height) Do
    For X:=0 To Pred(Target.Width) Do
    Begin
      C := (Buffer[X,Y] - Min) / (Max-Min);
      Dest^ := ColorCreate(C, C, C, C);
      Inc(Dest);
    End;

//  _SparkTexture.Image.Save('sparks.png');
  _SparkTexture.Refresh(Target);
  _SparkTexture.Update;
  ReleaseObject(Target);

  glGenQueries(1, @_Query); 
End;

Procedure LensFlare.Release;
Begin
  glDeleteQueries(1, @_Query);  

  ReleaseObject(_FlareTexture);

  ReleaseObject(_SparkTexture);

  LensFlare_Instance := Nil;
End;

Procedure LensFlare.DrawFlare(X,Y, Radius:Single; ID:Integer);
Var
  U,V:Integer;
  X1,Y1,X2,Y2:Single;
Begin
  U := ID Mod 2;
  V := ID Div 2;

  X1 := (X-Radius)/ GraphicsManager.Instance.Width;
  Y1 := (Y-Radius)/ GraphicsManager.Instance.Height;
  X2 := (X+Radius)/ GraphicsManager.Instance.Width;
  Y2 := (Y+Radius)/ GraphicsManager.Instance.Height;

	glBegin(GL_QUADS);
		glTexCoord2f(0.5 * Succ(U), 0.5 * V);
    glVertex2f(X2, Y1);

		glTexCoord2f(0.5 * Succ(U) ,0.5 * Succ(V));
    glVertex2f(X2, Y2);

		glTexCoord2f(0.5 * U, 0.5 * Succ(V));
    glVertex2f(X1, Y2);

		glTexCoord2f(0.5 * U, 0.5 * V);
    glVertex2f(X1, Y1);
	glEnd();  
End;

Procedure LensFlare.DrawSpark(X,Y, Radius:Single);
Var
  X1,Y1,X2,Y2:Single;
Begin
  X1 := (X-Radius)/ GraphicsManager.Instance.Width;
  Y1 := (Y-Radius)/ GraphicsManager.Instance.Height;
  X2 := (X+Radius)/ GraphicsManager.Instance.Width;
  Y2 := (Y+Radius)/ GraphicsManager.Instance.Height;

	glBegin(GL_QUADS);
		glTexCoord2f(1.0, 0.0);
    glVertex2f(X2, Y1);

		glTexCoord2f(1.0 , 1.0);
    glVertex2f(X2, Y2);

		glTexCoord2f(0.0, 1.0);
    glVertex2f(X1, Y2);

		glTexCoord2f(0.0, 0.0);
    glVertex2f(X1, Y1);
	glEnd();  
End;

Function LensFlare.GetOcclusion(X,Y:Single):Single;
Var
  Radius:Single;
  X1,Y1,X2,Y2:Single;
  FragmentCount:Integer;
Begin
  Radius := 40;

  X1 := (X-Radius)/ GraphicsManager.Instance.Width;
  Y1 := (Y-Radius)/ GraphicsManager.Instance.Height;
  X2 := (X+Radius)/ GraphicsManager.Instance.Width;
  Y2 := (Y+Radius)/ GraphicsManager.Instance.Height;

  glColorMask(False, False, False, False);  
  glBeginQuery( GL_SAMPLES_PASSED, _Query); 

	glBegin(GL_QUADS);
    glVertex3f(X2, Y1, -0.999);
    glVertex3f(X2, Y2, -0.999);
    glVertex3f(X1, Y2, -0.999);
    glVertex3f(X1, Y1, -0.999);
	glEnd();  

  glEndQuery( GL_SAMPLES_PASSED );        
  glColorMask(True, True, True, True);    

  glGetQueryObjectuiv(_Query, GL_QUERY_RESULT, @FragmentCount); 
  Result := FragmentCount / Sqr(Radius*2);
End;

Const
  FlareCount = 6;
  FlareSize = 100;
  FlareDistance:Array[1..FlareCount] Of Single = (1.0, 0.5, 0.125, -0.5, -0.25, -0.1818 );
  FlareScale:Array[1..FlareCount] Of Single = (1.0, 0.5, 1.0, 0.5, 0.25, 0.25);
  FlareID:Array[1..FlareCount] Of Integer = (1, 0, 3, 2, 3, 2);
//  0.33, 0.25

Function LensFlare.GetVisibility(Source:Light):FlareVisibility;
Var
  SunPos:Vector3D;
Begin
  Shader.Bind(Nil);

  SunPos := Source.Position;
  SunPos := GraphicsManager.Instance.ActiveViewport.ProjectPoint(SunPos);

  glMatrixMode(GL_PROJECTION);              
  glPushMatrix;                             
  glLoadIdentity();
	glOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0); 

  glMatrixMode(GL_MODELVIEW);               
  glPushMatrix;                             
	glLoadIdentity();               

  glEnable(GL_DEPTH_TEST);                  

  Result.Alpha := GetOcclusion(SunPos.X, SunPos.Y);
  Result.Position := SunPos;

  glMatrixMode(GL_PROJECTION);              
  glPopMatrix;                              
  glMatrixMode(GL_MODELVIEW);               
  glPopMatrix;                              
End;

Procedure LensFlare.Render(Visibility:FlareVisibility);
Var
  I:Integer;
  Cx, Cy:Single;
  Dx, Dy:Single;
Begin
  If (Visibility.Alpha<=0.0) Then
    Exit;

  Shader.Bind(_Shader);
  Shader.SetUniform('texture',0);

  glMatrixMode(GL_PROJECTION);                
  glPushMatrix;                               
  glLoadIdentity();
	glOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0); 

  glMatrixMode(GL_MODELVIEW);                 
  glPushMatrix;                               
  glLoadIdentity();

  Begin
    glDisable(GL_DEPTH_TEST);                       
    GraphicsManager.Instance.SetBlendMode(blendAdd);
    GraphicsManager.Instance.SetLighting(False);

    Texture.Bind(_FlareTexture);

    Cx := (GraphicsManager.Instance.Width * 0.5);
    Cy := (GraphicsManager.Instance.Height * 0.5);
    Dx := Cx - Visibility.Position.X;
    Dy := Cy - Visibility.Position.Y;

    For I:=1 To FlareCount Do
    Begin
      Shader.SetUniform('color', ColorCreate(1.0, 1.0, 1.0, (1.1 - FlareScale[I]) * 0.5 * Visibility.Alpha));
      DrawFlare(Visibility.Position.X + FlareDistance[I]   * Dx, Visibility.Position.Y + FlareDistance[I]   * Dy, FlareSize * FlareScale[I], FlareID[I]);
    End;

    Texture.Bind(_SparkTexture);
    Shader.SetUniform('color', ColorCreate(1.0, 1.0, 1.0, Visibility.Alpha * 0.5));
    DrawSpark(Visibility.Position.X + 0.33 * Dx, Visibility.Position.Y + 0.33 * Dy, FlareSize * 4);

    glEnable(GL_DEPTH_TEST);                                  
  End;

  glMatrixMode(GL_PROJECTION);            
  glPopMatrix;                            
  glMatrixMode(GL_MODELVIEW);             
  glPopMatrix;                            
End;

Initialization
Finalization
  ReleaseObject(LensFlare_Instance);
End.
