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
 * TERRA_Cubemap
 * Implements cube map textures
 ***********************************************************************************************************************
}
Unit TERRA_CubeMap;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Image, TERRA_FileManager, TERRA_Vector3D,
  {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

Const
  CubeFaceNames:Array[0..5] Of String = ( 'right', 'left', 'top','down', 'front', 'back');

Type
  CubeMapTexture = Class(TERRAObject)
    Protected
      _Handle:Cardinal;
      _Size:Cardinal;

      Procedure Init;

    Public
      Constructor Create(Size:Cardinal); Overload;
      Constructor Create(Const Name:TERRAString);  Overload;
      Destructor Destroy; Override;

      Class Procedure Bind(MyTexture:CubeMapTexture; Slot:Integer = 0);

      Procedure Save(Name:TERRAString);

	    Procedure Render(Position:Vector3D);

      Property Handle:Cardinal Read _Handle;
  End;

Implementation
Uses TERRA_Log, TERRA_GraphicsManager, TERRA_Camera, TERRA_Viewport;

// LTexture
Constructor CubeMapTexture.Create(Size:Cardinal);
Begin
  _Handle := 0;
  _Size := Size;
  Init;
End;

Constructor CubeMapTexture.Create(Const Name:TERRAString);
Var
  S, Ext:TERRAString;
  I, N:Integer;
  Src:Image;
  Info:ImageClassInfo;
Begin
  _Handle := 0;
  For I:=0 To 5 Do
  Begin
    N := 0;
    S := '';
    While (S='') And (N<GetImageExtensionCount()) Do
    Begin
      Info := GetImageExtension(N);
      S := FileManager.Instance.SearchResourceFile(Name + '_' + CubeFaceNames[I] + '.' + Info.Name);
      Inc(N);
    End;

    S := FileManager.Instance.SearchResourceFile(S);
    If (S='') Then
    Begin
      Log(logWarning, 'Cubemap', 'Could not load cubemap face '+CubeFaceNames[I]+' for '+Name);
      Continue;
    End;

    Src := Image.Create(S);
    If (I=0) Then
    Begin
      _Size := Src.Width;
      Init;
    End;

    glBindTexture(GL_TEXTURE_CUBE_MAP, _Handle);      
    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + I, 0, GL_RGBA8,	_Size, _Size, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Src.Pixels);  
    Src.Destroy;
  End;
End;

Destructor CubeMapTexture.Destroy;
Begin
  GraphicsManager.Instance.DeleteTexture(_Handle);
End;

Class Procedure CubeMapTexture.Bind(MyTexture:CubeMapTexture; Slot:Integer);
Begin
  glActiveTexture(GL_TEXTURE0 + Slot);  

  If (Assigned(MyTexture)) Then
  Begin
    {$IFDEF PC}
    glEnable(GL_TEXTURE_CUBE_MAP);
    {$ENDIF}                          
    glBindTexture(GL_TEXTURE_CUBE_MAP, MyTexture._Handle);  
  End Else
  Begin
    glBindTexture(GL_TEXTURE_CUBE_MAP, 0);    
  End;
End;

Procedure CubeMapTexture.Render(Position:Vector3D);
Var
  I:Integer;
  OldPos, OldView, Dir:Vector3D;
  OldFOV:Single;
  Src:Image;
  V:Viewport;
  Cam:Camera;
Begin
  OldPos := GraphicsManager.Instance.ActiveViewport.Camera.Position;
  OldView := GraphicsManager.Instance.ActiveViewport.Camera.View;
  OldFOV := GraphicsManager.Instance.ActiveViewport.Camera.FOV;

  GraphicsManager.Instance.ActiveViewport.Camera.SetPosition(Position);
  GraphicsManager.Instance.ActiveViewport.Camera.FOV := 90;

  For I:=0 To Pred(GraphicsManager.Instance.ViewportCount) Do
  Begin
    V := GraphicsManager.Instance.GetViewport(I);
    V.Active := False;
  End;

  Cam := Camera.Create('cubemap');
  V := Viewport.Create('cubemap', _Size, _Size);
  V.Camera.FOV := 90;
  V.Camera.SetPosition(Position);

  GraphicsManager.Instance.AddViewport(V);

  For I:=0 To 5 Do
  Begin
	  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT Or GL_STENCIL_BUFFER_BIT);

    Src := Image.Create(_Size, _Size);

    Case I Of
    0:  Dir := VectorCreate(1, 0, 0);
    1:  Dir := VectorCreate(0, 0, 1);
    2:  Dir := VectorCreate(-1, 0, 0);
    3:  Dir := VectorCreate(0, 0, -1);
    4:  Dir := VectorCreate(0, 0.9999, 0.0001);
    5:  Dir := VectorCreate(0, -0.9999, 0.0001);
    Else
    	Dir := VectorUp;
    End;
    Cam.SetView(Dir);

    GraphicsManager.Instance.RenderScene;
    glReadPixels(0, 0, GraphicsManager.Instance.Width, GraphicsManager.Instance.Height, GL_RGBA, GL_UNSIGNED_BYTE, Src.Pixels);

	  Src.Process(IMP_FlipVertical);
    Src.Save('cubemap_'+CubeFaceNames[I]+'.png');
    Src.Destroy;
 End;

  GraphicsManager.Instance.DeleteViewport(V);
  GraphicsManager.Instance.DeleteCamera(Cam);

  For I:=0 To Pred(GraphicsManager.Instance.ViewportCount) Do
  Begin
    V := GraphicsManager.Instance.GetViewport(I);
    V.Active := True;
  End;
End;

Procedure CubeMapTexture.Save(Name:TERRAString);
Var
  I:Integer;
  Src:Image;
Begin
  Exit;

  {$IFDEF PC}
  glEnable(GL_TEXTURE_CUBE_MAP);
  {$ENDIF}                            
  glBindTexture(GL_TEXTURE_CUBE_MAP, _Handle);  

    Src := Image.Create(_Size, _Size);
  For I:=0 To 5 Do
  Begin
// FIXME NO GLES SUPPORT    glGetTexImage(GL_TEXTURE_CUBE_MAP_POSITIVE_X + I, 0, GL_RGBA, GL_UNSIGNED_BYTE, Src.Pixels);
	  Src.Process(IMP_FlipVertical);
    Src.Save(Name+'_'+IntToString(I)+'.png');
  End;
  Src.Destroy;
End;

Procedure CubeMapTexture.Init;
Begin
  _Handle := GraphicsManager.Instance.GenerateTexture();
  glBindTexture(GL_TEXTURE_CUBE_MAP, _Handle);      

  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, GL_RGBA8,	_Size, _Size, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Nil);  
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, GL_RGBA8,	_Size, _Size, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Nil);  
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, GL_RGBA8,	_Size, _Size, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Nil);  
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, GL_RGBA8,	_Size, _Size, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Nil);  
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, GL_RGBA8,	_Size, _Size, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Nil);  
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, GL_RGBA8, _Size, _Size, 0,	GL_RGBA, GL_UNSIGNED_BYTE, Nil);  

  glTexParameterf(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);  
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);  
//  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);  

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);     
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);     
End;


{
	glViewport(0,0,CUBE_MAP_SIZE,CUBE_MAP_SIZE);
	glScissor(0,0,CUBE_MAP_SIZE,CUBE_MAP_SIZE);
	glEnable(GL_SCISSOR_TEST);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(90,1,1,2000);
	glMatrixMode(GL_MODELVIEW);
  for i := 0 to 5 do begin
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    case i of
    0:
      begin
        glRotatef( 90,0,1,0);
        glRotatef(180,1,0,0);
      end;
    1:
      begin
        glRotatef(-90,0,1,0);
        glRotatef(180,1,0,0);
      end;
    2: glRotatef(-90,1,0,0);
    3: glRotatef( 90,1,0,0);
    4: glRotatef(180,1,0,0);
    5: glRotatef(180,0,0,1);
    end;
    //
    glTranslatef(PosX,PosY,PosZ);
    glScalef(-1,-1,-1);
    RenderScene;

    glEnable(GL_TEXTURE_CUBE_MAP_ARB);
    glBindTexture(GL_TEXTURE_CUBE_MAP_ARB,CubeMap_TEX);
    glCopyTexSubImage2D(CubeMap_define[i],0,0,0,0,0,CUBE_MAP_SIZE,CUBE_MAP_SIZE);
    glDisable(GL_TEXTURE_CUBE_MAP_ARB);
  end;
	glScissor(0,0,SCREEN_WIDTH,SCREEN_HEIGHT);
	glDisable(GL_SCISSOR_TEST);

}
End.