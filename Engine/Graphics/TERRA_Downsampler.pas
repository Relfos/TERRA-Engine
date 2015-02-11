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
 * TERRA_Downsampler
 * Implements a framebuffer downsampler via shaders
 ***********************************************************************************************************************
}
Unit TERRA_Downsampler;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Shader, TERRA_RenderTarget, TERRA_Math, TERRA_Application, TERRA_Utils;

Type
  RenderTargetDownSampler = Class(TERRAObject)
    Protected
      _Name:TERRAString;

	    _Textures: Array Of RenderTarget;
      _TextureCount:Integer;

	    // Create the downsampler
    	Procedure Init(Width, Height, Format:Integer);

    	// Free memory
	    Procedure Release();

    Public
	    Constructor Create(Const Name:TERRAString; Width, Height:Integer);
	    Destructor Destroy; Override;


	    // Downsample a framebuffer using the shader
      // Return value is index of smallest texture downsampled
	    Function Update(Source:RenderTarget; Downsampler:Shader; Target:Integer = 0; Limit:Integer=0):Integer;

	    // Number of render texture used
      Property TextureCount:Integer Read _TextureCount;

      // Get a downsampled render texture
      Function GetRenderTexture(Index:Integer):RenderTarget;
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_GraphicsManager, TERRA_Texture
{$IFDEF FRAMEBUFFEROBJECTS},TERRA_FrameBufferObject{$ENDIF};

Constructor RenderTargetDownSampler.Create(Const Name:TERRAString; Width, Height:Integer);
Begin
  Self._Name := Name;
  Self.Init(Width, Height, {$IFDEF FRAMEBUFFEROBJECTS}FBO_COLOR8{$ELSE}0{$ENDIF});
End;

Destructor RenderTargetDownSampler.Destroy;
Begin
  Self.Release;
End;

Procedure RenderTargetDownSampler.Init(Width, Height, Format:Integer);
Var
  I, X,Y:Integer;
  Nx, Ny:Integer;
Begin
	// Calculate the number of texture if we want to get a 1x1 texture
	X := Width;
	Y := Height;

	Nx := 0;
	While (X>1) Do
	Begin
		Inc(nx);
		X := Trunc(X /2.0);
	End;

	Ny := 0;
	While (Y>1) Do
	Begin
    Inc(NY);
		Y := Trunc(Y/2.0);
	End;

	_TextureCount := IntMax(Nx, Ny);
  SetLength(_Textures, _TextureCount);

	// Create FrameBuffer Objects
	for I :=0 To Pred(_TextureCount) Do
	Begin
		Width := Width Div 2;
		Height := Height Div 2;
    {$IFDEF FRAMEBUFFEROBJECTS}
		_Textures[I] := FrameBufferObject.Create(_Name+'_target'+IntToString(I), Width, Height, Format, False, False, False, 1);
    {$ELSE}
    _Textures[I] := Nil;
    {$ENDIF}
	End;
End;

Procedure RenderTargetDownSampler.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TextureCount) Do
    _Textures[I].Destroy;

  _TextureCount := 0;
  SetLength(_Textures, 0);
End;

Function RenderTargetDownSampler.Update(Source:RenderTarget; Downsampler:Shader; Target:Integer = 0; Limit:Integer=0):Integer;
Var
  N, I:Integer;
  prevRT:RenderTarget;
  curRT:RenderTarget;
  W,H:Single;
Begin
  Result := 0;

	// Set max number of textures
  If (Limit <=0) Then
    N := _TextureCount
  Else
	  N := IntMin(Limit, _TextureCount);

	// Generate all down-sampled render texture
	For I:=0 To Pred(N) Do
	Begin
		curRt := _Textures[i];
    If (I=0) Then
		  prevRt := Source
    Else
      prevRT := _Textures[Pred(i)];

		// Render on current fbo
		curRt.BeginCapture();
		glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);  

		// Use previous render texture as input texture
		prevRt.Bind(0);

		// User shader to render
    ShaderManager.Instance.Bind(Downsampler);
    W := curRt.Width;
    H := curRt.Height;
    Downsampler.SetUniform('width', W);
    Downsampler.SetUniform('height', H);
    Downsampler.SetUniform('texture', 0);

		// Draw quad on the screen
    GraphicsManager.Instance.SetBlendMode(blendNone);
		GraphicsManager.Instance.DrawFullscreenQuad(DownSampler, 0, 0, 1.0, 1.0);
    curRt.EndCapture();

    Result := I;

    {IF application.Instance.Keys[Ord('T')] Then
      curRt.GetImage(0).Save('ds'+IntToString(I)+'.png');}
	End;

//  GraphicsManager.Instance.ActiveViewport.Restore();
End;


Function RenderTargetDownSampler.GetRenderTexture(Index:Integer):RenderTarget;
Begin
  If (Index<0) Or (Index>=_textureCount) Then
    Result := Nil
  Else
    Result := _Textures[Index];
End;

End.
