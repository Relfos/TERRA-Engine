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
  TERRA_String, TERRA_Renderer, TERRA_Texture, TERRA_Math, TERRA_Application, TERRA_Utils, TERRA_Resource;

Type
  RenderTargetSampler = Class(TERRAObject)
    Protected
      _Name:TERRAString;

	    _Targets:Array Of RenderTargetInterface;
      _Textures:Array Of Texture;
      _TargetCount:Integer;

      _ResultIndex:Integer;

    	Procedure Init(Width, Height:Integer; PixelSize:PixelSizeType); Virtual; Abstract;

    	// Free memory
	    Procedure Clear();

    Public
	    Constructor Create(Const Name:TERRAString; Width, Height:Integer; PixelSize:PixelSizeType);

	    Procedure Release; Override;

	    Procedure Update(Source:Texture; DownsamplerShader:ShaderInterface; First, Count:Integer); Virtual; Abstract;

	    // Number of render texture used
      Property TextureCount:Integer Read _TargetCount;

      // Get a downsampled render texture
      Function GetRenderTexture(Index:Integer):Texture;

      Function GetResult():Texture;
  End;

  RenderTargetDownSampler = Class(RenderTargetSampler)
    Protected
	    // Create the downsampler
    	Procedure Init(Width, Height:Integer; PixelSize:PixelSizeType); Override;

    Public
	    // Downsample a framebuffer using the shader
      // Return value is index of smallest texture downsampled
	    Procedure Update(Source:Texture; DownsamplerShader:ShaderInterface; First, Count:Integer); Override;
  End;

  RenderTargetBouncer = Class(RenderTargetSampler)
    Protected
	    // Create the downsampler
    	Procedure Init(Width, Height:Integer; PixelSize:PixelSizeType); Override;

    Public
	    // Downsample a framebuffer using the shader
      // Return value is index of smallest texture downsampled
	    Procedure Update(Source:Texture; DownsamplerShader:ShaderInterface; First, Count:Integer); Override;
  End;

Implementation
Uses TERRA_GraphicsManager, TERRA_InputManager, TERRA_Color;

{ RenderTargetSampler }
Constructor RenderTargetSampler.Create(Const Name:TERRAString; Width, Height:Integer; PixelSize:PixelSizeType);
Begin
  Self._Name := Name;
  Self.Init(Width, Height, PixelSize); // {$IFDEF FRAMEBUFFEROBJECTS}FBO_COLOR8{$ELSE}0{$ENDIF}); BIBI
End;

Function RenderTargetSampler.GetRenderTexture(Index:Integer):Texture;
Begin
  If (Index<0) Or (Index>=_TargetCount) Then
    Result := Nil
  Else
  Begin
    If (_Textures[Index] = Nil) Then
    Begin
      _Textures[Index] := Texture.Create(rtDynamic, Self._Name + '_rt'+IntToString(Index));
      _Textures[Index].InitFromSurface(_Targets[Index]);
      _Textures[Index].WrapMode := wrapNothing;
    End;

    Result := _Textures[Index];
  End;
End;

Procedure RenderTargetSampler.Release();
Begin
  Self.Clear();
End;


Procedure RenderTargetSampler.Clear();
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TargetCount) Do
  Begin
    ReleaseObject(_Textures[I]);
    ReleaseObject(_Targets[I]);
  End;

  _TargetCount := 0;
  SetLength(_Targets, 0);
  SetLength(_Textures, 0);
End;

Function RenderTargetSampler.GetResult: Texture;
Begin
  Result := Self.GetRenderTexture(_ResultIndex);
End;

{ RenderTargetDownSampler }
Procedure RenderTargetDownSampler.Init(Width, Height:Integer; PixelSize:PixelSizeType);
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

	_TargetCount := IntMax(Nx, Ny);
  SetLength(_Targets, _TargetCount);
  SetLength(_Textures, _TargetCount);

	// Create FrameBuffer Objects
	For I :=0 To Pred(_TargetCount) Do
	Begin
		Width := Width Div 2;
		Height := Height Div 2;
		_Targets[I] := GraphicsManager.Instance.Renderer.CreateRenderTarget();
    _Targets[I].Generate({_Name+'_target'+IntToString(I), }Width, Height, False, PixelSize, 1, False, False);

    _Targets[I].BackgroundColor := ColorRed;
	End;
End;

Procedure RenderTargetDownSampler.Update(Source:Texture; DownsamplerShader:ShaderInterface; First, Count:Integer);
Var
  N, I:Integer;
  curRT:RenderTargetInterface;
  Graphics:GraphicsManager;
Begin
  _ResultIndex := 0;

  Graphics := GraphicsManager.Instance;

	// Set max number of textures
  If (Count<0) Then
    N := _TargetCount
  Else
	  N := IntMin(Count, _TargetCount);

	// Generate all down-sampled render texture
	For I:=First To Pred(N) Do
	Begin
		curRt := _Targets[i];

    If (I>0) Then
      Source := Self.GetRenderTexture(Pred(i));

		// Render on current fbo
		curRt.BeginCapture(clearAll);

		// Use previous render texture as input texture
    Source.Bind(0);

		// User shader to render
    Graphics.Renderer.BindShader(DownsamplerShader);
    DownsamplerShader.SetFloatUniform('dx', 1 / curRt.Width);
    DownsamplerShader.SetFloatUniform('dy', 1 / curRt.Height);
    DownsamplerShader.SetIntegerUniform('texture', 0);

		// Draw quad on the screen
    Graphics.Renderer.SetBlendMode(blendNone);
		Graphics.DrawFullscreenQuad(DownsamplerShader, 0, 0, 1.0, 1.0);

    curRt.EndCapture();

    _ResultIndex := I;

    {IF InputManager.Instance.Keys.IsDown(Ord('T')) Then
      curRt.GetImage().Save('ds'+IntToString(I)+'.png');}
	End;

//  Graphics.ActiveViewport.Restore();
End;



{ RenderTargetBouncer }
Procedure RenderTargetBouncer.Init(Width, Height: Integer; PixelSize: PixelSizeType);
Var
  I:Integer;
Begin
	_TargetCount := 2;
  SetLength(_Targets, _TargetCount);
  SetLength(_Textures, _TargetCount);

	// Create FrameBuffer Objects
	For I :=0 To Pred(_TargetCount) Do
	Begin
		_Targets[I] := GraphicsManager.Instance.Renderer.CreateRenderTarget();
    _Targets[I].Generate(Width, Height, False, PixelSize, 1, False, False);
    _Targets[I].BackgroundColor := ColorRed;
	End;
End;

Procedure RenderTargetBouncer.Update(Source:Texture; DownsamplerShader: ShaderInterface; First, Count: Integer);
Var
  N, CurIndex:Integer;
  curRT:RenderTargetInterface;
  Graphics:GraphicsManager;
  Tex:Texture;
Begin
  _ResultIndex := 0;

  Graphics := GraphicsManager.Instance;

  Tex := Self.GetRenderTexture(0);
  If (Source = Tex) Then
      CurIndex := 1
    Else
      CurIndex := 0;

	// Generate all down-sampled render texture
	For N:=1 To Count Do
	Begin
    curRt := _Targets[curIndex];

		// Render on current fbo
		curRt.BeginCapture(clearAll);

		// Use previous render texture as input texture
    Source.Bind(0);

		// User shader to render
    Graphics.Renderer.BindShader(DownsamplerShader);
    DownsamplerShader.SetFloatUniform('dx', 1 / curRt.Width);
    DownsamplerShader.SetFloatUniform('dy', 1 / curRt.Height);
    DownsamplerShader.SetIntegerUniform('texture', 0);

		// Draw quad on the screen
    Graphics.Renderer.SetBlendMode(blendNone);
		Graphics.DrawFullscreenQuad(DownsamplerShader, 0, 0, 1.0, 1.0);

    curRt.EndCapture();

    _ResultIndex := CurIndex;

    IF InputManager.Instance.Keys.IsDown(Ord('T')) Then
      curRt.GetImage().Save('ds'+IntToString(curIndex)+'.png');

    Tex := Self.GetRenderTexture(0);
    If (Source = Tex) Then
    Begin
      Source := Self.GetRenderTexture(1);
      curIndex := 0;
    End Else
    Begin
      Source := Tex;
      curIndex := 1;
    End;
	End;

//  Graphics.ActiveViewport.Restore();
End;

End.
