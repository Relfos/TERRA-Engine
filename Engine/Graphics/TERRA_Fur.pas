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
 * TERRA_Fur
 * Implements fur rendering (via shells)
 ***********************************************************************************************************************
}

Unit TERRA_Fur;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Image, TERRA_Texture, TERRA_Math, TERRA_Shader;

Const
  FurTextureSize = 128;

Type
  FurSettings = Record
		Pattern:TERRAString;
    Thickness:Single;
    Waviness:Single;
    Length:Single;
		Density:Single;
  End;

  Fur = Class(TERRAObject)
    Protected
      _Size:Integer;
      _Shader:Shader;

      _FurTexture:Texture;
      _Pattern:Texture;
      _FurLength:Single;
      _Thickness:Single;

    Public
      Constructor Create(Settings:FurSettings; Animated:Boolean);

      Procedure Bind;
      Procedure Render(DisplayList:Integer; LOD:Single);
      Procedure Unbind;

      Procedure Release;
  End;

Implementation
Uses TERRA_ResourceManager, TERRA_GraphicsManager, TERRA_Color, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

Constructor Fur.Create(Settings:FurSettings; Animated:Boolean);
Var
  I, K:Integer;
  X, Y:Integer;
  Buffer:Image;
  Len:Single;
Begin
  _FurLength := Settings.Length;
  _Thickness := Settings.Thickness;

  _Size := FurTextureSize;
  Buffer := Image.Create(_Size, _Size);

	// This is slightly different than above, we now do it so that
	// as we move to the outer layers, we have less and less strands of hair

  // Set all the pixels to the same colour, transparent black
  Buffer.FillRectangleByUV(0.0, 0.0, 1.0, 1.0, ColorNull);
  K := Trunc(Settings.Density * FurTextureSize * 96);

  For I:=1 To K Do
  Begin
	  X := Random(_Size);
		Y := Random(_Size);
    Len := (1.0 - Settings.Waviness) + (RandomFloat * Settings.Waviness);

    Buffer.SetPixel(X, Y, ColorCreate(1.0, 1.0, 1.0, Len));
     // More transparent as we get closer to the tip ,length is from 0 to 1, so the tip or outer layer is 1.
  End;

  _FurTexture := Texture.New(_Size, _Size);
  _FurTexture.Refresh(Buffer);
  ReleaseObject(Buffer);

  _Pattern := ResourceManager.Instance.GetTexture(Settings.Pattern);

  If Animated Then
    _Shader := ResourceManager.Instance.GetShader('animated_fur')
  Else
    _Shader := ResourceManager.Instance.GetShader('static_fur');
End;

Procedure Fur.Release;
Begin
  ReleaseObject(_FurTexture);
End;

Procedure Fur.Bind;
Begin
//  GraphicsManager.Instance.SetBlendState(True);
  glEnable(GL_ALPHA_TEST);        
//  glDepthMask(False);           

  Shader.Bind(_Shader);           
End;

Procedure Fur.Render(DisplayList:Integer; LOD:Single);
Const
  ShellLOD:Array[0..MaxLODLevel] Of Integer = (32, 28, 20, 15);
Var
  I, K:Integer;
  Delta:Single;
  LayerCount:Integer;
  Layer, Length, Scale, Step:Single;
Begin
  Shader.SetUniform('UVScale', _Thickness);

  If (Not _FurTexture.IsReady) Then
    _FurTexture.Update;

  // Get the texture layer which will be used for the different shells
  // Sort of like an onion!..different textures for the different layers :)

  Texture.Bind(_FurTexture, 0);
  Texture.Bind(_Pattern, 1);

  Shader.SetUniform('furTexture', 0);
  Shader.SetUniform('patternTexture', 1);


  K := Trunc(LOD);
  If (K<MaxLODLevel) Then
  Begin
    Delta := Frac(LOD);
    LayerCount := Trunc((ShellLOD[K] * (1.0 - Delta)) + (ShellLOD[Succ(K)] * Delta));
  End Else
    LayerCount := ShellLOD[K];

  Step := ShellLOD[0] / LayerCount;
  Layer := 0.0;

  // Render Our Shells
  For I:=0 To Pred(LayerCount) Do
	Begin
    Length := (_FurLength * (Layer/ShellLOD[0]));
    Layer := Layer + Step;
    Scale := 1 - (I / LayerCount); // 0 to 1

    Shader.SetUniform('furLength', Length);
    Shader.SetUniform('furScale', Scale);
    Shader.SetUniform('layer', layer);
    
    // Render Textured Fur
    glCallList(DisplayList);  
  End;
End;

Procedure Fur.Unbind;
Begin
  //GraphicsManager.Instance.SetBlendState(False);
  glDisable(GL_ALPHA_TEST);     
//  glDepthMask(True);
//  Texture.Bind(Nil, 1);
//  Texture.Bind(Nil, 0);

  //Shader.Bind(Nil);
End;

End.
