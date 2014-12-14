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
 * TERRA_TextureTarget
 * Implements a generic texture target
 ***********************************************************************************************************************
}
Unit TERRA_TextureTarget;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Color, TERRA_Image, TERRA_RenderTarget, TERRA_Vector2D, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

Type
  TextureRenderTarget = Class(RenderTarget)
    Protected
	    Procedure InitCapture(Flags:Cardinal);  Override;

    Public
      Constructor Create(Name:AnsiString; Width, Height:Integer);
      Destructor Destroy; Override;

	    // Use it as a texture
	    Procedure Bind(Slot:Integer); Override;

	    // Render to this target
	    Procedure EndCapture;  Override;
  End;

Implementation
Uses TERRA_GraphicsManager, TERRA_Resource, TERRA_Math;

Constructor TextureRenderTarget.Create(Name:AnsiString; Width, Height: Integer);
Begin
  Self._Location := '';
  Self._Name := Name;
  Self._Size := Width * Height * 4;
  Self._Status := rsReady;

  _Width := Width;
  _Height := Height;

  _Ratio := VectorCreate2D(1.0, 1.0);

  _FrameCount := 1;
  SetLength(_Handles, 1);
  _Handles[0] := GraphicsManager.Instance.GenerateTexture();
  glBindTexture(GL_TEXTURE_2D, _Handles[0]);

//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);  
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);  
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);        
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);        

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, _Width, _Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Nil);  
  glBindTexture(GL_TEXTURE_2D, 0);  
End;

Destructor TextureRenderTarget.Destroy;
Begin
  GraphicsManager.Instance.DeleteTexture(_Handles[0]);
End;

Procedure TextureRenderTarget.InitCapture(Flags: Cardinal);
Begin
	GraphicsManager.Instance.SetViewArea(0,0, _Width, _Height);					// Set Our Viewport (Match Texture Size)

  If (Flags<>0) Then
  Begin
    glClearColor(_ClearColor.R/255.0, _ClearColor.G/255.0, _ClearColor.B/255.0, _ClearColor.A/255.0);
    glClear(Flags);
  End;

  _Active := True;
End;

Procedure TextureRenderTarget.EndCapture;
Begin
	glBindTexture(GL_TEXTURE_2D, _Handles[0]);			// Bind To The Blur Texture

	// Copy viewPort To target texture
	glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 0, 0, _Width, _Height, 0);

	glBindTexture(GL_TEXTURE_2D, 0);

  _Active := False;
End;

Procedure TextureRenderTarget.Bind(Slot: Integer); {FIXME}
Begin
	glActiveTexture(GL_TEXTURE0 + Slot);
  {$IFDEF PC}
	glEnable(GL_TEXTURE_2D);
  {$ENDIF}                  
	glBindTexture(GL_TEXTURE_2D, _Handles[0]);
End;

End.
