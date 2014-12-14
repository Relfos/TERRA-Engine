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
 * TERRA_VolumeTexture
 * Implements a 3D Volume Textue
 ***********************************************************************************************************************
}
Unit TERRA_VolumeTexture;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_IO, TERRA_Color;

Const
  MinTextureSize  = 2;

Type
  VolumeTexture = Class
	  Protected
		  _Width:Cardinal;
		  _Height:Cardinal;
		  _Depth:Cardinal;
      _Size:Integer;

      _Volume:Array Of Color;
      _Handle:Cardinal;
      _Format:Cardinal;

    Public
      Wrap:Boolean;

		  Constructor Create(Width,Height,Depth:Cardinal);
      Destructor Destroy;

      Class Procedure Bind(Texture:VolumeTexture; Slot:Integer);

      Procedure SetVolume(X,Y,Z:Integer; Color:Color);
      Function GetVolume(X,Y,Z:Integer):Color;

		  Function Load(Source:Stream):Boolean;
		  Function Save(Dest:Stream):Boolean;

      Function Update:Boolean;
      Procedure Unload;

      Property Handle:Cardinal Read _Handle;
  End;

Implementation
Uses TERRA_GraphicsManager;

// VolumeTexture
Constructor VolumeTexture.Create(Width,Height,Depth:Cardinal);
Begin
	Self._Width := Width;
	Self._Height := Height;
	Self._Depth := Depth;
	_Size := _Width * _Height * _Depth;

  SetLength(_Volume, _Size);
  _Size := _Size * 4;

  Wrap := False;
End;

Function VolumeTexture.Load(Source:Stream):Boolean;
Begin
  Unload;

	Source.Read(_Width, SizeOf(_Width));
	Source.Read(_Height, SizeOf(_Height));
	Source.Read(_Depth, SizeOf(_Depth));
	_Size := _Width * _Height * _Depth;
	SetLength(_Volume, _Size);
  _Size := _Size * 4;
	Source.Read(_Volume[0], _Size);
  Result := True;
End;

Function VolumeTexture.Save(Dest:Stream):Boolean;
Begin
	_Size := _Width * _Height * _Depth;
  _Size := _Size * 4;

	Dest.Write(_Width, SizeOf(_Width));
	Dest.Write(_Height, SizeOf(_Height));
	Dest.Write(_Depth, SizeOf(_Depth));
	Dest.Write(_Volume[0], _Size);
	Result := True;
End;

Destructor VolumeTexture.Destroy;
Begin
  Unload;
End;

Procedure VolumeTexture.Unload;
Begin
  GraphicsManager.Instance.DeleteTexture(_Handle);
  SetLength(_Volume, 0);
End;

Function VolumeTexture.Update:Boolean;
Begin
  Result := False;
  If Length(_Volume)<=0 Then
    Exit;

  If (_Handle=0) Then
  	_Handle := GraphicsManager.Instance.GenerateTexture();

  _Format := GL_RGBA8;
  _Size := _Width * _Height * _Depth * 4;
  glActiveTexture(GL_TEXTURE0);           
  glBindTexture(GL_TEXTURE_3D, _Handle);  

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR); 
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER,GL_LINEAR);                

  If (Wrap) Then
  Begin
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_REPEAT);       
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_REPEAT);       
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_REPEAT);       
  End Else
  Begin
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);  
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);  
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);  
  End;

  glTexParameteri(GL_TEXTURE_3D, GL_GENERATE_MIPMAP, 1);  

  glTexImage3D(GL_TEXTURE_3D, 0, _Format, _Width, _Height, _Depth, 0, GL_RGBA, GL_UNSIGNED_BYTE, _Volume);
  Result := True;
End;

Class Procedure VolumeTexture.Bind(Texture:VolumeTexture; Slot:Integer);
Begin
  glActiveTexture(GL_TEXTURE0 + Slot);  

  If Assigned(Texture) Then
  Begin
    If (Texture._Handle = 0) Then
      Texture.Update;

    If (Texture._Handle>0) Then
    Begin
      glEnable(GL_TEXTURE_3D);  
      glBindTexture(GL_TEXTURE_3D, Texture._Handle);  
    End;
  End Else
  Begin
    glBindTexture(GL_TEXTURE_3D, 0);  
    glDisable(GL_TEXTURE_3D); 
  End;
End;

Procedure VolumeTexture.SetVolume(X,Y,Z:Integer; Color:Color);
Begin
  _Volume[Z*(_Depth * _Height)+ Y * _Height + X] := Color;
End;

Function VolumeTexture.GetVolume(X,Y,Z:Integer):Color;
Begin
  Result := _Volume[Z*(_Depth * _Height)+ Y * _Height + X];
End;

End.