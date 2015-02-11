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
 * TERRA_PVR
 * Implements PVR loader
 ***********************************************************************************************************************
}
Unit TERRA_PVR;

{$I terra.inc}

Interface
Uses TERRA_Stream, TERRA_FileUtils, TERRA_Utils, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Log,
  TERRA_Texture, TERRA_Resource;

Type
  PVRTexLevel = Record
  	bytes:PByte;
    length:Cardinal;
    Width:Cardinal;
    Height:Cardinal;
  End;

  PVR_Texture = Class(Texture)
    Protected
      _Levels:Array Of PVRTexLevel;
      _LevelCount:Integer;
      _Bytes:PByte;
      _hasAlpha:Boolean;

    Public

      Function Load(Source:Stream):Boolean; Override;

      Function Update:Boolean; Override;
      Function Unload:Boolean; Override;
  End;

Const
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG   = $8C00;
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG   = $8C01;
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG  = $8C02;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG  = $8C03;

Implementation
Uses TERRA_Application;

Const
  PVR_TEXTURE_FLAG_TYPE_MASK	= $ff;

  gPVRTexIdentifier:FileHeader = 'PVR!';

	kPVRTextureFlagTypePVRTC_2 = 24;
	kPVRTextureFlagTypePVRTC_4 = 25;

Type
  PVRTexHeader = Record
	  headerLength:Cardinal;
	  height:Cardinal;
	  width:Cardinal;
	  numMipmaps:Cardinal;
	  flags:Cardinal;
	  dataLength:Cardinal;
	  bpp:Cardinal;
	  bitmaskRed:Cardinal;
	  bitmaskGreen:Cardinal;
	  bitmaskBlue:Cardinal;
	  bitmaskAlpha:Cardinal;
	  pvrTag:FileHeader;
	  numSurfs:Cardinal;
  End;

{ PVRTexture }

Function PVR_Texture.Load(Source: Stream):Boolean;
Var
  Header:PVRTexHeader;
  flags:Cardinal;
	dataOffset, dataSize:Cardinal;
	blockSize, widthBlocks, heightBlocks:Cardinal;
	formatFlags:Cardinal;
  dataLength:Cardinal;
  bpp, W,H:Integer;
Begin
  Result := False;
	dataLength := 0;
  dataOffset := 0;
  dataSize := 0;
	blockSize := 0;
  widthBlocks := 0;
  heightBlocks := 0;
	_width := 0;
  _height := 0;
  bpp := 4;
	_bytes := Nil;

	Source.Read(header, SizeOf(Header));

	If (header.pvrTag <> gPVRTexIdentifier) Then
  Begin
    Log(logDebug, 'PVR','Invalid tag: '+ header.pvrTag);
    Exit;
  End;

	flags := header.flags;
	formatFlags := flags And PVR_TEXTURE_FLAG_TYPE_MASK;

  _hasAlpha := header.bitmaskAlpha<>0;

  Case formatFlags Of
  kPVRTextureFlagTypePVRTC_4:
    Begin
			_Format := GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;
      Log(logDebug, 'PVR','PVRTC_4BPP RGBA');
		End;

  kPVRTextureFlagTypePVRTC_2:
    Begin
			_Format := GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG;
      Log(logDebug, 'PVR','PVRTC_2BPP RGBA');
		End;

    Else
    Begin
      RaiseError('UNKNOWN PVR FORMAT! '+IntToString(formatFlags));
      Exit;
		End;
  End;

  _width := header.width;
  _height := header.height;

  Log(logDebug, 'PVR', 'PVR Width:'+IntToString(_Width)+' Height:'+IntToString(_Height));

  dataLength := header.dataLength;
  GetMem(_Bytes, dataLength);
  Source.Read(_Bytes^, dataLength);

  _LevelCount := 0;
  W := _Width;
  H := _Height;

	// Calculate the data size for each texture level and respect the minimum number of blocks
  While (dataOffset < dataLength) Do
	Begin
    Inc(_LevelCount);
    SetLength(_Levels, _LevelCount);

		If (formatFlags = kPVRTextureFlagTypePVRTC_4) Then
    Begin
      blockSize := 4 * 4; // Pixel by pixel block size for 4bpp
			widthBlocks := W Div 4;
			heightBlocks := H Div 4;
			bpp := 4;
    End Else
		Begin
      blockSize := 8 * 4; // Pixel by pixel block size for 2bpp
			widthBlocks := W Div 8;
			heightBlocks := H Div 4;
			bpp := 2;
    End;

		// Clamp to minimum number of blocks
		if (widthBlocks < 2) Then
		  widthBlocks := 2;
    if (heightBlocks < 2) Then
		  heightBlocks := 2;

    dataSize := widthBlocks * heightBlocks * ((blockSize  * bpp) Div 8);

    _Levels[Pred(_LevelCount)].Length := dataSize;
    _Levels[Pred(_LevelCount)].Bytes := PByte(Cardinal(_Bytes)+dataOffset);

    _Levels[Pred(_LevelCount)].Width := W;
    _Levels[Pred(_LevelCount)].Height := H;

		Inc(dataOffset, dataSize);

    W := W Shr 1;
		H := H Shr 1;
    If (W<1) Then W := 1;
    If (H<1) Then H := 1;

    Result := True;
  End;
End;

Function PVR_Texture.Unload:Boolean;
Begin
  If Assigned(_Bytes) Then
  Begin
    FreeMem(_Bytes);
    _Bytes := Nil;
  End;

  _Status := rsUnloaded;
  Result := True;
End;

Function PVR_Texture.Update:Boolean;
Var
  I, Err:Integer;
Begin
  Inherited;

  Result := False;

  If (_Handle=0) Then
  	_Handle := GraphicsManager.Instance.GenerateTexture();  

  glActiveTexture(GL_TEXTURE0);
  {$IFDEF PC}
  glEnable(GL_TEXTURE_2D);
  {$ENDIF}
  glBindTexture(GL_TEXTURE_2D, _Handle);  

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  If (_LevelCount > 1) Then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  Else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  For I:=0 To Pred(_LevelCount) Do
  Begin
    Log(logDebug,'PVR', 'Uploading miplevel '+IntToString(I));
	  glCompressedTexImage2D(GL_TEXTURE_2D, I, _Format, _Levels[I].Width, _Levels[I].Height, 0, _Levels[I].Length, _Levels[I].Bytes);

		{err := glGetError();
	  If (err <> GL_NO_ERROR) Then
    Begin
		  Log(logError, 'PVR', 'Error uploading compressed texture. '+IntToString(_Levels[I].Width)+'x'+IntToString(_Levels[I].Height)+' ,Err:'+IntToString(Err));
  		Exit;
    End;}
  End;

  Result := True;
End;

Initialization
  RegisterTextureFormat(PVR_Texture, 'pvr');
End.
