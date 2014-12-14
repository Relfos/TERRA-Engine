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
 * TERRA_DDS
 * Implements DDS loader
 ***********************************************************************************************************************
}
Unit TERRA_DDS;

{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_IO, TERRA_FileUtils, TERRA_Utils, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Log,
  TERRA_Texture, TERRA_Resource;

Type
  DDSTexLevel = Record
  	Bytes:PByte;
    length:Cardinal;
    Width:Cardinal;
    Height:Cardinal;
  End;

  DDS_Texture = Class(Texture)
    Protected
      _Levels:Array Of DDSTexLevel;
      _LevelCount:Integer;
      _hasAlpha:Boolean;
      _Compressed:Boolean;
      _ExternalFormat:Integer;
      _type:Integer;

    Public

      Function Load(Source:Stream):Boolean; Override;

      Function Update:Boolean; Override;
      Function Unload:Boolean; Override;
  End;

Implementation

//  little-endian, of course
Const
  DDS_MAGIC = $20534444;

//  DDS_header.dwFlags
  DDSD_CAPS                   = $00000001;
  DDSD_HEIGHT                 = $00000002;
  DDSD_WIDTH                  = $00000004;
  DDSD_PITCH                  = $00000008;
  DDSD_PIXELFORMAT            = $00001000;
  DDSD_MIPMAPCOUNT            = $00020000;
  DDSD_LINEARSIZE             = $00080000;
  DDSD_DEPTH                  = $00800000;

//  DDS_header.sPixelFormat.dwFlags
  DDPF_ALPHAPIXELS            = $00000001;
  DDPF_FOURCC                 = $00000004;
  DDPF_INDEXED                = $00000020;
  DDPF_RGB                    = $00000040;

//  DDS_header.sCaps.dwCaps1
  DDSCAPS_COMPLEX             = $00000008;
  DDSCAPS_TEXTURE             = $00001000;
  DDSCAPS_MIPMAP              = $00400000;

//  DDS_header.sCaps.dwCaps2
  DDSCAPS2_CUBEMAP            = $00000200;
  DDSCAPS2_CUBEMAP_POSITIVEX  = $00000400;
  DDSCAPS2_CUBEMAP_NEGATIVEX  = $00000800;
  DDSCAPS2_CUBEMAP_POSITIVEY  = $00001000;
  DDSCAPS2_CUBEMAP_NEGATIVEY  = $00002000;
  DDSCAPS2_CUBEMAP_POSITIVEZ  = $00004000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ  = $00008000;
  DDSCAPS2_VOLUME             = $00200000;

  D3DFMT_DXT1:FileHeader     = 'DXT1';    //  DXT1 compression texture format
  D3DFMT_DXT2:FileHeader     = 'DXT2';    //  DXT2 compression texture format
  D3DFMT_DXT3:FileHeader     = 'DXT3';    //  DXT3 compression texture format
  D3DFMT_DXT4:FileHeader     = 'DXT4';    //  DXT4 compression texture format
  D3DFMT_DXT5:FileHeader     = 'DXT5';    //  DXT5 compression texture format

Type
    //  DDPIXELFORMAT
  DDS_pixelFormat = Packed Record
      dwSize:Cardinal;
      dwFlags:Cardinal;
      dwFourCC:FileHeader;
      dwRGBBitCount:Cardinal;
      dwRBitMask:Cardinal;
      dwGBitMask:Cardinal;
      dwBBitMask:Cardinal;
      dwAlphaBitMask:Cardinal;
  End;

  //  DDCAPS2
  DDS_CAPS2 = Packed Record
      dwCaps1:Cardinal;
      dwCaps2:Cardinal;
      dwDDSX:Cardinal;
      dwReserved:Cardinal;
  End;

  DDS_header = Packed Record
    dwMagic:Cardinal;
    dwSize:Cardinal;
    dwFlags:Cardinal;
    dwHeight:Cardinal;
    dwWidth:Cardinal;
    dwPitchOrLinearSize:Cardinal;
    dwDepth:Cardinal;
    dwMipMapCount:Cardinal;
    dwReserved1:Array[0.. 10] Of Cardinal;
    sPixelFormat:DDS_PixelFormat;
    sCaps:DDS_CAPS2;
    dwReserved2:Cardinal;
  End;

Function PF_IS_DXT1(pf:DDS_PixelFormat):Boolean;
Begin
  Result := (pf.dwFlags And DDPF_FOURCC<>0) And (pf.dwFourCC = D3DFMT_DXT1);
End;

Function PF_IS_DXT3(pf:DDS_PixelFormat):Boolean;
Begin
  Result := (pf.dwFlags And DDPF_FOURCC<>00) And (pf.dwFourCC = D3DFMT_DXT3);
End;

Function PF_IS_DXT5(pf:DDS_PixelFormat):Boolean;
Begin
  Result := (pf.dwFlags And DDPF_FOURCC<>0) And (pf.dwFourCC = D3DFMT_DXT5);
End;

Function PF_IS_BGRA8(pf:DDS_PixelFormat):Boolean;
Begin
  Result := (pf.dwFlags And DDPF_RGB<>0) And (pf.dwFlags And DDPF_ALPHAPIXELS<>0)
  And (pf.dwRGBBitCount = 32) And (pf.dwRBitMask = $ff0000)
  And (pf.dwGBitMask = $ff00) And (pf.dwBBitMask = $ff) And (pf.dwAlphaBitMask = $ff000000);
End;

Function PF_IS_BGR8(pf:DDS_PixelFormat):Boolean;
Begin
  Result :=
  ((pf.dwFlags And DDPF_ALPHAPIXELS<>0) And
  (pf.dwFlags And DDPF_ALPHAPIXELS=0) And
  (pf.dwRGBBitCount = 24) And
  (pf.dwRBitMask = $ff0000) And
  (pf.dwGBitMask = $ff00) And
  (pf.dwBBitMask = $ff))
End;

Function PF_IS_BGR5A1(pf:DDS_PixelFormat):Boolean;
Begin
  Result := (pf.dwFlags And DDPF_RGB<>0) And
   (pf.dwFlags And DDPF_ALPHAPIXELS<>0) And
   (pf.dwRGBBitCount = 16) And
   (pf.dwRBitMask = $00007c00) And
   (pf.dwGBitMask = $000003e0) And
   (pf.dwBBitMask = $0000001f) And
   (pf.dwAlphaBitMask = $00008000);
End;

Function PF_IS_BGR565(pf:DDS_PixelFormat):Boolean;
Begin
  Result := ((pf.dwFlags And DDPF_RGB<>0) And (pf.dwFlags And DDPF_ALPHAPIXELS=0)
   And (pf.dwRGBBitCount = 16) And (pf.dwRBitMask = $0000f800)
   And (pf.dwGBitMask = $000007e0) And (pf.dwBBitMask = $0000001f));
End;

Function PF_IS_INDEX8(pf:DDS_PixelFormat):Boolean;
Begin
  Result := ((pf.dwFlags And DDPF_INDEXED<>0) And (pf.dwRGBBitCount = 8));
End;

Type
  DdsLoadInfo = Packed Record
    compressed:Boolean;
    swap:Boolean;
    palette:Boolean;
    divSize:Cardinal;
    blockBytes:Cardinal;
    internalFormat:Cardinal;
    externalFormat:Cardinal;
    _type:Cardinal;
  End;

Const
 loadInfoDXT1:DdsLoadInfo = (Compressed: True; Swap:False; Palette:False; divSize:4; blockBytes:8; internalFormat: GL_COMPRESSED_RGBA_S3TC_DXT1);
 loadInfoDXT3:DdsLoadInfo = (Compressed: True; Swap:False; Palette:False; divSize:4; blockBytes:16; internalFormat: GL_COMPRESSED_RGBA_S3TC_DXT3);
 loadInfoDXT5:DdsLoadInfo = (Compressed: True; Swap:False; Palette:False; divSize:4; blockBytes:16; internalFormat: GL_COMPRESSED_RGBA_S3TC_DXT5);
 loadInfoBGRA8:DdsLoadInfo = (Compressed: False; Swap:False; Palette:False; divSize:1; blockBytes:4; internalFormat: GL_RGBA8; externalFormat:GL_BGRA; _type:GL_UNSIGNED_BYTE);
 loadInfoBGR8:DdsLoadInfo = (Compressed: False; Swap:False; Palette:False; divSize:1; blockBytes:3; internalFormat: GL_RGB8; externalFormat:GL_BGR; _type:GL_UNSIGNED_BYTE);
 loadInfoBGR5A1:DdsLoadInfo = (Compressed: False; Swap:True; Palette:False; divSize:1; blockBytes:2; internalFormat: GL_RGB5_A1; externalFormat:GL_BGRA; _type:GL_UNSIGNED_SHORT_1_5_5_5_REV);
 loadInfoBGR565:DdsLoadInfo = (Compressed: False; Swap:True; Palette:False; divSize:1; blockBytes:2; internalFormat: GL_RGB5; externalFormat:GL_RGB; _type:GL_UNSIGNED_SHORT_5_6_5);
 loadInfoIndex8:DdsLoadInfo = (Compressed: False; Swap:False; Palette:True; divSize:1; blockBytes:1; internalFormat: GL_RGB8; externalFormat:GL_BGRA; _type:GL_UNSIGNED_BYTE);

{ DDSTexture }
Function DDS_Texture.Load(Source: Stream): Boolean;
Var
  hdr:DDS_header ;
  s:Integer;
  i, x,y,Size:Integer;
  li:DdsLoadInfo;
Begin
  Result := False;

  //  DDS is so simple to read, too
  Source.Read(@hdr, sizeOf(DDS_header));
  If (hdr.dwMagic <> DDS_MAGIC ) Or (hdr.dwSize <> 124 ) Or (hdr.dwFlags And DDSD_PIXELFORMAT = 0) Or (hdr.dwFlags And DDSD_CAPS=0) Then
  Begin
    Log(logError, 'DDS', 'Invalid DDS image: '+ Source.Name);
    Exit;
  End;

  _Width := hdr.dwWidth;
  _Height := hdr.dwHeight;

  If (PF_IS_DXT1( hdr.sPixelFormat ) ) Then
    li := loadInfoDXT1
  Else
  If ( PF_IS_DXT3( hdr.sPixelFormat ) ) Then
    li := loadInfoDXT3
  Else
  If ( PF_IS_DXT5( hdr.sPixelFormat ) ) Then
    li := loadInfoDXT5
  Else
  If ( PF_IS_BGRA8( hdr.sPixelFormat ) ) Then
    li := loadInfoBGRA8
  Else
  If ( PF_IS_BGR8( hdr.sPixelFormat ) ) Then
    li := loadInfoBGR8
  Else
  If ( PF_IS_BGR5A1( hdr.sPixelFormat ) ) Then
    li := loadInfoBGR5A1
  Else
  If ( PF_IS_BGR565( hdr.sPixelFormat ) ) Then
    li := loadInfoBGR565
  Else
  If ( PF_IS_INDEX8( hdr.sPixelFormat ) ) Then
    li := loadInfoIndex8
  Else
  Begin
    Log(logError, 'DDS', 'Invalid pixel format in DDS: '+ Source.Name);
    Exit;
  End;

  //fixme: do cube maps later
  //fixme: do 3d later
  x := _Width;
  y := _Height;
  _Compressed := False;
  If (hdr.dwFlags And DDSD_MIPMAPCOUNT<>0) Then
    _LevelCount :=  hdr.dwMipMapCount
  Else
    _LevelCount := 1;

  If (li.compressed) Then
  Begin
    _Compressed := True;
    _Format := li.internalFormat;
    SetLength(_Levels, _LevelCount);
    For I := 0 To Pred(_LevelCount) Do
    Begin
      size := (IntMax( li.divSize, x ) Div li.divSize) * (IntMax( li.divSize, y ) Div li.divSize) * li.blockBytes;
      If (I=0) And (size <> hdr.dwPitchOrLinearSize ) Then
      Begin
        Log(logError, 'DDS', 'Invalid DDS dimensions.');
        Exit;
      End;

      GetMem(_Levels[I].Bytes, Size);
      Source.Read(_Levels[I].Bytes, size);
      _Levels[I].Width := X;
      _Levels[I].Height := Y;
      _Levels[I].length := Size;
      x := X Shr 1;
      y := Y Shr 1;
      If (X<1) Then X :=1;
      If (Y<1) Then Y :=1;
    End;

  End Else
  If ( li.palette ) Then
  Begin
    Log(logWarning, 'DDS', 'Formats with palette currently unsupported.');
    {//  currently, we unpack palette into BGRA
    //  I'm not sure we always get pitch...
    size: = hdr.dwPitchOrLinearSize * ySize;
    //  And I'm even less sure we don't get padding on the smaller MIP levels...

    _Format := li->externalFormat;
    cFormat = li->internalFormat;
    unsigned char * data = (unsigned char *)malloc( size );
    unsigned int palette[ 256 ];
    unsigned int * unpacked = (unsigned int *)malloc( size*sizeof( unsigned int ) );
    fread( palette, 4, 256, f );
    for( unsigned int ix = 0; ix < mipMapCount; ++ix )
    Begin
      fread( data, 1, size, f );
      for( unsigned int zz = 0; zz < size; ++zz )
      -  unpacked[ zz ] = palette[ data[ zz ] ];

      glPixelStorei( GL_UNPACK_ROW_LENGTH, y );
      glTexImage2D( GL_TEXTURE_2D, ix, li->internalFormat, x, y, 0, li->externalFormat, li->type, unpacked );
      gl->updateError();
      x = (x+1)>>1;
      y = (y+1)>>1;
      size = x * y * li->blockBytes;
    End;
    free( data );
    free( unpacked );}
  End Else
  Begin
    _ExternalFormat := li.externalFormat;
    _Format := li.internalFormat;
    _type := li._type;

    //fixme: how are MIP maps stored for 24-bit if pitch != ySize*3 ?
    For I:=0 To Pred(_LevelCount) Do
    Begin
      size := x * y * li.blockBytes;
      GetMem(_Levels[I].Bytes, Size);
      Source.Read(_Levels[I].Bytes, size);
      _Levels[I].Width := X;
      _Levels[I].Height := Y;
      _Levels[I].length := Size;
      x := X Shr 1;
      y := Y Shr 1;
      If (X<1) Then X :=1;
      If (Y<1) Then Y :=1;
    End;
  End;

  Result := True;
End;

Function DDS_Texture.Unload: Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_LevelCount) Do
  If (Assigned(_Levels[I].Bytes)) Then
  Begin
    FreeMem(_Levels[I].Bytes);
    _Levels[I].Bytes := Nil;
  End;
End;

Function DDS_Texture.Update: Boolean;
Var
  I, Err:Integer;
Begin
  Result := Inherited Update;

  If (_Handle=0) Then
  Begin
  	_Handle := GraphicsManager.Instance.GenerateTexture();  
  End;

  glActiveTexture(GL_TEXTURE0);
  {$IFDEF PC}
  glEnable(GL_TEXTURE_2D);
  {$ENDIF}
  glBindTexture(GL_TEXTURE_2D, _Handle);  

  glTexParameteri( GL_TEXTURE_2D, GL_GENERATE_MIPMAP, 0);

  If (_LevelCount > 1) Then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  Else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  For I:=0 To Pred(_LevelCount) Do
  Begin
    Log(logDebug,'DDS', 'Uploading miplevel '+IntToString(I));
    If _Compressed Then
	    glCompressedTexImage2D(GL_TEXTURE_2D, I, _Format, _Levels[I].Width, _Levels[I].Height, 0, _Levels[I].Length, _Levels[I].Bytes)
    Else
      glTexImage2D( GL_TEXTURE_2D, I, _Format, _Levels[I].Width, _Levels[I].Height, 0, _ExternalFormat, _type, _Levels[I].Bytes);

		{err := glGetError();
	  If (err <> GL_NO_ERROR) Then
    Begin
		  Log(logError, 'DDS', 'Error uploading compressed texture: '+IntToStr(Err));
  		Exit;
    End;}
  End;

  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, _LevelCount-1 );
  Result := True;
End;

Initialization
  RegisterTextureFormat(DDS_Texture, 'dds');
End.
