Unit TERRA_TextureImport;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_IO, TERRA_Application, TERRA_Image, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_PNG, TERRA_JPG, TERRA_TGA, TERRA_Color;

Type
  TextureImportSettings = Object
    preprocess:Boolean;
    compressTexture:Boolean;
    downScale:Boolean;
    mipMapChain:Boolean;
    globalPalette:Boolean;
    Path:AnsiString;

    Procedure DisplayHelp;
    Procedure Load(S:AnsiString);
  End;

Function ImportTexture(SourceFile, TargetDir:AnsiString; TargetPlatform:Integer; Settings:TextureImportSettings):AnsiString;

Implementation
Uses TERRA_Texture, TERRA_ColorTable, TERRA_INI, TERRA_Math, TERRA_ImportTools, SysUtils;

Const
	DS_RGB32_A3   = 1;
	DS_RGB4       = 2;
	DS_RGB16      = 3;
	DS_RGB256     = 4;
	DS_COMPRESSED = 5;
	DS_RGB8_A5    = 6;
	DS_RGBA       = 7;

Function ImportTexture(SourceFile, TargetDir:AnsiString; TargetPlatform:Integer; Settings:TextureImportSettings):AnsiString;
Var
  Handle, Format, Size:Integer;
  Source:Image;
  Colors:Array[0..256] Of Color;
  ColorCount:Integer;
  HasAlpha:Boolean;
  HasColorKey:Boolean;
  P:PColor;
  I,J,Level,Count:Integer;
  Found:Boolean;

  Data:Pointer;
  W,H:Integer;
  Header:FileHeader;
  DestFile:AnsiString;
  Dest:Stream;

  RAW:Array Of Word;
  MyColorQuantizer:ColorQuantizer;
  ColorTable:RGBQuadArray;
  C:Color;

  Function GetPaletteIndex(MyColor:Color; Size:Integer):Byte;
  Var
    I:Integer;
    Dist, Min:Single;
  Begin
    If (MyColor.A=0) Then
    Begin
      Result:=0;
      Exit;
    End;

    If Settings.globalPalette Then
    Begin
      Result := ColorRGB32To8(MyColor);
      Exit;
    End;

    Min:=99999;
    For I:=0 To Pred(Size) Do
    Begin
      Dist:=Sqrt(Sqr(ColorTable[i].R-MyColor.R) + Sqr(ColorTable[i].G-MyColor.G) + Sqr(ColorTable[i].B-MyColor.B));
      If (Dist<Min) Then
      Begin
        Min := Dist;
        Result := Succ(I);
      End;
    End;
  End;

Begin
  DestFile := TargetDir + '\' +GetFileName(SourceFile, True) + '.tex';

  If (Not AssetModified(SourceFile, DestFile)) Then
  Begin
    {$IFNDEF EXCLUDELOG}
    WriteLn('Skipping ',SourceFile,'...[Not modified]');
    {$ENDIF}
    Result := '';
    Exit;
  End;

  WriteLn('Importing ',SourceFile,'...');
  Result := DestFile;

  HasAlpha := False;
  HasColorKey := False;

  Source := Image.Create(SourceFile);

  W := NearestPowerOfTwo(Source.Width);
  H := NearestPowerOfTwo(Source.Height);
  If (W<>Source.Width) Or (H<>Source.Height) Then
    Settings.downScale := True;

  If (Settings.downScale) And ((W<>Source.Width) Or (H<>Source.Height))  Then
  Begin
    Write(#9, 'Resizing texture to ',W,'x',H,'...');

    Source.Resize(W, H);

    WriteLn('ok');
  End;

  Count := Source.Width * Source.Height;
  P := Source.Pixels;
  ColorCount := 0;
  While Count>0 Do
  Begin
    If P.A=0 Then
      HasColorKey := True
    Else
    If P.A<255 Then
      HasAlpha := True;

    Found := False;
    For I:=0 To Pred(ColorCount) Do
    If (Cardinal(Colors[I]) = Cardinal(P^)) Then
    Begin
      Found := True;
      Break;
    End;

    If (Not Found) And (ColorCount>255) Then
    Begin
      Colors[ColorCount] := P^;
      Inc(ColorCount);
    End;

    Dec(Count);
    Inc(P);
  End;

  If (TargetPlatform = osWindows) Or (TargetPlatform = osLinux) Then
  Begin
    If (Settings.compressTexture) Then
    Begin
      If (HasAlpha) Or (HasColorKey) Then
      Begin
  			Format := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT; //GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT;
        Write(#9, 'Converting texture to DXT5 format...');
  		End Else
      Begin
  			Format := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;//GL_COMPRESSED_SRGB_S3TC_DXT1_EXT;
        Write(#9, 'Converting texture to DXT1 format...');
      End;
    End Else
    Begin
  	  Format := GL_BGRA;

      Write(#9, 'Converting texture to BGRA format...');
    End;

  	Handle := GraphicsManager.Instance.GenerateTexture();  
    glActiveTexture(GL_TEXTURE0);  
    glBindTexture(GL_TEXTURE_2D, Handle);  

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,GL_LINEAR);  
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,GL_LINEAR);  

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);    

    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, 1);    

    glTexImage2D(GL_TEXTURE_2D, 0, Format, Source.Width, Source.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Source.Pixels);  

    If (Settings.compressTexture) Then
    Begin
      glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPRESSED, @Found);

      If (Not Found) Then
      Begin
        RaiseError('Could not compress texture!');
        Result := '';
        Exit;
      End;
    End;

    WriteLn('ok');

    Write(#9, 'Saving texture...');

    Dest := FileStream.Create(DestFile);
    Header := texHeaderPC;
    Dest.Write(Header, 4);

    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, @Format);        

    If (Settings.compressTexture) Then
      glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPRESSED_IMAGE_SIZE, @Size)
    Else
      Size := Source.Width * Source.Height * 4;

    GetMem(Data, Size);

    If (Settings.compressTexture) Then
      glGetCompressedTexImage(GL_TEXTURE_2D, 0, Data)
    Else
      glGetTexImage(GL_TEXTURE_2D, 0, Format, GL_UNSIGNED_BYTE, Data);

		Level := 0;
    Dest.Write(Format, 4);
    W := Source.Width;
    H := Source.Height;
    Dest.Write(W, 4);
    Dest.Write(H, 4);

    Dest.Write(Size, 4);
    Dest.Write(Data^, Size);
    FreeMem(Data);

    {
    Repeat
      glGetTexLevelParameteriv(GL_TEXTURE_2D, level, GL_TEXTURE_COMPRESSED_IMAGE_SIZE, @Size);
      GetMem(Data, Size);
      glGetCompressedTexImage(GL_TEXTURE_2D, level, Data);

      Dest.Write(Size, 4);
      Dest.Write(Data^, Size);
      FreeMem(Data);

      W := W Shr 1;
      H := H Shr 1;
      Inc(Level);
    Until (W<=4);}

{$IFNDEF EXCLUDENVSTRIPS}
    WriteLn('ok');
{$ENDIF}

    Dest.Destroy;

    GraphicsManager.Instance.DeleteTexture(Handle);    
  End Else
  Begin
    RaiseError('Invalid target platform');
    Result := '';
  End;

  Source.Destroy;
End;

{
GL_RGB32_A3 = 1, /*!< 32 color palette, 3 bits of alpha */
GL_RGB4 = 2, /*!< 4 color palette */
GL_RGB16 = 3, /*!< 16 color palette */
GL_RGB256 = 4, /*!< 256 color palette */
GL_COMPRESSED = 5, /*!< compressed texture */
GL_RGB8_A5 = 6, /*!< 8 color palette, 5 bits of alpha */
GL_RGBA = 7, /*!< 15 bit direct color, 1 bit of alpha */
GL_RGB = 8 /*!< 15 bit direct color, manually sets alpha bit to 1 */



http://pabut.homeip.net:8000/yagcd/chap14.html

tpl format:
- Note, all data is in Big-Endian

0x00 - 0x04: 4 byte identifier (0x0020AF30)
0x04 - 0x08: Number of entries in file;int/be
0x08 - 0x0C: Unknown (always 0x0000000C);int/be
0x0C - 0x??: Header_index[entries] Â  :int*2/be


Header index:
0x00 - 0x04: Location of image header in file;int/be
0x04 - 0x08: Location of palette header in file;int/be


Palette header:
0x00 - 0x02: Number of entries? Â ;short/be
0x02 - 0x04: Unknown Â  Â ;short/be
0x04 - 0x06: Unknown Â  Â ;short/be
0x06 - 0x08: Size of entry? Â ;short/be
0x08 - 0x0C: Location of palette in file;int/be


Image header:
0x00 - 0x02: Height Â  Â ;short/be
0x02 - 0x04: Width Â  Â ;short/be
0x04 - 0x08: Format Â  Â ;int/be
0x08 - 0x0C: Location of pixel data in file;int/be
0x0C - 0x10: Unknown (always 0x00000001);int/be
0x10 - 0x14: Unknown (always 0x00000001);int/be
0x14 - 0x18: Unknown (always 0x00000001);int/be
0x18 - 0x1C: Unknown (always 0x00000001);int/be
0x1C - 0x20: Unknown (always 0x00000000);int/be
0x20 - 0x24: Unknown (always 0x00000000);int/be


Format tags:
0x00000004: RGB565
0x00000005: ARGB1555
0x00000006: ARGB8888
0x00000008: 4BIT palettised
0x00000009: 8BIT palettised
0x0000000E: DXT1
}

{ TextureImportSettings }

Procedure TextureImportSettings.DisplayHelp;
Begin
{$IFNDEF EXCLUDENVSTRIPS}
  WriteLn(#9,'Texture options');
  WriteLn(#9#9,'Compress: Convert image to a compressed format');
  WriteLn(#9#9,'Optimize: Rescale image to recommended size');
  WriteLn(#9#9,'Mipmap:   Generate mipmap chain');
{$ENDIF}
End;

Procedure TextureImportSettings.Load(S:AnsiString);
Var
  INI:INIParser;
Begin
  INI := INIParser.Create;
  INI.ParseCommas := True;
  INI.AddToken('compress', tkBoolean, @compressTexture);
  INI.AddToken('optimize', tkBoolean, @downScale);
  INI.AddToken('mipmap', tkBoolean, @mipmapChain);
  INI.AddToken('globalpalette', tkBoolean, @globalpalette);
  INI.AddToken('preprocess', tkBoolean, @preprocess);
  INI.AddToken('path', tkString, @path);
  INI.LoadFromString(S);
  StringToInt(Path);
  INI.Destroy;
End;

End.