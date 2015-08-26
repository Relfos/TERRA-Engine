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
 * TERRA_BMP
 * Implements BMP loader/saver
 ***********************************************************************************************************************
}
Unit TERRA_BMP;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Color, TERRA_Utils, TERRA_String, TERRA_Stream, TERRA_Image, TERRA_FileFormat;

Type
  BMPFormat = Class(TERRAFileFormat)
    Public
      Function Identify(Source:Stream):Boolean; Override;
      Function Load(Target:TERRAObject; Source:Stream):Boolean; Override;
      Function Save(Target:TERRAObject; Dest:Stream):Boolean; Override;
  End;


Implementation
Uses TERRA_Log, TERRA_EngineManager;

Type
  BitmapFileHeader = Packed Record
    ID:Word;
    FileSize:Cardinal;
    Reserved:Cardinal;
    Offbits:Cardinal;
    HeaderSize:Cardinal;
    Width:Cardinal;
    Height:Cardinal;
    Planes:Word;
    BitCount:Word;
    Compression:Cardinal;
    SizeImage:Cardinal;
    XPelsPerMeter:Cardinal;
    YPelsPerMeter:Cardinal;
    ColorsUsed:Cardinal;
    ColorsImportant:Cardinal;
  End;

  BitmapRasterLine = Array[0..5096] Of Byte;

Const
  // constants for the biCompression field
  BI_RGB = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;

Procedure DecompressLine(Source:Stream; Buffer:PByteArray; Width:Word);
Var
  X,N:Word;
  First,Second:Byte;
Begin
  N:=0;
  Repeat
    Source.Read(@First,1);
    Source.Read(@Second,1);
    If First<>0 Then
    Begin
      For X:=0 To Pred(First) Do
        Buffer[N+X]:=Second;
      Inc(N,Pred(First));
    End Else
    Begin
      If Second<2 Then
        Break
      Else
      Begin
        For X:=0 To Pred(Second) Do
        Begin
          Source.Read(@First,1);
          Buffer[N+X]:=First;
        End;
        Inc(N,Pred(Second));
      End;
    End;
  Until (N>=Pred(Width));
End;

{ BMPFormat }
Function BMPFormat.Identify(Source: Stream): Boolean;
Var
  ID:Array[1..2] Of AnsiChar;
Begin
  Source.Read(@ID,2);
  Result := (ID='BM');
End;

Function BMPFormat.Load(Target: TERRAObject; Source: Stream): Boolean;
Var
  Header:BitmapFileHeader;
  Palette:ColorPalette;
  PaletteLength:Cardinal;
  Buffer:PByte;
  Line:BitmapRasterLine;
  Color:ColorRGBA;
  Index:Byte;
  I,J:Integer;
  Pad, Temp:Integer;
  Image:TERRAImage;
  It:ImageIterator;
Begin
  Result := False;
  Image := TERRAImage(Target);

  // Get header information
  Source.Read(@Header,SizeOf(BitmapFileHeader));

  If (Header.ID<>19778)Or(Header.Planes<>1)Then
  Begin
    Log(logError, 'BMP', 'Invalid bitmap file.');
    Exit;
  End;

  Image.New(Header.Width,Header.Height);

  If Header.BitCount<24 Then
  Begin
    PaletteLength:=Header.ColorsUsed;
    If PaletteLength=0 Then
      PaletteLength:=256;

    Source.Read(@Palette[0], PaletteLength*SizeOf(Color));
    Source.Seek(Header.OffBits);
  End;

  Pad:=(4 - ((Image.Width * 3) Mod 4));
  If Pad=4 Then
    Pad:=0;

  Case Header.BitCount Of
  4:  Begin
        GetMem(Buffer, Image.Width Shr 1);

        For J:=Pred(Header.Height) Downto 0 Do
        Begin
          Source.Read(Buffer, Header.Width Shr 1);
          Source.Read(@Temp, Pad);
          Image.LineDecodeRGBPalette4(Buffer, @(Palette[0]), J);
        End;

        FreeMem(Buffer);

      End;
  8:  Begin
        GetMem(Buffer, Image.Width);

        For J:=Pred(Header.Height) Downto 0 Do
        Begin
          If Header.Compression=0 Then
            Source.Read(Buffer , Header.Width)
          Else
            DecompressLine(Source, PByteArray(Buffer), Header.Width);
          Source.Read(@Temp, Pad);
          Image.LineDecodeRGBPalette8(Buffer, @(Palette[0]), J);
        End;

        FreeMem(Buffer);
      End;
  24: Begin
        GetMem(Buffer, Image.Width * 3);
        // Get the actual pixel data
        For J:=Pred(Image.Height) DownTo 0 Do
        Begin
          Source.Read(Buffer, Image.Width * 3);
          Source.Read(@Temp, Pad);
          Image.LineDecodeRGB24(Buffer, J);
        End;

        FreeMem(Buffer);
      End;
 32:  Begin
        // Get the actual pixel data, including alpha
        GetMem(Buffer, Image.Width * 4);
        // Get the actual pixel data
        For J:=Pred(Image.Height) DownTo 0 Do
        Begin
          Source.Read(Buffer, Image.Width * 4);
          Source.Read(@Temp, Pad);
          Image.LineDecodeRGB32(Buffer, J);
        End;

        FreeMem(Buffer);
      End;
  Else
    Begin
      Log(logError, 'BMP', 'Invalid bit depth.');
      Exit;
    End;
  End;

  Image.SwapChannels();
  
  Result := True;
End;

Function BMPFormat.Save(Target: TERRAObject; Dest: Stream): Boolean;
Var
  Header:BitmapFileHeader;
  SizeImage:Cardinal;
  I,J:Integer;
  Pad:Integer;
  SkipHeader:Boolean;
  Pixel:ColorRGBA;
  Image:TERRAImage;
Begin
  Result := False;
  Image := TERRAImage(Target);

  If Not Assigned(Image.RawPixels) Then
    Exit;

  SkipHeader := False;

  SizeImage:=(Image.Width*Image.Height*4);
  Pad:=(4 - ((Image.Width * 3) Mod 4));
  If Pad=4 Then
    Pad:=0;

  // Prepare header information
  FillChar(Header,SizeOf(Header),0);
  Header.ID := 19778;
  Header.FileSize := SizeOf(Header)+SizeImage;
  Header.OffBits := SizeOf(Header);
  Header.HeaderSize := SizeOf(Header)-14;
  Header.Planes := 1;
  Header.BitCount := 32;
  Header.Width := Image.Width;
  Header.Height := Image.Height;
  Header.Compression := 0;
  Header.SizeImage := SizeImage;
  Header.XPelsPerMeter := Image.Width*20;
  Header.YPelsPerMeter :=Image.Height*20;

  If SkipHeader Then
  Begin
    Dest.Write(@Header.HeaderSize, 4);
    Dest.Write(@Header.Width, 4);
    Header.Height := Header.Height * 2;
    Dest.Write(@Header.Height, 4);
    Dest.Write(@Header.Planes, 2);
    Dest.Write(@Header.BitCount, 2);
    Dest.Write(@Header.Compression, 4);
    Dest.Write(@Header.SizeImage, 4);
    Dest.Write(@Header.XPelsPerMeter, 4);
    Dest.Write(@Header.YPelsPerMeter, 4);
    Dest.Write(@Header.ColorsUsed, 4);
    Dest.Write(@Header.ColorsImportant, 4);
  End Else
    Dest.Write(@Header,SizeOf(Header));

  For J:=Pred(Image.Height) Downto 0 Do
  Begin
  	For I:=0 To Pred(Image.Width) Do
  	Begin
	  	Pixel := Image.GetPixel(I,J);

      Pixel := ColorCreate(Pixel.B, Pixel.G, Pixel.R, Pixel.A);
    	Dest.Write(@Pixel, 4);
    End;
	End;
End;

Begin
  Engine.Formats.Add(BMPFormat.Create(TERRAImage, 'bmp'));
End.
