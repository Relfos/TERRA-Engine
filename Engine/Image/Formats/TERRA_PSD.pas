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
 * TERRA_PSD
 * Implements PSD loader
 ***********************************************************************************************************************
}
Unit TERRA_PSD;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Color, TERRA_Utils, TERRA_String, TERRA_Stream, TERRA_Image, TERRA_FileFormat;

Type
  PSDFormat = Class(TERRAFileFormat)
    Public
      Function Identify(Source:Stream):Boolean; Override;
      Function Load(Target:TERRAObject; Source:Stream):Boolean; Override;
  End;


Implementation
Uses TERRA_Log, TERRA_EngineManager, TERRA_FileUtils;

// Photoshop PSD loader -- PD by Thatcher Ulrich, integration by Nicolas Schulz, tweaked by STB, ported to Delphi by Sergio Flores

Function PSDFormat.Load(Target:TERRAObject; Source:Stream):Boolean;
Var
  Image:TERRAImage;
  pixelCount:Integer;
  channelCount:Word;
  compression:Word;
  channel, i, count:Integer;
  len, val:Byte;
  Width, Height:Cardinal;
  ID:FileHeader;
  Ofs:Cardinal;
  Version, Depth, ColorMode:Word;
  P:PColorRGBA;

  Procedure ReadChannel;
  Begin
    Case Channel Of
      0: Source.Read(@P.R, 1);
      1: Source.Read(@P.G, 1);
      2: Source.Read(@P.B, 1);
      3: Source.Read(@P.A, 1);
    End;

    If (Channel=0) Then
      P.A := 255;

    Inc(P);
  End;

  Procedure FillChannel;
  Begin
    Case Channel Of
      0: P.R := Val;
      1: P.G := Val;
      2: P.B := Val;
      3: P.A := Val;
    End;

    If (Channel=0) Then
      P.A := 255;

    Inc(P);
  End;

Begin
  Image := TERRAImage(Target);
  
  // Check identifier
  Source.Read(@ID, 4);
  If (ID <> '8BPS') Then   // "8BPS"
  Begin
    Log(logError, 'PSD', 'Corrupt PSD image: '+ Source.Name);
    Exit;
  End;

  // Check file type version.
  Source.ReadWord(Version);
  ByteSwap16(Version);
  If (Version <> 1) Then
  Begin
    Log(logError, 'PSD', 'Unsupported version of PSD image: '+ Source.Name + ', version= '+ IntegerProperty.Stringify(Version));
    Exit;
  End;

  // Skip 6 reserved bytes.
  Source.Skip(6);

  // Read the number of channels (R, G, B, A, etc).
  Source.ReadWord(ChannelCount);
  ByteSwap16(ChannelCount);
  If (channelCount > 16) Then
  Begin
    Log(logError, 'PSD', 'Unsupported number of channels in PSD: '+ Source.Name + ', channels= '+ IntegerProperty.Stringify(channelCount));
    Exit;
  End;

  // Read the rows and columns of the image.
  Source.ReadCardinal(Height);
  Source.ReadCardinal(Width);
  ByteSwap32(Height);
  ByteSwap32(Width);

  // Make sure the depth is 8 bits.
  Source.ReadWord(Depth);
  ByteSwap16(Depth);
  If (Depth <> 8) Then
  Begin
    Log(logError, 'PSD', 'Unsupported bit depth in PSD: '+ Source.Name + ', bitdepth= '+ IntegerProperty.Stringify(Depth));
    Exit;
  End;

   // Make sure the color mode is RGB.
   // Valid options are:
   //   0: Bitmap
   //   1: Grayscale
   //   2: Indexed color
   //   3: RGB color
   //   4: CMYK color
   //   7: Multichannel
   //   8: Duotone
   //   9: Lab color
  Source.ReadWord(ColorMode);
  ByteSwap16(ColorMode);
  If (ColorMode <> 3) Then
  Begin
    Log(logError, 'PSD', 'Wrong color format, PSD is not in RGB color format: '+ Source.Name + ', format= '+ IntegerProperty.Stringify(ColorMode));
    Exit;
  End;

  // Skip the Mode Data.  (It's the palette for indexed color; other info for other modes.)
  Source.ReadCardinal(Ofs);
  ByteSwap32(Ofs);
  Source.skip(Ofs);

  // Skip the image resources.  (resolution, pen tool paths, etc)
  Source.ReadCardinal(Ofs);
  ByteSwap32(Ofs);
  Source.Skip(Ofs);

  // Skip the reserved data.
  Source.ReadCardinal(Ofs);
  ByteSwap32(Ofs);
  Source.Skip(Ofs);

  // Find out if the data is compressed.
  // Known values:
  //   0: no compression
  //   1: RLE compressed
  Source.ReadWord(Compression);
  ByteSwap16(Compression);

  If (Compression > 1) Then
  Begin
    Log(logError, 'PSD', 'PSD has an unknown compression format: '+ Source.Name + ', compression= '+ IntegerProperty.Stringify(Compression));
    Exit;
  End;

   // Create the destination image.
  Image.New(Width, Height);

  // Finally, the image data.
  If (compression = 1) Then
  Begin
      // RLE as used by .PSD and .TIFF
      // Loop until you get the number of unpacked bytes you are expecting:
      //     Read the next source byte into n.
      //     If n is between 0 and 127 inclusive, copy the next n+1 bytes literally.
      //     Else if n is between -127 and -1 inclusive, copy the next byte -n+1 times.
      //     Else if n is 128, noop.
      // Endloop

      // The RLE-compressed data is preceeded by a 2-byte data count for each row in the data,
      // which we're going to just skip.
      Source.Skip(Height * channelCount * 2 );

      // Read the RLE data by channel.
      For channel := 0 To Pred(channelCount) Do
      Begin
        P := Image.RawPixels;

         If (channel >= channelCount) Then
          Break;

         // Read the RLE data.
        count := 0;
        While (count < Image.pixelCount) Do
        Begin
          Source.ReadByte(Len);
          If (len = 128) Then
          Begin
              // No-op.
          End Else
          If (len < 128) Then
          Begin
            // Copy next len+1 bytes literally.
            Inc(len);
            Inc(Count, len);
            While (len>0) Do
            Begin
              ReadChannel();
              Dec(len);
            End;
          End Else
          If (len > 128) Then
          Begin
            // Next -len+1 bytes in the dest are replicated from next source byte.
            // (Interpret len as a negative 8-bit int.)
            Len := Len Xor $0FF;
            Inc(len, 2);
            Source.ReadByte(Val);
            Inc(count, len);
            While (len>0) Do
            Begin
              FillChannel();
              Dec(len);
            End;
          End;
        End;
     End;
  End Else
  Begin
      // We're at the raw image data.  It's each channel in order (Red, Green, Blue, Alpha, ...)
      // where each channel consists of an 8-bit value for each pixel in the image.

      // Read the data by channel.
      For channel := 0 To Pred(channelCount) Do
      Begin
        P := Image.RawPixels;

         If (channel >= channelCount) Then
          Break;

         // Read the data.
         For I := 0 To Pred(Image.pixelCount) Do
          ReadChannel();
      End;
  End;
End;

Function PSDFormat.Identify(Source:Stream):Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID, 4);
  Result := (ID = '8BPS');
End;

Begin
  Engine.Formats.Add(PSDFormat.Create(TERRAImage, 'psd'));

End.
