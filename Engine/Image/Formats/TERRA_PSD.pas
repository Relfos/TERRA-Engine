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
Uses TERRA_Utils, TERRA_IO, TERRA_Image;

Implementation
Uses TERRA_INI, TERRA_Color, TERRA_Log;

// Photoshop PSD loader -- PD by Thatcher Ulrich, integration by Nicolas Schulz, tweaked by STB, ported to Delphi by Sergio Flores

Procedure LoadPSD(Source:Stream; Image:Image);
Var
  pixelCount:Integer;
  channelCount:Word;
  compression:Word;
  channel, i, count:Integer;
  len, val:Byte;
  w,h:Integer;
  ID, Ofs:Cardinal;
  N:Word;
  P:PColor;

  Procedure ReadChannel;
  Begin
    Case Channel Of
      0: Source.Read(@P.R, 1);
      1: Source.Read(@P.G, 1);
      2: Source.Read(@P.B, 1);
      3: Source.Read(@P.A, 1);
    End;
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
    Inc(P);
  End;

Begin
  // Check identifier
  Source.Read(@ID, 4);
  If (ID <> $38425053) Then   // "8BPS"
  Begin
    Log(logError, 'PSD', 'Corrupt PSD image: '+ Source.Name);
    Exit;
  End;

  // Check file type version.
  Source.Read(@N, 2);
  If (N <> 1) Then
  Begin
    Log(logError, 'PSD', 'Unsupported version of PSD image: '+ Source.Name + ', version= '+ IntToString(N));
    Exit;
  End;

  // Skip 6 reserved bytes.
  Source.Skip(6);

  // Read the number of channels (R, G, B, A, etc).
  Source.Read(@channelCount, 2);
  If (channelCount < 0) Or (channelCount > 16) Then
  Begin
    Log(logError, 'PSD', 'Unsupported number of channels in PSD: '+ Source.Name + ', channels= '+ IntToString(channelCount));
    Exit;
  End;

  // Read the rows and columns of the image.
  Source.Read(@h, 4);
  Source.Read(@w, 4);

  // Make sure the depth is 8 bits.
  Source.Read(@n, 2);
  If (N <> 8) Then
  Begin
    Log(logError, 'PSD', 'Unsupported bit depth in PSD: '+ Source.Name + ', bitdepth= '+ IntToString(N));
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
  Source.Read(@n, 2);
  If (N <> 3) Then
  Begin
    Log(logError, 'PSD', 'Wrong color format, PSD is not in RGB color format: '+ Source.Name + ', format= '+ IntToString(N));
    Exit;
  End;

  // Skip the Mode Data.  (It's the palette for indexed color; other info for other modes.)
  Source.Read(@Ofs, 4);
  Source.skip(Ofs);

  // Skip the image resources.  (resolution, pen tool paths, etc)
  Source.Read(@Ofs, 4);
  Source.Skip(Ofs);

  // Skip the reserved data.
  Source.Read(@Ofs, 4);
  Source.Skip(Ofs);

  // Find out if the data is compressed.
  // Known values:
  //   0: no compression
  //   1: RLE compressed
  Source.Read(@Compression, 2);
  If (Compression > 1) Then
  Begin
    Log(logError, 'PSD', 'PSD has an unknown compression format: '+ Source.Name + ', compression= '+ IntToString(Compression));
    Exit;
  End;

   // Create the destination image.
  Image.New(W, H);

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
      Source.Skip(h * channelCount * 2 );

      // Read the RLE data by channel.
      For channel := 0 To 3 Do
      Begin
        P := Image.Pixels;
        If (Channel=0) Then
          P^ := ColorGrey(0);

         If (channel >= channelCount) Then
          Break;

         // Read the RLE data.
        count := 0;
        While (count < Image.pixelCount) Do
        Begin
          Source.Read(@len, 1);
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
            Source.Read(@val, 1);
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
      For channel := 0 To 3 Do
      Begin
        P := Image.Pixels;
        If (Channel=0) Then
          P^ := ColorGrey(0);

         If (channel >= channelCount) Then
          Break;

         // Read the data.
         For I := 0 To Pred(Image.pixelCount) Do
          ReadChannel();
      End;
   End;
End;

Function ValidatePSD(Stream:Stream):Boolean;
Var
  ID:Cardinal;
Begin
  Stream.Read(@ID, 4);
  Result := (ID = $38425053);   // "8BPS"
End;

Begin
  RegisterImageFormat('PSD', ValidatePSD, LoadPSD, Nil);
End.
