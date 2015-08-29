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
 * TERRA_TGA
 * Implements TGA loader
 ***********************************************************************************************************************
}

Unit TERRA_TGA;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Stream, TERRA_Image, TERRA_FileFormat;

Type
  TGAFormat = Class(TERRAFileFormat)
    Public
      Function Identify(Source:TERRAStream):Boolean; Override;
      Function Load(Target:TERRAObject; Source:TERRAStream):Boolean; Override;
      Function Save(Target:TERRAObject; Dest:TERRAStream):Boolean; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_EngineManager, TERRA_Utils, TERRA_FileUtils, TERRA_Application, TERRA_Color;

Type
  TGAHeader=Packed Record   // Header type for TGA images
    FileType:Byte;
    ColorMapType:Byte;
    ImageType:Byte;
    ColorMapSpec:Array[0..4] of Byte;
    OrigX:Word;
    OrigY:Word;
    Width:Word;
    Height:Word;
    BPP:Byte;
    ImageInfo:Byte;
  End;

{ TGAFormat }
Function TGAFormat.Identify(Source: TERRAStream): Boolean;
Var
  ID:FileHeader;
Begin
  Source.Seek(Source.Size - 18);
  Source.Read(@ID, 4);
  Result := (ID='TRUE');
End;

Function TGAFormat.Load(Target: TERRAObject; Source: TERRAStream): Boolean;
Var
  Header:TGAHeader;
  ColorDepth:Cardinal;
  PacketHdr,PacketLen:Byte;
  I,J:Integer;
  Color:ColorRGBA;
  PixelSize:Integer;
  N, Count:Integer;
  Flags:Cardinal;

  SrcLine:Array Of Byte;
  Dest:PColorRGBA;
  Image:TERRAImage;
Begin
  Image := TERRAImage(Target);
  Result := False;

  // Read in the bitmap file header
  Source.Read(@Header,SizeOf(Header));

  // Only support 24, 32 bit images
  If (Header.ImageType<>2) And    // TGA_RGB
     (Header.ImageType<>10) Then  // Compressed RGB
  Begin
    RaiseError('Invalid header.');
    Exit;
  End;

  // Don't support colormapped files
  If Header.ColorMapType<>0 Then
  Begin
    RaiseError('Colormapped texture not supported.');
    Exit;
  End;

  // Get the width, height, and color depth
  ColorDepth:=Header.BPP;

  If ColorDepth<24 Then
  Begin
    RaiseError('Invalid color format.');
    Exit;
  End;

  Image.New(Header.Width,Header.Height);
  PixelSize := ColorDepth Div 8;
  Color := ColorWhite;

  Case Header.ImageType Of
    2:  Begin   // Standard TGA file
          SetLength(SrcLine, Image.Width * PixelSize);
         // Get the actual pixel data
          For J:=0 To Pred(Image.Height) Do
          Begin
            Source.Read(@SrcLine[0], PixelSize * Image.Width);

            If (ColorDepth = 24) Then
              Image.LineDecodeBGR24(@SrcLine[0], Pred(Image.Height)-J)
            Else
              Image.LineDecodeBGR32(@SrcLine[0], Pred(Image.Height)-J);
          End;

          SetLength(SrcLine, 0);
        End;

    10: Begin // Compressed TGA files
          Count := Image.Width * Image.Height;
          N :=0;

          Dest := Image.RawPixels;

          // Extract pixel information from compressed data
          While (Count>0) Do
          Begin
            Source.ReadByte(PacketHdr);
            PacketLen := Succ(PacketHdr And $7F);

            If (PacketHdr And $80 <> 0) Then
            Begin
              // Run-length packet.
              Color.A := 255;
              Source.Read(@Color, PixelSize);
              Color := ColorSwap(Color);

              // Replicate the packet in the destination buffer.
              For I:=0 To Pred(PacketLen) Do
              Begin
                Dest^ := Color;
                Inc(Dest);
                Dec(Count);
              End;

            End Else
            Begin
            // Raw packet.
              For I:=0 To Pred(packetLen) Do
              Begin
                Color.A := 255;
                Source.Read(@Color, PixelSize);
                Color := ColorSwap(Color);
                Dest ^:= Color;
                Inc(Dest);
                Dec(Count);
              End;
            End;
        End;

        Image.FlipVertical();
    End;
  End;

  Result := True;
End;

Function TGAFormat.Save(Target:TERRAObject; Dest:TERRAStream):Boolean;
Var
  I,J:Integer;
  Header:TGAHeader;
  Color:ColorRGBA;
  Image:TERRAImage;
Begin
  Image := TERRAImage(Target);
  Result := True;

  FillChar(Header,SizeOf(Header),0);
  Header.ImageType := 2;
  Header.BPP := 32;
  Header.Width := Image.Width;
  Header.Height := Image.Height;

  Dest.Write(@Header,SizeOf(Header));
  For J:=0 To Pred(Image.Height) Do
    For I:=0 To Pred(Image.Width) Do
    Begin
      Color := ColorSwap(Image.GetPixel(I, Pred(Image.Height) - J));
      Dest.Write(@Color, SizeOf(Color));
    End;
End;


Begin
  Engine.Formats.Add(TGAFormat.Create(TERRAImage, 'tga'));
End.
