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
 * TERRA_TGA
 * Implements TGA loader
 ***********************************************************************************************************************
}

Unit TERRA_TGA;
{$I terra.inc}

Interface

Implementation
Uses TERRA_String, TERRA_Error, TERRA_Utils, TERRA_Stream, TERRA_Image, TERRA_Application, TERRA_Color;

Type
 LTGAHeader=Packed Record   // Header type for TGA images
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

Procedure TGALoad(Source:Stream; Image:Image);
Var
  Header:LTGAHeader;
  ColorDepth:Cardinal;
  PacketHdr,PacketLen:Byte;
  I,J:Integer;
  Color:TERRA_Color.Color;
  PixelSize:Integer;
  N, Count:Integer;
  Flags:Cardinal;
  Dest, Buffer:PColor;
Begin
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
  PixelSize:=ColorDepth Div 8;
  Color := ColorWhite;

  Case Header.ImageType Of
    2:  Begin   // Standard TGA file
       // Get the actual pixel data
          For J:=0 To Pred(Image.Height) Do
            For I:=0 To Pred(Image.Width) Do
            Begin
              Source.Read(@Color,PixelSize);
              Image.SetPixel(I,J, Color);
            End;
        End;
    10: Begin // Compressed TGA files
          Count:=Image.Width*Image.Height;
          GetMem(Buffer, Image.Width);
          Dest:=Buffer;
          J:=Pred(Image.Height);
          N:=0;
          Color.A := 255;

          // Extract pixel information from compressed data
          Repeat
            Source.Read(@PacketHdr,1);
            PacketLen:=Succ(PacketHdr And $7F);

            If (PacketHdr And $80 <> 0) Then
            Begin
              // Run-length packet.
              Source.Read(@Color, PixelSize);
              // Replicate the packet in the destination buffer.
              For I:=0 To Pred(PacketLen) Do
              Begin
                Dest^:=Color;
                Inc(Dest);
                Dec(Count);
                Inc(N);
                If (N=Image.Width) Then
                Begin
                  Image.LineDecodeBGR32(Buffer, J);
                  Dec(J);
                  Dest:=Buffer;
                  N:=0;
                End;
              End;
            End Else
            Begin
            // Raw packet.
              For I:=0 To Pred(packetLen) Do
              Begin
                Source.Read(@Color, PixelSize);
                Dest^:=Color;
                Inc(Dest);
                Dec(Count);
                Inc(N);
                If (N=Image.Width) Then
                Begin
                  Image.LineDecodeBGR32(Buffer, J);
                  Dec(J);
                  Dest:=Buffer;
                  N:=0;
                End;
              End;
            End;

        Until Count<=0;

        FreeMem(Buffer);
    End;
  End;

  Image.Process(IMP_SwapChannels Or IMP_FlipVertical);
End;

Procedure TGASave(Dest:Stream; Image:Image; Const Options:TERRAString);
Var
  I,J:Integer;
  Header:LTGAHeader;
  Color:TERRA_Color.Color;
Begin
  FillChar(Header,SizeOf(Header),0);
  Header.ImageType:=2;
  Header.BPP:=32;
  Header.Width:=Image.Width;
  Header.Height:=Image.Height;

  Dest.Write(@Header,SizeOf(Header));
  For J:=0 To Pred(Image.Height) Do
    For I:=0 To Pred(Image.Width) Do
    Begin
      Color := ColorSwap(Image.GetPixel(I, Pred(Image.Height) - J));
      Dest.Write(@Color, SizeOf(Color));
    End;
End;

Function ValidateTGA(Stream:Stream):Boolean;
Var
  ID:Word;
Begin
  Stream.ReadWord(ID);
  Result := (ID=0);
End;

Begin
  RegisterImageFormat('TGA', ValidateTGA, TGALoad, TGASave);
End.
