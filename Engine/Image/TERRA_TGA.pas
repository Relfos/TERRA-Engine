{
@abstract(TGA Loader)
@author(Sergio Flores <relfos@gmail.com>)
@created(October 29, 2005)
@lastmod(October 30, 2005)
Allows loading of TGA image files.

Supported features
Features marked with * are supported in the format, but not in the loader
#############################################
#   Colors        24 and 32bits             #
#   Greyscale     No                        #
#   Alpha         32bits                    #
#   Transparency  No                        #
#   Animation     No                        #
#   Layers        No                        #
#############################################

Version History
   31/10/05  • Implemented basic TGA loader
   13/07/06  • Fixed RLE decompression
             • Added support for 32-bit TGAs with Alphachannel
             • Now correctly loads vertical flipped TGAs
   24/01/07  • Added support for saving TGAs

}

Unit TERRA_TGA;
{$I terra.inc}

Interface

Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_IO, TERRA_Image, TERRA_Application, TERRA_Color;

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

  If PixelSize=4 Then
    Image.HasAlpha:=True;
End;

Procedure TGASave(Dest:Stream; Image:Image; Options:AnsiString);
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
  ID:Array[1..2] Of AnsiChar;
Begin
  Stream.Read(@ID,2);
  Result:=(ID=#0#0);
End;

Begin
  RegisterImageFormat('TGA',ValidateTGA,TGALoad,TGASave);
End.
