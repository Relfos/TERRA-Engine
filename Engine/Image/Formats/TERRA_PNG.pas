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
 * TERRA_PNG
 * Implements PNG loader/saver
 ***********************************************************************************************************************
}

Unit TERRA_PNG;
{$I terra.inc}

{-$DEFINE LOGGING}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Stream, TERRA_Image;

Function ValidatePNG(Source:Stream):Boolean;
Procedure PNGLoad(Source:Stream; MyImage:Image);
Procedure PNGSave(Dest:Stream; MyImage:Image; Const Options:TERRAString='');

Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_CRC32, TERRA_INI, TERRA_Color, TERRA_Log, TERRA_ZLib;

Type
  RGBQuad=Packed Record
    R,G,B:Byte;
  End;

  // ZLIB Decompression extra information
  ZStreamRec2=Packed Record
    ZLIB:z_stream;   // From ZLIB
    Data:Pointer;       // Additional info
    Source:Stream;
  End;

  PNGHeader=Packed Record
    Width:Integer;
    Height:Integer;
    BitDepth:Byte;
    ColorType:Byte;
    CompressionMethod:Byte;
    FilterMethod:Byte;
    InterlaceMethod:Byte;
  End;

  PNGLoader = Class(TERRAObject)
    Protected
      Buffer:Image;
      Header:PNGHeader;
      BytesPerRow:Cardinal;
      Offset:Cardinal;
      RowBuffer:Array[Boolean] Of PByteArray;
      RowUsed:Boolean;
      EndPos:Integer;   // Ending position for the current IDAT chunk
      Palette:Array[0..255]Of RGBQuad;
      PaletteAlpha:Array[0..255]Of Byte;

      Procedure FilterRow;
      Procedure DecodeNonInterlaced(Var ZLIBStream:ZStreamRec2);

    // Memory copy methods to decode
      Procedure CopyNonInterlacedRGB(Src,Dest:PByte);
      Procedure CopyNonInterlacedPalette(Src,Dest:PByte);
      Procedure CopyNonInterlacedRGBAlpha(Src,Dest:PByte);
      Procedure CopyNonInterlacedGrayscaleAlpha(Src,Dest:PByte);
	    Procedure CopyNonInterlacedGrayscale16(Src,Dest:PByte);
  End;

  LPNGChunkName=Array[0..3] Of AnsiChar;

  PPNGChunk=^LPNGChunk;
  LPNGChunk=Record
    Name:LPNGChunkName;
    Size:Integer;
  End;

  LPNGChunkHandler=Procedure(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);

  LPNGChunkRec=Record
    Name:LPNGChunkName;
    Handler:LPNGChunkHandler;
  End;

  Procedure SkipChunk(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);Forward;
  Procedure Process_IHDR(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);Forward;
  Procedure Process_PLTE(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);Forward;
  Procedure Process_IDAT(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);Forward;
  Procedure Process_TRNS(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);Forward;

Const
  PNGSignature:Array[0..7] Of AnsiChar = (#137, #80, #78, #71, #13, #10, #26, #10);

  ZLIBErrors:Array[-6..2] Of AnsiString=('incompatible version (-6)',
                                      'buffer error (-5)',
                                      'insufficient memory (-4)',
                                      'data error (-3)',
                                      'stream error (-2)',
                                      'file error (-1)',
                                      '(0)',
                                      'stream end (1)',
                                      'need dictionary (2)');

  MaxIDATSize = High(Word);

  // Interlacing modes
  imNone=0;
  imAdam7=1;

  // Avaliable PNG filters for mode 0
  FILTER_NONE    = 0;
  FILTER_SUB     = 1;
  FILTER_UP      = 2;
  FILTER_AVERAGE = 3;
  FILTER_PAETH   = 4;

  // Avaliable color modes for PNG
  COLOR_GRAYSCALE      = 0;
  COLOR_RGB            = 2;
  COLOR_PALETTE        = 3;
  COLOR_GRAYSCALEALPHA = 4;
  COLOR_RGBALPHA       = 6;

  ChunkListSize=4{16};
  ChunkList:Array[0..Pred(ChunkListSize)] Of LPNGChunkRec=
        ((Name:'IHDR'; Handler:Process_IHDR),
         (Name:'PLTE'; Handler:Process_PLTE),
         (Name:'IDAT'; Handler:Process_IDAT),
         (Name:'tRNS'; Handler:Process_TRNS){,
         (Name:'IEND'; Handler:SkipChunk),
         (Name:'MHDR'; Handler:SkipChunk),
         (Name:'MEND'; Handler:SkipChunk),
         (Name:'gAMA'; Handler:SkipChunk),
         (Name:'sBIT'; Handler:SkipChunk),
         (Name:'cHRM'; Handler:SkipChunk),
         (Name:'bKGD'; Handler:SkipChunk),
         (Name:'hIST'; Handler:SkipChunk),
         (Name:'tEXt'; Handler:SkipChunk),
         (Name:'zTXt'; Handler:SkipChunk),
         (Name:'pHYs'; Handler:SkipChunk),
         (Name:'tIME'; Handler:SkipChunk)});

Var
  _PNGCRCTable:CRC32Table;

// Calculates the paeth predictor
Function PaethPredictor(a, b, c: Byte): Byte;
Var
  p, pa, pb, pc:Integer;
Begin
  // a = left, b = above, c = upper left
  p := a + b - c;
  pa := Abs(p-a);      // distances to a, b, c
  pb := Abs(p-b);
  pc := Abs(p-c);

  // return nearest of a, b, c, breaking ties in order a, b, c
  If (Pa<=Pb) And (Pa<=Pc) Then
    Result:=A
  Else
  If Pb<=Pc then
    Result:=B
  Else
    Result:=C;
End;

Procedure GetPixelInfo(Header:PNGHeader; Var LineSize,Offset:Cardinal);
Begin
  // Calculate number of bytes for each line}
  Case Header.ColorType Of
   COLOR_GRAYSCALE,COLOR_PALETTE: LineSize:=(Header.Width * Header.BitDepth + 7) Div 8;
   COLOR_RGB: LineSize := (Header.Width * Header.BitDepth * 3) div 8;
   COLOR_GRAYSCALEALPHA: LineSize := (Header.Width * Header.BitDepth * 2) Div 8;
   COLOR_RGBALPHA: LineSize := (Header.Width * Header.BitDepth * 4) Div 8;
   Else LineSize:= 0;
  End;

  // Calculates byte offset
  Case Header.ColorType of
   COLOR_GRAYSCALE: If Header.BitDepth = 16 Then
                      Offset := 2
                    Else
                      Offset := 1;
   COLOR_PALETTE: Offset := 1;
   COLOR_RGB: Offset:=3 * Header.BitDepth Div 8;
   COLOR_GRAYSCALEALPHA: Offset := 2 * Header.BitDepth Div 8;
   COLOR_RGBALPHA: Offset := 4 * Header.BitDepth Div 8;
   Else Offset := 0;
  End ;
End;

Const
  ZLIBAllocate = High(Word);

// Initializes ZLIB for decompression
Function ZLIBInitInflate(Source:Stream):ZStreamRec2;
Begin
  // Fill record
  FillChar(Result, SizeOf(ZStreamRec2),0);

  // Set internal record information
  GetMem(Result.Data,ZLIBAllocate);
  Result.Source := Source;

  // Init decompression
  InflateInit_(@Result.zlib, zlib_version, SizeOf(z_Stream));
End;

// Terminates ZLIB for decompression
Procedure ZLIBTerminateInflate(Var ZLIBStream: ZStreamRec2);
Begin
  // Terminates decompression
  InflateEnd(ZLIBStream.zlib);
  /// Free internal record
  FreeMem(ZLIBStream.Data);
End;

// Initializes ZLIB for compression
Function ZLIBInitDeflate(Source:Stream):ZStreamRec2;
Begin
  // Fill record
  FillChar(Result,SizeOf(ZStreamRec2),0);

  // Set internal record information
  GetMem(Result.Data, MaxIDATSize);
  Result.Source := Source;
  Result.ZLIB.next_out := Result.Data;
  Result.ZLIB.avail_out := MaxIDATSize;

  // Init compression
  DeflateInit_(@Result.zlib, Z_DEFAULT_COMPRESSION, zlib_version, SizeOf(z_Stream));
End;


// Terminates ZLIB for compression
Procedure ZLIBTerminateDeflate(Var ZLIBStream: ZStreamRec2);
Begin
  // Terminates compression
  DeflateEnd(ZLIBStream.zlib);
  /// Free internal record
  FreeMem(ZLIBStream.Data);
End;

Procedure SkipChunk(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);
Begin
  Source.Skip(Chunk.Size);
End;

// Process header
Procedure Process_IHDR(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);
Var
  I:Integer;
Begin
  If Chunk.Size<SizeOf(PNG.Header) Then
  Begin
    RaiseError('Invalid header size');
    Exit;
  End;

  //Source.Read(PNG.Header,SizeOf(PNG.Header));
  Source.Read(@PNG.Header.Width,4);
  ByteSwap32(Cardinal(PNG.Header.Width));
  Source.Read(@PNG.Header.Height,4);
  ByteSwap32(Cardinal(PNG.Header.Height));
  Source.Read(@PNG.Header.BitDepth,1);
  Source.Read(@PNG.Header.ColorType,1);
  Source.Read(@PNG.Header.CompressionMethod,1);
  Source.Read(@PNG.Header.FilterMethod,1);
  Source.Read(@PNG.Header.InterlaceMethod,1);

  Source.Skip(Chunk.Size-SizeOf(PNG.Header));

  Log(logDebug,'PNG', 'Width:' +IntToString(PNG.Header.Width));
  Log(logDebug,'PNG', 'Height:' +IntToString(PNG.Header.Height));

  // Compression method must be 0 (inflate/deflate)
  If (PNG.Header.CompressionMethod<>0) then
  Begin
    RaiseError('Invalid compression method');
    Exit;
  End;

  If  (PNG.Header.InterlaceMethod<>imNone)
  And (PNG.Header.InterlaceMethod<>imAdam7) then
  Begin
    RaiseError('Invalid interlace method');
    Exit;
  End;

  With PNG Do
    Buffer.New(Header.Width,Header.Height);

  If PNG.Header.ColorType=COLOR_GRAYSCALE Then
  Begin
    For I:=0 To 255 Do
    Begin
      PNG.Palette[I].R:=I;
      PNG.Palette[I].G:=I;
      PNG.Palette[I].B:=I;
    End;
  End;
End;

Procedure Process_PLTE(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);
Begin
  Source.Read(@PNG.Palette, Chunk.Size);
End;

// Reads from ZLIB
Function IDATZlibRead(Var ZLIBStream:ZStreamRec2;
                      Buffer:Pointer;
                      Count:Cardinal;
                      Var EndPos:Integer):Integer;
Var
  IDATHeader:LPNGChunkName;
Begin
  Result:=-1;
  With ZLIBStream,ZLIBStream.ZLIB Do
  Begin
    // Set the buffer the zlib will read into
    next_out:=Buffer;
    avail_out:=Count;

    // Decode until it reach the Count variable
    While Avail_out>0 Do
    Begin
     // In case it needs more data and it's in the end of a IDAT chunk
      If  (Source.Position=EndPos)
      And (Avail_out>0) And (Avail_in=0) Then
      Begin
        // Skip chunk CRC
        Source.Skip(4);

        // Start reading the next chunk
        Source.Read(@EndPos, 4);        // Reads next chunk size
        ByteSwap32(Cardinal(EndPos));
        Source.Read(@IDATHeader, 4);    // Next chunk header

        // PNG specification says that multiple IDAT chunks must be consecutive
        If IDATHeader<>'IDAT' Then
        Begin
          RaiseError('IDAT chunk expected.');
          Exit;
        End;

        Inc(EndPos, Source.Position);
      End;


     // In case it needs compressed data to read from
      If Avail_in=0 Then
      Begin
        // In case it's trying to read more than it is avaliable
        If Source.Position+ZLIBAllocate>EndPos Then
          Avail_in := Source.Read(Data, EndPos-Source.Position)
        Else
          Avail_in := Source.Read(Data, ZLIBAllocate);

       // In case there is no more compressed data to read from
        If Avail_in=0 Then
        Begin
          Result:=Count-Avail_out;
          Exit;
        End;

        // Set next buffer to read and record current position
        next_in:=Data;

      End;

      Result := Inflate(zlib,0);

      // In case the result was not sucessfull
      If (Result<0) Then
      Begin
        RaiseError('ZLib error.'+zliberrors[Result]);
        Exit;
      End Else
        Result := Avail_In;
    End;
  End;
End;

Function WriteChunkHeader(Source:Stream; ChunkName:LPNGChunkName; ChunkSize:Integer):Cardinal;
Begin
  ByteSwap32(Cardinal(ChunkSize));
  Source.Write(@ChunkSize, 4);
  Result := Source.Position;
  Source.Write(@ChunkName, 4);
End;

Procedure WriteIDAT(Dest:Stream; Data:Pointer; Const Length:Cardinal);
Var
  OP,CRC:Cardinal;
Begin
  OP := WriteChunkHeader(Dest, 'IDAT', Length);
  Dest.Write(Data, Length);
  CRC := GetCRC32(Dest, OP, Length+4, _PNGCRCTable);
  ByteSwap32(Cardinal(CRC));
  Dest.Write(@CRC, 4);
End;

// Writes to ZLIB
Function IDATZlibWrite(Var ZLIBStream:ZStreamRec2;
                       Buffer:Pointer;
                       Const Length:Cardinal):Integer;
Begin
  Result:=0;
  With ZLIBStream, ZLIBStream.ZLIB Do
  Begin
    // Set data to be compressed
    next_in := Buffer;
    avail_in := Length;

    // Compress all the data avaliable to compress
    While avail_in > 0 Do
    Begin
      Result := Deflate(ZLIB, Z_NO_FLUSH);

      // In case the result was not sucessfull
      If (Result<>Z_OK) Then
      Begin
        RaiseError('ZLib error.'+zliberrors[Result]);
        Exit;
      End Else
        Result:=Avail_Out;

      // The whole buffer was used, save data to stream and restore buffer
      if ZLIBStream.ZLIB.avail_out <= 0 then
      begin
        {Writes this IDAT chunk}
        WriteIDAT(Source, Data, MaxIDATSize);

        {Restore buffer}
        next_out := Data;
        avail_out := MaxIDATSize;
      end;
    end;
  end;
End;

Procedure FinishIDATZlib(var ZLIBStream: ZStreamRec2);
begin
  with ZLIBStream, ZLIBStream.ZLIB do
  begin
    {Set data to be compressed}
    next_in := nil;
    avail_in := 0;

    while deflate(ZLIB,Z_FINISH) <> Z_STREAM_END do
    begin
      {Writes this IDAT chunk}
      WriteIDAT(Source, Data, MaxIDATSize - avail_out);
      {Re-update buffer}
      next_out := Data;
      avail_out := MaxIDATSize;
    end;

    if avail_out < MaxIDATSize then
      {Writes final IDAT}
      WriteIDAT(Source, Data, MaxIDATSize - avail_out);

  end {with ZLIBStream, ZLIBStream.ZLIB};
end;

Procedure Process_TRNS(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);
Begin
  If (PNG.Header.ColorType<>COLOR_PALETTE) Then
  Begin
    Source.Skip(Chunk.Size);
    Exit;
  End;

  Source.Read(@PNG.PaletteAlpha, Chunk.Size);
End;

Procedure Process_IDAT(PNG:PNGLoader; Chunk:PPNGChunk; Source:Stream);
Var
  ZLIBStream: ZStreamRec2;
Begin
  ZLIBStream := ZLIBInitInflate(Source);  // Initializes decompression

  GetPixelInfo(PNG.Header, PNG.BytesPerRow, PNG.Offset);
  PNG.EndPos:=Source.Position+Chunk.Size;

  GetMem(PNG.RowBuffer[False], Succ(PNG.BytesPerRow));
  GetMem(PNG.RowBuffer[True], Succ(PNG.BytesPerRow));

  FillChar(PNG.RowBuffer[False]^, Succ(PNG.BytesPerRow),0);
  // Set the variable to alternate the Row_Buffer item to use
  PNG.RowUsed:=True;

  // Call interlace method
  Case PNG.Header.InterlaceMethod of
    imNone:  PNG.DecodeNonInterlaced(ZLIBStream);
    Else
      RaiseError('Interlace method not supported in '+Source.Name);
//    imAdam7: DecodeInterlacedAdam7(stream, ZLIBStream, size, crcfile);
  End;

  // Terminates decompression
  ZLIBTerminateInflate(ZLIBStream);

  // Free memory
  FreeMem(PNG.RowBuffer[False]);
  FreeMem(PNG.RowBuffer[True]);
End;

// Copy 8 bits RGB image
Procedure PNGLoader.CopyNonInterlacedRGB(Src,Dest:PByte);
Var
  I:Integer;
  {$IFDEF PIXEL8}
  Temp:Color;
  {$ENDIF}
  {$IFDEF PIXEL32}
  Color:PColor;
  {$ENDIF}
Begin
  {$IFDEF PIXEL8}
  Temp.A := 255;
  {$ENDIF}
  {$IFDEF PIXEL32}
  Color := PColor(Dest);
  {$ENDIF}
  With Buffer Do
  For I:=0 To Pred(Width) Do
  Begin
    {$IFDEF PIXEL8}
    Temp.R :=Byte(Src^);Inc(Src);
    Temp.G :=Byte(Src^);Inc(Src);
    Temp.B :=Byte(Src^);Inc(Src);
    Temp.A := 255;
    Dest^ := ColorRGB32To8(Temp);
    Inc(Dest);
    {$ENDIF}

    {$IFDEF PIXEL32}
    {$IFDEF BGR}
    Color.R := Byte(Src^);Inc(Src);
    Color.G := Byte(Src^);Inc(Src);
    Color.B := Byte(Src^);Inc(Src);
    {$ENDIF}
    {$IFDEF RGB}
    Color.R := Byte(Src^);Inc(Src);
    Color.G := Byte(Src^);Inc(Src);
    Color.B := Byte(Src^);Inc(Src);
    {$ENDIF}
    Color.A := 255;
    Inc(Color);
    {$ENDIF}
  End;
End;

// Copy 8 bits per sample RGB images followed by an alpha byte
Procedure PNGLoader.CopyNonInterlacedRGBAlpha(Src,Dest:PByte);
Var
  I:Integer;
  {$IFDEF PIXEL8}
  Temp:Color;
  Color:PByte;
  {$ENDIF}
  {$IFDEF PIXEL32}
  Color:PColor;
  {$ENDIF}
Begin
  Color := Pointer(Dest);
  With Buffer Do
  For I:=0 To Pred(Width) Do
  Begin
    {$IFDEF PIXEL8}
    Temp.R :=Byte(Src^);Inc(Src);
    Temp.G :=Byte(Src^);Inc(Src);
    Temp.B :=Byte(Src^);Inc(Src);
    Temp.A :=Byte(Src^);Inc(Src);
    Color^:=ColorRGB32To8(Temp);
    {$ENDIF}

    {$IFDEF PIXEL32}
    {$IFDEF BGR}
    Color.R := Byte(Src^);Inc(Src);
    Color.G := Byte(Src^);Inc(Src);
    Color.B := Byte(Src^);Inc(Src);
    {$ENDIF}
    {$IFDEF RGB}
    Color.R := Byte(Src^);Inc(Src);
    Color.G := Byte(Src^);Inc(Src);
    Color.B := Byte(Src^);Inc(Src);
    {$ENDIF}

    Color.A := Byte(Src^);Inc(Src);
    If Color.A = 0 Then
      Cardinal(Color^):=0;
    {$ENDIF}
    Inc(Color);
  End;
end;

// Copy types using palettes (1, 4 or 8 bits per pixel)
Procedure PNGLoader.CopyNonInterlacedPalette(Src,Dest:PByte);
Var
  I:Integer;
  ByteData,N,K:Byte;
  {$IFDEF PIXEL8}
  Temp:Color;
  {$ENDIF}
Begin
  N := (8 Div Header.BitDepth);
  K := (8 - Header.BitDepth);

  With Buffer Do
  For I:=0 To Pred(Width) Do
  Begin
    // Copy pixel values
    ByteData := pByteArray(Src)^[I Div N];

    If Header.BitDepth<8 Then
    Begin
      // Moves the bits we need to the right
      ByteData := (ByteData Shr ((8 - Header.BitDepth) - (I Mod N) * Header.BitDepth));
      // Discard the unwanted pixels
      ByteData := ByteData And ($FF Shr K);
    End;

    {$IFDEF PIXEL8}
    Temp.R := Palette[ByteData].R;
    Temp.G := Palette[ByteData].G;

    Temp.B := Palette[ByteData].B;
    Temp.A := PaletteAlpha[ByteData];
    Dest^ := ColorRGB32To8(Temp);
    Inc(Dest);
    {$ENDIF}

    {$IFDEF PIXEL32}
    Byte(Dest^):=Palette[ByteData].R; Inc(Dest);
    Byte(Dest^):=Palette[ByteData].G; Inc(Dest);
    Byte(Dest^):=Palette[ByteData].B; Inc(Dest);
    Byte(Dest^):=PaletteAlpha[ByteData]; Inc(Dest);
    {$ENDIF}
  End;
End;

Procedure PNGLoader.CopyNonInterlacedGrayscaleAlpha(Src,Dest:PByte);
Var
  I:Integer;
  {$IFDEF PIXEL8}
  Color:PByte;
  Temp:TERRA_Color.Color;
  {$ENDIF}
  {$IFDEF PIXEL32}
  Color:PColor;
  {$ENDIF}
Begin
  Color:=Pointer(Dest);
  With Buffer Do
  For I:=0 To Pred(Width) Do
  Begin
    {$IFDEF PIXEL8}
    Temp.R := Byte(Src^);
    Temp.G := Temp.R;
    Temp.B := Temp.R;Inc(Src);
    Temp.A := Byte(Src^);Inc(Src);
    Color^:= ColorRGB32To8(Temp);
    {$ENDIF}

    {$IFDEF PIXEL32}
    Color.R := Byte(Src^);
    Color.G := Byte(Src^);
    Color.B := Byte(Src^);Inc(Src);
    Color.A := Byte(Src^);Inc(Src);
    If Color.A=0 Then
      Cardinal(Color^):=0;
    {$ENDIF}
    Inc(Color);
  End;
End;

Procedure PNGLoader.CopyNonInterlacedGrayscale16(Src,Dest:PByte);
Var
  I:Integer;
  A,B:Byte;
  Color:PColor;
Begin
  Color:=Pointer(Dest);
  With Buffer Do
  For I:=0 To Pred(Width) Do
  Begin
    A := Byte(Src^);Inc(Src);
    B := Byte(Src^);Inc(Src);
    Color^:=ColorGrey(Trunc((A*256+B)/(256)), 255);
    Inc(Color);
  End;
End;


Procedure PNGLoader.DecodeNonInterlaced(Var ZLIBStream:ZStreamRec2);
Var
  J:Cardinal;
  Data:PColor;
  CopyProc:Procedure(Src,Dest:PByte) Of Object;
Begin
  CopyProc:=Nil;
  // Determines the method to copy the image data
  Case Header.ColorType of
   COLOR_RGB:
       Case Header.BitDepth Of
        8:  CopyProc := CopyNonInterlacedRGB;
        16: RaiseError('Decoder.NonInterlacedRGB16 not implemented!');
       End;
    COLOR_PALETTE, COLOR_GRAYSCALE:
       Case Header.BitDepth Of
        1, 2, 4, 8:
          CopyProc := CopyNonInterlacedPalette;
        16: RaiseError('Decoder.CopyNonInterlacedGrayscale16 not implemented!');
       End;
   COLOR_RGBALPHA:
      Case Header.BitDepth of
        8  : CopyProc := CopyNonInterlacedRGBAlpha;
        16: CopyProc := CopyNonInterlacedGrayscale16;
      End;
  COLOR_GRAYSCALEALPHA:
      Case Header.BitDepth of
        8  : CopyProc := CopyNonInterlacedGrayscaleAlpha;
       16  : RaiseError('Decoder.CopyNonInterlacedGrayscaleAlpha16 not implemented!');
      End;
  End;

  // Get the image data pointer
  Data := Buffer.Pixels;

  // Reads each line
  With Buffer Do
  For J:=0 To Pred(Height) Do
  Begin
    // Read this line Row_Buffer[RowUsed][0] is the filter type for this line
    IDATZlibRead(ZLIBStream, @RowBuffer[RowUsed][0], Succ(BytesPerRow), EndPos);

    // Filter the current row
    FilterRow;

    // Copies non interlaced row to image
    CopyProc(@RowBuffer[RowUsed][1], PByte(Data));

    // Invert line used
    RowUsed := Not RowUsed;

    Inc(Data, Width);
  End;
End;


// Filter the current line
Procedure PNGLoader.FilterRow;
Var
  I:Cardinal;
  Paeth:Byte;
  PP,Left,Above,AboveLeft:Integer;
Begin
  // Test the filter
  Case RowBuffer[RowUsed]^[0] Of
    FILTER_NONE: Begin
                 End;

    FILTER_SUB:
      For I:=Succ(Offset) To BytesPerRow Do
        RowBuffer[RowUsed][I]:=(RowBuffer[RowUsed][I] + RowBuffer[RowUsed][I-Offset]) And 255;
                                                          
    FILTER_UP:
      For I:=1 To BytesPerRow Do
        RowBuffer[RowUsed][I]:=(RowBuffer[RowUsed][I] + RowBuffer[Not RowUsed][I]) And 255;

    FILTER_AVERAGE:
      For I:=1 To BytesPerRow Do
      Begin
        // Obtains up and left pixels
        Above:=RowBuffer[Not RowUsed][I];
        If Pred(I)<Offset Then
          Left:=0
        Else
          Left:=RowBuffer[RowUsed][I-Offset];

        // Calculates
        RowBuffer[RowUsed][I]:=(RowBuffer[RowUsed][I]+(Left + Above) Div 2) And $FF;
      End;

    FILTER_PAETH:
      Begin
      // Initialize
        Left:=0;
        AboveLeft:=0;

        // Test each byte
        For I:=1 To BytesPerRow Do
        Begin
          // Obtains left and top-left pixels
          If (Pred(I) >= Offset) Then
          Begin
            Left:= RowBuffer[RowUsed][I-Offset];
            AboveLeft := RowBuffer[Not RowUsed][I-Offset];
          End;

          // Obtains above pixel
          Above:=RowBuffer[Not RowUsed][I];

          // Obtains current pixel and paeth predictor
          Paeth:=RowBuffer[RowUsed][I];
          PP := PaethPredictor(Left, Above, AboveLeft);

          // Calculates
          RowBuffer[RowUsed][I] := (Paeth + PP) And $FF;
        End;
      End;

  End;
End;

Procedure PNGLoad(Source:Stream; MyImage:Image);
Var
  I,J:Integer;
  Signature:Array[0..7] Of AnsiChar;
  HasIDAT:Boolean;
  Chunk:LPNGChunk;
  ChunkHandler:LPNGChunkHandler;
  Loader:PNGLoader;
Begin
  {$IFDEF DEBUG_CORE}
  Log(logDebug, 'PNG', 'Got PNG stream: '+Source.Name+' -> '+IntToString(Source.Position)+' / '+IntToString(Source.Size));

  Log(logDebug, 'PNG', 'Reading header...');
  {$ENDIF}
  Source.Read(@Signature[0],8);

  If Signature<>PNGSignature then
  Begin
    RaiseError('Invalid header.');
    Exit;
  End;

  HasIDAT := False;
  ChunkHandler := Nil;

  Loader := PNGLoader.Create;
  Loader.Buffer := MyImage;
  For I:=0 To 255 Do
    Loader.PaletteAlpha[I] := 255;

  // Load chunks
  Repeat
    {$IFDEF DEBUG_CORE}
    Log(logDebug, 'PNG', 'Reading chunks...'+IntToString(Source.Position));
    {$ENDIF}

    Source.Read(@Chunk.Size, 4);
    ByteSwap32(Cardinal(Chunk.Size));
    Source.Read(@Chunk.Name[0], 4);

    {$IFDEF DEBUG_CORE}
    Log(logDebug, 'PNG', 'Found chunk: '+Chunk.Name + ' , size: '+IntToString(Chunk.Size));
    {$ENDIF}

    //DisplayMessage('Chunk:' +Chunk.Name);
    // The first chunk need to be a header chunk
    If (Not Assigned(ChunkHandler))And(Chunk.Name <> 'IHDR') then
    Begin
      RaiseError('Header not found.');
      Exit;
    End;

    // Has a previous IDAT
    If (HasIDAT And (Chunk.Name='IDAT')) Or (Chunk.Name='cHRM') Then
    Begin
      Source.Skip(Chunk.Size+4);
      Continue;
    End;

    // Tell it has an IDAT chunk
    If Chunk.Name='IDAT' Then
     HasIDAT:=True;

    // Find chunk handler
    J:=-1;
    For I:=0 To Pred(ChunkListSize) Do
    If ChunkList[I].Name=Chunk.Name Then
    Begin
      J:=I;
      Break;
    End;

    // If chunk not registered, skip it
    If J=-1 Then
      ChunkHandler:=SkipChunk
    Else
      ChunkHandler:=ChunkList[I].Handler;

    ChunkHandler(Loader,@Chunk,Source);

    // Skip CRC
    Source.Skip(4);

  // Terminates when it reaches the IEND chunk
  Until (Chunk.Name='IEND');

  // Check if there is data
  If Not HasIDAT Then
  Begin
    RaiseError('Image data not found.');
    Exit;
  End;

  {
  With Loader Do
  Begin
    If (Header.ColorType=COLOR_GRAYSCALEALPHA)
    Or (Header.ColorType=COLOR_RGBALPHA) Then
      Buffer.HasAlpha:=True;
  End;}

  ReleaseObject(Loader);
End;

Procedure EncodeNonInterlacedRGB(Src:PColor; Dest:PByte; Width:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Width) Do
  Begin
    Dest^ := Src.R; Inc(Dest);
    Dest^ := Src.G; Inc(Dest);
    Dest^ := Src.B; Inc(Dest);
    Inc(Src);
  End;
End;

Procedure EncodeNonInterlacedRGBA(Src:PColor; Dest:PByte; Width:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(Width) Do
  Begin
    Dest^ := Src.R; Inc(Dest);
    Dest^ := Src.G; Inc(Dest);
    Dest^ := Src.B; Inc(Dest);
    Dest^ := Src.A; Inc(Dest);
    Inc(Src);
  End;
End;

Procedure PNGSave(Dest:Stream; MyImage:Image; Const Options:TERRAString='');
Const
  BUFFER = 5;
Var
  ZLIBStream:ZStreamRec2;
  Encode_Buffer: Array[0..5] of PByteArray;
  LineSize,Offset:Cardinal;
  Header:PNGHeader;

Function FilterRow:Byte;
Var
  Run, LongestRun:Integer;
  ii, jj:Cardinal;
  Last, Above, LastAbove: Byte;
Begin
  // FILTER_SUB
    for ii := 0 to Pred(LineSize) do
    begin
      if (ii >= Offset) then
        last := Encode_Buffer[BUFFER]^[ii - Offset]
      else
        last := 0;
      Encode_Buffer[FILTER_SUB]^[ii] := Byte(Encode_Buffer[BUFFER]^[ii] - last);
    end;

  // FILTER_UP
    for ii := 0 to Pred(LineSize) do
      Encode_Buffer[FILTER_UP]^[ii] := Byte(Encode_Buffer[BUFFER]^[ii] - Encode_Buffer[FILTER_NONE]^[ii]);

  // FILTER_AVERAGE
    for ii := 0 to Pred(LineSize) do
    begin
      if (ii >= Offset) then
        last := Encode_Buffer[BUFFER]^[ii - Offset]
      else
        last := 0;
      // Get the pixel above
      above := Encode_Buffer[FILTER_NONE]^[ii];

      // Calculates formula to the average pixel
      Encode_Buffer[FILTER_AVERAGE]^[ii] := Byte(Encode_Buffer[BUFFER]^[ii] - (above + last) div 2 );
    End;

  // FILTER_PAETH
    last := 0;
    lastabove := 0;
    For ii := 0 to Pred(LineSize) Do
    begin
      {In case this pixel is not the first in the line obtains the}
      {previous one and the one above the previous}
      if (ii >= Offset) then
      begin
        last := Encode_Buffer[BUFFER]^[ii - Offset];
        lastabove := Encode_Buffer[FILTER_NONE]^[ii - Offset];
      end;
      // Obtains the pixel above
      above := Encode_Buffer[FILTER_NONE]^[ii];
      // Calculate paeth filter for this byte
      Encode_Buffer[FILTER_PAETH]^[ii] := Byte(Encode_Buffer[BUFFER]^[ii] - PaethPredictor(last, above, lastabove));
    end;

  {Now calculates the same line using no filter, which is necessary}
  {in order to have data to the filters when the next line comes}
  Move(Encode_Buffer[BUFFER]^[0], Encode_Buffer[FILTER_NONE]^[0], LineSize);

  {Check which filter is the best by checking which has the larger}
  {sequence of the same byte, since they are best compressed}
  LongestRun := 0; Result := FILTER_NONE;
  for ii := FILTER_NONE TO FILTER_PAETH do
  Begin
    Run := 0;

    {Check using a sequence of four bytes}
    for jj := 2 to Pred(LineSize) do
      if (Encode_Buffer[ii]^[jj] = Encode_Buffer [ii]^[jj-1]) or
          (Encode_Buffer[ii]^[jj] = Encode_Buffer [ii]^[jj-2]) then
            inc(Run);  {Count the number of sequences}

    {Check if this one is the best so far}
    if (Run > LongestRun) then
    begin
      Result := ii;
      LongestRun := Run;
    end;
  end;
End;

Procedure EncodeNonInterlaced(Var ZLIBStream:ZStreamRec2);
Var
  J:Cardinal;
  Filter:Byte;
  Pixels:PColor;
  CopyProc: procedure(Src:PColor; Dest:PByte; Width:Integer);
Begin
  CopyProc := Nil;
  Case Header.ColorType of
    COLOR_RGB: CopyProc := EncodeNonInterlacedRGB;
    COLOR_RGBALPHA: CopyProc := EncodeNonInterlacedRGBA;
  End;

  Pixels := MyImage.Pixels;
  For J:=0 To Pred(MyImage.Height) Do
  Begin
    CopyProc(Pixels, @Encode_Buffer[BUFFER][0], MyImage.Width);
    Filter := FilterRow;

    // Compress data
    IDATZlibWrite(ZLIBStream, @Filter, 1);
    IDATZlibWrite(ZLIBStream, @Encode_Buffer[Filter][0], LineSize);

    // Adjust pointers to the actual image data
    Inc(Pixels, MyImage.Width);
  End;

  // Compress and finishes copying the remaining data
  FinishIDATZlib(ZLIBStream);
end;

Procedure WriteImageData(Filter:Byte);
Var
  I:Integer;
Begin
  GetPixelInfo(Header, LineSize, Offset);

  For I:=0 To BUFFER Do
  Begin
    GetMem(Encode_Buffer[I], LineSize);
    FillChar(Encode_Buffer[I]^, LineSize, 0);
  End;

  ZLIBStream := ZLIBInitDeflate(Dest);
  EncodeNonInterlaced(ZLIBStream);
  ZLIBTerminateDeflate(ZLIBStream);

  // Release allocated memory
  For I:=0 To BUFFER Do
    FreeMem(Encode_Buffer[I], LineSize);
End;

Var
  Signature:Array[0..7] Of TERRAChar;
  OP,CRC:Cardinal;
  Depth:Integer;
  Parser:INIParser;
Begin
  Move(PNGSignature, Signature, 8);
  Dest.Write(@Signature[0], 8);

  Header.Width := MyImage.Width;
  Header.Height := MyImage.Height;
  Header.BitDepth:=8;
  Header.InterlaceMethod:=imNone;
  Header.CompressionMethod:=0;
  Header.FilterMethod:=0;

  Depth := 32;
  If Options<>'' Then
  Begin
    Parser := INIParser.Create;
    Parser.ParseCommas := True;
    Parser.AddToken('Depth',tkInteger,@Depth);
    Parser.LoadFromString(Options);
    ReleaseObject(Parser);
  End;

  If Depth=32 Then
    Header.ColorType:=COLOR_RGBAlpha
  Else
    Header.ColorType:=COLOR_RGB;

  OP:=WriteChunkHeader(Dest, 'IHDR', SizeOf(Header));
  ByteSwap32(Cardinal(Header.Width));
  Dest.Write(@Header.Width,4);
  ByteSwap32(Cardinal(Header.Width));
  ByteSwap32(Cardinal(Header.Height));
  Dest.Write(@Header.Height,4);
  ByteSwap32(Cardinal(Header.Height));
  Dest.Write(@Header.BitDepth,1);
  Dest.Write(@Header.ColorType,1);
  Dest.Write(@Header.CompressionMethod,1);
  Dest.Write(@Header.FilterMethod,1);
  Dest.Write(@Header.InterlaceMethod,1);
  CRC:= GetCRC32(Dest, OP, SizeOf(Header)+4, _PNGCRCTable);
  ByteSwap32(Cardinal(CRC));
  Dest.Write(@CRC,4);

  WriteImageData(FILTER_PAETH);

  WriteChunkHeader(Dest, 'IEND', 0);
  CRC:= GetCRC32('IEND', _PNGCRCTable);
  ByteSwap32(Cardinal(CRC));
  Dest.Write(@CRC, 4);
End;

Function ValidatePNG(Source:Stream):Boolean;
Var
  ID:Array[1..3] Of AnsiChar;
Begin
  Source.Skip(1);
  Source.Read(@ID,3);
  Result:=(ID='PNG');
End;

Initialization
  Log(logDebug, 'PNG', 'Initializing');
  _PNGCRCTable := CRC32Table.Create(PNGPolynomial);
  
  RegisterImageFormat('PNG', ValidatePNG, PNGLoad, PNGSave);
Finalization
  ReleaseObject(_PNGCRCTable);
End.

