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
 * TERRA_JPG
 * Implements JPG loader
 ***********************************************************************************************************************
}

Unit TERRA_JPG;

{$i terra.inc}

{$IFDEF FPC}
{$DEFINE USEPASJPEGLIB} // Enable this switch to use the PasJPEG library
                         // If you disable this, be sure to removed any references to
                         // PasJPEG in your library path, otherwise the compiler
                         // will report an error about wrong version of JPEG unit
{$ELSE}
{$IFDEF CONDITIONALEXPRESSIONS}
{$DEFINE USEPASJPEGLIB}
{$ENDIF}
{$ENDIF}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Stream, TERRA_Image, TERRA_Log, TERRA_FileFormat, TERRA_Application,
    {$IFDEF USEPASJPEGLIB}
     jmorecfg, jpeglib, jerror, jdeferr, jdapimin,
     jdapistd, jdmarker, jdmaster
    {$ELSE}Classes, Graphics, JPEG{$ENDIF};

Type
  JPEGFormat = Class(TERRAFileFormat)
    Public
      Function Identify(Source:Stream):Boolean; Override;
      Function Load(Target:TERRAObject; Source:Stream):Boolean; Override;

      {$IFNDEF USEPASJPEGLIB}
      Function Save(Target:TERRAObject; Dest:Stream):Boolean; Override;
      {$ENDIF}
  End;

Implementation
Uses TERRA_Error, TERRA_EngineManager, TERRA_FileStream, TERRA_FileUtils, TERRA_Color;

{$IFDEF USEPASJPEGLIB}
Const
  INPUT_BUF_SIZE = 4096;

Type
  PDecoderSource=^LDecoderSource;
  LDecoderSource=Record
    pub:jpeg_source_mgr;	//  public fields
    Stream:Stream;		    //  source stream}
    buffer:JOCTET_FIELD_PTR;  //  start of buffer}
  End;

  PImageDest = ^ImageDest;
  ImageDest = Record
    Output:TERRAImage;  // Target ouput image
    Color:PColorRGBA;
    Row:Integer;

    // image info
    LineWidth:JDIMENSION;   // JSAMPLEs per row
    Grayscale:Boolean;      // grayscale or quantized color table ?

    // pixelrow buffer
    Buffer:JSAMPARRAY;        // pixelrow buffer
    BufferHeight:JDIMENSION;  // normally, we'll use 1
  End;

Procedure DecoderSource_Init(cinfo : j_decompress_ptr);
Begin
  // nothing to do here
End;

Procedure DecoderSource_Finish(cinfo:j_decompress_ptr);
Begin
  // nothing to do here
End;

Function DecoderSource_FillInputBuffer(cinfo:j_decompress_ptr):Boolean;
Var
  Source:PDecoderSource;
  BytesRead:Integer;
Begin
  Source := PDecoderSource(cinfo.src);
  BytesRead := Source.Stream.Read(Source.Buffer,INPUT_BUF_SIZE);
  If (BytesRead<=0) Then
  Begin
    WARNMS(j_common_ptr(cinfo), JWRN_JPEG_EOF);

    // Insert a fake EOI marker
    Source.Buffer[0]:=JOCTET($FF);
    Source.Buffer[1]:=JOCTET(JPEG_EOI);
    BytesRead:=2;
  End;

  Source.pub.next_input_byte:=JOCTETptr(Source.buffer);
  Source.pub.bytes_in_buffer:=BytesRead;
  Result:=True;
End;

Procedure DecoderSource_SkipInputData(cinfo:j_decompress_ptr;
                                      BytesToSkip:Long);
Var
  Source:PDecoderSource;
begin
  Source:=PDecoderSource(cInfo.src);
  If (BytesToSkip>0) Then
  Begin
    While (BytesToSkip > long(Source.pub.bytes_in_buffer)) Do
    Begin
      Dec(BytesToSkip, long(Source.pub.bytes_in_buffer));
      DecoderSource_FillInputBuffer(cInfo);
      { note we assume that fill_input_buffer will never return FALSE,
        so suspension need not be handled. }
    End;
    Inc(Source.pub.next_input_byte, size_t(BytesToSkip));
    Dec(Source.pub.bytes_in_buffer, size_t(BytesToSkip));
  End;
End;

Procedure output_message(cinfo:j_common_ptr);
Var
  Buffer:AnsiString;
Begin
  // Create the message
  //cInfo.err.format_message(cinfo, buffer); // TODO
//  Log(logWarning,'JPG','PasJPEG.InternalError: ' + Buffer);
End;

Function jpeg_LEAF_error(Var err:jpeg_error_mgr):jpeg_error_mgr_ptr;
Begin
  jpeg_std_error(err);
  err.output_message := output_message;
  Result:=@err;
End;

procedure jpeg_stream_src(cinfo : j_decompress_ptr; Const Stream:Stream);
Var
  Source:PDecoderSource;
Begin
  If (Not Assigned(cInfo.src)) Then
  Begin // first time for this JPEG object?
    cInfo.src:=jpeg_source_mgr_ptr(cInfo.mem.alloc_small(j_common_ptr(cinfo),
                                   JPOOL_PERMANENT, SizeOf(LDecoderSource)));
    Source:=PDecoderSource(cInfo.src);
    Source.Buffer:=JOCTET_FIELD_PTR(cInfo.Mem.Alloc_small(j_common_ptr(cinfo), JPOOL_PERMANENT,
				                                                  INPUT_BUF_SIZE*SizeOf(JOCTET)));
  End;

  Source:=PDecoderSource (cInfo.src);
  // override pub's method pointers
  Source.pub.init_source := DecoderSource_Init;
  Source.pub.fill_input_buffer := DecoderSource_FillInputBuffer;
  Source.pub.skip_input_data := DecoderSource_SkipInputData;
  Source.pub.term_source := DecoderSource_Finish;
  Source.pub.resync_to_restart := jpeg_resync_to_restart; {use default method}

  // define our fields
  Source.Stream := Stream;
  Source.pub.bytes_in_buffer := 0;   // forces fill_input_buffer on first read
  Source.pub.next_input_byte := Nil; // until buffer loaded
end;

Function jinit_ImageDest(cinfo:j_decompress_ptr; Target:TERRAImage):PImageDest;
Var
  Dest:PImageDest;
Begin
  Dest := PImageDest(cInfo.Mem.alloc_small(j_common_ptr(cinfo),JPOOL_IMAGE, SizeOf(ImageDest)));
  Dest.Output := Target;
  Dest.Color := Target.RawPixels;
  Dest.Row:=0;

  // image info
  jpeg_calc_output_dimensions(cInfo);
  Dest.LineWidth:=cInfo.Output_width * Cardinal(cInfo.Output_Components);

  If (cInfo.out_color_space=JCS_GRAYSCALE) Then
    Dest.Grayscale:=True
  Else
  If (cInfo.out_color_space=JCS_RGB) Then
    If (cInfo.quantize_colors) then
      Dest.Grayscale:=True
    Else
      Dest.Grayscale:=False
  Else
    Halt; //ERREXIT(j_common_ptr(cinfo), JERR_BMP_COLORSPACE);

  // decompress buffer
  Dest.Buffer:=cInfo.mem.alloc_sarray(j_common_ptr(cinfo), JPOOL_IMAGE,
                                      Dest.LineWidth, JDIMENSION (1));
  Dest.BufferHeight:=1;

  // result
  Result:=Dest;
End;

Procedure WritePixelRow(cinfo:j_decompress_ptr; Dest:PImageDest; RowCount:JDIMENSION);
Var
  I:JDIMENSION;
  Inptr:JSAMPLE_PTR;
Begin
  InPtr:=JSAMPLE_PTR(Dest.Buffer^[0]);
  If Not Dest.Grayscale Then
  Begin
    Dest.Output.LineDecodeBGR24(InPtr, Dest.Row);
    Inc(Dest.Row);
  End Else
  Begin
    RaiseError('Grayscale JPG unsupported.');
    Exit;
  End;
End;
{$ENDIF}

{ JPEGFormat }
Function JPEGFormat.Identify(Source: Stream): Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID, 4);
  Result := CompareFileHeader(ID, 'ÿØÿà');
End;

Function JPEGFormat.Load(Target: TERRAObject; Source: Stream): Boolean;
{$IFDEF USEPASJPEGLIB}
Var
  cinfo:jpeg_decompress_struct;
  jerr:jpeg_error_mgr;
  Dest:PImageDest;
  ScanlineCount:JDIMENSION;
  Image:TERRAImage;
Begin
  Image := TERRAImage(Target);
  //  Initialize the JPEG decompression object error handling.
  cinfo.err:=jpeg_LEAF_error(jerr);
  jpeg_create_decompress(@cinfo);

  // Specify data source for decompression
  jpeg_stream_src(@cinfo, Source);

  cInfo.dither_mode := JDITHER_NONE;
  cInfo.dct_method := JDCT_FASTEST;
  cInfo.two_pass_quantize := False;
  cInfo.do_fancy_upsampling := False;
  cInfo.do_block_smoothing := False;

  // Read file header, set default decompression parameters
  jpeg_read_header(@cInfo, True);

  Image.New(cInfo.image_width, cInfo.image_height);
  Dest:=jinit_ImageDest(@cInfo, Image);

  // Start decompressor
  jpeg_start_decompress(@cInfo);

  While (cInfo.Output_Scanline<cInfo.Output_Height) Do
  Begin
    ScanlineCount := jpeg_read_scanlines(@cInfo, Dest.buffer, Dest.BufferHeight);
    WritePixelrow(@cInfo, Dest, ScanlineCount);
  End;

  jpeg_finish_decompress(@cInfo);
  jpeg_destroy_decompress(@cInfo);
End;
{$ELSE}
Var
  I, J:Integer;
  N:Byte;
  BMP:TBitmap;
  JPG:TJPEGImage;
  Stream:TMemoryStream;
  P:PColor;
  Image:TERRAImage;
Begin
  Image := TERRAImage(Target);

  Stream:=TMemoryStream.Create;
  Stream.SetSize(Source.Size);
  Source.Read(Stream.Memory,Source.Size);

  JPG:=TJPEGImage.Create;
  JPG.LoadFromStream(Stream);
  ReleaseObject(Stream);

  // Create Bitmap
  BMP:=TBitmap.Create;
  BMP.pixelformat:=pf32bit;
  BMP.width:=JPG.width;
  BMP.height:=JPG.height;
  BMP.canvas.draw(0,0,JPG);        // Copy the JPEG onto the Bitmap

  Image.New(BMP.Width,BMP.Height);

  For J:=0 to Pred(Image.Height) do
  Begin
    P := PColor(Integer(Image.Pixels)+(J*Image.Width*4));
    Move(BMP.Scanline[J]^,P^,Image.Width*4);
    For I:=0 to Pred(Image.Width) do
    Begin
      {$IFDEF FPC}
      N := P.R;
      P.R := P.B;
      P.B := N;
      {$ENDIF}
      P.A := 255;
      Inc(P);
    End;
  End;

  ReleaseObject(BMP);
  ReleaseObject(JPG);

  Image.Process(IMP_SwapChannels);
End;
{$ENDIF}

{$IFNDEF USEPASJPEGLIB}
Function JPEGFormat.Save(Target: TERRAObject; Dest: Stream): Boolean;
Const
  BufferSize = 1024;
Var
  J:Integer;
  JPG:TJPEGImage;
  Bitmap:TBitmap;
  Stream:TMemoryStream;
  Buffer:Array[0..Pred(BufferSize)] Of Byte;
  Count,Size:Integer;
  Parser:INIParser;
  Quality:Integer;
  Image:TERRAImage;
Begin
  Image := TERRAImage(Target);
  Quality:=100;
  If Options<>'' Then
  Begin
    Parser := INIParser.Create;
    Parser.ParseCommas:=True;
    Parser.AddToken('Quality',tkInteger,@Quality);
    Parser.LoadFromString(Options);
    ReleaseObject(Parser);
  End;

  Bitmap:=TBitmap.Create;
  Bitmap.PixelFormat:=pf32bit;
  Bitmap.Width:=Image.Width;
  Bitmap.Height:=Image.Height;

  Image.Process(IMP_SwapChannels);
  For J:=0 to Pred(Image.Height) do
    Move(PColor(Integer(Image.Pixels)+(J*Image.Width*4))^,Bitmap.Scanline[J]^,Image.Width*4);
  Image.Process(IMP_SwapChannels);

  JPG:=TJPEGImage.Create;
  JPG.Assign(Bitmap);
  JPG.CompressionQuality:=Quality;
  JPG.Compress;
  Stream:=TMemoryStream.Create;
  Stream.SetSize(Image.Size);
  JPG.SaveToStream(Stream);

  Size:=Stream.Position;
  Stream.Seek(0,soFromBeginning);

  While (Size>0) Do
  Begin
    Count:=BufferSize;
    If Size<Count Then
      Count:=Size;

    Stream.Read(Buffer, Count);
    Dest.Write(@Buffer[0], Count);
    Dec(Size, Count);
  End;

  ReleaseObject(Stream);
  ReleaseObject(JPG);

  ReleaseObject(Bitmap);
End;
{$ENDIF}

Begin
  Engine.Formats.Add(JPEGFormat.Create(TERRAImage, 'jpg'));
End.
