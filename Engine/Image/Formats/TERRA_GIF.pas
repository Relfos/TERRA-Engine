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
 * TERRA_GIF
 * Implements GIF loader
 ***********************************************************************************************************************
}

Unit TERRA_GIF;

{$I terra.inc}

{$DEFINE DECODEOPTIMIZED}

Interface
Uses TERRA_Error, TERRA_Utils, TERRA_Application, TERRA_IO, TERRA_Image, TERRA_Math, TERRA_Color;

Implementation

Type
  GIFHeader=Packed Record
    Signature:Array[1..3] Of AnsiChar;
    Version:Array[1..3] Of AnsiChar;
    Width,Height:Word;
    Flags:Byte;
    Background:Byte;
    AspectRatio:Byte;
  End;

  GIFImageDescriptor=Packed Record
    x,y:Word;
    xd,yd:word;
    Flags:byte;
  End;

  GIFExtensionBlock=Packed Record
    FunctionCode:Byte;
  End;

  GIFGraphicControlExtension=Packed Record
    BlockSize:Byte;
    Flags:Byte;
    DelayTime:Word;
    TransparencyColor:Byte;
  End;

  GIFRGB=Record
    Red,Green,Blue:Byte;
  End;

Const
  ilStart:Array[1..4] Of longint=(0,4,2,1);
  ilStep:Array[1..4] Of longint=(8,8,4,2);

Type
  PPalette=^GIFPalette;
  GIFPalette=Array[0..255] Of Color;

  GIFLoader=Class
    Protected
      MyImage:Image;
      TransparencyColor:Integer;

      Procedure DecodeGIFLZW(Source:Stream; FrameBuffer:Pointer; Palette:PPalette; Interlaced:Boolean);
      Procedure CopyFrame(FrameBuffer:Pointer; X,Y,FrameWidth,FrameHeight:Word);
      Procedure MaskFrame;

      Procedure Load(Source:Stream);
  End;

Procedure DumpData(Source:Stream);
Var
  Count:Byte;
Begin
  Repeat
    Source.Read(@Count,1);
    Source.Skip(Count);
  Until (Count=0) Or (Source.EOF);
End;

Procedure GIFLoader.MaskFrame;
Var
  Src,Dest:PColor;
  PrevFrame:Pointer;
  I,J:Cardinal;
Begin
  If MyImage.CurrentFrame<=0 Then
    Exit;

  MyImage.SetCurrentFrame(Pred(MyImage.CurrentFrame));
  PrevFrame := MyImage.Pixels;
  MyImage.SetCurrentFrame(Succ(MyImage.CurrentFrame));

  For J:=0 To Pred(MyImage.Height) Do
    For I:=0 To Pred(MyImage.Width) Do
    Begin
      Src := PColor(PtrUInt(PrevFrame)+(J*MyImage.Width*PixelSize)+(I*PixelSize));
      Dest := PColor(PtrUInt(MyImage.Pixels)+(J*MyImage.Width*PixelSize)+(I*PixelSize));
      If (Dest.A=0) And (Src.A=255) Then
        Dest^ := Src^;
    End;
End;

Procedure GIFLoader.CopyFrame(FrameBuffer:Pointer; X,Y,FrameWidth,FrameHeight:Word);
Var
  I:Cardinal;
  PrevFrame:Pointer;
  {$IFDEF PIXEL8}
  Color:PByte;
  {$ENDIF}
  {$IFDEF PIXEL32}
  Color:PColor;
  {$ENDIF}
Begin
  If MyImage.CurrentFrame<=0 Then
  Begin
    Move(FrameBuffer^, MyImage.Pixels^, MyImage.Size);
    Exit;
  End;

  MyImage.SetCurrentFrame(Pred(MyImage.CurrentFrame));
  PrevFrame := MyImage.Pixels;
  MyImage.SetCurrentFrame(Succ(MyImage.CurrentFrame));
  Move(PrevFrame^, MyImage.Pixels^, MyImage.Size);

  For I:=Y To Pred(Y+FrameHeight) Do
  Begin
    {$IFDEF PIXEL8}
    Color:=PByte(PtrUInt(Image.Pixels)+I*Image.Width+X);
    Move(FrameBuffer^,Color^,FrameWidth);
    Inc(PByte(FrameBuffer),FrameWidth);
    {$ENDIF}
    {$IFDEF PIXEL32}
    Color := PColor(PtrUInt(MyImage.Pixels)+I*MyImage.Width*PixelSize+X*PixelSize);
    Move(FrameBuffer^,Color^,FrameWidth*PixelSize);
    Inc(PByte(FrameBuffer),FrameWidth*PixelSize);
    {$ENDIF}
  End;
End;

Procedure GIFLoader.DecodeGIFLZW(Source:Stream; FrameBuffer:Pointer; Palette:PPalette; Interlaced:Boolean);
Var
  xd,yd:Cardinal;
Const
  Tablen=4095;
Type
  PStr=^TStr;
  TStr=Record
    Prefix:Pstr;
    Suffix:Longint;
  End;

  PStrTab=^TStrTab;
  TStrTab=Array[0..TabLen] Of TStr;

Var
  StrTab:PStrTab;
  OldCode,CurCode:Longint;
  Clearcode,EndCode:Longint;
  CodeSize,CodeLen,CodeMask:Longint;
  StrIdx:Longint;
  BitBuf,BitSinBuf:Longint;
  BytBuf:Array[0..255] Of Byte;
  BytInBuf,BytBufIdx:Byte;
  EndOfSrc:Boolean;
  xcnt,ycnt,pcnt,ystep,pass:Cardinal;
  ImagePtr:Pointer;

  Procedure InitStringTable;
  Var
    I:Longint;
  Begin
    System.New(StrTab);
    Clearcode:=1 Shl CodeSize;
    EndCode:=Succ(ClearCode);
    StrIdx:=Succ(EndCode);
    CodeLen:=Succ(CodeSize);
    CodeMask:=Pred(1 SHL CodeLen);
    For I:=0 To Pred(ClearCode) Do
    Begin
      StrTab^[I].Prefix:=Nil;
      StrTab^[I].Suffix:=I;
    End;
    For I:=ClearCode To TabLen Do
    Begin
      StrTab^[I].Prefix:=Nil;
      StrTab^[I].Suffix:=0;
    End;
  End;

  Procedure ClearStringTable;
  Var
    I:Longint;
  Begin
    ClearCode:=1 Shl CodeSize;
    EndCode:=Succ(ClearCode);
    StrIdx:=Succ(EndCode);
    CodeLen:=Succ(CodeSize);
    CodeMask:=Pred(1 Shl CodeLen);
    For I:=ClearCode To TabLen Do
    Begin
      StrTab^[I].Prefix:=Nil;
      StrTab^[I].Suffix:=0;
    End;
  End;

  Procedure DoneStringTable;
  Begin
    Dispose(StrTab);
  End;

  Function GetNextCode:Longint;
  Begin
    While (BitsInBuf<CodeLen) Do
    Begin
      If (BytInBuf=0) Then
      Begin
        Source.Read(@BytInBuf,1);
        If (BytInBuf=0) Then
          EndOfSrc:=True;
        Source.Read(@BytBuf,BytInBuf);
        BytBufIdx:=0;
      End;
      BitBuf:=BitBuf Or (Longint(Byte(BytBuf[BytBufIdx])) Shl BitsInBuf);
      Inc(BytBufIdx);
      Dec(BytInBuf);
      Inc(BitsInBuf,8);
    End;

    Result:=Bitbuf And CodeMask;

    BitBuf:=Bitbuf Shr CodeLen;
    Dec(BitsInBuf,CodeLen);
  End;

  Procedure AddStrToTab(Prefix:PStr;Suffix:Longint);
  Begin
    StrTab^[StrIdx].Prefix:=Prefix;
    StrTab^[StrIdx].Suffix:=Suffix;
    Inc(StrIdx);
    Case StrIdx Of
      0..1:       CodeLen:=1;
      2..3:       CodeLen:=2;
      4..7:       CodeLen:=3;
      8..15:      CodeLen:=4;
      16..31:     CodeLen:=5;
      32..63:     CodeLen:=6;
      64..127:    CodeLen:=7;
      128..255:   CodeLen:=8;
      256..511:   CodeLen:=9;
      512..1023:  CodeLen:=10;
      1024..2047: CodeLen:=11;
      2048..4096: CodeLen:=12;
    End;
      CodeMask:=Pred(1 Shl CodeLen);
  End;

  Function CodeToStr(Code:Longint):PStr;
  Begin
    Result:=Addr(StrTab^[Code]);
  End;

  Procedure WriteStr(S:PStr);
  Begin
    If (Assigned(S^.Prefix)) Then
      WriteStr(S^.Prefix);

    If (ycnt>=yd) Then
    Begin
      If Interlaced Then
      Begin
        While (ycnt>=yd) And (pass<5) Do
        Begin
          Inc(Pass);
          ycnt:=ilstart[Pass];
          ystep:=ilstep[Pass];
          ImagePtr := Pointer(PtrUInt(FrameBuffer)+ycnt*(MyImage.Width*PixelSize));
        End;
      End;
    End;

    {$IFDEF PIXEL8}
    If S^.Suffix = TransparencyColor Then
      PByte(PtrUInt(ImagePtr)+xcnt)^:=0
    Else
      PByte(PtrUInt(ImagePtr)+xcnt)^:=ColorRGB32To8(Palette^[S^.Suffix]);
    {$ENDIF}
    {$IFDEF PIXEL32}
    If S^.Suffix = TransparencyColor Then
      PColor(PtrUInt(ImagePtr)+(xcnt*PixelSize))^ := ColorNull
    Else
      PColor(PtrUInt(ImagePtr)+(xcnt*PixelSize))^:=Palette^[S^.Suffix];
    {$ENDIF}

    Inc(xcnt);
    If (xcnt>=xd) Then
    Begin
      Inc(pcnt);
      xcnt:=0;
      Inc(ycnt,ystep);

      ImagePtr:=Pointer(PtrUInt(ImagePtr)+ystep*(MyImage.Width*PixelSize));
      If Not Interlaced Then
        If (ycnt>=yd) Then
        Begin
          Inc(Pass);
        End;
    End;
  End;

  Function FirstChar(S:PStr):Byte;
  Begin
    While (S^.Prefix<>Nil) Do
      S:=S^.Prefix;

    Result:=S^.Suffix;
  End;

  Begin
    EndOfSrc:=False;
    xd := MyImage.Width;
    yd := MyImage.Height;
    xcnt:=0;
    pcnt:=0;
    If Interlaced Then
    Begin
      Pass:=1;
      ycnt:=ilStart[Pass];
      ystep:=ilStep[Pass];
    End Else
    Begin
      Pass:=4;
      ycnt:=0;
      ystep:=1;
    End;

    ImagePtr:=Pointer(PtrUInt(FrameBuffer)+ycnt*(MyImage.Width*PixelSize));
    OldCode:=0;
    BitBuf:=0;
    BitsInBuf:=0;
    BytInBuf:=0;
    BytBufIdx:=0;
    CodeSize:=0;
    Source.Read(@CodeSize,1);

    InitStringTable;
    CurCode:=GetNextCode;
    While (CurCode<>EndCode) And (Pass<5) And (Not EndOfSrc) Do
    Begin
      If (CurCode=ClearCode) Then
      Begin
        ClearStringTable;
        Repeat
          CurCode:=GetNextCode;
        Until (CurCode<>ClearCode);

        If (CurCode=EndCode) Then Break;

        WriteStr(CodeToStr(curcode));
        OldCode:=CurCode;
      End Else
      Begin
        If (CurCode<StrIdx) Then
        Begin
          WriteStr(CodeToStr(CurCode));
          AddStrToTab(CodeToStr(OldCode),FirstChar(CodeToStr(CurCode)));
          OldCode:=CurCode;
        End Else
        Begin
          If (CurCode>StrIdx) Then
            Break;

          AddStrToTab(CodeToStr(OldCode),FirstChar(CodeToStr(OldCode)));
          WriteStr(CodeToStr(StrIdx-1));
          OldCode:=CurCode;
        End;
      End;

      Curcode:=GetNextCode;
    End;

    DoneStringTable;
    If Not EndOfSrc Then
      DumpData(Source);
  End;


Procedure GIFLoader.Load(Source:Stream);
var
  GIFHeader:TERRA_GIF.GIFHeader;
  GIFBlockID:AnsiChar;
  GIFImageDescriptor:TERRA_GIF.GIFImageDescriptor;
  GIFExtensionBlock:TERRA_GIF.GIFExtensionBlock;
  GIFGraphicControlExtension:TERRA_GIF.GIFGraphicControlExtension;
  Xd,Yd:Longint;

  I,K:Integer;
  Frame:Integer;
  GlobalPalette,LocalPalette:GIFPalette;
  Palette:PPalette;
  RGB:GIFRGB;

  {$IFDEF DECODEOPTIMIZED}
  FrameBuffer:Pointer;
  {$ENDIF}
Begin
  TransparencyColor:=-1;
  Frame:=0;

  {$IFDEF DECODEOPTIMIZED}
  FrameBuffer:=Nil;
  {$ENDIF}

  Source.Read(@GIFHeader,SizeOf(GIFHeader));
  If (GIFHeader.Signature<>'GIF') Then
  Begin
    RaiseError('Invalid header.');
    Exit;
  End;

  If (GIFHeader.Flags And $80<>0) Then
  Begin
    K:=(1 SHL (GIFHeader.Flags And $07+1));

    For I:=0 To Pred(K) Do
    Begin
      Source.Read(@rgb,3);
      {$IFDEF RGB}
      GlobalPalette[i].R := RGB.Red;
      GlobalPalette[i].G := RGB.Green;
      GlobalPalette[i].B := RGB.Blue;
      {$ENDIF}
      {$IFDEF BGR}
      GlobalPalette[i].B := RGB.Red;
      GlobalPalette[i].G := RGB.Green;
      GlobalPalette[i].R := RGB.Blue;
      {$ENDIF}
      GlobalPalette[i].A := 255;
    End;

    For I:=K To 255 Do
      GlobalPalette[i] := ColorBlack;
  End;

  Repeat
    Source.Read(@GIFBlockID,1);
    Case GIFBlockID Of
    ';':;
    ',':Begin //image separator
          Palette:=@GlobalPalette;
          Source.Read(@GIFImageDescriptor,SizeOf(GIFImageDescriptor));
          If (GIFImageDescriptor.Flags And $80<>0) Then
          Begin
            Palette:=@LocalPalette;
            K:=(2 SHL (GIFImageDescriptor.Flags AND $07));
            For I:=0 To Pred(K) Do
            Begin
              Source.Read(@rgb,3);
              LocalPalette[I].R := rgb.Red;
              LocalPalette[I].G := rgb.Green;
              LocalPalette[I].B := rgb.Blue;
              LocalPalette[I].A := 255;
            End;

            For I:=K To 255 Do
              LocalPalette[i] := ColorBlack;
          End;

          xd:= GIFImageDescriptor.xd;
          yd:= GIFImageDescriptor.yd;

          If Frame=0 Then
          Begin
            MyImage.New(xd,yd);
            {$IFDEF DECODEOPTIMIZED}
            GetMem(FrameBuffer, MyImage.Size);
            {$ENDIF}
          End Else
            MyImage.AddFrame();

          {$IFDEF DECODEOPTIMIZED}
          DecodeGIFLZW(Source, FrameBuffer, Palette,(GIFImageDescriptor.flags AND $40=$40));
          CopyFrame(FrameBuffer, GIFImageDescriptor.x, GIFImageDescriptor.y, GIFImageDescriptor.xd, GIFImageDescriptor.yd);
          {$ELSE}
          DecodeGIFLZW(Source, Pixels, Palette,(GIFImageDescriptor.flags AND $40=$40));
          {$ENDIF}

          {$IFDEF PIXEL32}
          MaskFrame;
          {$ENDIF}
          Inc(Frame);
        End;
    '!':Begin
          Source.Read(@GIFExtensionBlock, SizeOf(GIFExtensionBlock));

          Case GIFExtensionBlock.FunctionCode Of
              $F9:Begin
                    Source.Read(@GIFGraphicControlExtension,SizeOf(GIFGraphicControlExtension));

                    TransparencyColor:=GIFGraphicControlExtension.TransparencyColor;
                    DumpData(Source);
                  End;
              Else
                  DumpData(Source);
          End;
        End;
    Else
      Exit;
    End;
  Until (GIFBlockID=';') Or (Source.EOF);

  {$IFDEF DECODEOPTIMIZED}
  FreeMem(FrameBuffer);
  {$ENDIF}
End;

Procedure GIFLoad(Source:Stream; MyImage:Image);
Var
  Loader:GIFLoader;
Begin
  Loader := GIFLoader.Create;
  Loader.MyImage := MyImage;
  Loader.Load(Source);
  Loader.Destroy;

  MyImage.Process(IMP_SetColorKey, ColorCreate(255,0, 255, 255));
End;

Function ValidateGIF(Stream:Stream):Boolean;
Var
  ID:Array[1..3] Of AnsiChar;
Begin
  Stream.Read(@ID,3);
  Result:=(ID='GIF');
End;

Begin
  RegisterImageFormat('GIF', ValidateGIF, GIFLoad);
End.
