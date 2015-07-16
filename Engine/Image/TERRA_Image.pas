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
 * TERRA_Image
 * Implements image manipulation class
 ***********************************************************************************************************************
}
Unit TERRA_Image;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Color;

Const
  // Image processing flags
  IMP_SwapChannels          = 1;
  IMP_FillColor             = 2;
  IMP_FillAlpha             = 4;
  IMP_SetColorKey           = 8;
  IMP_SetAlphaFromLuminance = 16;
  IMP_SetGreyscale          = 32;
  IMP_FlipVertical          = 64;
  IMP_FlipHorizontal        = 128;
  IMP_ScaleColor            = 256;

  componentRed    = 0;
  componentGreen  = 1;
  componentBlue   = 2;
  componentAlpha  = 3;

  maskRed = 1;
  maskGreen = 2;
  maskBlue = 4;
  maskAlpha = 8;
  maskRGB = maskRed Or maskGreen Or maskBlue;
  maskRGBA = maskRGB Or maskAlpha;

  PixelSize:Cardinal = 4;

Type
  ImageTransparencyType = (imageUnknown, imageOpaque, imageTransparent, imageTranslucent);

  ImageFrame = Class(TERRAObject)
    Protected
      _Data:Array Of Color;
    Public
      Constructor Create(Width, Height:Integer);
      Procedure Release; Override;

  End;

  Image = Class(TERRAObject)
    Protected
      _Frames:Array Of ImageFrame;
      _FrameCount:Cardinal;

      _Pixels:ImageFrame;

      _Width:Cardinal;
      _Height:Cardinal;
      _Size:Cardinal;

      _CurrentFrame:Cardinal;

      _TransparencyType:ImageTransparencyType;

      Procedure Discard;
      Procedure FillAlpha(AlphaValue:Byte=255);

      Function GetPixelCount:Cardinal;
      Function GetPixels:PColor;

      Function GetImageTransparencyType:ImageTransparencyType;

    Public
      Constructor Create(Width, Height:Integer);Overload;
      Constructor Create(Source:Stream);Overload;
      Constructor Create(FileName:TERRAString);Overload;
      Constructor Create(Source:Image);Overload;
      Procedure Release; Override;

      Procedure Load(Source:Stream);Overload;
      Procedure Load(FileName:TERRAString);Overload;

      Procedure Save(Dest:Stream; Format:TERRAString; Options:TERRAString='');Overload;
      Procedure Save(Filename:TERRAString; Format:TERRAString=''; Options:TERRAString='');Overload;

      Procedure Copy(Source:Image);
      Procedure Resize(Const NewWidth,NewHeight:Cardinal);

      Procedure LinearResize(Const NewWidth,NewHeight:Cardinal);

      Procedure New(Const Width,Height:Cardinal);
      Function AddFrame:ImageFrame;
      Procedure NextFrame(Skip:Cardinal=1);
      Procedure SetCurrentFrame(ID:Cardinal);

      {$IFDEF NDS}Function AutoTile:Cardinal;{$ENDIF}

      Procedure Process(Flags:Cardinal; Color:Color); Overload;
      Procedure Process(Flags:Cardinal); Overload;

      Procedure BlitByUV(Const U,V,U1,V1,U2,V2:Single; Const Source:Image);
      Procedure Blit(X,Y,X1,Y1,X2,Y2:Integer; Const Source:Image);

      Procedure BlitAlphaMapByUV(Const U,V,U1,V1,U2,V2,AU1,AV1,AU2,AV2:Single; Const Source,AlphaMap:Image);
      Procedure BlitAlphaMap(X,Y,X1,Y1,X2,Y2,AX1,AY1,AX2,AY2:Integer; Const Source,AlphaMap:Image);

      Procedure BlitWithAlphaByUV(Const U,V,U1,V1,U2,V2:Single; Const Source:Image; ForceBlend:Boolean = True);
      Procedure BlitWithAlpha(X,Y,X1,Y1,X2,Y2:Integer; Const Source:Image; ForceBlend:Boolean = True);

      Procedure BlitWithMaskByUV(Const U,V,U1,V1,U2,V2:Single; Const Color:Color; Const Source:Image);
      Procedure BlitWithMask(X,Y,X1,Y1,X2,Y2:Integer; Const Color:Color; Const Source:Image);

      Function SubImage(X1,Y1,X2,Y2:Integer):Image;

      Function Combine(Layer:Image; Alpha:Single; Mode:ColorCombineMode; Mask:Cardinal):Boolean;

      Procedure ShiftHue(ShiftAmmount:Integer);

      Procedure LineByUV(Const U1,V1,U2,V2:Single; Const Color:Color);
      Procedure Line(X1,Y1,X2,Y2:Integer; Const Color:Color);
      Procedure LineAlpha(X1,Y1,X2,Y2:Integer; Const Color:Color);

      Procedure DrawRectangleByUV(Const U1,V1,U2,V2:Single; Const Color:Color);
      Procedure DrawRectangle(X1,Y1,X2,Y2:Integer; Const Color:Color);

      Procedure FillRectangleByUV(Const U1,V1,U2,V2:Single; Const Color:Color);
      Procedure FillRectangle(X1,Y1,X2,Y2:Integer; Const Color:Color);

      Procedure DrawCircleByUV(Const xCenter,yCenter:Single; Const Radius:Integer; Const Color:Color);
      Procedure DrawCircle(xCenter,yCenter:Integer; Const Radius:Integer; Const Color:Color);

      Procedure FillCircleByUV(Const xCenter,yCenter:Single; Const Radius:Integer; Const Color:Color);
      Procedure FillCircle(xCenter,yCenter:Integer; Const Radius:Integer; Const Color:Color);

      Function GetPixel(X,Y:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
      Function GetPixelByUV(Const U,V:Single):Color; {$IFDEF FPC}Inline;{$ENDIF}
      Function GetComponent(X,Y,Component:Integer):Byte; {$IFDEF FPC}Inline;{$ENDIF}

      Procedure SetPixelByOffset(Ofs:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
      Procedure SetPixel(X,Y:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
      Procedure SetPixelByUV(Const U,V:Single; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}

      //Procedure AddPixel(X,Y:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
      Procedure MixPixel(X,Y:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}

      Function MipMap():Image;

      Procedure LineDecodeRGBPalette4(Buffer, Palette:Pointer; Line:Cardinal);
      Procedure LineDecodeRGBPalette8(Buffer, Palette:Pointer; Line:Cardinal);
      Procedure LineDecodeRGB8(Buffer:Pointer; Line:Cardinal);
      Procedure LineDecodeRGB16(Buffer:Pointer; Line:Cardinal);
      Procedure LineDecodeRGB24(Buffer:Pointer; Line:Cardinal);
      Procedure LineDecodeRGB32(Buffer:Pointer; Line:Cardinal);

      Procedure LineDecodeBGRPalette4(Buffer, Palette:Pointer; Line:Cardinal);
      Procedure LineDecodeBGRPalette8(Buffer, Palette:Pointer; Line:Cardinal);
      Procedure LineDecodeBGR8(Buffer:Pointer; Line:Cardinal);
      Procedure LineDecodeBGR16(Buffer:Pointer; Line:Cardinal);
      Procedure LineDecodeBGR24(Buffer:Pointer; Line:Cardinal);
      Procedure LineDecodeBGR32(Buffer:Pointer; Line:Cardinal);

      Function GetPixelOffset(X,Y:Integer):PColor;
      Function GetLineOffset(Y:Integer):PColor;

      Property Width:Cardinal Read _Width;
      Property Height:Cardinal Read _Height;
      Property PixelCount:Cardinal Read GetPixelCount;
      Property Size:Cardinal Read _Size;
      Property Pixels:PColor Read GetPixels;

      Property CurrentFrame:Cardinal Read _CurrentFrame Write SetCurrentFrame;
      Property FrameCount:Cardinal Read _FrameCount;

      Property TransparencyType:ImageTransparencyType Read GetImageTransparencyType;
  End;


  ImageStreamValidateFunction = Function(Source:Stream):Boolean;
  ImageLoader = Procedure(Source:Stream; Image:Image);
  ImageSaver = Procedure(Source:Stream; Image:Image; Const Options:TERRAString='');

  ImageClassInfo = Record
    Name:TERRAString;
    Validate:ImageStreamValidateFunction;
    Loader:ImageLoader;
    Saver:ImageSaver;
  End;

  Function GetImageLoader(Source:Stream):ImageLoader;
  Function GetImageSaver(Const Format:TERRAString):ImageSaver;
  Procedure RegisterImageFormat(Name:TERRAString;
                                Validate:ImageStreamValidateFunction;
                                Loader:ImageLoader;
                                Saver:ImageSaver=Nil);

  Function GetImageExtensionCount():Integer;
  Function GetImageExtension(Index:Integer):ImageClassInfo;

Implementation
Uses TERRA_FileStream, TERRA_FileUtils, TERRA_FileManager, TERRA_Math, TERRA_Log;

Var
  _ImageExtensions:Array Of ImageClassInfo;
  _ImageExtensionCount:Integer;

Function GetImageExtensionCount():Integer;
Begin
  Result := _ImageExtensionCount;
End;

Function GetImageExtension(Index:Integer):ImageClassInfo;
Begin
  If (Index>=0) And (Index<_ImageExtensionCount) Then
    Result := _ImageExtensions[Index]
  Else
  	FillChar(Result, SizeOf(Result), 0);
End;

Function GetImageLoader(Source:Stream):ImageLoader;
Var
  Pos:Cardinal;
  I:Integer;
Begin
  Result := Nil;
  If Not Assigned(Source) Then
    Exit;

  Pos := Source.Position;

  For I:=0 To Pred(_ImageExtensionCount) Do
  Begin
    Source.Seek(Pos);
    If _ImageExtensions[I].Validate(Source) Then
    Begin
      Log(logDebug, 'Image', 'Found '+_ImageExtensions[I].Name);
      Result := _ImageExtensions[I].Loader;
      Break;
    End;
  End;

  Source.Seek(Pos);
End;

Function GetImageSaver(Const Format:TERRAString):ImageSaver;
Var
  I:Integer;
Begin
  Result := Nil;

  For I:=0 To Pred(_ImageExtensionCount) Do
  If StringEquals(_ImageExtensions[I].Name, Format) Then
  Begin
    Result := _ImageExtensions[I].Saver;
    Exit;
  End;
End;

Procedure RegisterImageFormat(Name:TERRAString;
                              Validate:ImageStreamValidateFunction;
                              Loader:ImageLoader;
                              Saver:ImageSaver=Nil);
Var
  I,N:Integer;
Begin
  Name := StringLower(Name);

  For I:=0 To Pred(_ImageExtensionCount) Do
  If (_ImageExtensions[I].Name = Name) Then
    Exit;

  N := _ImageExtensionCount;
  Inc(_ImageExtensionCount);
  SetLength(_ImageExtensions, _ImageExtensionCount);
  _ImageExtensions[N].Name := Name;
  _ImageExtensions[N].Validate :=Validate;
  _ImageExtensions[N].Loader := Loader;
  _ImageExtensions[N].Saver := Saver;
End;

{ Image }
Constructor Image.Create(Width, Height:Integer);
Begin
  _CurrentFrame := 0;
  _FrameCount := 0;
  New(Width, Height);
End;

Constructor Image.Create(Source:Stream);
Begin
  Load(Source);
End;

Constructor Image.Create(FileName:TERRAString);
Begin
  Load(FileName);
End;

Constructor Image.Create(Source:Image);
Begin
  Copy(Source);
End;

Procedure Image.Release;
Begin
  Discard;
End;

Procedure Image.New(Const Width,Height:Cardinal);
Begin
  Discard();

  _TransparencyType := imageUnknown;
  _CurrentFrame := MaxInt;

  _Width := Width;
  _Height := Height;
  _Size := Width * Height * PixelSize;

  _FrameCount := 0;
  Self.AddFrame();
End;

Function Image.AddFrame():ImageFrame;
Var
  K:Integer;
Begin
  Inc(_FrameCount);
  SetLength(_Frames,_FrameCount);

  K := Pred(_FrameCount);
  _Frames[K] := ImageFrame.Create(_Width, _Height);
  Result := _Frames[K];
  SetCurrentFrame(K);
End;

Function Image.GetPixelCount:Cardinal;
Begin
  Result := Width * Height;
End;

Function Image.GetPixels:PColor;
Begin
  Result := @_Pixels._Data[0];
End;

Procedure Image.Copy(Source:Image);
Var
  I:Cardinal;
Begin
  {Log(logDebug, 'Game', 'Copying image');
  Log(logDebug, 'Game', 'Width:'+IntToString(Source.Width));
  Log(logDebug, 'Game', 'Height:'+IntToString(Source.Height));}

  New(Source.Width, Source.Height);

  If Source.FrameCount<=0 Then
    Exit;

  For I:=0 To Pred(Source.FrameCount) Do
  Begin
    Move(Source._Frames[I]._Data[0], Self._Frames[I]._Data[0], Size);

    If I<Pred(Source.FrameCount) Then
      Self.AddFrame();
  End;

  _TransparencyType := Source._TransparencyType;
End;

Procedure Image.Resize(Const NewWidth,NewHeight:Cardinal);
Const
  FixedPointBits = 12;
Var
  K:Integer;
  X, Y:Cardinal;
  AX,AY,BX,BY,CX,CY,DX,DY:Single;
  U,V,UV:Single;
  OneMinusU, OneMinusV, oneMinusUOneMinusV:Single;
  uOneMinusV, vOneMinusU:Single;
  srcX, srcY, srcXStep, srcYStep:Single;
  pSrcPixelA, pSrcPixelB, pSrcPixelC, pSrcPixelD:Color;
  Dest:ImageFrame;
  Pixel:Color;
Begin
  If (NewWidth=Width) And (NewHeight=Height) Then
    Exit;

  If (Width<=0) Or (Height<=0) Then
    Exit;

  If (NewWidth<=0) Or (NewHeight<=0) Then
  Begin
    _Width := 0;
    _Height := 0;
    _Size := 0;
    Exit;
  End;

  // Resizes the bitmap image using bilinear sampling.
  srcX := 0.0;
  srcY := 0.0;
  srcXStep := _width / NewWidth;
  srcYStep := _height / NewHeight;

  For K:=0 To Pred(FrameCount) Do
  Begin
    SetCurrentFrame(K);
    Dest := ImageFrame.Create(NewWidth, NewHeight);

    For Y := 0 To Pred(NewHeight) Do
    Begin
      For X:= 0 To Pred(NewWidth) Do
      Begin
        ax := floor(srcX);
        u := srcX - ax;
        if (srcXStep>1.0) Then
          u := 0.5;

        ay := floor(srcY);
        v := srcY - ay;
        if (srcYStep>1.0) Then
          v := 0.5;

        dx := ax + 1.0;
        dy := ay + 1.0;

        if (dx >= _width) Then
          dx := _width - 1.0;

        if (dy >= _height) Then
          dy := _height - 1.0;

        bx := dx;
        by := ay;

        cx := ax;
        cy := dy;

        uv := u * v;
        oneMinusU := 1.0 - u;
        oneMinusV := 1.0 - v;
        uOneMinusV := u * oneMinusV;
        vOneMinusU := v * oneMinusU;
        oneMinusUOneMinusV := oneMinusU * oneMinusV;

			  If (ax<0) Then ax := 0;
			  If (ax>=_width) Then ax := _width-1;
			  If (bx<0) Then bx := 0;
			  If (bx>=_width) Then bx := _width-1;
			  If (cx<0) Then cx := 0;
			  If (cx>=_width) Then cx := _width-1;
			  If (dx<0) Then dx := 0;
			  If (dx>=_width) Then dx := _width-1;

			  If (ay<0) Then ay := 0;
			  If (ay>=_height) Then ay := _height-1;
			  If (by<0) Then by := 0;
			  If (by>=_height) Then by := _height-1;
			  If (cy<0) Then cy := 0;
			  If (cy>=_height) Then cy := _height-1;
			  If (dy<0) Then dy := 0;
			  If (dy>=_height) Then dy := _height-1;

        pSrcPixelA := _Pixels._Data[Trunc((ay * _width) + ax)];
        pSrcPixelB := _Pixels._Data[Trunc((by * _width) + bx)];
        pSrcPixelC := _Pixels._Data[Trunc((cy * _width) + cx)];
        pSrcPixelD := _Pixels._Data[Trunc((dy * _width) + dx)];

			  pixel.R := Trunc(pSrcPixelA.R * oneMinusUOneMinusV + pSrcPixelB.R * uOneMinusV + pSrcPixelC.R * vOneMinusU + pSrcPixelD.R * uv);
			  pixel.G := Trunc(pSrcPixelA.G * oneMinusUOneMinusV + pSrcPixelB.G * uOneMinusV + pSrcPixelC.G * vOneMinusU + pSrcPixelD.G * uv);
			  pixel.B := Trunc(pSrcPixelA.B * oneMinusUOneMinusV + pSrcPixelB.B * uOneMinusV + pSrcPixelC.B * vOneMinusU + pSrcPixelD.B * uv);
			  pixel.A := Trunc(pSrcPixelA.A * oneMinusUOneMinusV + pSrcPixelB.A * uOneMinusV + pSrcPixelC.A * vOneMinusU + pSrcPixelD.A * uv);

        Dest._Data[(Y * NewWidth + X)] := Pixel;
        srcX := srcX + srcXStep;
      End;

      srcX := 0.0;
      srcY := srcY + srcYStep;
    End;

    ReleaseObject(_Frames[K]);
    _Frames[K]:= Dest;
  End;

  _Width := NewWidth;
  _Height := NewHeight;
  _Size := _Width * _Height * PixelSize;
  _CurrentFrame := MaxInt;
  SetCurrentFrame(0);
End;

Procedure Image.LinearResize(Const NewWidth,NewHeight:Cardinal);
Const
  FixedPointBits = 12;
Var
  I,J,K:Cardinal;
  Sx,Sy:Cardinal;
  NX,NY:Cardinal;
  PX,PY:Cardinal;
  Buffer:ImageFrame;
  Dest:PColor;
Begin
  If (NewWidth=Width)And(NewHeight=Height) Then
    Exit;

  If (_Pixels = Nil) Then
  Begin
    Self.New(Width, Height);
    Exit;
  End;

  Sx := Trunc((Width / NewWidth)*(1 Shl FixedPointBits));
  Sy := Trunc((Height/ NewHeight)*(1 Shl FixedPointBits));

  For K:=0 To Pred(FrameCount) Do
  Begin
    Buffer := ImageFrame.Create(NewWidth, NewHeight);

    Dest := @Buffer._Data[0];
    NX := 0;
    NY := 0;
    For J:=0 To Pred(NewHeight) Do
    Begin
      For I:=0 To Pred(NewWidth) Do
      Begin
        PX := (NX Shr FixedPointBits);
        PY := (NY Shr FixedPointBits);

        Dest^ := _Pixels._Data[(PY*Width+PX)];
        Inc(Dest);

        Inc(NX, SX);
      End;

      Inc(NY, SY);
      NX:=0;
    End;

    ReleaseObject(_Frames[K]);
    _Frames[K] := Buffer;
  End;

  _Width := NewWidth;
  _Height := NewHeight;
  _Size := _Width * _Height * PixelSize;
  _CurrentFrame := MaxInt;
  SetCurrentFrame(0);
End;

Procedure Image.SetCurrentFrame(ID:Cardinal);
Begin
  If (ID>=_FrameCount) Then
    ID:=0;

  If (ID=_CurrentFrame) Then
    Exit;

  _CurrentFrame:=ID;
  _Pixels := _Frames[ID];
End;

Procedure Image.NextFrame(Skip:Cardinal=1);
Begin
  If Skip<=0 Then
    Exit;
  SetCurrentFrame((_CurrentFrame+Skip) Mod _FrameCount);
End;

Procedure Image.Discard;
Var
  I:Integer;
Begin
  If (_FrameCount>0) Then
  For I:=0 To Pred(_FrameCount) Do
  If Assigned(_Frames[I]) Then
  Begin
    ReleaseObject(_Frames[I]);
    _Frames[I] := Nil;
  End;

  SetLength(_Frames,0);
  _FrameCount:=0;
  _CurrentFrame:=0;
  _Pixels := Nil;
End;

Procedure Image.FillAlpha(AlphaValue:Byte=255);
Var
  Color:PColor;
  I,J:Integer;
Begin
  Color:=Pixels;
  For J:=0 To Pred(Height) Do
    For I:=0 To Pred(Width) Do
    Begin
      Color.A:=AlphaValue;
      Inc(Color);
    End;
End;

Procedure Image.Process(Flags:Cardinal);
Begin
  Process(Flags, ColorWhite);
End;

Procedure Image.Process(Flags:Cardinal; Color:Color);
Var
  N:Cardinal;
  I,J,K:Cardinal;
  Source,Dest:PColor;

  SwapChannels, SetColorKey, ScaleColor:Boolean;
  FilColor,FillAlpha:Boolean;
  SetGreyscale,SetAlphaFromLuminance:Boolean;
  FlipHorizontal,FlipVertical:Boolean;
Begin
  SetAlphaFromLuminance := (Flags And IMP_SetAlphaFromLuminance<>0);
  SetGreyscale := (Flags And IMP_SetGreyscale<>0);
  SwapChannels := (Flags And IMP_SwapChannels<>0);
  SetColorKey := (Flags And IMP_SetColorKey<>0);
  FillAlpha := (Flags And IMP_FillAlpha<>0);
  FilColor := (Flags And IMP_FillColor<>0);
  FlipHorizontal := (Flags And IMP_FlipHorizontal<>0);
  FlipVertical := (Flags And IMP_FlipVertical<>0);
  ScaleColor := (Flags And IMP_ScaleColor<>0);

  If (_Width = 0) Or (_Height = 0) Then
    Exit;

  If (FillAlpha) And (Color.A = 0) Then
    _TransparencyType := imageTransparent;

  For K:=0 To Pred(_FrameCount) Do
  Begin
    Source := @_Frames[K]._Data[0];
    For J:=0 To Pred(Height) Do
      For I:=0 To Pred(Width) Do
      Begin
        If (FillAlpha) And (FilColor) Then
          Source^:=Color
        Else
        If (FilColor) Then
        Begin
          Source.R := Color.R;
          Source.G := Color.G;
          Source.B := Color.B;
        End;

        If (FillAlpha)And(Not FilColor) Then
        Begin
          Source.A:=Color.A;
        End Else
        If (SetAlphaFromLuminance) Then
        Begin
          N:=Source.R+Source.G+Source.B;
          Source.A:=(N Shl 1)+N;
        End;

        If (SetGreyscale) Then
        Begin
          Source^ := ColorGrey(ColorLuminance(Source^), Source.A);
        End;

        If (ScaleColor) Then
        Begin
          Source^ := ColorMultiply(Source^, Color);
        End;

        If (SetColorKey) And (Cardinal(Source^)=Cardinal(Color)) Then
        Begin
          Cardinal(Source^) := 0;
          _TransparencyType := imageTransparent;
        End;

        If (SwapChannels) Then
        Begin
          N:=Source.R;
          Source.R:=Source.B;
          Source.B:=N;
        End;

        Inc(Source);
      End;

    If (FlipHorizontal) Then
    Begin
      N := _Width Shr 1;
      If (Not Odd(_Width)) Then
        Dec(N);

      For J:=0 To Pred(_Height) Do
      Begin
        Source := @_Frames[K]._Data[(J*_Width)];
        Dest := @_Frames[K]._Data[(J*_Width+Pred(_Width))];

        For I:=0 To N Do
        Begin
          Color := Source^;
          Source ^ := Dest^;
          Dest^ := Color;

          Inc(Source);
          Dec(Dest);
        End;

        Inc(Source, N);
      End;
    End;

    If (FlipVertical) Then
    Begin
      Source := Pixels;
      N := _Height Shr 1;
      If (Not Odd(_Height)) Then
        Dec(N);

      For J:=0 To N Do
        For I:=0 To Pred(_Width) Do
        Begin
          Dest := @_Frames[K]._Data[((Pred(Height)-J)*_Width+I)];

          Color := Source^;
          Source^ := Dest^;
          Dest^ := Color;

          Inc(Source);
        End;
    End;
  End;
End;

Procedure Image.BlitByUV(Const U,V,U1,V1,U2,V2:Single; Const Source:Image);
Begin
  Blit(Integer(Round(U*Width)), Integer(Round(V*Height)),
       Integer(Round(U1*Source.Width)), Integer(Round(V1*Source.Height)),
       Integer(Round(U2*Source.Width)), Integer(Round(V2*Source.Height)), Source);
End;

Procedure Image.BlitAlphaMapByUV(Const U,V,U1,V1,U2,V2,AU1,AV1,AU2,AV2:Single; Const Source,AlphaMap:Image);
Begin
  BlitAlphaMap(Integer(Round(U*Width)), Integer(Round(V*Height)),
              Integer(Round(U1*Source.Width)), Integer(Round(V1*Source.Height)),
              Integer(Round(U2*Source.Width)), Integer(Round(V2*Source.Height)),
              Integer(Round(AU1*AlphaMap.Width)), Integer(Round(AV1*AlphaMap.Height)),
              Integer(Round(AU2*AlphaMap.Width)), Integer(Round(AV2*AlphaMap.Height)),
              Source,AlphaMap);
End;

Procedure Image.BlitWithAlphaByUV(Const U,V,U1,V1,U2,V2:Single; Const Source:Image; ForceBlend:Boolean);
Begin
  BlitWithAlpha(Integer(Round(U*Width)), Integer(Round(V*Height)),
                  Integer(Round(U1*Source.Width)), Integer(Round(V1*Source.Height)),
                  Integer(Round(U2*Source.Width)), Integer(Round(V2*Source.Height)),
                  Source, ForceBlend);
End;


Procedure Image.BlitWithMaskByUV(Const U,V,U1,V1,U2,V2:Single; Const Color:Color; Const Source:Image);
Begin
  BlitWithMask(Integer(Round(U*Width)), Integer(Round(V*Height)),
                  Integer(Round(U1*Source.Width)), Integer(Round(V1*Source.Height)),
                  Integer(Round(U2*Source.Width)), Integer(Round(V2*Source.Height)),
                  Color, Source);
End;

Procedure Image.LineByUV(Const U1,V1,U2,V2:Single; Const Color:Color);
Begin
  Line(Integer(Trunc(U1*Width)), Integer(Trunc(V1*Height)),
       Integer(Trunc(U2*Width)), Integer(Trunc(V2*Height)), Color);
End;

Procedure Image.Blit(X,Y, X1,Y1,X2,Y2:Integer; Const Source:Image);
Var
  Dest, Data:PColor;
  I,J:Integer;
  BlitSize,BlitHeight:Integer;
Begin
  If (X>=_Width) Or (Y>=_Height) Then
    Exit;

  If (X1>=Source.Width) Or (Y1>=Source.Height) Then
    Exit;

  If (X<0) Then
  Begin
    X1 := X1 - X;
    If (X1>=X2) Then
      Exit;

    X := 0;
  End;

  If (Y<0) Then
  Begin
    Y1 := Y1 - Y;
    If (Y1>=Y2) Then
      Exit;

    Y := 0;
  End;
    
  BlitHeight := Y2-Y1;
  BlitSize := X2-X1;

  If (BlitHeight<=0) Or (BlitSize<=0) Then
    Exit;

  For J:=0 To Pred(BlitHeight) Do
    For I:=0 To Pred(BlitSize) Do
      SetPixel(X+I, Y+J, Source.GetPixel(X1+I, Y1+J));
    Exit;

{  Dest := Self.GetPixelOffset(X,Y);
  Data := Source.GetPixelOffset(X1, Y1);
  While (BlitHeight>0) Do
  Begin
    SafeMove(Data^, Dest^, BlitSize, Self.Pixels, Self._Size);
    Inc(Dest, Width);
    Inc(Data, Source.Width);
    Dec(BlitHeight);
  End;}
End;

Procedure Image.BlitAlphaMap(X,Y,X1,Y1,X2,Y2,AX1,AY1,AX2,AY2:Integer; Const Source,AlphaMap:Image);
Var
  I,J:Integer;
  BlitSize,BlitHeight:Integer;
  AX,AY,ADX,ADY:Single;
  A,B,C:Color;
  Alpha:Cardinal;
Begin
  If (X>=_Width) Or (Y>=_Height) Then
    Exit;

  If (X1>=Source.Width) Or (Y1>=Source.Height) Then
    Exit;

  If (X<0) Then
  Begin
    X1 := X1 - X;
    If (X1>=X2) Then
      Exit;

    X := 0;
  End;

  If (Y<0) Then
  Begin
    Y1 := Y1 - Y;
    If (Y1>=Y2) Then
      Exit;

    Y := 0;
  End;
    
  BlitHeight := (Y2-Y1);
  BlitSize := (X2-X1);
  If (BlitHeight<=0) Or (BlitSize<=0) Then
    Exit;

  AX := AX1;
  AY := AY1;
  ADX := (AX2-AX1) / BlitSize;
  ADY := (AY2-AY1) / BlitHeight;
          
  For J:=0 To Pred(BlitHeight) Do
  Begin
    For I:=0 To Pred(BlitSize) Do
    Begin
    	Alpha := AlphaMap.GetPixel(Integer(Trunc(AX)), Integer(Trunc(AY))).R;
    	
    	A := Source.GetPixel(X1+I, Y1+J);
    	B :=  Self.GetPixel(X+I, Y+J);
    	C := ColorBlend(A, B, Alpha);
 	    SetPixel(X+I, Y+J, C);
      
      AX:=AX+ADX;
    End;

    AX:=AX1;
    AY:=AY+ADY;
  End;
End;

Procedure Image.BlitWithAlpha(X,Y,X1,Y1,X2,Y2:Integer; Const Source:Image; ForceBlend:Boolean);
Var
  I,J,BlitSize,BlitHeight:Integer;
  Data,Dest:PColor;
Begin
  X1:=IntMax(X1,0);
  X2:=IntMin(X2,Integer(Source.Width));

  Y1:=IntMax(Y1,0);
  Y2:=IntMin(Y2,Integer(Source.Height));

  If (X<0) Then
  Begin
    X1 := X1 - X;
    If (X1>=X2) Then
      Exit;

    X := 0;
  End;

  If (Y<0) Then
  Begin
    Y1 := Y1 - Y;
    If (Y1>=Y2) Then
      Exit;

    Y := 0;
  End;

  BlitHeight:=(Y2-Y1);
  BlitSize:=(X2-X1);

  If (X+BlitSize >= Self.Width) Then
    BlitSize := Self.Width-X;

  If (Y+BlitHeight>= Self.Height) Then
    BlitHeight := Self.Height-Y;

  Dest := @_Pixels._Data[Y*Width +X];
  Data := @Source._Pixels._Data[Y1* Source.Width +X1];
  J := Y;
  While (BlitHeight>0) Do
  Begin
    If J>=0 Then

    For I:=1 To BlitSize Do
    Begin
      If (ForceBlend) Then
      Begin
        Dest^ := ColorBlend(Data^, Dest^);
      End Else
      If (Data.A>0) Then
      Begin
        Data.A := Dest.A;
        Dest^ := ColorScale(Data^, 1.3);
      End;

      Inc(Dest);
      Inc(Data);
    End;
    Inc(Dest, (Self.Width-BlitSize));
    Inc(Data, (Source.Width-BlitSize));
    Dec(BlitHeight);
    Inc(J);
  End;
End;

Procedure Image.BlitWithMask(X,Y,X1,Y1,X2,Y2:Integer; Const Color:Color; Const Source:Image);
Var
  I,BlitSize,BlitHeight:Integer;
  Data,Dest:PColor;
Begin
  X1:=IntMax(X1,0);
  X2:=IntMin(X2,Integer(Pred(Source.Width)));

  Y1:=IntMax(Y1,0);
  Y2:=IntMin(Y2,Integer(Pred(Source.Height)));

  BlitHeight:=(Y2-Y1);
  BlitSize:=(X2-X1);

  If (X+BlitSize>=Self.Width) Then
    BlitSize:=Self.Width-X;
  If (Y+BlitHeight>=Self.Height) Then
    BlitHeight:=Self.Height-Y;

  Dest := @_Pixels._Data[Y*Width +X];
  Data := @Source._Pixels._Data[Y1* Source.Width +X1];
  While (BlitHeight>0) Do
  Begin
    For I:=1 To BlitSize Do
    Begin
      Dest^ := ColorBlend(Color, Dest^, Data.A);
      Inc(Dest);
      Inc(Data);
    End;
    Inc(Dest, (Self.Width-BlitSize));
    Inc(Data, (Source.Width-BlitSize));
    Dec(BlitHeight);
  End;
End;

// Bresenham's line algorithm
Procedure Image.Line(X1,Y1,X2,Y2:Integer; Const Color:Color);
Var
  I,DeltaX,DeltaY,NumPixels:Integer;
  D,Dinc1,Dinc2:Integer;
  X,XInc1,XInc2:Integer;
  Y,YInc1,YInc2:Integer;
Begin
  //calculate deltaX and deltaY
  DeltaX:=Abs(x2-x1);
  DeltaY:=Abs(y2-y1);
  //initialize
  If (DeltaX>=DeltaY) Then
  Begin
    //If x is independent variable
    NumPixels:=Succ(DeltaX);
    D:=(2*DeltaY)-DeltaX;
    DInc1:=DeltaY Shl 1;
    DInc2:=(DeltaY-DeltaX) Shl 1;
    XInc1:=1;
    XInc2:=1;
    YInc1:=0;
    YInc2:=1;
  End Else
  Begin
    //if y is independent variable
    NumPixels:=Succ(DeltaY);
    D:=(2*DeltaX)-DeltaY;
    DInc1:=DeltaX Shl 1;
    DInc2:=(DeltaX-DeltaY) Shl 1;
    xinc1:=0;
    xinc2:=1;
    yinc1:=1;
    yinc2:=1;
  End;
  //move in the right direction
  If (X1>X2) Then
  Begin
    XInc1:=-XInc1;
    XInc2:=-Xinc2;
  End;
  If (Y1>Y2) Then
  Begin
    YInc1:=-YInc1;
    YInc2:=-YInc2;
  End;

  x:=x1;
  y:=y1;
  //draw the pixels
  For i:=1 To Pred(numpixels) Do
  Begin
    SetPixel(x,y,Color);
    If (d<0) Then
    Begin
      Inc(D,DInc1);
      Inc(X,XInc1);
      Inc(Y,YInc1);
    End Else
    Begin
      Inc(D,DInc2);
      Inc(X,XInc2);
      Inc(Y,YInc2);
    End;
  End;
End;

Procedure Image.LineAlpha(X1,Y1,X2,Y2:Integer; Const Color:Color);
Var
  I,DeltaX,DeltaY,NumPixels:Integer;
  D,Dinc1,Dinc2:Integer;
  X,XInc1,XInc2:Integer;
  Y,YInc1,YInc2:Integer;
Begin
  //calculate deltaX and deltaY
  DeltaX:=Abs(x2-x1);
  DeltaY:=Abs(y2-y1);
  //initialize
  If (DeltaX>=DeltaY) Then
  Begin
    //If x is independent variable
    NumPixels:=Succ(DeltaX);
    D:=(2*DeltaY)-DeltaX;
    DInc1:=DeltaY Shl 1;
    DInc2:=(DeltaY-DeltaX) Shl 1;
    XInc1:=1;
    XInc2:=1;
    YInc1:=0;
    YInc2:=1;
  End Else
  Begin
    //if y is independent variable
    NumPixels:=Succ(DeltaY);
    D:=(2*DeltaX)-DeltaY;
    DInc1:=DeltaX Shl 1;
    DInc2:=(DeltaX-DeltaY) Shl 1;
    xinc1:=0;
    xinc2:=1;
    yinc1:=1;
    yinc2:=1;
  End;
  //move in the right direction
  If (X1>X2) Then
  Begin
    XInc1:=-XInc1;
    XInc2:=-Xinc2;
  End;
  If (Y1>Y2) Then
  Begin
    YInc1:=-YInc1;
    YInc2:=-YInc2;
  End;

  x:=x1;
  y:=y1;
  //draw the pixels
  For i:=1 To Pred(numpixels) Do
  Begin
    MixPixel(x,y,Color);
    If (d<0) Then
    Begin
      Inc(D,DInc1);
      Inc(X,XInc1);
      Inc(Y,YInc1);
    End Else
    Begin
      Inc(D,DInc2);
      Inc(X,XInc2);
      Inc(Y,YInc2);
    End;
  End;
End;

Procedure Image.DrawRectangleByUV(Const U1,V1,U2,V2:Single; Const Color:Color);
Begin
  DrawRectangle(Integer(Trunc(U1*Width)), Integer(Trunc(V1*Height)),
                Integer(Trunc(U2*Width)), Integer(Trunc(V2*Height)), Color);
End;

Procedure Image.FillRectangleByUV(Const U1,V1,U2,V2:Single; Const Color:Color);
Begin
  FillRectangle(Integer(Trunc(U1*Width)), Integer(Trunc(V1*Height)),
                Integer(Trunc(U2*Width)), Integer(Trunc(V2*Height)), Color);
End;

// Fast rectangle draw
Procedure Image.DrawRectangle(X1,Y1,X2,Y2:Integer; Const Color:Color);
Var
  J,LineSize,LineSkip:Integer;
  Dest:PColor;
Begin
  X1 := IntMax(X1,0);
  X2 := IntMin(X2,Integer(Pred(Width)));

  Y1 := IntMax(Y1,0);
  Y2 := IntMin(Y2,Integer(Pred(Height)));

  LineSize := (X2-X1);
  If (LineSize <= 0) Then
    Exit;

  LineSkip := Succ(Integer(Width) - LineSize);

  Dest := @_Pixels._Data[Y1*Width + X1 ];

  // Fill top line
  FillLong(Dest^, LineSize, Cardinal(Color));

  Inc(Dest,Width);
  // Fill lateral lines
  For J:=Succ(Y1) To Pred(Y2) Do
  Begin
    Dest^ := Color;
    Inc(Dest, Pred(LineSize));
    Dest^ := Color;
    Inc(Dest, LineSkip);
  End;

  // Fill bottom line
  FillLong(Dest^, LineSize, Cardinal(Color));
End;

// Fast rectangle fill
Procedure Image.FillRectangle(X1,Y1,X2,Y2:Integer; Const Color:Color);
Var
  J,LineSize:Integer;
  Dest:PColor;
Begin
  X1:=IntMax(X1,0);
  X2:=IntMin(X2,Integer(Pred(Width)));

  Y1:=IntMax(Y1,0);
  Y2:=IntMin(Y2,Integer(Pred(Height)));

  LineSize:=Succ(X2-X1);
  If (LineSize<=0) Then
    Exit;

  Dest := @_Pixels._Data[Y1*Width + X1 ];
  For J:=Y1 To Y2 Do
  Begin
    FillLong(Dest^,LineSize,Cardinal(Color));
    Inc(Dest,Width);
  End;
End;

Procedure Image.DrawCircleByUV(Const xCenter,yCenter:Single; Const Radius:Integer; Const Color:Color);
Begin
  DrawCircle(Integer(Round(xCenter*Width)),Integer(Round(yCenter*Height)), Radius, Color);
End;

Procedure Image.FillCircleByUV(Const xCenter,yCenter:Single; Const Radius:Integer; Const Color:Color);
Begin
  FillCircle(Integer(Round(xCenter*Width)),Integer(Round(yCenter*Height)), Radius, Color);
End;

// Bresenham's circle algorithm
Procedure Image.DrawCircle(xCenter,yCenter:Integer; Const Radius:Integer; Const Color:Color);
Var
  X,Y,P:Integer;
Begin
  x:=0;
  Y:=Radius;
  P:=3-Radius*2;
  While (X<=Y) Do
  Begin
    SetPixel(xCenter + x, yCenter + y, Color);
    SetPixel(xCenter - x, yCenter + y, Color);
    SetPixel(xCenter + x, yCenter - y, Color);
    SetPixel(xCenter - x, yCenter - y, Color);
    SetPixel(xCenter + y, yCenter + x, Color);
    SetPixel(xCenter - y, yCenter + x, Color);
    SetPixel(xCenter + y, yCenter - x, Color);
    SetPixel(xCenter - y, yCenter - x, Color);
    If (P<0) Then
    Begin
      Inc(X);
      Inc(P,4 *X + 6);
    End Else
    Begin
      Inc(X);
      Dec(Y);
      Inc(P, 4*(X-Y)+10);
    End;
  End;
End;

Procedure Image.FillCircle(xCenter,yCenter:Integer; Const Radius:Integer; Const Color:Color);
Var
  A,B,I:Integer;
Begin
  For A:=0 To Pred(Radius) Do
  Begin
    B := Trunc(Sqrt((Sqr(Radius) - Sqr(A))));
    For I:=-B To B Do
    Begin
      SetPixel(xCenter+I,yCenter-Pred(A), Color);
      SetPixel(xCenter+I,yCenter+Pred(A), Color);
    End;
  End;
End;

Function Image.GetLineOffset(Y:Integer):PColor;
Begin
  If (_Height<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;

  If (Y<0) Then
    Y := 0
  Else
  If (Y>=_Height) Then
    Y := Pred(_Height);

  Result := @_Pixels._Data[Y * Width];
End;

Function Image.GetPixelOffset(X,Y:Integer):PColor;
Begin
  If (X<0) Then
    X := 0;

  If (Y<0) Then
    Y := 0;

  If (_Pixels._Data = Nil) Then
    Result := Nil
  Else
    Result := @_Pixels._Data[Y * Width + X];
End;

Function Image.GetPixelByUV(Const U,V:Single):Color; {$IFDEF FPC}Inline;{$ENDIF}
Var
  X,Y:Integer;
Begin
  X := Trunc(U*Width) Mod Width;
  Y := Trunc(V*Height) Mod Height;
  Result := GetPixel(X,Y);
End;

Function Image.GetPixel(X,Y:Integer):Color; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (Pixels = Nil) Or (Width<=0) Or (Height<=0)Then
  Begin
    Result := ColorNull;
    Exit;
  End;

  If (X<0) Then X := 0;
  If (Y<0) Then Y := 0;
  If (X>=Width) Then X := Pred(Width);
  If (Y>=Height) Then Y := Pred(Height);

  Result := GetPixelOffset(X,Y)^;
End;

Function Image.GetComponent(X,Y,Component:Integer):Byte; {$IFDEF FPC}Inline;{$ENDIF}
Var
  P:Color;
Begin
  P := GetPixel(X,Y);
  Result := PByteArray(@P)[Component];
End;

Procedure Image.SetPixelByUV(Const U,V:Single; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
Var
  X,Y:Integer;
Begin
  X:=Trunc(U*Width) Mod Width;
  Y:=Trunc(V*Height) Mod Height;
  SetPixel(X,Y,Color);
End;

Procedure Image.SetPixel(X,Y:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  If (X<0) Or (Y<0) Or (X>=Integer(Width)) Or (Y>=Integer(Height)) Then
    Exit;

  SetPixelByOffset(Y*Width+X, Color);
End;

Procedure Image.SetPixelByOffset(Ofs:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
Var
  Dest:PColor;
Begin
  If (Ofs<0) Or (Ofs >= Width*Height) Then
    Exit;

  _Pixels._Data[Ofs] := Color;
End;

Procedure Image.MixPixel(X,Y:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
Var
  Dest:PColor;
Begin
  If (X<0) Then X:=0;
  If (Y<0) Then Y:=0;
  If (X>=Integer(Width)) Then X:=Pred(Integer(Width));
  If (Y>=Integer(Height)) Then Y:=Pred(Integer(Height));

  Dest :=  @_Pixels._Data[Y*Width+X];
  Dest^ := ColorBlend(Color, Dest^);
End;

(*Procedure Image.AddPixel(X,Y:Integer; Const Color:Color); {$IFDEF FPC}Inline;{$ENDIF}
Var
  Dest:PColor;
Begin
  If (X<0) Or (Y<0) Or (X>=Width) Or (Y>=Height) Then
    Exit;

  {$IFDEF PIXEL8}
  //TODO
  {$ELSE}
  Dest := @_Pixels._Data[Y*Width+X];
  Dest^ := ColorAdd(Dest^, Color);
  {$ENDIF}
End;*)

Procedure Image.Load(FileName:TERRAString);
Var
  Source:Stream;
Begin
  Source := FileManager.Instance.OpenStream(FileName);
  If Assigned(Source) Then
  Begin
    Load(Source);
    ReleaseObject(Source);
  End;
End;

Procedure Image.Load(Source:Stream);
Var
  Loader:ImageLoader;
Begin
  If Source = Nil Then
  Begin
    Log(logDebug, 'Image', 'Invalid image stream!');
    Exit;
  End;

  Log(logDebug, 'Image', 'Searching formats');
  Loader := GetImageLoader(Source);
  If Not Assigned(Loader) Then
  Begin
    Self.New(4, 4);
    Log(logDebug, 'Image', 'Unknown image format. ['+Source.Name+']');
    Exit;
  End;

  Log(logDebug, 'Image', 'Loading image from loader ');
  Loader(Source, Self);
  Log(logDebug, 'Image', 'Image loaded');
End;

Procedure Image.Save(Dest:Stream; Format:TERRAString; Options:TERRAString='');
Var
  Saver:ImageSaver;
Begin
  If (_Pixels = Nil) Then
    Exit;

  Saver := GetImageSaver(Format);
  If Not Assigned(Saver) Then
  Begin
    Log(logError, 'Image', 'Cannot save image to '+Format+' format. ['+Dest.Name+']');
    Exit;
  End;

  Log(logDebug, 'Image', 'Saving image in '+Format+' format');
  Saver(Dest, Self, Options);
End;

Procedure Image.Save(Filename:TERRAString; Format:TERRAString=''; Options:TERRAString='');
Var
  Dest:Stream;
Begin
  If Format='' Then
    Format := GetFileExtension(FileName);

  Dest := FileStream.Create(FileName);
  Save(Dest, Format, Options);
  ReleaseObject(Dest);
End;


{$IFDEF PIXEL8}
Procedure Image.LineDecodeRGB8(Buffer: Pointer; Line: Cardinal);
Var
  Dest:PByte;
Begin
fsdfs
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Move(Buffer^, Dest^, _Width);
End;

Procedure Image.LineDecodeRGB16(Buffer: Pointer; Line: Cardinal);
Var
  Source:PWord;
  Dest:PByte;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;
sfds
  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
//    Dest^:=ColorRGB16To8(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGB24(Buffer: Pointer; Line: Cardinal);
Var
  Source:PByte;
  Dest:PByte;
  Temp:Color;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  Temp.A:=255;
  While (Count>0) Do
  Begin
    {$IFDEF RGB}
    Temp.R:=Source^; Inc(Source);
    Temp.G:=Source^; Inc(Source);
    Temp.B:=Source^; Inc(Source);
    {$ENDIF}
    {$IFDEF BGR}
    Temp.B:=Source^; Inc(Source);
    Temp.G:=Source^; Inc(Source);
    Temp.R:=Source^; Inc(Source);
    {$ENDIF}
    Dest^:=ColorRGB32To8(Temp);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGB32(Buffer: Pointer; Line: Cardinal);
Var
  Source:PColor;
  Dest:PByte;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
    Dest^:=ColorRGB32To8(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGBPalette4(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source, ColorTable:PByte;
  Dest:PByte;
  Count:Integer;
  Index:Integer;
  Temp:Color;
  A,B:Byte;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width Shr 1;
  Source:=Buffer;

  Temp.A:=255;
  While (Count>0) Do
  Begin
    A:=Source^;
    B:=A And $0F;
    A:=(A Shr  4) And $0F;
    ColorTable:=PByte(Cardinal(Palette)+ A*4);
    {$IFDEF RGB}
    Temp.R:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.B:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    {$IFDEF BGR}
    Temp.B:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.R:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    Dest^:=ColorRGB32To8(Temp);
    Inc(Dest);

    ColorTable:=PByte(Cardinal(Palette)+ B*4);
    {$IFDEF RGB}
    Temp.R:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.B:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    {$IFDEF BGR}
    Temp.B:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.R:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    Dest^:=ColorRGB32To8(Temp);
    Inc(Dest);

    Inc(Source);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGBPalette8(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source, ColorTable:PByte;
  Dest:PByte;
  Count:Integer;
  Index:Integer;
  Temp:Color;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  Temp.A:=255;
  While (Count>0) Do
  Begin
    ColorTable:=PByte(Cardinal(Palette)+ (Source^)*4);
    {$IFDEF RGB}
    Temp.R:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.B:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    {$IFDEF BGR}
    Temp.B:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.R:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    Inc(ColorTable);
    Dest^:=ColorRGB32To8(Temp);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGR8(Buffer: Pointer; Line:Cardinal);
Var
  Dest:PByte;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Move(Buffer^, Dest^, _Width);
End;

Procedure Image.LineDecodeBGR16(Buffer: Pointer; Line:Cardinal);
Var
  Source:PWord;
  Dest:PByte;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
//    Dest^:=ColorBGR16To8(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGR24(Buffer: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PByte;
  Temp:Color;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  Temp.A:=255;
  While (Count>0) Do
  Begin
    {$IFDEF BGR}
    Temp.R:=Source^; Inc(Source);
    Temp.G:=Source^; Inc(Source);
    Temp.B:=Source^; Inc(Source);
    {$ENDIF}
    {$IFDEF RGB}
    Temp.B:=Source^; Inc(Source);
    Temp.G:=Source^; Inc(Source);
    Temp.R:=Source^; Inc(Source);
    {$ENDIF}
    Dest^:=ColorRGB32To8(Temp);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGR32(Buffer: Pointer; Line:Cardinal);
Var
  Source:PColor;
  Dest:PByte;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
    Dest^:=ColorBGR32To8(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGRPalette4(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source, ColorTable:PByte;
  Dest:PByte;
  Count:Integer;
  Index:Integer;
  Temp:Color;
  A,B:Byte;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width Shr 1;
  Source:=Buffer;

  Temp.A:=255;
  While (Count>0) Do
  Begin
    A:=Source^;
    B:=A And $0F;
    A:=(A Shr  4) And $0F;
    ColorTable:=PByte(Cardinal(Palette)+ A*4);
    {$IFDEF BGR}
    Temp.R:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.B:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    {$IFDEF RGB}
    Temp.B:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.R:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    Dest^:=ColorRGB32To8(Temp);
    Inc(Dest);

    ColorTable:=PByte(Cardinal(Palette)+ B*4);
    {$IFDEF BGR}
    Temp.R:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.B:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    {$IFDEF RGB}
    Temp.B:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.R:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    Dest^:=ColorRGB32To8(Temp);
    Inc(Dest);

    Inc(Source);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGRPalette8(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source, ColorTable:PByte;
  Dest:PByte;
  Count:Integer;
  Index:Integer;
  Temp:Color;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count := _Width;
  Source := Buffer;

  Temp.A := 255;
  While (Count>0) Do
  Begin
    ColorTable:=PByte(Cardinal(Palette)+ (Source^)*4);
    {$IFDEF BGR}
    Temp.R:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.B:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    {$IFDEF RGB}
    Temp.B:=ColorTable^; Inc(ColorTable);
    Temp.G:=ColorTable^; Inc(ColorTable);
    Temp.R:=ColorTable^; Inc(ColorTable);
    {$ENDIF}
    Inc(ColorTable);
    Dest^:=ColorRGB32To8(Temp);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

{$ENDIF}

{$IFDEF PIXEL32}
Procedure Image.LineDecodeRGB8(Buffer: Pointer; Line: Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
    Dest ^:= ColorRGB8To32(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGB16(Buffer: Pointer; Line:Cardinal);
Var
  Source:PWord;
  Dest:PColor;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
    Dest^:=ColorRGB16To32(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGB24(Buffer: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;
  While (Count>0) Do
  Begin
    {$IFDEF RGB}
    Dest.R:=Source^; Inc(Source);
    Dest.G:=Source^; Inc(Source);
    Dest.B:=Source^; Inc(Source);
    {$ENDIF}
    {$IFDEF BGR}
    Dest.B:=Source^; Inc(Source);
    Dest.G:=Source^; Inc(Source);
    Dest.R:=Source^; Inc(Source);
    {$ENDIF}
    Dest.A:=255;
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGB32(Buffer: Pointer; Line:Cardinal);
Var
  Dest:PColor;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Move(Buffer^, Dest^, _Width*PixelSize);
End;

Procedure Image.LineDecodeRGBPalette4(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
  A,B:Byte;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count := _Width;
  Source := Buffer;

  While (Count>0) Do
  Begin
    A:=Source^;
    B:=A And $0F;
    A:=(A Shr  4) And $0F;
    Dest^:=PColorPalette(Palette)[A];
    Inc(Dest);
    Dest^:=PColorPalette(Palette)[B];
    Inc(Dest);

    Inc(Source);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeRGBPalette8(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;
  While (Count>0) Do
  Begin
    Dest^ := PColorPalette(Palette)[Source^];
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGR8(Buffer: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;
  While (Count>0) Do
  Begin
    Dest^:=ColorBGR8To32(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGR16(Buffer: Pointer; Line:Cardinal);
Var
  Source:PWord;
  Dest:PColor;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count := _Width;
  Source := Buffer;

  While (Count>0) Do
  Begin
    Dest^:=ColorBGR16To32(Source^);
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGR24(Buffer: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
Begin
  If (Line>=_Height) Or (Buffer = Nil) Then
    Exit;

  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count := _Width;
  Source := Buffer;

  While (Count>0) Do
  Begin
    {$IFDEF BGR}
    Dest.R := Source^; Inc(Source);
    Dest.G := Source^; Inc(Source);
    Dest.B := Source^; Inc(Source);
    {$ENDIF}
    {$IFDEF RGB}
    Dest.B := Source^; Inc(Source);
    Dest.G := Source^; Inc(Source);
    Dest.R := Source^; Inc(Source);
    {$ENDIF}
    Dest.A := 255;
    Inc(Dest);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGR32(Buffer: Pointer; Line:Cardinal);
Var
  Dest:PColor;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Move(Buffer^, Dest^, _Width*PixelSize);
End;

Procedure Image.LineDecodeBGRPalette4(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
  A,B:Byte;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
    A:=Source^;
    B:=A And $0F;
    A:=(A Shr  4) And $0F;
    Dest^:=PColorPalette(Palette)[A];
    Inc(Dest);
    Dest^:=PColorPalette(Palette)[B];
    Inc(Dest);

    Inc(Source);
    Dec(Count);
  End;
End;

Procedure Image.LineDecodeBGRPalette8(Buffer, Palette: Pointer; Line:Cardinal);
Var
  Source:PByte;
  Dest:PColor;
  Count:Integer;
Begin
  Dest := Self.GetLineOffset(Line);
  If (Dest =  Nil) Then
    Exit;

  Count:=_Width;
  Source:=Buffer;

  While (Count>0) Do
  Begin
    Dest^:=PColorPalette(Palette)[Source^];
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  End;
End;
{$ENDIF}

{$IFDEF NDS}
Function Image.AutoTile:Cardinal;
Var
	TileCount:Integer;
	Start, Dest, Temp:PByte;
	I,J,X,Y:Integer;
Begin
  GetMem(Temp, _Size);
  Dest:=Temp;
  TileCount:=((Width Shr 3) * (Height Shr 3));
	X:=0;
	Y:=0;
	For I:=0 To Pred(TileCount) Do
	Begin
		For J:=0 To 7 Do
		Begin
      Start := @_Pixels._Data[X+((Y+J)*_Width)];
			Move(Start^, Dest^, 8);
			Inc(Dest, 8);
		End;

		Inc(X,8);
		If (X>=_Width) Then
		Begin
			X:=0;
			Inc(Y,8);
		End;
	End;

  Move(Temp^, _Pixels._Data[0], _Size);
  FreeMem(Temp);
  Result := TileCount;
End;
{$ENDIF}

Function Image.SubImage(X1,Y1,X2,Y2:Integer):Image;
Var
  W,H:Integer;
  I,J:Integer;
Begin
  W := Pred(X2-X1);
  H := Pred(Y2-Y1);
  If (W<=0) Or (H<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := Image.Create(W, H);
  For J:=0 To Pred(H) Do
    For I:=0 To Pred(W) Do
    Begin
      Result.SetPixel(I,J, Self.GetPixel(X1+I, Y1+J));
    End;
End;

Function Image.Combine(Layer:Image; Alpha:Single; Mode:ColorCombineMode; Mask:Cardinal):Boolean;
Var
  A,B:PColor;
  C:Color;
  InvAlpha:Single;
  Count:Integer;
Begin
  Result := False;

  If (Layer = Nil) Then
    Exit;

  If (Layer.Width<>Self.Width) Or (Layer.Height<>Self.Height) Then
    Exit;

  A := Self.Pixels;
  B := Layer.Pixels;

  Count := Self.Width * Self.Height;

  InvAlpha := 1.0 - Alpha;

  While Count>0 Do
  Begin
    C := ColorCombine(A^, B^, Mode);

    If (Mask And maskRed<>0) Then
      A.R := Trunc(A.R * InvAlpha + C.R * Alpha);

    If (Mask And maskGreen<>0) Then
      A.g := Trunc(A.G * InvAlpha + C.G * Alpha);

    If (Mask And maskBlue<>0) Then
      A.B := Trunc(A.B * InvAlpha + C.B * Alpha);

    If (Mask And maskAlpha<>0) Then
      A.A := Trunc(A.A * InvAlpha + C.A * Alpha);

    If (Mask = maskAlpha) And (A.A<250) Then
    Begin
      A.R := 0;
      A.G := 0;
      A.B := 0;
    End;

    //A^:= ColorMix(C, A^, Alpha);

    Inc(A);
    Inc(B);
    Dec(Count);
  End;


  Result := True;
End;

Function Image.MipMap(): Image;
Var
  I,J:Integer;
  PX, PY:Single;
  A, B, C, D, F:Color;
Begin
  Result := Image.Create(Self.Width Shr 1, Self.Height Shr 1);

  For I:=0 To Pred(Result.Width) Do
    For J:=0 To Pred(Result.Height) Do
    Begin
      PX := (I * 2.0) + 0.5;
      PY := (J * 2.0) + 0.5;

      A := Self.GetPixel(Trunc(PX), Trunc(PY));
      B := Self.GetPixel(Round(PX), Trunc(PY));
      C := Self.GetPixel(Round(PX), Round(PY));
      D := Self.GetPixel(Trunc(PX), Round(PY));

      F.R := Trunc((A.R + B.R + C.R + D.R) * 0.25);
      F.G := Trunc((A.G + B.G + C.G + D.G) * 0.25);
      F.B := Trunc((A.B + B.B + C.B + D.B) * 0.25);
      F.A := Trunc((A.A + B.A + C.A + D.A) * 0.25);

      Result.SetPixel(I, J, F);
    End;
End;

Procedure Image.ShiftHue(ShiftAmmount:Integer);
Var
  P:PColor;
  Count:Integer;
  Temp:ColorHSL;
  Hue:Integer;
  C:Color;
Begin
  Count := Self.Width * Self.Height;
  P := Self.Pixels;
  While Count>0 Do
  Begin
    Temp := ColorRGBToHSL(P^);

    Hue := Temp.H + ShiftAmmount;

    Temp.H := Byte(Hue);

    P^ := ColorHSLToRGB(Temp);

    Inc(P);
    Dec(Count);
  End;
End;

Function Image.GetImageTransparencyType:ImageTransparencyType;
Var
  P:PColor;
  Count:Integer;
Begin
  If _TransparencyType = imageUnknown Then
  Begin
    P := Self.Pixels;
    Count := Self.Width * Self.Height;

    _TransparencyType := imageOpaque;
    While Count>0 Do
    Begin
      If (P.A<255) Then
      Begin
        If (P.A>0) Then
        Begin
          _TransparencyType := imageTranslucent;
          Break;
        End Else
          _TransparencyType := imageTransparent;
      End;

      Inc(P);
      Dec(Count);
    End;
  End;

  Result := _TransparencyType;
End;

{ ImageFrame }
Constructor ImageFrame.Create(Width, Height:Integer);
Begin
  SetLength(_Data, Width * Height);
  If (Width>0) And (Height>0) Then
	  FillChar(_Data[0], Width * Height * PixelSize, 0);
End;

Procedure ImageFrame.Release;
Begin
  SetLength(_Data, 0);
End;

End.
