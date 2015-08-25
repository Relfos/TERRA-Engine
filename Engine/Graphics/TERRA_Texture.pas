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
 * TERRA_Texture
 * Implements a Texture resource
 ***********************************************************************************************************************
}
Unit TERRA_Texture;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Object, TERRA_Image, TERRA_Stream, TERRA_Color, TERRA_Vector2D, TERRA_Math,
  TERRA_Resource, TERRA_ResourceManager, TERRA_Renderer;

{$IFDEF MOBILE}
{-$DEFINE TEXTURES16BIT}
{$ENDIF}

Const
  MinTextureSize  = 2;

Type
  TERRATexture = Class(TERRAResource)
    Protected
      _Handles:Array Of SurfaceInterface;
      _FrameCount:Integer;

      _Width:Cardinal;
      _Height:Cardinal;

      _AnimationStart:Cardinal;

      _TargetFormat:TextureColorFormat;
      _ByteFormat:PixelSizeType;

      _Source:TERRAImage;
      _Ratio:Vector2D;

      _Managed:Boolean;

      _CurrentFrame:Integer;

      _SettingsChanged:Boolean;
      _WrapMode:TextureWrapMode;
      _MipMapped:Boolean;
      _Filter:TextureFilterMode;

      _TransparencyType:ImageTransparencyType;

      Function GetCurrentFrame():Integer;
      Function GetCurrent:SurfaceInterface;
                                       
      Procedure AdjustRatio(Source:TERRAImage);

      Function IsNPOT():Boolean;

      Function DetectBestFormat(Source:Pointer; SourceFormat:TextureColorFormat):TextureColorFormat;
      Function ConvertToFormat(Source:Pointer; SourceFormat, TargetFormat:TextureColorFormat):Pointer;

      Procedure ApplySettings(Slot:Integer);

      Function GetOrigin: SurfaceOrigin;

      Function GetTransparencyType: ImageTransparencyType;
      
    Public
      Uncompressed:Boolean;
      PreserveQuality:Boolean;

      Constructor Create(Kind:ResourceType; Location:TERRAString);

      Procedure InitFromSize(TextureWidth, TextureHeight:Cardinal; FillColor:ColorRGBA);
      Procedure InitFromImage(Source:TERRAImage);
      Procedure InitFromSurface(Surface:SurfaceInterface);

      Function IsValid():Boolean;

      Function Load(Source:Stream):Boolean; Override;
      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;
      Class Function GetManager:Pointer; Override;

      Function Bind(Slot:Integer):Boolean;

      Class Function GetBindAtSlot(Index:Integer):TERRATexture;

      Procedure UpdateRect(Source:TERRAImage; X,Y:Integer); Overload;
      Procedure UpdateRect(Source:TERRAImage); Overload;

      Procedure SetWrapMode(Value:TextureWrapMode);
      Procedure SetMipMapping(Value:Boolean);
      Procedure SetFilter(Value:TextureFilterMode);

	    Procedure Save(Const FileName:TERRAString);

      Function GetImage():TERRAImage;
      Function GetPixel(X,Y:Integer):ColorRGBA; Virtual;

      Class Function LoadFromFile(Const FileName:TERRAString):TERRATexture;

      //Property Format:Cardinal Read _Format;

      Property Width:Cardinal Read _Width;
      Property Height:Cardinal Read _Height;
      Property Ratio:Vector2D Read _Ratio;

      Property WrapMode:TextureWrapMode Read _WrapMode Write SetWrapMode;
      Property MipMapped:Boolean Read _MipMapped Write SetMipMapping;
      Property Filter:TextureFilterMode Read _Filter Write SetFilter;

      Property TransparencyType:ImageTransparencyType Read GetTransparencyType;

      Property Origin: SurfaceOrigin Read GetOrigin;

      Property SizeInBytes:Cardinal Read _SizeInBytes;

      Property Current:SurfaceInterface Read GetCurrent;
  End;

  TextureProperty = Class(TERRAObject)
    Protected
      _Value:TERRATexture;

    Public
      Constructor Create(Const Name:TERRAString; InitValue:TERRATexture);

      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:TERRATexture Read _Value Write _Value;
  End;


  DefaultColorTableTexture = Class(TERRATexture)
    Public
      Function Build():Boolean; Override;
  End;



Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_Application, TERRA_EngineManager, TERRA_Log, TERRA_GraphicsManager, TERRA_OS,
  TERRA_FileUtils, TERRA_FileStream, TERRA_FileManager, TERRA_ColorGrading, TERRA_Noise;

{ TERRATexture }
Constructor TERRATexture.Create(Kind:ResourceType; Location:TERRAString);
Begin
  Inherited Create(Kind, Location);

  _TargetFormat := textureFormat_RGBA;
  _ByteFormat := pixelSizeByte;
  _Ratio := VectorCreate2D(1, 1);

  _SettingsChanged := True;
  _WrapMode := wrapAll;
  _MipMapped := GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable;
  _Filter := filterBilinear;

  _Managed := False;
End;

Procedure TERRATexture.InitFromSurface(Surface: SurfaceInterface);
Begin
  If (Surface = Nil) Then
  Begin
    Self.InitFromSize(128, 128, ColorRed);
    Exit;
  End;

  _Width := Surface.Width;
  _Height := Surface.Height;

  _FrameCount := 1;

  SetLength(_Handles, _FrameCount);
  _Handles[0] := Surface;

  Uncompressed := False;

  Self.SetStatus(rsReady);

  _SettingsChanged := True;

  _TransparencyType := imageTransparent;

  _Managed := True;
End;

Procedure TERRATexture.InitFromSize(TextureWidth, TextureHeight:Cardinal; FillColor:ColorRGBA);
Begin
  _Width := TextureWidth;
  _Height := TextureHeight;

  If (Not GraphicsManager.Instance.Renderer.Features.NPOT.Avaliable) Then
  Begin
    _Width := IntMax(NearestPowerOfTwo(_Width), MinTextureSize);
    _Height := IntMax(NearestPowerOfTwo(_Height), MinTextureSize);

    If GraphicsManager.Instance.Renderer.Features.MaxTextureSize>0 Then
    Begin
      _Width := IntMin(_Width, GraphicsManager.Instance.Renderer.Features.MaxTextureSize);
      _Height := IntMin(_Height, GraphicsManager.Instance.Renderer.Features.MaxTextureSize);
    End;

    _Ratio := VectorCreate2D(_Width/TextureWidth, _Height/TextureHeight);
  End;

  _Source := TERRAImage.Create(_Width, _Height);
  _Source.ClearWithColor(FillColor, maskRGBA);

  _TransparencyType := imageOpaque;

  Uncompressed := False;

  Self.Update();
End;

Procedure TERRATexture.InitFromImage(Source:TERRAImage);
Begin
  _TransparencyType := imageUnknown;

  If (Source = Nil) Then
    Self.InitFromSize(128, 128, ColorWhite)
  Else
  Begin
    AdjustRatio(Source);
    Self.InitFromSize(Source.Width, Source.Height, ColorWhite);
    Self.UpdateRect(Source);
  End;
End;

Function TERRATexture.Load(Source: Stream):Boolean;
Var
  Ofs:Cardinal;
Begin
  Uncompressed := False;
  Ofs := Source.Position;
  _Source := TERRAImage.Create(Source);

(*  If (StringContains('monster', Source.Name)) Then
    IntToString(2);*)

  _TransparencyType := _Source.TransparencyType;

  AdjustRatio(_Source);

  _Width := _Source.Width;
  _Height := _Source.Height;

  If (StringContains('_normal', Source.Name)) Then
    Uncompressed := True;

  _TargetFormat := textureFormat_RGBA;
  _ByteFormat := pixelSizeByte;

  Result := True;
End;

Function TERRATexture.Unload:Boolean;
Var
  I,S:Integer;
Begin
  If (Length(_Handles)>0) And (Not _Managed) Then
  Begin
    For I:=0 To Pred(_FrameCount) Do
    If (Assigned(_Handles[I])) And (_Handles[I].IsValid()) Then
      ReleaseObject(_Handles[I]);

    _Handles := Nil;
    _FrameCount := 0;
  End;

  ReleaseObject(_Source);

  _TransparencyType := imageUnknown;

  Result := Inherited Unload();
End;

{$DEFINE FORCERGBA}
{$IFDEF IPHONE}
{$DEFINE FORCERGBA}
{$ENDIF}
{$IFDEF LINUX}
{$DEFINE FORCERGBA}
{$ENDIF}

Function TERRATexture.Update:Boolean;
Var
  W,H,I, J, S:Cardinal;
  Pixels:PWord;
  SourceFormat:TextureColorFormat;
  Tex:TextureInterface;
  Temp:SurfaceInterface;
Begin
  Inherited Update();

  Result := False;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Allocating pixels');{$ENDIF}
  If (Not Assigned(_Source)) Then
  Begin
    _Source := TERRAImage.Create(_Width, _Height);
    _Source.ClearWithColor(ColorWhite, maskRGB);
    _TransparencyType := imageOpaque;
    Exit;
  End;

  _FrameCount := _Source.FrameCount;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Generating texture');{$ENDIF}
  If (Length(_Handles)<=0) Then
    SetLength(_Handles, _FrameCount);

  _CurrentFrame := 0;

  SourceFormat := textureFormat_RGBA;
  _TargetFormat := DetectBestFormat(_Source, SourceFormat);
  Pixels := ConvertToFormat(_Source.RawPixels, SourceFormat, _TargetFormat);

  _SizeInBytes := 0;
  For I:=0 To Pred(_FrameCount) Do
  If _Handles[I] = Nil Then
  Begin
    Temp := _Handles[I];

    If (_FrameCount>0) Then
    Begin
      _Source.SetCurrentFrame(I);
      Pixels := PWord(_Source.RawPixels);
    End;

    Tex := GraphicsManager.Instance.Renderer.CreateTexture();
    Tex.Generate(Pixels, _Source.Width, _Source.Height, SourceFormat, _TargetFormat, _ByteFormat);

    _Handles[I] := Tex;
    Inc(_SizeInBytes, _Handles[I].SizeInBytes);

    ReleaseObject(Temp);
  End;


  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Freeing pixels');{$ENDIF}

  Self.SetStatus(rsReady);

  Result := True;

  _AnimationStart := Application.GetTime();
  _CurrentFrame := 0;

  _SettingsChanged := True;
End;

Const
  MaxSlots = 8;
Var
  _TextureSlots:Array[0..Pred(MaxSlots)] Of TERRATexture;

Class Function TERRATexture.GetBindAtSlot(Index:Integer):TERRATexture;
Begin
  Result := _TextureSlots[Index];
End;

Function TERRATexture.Bind(Slot:Integer):Boolean;
Var
  I:Integer;
Begin
  Result := False;
  If (Self = Nil) Or (Not Self.IsReady()) Then
  Begin
    //glBindTexture(GL_TEXTURE_2D, 0);
    Exit;
  End;

  If (Slot<0) Or (Slot>=MaxSlots) Then
    Exit;

(*  For I:=0 To Pred(MaxSlots) Do
  If (_TextureSlots[I] = Self) And (Engine.Textures.WhiteTexture<>Self) Then
  Begin
    Engine.Textures.WhiteTexture.Bind(I);
    Exit;
  End;*)

  _TextureSlots[Slot] := Self;

  _CurrentFrame := GetCurrentFrame();

  If (Self.Current = Nil) Then
    Exit;

  If (Not Self.Current.IsValid()) Then
  Begin
    Self.Unload();
    Self.Rebuild();
    Exit;
  End;

  Result := GraphicsManager.Instance.Renderer.BindSurface(Self.Current, Slot);

  If (_SettingsChanged) Then
  Begin
  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Applying texture settings');{$ENDIF}
    _SettingsChanged := False;
    Self.ApplySettings(Slot);
  End;
End;

Procedure TERRATexture.UpdateRect(Source:TERRAImage; X,Y:Integer);
Var
  Pixels:PWord;
  SourceFormat:TextureColorFormat;
Begin
  If (Self.TransparencyType = imageUnknown) Or (Self.TransparencyType = imageOpaque) Then
    _TransparencyType := Source.TransparencyType;

  If Length(_Handles)<=0 Then
  Begin
    If (Self.Width = Source.Width) And (Self.Height = Source.Height) Then
    Begin
      ReleaseObject(_Source);

      _Source := TERRAImage.Create(Source);
    End Else
    If (Assigned(_Source)) Then
      _Source.Blit(X,Y, 0, 0, Source.Width, Source.Height, Source);

    Exit;
  End;

  SourceFormat := textureFormat_RGBA;
  Pixels := Self.ConvertToFormat(Source.RawPixels, SourceFormat, _TargetFormat);

  If Self.Current Is TextureInterface Then
    TextureInterface(Self.Current).Update(Pixels, X, Y, Source.Width, Source.Height)
  Else
    RaiseError('Trying to update something that is not a TextureInterface!');
End;

Procedure TERRATexture.UpdateRect(Source:TERRAImage);
Begin
  If (Self.Width<>Source.Width) Or (Self.Height <> Source.Height) Then
  Begin
    RaiseError('Invalid texture dimensions: '+ IntegerProperty.Stringify(Self.Width)+' x' +  IntegerProperty.Stringify(Self.Height));
    Exit;
  End;

  Self.UpdateRect(Source, 0, 0);
End;

Class Function TERRATexture.GetManager: Pointer;
Begin
  Result := Engine.Textures;
End;

(*Procedure TextureManager.OnContextLost;
Begin
  Inherited;

  If Assigned(_WhiteTexture) Then
    _WhiteTERRATexture.Unload();

  If Assigned(_BlackTexture) Then
    _BlackTERRATexture.Unload();

  If Assigned(_NullTexture) Then
    _NullTERRATexture.Unload();

  If Assigned(_DefaultNormalMap) Then
    _DefaultNormalMap.Unload();

  If Assigned(_DefaultColorTable) Then
    _DefaultColorTable.Unload();
End;*)

{ DefaultColorTable }
Function DefaultColorTableTexture.Build():Boolean;
Var
  Temp:TERRAImage;
Begin
  Temp := CreateColorTable(32);
  Self.UpdateRect(Temp);
  ReleaseObject(Temp);

  Self.MipMapped := False;
  Self.WrapMode := wrapNothing;

  Result := True;
End;

Function TERRATexture.GetCurrentFrame: Integer;
Var
  Delta:Single;
Begin
  If (_FrameCount<=1) Then
    Result := 0
  Else
  Begin
    Delta := (Application.GetTime - _AnimationStart);
    Delta := Delta / 1000;
    If (Delta>1) Then
      Delta := Frac(Delta);

    Result := Trunc(Delta * Pred(_FrameCount));
  End;
End;


Procedure TERRATexture.AdjustRatio(Source:TERRAImage);
Var
  W,H:Cardinal;
Begin
  If Source = Nil Then
    Exit;

  If (Not GraphicsManager.Instance.Renderer.Features.NPOT.Avaliable) Then
  Begin
    W := IntMax(NearestPowerOfTwo(Source.Width), MinTextureSize);
    H := IntMax(NearestPowerOfTwo(Source.Height), MinTextureSize);

    If GraphicsManager.Instance.Renderer.Features.MaxTextureSize>0 Then
    Begin
      W := IntMin(W, GraphicsManager.Instance.Renderer.Features.MaxTextureSize);
      H := IntMin(H, GraphicsManager.Instance.Renderer.Features.MaxTextureSize);
    End;

    _Ratio := VectorCreate2D(W/Source.Width, H/Source.Height);

    If (W<>Source.Width) Or (H<>Source.Height) Then
      Log(logDebug, 'Texture', self.Name+ ' needs resizing: '+ IntegerProperty.Stringify(W) +' ' + IntegerProperty.Stringify(H));

    Source.Resize(W,H);
  End Else
  Begin
    _Ratio := VectorCreate2D(1, 1);
  End;
End;

Var
  Scratch16:Array Of Word;

Function TERRATexture.ConvertToFormat(Source:Pointer; SourceFormat, TargetFormat:TextureColorFormat):Pointer;
Begin
  Result := Source;
End;

Function TERRATexture.DetectBestFormat(Source:Pointer; SourceFormat:TextureColorFormat):TextureColorFormat;
Begin
  Result := SourceFormat;
End;

(*
Procedure TERRATexture.ConvertToFormat(Source:TERRAImage; Var Pixels:PWord);
Var
  HasMask:Boolean;
  HasAlpha:Boolean;
  Alpha:Byte;
  C:Color;
  P:PColor;
  OP:PWord;
  X, I, Count:Integer;
Begin
  If (_SourceFormat <=0) Then
  Begin
    _ByteFormat := GL_UNSIGNED_BYTE;
    _SourceFormat := GL_RGBA;
    _TargetFormat := GL_RGBA8;
  End;

  Source.SetCurrentFrame(0);
  Pixels := PWord(Source.Pixels);

  If (_Dynamic) Or (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Or (PreserveQuality) Or (_FrameCount>1) Then
    Exit;

  {$IFDEF PC}
    {$IFNDEF FORCERGBA}
    If (GraphicsManager.Instance.Settings.TextureCompression.Avaliable) And (Not Uncompressed) Then
      _TargetFormat := GL_COMPRESSED_RGBA;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF TEXTURES16BIT}
  If (Odd(_Width)) Or (Not GraphicsManager.Instance.Settings.TextureCompression.Enabled) Then
    Exit;

  HasMask := False;
  HasAlpha := False;

{    If (_Name='SPRITE320') Then
       IntegerProperty.Stringify(2);}

  
  I := 0;
  Count := Source.Width * Source.Height;
  P := Source.Pixels;

  While (I<Count) Do
  Begin
    Alpha := P.A;

    If (Alpha=0) Then
      HasMask := True
    Else
    If (Alpha<255) Then
    Begin
      HasAlpha := True;
      Break;
    End;

    Inc(P);
    Inc(I);
  End;

  If Length(Scratch16)<Count Then
    SetLength(Scratch16, Count);

  Pixels := @Scratch16[0];
  OP := PWord(Pixels);
  P := Source.Pixels;
  I := 0;
  X := 0;

  If (HasAlpha) Then
  Begin
    Log(logDebug, 'Texture', 'Converting '+_Name+' to RGBA4444');
    While (I<Count) Do
    Begin
      C := P^;

      C.R := C.R Shr 4;
      C.G := C.G Shr 4;
      C.B := C.B Shr 4;
      C.A := C.A Shr 4;

      OP^ := C.A + (C.B Shl 4) + (C.G Shl 8) + (C.R Shl 12);

      Inc(P);
      Inc(OP);
      Inc(I);
    End;

    _TargetFormat := GL_RGBA;
    _ByteFormat := GL_UNSIGNED_SHORT_4_4_4_4;
  End Else
  If (HasMask) Then
  Begin
    Log(logDebug, 'Texture', 'Converting '+_Name+' to RGBA5551');
    While (I<Count) Do
    Begin
      C := P^;

      C.R := C.R Shr 3;
      C.G := C.G Shr 3;
      C.B := C.B Shr 3;
      C.A := C.A Shr 7;

      OP^ := C.A + (C.B Shl 1) + (C.G Shl 6) + (C.R Shl 11);

      Inc(P);
      Inc(OP);
      Inc(I);
    End;

    _TargetFormat := GL_RGBA;
    _ByteFormat := GL_UNSIGNED_SHORT_5_5_5_1;
  End Else
  Begin
    Log(logDebug, 'Texture', 'Converting '+_Name+' to RGB565');
    While (I<Count) Do
    Begin
      C := P^;

      C.R := C.R Shr 3;
      C.G := C.G Shr 2;
      C.B := C.B Shr 3;

      OP^ := C.B + ((C.G Shl 5) + (C.R Shl 11));

      Inc(P);
      Inc(OP);
      Inc(I);
    End;

    _TargetFormat := GL_RGB;
    _ByteFormat := GL_UNSIGNED_SHORT_5_6_5;
  End;

  _SourceFormat := _TargetFormat;
  {$ENDIF}
End;*)


{Function TERRATexture.GetHandle(Frame:Integer): Cardinal;
Begin
  If (Frame<0) Or (Frame>=_FrameCount) Then
    Frame := _CurrentFrame;

  Result := _Handles[Frame];
End;}

Procedure TERRATexture.SetFilter(Value: TextureFilterMode);
Begin
  Self._Filter := Value;
  Self._SettingsChanged := True;
End;

Procedure TERRATexture.SetMipMapping(Value: Boolean);
Begin
  Self._MipMapped := Value;
  Self._SettingsChanged := True;
End;

Procedure TERRATexture.SetWrapMode(Value: TextureWrapMode);
Begin
  Self._WrapMode := Value;
  Self._SettingsChanged := True;
End;

Function TERRATexture.GetCurrent:SurfaceInterface;
Begin
  If (_CurrentFrame>=_FrameCount) Then
    Result := Nil
  Else
    Result := _Handles[_CurrentFrame];
End;

Procedure TERRATexture.ApplySettings(Slot: Integer);
Begin
  {$IFDEF MOBILE}
  If (IsNPOT()) Then
  Begin
    Filter := filterLinear;
    MipMapped := False;
    WrapMode := wrapNothing;
  End;
  {$ENDIF}

  Self.Current.WrapMode := Self.WrapMode;
  Self.Current.MipMapped := Self.MipMapped;
  Self.Current.Filter := Self.Filter;
End;

Procedure TERRATexture.Save(const FileName: TERRAString);
Var
  Img:TERRAImage;
Begin
  Img := Self.GetImage();
  If Assigned(Img) Then
  Begin
    Img.Save(FileName);
    ReleaseObject(Img);
  End;
End;

Function TERRATexture.IsNPOT: Boolean;
Var
  W, H:Cardinal;
Begin
  W := NearestPowerOfTwo(Self.Width);
  H := NearestPowerOfTwo(Self.Height);

  Result := (W<>Self.Width) Or (H<>Self.Height);
End;

Function TERRATexture.GetImage:TERRAImage;
Begin
  Result := Self.Current.GetImage();
End;

Function TERRATexture.GetPixel(X, Y: Integer):ColorRGBA;
Begin
  Result := Self.Current.GetPixel(X, Y);
End;

Function TERRATexture.GetOrigin: SurfaceOrigin;
Begin
  If Assigned(Self.Current) Then
    Result := Self.Current.Origin
  Else
    Result := surfaceBottomRight;
End;

Class Function TERRATexture.LoadFromFile(const FileName: TERRAString): TERRATexture;
Var
  Src:Stream;
Begin
  Src := Engine.Files.OpenStream(FileName);
  If Assigned(Src) Then
  Begin
    Result := TERRATexture.Create(rtDynamic, FileName);
    Result.Load(Src);
    Result.Update();
    ReleaseObject(Src);
  End Else
    Result := Nil;
End;

Function TERRATexture.IsValid: Boolean;
Begin
  Result := (Assigned(Self.Current)) And (Self.Current.IsValid());
End;

Function TERRATexture.GetTransparencyType:ImageTransparencyType;
Begin
  If (Self._TransparencyType = imageUnknown) Then
  Begin
    If _Source = Nil Then
    Begin
      Result := imageOpaque;
      Exit;
    End;

    Self._TransparencyType := _Source.TransparencyType;
  End;

  Result := Self._TransparencyType;
End;

{ TextureProperty }
Constructor TextureProperty.Create(const Name: TERRAString; InitValue:TERRATexture);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
End;

Function TextureProperty.GetBlob: TERRAString;
Begin
  If Assigned(_Value) Then
    Result := Self._Value.Name
  Else
    Result := '';

  If Result = '' Then
    Result := '#';
End;

Procedure TextureProperty.SetBlob(const Blob: TERRAString);
Begin
  If Blob<>'#' Then
    _Value := Engine.Textures.GetItem(Blob)
  Else
    _Value := Engine.Textures.WhiteTexture;
End;

Function TextureProperty.GetObjectType: TERRAString;
Begin
  Result := 'texture';
End;


End.
