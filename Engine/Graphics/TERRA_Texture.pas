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
  TERRA_String, TERRA_Image, TERRA_Stream, TERRA_Color, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF},
  TERRA_Vector2D, TERRA_Math, TERRA_Resource, TERRA_ResourceManager;

{$IFDEF MOBILE}
{´-$DEFINE TEXTURES16BIT}
{$ENDIF}

{-$DEFINE KEEPCOPYONRAM}

Const
  MinTextureSize  = 2;

  textureFilterPoint = 0;
  textureFilterBilinear = 1;

Type
  Texture = Class(Resource)
    Protected
      _Handles:Array Of Cardinal;
      _FrameCount:Integer;
      _AnimationStart:Cardinal;

      _TargetFormat:Cardinal;
      _SourceFormat:Cardinal;
      _ByteFormat:Cardinal;

      _Width:Cardinal;
      _Height:Cardinal;
      _Source:Image;
      _Ratio:Vector2D;

      _SettingsChanged:Boolean;
      _Wrap:Boolean;
      _MipMapped:Boolean;
      _BilinearFilter:Boolean;
      _NPOT:Boolean;

      _Dynamic:Boolean;

      _CurrentFrame:Integer;

      Procedure ApplySettings;

      Function GetCurrentFrame():Integer;

      Procedure ConvertToBestFormat(Source:Image; Var Pixels:PWord);

      Procedure AdjustRatio(Source:Image);
      Procedure CheckNPOT();

    Public
      Uncompressed:Boolean;
      PreserveQuality:Boolean;

      Constructor New(Const Name:TERRAString; TextureWidth, TextureHeight:Cardinal); Overload;
      Constructor New(Const Name:TERRAString; Source:Image); Overload;

      Procedure Build(); Virtual;

      Function Load(Source:Stream):Boolean; Override;
      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;
      Class Function GetManager:Pointer; Override;

      Class Function RAM:Cardinal;

      Procedure Bind(Slot:Integer); Virtual;

      Function GetImage():Image; Virtual;
      Function GetPixel(X,Y:Integer):Color;  Virtual;
      Function GetHandle(Frame:Integer): Cardinal;

      Procedure UpdateRect(Source:Image; X,Y:Integer); Overload;
      Procedure UpdateRect(Source:Image); Overload;

      Procedure SetWrap(Value:Boolean);
      Procedure SetMipMapping(Value:Boolean);
      Procedure SetFilter(Value:Boolean);

      //Property Format:Cardinal Read _Format;

      Property Width:Cardinal Read _Width;
      Property Height:Cardinal Read _Height;
      Property Ratio:Vector2D Read _Ratio;

      Property Wrap:Boolean Read _Wrap Write SetWrap;
      Property MipMapped:Boolean Read _MipMapped Write SetMipMapping;
      Property BilinearFilter:Boolean Read _BilinearFilter Write SetFilter;
  End;

  TextureClass = Class Of Texture;
  TextureFormat = Record
    Extension:TERRAString;
    ClassType:TextureClass;
  End;

  TextureManager = Class(ResourceManager)
    Protected
      _DefaultColorTable:Texture;
      _DefaultNormalMap:Texture;

      _WhiteTexture:Texture;
      _BlackTexture:Texture;
      _NullTexture:Texture;

      Function GetDefaultNormalMap:Texture;
      Function GetDefaultColorTable:Texture;

      Function CreateTextureWithColor(Name:TERRAString; TexColor:Color):Texture;
      Procedure FillTextureWithColor(Tex:Texture; TexColor:Color);

      Function GetWhiteTexture:Texture;
      Function GetNullTexture:Texture;
      Function GetBlackTexture:Texture;


    Public
      Procedure OnContextLost; Override;

      Class Function Instance:TextureManager;
      Function GetTexture(Name:TERRAString):Texture;

      Procedure Release; Override;

      Property NullTexture:Texture Read GetNullTexture;
      Property WhiteTexture:Texture Read GetWhiteTexture;
      Property BlackTexture:Texture Read GetBlackTexture;

      Property DefaultColorTable:Texture Read GetDefaultColorTable;
      Property DefaultNormalMap:Texture Read GetDefaultNormalMap;

      Property Textures[Name:TERRAString]:Texture Read GetTexture; Default;
  End;

  DefaultColorTableTexture = Class(Texture)
    Public
      Procedure Build(); Override;
  End;

Var
  _TextureFormatList:Array Of TextureFormat;
  _TextureFormatCount:Integer = 0;
  _TextureMemory:Cardinal;

Procedure RegisterTextureFormat(ClassType:TextureClass; Extension:TERRAString);

Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_Application, TERRA_Log, TERRA_GraphicsManager, TERRA_OS,
  TERRA_FileUtils, TERRA_FileStream, TERRA_FileManager, TERRA_ColorGrading
  {$IFDEF FRAMEBUFFEROBJECTS},TERRA_FramebufferObject{$ENDIF};

Var
  _TextureManager:ApplicationObject = Nil;

Procedure RegisterTextureFormat(ClassType:TextureClass; Extension:TERRAString);
Begin
  Inc(_TextureFormatCount);
  SetLength(_TextureFormatList, _TextureFormatCount);
  _TextureFormatList[Pred(_TextureFormatCount)].Extension := Extension;
  _TextureFormatList[Pred(_TextureFormatCount)].ClassType := ClassType;
End;
  
Class Function TextureManager.Instance:TextureManager;
Begin
  If _TextureManager = Nil Then
    _TextureManager := InitializeApplicationComponent(TextureManager, GraphicsManager);
    
  Result := TextureManager(_TextureManager.Instance);
End;

Function TextureManager.GetTexture(Name:TERRAString):Texture;
Var
  I:Integer;
Var
  S:TERRAString;
  TextureFormat:TextureClass;
  Info:ImageClassInfo;
Begin
  Result := Nil;

  Name := StringTrim(Name);
  Name := GetFileName(Name, True);
  If (Name='') Then
    Exit;

  Result := Texture(GetResource(Name));
  If Not Assigned(Result) Then
  Begin
    S := '';
    TextureFormat := Nil;

    I := 0;
    While (S='') And (I<_TextureFormatCount) Do
    Begin
      S := FileManager.Instance.SearchResourceFile(Name+'.'+_TextureFormatList[I].Extension);
      If S<>'' Then
      Begin
        TextureFormat := _TextureFormatList[I].ClassType;
        Break;
      End;
      Inc(I);
    End;

    I := 0;
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Searching for file with extension for '+Name);{$ENDIF}
    While (S='') And (I<GetImageExtensionCount()) Do
    Begin
      Info := GetImageExtension(I);
      S := FileManager.Instance.SearchResourceFile(Name+'.'+Info.Name);
      Inc(I);
    End;

    If S<>'' Then
    Begin
      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Found '+S+'...');{$ENDIF}

      If Assigned(TextureFormat) Then
        Result := TextureFormat.Create(S)
      Else
        Result := Texture.Create(S);

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture class instantiated sucessfully!');{$ENDIF}

      If (Pos('_',S)>0) Then
        Result.Priority := 30
      Else
        Result.Priority := 50;

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture loading priority set!');{$ENDIF}

      Result.MipMapped := (GraphicsManager.Instance.Settings.Shaders.Avaliable);
      Result.Wrap := True;
      Result.BilinearFilter := True;

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture settings set!');{$ENDIF}

      Self.AddResource(Result);

      {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture added to manager!');{$ENDIF}
    End Else
    Begin
      S := Self.ResolveResourceLink(Name);
      If S<>'' Then
      Begin
        Result := Self.GetTexture(S);
        If Assigned(Result) Then
          Exit;
      End;

      {If ValidateError Then
        RaiseError('Could not find texture. ['+Name+']');}
    End;
  End;
End;

Function TextureManager.CreateTextureWithColor(Name:TERRAString; TexColor:Color):Texture;
Begin
  Result := Texture.New(Name, 64, 64);
  Result.Uncompressed := True;
  Result.MipMapped := False;
  Result.BilinearFilter := False;
  Result.Update();
  Self.FillTextureWithColor(Result, TexColor);
End;

Procedure TextureManager.FillTextureWithColor(Tex: Texture; TexColor: Color);
Var
  Buffer:Image;
Begin
  If (Tex = Nil) Then
    Exit;

  Buffer := Image.Create(Tex.Width, Tex.Height);
  Buffer.FillRectangleByUV(0,0,1,1, TexColor);
  Tex.UpdateRect(Buffer, 0, 0);
  Buffer.Release();
End;

Function TextureManager.GetBlackTexture: Texture;
Begin
  If (Not Assigned(_BlackTexture)) Then
    _BlackTexture := Self.CreateTextureWithColor('default_black', ColorBlack)
  Else
  If (_BlackTexture.Status <> rsReady) Then
  Begin
    _BlackTexture.Update();
    FillTextureWithColor(_BlackTexture, ColorBlack);
  End;

  Result := _BlackTexture;
End;

Function TextureManager.GetWhiteTexture: Texture;
Begin
  If (Not Assigned(_WhiteTexture)) Then
    _WhiteTexture := Self.CreateTextureWithColor('default_white', ColorWhite)
  Else
  If (_WhiteTexture.Status <> rsReady) Then
  Begin
    _WhiteTexture.Update();
    FillTextureWithColor(_WhiteTexture, ColorWhite);
  End;

  Result := _WhiteTexture;
End;

Function TextureManager.GetNullTexture: Texture;
Begin
  If (Not Assigned(_NullTexture)) Then
    _NullTexture := Self.CreateTextureWithColor('default_null', ColorNull)
  Else
  If (_NullTexture.Status <> rsReady) Then
  Begin
    _NullTexture.Update();
    FillTextureWithColor(_NullTexture, ColorNull);
  End;

  Result := _NullTexture;
End;

Function GetDefaultNormalColor():Color;
Begin
  Result := ColorCreate(128,128,255);
End;

Function TextureManager.GetDefaultNormalMap:Texture;
Begin
  If (Not Assigned(_DefaultNormalMap)) Then
    _DefaultNormalMap := Self.CreateTextureWithColor('default_normal', GetDefaultNormalColor())
  Else
  If (_DefaultNormalMap.Status <> rsReady) Then
  Begin
    _DefaultNormalMap.Update();
    FillTextureWithColor(_DefaultNormalMap, GetDefaultNormalColor());
  End;

  Result := _DefaultNormalMap;
End;

Function MyTest(P:Color):Color; CDecl;
Begin
  Result :=P; Exit;

   P:= ColorRGBToHSL(P);
   P.R := 140;
   Result := ColorHSLToRGB(P);
   Result := ColorGreen;
End;

Function TextureManager.GetDefaultColorTable:Texture;
Begin
  If (Not Assigned(_DefaultColorTable)) Then
    _DefaultColorTable := DefaultColorTableTexture.New('default_colortable', 256, 16);

  Result := _DefaultColorTable;
End;

Procedure TextureManager.Release;
begin
  If Assigned(_WhiteTexture) Then
    _WhiteTexture.Release;

  If Assigned(_BlackTexture) Then
    _BlackTexture.Release();

  If Assigned(_NullTexture) Then
    _NullTexture.Release();

  If Assigned(_DefaultColorTable) Then
    _DefaultColorTable.Release();

  If (Assigned(_DefaultNormalMap)) Then
    _DefaultNormalMap.Release;

  Inherited;

  _TextureManager := Nil;
End;

{ Texture }
Class Function Texture.RAM:Cardinal;
Begin
  Result := _TextureMemory;
End;

Constructor Texture.New(Const Name:TERRAString; TextureWidth, TextureHeight:Cardinal);
Begin
  _Location := '';
  _Name := Name;
  _Width := TextureWidth;
  _Height := TextureHeight;
  _Ratio := VectorCreate2D(1, 1);

  _SourceFormat := 0;
  _TargetFormat := 0;
  _ByteFormat := 0;

  If (Not GraphicsManager.Instance.Settings.NPOT.Avaliable) Then
  Begin
    _Width := IntMin(IntMax(NearestPowerOfTwo(_Width), MinTextureSize), GraphicsManager.Instance.Settings.MaxTextureSize);
    _Height := IntMin(IntMax(NearestPowerOfTwo(_Height), MinTextureSize), GraphicsManager.Instance.Settings.MaxTextureSize);

    _Ratio := VectorCreate2D(_Width/TextureWidth, _Height/TextureHeight);
  End;

  _Source := Image.Create(_Width, _Height);
  _Source.Process(IMP_FillColor, ColorWhite);

  _Dynamic := True;

  CheckNPOT();

  Uncompressed := False;
  _Wrap := True;
  _MipMapped := True;
  _BilinearFilter := True;
End;

Constructor Texture.New(Const Name:TERRAString; Source:Image);
Begin
  If (Source = Nil) Then
    Self.New(Name, 128, 128)
  Else
  Begin
    AdjustRatio(Source);
    Self.New(Name, Source.Width, Source.Height);
    Self.Update();
    Self.UpdateRect(Source);
  End;
End;

Function Texture.Load(Source: Stream):Boolean;
Var
  Ofs:Cardinal;
Begin
  _Dynamic := False;
  Uncompressed := False;
  Ofs := Source.Position;
  _Source := Image.Create(Source);

  AdjustRatio(_Source);

  _Width := _Source.Width;
  _Height := _Source.Height;
  
  CheckNPOT();

  If (StringContains('_normal', Source.Name)) Then
    Uncompressed := True;

  _SourceFormat := 0;
  _TargetFormat := 0;
  _ByteFormat := 0;

  Result := True;
End;

Function Texture.Unload:Boolean;
Var
	MemCount:Integer;
  I,S:Integer;
Begin
  If (Length(_Handles)>0) Then
  Begin
  	MemCount := _Size * _FrameCount;
  	If (_TextureMemory>=MemCount) Then
	    Dec(_TextureMemory, MemCount);

    For I:=0 To Pred(_FrameCount) Do
    If (Application.Instance<>Nil) And (Self._ContextID = Application.Instance.ContextID) Then
      GraphicsManager.Instance.DeleteTexture(_Handles[I]);

    _Handles := Nil;
    _FrameCount := 0;
  End;

  If (Assigned(_Source)) Then
  Begin
    _Source.Release();
	  _Source := Nil;
	End;

  _Status := rsUnloaded;
  Result := True;
End;

{$DEFINE FORCERGBA}
{$IFDEF IPHONE}
{$DEFINE FORCERGBA}
{$ENDIF}
{$IFDEF LINUX}
{$DEFINE FORCERGBA}
{$ENDIF}

Function Texture.Update:Boolean;
Var
  W,H,I, J, S:Cardinal;
  Mult:Single;
  Pixels:PWord;
Begin
  Inherited Update();

  Result := False;

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'Update');{$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Allocating pixels');{$ENDIF}
  If (Not Assigned(_Source)) Then
  Begin
    _Source := Image.Create(_Width, _Height);
    _Source.Process(IMP_FillColor, ColorWhite);
  End;

  _FrameCount := _Source.FrameCount;

  If Application.Instance.IsConsole Then
  Begin
    Result := True;
    Exit;
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Generating texture');{$ENDIF}
  If (Length(_Handles)<=0) Then
  Begin
    SetLength(_Handles, _FrameCount);
    For I:=0 To Pred(_FrameCount) Do
  	  _Handles[I] := GraphicsManager.Instance.GenerateTexture();
  End;

  ConvertToBestFormat(_Source, Pixels);

  For I:=0 To Pred(_FrameCount) Do
  Begin
    If (_FrameCount>0) Then
    Begin
      _Source.SetCurrentFrame(I);
      Pixels := PWord(_Source.Pixels);
    End;

    glActiveTexture(GL_TEXTURE0);
    {$IFDEF PC}
    glEnable(GL_TEXTURE_2D);
    {$ENDIF}
    glBindTexture(GL_TEXTURE_2D, _Handles[I]);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Uploading texture frame '+IntToString(I));{$ENDIF}
    glTexImage2D(GL_TEXTURE_2D, 0, _TargetFormat, _Width, _Height, 0, _SourceFormat, _ByteFormat, Pixels);

    //_Source.Save('debug\temp\pp'+IntTOString(I)+'.png');

(*  If (_Format = GL_COMPRESSED_RGBA) Then
  Begin
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, @_Format);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPRESSED_IMAGE_SIZE, @_Size);
  End;
*)
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Applying texture settings');{$ENDIF}
    ApplySettings();
  End;

  Case _ByteFormat Of
  GL_UNSIGNED_SHORT_4_4_4_4,
  GL_UNSIGNED_SHORT_5_5_5_1,
  GL_UNSIGNED_SHORT_5_6_5:
    Begin
      Mult := 2;
    End;

  Else
    Mult := 4.0;
  End;

  _Size := Trunc(Mult * _Width * _Height);

  {$IFDEF PC}
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @_Width);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @_Height);
  {$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Freeing pixels');{$ENDIF}

  {$IFNDEF KEEPCOPYONRAM}
  _Source.Release();
  _Source := Nil;
  {$ENDIF}

  If (Self._Location='') Then
    Self._Status := rsReady;

  Inc(_TextureMemory, _Size * _FrameCount);

  Result := True;

  Self.Build();

  _AnimationStart := GetTime();
  _CurrentFrame := 0;

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure Texture.Build;
Begin
// do nothing
End;

Var
  _TextureSlots:Array[0..7] Of Texture;

Procedure Texture.Bind(Slot:Integer);
Begin
  glActiveTexture(GL_TEXTURE0 + Slot);

  {$IFDEF PC}
  glEnable(GL_TEXTURE_2D);
  {$ENDIF}

  If (Self = Nil) Or (Not Self.IsReady()) Then
  Begin
    glBindTexture(GL_TEXTURE_2D, 0);
    Exit;
  End;

{  If (_TextureSlots[Slot] = MyTexture) Then
    Exit;

  _TextureSlots[Slot] := MyTexture;}

  _CurrentFrame := GetCurrentFrame();

  glBindTexture(GL_TEXTURE_2D, _Handles[_CurrentFrame]);
  If (_SettingsChanged) Then
    Self.ApplySettings();
End;

Procedure Texture.UpdateRect(Source:Image; X,Y:Integer);
Var
  Pixels:PWord;
Begin
  If Length(_Handles)<=0 Then
  Begin
    If (_Width=Source.Width) And (_Height = Source.Height) Then
    Begin
      If (_Source <> Nil) Then
        _Source.Release();

      _Source := Image.Create(Source);
    End Else
    If (Assigned(_Source)) Then
      _Source.Blit(X,Y, 0, 0, Source.Width, Source.Height, Source);

    Exit;
  End;

  Self.ConvertToBestFormat(Source, Pixels);

	glBindTexture(GL_TEXTURE_2D, _Handles[0]);
	//glTexSubImage2D(GL_TEXTURE_2D, 0, X, Y, Source.Width, Source.Height, _TargetFormat, _ByteFormat, Pixels);
  glTexSubImage2D(GL_TEXTURE_2D, 0, X, Y, Source.Width, Source.Height, GL_RGBA, GL_UNSIGNED_BYTE, Pixels);

  If (MipMapped) Then
    Self.ApplySettings();
End;

Procedure Texture.UpdateRect(Source:Image);
Var
  Pixels:PByte;
Begin
  If (_Width<>Source.Width) Or (_Height <> Source.Height) Then
  Begin
    RaiseError('Invalid texture dimensions: '+IntToString(_Width)+' x' + IntToString(_Height));
    Exit;
  End;

  Self.UpdateRect(Source, 0, 0);
End;

Procedure Texture.ApplySettings;
Begin
  _SettingsChanged := False;

  {$IFDEF PC}
    {$IFNDEF WINDOWS}
    MipMapped := False; {FIXME}
    {$ENDIF}
  {$ENDIF}

  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
    MipMapped := False;

  {$IFDEF MOBILE}
  If (_NPOT) Then
  Begin
    BilinearFilter := False;
    MipMapped := False;
    Wrap := False;
  End;
  {$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Setting texture filtering for '+Name);{$ENDIF}

  If (BilinearFilter) Then
  Begin
    If (MipMapped) Then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR)
    Else
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  End Else
  Begin
    If (MipMapped) Then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST)
    Else
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Setting wrap mode for '+Name);{$ENDIF}

  If (Wrap) Then
  Begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  End Else
  Begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  End;

  {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Generating mipmap for '+Name);{$ENDIF}
  If (MipMapped) Then
  Begin
    glGenerateMipmap(GL_TEXTURE_2D);
  End;

  {$IFNDEF MOBILE}
	If (GraphicsManager.Instance.Settings.Textures.Quality>=QualityHigh) And (GraphicsManager.Instance.Settings.MaxAnisotrophy > 1) Then
  Begin
	  glTexParameteri(GL_TEXTURE_2D, GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, GraphicsManager.Instance.Settings.MaxAnisotrophy);
  End;
  {$ENDIF}
End;

Procedure Texture.SetWrap(Value:Boolean);
Begin
  If (_Wrap= Value) Then
    Exit;

  _SettingsChanged := True;
  _Wrap := Value;
End;

Procedure Texture.SetMipMapping(Value:Boolean);
Begin
  If (_MipMapped = Value) Then
    Exit;

  _SettingsChanged := True;
  _MipMapped := Value;
End;

Procedure Texture.SetFilter(Value:Boolean);
Begin
  If (_BilinearFilter = Value) Then
    Exit;

  _SettingsChanged := True;
  _BilinearFilter := Value;
End;

Function Texture.GetImage:Image;
Begin
  Log(logDebug, 'Texture', 'Getting image from texture '+Self.Name);

  If Assigned(_Source) Then
    Result := Image.Create(_Source)
  Else
  Begin
    Result := Image.Create(_Width, _Height);

  {$IFDEF PC}
    glActiveTexture(GL_TEXTURE0);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, _Handles[_CurrentFrame]);

    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @_Width);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @_Height);

    glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, Result.Pixels);

    Result.Process(IMP_FlipVertical);
  {$ENDIF}
  End;
End;


Class Function Texture.GetManager: Pointer;
Begin
  Result := TextureManager.Instance;
End;

Function Texture.GetPixel(X, Y: Integer): Color;
Begin
  Result := ColorBlack;
End;

Procedure TextureManager.OnContextLost;
Begin
  Inherited;

  If Assigned(_WhiteTexture) Then
    _WhiteTexture.Unload();

  If Assigned(_BlackTexture) Then
    _BlackTexture.Unload();

  If Assigned(_NullTexture) Then
    _NullTexture.Unload();

  If Assigned(_DefaultNormalMap) Then
    _DefaultNormalMap.Unload();

  If Assigned(_DefaultColorTable) Then
    _DefaultColorTable.Unload();
End;

{ DefaultColorTable }
Procedure DefaultColorTableTexture.Build;
Var
  Temp:Image;
Begin
  Temp := CreateColorTable(16);
  Self.UpdateRect(Temp);
  Temp.Release;

  Self.MipMapped := False;
  Self.Wrap := False;
End;

Function Texture.GetHandle(Frame:Integer): Cardinal;
Begin
  If (Frame<0) Or (Frame>=_FrameCount) Then
    Frame := _CurrentFrame;

  Result := _Handles[Frame];
End;

Function Texture.GetCurrentFrame: Integer;
Var
  Delta:Single;
Begin
  If (_FrameCount<=1) Then
    Result := 0
  Else
  Begin
    Delta := (GetTime - _AnimationStart);
    Delta := Delta / 1000;
    If (Delta>1) Then
      Delta := Frac(Delta);

    Result := Trunc(Delta * Pred(_FrameCount));
  End;
End;

Var
  Scratch16:Array Of Word;

Procedure Texture.ConvertToBestFormat(Source:Image; Var Pixels:PWord);
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
      IntToString(2);}

  
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
End;

Procedure Texture.CheckNPOT;
Var
  W,H:Cardinal;
Begin
  W := NearestPowerOfTwo(_Width);
  H := NearestPowerOfTwo(_Height);

  _NPOT := (W<>_Width) Or (H<>_Height);
End;

Procedure Texture.AdjustRatio(Source:Image);
Var
  W,H:Cardinal;
Begin
  If Source = Nil Then
    Exit;

  If (Not GraphicsManager.Instance.Settings.NPOT.Avaliable) Then
  Begin
    W := IntMin(IntMax(NearestPowerOfTwo(Source.Width), MinTextureSize), GraphicsManager.Instance.Settings.MaxTextureSize);
    H := IntMin(IntMax(NearestPowerOfTwo(Source.Height), MinTextureSize), GraphicsManager.Instance.Settings.MaxTextureSize);

    _Ratio := VectorCreate2D(W/Source.Width, H/Source.Height);

    If (W<>Source.Width) Or (H<>Source.Height) Then
      Log(logDebug, 'Texture', self.Name+ ' needs resizing: '+IntToString(W) +' ' +IntToString(H));

    Source.Resize(W,H);
  End Else
  Begin
    _Ratio := VectorCreate2D(1, 1);
  End;
End;


End.
