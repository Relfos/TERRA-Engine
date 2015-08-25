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
 * TERRA_Font
 * Implements a Font Resource with unicode text support
 ***********************************************************************************************************************
}
Unit TERRA_FontManager;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Resource, TERRA_Stream, TERRA_Renderer, TERRA_ResourceManager, TERRA_Font, TERRA_Viewport, TERRA_Matrix3x3, TERRA_Color, TERRA_ClipRect;

Type
  FontManager = Class(ResourceManager)
    Protected
      _DefaultFont:TERRAFont;

      Function GetDefaultFont:TERRAFont;

    Public
      Procedure Release; Override;

      Procedure Update; Override;

      Function DrawGlyph(View:TERRAViewport; X,Y,Z:Single; Const Transform:Matrix3x3; Const Scale:Single; Glyph:FontGlyph; Const Outline, Glow, A,B,C,D:ColorRGBA; Clip:TERRAClipRect; Italics:Boolean; Var DestSprite:FontSprite):Boolean;

      Function GetItem(Name:TERRAString):TERRAFont;

      Property DefaultFont:TERRAFont Read GetDefaultFont;

      Property Fonts[Name:TERRAString]:TERRAFont Read GetItem; Default;
   End;

  Function GetFontLoader(Source:Stream):FontLoader;
  Procedure RegisterFontFormat(Name:TERRAString; Validate:FontStreamValidateFunction; Loader:FontLoader);


Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Application, TERRA_Sort, TERRA_TextureManager, TERRA_Sprite,
  TERRA_Log, TERRA_FileUtils, TERRA_MemoryStream, TERRA_ImageDrawing, TERRA_Image, TERRA_Collections,
  TERRA_GraphicsManager, TERRA_FileManager, TERRA_Packer, TERRA_DistanceField, TERRA_TextureAtlas, TERRA_Texture;

Var
  _FontExtensions:Array Of FontClassInfo;
  _FontExtensionCount:Integer;

Function GetFontLoader(Source:Stream):FontLoader;
Var
  Pos:Cardinal;
  I:Integer;
Begin
  Result := Nil;
  If Not Assigned(Source) Then
    Exit;

  Pos := Source.Position;

  For I:=0 To Pred(_FontExtensionCount) Do
  Begin
    Source.Seek(Pos);
    If _FontExtensions[I].Validate(Source) Then
    Begin
      Log(logDebug, 'Font', 'Found '+_FontExtensions[I].Name);
      Result := _FontExtensions[I].Loader;
      Break;
    End;
  End;

  Source.Seek(Pos);
End;

Procedure RegisterFontFormat(Name:TERRAString; Validate:FontStreamValidateFunction; Loader:FontLoader);
Var
  I,N:Integer;
Begin
  Name := StringLower(Name);

  For I:=0 To Pred(_FontExtensionCount) Do
  If (_FontExtensions[I].Name = Name) Then
    Exit;

  N := _FontExtensionCount;
  Inc(_FontExtensionCount);
  SetLength(_FontExtensions, _FontExtensionCount);
  _FontExtensions[N].Name := Name;
  _FontExtensions[N].Validate :=Validate;
  _FontExtensions[N].Loader := Loader;
End;

{ FontManager }
Function FontManager.GetItem(Name:TERRAString):TERRAFont;
Var
  FontName, FileName, S:TERRAString;
  I:Integer;
Begin
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  FontName := Name;

  Name := GetFileName(Name, True);

  S := '';
  I := 0;
  While (S='') And (I<_FontExtensionCount) Do
  Begin
    FileName := FontName+'.'+_FontExtensions[I].Name;
    S := FileManager.Instance.SearchResourceFile(FileName);
    Inc(I);
  End;

  If S<>'' Then
  Begin
    Result := TERRAFont.Create(rtLoaded, S);
    Result.Priority := 90;
    Self.AddResource(Result);
  End Else
    Log(logWarning, 'Font', 'Could not find font. ['+Name +']');
End;

Function FontManager.DrawGlyph(View:TERRAViewport; X,Y,Z:Single; Const Transform:Matrix3x3; Const Scale:Single; Glyph:FontGlyph; Const Outline, Glow, A,B,C,D:ColorRGBA; Clip:TERRAClipRect; Italics:Boolean; Var DestSprite:FontSprite):Boolean;
Var
  Filter:TextureFilterMode;
  Item:TextureAtlasItem;
  Target:FontSprite;
  Tex:TERRATexture;
  Skew:Single;
Begin
  Result := False;

  If DestSprite = Nil Then
    Exit;

  Filter := filterBilinear;

  Item := Glyph.Item;
  If Item = Nil Then
    Exit;

  Tex := Glyph.Font.Atlas.GetTexture(Item.PageID);
  If Tex = Nil Then
    Exit;

  If (Italics) Then
    Skew := 5.0
  Else
    Skew := 0.0;

  DestSprite.Flags := DestSprite.Flags Or Sprite_Font;
  DestSprite.Layer := Z;
  DestSprite.Texture := Tex;
  DestSprite.SetTransform(Transform);
  DestSprite.ClipRect := Clip;
  DestSprite.Outline := Outline;
  DestSprite.Glow := Glow;

  DestSprite.AddGlyph(X, Y, Glyph, A, B, C, D, Skew, Scale);

  Result := True;
End;

{$I default_font.inc}
Function FontManager.GetDefaultFont: TERRAFont;
Var
  Glyph:FontGlyph;
  I, ID:Integer;
  Src:Stream;
  SrcImg, SubImg:TERRAImage;

  Procedure SubPic(X, Y, W, H:Integer);
  Begin
    SubImg := SrcImg.Crop(X, Y, X + W , Y + H);
  End;
Begin
  Result := _DefaultFont;
  If Assigned(Result) Then
    Exit;

  _DefaultFont := TERRAFont.Create(rtDynamic, 'default_font');
  Result := _DefaultFont;

  Src := MemoryStream.Create(bm_size, @bm_data[0]);
  SrcImg := TERRAImage.Create(Src);
  ReleaseObject(Src);

  For I:=32 To 128 Do
  Begin
    ID := I;
    Glyph := Nil;
    SubImg := Nil;

	  Case ID Of
	  32: SubPic(8*20, 12*2, 4, 12);
	  Ord('#'):	SubPic(8*10, 12*2, 8, 12);
	  Ord('!'):	SubPic(8*11, 12*2, 4, 12);
	  Ord('?'): SubPic(8*12, 12*2, 8, 12);
	  Ord(','): SubPic(8*13, 12*2, 4, 12);
    Ord('.'): SubPic(8*14, 12*2, 4, 12);
	  Ord('$'): SubPic(8*15, 12*2, 8, 12);
	  Ord(':'): SubPic(8*16, 12*2, 8, 12);
	  Ord('+'): SubPic(8*17, 12*2, 8, 12);
	  Ord('-'): SubPic(8*18, 12*2, 8, 12);
	  Ord(''''):SubPic(8*19, 12*2, 4, 12);
	  48..57: SubPic(8*(ID-48), 12*2, 8, 12);
	  65..90: SubPic(8*(ID-65), 12*0, 8, 12);
	  97..122:  SubPic(8*(ID-97), 12*1, 8, 12);
	  End;

    If Assigned(SubImg) Then
    Begin
      Glyph := Result.AddGlyph(ID, SubImg, 0, 0);
      ReleaseObject(SubImg);
    End;

	  If (Assigned(Glyph)) Then
    Begin
		  If (ID=73) Then
  			Glyph.XAdvance := 5
      Else
		  If (ID=105) Then
			  Glyph.XAdvance := 5
      Else
    		Glyph.XAdvance := Glyph.Width;
    End;
  End;

  ReleaseObject(SrcImg);

  Result.Update();
End;

Procedure FontManager.Release;
Begin
  ReleaseObject(_DefaultFont);

  Inherited;
End;


Procedure FontManager.Update();
Var
  It:Iterator;
  Fnt:TERRAFont;
Begin
  If (Assigned(Self._DefaultFont)) And (Self._DefaultFont.NeedsRebuild) Then
    Self._DefaultFont.RebuildPages();

  It := Self.Resources.GetIterator();
  While It.HasNext() Do
  Begin
    Fnt := TERRAFont(It.Value);

    If (Fnt.NeedsRebuild) Then
      Fnt.RebuildPages();
  End;
  ReleaseObject(It);
End;


End.
