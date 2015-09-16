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

      Function GetItem(Name:TERRAString):TERRAFont;

      Property DefaultFont:TERRAFont Read GetDefaultFont;

      Property Fonts[Name:TERRAString]:TERRAFont Read GetItem; Default;
   End;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Application, TERRA_Sort, TERRA_TextureManager, TERRA_Sprite, TERRA_Engine,
  TERRA_Log, TERRA_FileUtils, TERRA_MemoryStream, TERRA_ImageDrawing, TERRA_Image, TERRA_Collections,
 TERRA_GraphicsManager, TERRA_FileManager, TERRA_FileFormat, TERRA_Packer, TERRA_DistanceField, TERRA_TextureAtlas, TERRA_Texture;


{ FontManager }
Function FontManager.GetItem(Name:TERRAString):TERRAFont;
Var
  Format:TERRAFileFormat;
  Location:TERRALocation;
Begin
  Result := Nil;

  If (Name='') Then
    Exit;

  Name := GetFileName(Name, True);

  Result := TERRAFont(Self.GetResource(Name));
  If Assigned(Result) Then
    Exit;

  Format := Engine.Formats.FindLocationFromName(Name, TERRAFont, Location);
  If Assigned(Format) Then
  Begin
    Result := TERRAFont.Create(rtLoaded, Location);
    Result.Priority := 90;
    Self.AddResource(Result);
  End Else
    Engine.Log.Write(logWarning, 'Font', 'Could not find font. ['+Name +']');
End;


{$I default_font.inc}
Function FontManager.GetDefaultFont: TERRAFont;
Var
  Glyph:FontGlyph;
  I:Integer;
  ID:TERRAChar;
  Src:TERRAStream;
  SrcImg, SubImg:TERRAImage;

  Procedure SubPic(X, Y, W, H:Integer);
  Begin
    SubImg := SrcImg.Crop(X, Y, X + W , Y + H);
  End;
Begin
  Result := _DefaultFont;
  If Assigned(Result) Then
    Exit;

  _DefaultFont := TERRAFont.Create(rtDynamic);
  Result := _DefaultFont;

  Src := MemoryStream.Create(bm_size, @bm_data[0]);
  SrcImg := TERRAImage.Create(Src);
  ReleaseObject(Src);

  For I:=32 To 128 Do
  Begin
    ID := TERRAChar(I);
    Glyph := Nil;
    SubImg := Nil;

	  Case ID Of
	  #32: SubPic(8*20, 12*2, 4, 12);
	  '#':	SubPic(8*10, 12*2, 8, 12);
	  '!':	SubPic(8*11, 12*2, 4, 12);
	  '?': SubPic(8*12, 12*2, 8, 12);
	  ',': SubPic(8*13, 12*2, 4, 12);
    '.': SubPic(8*14, 12*2, 4, 12);
	  '$': SubPic(8*15, 12*2, 8, 12);
	  ':': SubPic(8*16, 12*2, 8, 12);
	  '+': SubPic(8*17, 12*2, 8, 12);
	  '-': SubPic(8*18, 12*2, 8, 12);
	  '''':SubPic(8*19, 12*2, 4, 12);
	  #48..#57: SubPic(8*(Ord(ID)-48), 12*2, 8, 12);
	  #65..#90: SubPic(8*(Ord(ID)-65), 12*0, 8, 12);
	  #97..#122:  SubPic(8*(Ord(ID)-97), 12*1, 8, 12);
	  End;

    If Assigned(SubImg) Then
    Begin
      Glyph := Result.AddGlyphFromImage(ID, SubImg, 0, 0);
      ReleaseObject(SubImg);
    End;

	  If (Assigned(Glyph)) Then
    Begin
		  If (ID=#73) Then
  			Glyph.XAdvance := 5
      Else
		  If (ID=#105) Then
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
  It:TERRAIterator;
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
