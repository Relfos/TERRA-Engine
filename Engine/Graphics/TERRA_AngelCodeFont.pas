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
 * TERRA_AngelCodeFont
 * Implements bitmap fonts made with the Angelcode Font tool (binary format only)
 ***********************************************************************************************************************
}
Unit TERRA_AngelCodeFont;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Font, TERRA_Stream, TERRA_FileUtils, TERRA_Image;

Implementation

Const
  BMFFont:FileHeader = 'BMF'#3;

Function ValidateFNT(Source:Stream):Boolean;
Var
  Header:FileHeader;
Begin
  Source.Read(@Header, SizeOf(Header));
  Result := (Header = BMFFont);
End;

Function LoadFNT(Source:Stream; Font:Font):Boolean;
Var
  S:TERRAString;
  I,J:Integer;
  BlockSize, CharCount:Integer;
  Tag:Byte;
  Id:Cardinal;
  Header:FileHeader;

  PageCount:Word;
  Pages:Array Of Image;

  Glyph:FontGlyph;

  Next:Cardinal;
  Ammount:SmallInt;

  Img:Image;

  FileName:TERRAString;

  GlyphID:Cardinal;
  GX, GY:Word;
  GlyphWidth, GlyphHeight:Word;
  XOfs, YOfs, XAdvance:SmallInt;
  PageID:Byte;

  _LineHeight:Word;
  _Base:Word;
  _scaleW:Word;
  _scaleH:Word;
Begin
  Source.Read(@Header, SizeOf(Header));
  Result := (Header = BMFFont);
  If Not Result Then
    Exit;

  PageCount := 0;

  Repeat
    Source.Read(@Tag, 1);
    Source.Read(@BlockSize, 4);

    Case Tag Of
    2:  Begin
          Source.Read(@_LineHeight, 2);
          Source.Read(@_Base, 2);
          Source.Read(@_scaleW, 2);
          Source.Read(@_scaleH, 2);
          Source.Read(@PageCount, 2);
          Source.Skip(5);

          SetLength(Pages, PageCount);
      End;

    3:  Begin
          For I:=0 To Pred(PageCount) Do
          Begin
            Source.ReadString(FileName, True);
            Img := Image.Create(FileName);
          End;
        End;

    4:  Begin
          CharCount := BlockSize Div 20;
          For I:=0 To Pred(CharCount) Do
          Begin
            Source.ReadCardinal(GlyphID);
            Source.ReadWord(GX);
            Source.ReadWord(GY);
            Source.ReadWord(GlyphWidth);
            Source.ReadWord(GlyphHeight);
            Source.ReadSmallInt(XOfs);
            Source.ReadSmallInt(YOfs);
            Source.ReadSmallInt(XAdvance);
            Source.ReadByte(PageID);
            Source.Skip(1);
            Glyph.KerningCount := 0;

            Glyph := Font.AddGlyph(ID, Pages[PageID].SubImage(GX, GY, GX + GlyphWidth, GY + GlyphHeight), XOfs, YOfs, XAdvance);

            (*{$IFDEF DISTANCEFIELD}
            Glyph.X := Glyph.X Div 8;
            Glyph.Y := Glyph.Y Div 8;
            Glyph.Width := Glyph.Width Div 8;
            Glyph.Height := Glyph.Height Div 8;
            Glyph.XOfs := Glyph.XOfs Div 8;
            Glyph.YOfs := Glyph.YOfs Div 8;
            Glyph.XAdvance := Glyph.XAdvance Div 8;
            {$ENDIF}*)
          End;
        End;

    5:  Begin
          CharCount := BlockSize Div 10;
          For I:=0 To Pred(CharCount) Do
          Begin
            Source.Read(@ID, 4);
            Glyph := Font.GetGlyph(ID);
            If (Glyph = Nil) Then
            Begin
              Source.Skip(6);
              Continue;
            End;

            Source.Read(@Next, 4);
            Source.Read(@Ammount, 2);

            Glyph.AddKerning(Next, Ammount);

            (*{$IFDEF DISTANCEFIELD}
            Kerning.Ammount := Kerning.Ammount Div 8;
            {$ENDIF}*)
          End;

        End;

    Else
      Begin
        Source.Skip(BlockSize);
      End;
    End;
  Until Source.EOF;

  Result := True;
End;

{Function Font.Save(FileName:TERRAString): Boolean;
Var
  S:TERRAString;
  I,J:Integer;
  BlockSize:Integer;
  Tag:Byte;
  Id:Cardinal;
  Header:FileHeader;
  Glyph:PFontGlyph;
  Kerning:PFontKerning;
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  Header := BMFFont;
  Dest.Write(@Header, SizeOf(Header));

  Tag := 2;
  Dest.Write(@Tag, 1);
  BlockSize := 2 + 2 + 2 + 2 +2 + 5;
  Dest.Write(@BlockSize, 4);
  Dest.Write(@_LineHeight, 2);
  Dest.Write(@_Base, 2);
  Dest.Write(@_scaleW, 2);
  Dest.Write(@_scaleH, 2);
  Dest.Write(@_PageCount, 2);
  For I:=1 To 5 Do
    Dest.Write(@Tag, 1);

  Tag := 3;
  Dest.Write(@Tag, 1);
  BlockSize := 0;
  For I:=0 To Pred(_PageCount) Do
    Inc(BlockSize, Length(_Pages[I]._FileName) + 1 );
  Dest.Write(@BlockSize, 4);
  For I:=0 To Pred(_PageCount) Do
    Dest.WriteString(_Pages[I]._FileName, True);

  Tag := 4;
  Dest.Write(@Tag, 1);
  BlockSize := _GlyphCount * (4+2+2+2+2+2+2+2+1+1);
  Dest.Write(@BlockSize, 4);
  For I:=0 To Pred(_GlyphCount) Do
  Begin
    Glyph := @(_Glyphs[I]);
    Dest.Write(@Glyph.ID, 4);
    Dest.Write(@Glyph.X,2);
    Dest.Write(@Glyph.Y,2);
    Dest.Write(@Glyph.Width,2);
    Dest.Write(@Glyph.Height,2);
    Dest.Write(@Glyph.XOfs ,2);
    Dest.Write(@Glyph.YOfs,2);
    Dest.Write(@Glyph.XAdvance,2);
    Dest.Write(@Glyph.Page,1);
    Dest.Write(@Tag, 1);
  End;

  Tag := 5;
  Dest.Write(@Tag, 1);
  BlockSize := 0;
  For I:=0 To Pred(_GlyphCount) Do
    For J:=0 To Pred(_Glyphs[I].KerningCount) Do
      Inc(BlockSize, 4+4+2);
  Dest.Write(@BlockSize, 4);
  For I:=0 To Pred(_GlyphCount) Do
    For J:=0 To Pred(_Glyphs[I].KerningCount) Do
    Begin
      Kerning := @_Glyphs[I].KerningList[J];
      Dest.Write(@_Glyphs[I].ID, 4);
      Dest.Write(@Kerning.Next, 4);
      Dest.Write(@Kerning.Ammount, 2);
    End;

  ReleaseObject(Dest);
	Result := True;
End;
}

Initialization
  RegisterFontFormat('FNT', ValidateFNT, LoadFNT);
End.
