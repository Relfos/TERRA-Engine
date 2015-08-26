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
 * TERRA_TTF
 * Implements TTF font loading
 ***********************************************************************************************************************
}
Unit TERRA_TTF;

{$I terra.inc}
Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Font, TERRA_Color, TERRA_FileUtils, TERRA_EdgeList, TERRA_Image, TERRA_FileFormat;

{$RANGECHECKS OFF}

Type
  TTFFormat = Class(TERRAFileFormat)
    Public
      Function Identify(Source:TERRAStream):Boolean; Override;
      Function Load(Target:TERRAObject; Source:TERRAStream):Boolean; Override;
  End;


  TStBttVertex = record
     x,y,cx,cy: Smallint;
     vertexType,padding: Byte;
  end;

  TStBttVertexArray = Record
    List:Array Of TStBttVertex;
    Count:Integer;
  End;

  TStBttPoint = record
     x,y: Single;
  end;

  TStBttPointArray = Record
    List:Array Of TStBttPoint;
    Count:Integer;
  End;

  TStBttActiveEdge = Class(TERRAObject)
    x,dx: Integer;
    ey: Single;
    next: TStBttActiveEdge;
    valid: Integer;
  End;

  TTFContourArray = Record
    List:Array Of Integer;
    Count:Integer;
  End;

  TTFFont = Class(FontGlyphFactory)
    Private
      _Scale:Single;
      _Ready:Boolean;

      _Scanline:Array Of Byte;

      _GlyphCount:Integer;            // number of glyphs, needed for range checking


      _Data:PByteArray;              // pointer to .ttf file
      _DataSize:Cardinal;

      loca,head,glyf,hhea,hmtx, kern: Cardinal; // table locations as offset from start of .ttf
      index_map: Integer;                // a cmap mapping for our chosen character encoding
      indexToLocFormat: Integer;         // format needed to map from glyph index to glyph


      Function ttBYTE(Const offset: Cardinal): Byte;
      Function ttUSHORT(Const offset: Cardinal): Word;
      Function ttSHORT(Const offset: Cardinal): Smallint;
      Function ttULONG(Const offset: Cardinal): Cardinal;
      Function ttLONG(Const offset: Cardinal): Integer;

      Function stbtt_tag(Const offset: Cardinal; Const TableTag:FileHeader): Boolean;
      Function stbtt__find_table(Const TableTag:FileHeader): Cardinal;

      Procedure stbtt_GetGlyphHMetrics(GlyphIndex: Integer; var advanceWidth, leftSideBearing: Integer);
      Function stbtt_GetGlyphKernAdvance(glyph1: Integer; glyph2: Integer): Integer;
      Function stbtt_GetGlyphBitmap(scale_x, scale_y, shift_x, shift_y: Single; glyph: Integer; var xoff, yoff: Integer):TERRAImage;

      Procedure stbtt_GetGlyphShape(glyph_index: Integer; Out Result:TStBttVertexArray);
      Function stbtt__GetGlyfOffset(glyph_index: Integer): Integer;
      Function stbtt_GetGlyphBox(glyph_index: Integer; var x0, y0, x1, y1: Integer): Integer;
      Procedure stbtt_GetGlyphBitmapBox(glyph: Integer; scale_x, scale_y, shift_x, shift_y: Single; var ix0, iy0, ix1, iy1: Integer);

      Function stbtt_GetCodepointKernAdvance(ch1: Integer; ch2: Integer): Integer;
      Procedure stbtt_GetCodepointHMetrics(codepoint: Integer; var advanceWidth, leftSideBearing: Integer);
      Procedure stbtt_GetCodepointShape(unicode_codepoint: Integer; Out vertices: TStBttVertexArray);
      Function stbtt_GetCodepointBox(codepoint: Integer; var x0, y0, x1, y1: Integer): Integer;
      Procedure stbtt_GetCodepointBitmapBox(codepoint: Integer; scale_x, scale_y: Single; var ix0, iy0, ix1, iy1: Integer);

      Procedure stbtt_Rasterize(resultBitmap:TERRAImage; FlatnessInPixels: Single; Var Vertices:TStBttVertexArray; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff, Invert: Integer);
      Procedure stbtt_GetFontVMetrics(var ascent, descent, lineGap: Integer);
      Procedure stbtt_setvertex(var v:TStBttVertex; typeByte: Byte; x, y, cx, cy: Smallint);
      Procedure stbtt__rasterize(resultBitmap:TERRAImage; Var Pts:TStBttPointArray; Var Windings:TTFContourArray; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff, Invert: Integer);
      Procedure stbtt_FlattenCurves(Var Vertices:TStBttVertexArray; ObjSpaceFlatness: Single; Out Contours:TTFContourArray; Out Windings:TStBttPointArray);
      Procedure stbtt__add_point(Var points:TStBttPointArray; n: Integer; x, y: Single);
      Function stbtt__tesselate_curve(Var points:TStBttPointArray; var num_points: Integer; x0, y0, x1, y1, x2, y2, objspace_flatness_squared: Single; n: Integer): Integer;
      Procedure stbtt__rasterize_sorted_edges(resultBitmap:TERRAImage; e:EdgeList; vsubsample, off_x, off_y: Integer);
      Procedure stbtt__fill_active_edges(scanline: PByteArray; len: Integer; e:TStBttActiveEdge; max_weight: Integer);
      Function new_active(Const e: TStBttEdge; off_x: Integer; start_point: Single):TStBttActiveEdge;

    Public
      Procedure LoadFromStream(Source:TERRAStream); Overload;
      Procedure Release; Override;

      Function stbtt_FindGlyphIndex(unicode_codepoint: Integer):Word;

      Function GetCodepointBitmap(scaleX, scaleY: Single; codepoint: Integer; var xoff, yoff: Integer):TERRAImage;
      Function ScaleForPixelHeight(height: Single): Single;

      Function HasGlyph(ID:Cardinal):Boolean;

      Function InitGlyph(Font:TERRAFont; ID:Cardinal; Size:Integer):FontGlyph; Override;
      Function GetKerning(Current, Next:Cardinal):Integer; Override;

      Property Ready:Boolean Read _Ready;
    End;

Implementation

Uses TERRA_Log, TERRA_FileManager, TERRA_FontManager, TERRA_EngineManager, Math;

// platformID
const   STBTT_PLATFORM_ID_UNICODE   = 0;
const   STBTT_PLATFORM_ID_MAC       = 1;
const   STBTT_PLATFORM_ID_ISO       = 2;
const   STBTT_PLATFORM_ID_MICROSOFT = 3;

const   STBTT_MS_EID_SYMBOL        = 0;
const   STBTT_MS_EID_UNICODE_BMP   = 1;
const   STBTT_MS_EID_SHIFTJIS      = 2;
const   STBTT_MS_EID_UNICODE_FULL  = 10;

const  FIXSHIFT  = 10;
const  FIX       = (1 shl FIXSHIFT);
const  FIXMASK   = (FIX-1);

const STBTT_vmove = 1;
const STBTT_vline = 2;
const STBTT_vcurve = 3;

{ TTF }
Procedure TTFFont.LoadFromStream(Source:TERRAStream);
var
  cmap, t: Cardinal;
  encoding_record: Cardinal;
  i,numTables: Cardinal;
begin
  _Data := Nil;
  _DataSize := 0;
  _Ready := False;

  If (Source = Nil) then
  Begin
    Log(logError, 'TTF', 'Null stream!');
    Exit;
  End;

  // read file
  _DataSize := Source.Size;
  GetMem(_Data, _DataSize);
  Source.Read(@_Data[0], _DataSize);

  cmap := stbtt__find_table('cmap');
  loca := stbtt__find_table('loca');
  head := stbtt__find_table('head');
  glyf := stbtt__find_table('glyf');
  hhea := stbtt__find_table('hhea');
  hmtx := stbtt__find_table('hmtx');
  kern := stbtt__find_table('kern');

  If (cmap=0) or (loca=0) or (head=0) or (glyf=0) or (hhea=0) or (hmtx=0) then
  Begin
    Log(logError, 'TTF', 'Invalid font file: '+ Source.Name);
    Exit;
  End;

   t := stbtt__find_table('maxp');

   if t<>0 then
      _GlyphCount := ttUSHORT(t+4)
   else
      _GlyphCount := -1;

   // find a cmap encoding table we understand *now* to avoid searching
   // later. (todo: could make this installable)
   // the same regardless of glyph.
   numTables := Integer(ttUSHORT( cmap + 2));
   index_map := 0;

   for i:=0 to numTables-1 do
   begin
      encoding_record := cmap + 4 + 8 * i;
      // find an encoding we understand:
      case ttUSHORT(encoding_record) of
        STBTT_PLATFORM_ID_MICROSOFT:
        case ttUSHORT(encoding_record+2) of
          STBTT_MS_EID_UNICODE_BMP,
          STBTT_MS_EID_UNICODE_FULL:
            // MS/Unicode
            index_map := cmap + ttULONG( encoding_record+4);
        end;
      end;
   end;

   If index_map = 0 then
   Begin
    Log(logError, 'TTF', 'Could not find font index map: '+ Source.Name);
    Exit;
   End;

   indexToLocFormat := ttUSHORT(head + 50);

   _Ready := True;

   //t := stbtt_FindGlyphIndex(35821);
   //IntToString(T);
End;

Procedure TTFFont.Release;
Begin
  If Assigned(_Data) Then
  Begin
    FreeMem(_Data);
    _Data := Nil;
  End;
End;

function TTFFont.ttBYTE(Const offset:Cardinal):Byte;
begin
  If (Offset>=_DataSize) Then
    Result := 0
  Else
  Begin
    Result := _Data[offset];
  End;
end;

function TTFFont.ttUSHORT(Const offset:Cardinal): Word;
begin
  If (Offset>=_DataSize) Then
    Result := 0
  Else
  Begin
    Result := (_Data[offset] shl 8) + _Data[offset+1];
  End;
end;

function TTFFont.ttSHORT(Const offset:Cardinal): Smallint;
begin
  If (Offset>=_DataSize) Then
    Result := 0
  Else
    Result := (SmallInt(_Data[offset]) shl 8) + SmallInt(_Data[offset+1]);
end;

function TTFFont.ttULONG(Const offset:Cardinal): Cardinal;
begin
  If (Offset>=_DataSize) Then
    Result := 0
  Else
    Result := (PtrUInt(_Data[offset]) shl 24) + (PtrUInt(_Data[offset+1]) shl 16) + (PtrUInt(_Data[offset+2]) shl 8) + Cardinal(_Data[offset+3]);
end;

function TTFFont.ttLONG(Const offset:Cardinal): Integer;
begin
  If (Offset>=_DataSize) Then
    Result := 0
  Else
    Result := PtrUInt(_Data[offset] shl 24) + PtrUInt(_Data[offset+1] shl 16) + PtrUInt(_Data[offset+2] shl 8) + PtrUInt(_Data[offset+3]);
end;

function TTFFont.stbtt_tag(Const offset:Cardinal; Const TableTag:FileHeader):Boolean;
begin
  If (Offset>=_DataSize) Then
    Result := False
  Else
    Result:=(_data[offset]=Byte(TableTag[1])) and (_data[offset+1]=Byte(TableTag[2])) and (_data[offset+2]=Byte(TableTag[3])) and (_data[offset+3]=Byte(TableTag[4]));
end;


// OPTIMIZE: binary search
function TTFFont.stbtt__find_table(Const TableTag:FileHeader): Cardinal;
var
  num_tables: Integer;
  tabledir: Cardinal;
  i: Cardinal;
  loc: Cardinal;
Begin
  num_tables := ttUSHORT(4);

  If (num_tables<=0) Then
  Begin
    Result := 0;
    Exit;
  End;

  tabledir := 12;

  For i:=0 to num_tables-1 do
  Begin
    Loc := tabledir + 16*i;
    If stbtt_tag(loc, TableTag) then
    Begin
      Result := ttULONG(loc+8);
      Break;
    End;
  End;
End;

procedure TTFFont.stbtt_GetGlyphHMetrics(GlyphIndex: Integer; var advanceWidth, leftSideBearing: Integer);
var
   numOfLongHorMetrics: Cardinal;
begin
   numOfLongHorMetrics := Integer(ttUSHORT(hhea + 34));
   if GlyphIndex < numOfLongHorMetrics then
   begin
      advanceWidth := Integer(ttSHORT(hmtx + 4* GlyphIndex));
      leftSideBearing := Integer(ttSHORT(hmtx + 4*GlyphIndex + 2));
   end else begin
      advanceWidth := Integer(ttSHORT(hmtx + 4*(numOfLongHorMetrics-1)));
      leftSideBearing := Integer(ttSHORT( hmtx + 4*numOfLongHorMetrics + 2*(GlyphIndex - numOfLongHorMetrics)));
   end;
end;

function TTFFont.stbtt_GetGlyphKernAdvance(glyph1: Integer; glyph2: Integer): Integer;
Var
  needle, straw:Cardinal;
  l, r, m:Integer;
Begin
  Result := 0;
  If (Self.kern<=0) Then
    Exit;

   // we only look at the first table. it must be 'horizontal' and format 0.
  If (ttUSHORT( Self.kern +2) < 1) Then // number of tables
    Exit;
  If (ttUSHORT( Self.kern +8) <> 1) Then // horizontal flag, format
    Exit;

  l := 0;
  r := ttUSHORT(Self.kern + 10) - 1;
  needle := (glyph1 Shl 16) Or glyph2;
  While (l <= r) Do
  Begin
    m := (l + r) Shr 1;
    straw := ttULONG(Self.kern +18+(m*6)); // note: unaligned read
    If (needle < straw) Then
      r := m - 1
    Else
    If (needle > straw) Then
      l := m + 1
    Else
    Begin
      Result := ttSHORT(Self.kern + 22+(m*6));
      Exit;
    End;
  End;
End;

function TTFFont.stbtt_GetCodepointKernAdvance(ch1: Integer; ch2: Integer): Integer;
begin
  If (kern<=0) Then
    Result := 0
  Else
    Result := stbtt_GetGlyphKernAdvance(stbtt_FindGlyphIndex(ch1), stbtt_FindGlyphIndex(ch2));
end;

procedure TTFFont.stbtt_GetCodepointHMetrics(codepoint: Integer; var advanceWidth, leftSideBearing: Integer);
begin
   stbtt_GetGlyphHMetrics(stbtt_FindGlyphIndex(codepoint), advanceWidth, leftSideBearing);
end;

procedure TTFFont.stbtt_GetFontVMetrics(var ascent, descent, lineGap: Integer);
begin
   ascent  := Integer(ttSHORT( hhea + 4));
   descent := Integer(ttSHORT( hhea + 6));
   lineGap := Integer(ttSHORT( hhea + 8));
end;

Function TTFFont.ScaleForPixelHeight(height: Single): Single;
Var
   fHeight: Integer;
Begin
  If (_Ready) Then
  Begin
    fHeight := Integer(ttSHORT( hhea + 4) - ttSHORT( hhea + 6));
    Result := height / fheight;
  End Else
    Result := 0.0;
End;

Function TTFFont.GetCodepointBitmap(scaleX, scaleY: Single; codepoint: Integer; var xoff, yoff: Integer):TERRAImage;
Begin
  If (_Ready) Then
    Result := stbtt_GetGlyphBitmap(scaleX, scaleY, 0, 0, stbtt_FindGlyphIndex(codepoint), xoff, yoff)
  Else
    Result := Nil;
End;

function TTFFont.stbtt_GetGlyphBitmap(scale_x, scale_y, shift_x, shift_y: Single; glyph: Integer; var xoff, yoff: Integer):TERRAImage;
var
   ix0,iy0,ix1,iy1: Integer;
   w,h:Integer;
   vertices:TStBttVertexArray;
begin
  Result := nil;
  stbtt_GetGlyphShape(glyph, vertices);

  if scale_x = 0 then
    scale_x := scale_y;

  if scale_y = 0 then
  begin
    if scale_x = 0 then
      Exit;

    scale_y := scale_x;
  end;

  stbtt_GetGlyphBitmapBox(glyph, scale_x, scale_y, shift_x, shift_y, ix0, iy0, ix1, iy1);

  w := (ix1 - ix0);
  h := (iy1 - iy0);

  If (w<=0) Or (h<=0) Then
    Exit;

  // now we get the size
  Result := TERRAImage.Create(w, h);
  Result.ClearWithColor(ColorNull);
  xoff   := ix0;
  yoff   := iy0;
  stbtt_Rasterize(Result, 0.35, vertices, scale_x, scale_y, shift_x, shift_y, ix0, iy0, 1);
End;

Function TTFFont.stbtt_FindGlyphIndex(unicode_codepoint: Integer):Word;
var
   format: Smallint;
   bytes: Integer;
   first, count: Cardinal;
   segcount, searchRange, entrySelector, rangeShift: Word;
   item, offset, startValue, endValue: Word;
   startValue2, endValue2: Word;
   endCount: Cardinal;
   search: Cardinal;
   ngroups: Word;
   low,high: Integer;
   g: Word;
   n, mid: Integer;
   start_char: Cardinal;
   end_char: Cardinal;
   start_glyph: Cardinal;
Begin
   format := ttUSHORT( index_map);

   Case format Of

   0: // apple byte encoding
   Begin
      bytes := Integer(ttUSHORT( index_map + 2));
      if unicode_codepoint < bytes-6 then
      begin
         Result := Integer(_Data[index_map + 6 + unicode_codepoint]);
         Exit;
      end;
      Result := 0;
   End;

   6:
   Begin
      first := Cardinal(ttUSHORT( index_map + 6));
      count := Cardinal(ttUSHORT( index_map + 8));
      if (Cardinal(unicode_codepoint) >= first) and (unicode_codepoint < first+count) then
      begin
         Result := Integer(ttUSHORT( PtrUInt(index_map) + 10 + (unicode_codepoint - first)*2));
         Exit;
      end;
      Result := 0;
   End;

   2:
   Begin
      //STBTT_assert(0); // TODO: high-byte mapping for japanese/chinese/korean
      Result := 0;
      Exit;
   End;

   4: // standard mapping for windows fonts: binary search collection of ranges
   Begin
      segcount := ttUSHORT( index_map+6) shr 1;
      searchRange := ttUSHORT( index_map+8) shr 1;
      entrySelector := ttUSHORT( index_map+10);
      rangeShift := ttUSHORT( index_map+12) shr 1;

      // do a binary search of the segments
      endCount := index_map + 14;
      search := endCount;

      if unicode_codepoint > $FFFF then
      begin
         Result := 0;
         Exit;
      end;

      // they lie from endCount .. endCount + segCount
      // but searchRange is the nearest power of two, so...
      if unicode_codepoint >= Integer(ttUSHORT( search + rangeShift*2)) then
         Inc(search, rangeShift*2);

      // now decrement to bias correctly to find smallest
      Dec(search, 2);
      while entrySelector<>0 do
      begin
         //stbtt_uint16 start, end;
         searchRange := searchRange shr 1;
         startValue2 := ttUSHORT( search + 2 + segcount*2 + 2);
         endValue2 := ttUSHORT( search + 2);
         startValue2 := ttUSHORT( search + searchRange*2 + segcount*2 + 2);
         endValue2 := ttUSHORT( search + searchRange*2);

         if unicode_codepoint > endValue2 then
            Inc(search, searchRange*2);
         Dec(entrySelector);
      end;
      Inc(search, 2);

      item := Word((search - endCount) shr 1);

      //STBTT_assert(unicode_codepoint <= ttUSHORT(data + endCount + 2*item));
      startValue := ttUSHORT( index_map + 14 + segcount*2 + 2 + 2*item);
      endValue := ttUSHORT(index_map + 14 + 2 + 2*item);
      if unicode_codepoint < startValue then
      begin
        //IntToString(unicode_codepoint); //BOO
         Result:=0;
         Exit;
      end;

      offset := Integer(ttUSHORT( index_map + 14 + segcount*6 + 2 + 2*item));
      if offset = 0 then
      begin
        n := ttSHORT( index_map + 14 + segcount*4 + 2 + 2*item);
         Result := unicode_codepoint + n;
         Exit;
      end;

      Result := ttUSHORT( offset + (unicode_codepoint-startValue)*2 + index_map + 14 + segcount*6 + 2 + 2*item);
   End;

   12:
   Begin
      ngroups := ttUSHORT( index_map+6);
      g := 0;
      low := 0;
      high := Integer(ngroups);
      // Binary search the right group.
      while low <= high do
      begin
         mid := low + ((high-low) shr 1); // rounds down, so low <= mid < high
         start_char := ttULONG( index_map+16+mid*12);
         end_char := ttULONG( index_map+16+mid*12+4);
         if unicode_codepoint < start_char then
            high := mid-1
         else
         if unicode_codepoint > end_char then
            low := mid+1
         else
         Begin
            start_glyph := ttULONG( index_map+16+mid*12+8);
            Result := start_glyph + unicode_codepoint - start_char;
            Exit;
         End;
      end;
      Result := 0; // not found
   End;

  Else
    Begin
     // TODO
     Result := 0;
    End;

  End;
End;

Procedure TTFFont.stbtt_GetCodepointShape(unicode_codepoint: Integer; Out vertices: TStBttVertexArray);
begin
   stbtt_GetGlyphShape(stbtt_FindGlyphIndex(unicode_codepoint), vertices);
end;

procedure TTFFont.stbtt_setvertex(var v:TStBttVertex; typeByte: Byte; x, y, cx, cy: Smallint);
begin
   v.vertexType := typeByte;
   v.x := x;
   v.y := y;
   v.cx := cx;
   v.cy := cy;
end;

function TTFFont.stbtt__GetGlyfOffset(glyph_index: Integer): Integer;
var
   g1, g2: Integer;
begin
   if glyph_index >= _GlyphCount then
   begin
      Result := -1; // glyph index out of range
      Exit;
   end;

   if indexToLocFormat >= 2 then
   begin
      Result := -1; // unknown index->glyph map format
      Exit;
   end;

   if indexToLocFormat = 0 then
   begin
      g1 := glyf + ttUSHORT( loca + glyph_index * 2) * 2;
      g2 := glyf + ttUSHORT( loca + glyph_index * 2 + 2) * 2;
   end else begin
      g1 := glyf + ttULONG( loca + glyph_index * 4);
      g2 := glyf + ttULONG( loca + glyph_index * 4 + 4);
   end;

   if g1=g2 then
     Result := -1
   else
     Result := g1; // if length is 0, return -1
end;

function TTFFont.stbtt_GetGlyphBox(glyph_index: Integer; var x0, y0, x1, y1: Integer): Integer;
var
  g: Integer;
begin
   g := stbtt__GetGlyfOffset(glyph_index);
   if g < 0 then
   begin
     Result := 0;
     Exit;
   end;

   x0 := ttSHORT( g + 2);
   y0 := ttSHORT( g + 4);
   x1 := ttSHORT( g + 6);
   y1 := ttSHORT( g + 8);

   Result := 1;
end;

function TTFFont.stbtt_GetCodepointBox(codepoint: Integer; var x0, y0, x1, y1: Integer): Integer;
begin
   Result := stbtt_GetGlyphBox(stbtt_FindGlyphIndex(codepoint), x0, y0, x1, y1);
end;

Procedure TTFFont.stbtt_GetGlyphShape(glyph_index: Integer; Out Result:TStBttVertexArray);
var
   numberOfContours: Smallint;
   endPtsOfContours:Cardinal;
   g: Integer;

   flags,flagcount: Byte;
   ins,i,j,m,n,next_move,was_off,off: Integer;
   x,y,cx,cy,sx,sy,dx,dy: Smallint;
   more: Integer;
   comp2:Cardinal;
   points: PByteArray;

   gidx: Word;
   
   comp_verts: TStBttVertexArray;
   ms,ns: Single;
   mtx: array[0..5] of Single;
   xx, yy: Smallint;
   PointIndex: Integer;

   TempSize:Integer;
begin
   //stbtt_uint8 *data = info->data;
   Result.List := Nil;
   Result.Count := 0;
   g := stbtt__GetGlyfOffset(glyph_index);

   If (g < 0) Then
     Exit;

   numberOfContours := ttSHORT(g);

   if numberOfContours > 0 then
   begin
      flags :=0;
      j := 0;
      was_off :=0;
      endPtsOfContours := g + 10;
      ins := ttUSHORT(g + 10 + numberOfContours * 2);
      points := PByteArray(PtrUInt(_Data) + PtrUInt(g) + 10 + PtrUInt(numberOfContours) * 2 + 2 + PtrUInt(ins));

      n := 1 + ttUSHORT(endPtsOfContours+ numberOfContours*2-2);

      m := n + numberOfContours;  // a loose bound on how many vertices we might need

      Result.Count := M;
      SetLength(Result.List, Result.Count);

      next_move := 0;
      flagcount := 0;

      // in first pass, we load uninterpreted data into the allocated array
      // above, shifted to the end of the array so we won't overwrite it when
      // we create our final data starting from the front

      off := m - n; // starting offset for uninterpreted data, regardless of how m ends up being calculated

      // first load flags

      PointIndex := 0;
      for i:=0 to n-1 do
      begin
         if flagcount = 0 then
         begin
            flags := points[PointIndex];
            Inc(PointIndex);
            if (Flags and 8)<>0 then
            begin
               flagcount := points[PointIndex];
               Inc(PointIndex);
            end;
         end else
            Dec(flagcount);

         Result.List[off+i].vertexType := flags;
      end;

      // now load x coordinates
      x := 0;
      for i:=0 to n-1 do
      begin
         flags := Result.List[off+i].vertexType;
         if (flags and 2)<>0 then
         begin
            dx := points[PointIndex];
            Inc(PointIndex);

            if (flags and 16)<>0 then
              Inc(x, dx)
            else
              Dec(x, dx); // ???
         end else begin
            if (flags and 16)=0 then
            begin
               x := x + (Smallint(points[PointIndex]) shl 8) + Smallint(points[PointIndex+1]);
               Inc(PointIndex, 2);
            end;
         end;
         Result.List[off+i].x := x;
      end;

      // now load y coordinates
      y := 0;
      for i:=0 to n-1 do
      begin
         flags := Result.List[off+i].vertexType;
         if (flags and 4)<>0 then
         begin
            dy := points[PointIndex];
            Inc(PointIndex);

            if (flags and 32)<>0 then
              Inc(y, dy)
            else
              Dec(y, dy); // ???
         end else begin
            if (flags and 32)=0 then
            begin
               y := y + (Smallint(points[PointIndex]) shl 8) + Smallint(points[PointIndex+1]);
               Inc(PointIndex, 2);
            end;
         end;
         Result.List[off+i].y := y;
      end;

      // now convert them to our format
      Result.Count := 0;
      sx := 0;
      sy := 0;
      cx := 0;
      cy := 0;
      i := 0;
      while i<n do
      begin
         flags := Result.List[off+i].vertexType;
         x := Smallint(Result.List[off+i].x);
         y := Smallint(Result.List[off+i].y);
         if next_move = i then
         begin
            // when we get to the end, we have to close the shape explicitly
            if i<>0 then
            begin
               if was_off<>0 then
                  stbtt_setvertex(Result.List[Result.Count], STBTT_vcurve, sx, sy, cx, cy)
               else
                  stbtt_setvertex(Result.List[Result.Count], STBTT_vline, sx, sy, 0, 0);

               Inc(Result.Count);
            end;

            // now start the new one
            stbtt_setvertex(Result.List[Result.Count], STBTT_vmove,x,y,0,0);
            Inc(Result.Count);
            next_move := 1 + ttUSHORT(endPtsOfContours+j*2);
            Inc(j);
            was_off := 0;
            sx := x;
            sy := y;
         end else begin
            if (flags and 1) = 0 then  // if it's a curve
            begin
               if was_off<>0 then // two off-curve control points in a row means interpolate an on-curve midpoint
               begin
                  stbtt_setvertex(Result.List[Result.Count], STBTT_vcurve, (cx+x) shr 1, (cy+y) shr 1, cx, cy);
                  Inc(Result.Count);
               end;
               cx := x;
               cy := y;
               was_off := 1;
            end else begin
               if was_off<>0 then
                  stbtt_setvertex(Result.List[Result.Count], STBTT_vcurve, x,y, cx, cy)
               else
                  stbtt_setvertex(Result.List[Result.Count], STBTT_vline, x,y,0,0);
               Inc(Result.Count);
               was_off := 0;
            end;
         end;
         Inc(i);
      end;
      if i<>0 then
      begin
         if was_off<>0 then
            stbtt_setvertex(Result.List[Result.Count], STBTT_vcurve,sx,sy,cx,cy)
         else
            stbtt_setvertex(Result.List[Result.Count], STBTT_vline,sx,sy,0,0);
         Inc(Result.Count);
      end;
   end else
   if numberOfContours = -1 then
   begin
      // Compound shapes.
      more := 1;
      comp2 := g + 10;
      Result.Count := 0;
      Result.List := Nil;

      While more<>0 Do
      Begin
         comp_verts.Count := 0;
         comp_verts.List := nil;

         mtx[0] := 1;
         mtx[1] := 0;
         mtx[2] := 0;
         mtx[3] := 1;
         mtx[4] := 0;
         mtx[5] := 0;

         flags := ttSHORT(comp2);
         Inc(Comp2, 2);
         gidx := ttSHORT(comp2);
         Inc(Comp2, 2);

         if (flags and 2)<>0 then // XY values
         begin
            If (flags and 1)<>0 then  // shorts
            Begin
               mtx[4] := ttSHORT(comp2);
               Inc(Comp2, 2);
               mtx[5] := ttSHORT(comp2);
               Inc(Comp2, 2);
            End Else
            Begin
               mtx[4] := ttByte(comp2);
               Inc(Comp2);
               mtx[5] := ttByte(comp2);
               Inc(Comp2);
            end;
         End Else
         begin
            // TODO handle matching point
            //STBTT_assert(0);
         end;
         if (flags and (1 shl 3))<>0 then  // WE_HAVE_A_SCALE
         begin
            mtx[0] := ttSHORT(comp2)/16384.0;
            mtx[1] := 0;
            mtx[2] := 0;
            mtx[3] := ttSHORT(comp2)/16384.0;
            Inc(Comp2, 2);
         end else if (flags and (1 shl 6))<>0 then // WE_HAVE_AN_X_AND_YSCALE
         begin
            mtx[0] := ttSHORT(comp2)/16384.0;
            Inc(Comp2, 2);
            mtx[1] := 0;
            mtx[2] := 0;
            mtx[3] := ttSHORT(comp2)/16384.0;
            Inc(Comp2, 2);
         end else if (flags and (1 shl 7))<>0 then// WE_HAVE_A_TWO_BY_TWO
         begin
            mtx[0] := ttSHORT(comp2)/16384.0;
            Inc(Comp2, 2);
            mtx[1] := ttSHORT(comp2)/16384.0;
            Inc(Comp2, 2);
            mtx[2] := ttSHORT(comp2)/16384.0;
            Inc(Comp2, 2);
            mtx[3] := ttSHORT(comp2)/16384.0;
            Inc(Comp2, 2);
         end;

         // Find transformation scales.
         ms := Sqrt(mtx[0]*mtx[0] + mtx[1]*mtx[1]);
         ns := Sqrt(mtx[2]*mtx[2] + mtx[3]*mtx[3]);

         // Get indexed glyph.
         stbtt_GetGlyphShape(gidx, comp_verts);
         If comp_verts.Count > 0 Then
         Begin
            // Transform vertices.
            for i := 0 to comp_verts.Count-1 do
            begin
               //comp_verts[i];
               xx := comp_verts.List[i].x;
               yy := comp_verts.List[i].y;

               comp_verts.List[i].x := Smallint(Round(ms * (mtx[0]*xx + mtx[2]*yy + mtx[4])));
               comp_verts.List[i].y := Smallint(Round(ns * (mtx[1]*xx + mtx[3]*yy + mtx[5])));

               xx := comp_verts.List[i].cx;
               yy := comp_verts.List[i].cy;

               comp_verts.List[i].cx := Smallint(Round(ms * (mtx[0]*xx + mtx[2]*yy + mtx[4])));
               comp_verts.List[i].cy := Smallint(Round(ns * (mtx[1]*xx + mtx[3]*yy + mtx[5])));
            end;
            
            // Append vertices.
            TempSize := Result.Count + Comp_Verts.Count;
            SetLength(Result.List, TempSize);

            For I:=0 To Pred(Comp_Verts.Count) Do
            Begin
              Result.List[Result.Count + I] := Comp_Verts.List[I];
            End;

            Result.Count := TempSize;
         end;

         // More components ?
         more := flags and  (1 shl 5);
      End;
   End Else
   If numberOfContours < 0 Then
   Begin
      // TODO other compound variations?
      //STBTT_assert(0);
   End Else
   Begin
      // numberOfCounters == 0, do nothing
   End;
End;

// antialiasing software rasterizer
procedure TTFFont.stbtt_GetGlyphBitmapBox(glyph: Integer; scale_x, scale_y, shift_x, shift_y: Single; var ix0, iy0, ix1, iy1: Integer);
var
   x0,y0,x1,y1: Integer;
begin
   if stbtt_GetGlyphBox(glyph, x0,y0,x1,y1)=0 then
   begin
      x0 := 0;
      y0 := 0;
      x1 := 0;
      y1 := 0; // e.g. space character
   end;

   // now move to integral bboxes (treating pixels as little squares, what pixels get touched)?
   ix0 := Floor(x0 * scale_x + shift_x);
   iy0 := -Ceil(y1 * scale_y + shift_y);
   ix1 :=  Ceil(x1 * scale_x + shift_x);
   iy1 := -Floor(y0 * scale_y + shift_y);
end;

procedure TTFFont.stbtt_GetCodepointBitmapBox(codepoint: Integer; scale_x, scale_y: Single; var ix0, iy0, ix1, iy1: Integer);
begin
   stbtt_GetGlyphBitmapBox(stbtt_FindGlyphIndex(codepoint), scale_x, scale_y, 0, 0, ix0, iy0, ix1, iy1);
end;

procedure TTFFont.stbtt__add_point(Var points:TStBttPointArray; n: Integer; x, y: Single);
Begin
  If points.Count<=0 Then
    Exit; // during first pass, it's unallocated

   points.List[n].x := x;
   points.List[n].y := y;
End;

// tesselate until threshhold p is happy... TODO warped to compensate for non-linear stretching
function TTFFont.stbtt__tesselate_curve(Var points:TStBttPointArray; var num_points: Integer; x0, y0, x1, y1, x2, y2, objspace_flatness_squared: Single; n: Integer): Integer;
var
  mx, my, dx, dy: Single;
begin
   // midpoint
   mx := (x0 + 2*x1 + x2)/4;
   my := (y0 + 2*y1 + y2)/4;
   // versus directly drawn line
   dx := (x0+x2)/2 - mx;
   dy := (y0+y2)/2 - my;
   if n > 16 then // 65536 segments on one curve better be enough!
   begin
      Result := 1;
      Exit;
   end;

   if dx*dx+dy*dy > objspace_flatness_squared then  // half-pixel error allowed... need to be smaller if AA
   begin
      stbtt__tesselate_curve(points, num_points, x0,y0, (x0+x1)/2.0,(y0+y1)/2.0, mx,my, objspace_flatness_squared, n+1);
      stbtt__tesselate_curve(points, num_points, mx,my, (x1+x2)/2.0,(y1+y2)/2.0, x2,y2, objspace_flatness_squared, n+1);
   end else begin
      stbtt__add_point(points, num_points, x2, y2);
      Inc(num_points);
   end;
   Result := 1;
end;


// returns number of contours
Procedure TTFFont.stbtt_FlattenCurves(Var Vertices:TStBttVertexArray; ObjSpaceFlatness: Single; Out Contours:TTFContourArray; Out Windings:TStBttPointArray);
var
   NumPoints: Integer;
   objspace_flatness_squared: Single;
   i, n, start, pass: Integer;
   x, y: Single;
begin
   Windings.Count := 0;
   Windings.List:= nil;
   NumPoints := 0;

   objspace_flatness_squared := Sqr(ObjSpaceFlatness);
   n := 0;
   start := 0;

   // count how many "moves" there are to get the contour count
  for i:=0 to Vertices.Count-1 do
  If vertices.List[i].vertexType = STBTT_vmove then
    Inc(n);

  Contours.List := Nil;
  Contours.Count := n;
  If n = 0 Then
    Exit;

  SetLength(Contours.List, N);

   // make two passes through the points so we don't need to realloc
   for pass:=0 to 1 do
   begin
      x := 0;
      y := 0;
      If (pass = 1) Then
      Begin
        Windings.Count := NumPoints * 2 ;
        SetLength(Windings.List, Windings.Count);
      End;

      NumPoints := 0;
      n := -1;

      for i:=0 to Vertices.Count-1 do
      begin
         case vertices.List[i].vertexType of
            STBTT_vmove:
            begin
               // start the next contour
               if n >= 0 then
                  Contours.List[n] := NumPoints - start;
               Inc(n);
               start := NumPoints;

               x := vertices.List[i].x;
               y := vertices.List[i].y;
               stbtt__add_point(Windings, NumPoints, x,y);
               Inc(NumPoints);
            end;

            STBTT_vline:
            begin
               x := vertices.List[i].x;
               y := vertices.List[i].y;
               stbtt__add_point(Windings, NumPoints, x, y);
               Inc(NumPoints);
            end;

            STBTT_vcurve:
            begin
               stbtt__tesselate_curve(Windings, NumPoints, x,y,
                                        vertices.List[i].cx, vertices.List[i].cy,
                                        vertices.List[i].x,  vertices.List[i].y,
                                        objspace_flatness_squared, 0);
               x := vertices.List[i].x;
               y := vertices.List[i].y;
            end;
         end;
      end;
      Contours.List[n] := NumPoints - start;
   end;
End;

Procedure TTFFont.stbtt_Rasterize(resultBitmap:TERRAImage; FlatnessInPixels: Single; Var Vertices:TStBttVertexArray; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff, Invert: Integer);
Var
  Scale: Single;
  WindingLengths:TTFContourArray;
  Windings: TStBttPointArray;
Begin
  Scale := Min(ScaleX, ScaleY);

  stbtt_FlattenCurves(Vertices, FlatnessInPixels / Scale, WindingLengths, Windings);
  If windings.Count>0 then
  Begin
    stbtt__rasterize(resultBitmap, Windings, WindingLengths, ScaleX, ScaleY, ShiftX, ShiftY, XOff, YOff, Invert);
      //FreeMem(Windings);
  End;
End;

Procedure TTFFont.stbtt__rasterize(resultBitmap:TERRAImage; Var Pts: TStBttPointArray; Var Windings:TTFContourArray; ScaleX, ScaleY, ShiftX,ShiftY: Single; XOff, YOff, Invert: Integer);
Var
  YScaleInv: Single;
  i,j,k,m, a,b, VSubSample: Integer;
  e: EdgeList;
  en: TStBttEdge;

  ptOfs:Cardinal;

Function p(Index:Integer): TStBttPoint;
Begin
  Result := Pts.List[ptOfs + index];
End;

Begin
   If Invert<>0 Then
    YScaleInv := -ScaleY
   Else
    YScaleInv := ScaleY;

   If resultBitmap.Height<8 then
    VSubSample := 15
   Else
    VSubSample := 5;
   // vsubsample should divide 255 evenly; otherwise we won't reach full opacity

   e := EdgeList.Create();
   m := 0;

   for i:=0 to Pred(Windings.Count) do
   begin
      ptOfs := M;

      Inc(m, Windings.List[i]);
      j := Windings.List[i]-1;
      k:=0;
      while k < Windings.List[i] do
      begin

         a := k;
         b := j;

         // skip the edge if horizontal
         if p(j).y <> p(k).y then
         begin
            // add edge from j to k to the list
            en.invert := 0;

            if ((invert<>0) and (p(j).y > p(k).y)) or
              ((invert=0) and (p(j).y < p(k).y)) then
            begin
              en.invert := 1;
              a := j;
              b := k;
            end;

            en.x0 := p(a).x * ScaleX + ShiftX;
            en.y0 := p(a).y * YScaleInv * VSubSample + ShiftY;
            en.x1 := p(b).x * ScaleX + ShiftX;
            en.y1 := p(b).y * YScaleInv * VSubSample + ShiftY;

            e.Add(en);
         end;

         j:=k;
         Inc(k);
      end;
   end;

   SetLength(Pts.List, 0);
   Pts.Count := 0;

   SetLength(Windings.List, 0);
   Windings.Count := 0;

   // now sort the edges by their highest point (should snap to integer, and then by x)
   e.Sort();

   FillChar(en, SizeOf(TStBttEdge), 0);
   en.y0 := 10000000;
   e.Add(en);

   // now, traverse the scanlines and find the intersections on each scanline, use xor winding rule
   stbtt__rasterize_sorted_edges(resultBitmap, e, VSubSample, XOff, YOff);

   ReleaseObject(e);
end;

Function TTFFont.new_active(Const e: TStBttEdge; off_x: Integer; start_point: Single):TStBttActiveEdge;
Var
  z:TStBttActiveEdge;
  dxdy: Single;
Begin
  Z := TStBttActiveEdge.Create(); // TODO: make a pool of these!!!

  dxdy := (e.x1 - e.x0) / (e.y1 - e.y0);
  //STBTT_assert(e->y0 <= start_point);

  // round dx down to avoid going too far
  If dxdy < 0 then
    z.dx := -Floor(FIX * -dxdy)
  Else
    z.dx := Floor(FIX * dxdy);

  z.x := Floor(FIX * (e.x0 + dxdy * (start_point - e.y0)));
  Dec(z.x, off_x * FIX);
  z.ey := e.y1;
  z.next := nil;

  If e.invert<>0 Then
    z.valid := 1
  Else
    z.valid := -1;

  Result := z;
end;

// note: this routine clips fills that extend off the edges... ideally this
// wouldn't happen, but it could happen if the truetype glyph bounding boxes
// are wrong, or if the user supplies a too-small bitmap
Procedure TTFFont.stbtt__fill_active_edges(scanline: PByteArray; len: Integer; e: TStBttActiveEdge; max_weight: Integer);
Var
   x0,x1,w: Integer;
   i,j: Integer;
begin
  // non-zero winding fill
  x0 := 0;
  w := 0;

  While Assigned(e) Do
  Begin
    If w = 0 Then
    Begin
      // if we're currently at zero, we need to record the edge start point
      x0 := e.x;
      Inc(w, e.valid);
    End Else
    Begin
      x1 := e.x;
      Inc(w, e.valid);
      // if we went to zero, we need to draw
      If w = 0 then
      Begin
        i := x0 shr FIXSHIFT;
        j := x1 shr FIXSHIFT;

        If (i < len) and (j >= 0) then
        Begin
          If i = j Then
          Begin
            // x0,x1 are the same pixel, so compute combined coverage
            scanline[i] := scanline[i] + Byte(((x1 - x0) * max_weight) shr FIXSHIFT);
          End Else
          Begin
            If i >= 0 Then // add antialiasing for x0
              scanline[i] := scanline[i] + Byte(((FIX - (x0 and FIXMASK)) * max_weight) shr FIXSHIFT)
            Else
              i := -1; // clip

            If (j < len) Then// add antialiasing for x1
              scanline[j] := scanline[j] + Byte(((x1 and FIXMASK) * max_weight) shr FIXSHIFT)
            Else
              j := len; // clip

            Inc(i);
            While i<j Do // fill pixels between x0 and x1
            Begin
              scanline[i] := scanline[i] + Byte(max_weight);
              Inc(i);
            End;
          End;
        End;
      End;
    End;

    e := e.next;
  End;
End;

Procedure TTFFont.stbtt__rasterize_sorted_edges(resultBitmap:TERRAImage; e: EdgeList; vsubsample, off_x, off_y: Integer);
Var
  active: TStBttActiveEdge;
  y,j,s,iii: Integer;
  max_weight: Integer;
  scan_y: Single;
  Temp, Prev: TStBttActiveEdge;
  p, z: TStBttActiveEdge;
  changed:Boolean;
  t, q: TStBttActiveEdge;
  eIndex: Integer;

  n, cnt, ofs: Integer;

  Color:ColorRGBA;
Begin
  Color := ColorWhite;

  eIndex := 0;
  n := e.Count-1;

  Active := nil;
  j := 0;
  max_weight := 255 div vsubsample;  // weight per vertical scanline

  y := off_y * vsubsample;

  E.Fix(N, (off_y + resultBitmap.Height) * vsubsample + 1);

  If (Length(_Scanline)<=resultBitmap.Width) Then
    SetLength(_Scanline, Succ(resultBitmap.Width));

  While (j < resultBitmap.Height) do
  Begin
    For iii:=0 to resultBitmap.Width do
      _Scanline[iii] := 0;

    For s:=0 to vsubsample-1 do
    Begin
      // find center of pixel for this scanline
      scan_y := y + 0.5;

      // update all active edges;
      // remove all active edges that terminate before the center of this scanline
      Temp := Active;
      Prev := Nil;
      While Assigned(Temp) do
      Begin
        If (Temp.ey <= scan_y) then
        Begin
          // delete from list
          If Assigned(Prev) Then
            Prev.Next := Temp.next
          Else
            Active := Temp.next;

          Z := Temp;
          ReleaseObject(Z);

          Temp := Temp.next;
        End Else
        Begin
          Inc(Temp.x, Temp.dx); // advance to position for current scanline

          Prev := Temp;
          Temp := Temp.next; // advance through list
        End;
      End;

      // resort the list if needed
      Repeat
        Changed := False;
        Temp := Active;
        While (Assigned(Temp)) And (Assigned(Temp.Next)) Do
        Begin
          If Temp.x > Temp.Next.x then
          Begin
            t := Temp;
            q := t.next;

            t.next := q.next;
            q.next := t;
            Temp := q;

            Changed := True;
          End;

          Temp := Temp.next;
        End;

      Until (Not Changed);

      // insert all edges that start before the center of this scanline -- omit ones that also end on this scanline
      While (e.Get(eIndex).y0 <= scan_y) Do
      Begin
        If (e.Get(eIndex).y1 > scan_y) Then
        Begin
          z := new_active(e.Get(eIndex), off_x, scan_y);
          // find insertion point
          If active = nil then
            active := z
          Else
          If (z.x < active.x) then                  // insert at front
          Begin
            z.next := active;
            active := z;
          End Else
          Begin
            // find thing to insert AFTER
            p := active;
            While (Assigned(p.next)) and (p.next.x < z.x) Do
              p := p.next;

            // at this point, p->next->x is NOT < z->x
            z.next := p.next;
            p.next := z;
          End;
        End;

        Inc(eIndex);
      End;

      // now process all active edges in XOR fashion
      If Assigned(active) Then
        stbtt__fill_active_edges(@_Scanline[0], resultBitmap.Width, active, max_weight);

      Inc(y);
    End;

    For iii:=0 to Pred(resultBitmap.Width) Do
    If (_Scanline[iii]>0) Then // OPTIMIZATION?
    Begin
      Color.A := _Scanline[iii];
      ResultBitmap.SetPixel(iii, j, Color);
    End;

    Inc(j);
  End;

  While Assigned(Active) Do
  Begin
    z := active;
    Active := Active.Next;
    ReleaseObject(Z);
   End;
End;

Function TTFFont.GetKerning(Current, Next: Cardinal): Integer;
Begin
  Result := Trunc(stbtt_GetCodepointKernAdvance(Current, Next) * _Scale);
End;

Function TTFFont.HasGlyph(ID: Cardinal): Boolean;
Var
  P:Integer;
begin
  P := stbtt_FindGlyphIndex(ID);
  Result := (P>0);
End;

Function TTFFont.InitGlyph(Font:TERRAFont; ID: Cardinal; Size:Integer): FontGlyph;
Var
  W,H,XOfs,YOfs, XAdv,lsb:Integer;
  OpID:Cardinal;
  I,J:Integer;
  Img:TERRAImage;
Begin
  Result := Nil;

  If (Not Self.HasGlyph(ID)) Then
    Exit;

  _Scale := Self.ScaleForPixelHeight(Size);

  {$IFDEF DEBUG_FONTS}Log(logDebug,'Font','Rendering ttf '+IntToString(ID));{$ENDIF}

  If (ID = 32) Then
  Begin
    OpID := Ord('E');
    Img := GetCodepointBitmap(_Scale, _Scale, OpID, xofs, yofs);
    ReleaseObject(Img);
    Img := TERRAImage.Create(4,4);
    stbtt_GetCodepointHMetrics(OpID, XAdv, lsb);
    Result := Font.AddGlyph(ID, Img, XOfs, YOfs, Trunc(XAdv*_Scale));
    ReleaseObject(Img);
    Exit;
  End;

  Img := GetCodepointBitmap(_Scale, _Scale, ID, xofs, yofs);
  If Not Assigned(Img) Then
    Exit;

  //Img.Save('out\g'+IntToString(ID)+'.png');

  W := Img.Width;
  H := Img.Height;

  stbtt_GetCodepointHMetrics(ID, XAdv, lsb);

  Result := Font.AddGlyph(ID, Img, XOfs, YOfs, Trunc(XAdv*_Scale));

  //Img.Save('glyphs\g'+IntToString(ID)+'.png');

  ReleaseObject(Img);
End;

{ TTFFormat }
Function TTFFormat.Identify(Source: TERRAStream): Boolean;
Var
   Major, Minor:Word;
Begin
  Source.ReadWord(Major);
  Source.ReadWord(Minor);
  ByteSwap16(Major);
  ByteSwap16(Minor);
  Result := (Major = 1) And (Minor = 0);
End;

Function TTFFormat.Load(Target: TERRAObject; Source: TERRAStream): Boolean;
Var
  Font:TERRAFont;
  Factory:TTFFont;
  Img:TERRAImage;
  Size:Integer;
  Scale:Single;
  XAdv,lsb:Integer;
Begin
  Font := TERRAFont(Target);
  Factory := TTFFont.Create();

  Factory.LoadFromStream(Source);
  Result := Factory.Ready;
  If Result Then
  Begin
    Font.AddGlyphFactory(Factory);
  End;
End;

Initialization
  Engine.Formats.Add(TTFFormat.Create(TERRAFont, 'ttf'));
End.


