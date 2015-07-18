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
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Font, TERRA_Color, TERRA_Image;

{$RANGECHECKS OFF}

Const
  MaxListSize = Maxint div 16;

Type
  EdgeListSortCompare = function (Item1, Item2: Pointer): Integer;

  EdgeList = Class(TERRAObject)
  Private
    FList: PPointerArray;
    FCount: Integer;
    FCapacity: Integer;

  Protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);

    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);

  Public
    Procedure Release; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);

    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);

    procedure Sort(Compare: EdgeListSortCompare);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerArray read FList;
  end;

  TStBttVertex = record
     x,y,cx,cy: Smallint;
     vertexType,padding: Byte;
  end;
  PStBttVertex = ^TStBttVertex;

  TStBttVertexArray = array[0..32767] of TStBttVertex;
  PStBttVertexArray = ^TStBttVertexArray;

  TStBttPoint = record
     x,y: Single;
  end;
  PStBttPoint = ^TStBttPoint;

  TStBttPointArray = array[0..32767] of TStBttPoint;
  PStBttPointArray = ^TStBttPointArray;

  TIntegerArray = array[0..32767] of Integer;
  PIntegerArray = ^TIntegerArray;

  type TStBttEdge = record
    x0,y0, x1,y1: Single;
    invert: Integer;
  end;
  PStBttEdge = ^TStBttEdge;

  type TStBttActiveEdge = record
    x,dx: Integer;
    ey: Single;
    next: Pointer;
    valid: Integer;
  end;
  PStBttActiveEdge = ^TStBttActiveEdge;

Type
  TTFFont = Class(FontGlyphFactory)
    Private
      _Scale:Single;

      _Ready:Boolean;
      Data: PByteArray;              // pointer to .ttf file
      FontStart: Integer;            // offset of start of font
      numGlyphs: Integer;            // number of glyphs, needed for range checking

      loca,head,glyf,hhea,hmtx, kern: Cardinal; // table locations as offset from start of .ttf
      index_map: Integer;                // a cmap mapping for our chosen character encoding
      indexToLocFormat: Integer;         // format needed to map from glyph index to glyph

      BufferSize: Int64;
      TtfBuffer: PByteArray;

      Function ttUSHORT(p: PByteArray; offset: Cardinal): Word;
      Function ttSHORT(p: PByteArray; offset: Cardinal): Smallint;
      Function ttULONG(p: PByteArray; offset: Cardinal): Cardinal;
      Function ttLONG(p: PByteArray; offset: Cardinal): Integer;

      Function stbtt_tag(data: PByteArray; offset: Cardinal; TableTag: PAnsiChar): Boolean;
      Function stbtt__find_table(data: PByteArray; fontstart: Cardinal; TableTag: PAnsiChar): Cardinal;

      Procedure stbtt_GetGlyphHMetrics(GlyphIndex: Integer; var advanceWidth, leftSideBearing: Integer);
      Function stbtt_GetGlyphKernAdvance(glyph1: Integer; glyph2: Integer): Integer;
      Function stbtt_GetGlyphBitmap(scale_x, scale_y, shift_x, shift_y: Single; glyph: Integer; var xoff, yoff: Integer): Image;
      //Procedure stbtt_MakeGlyphBitmap(output: PByteArray; out_w, out_h, out_stride: Integer; scale_x, scale_y, shift_x, shift_y: Single; glyph: Integer);

      Function stbtt_GetGlyphShape(glyph_index: Integer; var PVertices: PStBttVertexArray): Integer;
      Function stbtt__GetGlyfOffset(glyph_index: Integer): Integer;
      Function stbtt_GetGlyphBox(glyph_index: Integer; var x0, y0, x1, y1: Integer): Integer;
      Procedure stbtt_GetGlyphBitmapBox(glyph: Integer; scale_x, scale_y, shift_x, shift_y: Single; var ix0, iy0, ix1, iy1: Integer);

      Function stbtt_GetCodepointKernAdvance(ch1: Integer; ch2: Integer): Integer;
      Procedure stbtt_GetCodepointHMetrics(codepoint: Integer; var advanceWidth, leftSideBearing: Integer);
      Function stbtt_GetCodepointShape(unicode_codepoint: Integer; var vertices: PStBttVertexArray): Integer;
      Function stbtt_GetCodepointBox(codepoint: Integer; var x0, y0, x1, y1: Integer): Integer;
      Procedure stbtt_GetCodepointBitmapBox(codepoint: Integer; scale_x, scale_y: Single; var ix0, iy0, ix1, iy1: Integer);

      Procedure stbtt_Rasterize(resultBitmap:Image; FlatnessInPixels: Single; Vertices: PStBttVertexArray; NumVerts: Integer; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff, Invert: Integer);
      Procedure stbtt_GetFontVMetrics(var ascent, descent, lineGap: Integer);
      Procedure stbtt_setvertex(var v:TStBttVertex; typeByte: Byte; x, y, cx, cy: Smallint);
      Procedure stbtt__rasterize(resultBitmap:Image; Pts: PStBttPointArray; WCount: PIntegerArray; Windings: Integer; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff, Invert: Integer);
      Function stbtt_FlattenCurves(Vertices: PStBttVertexArray; num_verts:Integer; ObjSpaceFlatness: Single; var contour_lengths: PIntegerArray; var num_contours: Integer): PStBttPointArray;
      Procedure stbtt__add_point(points: PStBttPointArray; n: Integer; x, y: Single);
      Function stbtt__tesselate_curve(points:PStBttPointArray; var num_points: Integer; x0, y0, x1, y1, x2, y2, objspace_flatness_squared: Single; n: Integer): Integer;
      Procedure stbtt__rasterize_sorted_edges(resultBitmap:Image; e: EdgeList; n, vsubsample, off_x, off_y: Integer);
      Procedure stbtt__fill_active_edges(scanline: PByteArray; len: Integer; e: PStBttActiveEdge; max_weight: Integer);
      Function new_active(e: PStBttEdge; off_x: Integer; start_point: Single): PStBttActiveEdge;

    Public
      Constructor Create(Source:Stream); Overload;
      Constructor Create(Const FileName:TERRAString); Overload;

      Procedure Release; Override;

      Function stbtt_FindGlyphIndex(unicode_codepoint: Integer):Word;

      Function GetCodepointBitmap(scaleX, scaleY: Single; codepoint: Integer; var xoff, yoff: Integer):Image;
      Function ScaleForPixelHeight(height: Single): Single;

      Function HasGlyph(ID:Cardinal):Boolean;

      Function InitGlyph(Font:TERRAFont; ID:Cardinal; Size:Integer):FontGlyph; Override;
      Function GetKerning(Current, Next:Cardinal):Integer; Override;

      Property Ready:Boolean Read _Ready;
    End;

Implementation

Uses TERRA_Log, TERRA_FileManager, Math
{$IFDEF DISTANCEFIELDFONTS},TERRA_DistanceField{$ENDIF};

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



{ EdgeList }
Procedure EdgeList.Release;
begin
  Clear;
end;

function EdgeList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure EdgeList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure EdgeList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Exit;

  Temp := Items[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
end;

function EdgeList.First: Pointer;
begin
  Result := Get(0);
end;

function EdgeList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= FCount) then
  Begin
  	Result := Nil;
  	Exit;
  End;
  
  Result := FList^[Index];
end;

procedure EdgeList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function EdgeList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure EdgeList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Exit;
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
end;

function EdgeList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure EdgeList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Exit;
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure EdgeList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Exit;
  if Item <> FList^[Index] then
  begin
    Temp := FList^[Index];
    FList^[Index] := Item;
  end;
end;

procedure EdgeList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Exit;
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure EdgeList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Exit;
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure QuickSort(SorEdgeList: PPointerArray; L, R: Integer; SCompare: EdgeListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SorEdgeList^[(L + R) shr 1];
    repeat
      while SCompare(SorEdgeList^[I], P) < 0 do
        Inc(I);
      while SCompare(SorEdgeList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SorEdgeList^[I];
        SorEdgeList^[I] := SorEdgeList^[J];
        SorEdgeList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SorEdgeList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure EdgeList.Sort(Compare: EdgeListSortCompare);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1, Compare);
end;


{ TTF }
Constructor TTFFont.Create(Const FileName:TERRAString);
Var
  Source:Stream;
Begin
  Source := FileManager.Instance.OpenStream(FileName);
  If Assigned(Source) Then
  Begin
    Create(Source);
    ReleaseObject(Source);
  End;
End;


Constructor TTFFont.Create(Source:Stream);
var
  cmap, t: Cardinal;
  encoding_record: Cardinal;
  i,numTables: Cardinal;
begin
  TtfBuffer := nil;
  BufferSize := 0;
  _Ready := False;

  If (Source = Nil) then
  Begin
    Log(logError, 'TTF', 'Null stream!');
    Exit;
  End;

  // read file
  BufferSize := Source.Size;
  GetMem(TtfBuffer, BufferSize);
  Source.Read(@TtfBuffer[0], BufferSize);
  Data := TtfBuffer;
  FontStart := 0;

  cmap := stbtt__find_table(Data, FontStart, 'cmap');
  loca := stbtt__find_table(Data, FontStart, 'loca');
  head := stbtt__find_table(Data, FontStart, 'head');
  glyf := stbtt__find_table(Data, FontStart, 'glyf');
  hhea := stbtt__find_table(Data, FontStart, 'hhea');
  hmtx := stbtt__find_table(Data, FontStart, 'hmtx');
  kern := stbtt__find_table(Data, FontStart, 'kern');

  If (cmap=0) or (loca=0) or (head=0) or (glyf=0) or (hhea=0) or (hmtx=0) then
  Begin
    Log(logError, 'TTF', 'Invalid font file: '+ Source.Name);
    Exit;
  End;

   t := stbtt__find_table(Data, FontStart, 'maxp');

   if t<>0 then
      numGlyphs := ttUSHORT(data, t+4)
   else
      numGlyphs := -1;

   // find a cmap encoding table we understand *now* to avoid searching
   // later. (todo: could make this installable)
   // the same regardless of glyph.
   numTables := Integer(ttUSHORT(data, cmap + 2));
   index_map := 0;

   for i:=0 to numTables-1 do
   begin
      encoding_record := cmap + 4 + 8 * i;
      // find an encoding we understand:
      case ttUSHORT(data, encoding_record) of
        STBTT_PLATFORM_ID_MICROSOFT:
        case ttUSHORT(data, encoding_record+2) of
          STBTT_MS_EID_UNICODE_BMP,
          STBTT_MS_EID_UNICODE_FULL:
            // MS/Unicode
            index_map := cmap + ttULONG(data, encoding_record+4);
        end;
      end;
   end;

   If index_map = 0 then
   Begin
    Log(logError, 'TTF', 'Could not find font index map: '+ Source.Name);
    Exit;
   End;

   indexToLocFormat := ttUSHORT(data, head + 50);

   _Ready := True;

   t := stbtt_FindGlyphIndex(35821);
   //IntToString(T);
End;

Procedure TTFFont.Release;
Begin
  If Assigned(TtfBuffer) Then
  Begin
    FreeMem(TtfBuffer);
    TtfBuffer := Nil;
  End;
End;

function TTFFont.ttUSHORT(p: PByteArray; offset: Cardinal): Word;
begin
  If (Succ(Offset)>=BufferSize) Then
    Result := 0
  Else
  Begin
    Result := (P[offset] shl 8) + P[offset+1];
  End;
end;

function TTFFont.ttSHORT(p: PByteArray; offset: Cardinal): Smallint;
begin
  If (Succ(Offset)>=BufferSize) Then
    Result := 0
  Else
    Result := (SmallInt(p[offset]) shl 8) + SmallInt(p[offset+1]);
end;

function TTFFont.ttULONG(p: PByteArray; offset: Cardinal): Cardinal;
begin
  If (Succ(Offset)>=BufferSize) Then
    Result := 0
  Else
    Result := (PtrUInt(p[offset]) shl 24) + (PtrUInt(p[offset+1]) shl 16) + (PtrUInt(p[offset+2]) shl 8) + Cardinal(p[offset+3]);
end;

function TTFFont.ttLONG(p: PByteArray; offset: Cardinal): Integer;
begin
  If (Succ(Offset)>=BufferSize) Then
    Result := 0
  Else
    Result := PtrUInt(p[offset] shl 24) + PtrUInt(p[offset+1] shl 16) + PtrUInt(p[offset+2] shl 8) + PtrUInt(p[offset+3]);
end;

function TTFFont.stbtt_tag(data: PByteArray; offset: Cardinal; TableTag: PAnsiChar): Boolean;
begin
  If (Succ(Offset)>=BufferSize) Then
    Result := False
  Else
    Result:=(data[offset]=Byte(TableTag[0])) and (data[offset+1]=Byte(TableTag[1])) and (data[offset+2]=Byte(TableTag[2])) and (data[offset+3]=Byte(TableTag[3]));
end;

function EdgeCompare(Item1, Item2: Pointer): Integer;
var
   pa, pb: PStBttEdge;
begin
   pa := PStBttEdge(Item1);
   pb := PStBttEdge(Item2);

   if pa^.y0 < pb^.y0 then
      Result := -1
   else
   if pa^.y0 > pb^.y0 then
      Result := 1
   else
      Result := 0;
end;

// @OPTIMIZE: binary search
function TTFFont.stbtt__find_table(data: PByteArray; fontstart: Cardinal; TableTag: PAnsiChar): Cardinal;
var
  num_tables: Integer;
  tabledir: Cardinal;
  i: Cardinal;
  loc: Cardinal;
begin
   num_tables := ttUSHORT(data, fontstart+4);
   tabledir := fontstart + 12;

   Result := 0;
   If (num_tables>0) Then
   for i:=0 to num_tables-1 do
   begin
      loc := tabledir + 16*i;
      if stbtt_tag(data, loc+0, TableTag) then
      begin
         Result := ttULONG(data, loc+8);
         Break;
      end;
   end;
end;

procedure TTFFont.stbtt_GetGlyphHMetrics(GlyphIndex: Integer; var advanceWidth, leftSideBearing: Integer);
var
   numOfLongHorMetrics: Cardinal;
begin
   numOfLongHorMetrics := Integer(ttUSHORT(data, hhea + 34));
   if GlyphIndex < numOfLongHorMetrics then
   begin
      advanceWidth := Integer(ttSHORT(data, hmtx + 4*PtrUInt(GlyphIndex)));
      leftSideBearing := Integer(ttSHORT(data, hmtx + 4*PtrUInt(GlyphIndex) + 2));
   end else begin
      advanceWidth := Integer(ttSHORT(data, hmtx + 4*(numOfLongHorMetrics-1)));
      leftSideBearing := Integer(ttSHORT(data, hmtx + 4*numOfLongHorMetrics + 2*(PtrUInt(GlyphIndex) - numOfLongHorMetrics)));
   end;
end;

function TTFFont.stbtt_GetGlyphKernAdvance(glyph1: Integer; glyph2: Integer): Integer;
Var
  data:PByteArray;
  needle, straw:Cardinal;
  l, r, m:Integer;
Begin
  Result := 0;
  If (Self.kern<=0) Then
    Exit;
  data := PByteArray(PtrUInt(Self.data) + Self.kern);

   // we only look at the first table. it must be 'horizontal' and format 0.
  If (ttUSHORT(data, 2) < 1) Then // number of tables
    Exit;
  If (ttUSHORT(data, 8) <> 1) Then // horizontal flag, format
    Exit;

  l := 0;
  r := ttUSHORT(data, 10) - 1;
  needle := (glyph1 Shl 16) Or glyph2;
  While (l <= r) Do
  Begin
    m := (l + r) Shr 1;
    straw := ttULONG(data, 18+(m*6)); // note: unaligned read
    If (needle < straw) Then
      r := m - 1
    Else
    If (needle > straw) Then
      l := m + 1
    Else
    Begin
      Result := ttSHORT(data, 22+(m*6));
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
   ascent  := Integer(ttSHORT(data, hhea + 4));
   descent := Integer(ttSHORT(data, hhea + 6));
   lineGap := Integer(ttSHORT(data, hhea + 8));
end;

Function TTFFont.ScaleForPixelHeight(height: Single): Single;
Var
   fHeight: Integer;
Begin
  If (_Ready) Then
  Begin
    fHeight := Integer(ttSHORT(data, hhea + 4) - ttSHORT(data, hhea + 6));
    Result := height / fheight;
  End Else
    Result := 0.0;
End;

Function TTFFont.GetCodepointBitmap(scaleX, scaleY: Single; codepoint: Integer; var xoff, yoff: Integer):Image;
Begin
  If (_Ready) Then
    Result := stbtt_GetGlyphBitmap(scaleX, scaleY, 0, 0, stbtt_FindGlyphIndex(codepoint), xoff, yoff)
  Else
    Result := Nil;
End;

function TTFFont.stbtt_GetGlyphBitmap(scale_x, scale_y, shift_x, shift_y: Single; glyph: Integer; var xoff, yoff: Integer): Image;
var
   ix0,iy0,ix1,iy1: Integer;
   num_verts: Integer;
   w,h:Integer;
   vertices: PStBttVertexArray;
begin
  Result := nil;
  vertices := nil;
  num_verts := stbtt_GetGlyphShape(glyph, vertices);

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
  Result := Image.Create(w, h);
  xoff   := ix0;
  yoff   := iy0;
  stbtt_Rasterize(Result, 0.35, vertices, num_verts, scale_x, scale_y, shift_x, shift_y, ix0, iy0, 1);

  If vertices<>nil Then
    FreeMem(vertices);
End;

{procedure TTFFont.stbtt_MakeGlyphBitmap(output: PByteArray; out_w, out_h, out_stride: Integer; scale_x, scale_y, shift_x, shift_y: Single; glyph: Integer);
var
   ix0,iy0,ix1,iy1: Integer;
   num_verts: Integer;
   vertices: PStBttVertexArray;
   dummy1, dummy2: Integer;
begin
   num_verts := stbtt_GetGlyphShape(glyph, vertices);

   stbtt_GetGlyphBitmapBox(glyph, scale_x, scale_y, shift_x, shift_y, ix0,iy0, dummy1, dummy2);
   gbm.pixels := output;
   gbm.w := out_w;
   gbm.h := out_h;
   gbm.stride := out_stride;

   if (gbm.w<>0) and (gbm.h<>0) then
      stbtt_Rasterize(gbm, 0.35, vertices, num_verts, scale_x, scale_y, shift_x, shift_y, ix0, iy0, 1);

   FreeMem(vertices);
end;}

function TTFFont.stbtt_FindGlyphIndex(unicode_codepoint: Integer):Word;
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
begin
   //stbtt_uint8 *data = info->data;
   //stbtt_uint32 index_map = info->index_map;

   format := ttUSHORT(data, index_map + 0);

   if format = 0 then // apple byte encoding
   begin
      bytes := Integer(ttUSHORT(data, index_map + 2));
      if unicode_codepoint < bytes-6 then
      begin
         Result := Integer(data[index_map + 6 + unicode_codepoint]);
         Exit;
      end;
      Result := 0;
      Exit;
   end else if format = 6 then
   begin
      first := Cardinal(ttUSHORT(data, index_map + 6));
      count := Cardinal(ttUSHORT(data, index_map + 8));
      if (Cardinal(unicode_codepoint) >= first) and (PtrUInt(unicode_codepoint) < first+count) then
      begin
         Result := Integer(ttUSHORT(data, PtrUInt(index_map) + 10 + (PtrUInt(unicode_codepoint) - first)*2));
         Exit;
      end;
      Result := 0;
      Exit;
   end else if format = 2 then
   begin
      //STBTT_assert(0); // @TODO: high-byte mapping for japanese/chinese/korean
      Result := 0;
      Exit;
   end else if format = 4 then // standard mapping for windows fonts: binary search collection of ranges
   begin
      segcount := ttUSHORT(data, index_map+6) shr 1;
      searchRange := ttUSHORT(data, index_map+8) shr 1;
      entrySelector := ttUSHORT(data, index_map+10);
      rangeShift := ttUSHORT(data, index_map+12) shr 1;

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
      if unicode_codepoint >= Integer(ttUSHORT(data, search + rangeShift*2)) then
         Inc(search, rangeShift*2);

      // now decrement to bias correctly to find smallest
      Dec(search, 2);
      while entrySelector<>0 do
      begin
         //stbtt_uint16 start, end;
         searchRange := searchRange shr 1;
         startValue2 := ttUSHORT(data, search + 2 + segcount*2 + 2);
         endValue2 := ttUSHORT(data, search + 2);
         startValue2 := ttUSHORT(data, search + searchRange*2 + segcount*2 + 2);
         endValue2 := ttUSHORT(data, search + searchRange*2);

         if unicode_codepoint > endValue2 then
            Inc(search, searchRange*2);
         Dec(entrySelector);
      end;
      Inc(search, 2);

      item := Word((search - endCount) shr 1);

      //STBTT_assert(unicode_codepoint <= ttUSHORT(data + endCount + 2*item));
      startValue := ttUSHORT(data, index_map + 14 + segcount*2 + 2 + 2*item);
      endValue := ttUSHORT(data, index_map + 14 + 2 + 2*item);
      if unicode_codepoint < startValue then
      begin
        //IntToString(unicode_codepoint); //BOO
         Result:=0;
         Exit;
      end;

      offset := Integer(ttUSHORT(data, index_map + 14 + segcount*6 + 2 + 2*item));
      if offset = 0 then
      begin
        n := ttSHORT(data, index_map + 14 + segcount*4 + 2 + 2*item);
         Result := unicode_codepoint + n;
         Exit;
      end;

      Result := ttUSHORT(data, offset + (unicode_codepoint-startValue)*2 + index_map + 14 + segcount*6 + 2 + 2*item);
      Exit;
   end else if format = 12 then
   begin
      ngroups := ttUSHORT(data, index_map+6);
      g := 0;
      low := 0;
      high := Integer(ngroups);
      // Binary search the right group.
      while low <= high do
      begin
         mid := low + ((high-low) shr 1); // rounds down, so low <= mid < high
         start_char := ttULONG(data, index_map+16+mid*12);
         end_char := ttULONG(data, index_map+16+mid*12+4);
         if PtrUInt(unicode_codepoint) < start_char then
            high := mid-1
         else
         if PtrUInt(unicode_codepoint) > end_char then
            low := mid+1
         else
         Begin
            start_glyph := ttULONG(data, index_map+16+mid*12+8);
            Result := PtrUInt(start_glyph + PtrUInt(unicode_codepoint) - start_char);
            Exit;
         End;
      end;
      Result := 0; // not found
      Exit;
   end;
   // @TODO
   Result := 0;
   Exit;
end;

function TTFFont.stbtt_GetCodepointShape(unicode_codepoint: Integer; var vertices: PStBttVertexArray): Integer;
begin
   Result := stbtt_GetGlyphShape(stbtt_FindGlyphIndex(unicode_codepoint), vertices);
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
   if glyph_index >= numGlyphs then
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
      g1 := glyf + ttUSHORT(data, loca + glyph_index * 2) * 2;
      g2 := glyf + ttUSHORT(data, loca + glyph_index * 2 + 2) * 2;
   end else begin
      g1 := glyf + ttULONG(data, loca + glyph_index * 4);
      g2 := glyf + ttULONG(data, loca + glyph_index * 4 + 4);
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

   x0 := ttSHORT(data, g + 2);
   y0 := ttSHORT(data, g + 4);
   x1 := ttSHORT(data, g + 6);
   y1 := ttSHORT(data, g + 8);

   Result := 1;
end;

function TTFFont.stbtt_GetCodepointBox(codepoint: Integer; var x0, y0, x1, y1: Integer): Integer;
begin
   Result := stbtt_GetGlyphBox(stbtt_FindGlyphIndex(codepoint), x0, y0, x1, y1);
end;

function TTFFont.stbtt_GetGlyphShape(glyph_index: Integer; var PVertices: PStBttVertexArray): Integer;
var
   numberOfContours: Smallint;
   endPtsOfContours: PByteArray;
   vertices: PStBttVertexArray;
   g, num_vertices: Integer;

   flags,flagcount: Byte;
   ins,i,j,m,n,next_move,was_off,off: Integer;
   x,y,cx,cy,sx,sy,dx,dy: Smallint;
   more: Integer;
   comp2: PByteArray;
   points: PByteArray;

   gidx: Word;
   comp_num_verts: Integer;
   comp_verts, tmp: PStBttVertexArray;
   ms,ns: Single;
   mtx: array[0..5] of Single;
   xx, yy: Smallint;
   PointIndex: Integer;

   tmpvx:PStBttVertex;
begin
   //stbtt_uint8 *data = info->data;
   vertices := nil;
   num_vertices := 0;
   g := stbtt__GetGlyfOffset(glyph_index);

   PVertices := nil;

   if (g < 0) then
   begin
     Result := 0;
     Exit;
   end;

   numberOfContours := ttSHORT(data, g);

   if numberOfContours > 0 then
   begin
      flags :=0;
      j := 0;
      was_off :=0;
      endPtsOfContours := PByteArray(PtrUInt(data) + PtrUInt(g) + 10);
      ins := ttUSHORT(data, g + 10 + numberOfContours * 2);
      points := PByteArray(PtrUInt(data) + PtrUInt(g) + 10 + PtrUInt(numberOfContours) * 2 + 2 + PtrUInt(ins));

      n := 1 + ttUSHORT(endPtsOfContours, numberOfContours*2-2);

      m := n + numberOfContours;  // a loose bound on how many vertices we might need
      GetMem(vertices, m * sizeof(TStBttVertex));
      if vertices = nil then
      begin
         Result := 0;
         Exit;
      end;

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
         vertices[off+i].vertexType := flags;
      end;

      // now load x coordinates
      x := 0;
      for i:=0 to n-1 do
      begin
         flags := vertices[off+i].vertexType;
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
         vertices[off+i].x := x;
      end;

      // now load y coordinates
      y := 0;
      for i:=0 to n-1 do
      begin
         flags := vertices[off+i].vertexType;
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
         vertices[off+i].y := y;
      end;

      // now convert them to our format
      num_vertices := 0;
      sx := 0;
      sy := 0;
      cx := 0;
      cy := 0;
      i := 0;
      while i<n do
      begin
         flags := vertices[off+i].vertexType;
         x := Smallint(vertices[off+i].x);
         y := Smallint(vertices[off+i].y);
         if next_move = i then
         begin
            // when we get to the end, we have to close the shape explicitly
            if i<>0 then
            begin
               if was_off<>0 then
                  stbtt_setvertex(vertices[num_vertices], STBTT_vcurve, sx, sy, cx, cy)
               else
                  stbtt_setvertex(vertices[num_vertices], STBTT_vline, sx, sy, 0, 0);

               Inc(num_vertices);
            end;

            // now start the new one
            stbtt_setvertex(vertices[num_vertices], STBTT_vmove,x,y,0,0);
            Inc(num_vertices);
            next_move := 1 + ttUSHORT(endPtsOfContours, j*2);
            Inc(j);
            was_off := 0;
            sx := x;
            sy := y;
         end else begin
            if (flags and 1) = 0 then  // if it's a curve
            begin
               if was_off<>0 then // two off-curve control points in a row means interpolate an on-curve midpoint
               begin
                  stbtt_setvertex(vertices[num_vertices], STBTT_vcurve, (cx+x) shr 1, (cy+y) shr 1, cx, cy);
                  Inc(num_vertices);
               end;
               cx := x;
               cy := y;
               was_off := 1;
            end else begin
               if was_off<>0 then
                  stbtt_setvertex(vertices[num_vertices], STBTT_vcurve, x,y, cx, cy)
               else
                  stbtt_setvertex(vertices[num_vertices], STBTT_vline, x,y,0,0);
               Inc(num_vertices);
               was_off := 0;
            end;
         end;
         Inc(i);
      end;
      if i<>0 then
      begin
         if was_off<>0 then
            stbtt_setvertex(vertices[num_vertices], STBTT_vcurve,sx,sy,cx,cy)
         else
            stbtt_setvertex(vertices[num_vertices], STBTT_vline,sx,sy,0,0);
         Inc(num_vertices);
      end;
   end else if numberOfContours = -1 then
   begin
      // Compound shapes.
      more := 1;
      comp2 := PByteArray(PtrUInt(data) + PtrUInt(g) + 10);
      num_vertices := 0;
      vertices := nil;
      while more<>0 do
      begin
         comp_num_verts := 0;
         comp_verts := nil;
         tmp := nil;
         mtx[0] := 1;
         mtx[1] := 0;
         mtx[2] := 0;
         mtx[3] := 1;
         mtx[4] := 0;
         mtx[5] := 0;

         flags := ttSHORT(comp2, 0);
         comp2 := Pointer(PtrUInt(comp2)+ 2);
         gidx := ttSHORT(comp2, 0);
         comp2 := Pointer(PtrUInt(comp2)+ 2);

         if (flags and 2)<>0 then // XY values
         begin
            if (flags and 1)<>0 then  // shorts
            begin
               mtx[4] := ttSHORT(comp2, 0);
               comp2 := Pointer(PtrUInt(comp2)+ 2);
               mtx[5] := ttSHORT(comp2, 0);
               comp2 := Pointer(PtrUInt(comp2)+ 2);
            end else begin
               mtx[4] := comp2[0];
               comp2 := Pointer(PtrUInt(comp2)+ 1);
               mtx[5] := comp2[0];
               comp2 := Pointer(PtrUInt(comp2)+ 1);
            end;
         end
         else begin
            // @TODO handle matching point
            //STBTT_assert(0);
         end;
         if (flags and (1 shl 3))<>0 then  // WE_HAVE_A_SCALE
         begin
            mtx[0] := ttSHORT(comp2, 0)/16384.0;
            mtx[1] := 0;
            mtx[2] := 0;
            mtx[3] := ttSHORT(comp2, 0)/16384.0;
            comp2 := Pointer(PtrUInt(comp2)+ 2);
         end else if (flags and (1 shl 6))<>0 then // WE_HAVE_AN_X_AND_YSCALE
         begin
            mtx[0] := ttSHORT(comp2, 0)/16384.0;
            comp2 := Pointer(PtrUInt(comp2)+ 2);
            mtx[1] := 0;
            mtx[2] := 0;
            mtx[3] := ttSHORT(comp2, 0)/16384.0;
            comp2 := Pointer(PtrUInt(comp2)+ 2);
         end else if (flags and (1 shl 7))<>0 then// WE_HAVE_A_TWO_BY_TWO
         begin
            mtx[0] := ttSHORT(comp2, 0)/16384.0;
            comp2 := Pointer(PtrUInt(comp2)+ 2);
            mtx[1] := ttSHORT(comp2, 0)/16384.0;
            comp2 := Pointer(PtrUInt(comp2)+ 2);
            mtx[2] := ttSHORT(comp2, 0)/16384.0;
            comp2 := Pointer(PtrUInt(comp2)+ 2);
            mtx[3] := ttSHORT(comp2, 0)/16384.0;
            comp2 := Pointer(PtrUInt(comp2)+ 2);
         end;

         // Find transformation scales.
         ms := Sqrt(mtx[0]*mtx[0] + mtx[1]*mtx[1]);
         ns := Sqrt(mtx[2]*mtx[2] + mtx[3]*mtx[3]);

         // Get indexed glyph.
         comp_num_verts := stbtt_GetGlyphShape(gidx, comp_verts);
         if comp_num_verts > 0 then
         begin
            // Transform vertices.
            for i := 0 to comp_num_verts-1 do
            begin
               //comp_verts[i];
               xx := comp_verts[i].x; yy := comp_verts[i].y;
               comp_verts[i].x := Smallint(Round(ms * (mtx[0]*xx + mtx[2]*yy + mtx[4])));
               comp_verts[i].y := Smallint(Round(ns * (mtx[1]*xx + mtx[3]*yy + mtx[5])));
               xx := comp_verts[i].cx;
               yy := comp_verts[i].cy;
               comp_verts[i].cx := Smallint(Round(ms * (mtx[0]*xx + mtx[2]*yy + mtx[4])));
               comp_verts[i].cy := Smallint(Round(ns * (mtx[1]*xx + mtx[3]*yy + mtx[5])));
            end;
            // Append vertices.
            GetMem(tmp, (num_vertices+comp_num_verts)*SizeOf(TStBttVertex));
            Move(vertices^, tmp^, num_vertices*sizeof(TStBttVertex));
            tmpvx := PStBttVertex(tmp);
            Inc(tmpvx, num_vertices);
            Move(comp_verts^, tmpvx^, comp_num_verts*sizeof(TStBttVertex));
            FreeMem(vertices);                                          
            vertices := tmp;

            FreeMem(comp_verts);
            Inc(num_vertices, comp_num_verts);
         end;
         // More components ?
         more := flags and  (1 shl 5);
      end;
   end else if numberOfContours < 0 then begin
      // @TODO other compound variations?
      //STBTT_assert(0);
   end else begin
      // numberOfCounters == 0, do nothing
   end;

   pvertices := vertices;
   Result := num_vertices;
end;


//
// antialiasing software rasterizer
//

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

procedure TTFFont.stbtt__add_point(points: PStBttPointArray; n: Integer; x, y: Single);
begin
   if points=nil then Exit; // during first pass, it's unallocated

   points[n].x := x;
   points[n].y := y;
end;

// tesselate until threshhold p is happy... @TODO warped to compensate for non-linear stretching
function TTFFont.stbtt__tesselate_curve(points:PStBttPointArray; var num_points: Integer; x0, y0, x1, y1, x2, y2, objspace_flatness_squared: Single; n: Integer): Integer;
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
function TTFFont.stbtt_FlattenCurves(Vertices: PStBttVertexArray; num_verts:Integer; ObjSpaceFlatness: Single; var contour_lengths: PIntegerArray; var num_contours: Integer): PStBttPointArray;
var
   points: PStBttPointArray;
   NumPoints: Integer;
   objspace_flatness_squared: Single;
   i, n, start, pass: Integer;
   x, y: Single;
begin
   points := nil;
   NumPoints := 0;

   objspace_flatness_squared := Sqr(ObjSpaceFlatness);
   n := 0;
   start := 0;

   // count how many "moves" there are to get the contour count
   for i:=0 to num_verts-1 do
      if vertices[i].vertexType = STBTT_vmove then Inc(n);

   num_contours := n;
   if n = 0 then
   begin
     Result := nil;
     Exit;
   end;

   GetMem(contour_lengths, n*sizeof(Integer));

   // make two passes through the points so we don't need to realloc
   for pass:=0 to 1 do
   begin
      x := 0;
      y := 0;
      if pass = 1 then
      begin
         GetMem(points, NumPoints * 2 * sizeof(TStBttPoint));
      end;
      NumPoints := 0;
      n := -1;

      for i:=0 to num_verts-1 do
      begin
         case vertices[i].vertexType of
            STBTT_vmove:
            begin
               // start the next contour
               if n >= 0 then
                  contour_lengths[n] := NumPoints - start;
               Inc(n);
               start := NumPoints;

               x := vertices[i].x;
               y := vertices[i].y;
               stbtt__add_point(points, NumPoints, x,y);
               Inc(NumPoints);
            end;
            STBTT_vline:
            begin
               x := vertices[i].x;
               y := vertices[i].y;
               stbtt__add_point(points, NumPoints, x, y);
               Inc(NumPoints);
            end;
            STBTT_vcurve:
            begin
               stbtt__tesselate_curve(points, NumPoints, x,y,
                                        vertices[i].cx, vertices[i].cy,
                                        vertices[i].x,  vertices[i].y,
                                        objspace_flatness_squared, 0);
               x := vertices[i].x;
               y := vertices[i].y;
            end;
         end;
      end;
      contour_lengths[n] := NumPoints - start;
   end;

   Result := points;
end;

procedure TTFFont.stbtt_Rasterize(resultBitmap:Image; FlatnessInPixels: Single; Vertices: PStBttVertexArray; NumVerts: Integer; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff, Invert: Integer);
var
  Scale: Single;
  WindingCount: Integer;
  WindingLengths: PIntegerArray;
  Windings: PStBttPointArray;
begin
   Scale := Min(ScaleX, ScaleY);

   Windings := stbtt_FlattenCurves(Vertices, NumVerts, FlatnessInPixels / Scale, WindingLengths, WindingCount);
   if windings<>nil then
   begin
      stbtt__rasterize(resultBitmap, Windings, WindingLengths, WindingCount, ScaleX, ScaleY, ShiftX, ShiftY, XOff, YOff, Invert);
      //FreeMem(Windings);
   end;
end;

Procedure TTFFont.stbtt__rasterize(resultBitmap:Image; Pts: PStBttPointArray; WCount: PIntegerArray; Windings: Integer; ScaleX, ScaleY, ShiftX,ShiftY: Single; XOff, YOff, Invert: Integer);
Var
  YScaleInv: Single;
  i,j,k,m, a,b, VSubSample: Integer;
  e: EdgeList;
  p: PStBttPointArray;
  en: ^TStBttEdge;
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

   for i:=0 to windings-1 do
   begin
      p := @Pts[m];
      Inc(m, wcount[i]);
      j := wcount[i]-1;
      k:=0;
      while k < wcount[i] do
      begin
         a := k;
         b := j;

         // skip the edge if horizontal
         if p[j].y <> p[k].y then
         begin
            New(en);
            e.Add(en);

            // add edge from j to k to the list
            en^.invert := 0;

            if ((invert<>0) and (p[j].y > p[k].y)) or
              ((invert=0) and (p[j].y < p[k].y)) then
            begin
              en^.invert := 1;
              a := j;
              b := k;
            end;

            en^.x0 := p[a].x * ScaleX + ShiftX;
            en^.y0 := p[a].y * YScaleInv * VSubSample + ShiftY;
            en^.x1 := p[b].x * ScaleX + ShiftX;
            en^.y1 := p[b].y * YScaleInv * VSubSample + ShiftY;
         end;

         j:=k;
         Inc(k);
      end;
   end;
   FreeMem(Pts);
   FreeMem(WCount);

   // now sort the edges by their highest point (should snap to integer, and then by x)
   e.Sort(EdgeCompare);

   GetMem(en, SizeOf(TStBttEdge));
   en^.y0 := 10000000;
   e.Add(en);

   // now, traverse the scanlines and find the intersections on each scanline, use xor winding rule
   stbtt__rasterize_sorted_edges(resultBitmap, e, e.Count-1, VSubSample, XOff, YOff);

   For I:=0 to Pred(e.Count) Do
   Begin
      FreeMem(e[i]);
   End;
   ReleaseObject(e);
end;

function TTFFont.new_active(e: PStBttEdge; off_x: Integer; start_point: Single): PStBttActiveEdge;
var
   z: PStBttActiveEdge;
   dxdy: Single;
begin
   New(z); // @TODO: make a pool of these!!!
   dxdy := (e^.x1 - e^.x0) / (e^.y1 - e^.y0);
   //STBTT_assert(e->y0 <= start_point);

   // round dx down to avoid going too far
   if dxdy < 0 then
      z^.dx := -Floor(FIX * -dxdy)
   else
      z^.dx := Floor(FIX * dxdy);
   z^.x := Floor(FIX * (e^.x0 + dxdy * (start_point - e^.y0)));
   Dec(z^.x, off_x * FIX);
   z^.ey := e^.y1;
   z^.next := nil;
   if e^.invert<>0 then z^.valid := 1 else z^.valid := -1;
   Result := z;
end;

// note: this routine clips fills that extend off the edges... ideally this
// wouldn't happen, but it could happen if the truetype glyph bounding boxes
// are wrong, or if the user supplies a too-small bitmap
procedure TTFFont.stbtt__fill_active_edges(scanline: PByteArray; len: Integer; e: PStBttActiveEdge; max_weight: Integer);
var
   x0,x1,w: Integer;
   i,j: Integer;
begin
   // non-zero winding fill
   x0 := 0;
   w := 0;

   while e<>nil do
   begin
      if w = 0 then
      begin
         // if we're currently at zero, we need to record the edge start point
         x0 := e^.x;
         Inc(w, e^.valid);
      end else begin
         x1 := e^.x;
         Inc(w, e^.valid);
         // if we went to zero, we need to draw
         if w = 0 then
         begin
            i := x0 shr FIXSHIFT;
            j := x1 shr FIXSHIFT;

            if (i < len) and (j >= 0) then
            begin
               if i = j then
               begin
                  // x0,x1 are the same pixel, so compute combined coverage
                  scanline[i] := scanline[i] + Byte(((x1 - x0) * max_weight) shr FIXSHIFT);
               end else begin
                  if i >= 0 then // add antialiasing for x0
                     scanline[i] := scanline[i] + Byte(((FIX - (x0 and FIXMASK)) * max_weight) shr FIXSHIFT)
                  else
                     i := -1; // clip

                  if (j < len) then// add antialiasing for x1
                     scanline[j] := scanline[j] + Byte(((x1 and FIXMASK) * max_weight) shr FIXSHIFT)
                  else
                     j := len; // clip

                  Inc(i);
                  while i<j do // fill pixels between x0 and x1
                  begin
                     scanline[i] := scanline[i] + Byte(max_weight);
                     Inc(i);
                  end;
               end;
            end;
         end;
      end;

      e := PStBttActiveEdge(e^.next);
   end;
end;


procedure TTFFont.stbtt__rasterize_sorted_edges(resultBitmap:Image; e: EdgeList; n, vsubsample, off_x, off_y: Integer);
var
   active: PStBttActiveEdge;
   y,j,s,iii: Integer;
   max_weight: Integer;
   scanline_data: ByteArray;
   scanline: PByteArray;
   scan_y: Single;
   step: ^PStBttActiveEdge;
   p, z: PStBttActiveEdge;
   changed: Integer;
   t, q: PStBttActiveEdge;
   eIndex: Integer;
   en : PStBttEdge;

   cnt, ofs: Integer;
begin
   eIndex := 0;
   active := nil;
   j := 0;
   max_weight := 255 div vsubsample;  // weight per vertical scanline

   scanline := @scanline_data;

   y := off_y * vsubsample;
   PStBttEdge(e.Items[n])^.y0 := (off_y + resultBitmap.Height) * vsubsample + 1;

   while (j < resultBitmap.Height) do
   begin
      for iii:=0 to resultBitmap.Width do
         scanline[iii] := 0;

      for s:=0 to vsubsample-1 do
      begin
         // find center of pixel for this scanline
         scan_y := y + 0.5;
         step := @active;

         // update all active edges;
         // remove all active edges that terminate before the center of this scanline
         while step^<>nil do
         begin
            z := step^;
            if (z^.ey <= scan_y) then
            begin
               step^ := z^.next; // delete from list
               z^.valid := 0;
               Dispose(z);
            end else begin
               Inc(z^.x, z^.dx); // advance to position for current scanline
               step := @(PStBttActiveEdge((step^)^.next)); // advance through list
            end;
         end;

         // resort the list if needed
         while true do
         begin
            changed := 0;
            step := @active;
            while (step^<>nil) and (step^.next<>nil) do
            begin
               if step^.x > PStBttActiveEdge(step^.next)^.x then
               begin
                  t := step^;
                  q := t^.next;

                  t^.next := q^.next;
                  q^.next := t;
                  step^ := q;
                  changed := 1;
               end;
               step := @(step^.next);
            end;
            if changed=0 then
               Break;
         end;

         // insert all edges that start before the center of this scanline -- omit ones that also end on this scanline
         while (PStBttEdge(e[eIndex])^.y0 <= scan_y) do
         begin
            if (PStBttEdge(e[eIndex])^.y1 > scan_y) then
            begin
               z := new_active(PStBttEdge(e[eIndex]), off_x, scan_y);
               // find insertion point
               if active = nil then
                  active := z
               else if (z^.x < active^.x) then                  // insert at front
               begin
                  z^.next := active;
                  active := z;
               end else begin
                  // find thing to insert AFTER
                  p := active;
                  while (p^.next<>nil) and (PStBttActiveEdge(p^.next)^.x < z^.x) do
                     p := PStBttActiveEdge(p^.next);
                  // at this point, p->next->x is NOT < z->x
                  z^.next := p^.next;
                  p^.next := z;
               end;
            end;
            Inc(eIndex);
         end;

         // now process all active edges in XOR fashion
         if active<>nil then
            stbtt__fill_active_edges(scanline, resultBitmap.Width, active, max_weight);


         Inc(y);
      end;

      For iii:=0 to Pred(resultBitmap.Width) Do
        resultBitmap.SetPixel(iii, j, ColorGrey(scanline[iii], scanline[iii]));

      Inc(j);
   end;

   while active<>nil do
   begin
      z := active;
      active := active^.next;
      Dispose(z);
   end;

   if scanline<>@scanline_data then
      FreeMem(scanline);
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
  LocalScale:Integer;
  Img:Image;
  {$IFDEF DISTANCEFIELDFONTS}
  Temp:Image;
  {$ENDIF}
Begin
  Result := Nil;

  If (Not Self.HasGlyph(ID)) Then
    Exit;

  {$IFDEF DISTANCEFIELDFONTS}
  LocalScale := FontQuality;
  {$ELSE}
  LocalScale := 1;
  {$ENDIF}

  _Scale := Self.ScaleForPixelHeight(Size);

  {$IFDEF DEBUG_FONTS}Log(logDebug,'Font','Rendering ttf '+IntToString(ID));{$ENDIF}

  If (ID = 32) Then
  Begin
    OpID := Ord('E');
    Img := GetCodepointBitmap(_Scale * LocalScale, _Scale * LocalScale, OpID, xofs, yofs);
    ReleaseObject(Img);
    Img := Image.Create(4,4);
    stbtt_GetCodepointHMetrics(OpID, XAdv, lsb);
    Result := Font.AddGlyph(ID, Img, XOfs, YOfs, Trunc(XAdv*_Scale));
    ReleaseObject(Img);
    Exit;
  End;

  Img := GetCodepointBitmap(_Scale * LocalScale, _Scale * LocalScale, ID, xofs, yofs);
  If Not Assigned(Img) Then
    Exit;

  //Img.Save('out\g'+IntToString(ID)+'.png');

  W := Img.Width;
  H := Img.Height;
  W := W Div LocalScale;
  H := H Div LocalScale;
  XOfs := XOfs Div LocalScale;
  YOfs := YOfs Div LocalScale;

  {$IFDEF DISTANCEFIELDFONTS}
  Temp := CreateDistanceField(Img, componentAlpha, LocalScale, LocalScale*2);
  ReleaseObject(Img);
  Img := Temp;
  {$ENDIF}

  stbtt_GetCodepointHMetrics(ID, XAdv, lsb);

  Result := Font.AddGlyph(ID, Img, XOfs, YOfs, Trunc(XAdv*_Scale));

  //Img.Save('glyphs\g'+IntToString(ID)+'.png');

  ReleaseObject(Img);
End;


Function LoadTTF(Source:Stream; Font:TERRAFont):Boolean;
Var
  Factory:TTFFont;
  Img:Image;
  Size:Integer;
  Scale:Single;
  XAdv,lsb:Integer;
Begin
  Factory := TTFFont.Create(Source);
  Result := Factory.Ready;
  If Result Then
  Begin
    Font.AddGlyphFactory(Factory);
  End;
End;

Function ValidateTTF(Source:Stream):Boolean;
Var
   Major, Minor:Word;
Begin
  Source.Read(@Major, 2);
  Source.Read(@Minor, 2);
  ByteSwap16(Major);
  ByteSwap16(Minor);
  Result := (Major = 1) And (Minor = 0);
End;


Initialization
  RegisterFontFormat('TTF', ValidateTTF, LoadTTF);
End.


