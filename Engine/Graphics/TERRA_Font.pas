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
Unit TERRA_Font;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Utils, TERRA_Resource, TERRA_Stream, TERRA_Image, TERRA_Color, TERRA_Vector2D,
  TERRA_Math, TERRA_Texture, TERRA_SpriteManager, TERRA_Renderer,
  TERRA_ResourceManager, TERRA_Matrix4x4, TERRA_Matrix3x3, TERRA_ClipRect, TERRA_Collections;


Const
  TabSize = 100;

  {$IFDEF DISTANCEFIELDFONTS}
  FontQuality = 4;
  {$ELSE}
  FontQuality = 1;
  {$ENDIF}

  DefaultFontPageWidth = 256 * FontQuality;
  DefaultFontPageHeight = 512 * FontQuality;

  fontmode_Sprite   = 0;
  fontmode_Measure  = 1;
  fontmode_Offscreen=2;

  fontControlColor = 1;
  fontControlCornerColorA = 2;
  fontControlCornerColorB = 3;
  fontControlCornerColorC = 4;
  fontControlCornerColorD = 5;
  fontControlTab = 8;
  fontControlBlink = 9;
  fontControlItalics = 11;
  fontControlWave = 12;
  fontControlNewLine = 13;
  fontControlSprite = 14;
  fontControlEnd = 31;

Type
  FontGradient = (
    gradientNone      = 0,
    gradientHorizontal= 1,
    gradientVertical  = 2
  );


  PFontKerning = ^FontKerning;
  FontKerning = Record
    Next:Cardinal;
    Ammount:SmallInt;
  End;

  Font=Class;
  FontGlyphFactory = Class;

  FontGlyph = Class(TERRAObject)
    Protected
      _Temp:Image;
      _Source:Image;
      _Font:Font;
      _Factory:FontGlyphFactory;

    Public
      ID:Cardinal;
      X, Y:Word;
      Width:Word;
      Height:Word;
      XOfs,YOfs:SmallInt;
      XAdvance:SmallInt;
      Page:Byte;

      KerningList:Array Of FontKerning;
      KerningCount:Integer;

      Procedure Release; Override;

      Function GetAdvance(Next:Cardinal):Integer;
      Procedure AddKerning(Next:Cardinal; Ammount:SmallInt);

      Function GetImage():Image;

      Function IsLoading():Boolean;
  End;

  FontPage = Class(TERRAObject)
    Protected
      _ID:Integer;
      _Texture:Texture;
      _Image:Image;
      _Font:Font;

      _OptimizedWidth:Cardinal;
      _OptimizedHeight:Cardinal;

    Public
      Constructor Create(ID:Integer);
      Procedure Release; Override;

      Procedure SetImage(Source:Image);

      Procedure DrawGlyph(X,Y,Z:Single; Const Transform:Matrix3x3; Glyph:FontGlyph; Outline, A,B,C,D:Color; Clip:ClipRect; Italics:Boolean);

      Property Texture:Texture Read _Texture;
  End;

  FontEffect = Record
    Effect:TERRAChar;
    Arg:TERRAString;
  End;


  FontGlyphFactory = Class(TERRAObject)
    Protected
      _Next:FontGlyphFactory;
      _Scale:Single;

    Public
      Function InitGlyph(Font:Font; ID:Cardinal; Size:Integer):FontGlyph; Virtual; Abstract;
      Function GetKerning(Current, Next:Cardinal):Integer; Virtual; Abstract;
  End;

  Font = Class(Resource)
    Protected
      _TextSize:Integer;

      _Glyphs:Array Of FontGlyph;
      _GlyphCount:Integer;

      _PageCount:Integer;
      _Pages:Array Of FontPage;

      _AvgHeight:Single;

      _Factory:FontGlyphFactory;
      _NeedsRebuild:Boolean;
      _AddingGlyph:Boolean;
      _Loading:Boolean;

      Procedure Rebuild();

    Public
      Function Load(Source:Stream):Boolean; Override;
      //Function Save(FileName:TERRAString):Boolean;

      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;
      Procedure OnContextLost; Override;

      Function AddPage():FontPage;
      Function GetPage(Index:Integer):FontPage;

      Function AddGlyph(ID:Cardinal; Source:Image; XOfs,YOfs:SmallInt; XAdvance:SmallInt = -1):FontGlyph; Overload;
      Function AddGlyph(ID:Cardinal; FileName:TERRAString; XOfs,YOfs:SmallInt; XAdvance:SmallInt = -1):FontGlyph; Overload;
      Function AddEmptyGlyph():FontGlyph;
      Function GetGlyph(ID:Cardinal; CreatedIfNeeded:Boolean = True):FontGlyph;
      Procedure SortGlyphs();

      Procedure RecalculateMetrics();

      Function SelectPage(Index, Slot:Integer):FontPage;

      Class Function GetManager:Pointer; Override;

      Procedure AddGlyphFactory(Factory:FontGlyphFactory; Scale:Single = 1.0);

      Property TextSize:Integer Read _TextSize;

      Property PageCount:Integer Read _PageCount;

      Property NewLineOffset:Single Read _AvgHeight Write _AvgHeight;
  End;

  FontStreamValidateFunction = Function(Source:Stream):Boolean;
  FontLoader = Function(Source:Stream; Font:Font):Boolean;

  FontClassInfo = Record
    Name:TERRAString;
    Validate:FontStreamValidateFunction;
    Loader:FontLoader;
  End;

  FontManager = Class(ResourceManager)
    Protected
      _DefaultFont:Font;

      Function GetDefaultFont: Font;

    Public
      Procedure Release; Override;

      Class Function Instance:FontManager;

      Function GetFont(Name:TERRAString; ValidateError:Boolean = True):Font;

      Property DefaultFont:Font Read GetDefaultFont;
   End;

  Function GetFontLoader(Source:Stream):FontLoader;
  Procedure RegisterFontFormat(Name:TERRAString; Validate:FontStreamValidateFunction; Loader:FontLoader);

  Function ConvertFontCodes(S:TERRAString):TERRAString;
  Function UnconvertFontCodes(S:TERRAString):TERRAString;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Application, TERRA_Sort,
  TERRA_Log, TERRA_FileUtils, TERRA_MemoryStream,
  TERRA_GraphicsManager, TERRA_FileManager, TERRA_Packer;

Var
  _FontExtensions:Array Of FontClassInfo;
  _FontExtensionCount:Integer;
  _FontManager_Instance:ApplicationObject;

Type
  GlyphSort = Class(Sort)
    Public
      Class Procedure Swap(Data:Pointer; A,B:Integer); Override;
      Class Procedure SetPivot(Data:Pointer; A:Integer); Override;
      Class Function Compare(Data:Pointer; A:Integer):Integer; Override;
  End;

Var
  _GlyphPivot:Cardinal;

{ GlyphSort }
Class Procedure GlyphSort.SetPivot(Data:Pointer; A:Integer);
Var
  Fnt:Font;
Begin
  Fnt := Font(Data);
  _GlyphPivot := Fnt._Glyphs[A].ID;
End;

Class Function GlyphSort.Compare(Data:Pointer; A:Integer):Integer;
Var
  Fnt:Font;
Begin
  Fnt := Font(Data);
  If (Fnt._Glyphs[A].ID < _GlyphPivot) Then
    Result := 1
  Else
  If (Fnt._Glyphs[A].ID > _GlyphPivot) Then
    Result := -1
  Else
    Result := 0;
End;

Class Procedure GlyphSort.Swap(Data:Pointer; A,B:Integer);
Var
  Fnt:Font;
  Temp:FontGlyph;
Begin
  Fnt := Font(Data);
  Temp := Fnt._Glyphs[A];
  Fnt._Glyphs[A] := Fnt._Glyphs[B];
  Fnt._Glyphs[B] := Temp;
End;


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
Class Function FontManager.Instance:FontManager;
Begin
  If _FontManager_Instance = Nil Then
    _FontManager_Instance := InitializeApplicationComponent(FontManager, TextureManager);

  Result := FontManager(_FontManager_Instance.Instance);
End;

Type
  PFontSearch = ^FontSearch;
  FontSearch = Record
    Name:TERRAString;
    TextSize:Integer;
  End;

Function SearchFontByNameAndSize(P:CollectionObject; UserData:Pointer):Boolean; CDecl;
Begin
  Result := (Resource(P).Name = PFontSearch(Userdata).Name) And (Font(P).TextSize = PFontSearch(Userdata).TextSize);
End;

Function FontManager.GetFont(Name:TERRAString; ValidateError:Boolean):Font;
Var
  FontName, FileName, S:TERRAString;
  I:Integer;
  Size:Integer;
  Params:FontSearch;
Begin
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  I := StringCharPosReverse(Ord('@'), Name);
  If (I>0) Then
  Begin
    FontName := Copy(Name, 1, Pred(I));
    S := Copy(Name, Succ(I), MaxInt);
    Size := StringToInt(S);
    If Size<=0 Then
    Begin
      FontName := Name;
      Size := 30;
    End;
  End Else
  Begin
    FontName := Name;
    Size := 30;
  End;

  Name := GetFileName(Name, True);
  Params.Name := Name;
  Params.TextSize := Size;
  Result := Font(_Resources.Search(SearchFontByNameAndSize, @Params));
  If (Assigned(Result)) Then
    Exit;

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
    Result := Font.Create(S);
    Result._TextSize := Size;
    Result.Priority := 90;
    Result._Key := Name;
    Self.AddResource(Result);
  End Else
  If ValidateError Then
    RaiseError('Could not find font. ['+Name +']');
End;

Procedure FontManager.Release;
Begin
  Inherited;

  ReleaseObject(_DefaultFont);
  _FontManager_Instance := Nil;
End;

{$I default_font.inc}
Function FontManager.GetDefaultFont: Font;
Var
  Glyph:FontGlyph;
  Page:FontPage;
  I, ID:Integer;
  Src:Stream;
  Img:Image;
Begin
  Result := _DefaultFont;
  If Assigned(Result) Then
    Exit;

  _DefaultFont := Font.Create('');
  Result := _DefaultFont;
  Page := _DefaultFont.AddPage();

  For I:=32 To 128 Do
  Begin
    ID := I;
    Glyph := Nil;

	  Case ID Of
	  32:
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*20;
		  Glyph.Y := 12*2;
		  Glyph.Width := 4;
		  Glyph.Height := 12;
		End;

	  Ord('#'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*10;
		  Glyph.Y := 12*2;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;

	  Ord('!'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*11;
		  Glyph.Y := 12*2;
		  Glyph.Width := 4;
		  Glyph.Height := 12;
		End;

	  Ord('?'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*12;
		  Glyph.Y := 12*2;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;

	  Ord(','):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*13;
		  Glyph.Y := 12*2;
		  Glyph.Width := 4;
		  Glyph.Height := 12;
		End;

	  Ord('.'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*14;
		  Glyph.Y := 12*2;
		  Glyph.Width := 4;
		  Glyph.Height := 12;
		End;

	  Ord('$'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*15;
		  Glyph.Y := 12*2;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;

	  Ord(':'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*16;
		  Glyph.Y := 12*2;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;

	  Ord('+'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*17;
		  Glyph.Y := 12*2;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;

	  Ord('-'):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*18;
		  Glyph.Y := 12*2;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;

	  Ord(''''):
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Glyph.X := 8*19;
		  Glyph.Y := 12*2;
		  Glyph.Width := 4;
		  Glyph.Height := 12;
		End;

	  48..57:
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  Dec(ID, 48);
		  Glyph.X := 8*ID;
		  Glyph.Y := 12*2;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;
	  65..90:
		Begin
		  Glyph := Result.AddEmptyGlyph();

		  If (ID=73) Then
			Glyph.XAdvance := 5;
		  Dec(ID, 65);
		  Glyph.X := 8*ID;
		  Glyph.Y := 12*0;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;
	  97..122:
		Begin
		  Glyph := Result.AddEmptyGlyph();
		  If (ID=105) Then
			Glyph.XAdvance := 5;
		  Dec(ID, 97);
		  Glyph.X := 8*ID;
		  Glyph.Y := 12*1;
		  Glyph.Width := 8;
		  Glyph.Height := 12;
		End;
	  End;

	  If (Assigned(Glyph)) Then
    Begin
      Glyph.ID := I;
      If (Glyph.XAdvance<=0) Then
    		Glyph.XAdvance := Glyph.Width;
    End;
  End;

  Src := MemoryStream.Create(bm_size, @bm_data[0]);
  Img := Image.Create(Src);
  Page.SetImage(Img);
  Src.Release();
  Img.Release();

  Result.RecalculateMetrics();
End;

{ FontGlyph }
Procedure FontGlyph.AddKerning(Next: Cardinal; Ammount: SmallInt);
Var
  N:Integer;
Begin
  N := Self.KerningCount;
  Inc(Self.KerningCount);
  SetLength(Self.KerningList, Self.KerningCount);
  Self.KerningList[N].Next := Next;
  Self.KerningList[N].Ammount := Ammount;
End;

Procedure FontGlyph.Release;
Begin
  If (Assigned(_Temp)) And (_Temp<>_Source) Then
    _Temp.Release();

  If (Assigned(_Source)) Then
    _Source.Release();
End;

Function FontGlyph.GetAdvance(Next:Cardinal):Integer;
Var
  I:Integer;
Begin
  Result := (XAdvance - XOfs);

  For I:=0 To Pred(KerningCount) Do
  If (KerningList[I].Next = Next) Then
  Begin
    Inc(Result, KerningList[I].Ammount);
    Exit;
  End;

  If Assigned(Self._Factory) Then
    Inc(Result, Self._Factory.GetKerning(ID, Next));
End;

Function FontGlyph.GetImage: Image;
Begin
  If Assigned(_Temp) Then
    Result := _Temp
  Else
  If Assigned(_Source) Then
    Result := _Source
  Else
    Result := Nil;
End;

Function FontGlyph.IsLoading: Boolean;
Begin
  Result := Assigned(_Temp);
End;

{ FontPage }
Constructor FontPage.Create(ID: Integer);
Begin
  Self._ID := ID;
End;

Procedure FontPage.Release;
Begin
  If Assigned(_Texture) Then
    _Texture.Release();

  If Assigned(_Image) Then
    _Image.Release();
End;

Procedure FontPage.DrawGlyph(X,Y,Z:Single; Const Transform:Matrix3x3; Glyph:FontGlyph; Outline, A,B,C,D:Color; Clip:ClipRect; Italics:Boolean);
Var
  S:QuadSprite;
  Filter:TextureFilterMode;
Begin
  {$IFDEF DISTANCEFIELDFONTS}
  Filter := filterBilinear;
  {$ELSE}
  Filter := filterLinear;
  {$ENDIF}

  S := SpriteManager.Instance.DrawSpriteWithOutline(X + Glyph.XOfs, Y + Glyph.YOfs, Z, _Texture, Outline, Nil, blendBlend, 1.0, Filter, True);
  S.SetColors(A,B,C,D);

  S.SetTransform(Transform);
  S.ClipRect := Clip;
//  S.SetScale(Scale);
  S.Rect.PixelRemap(Glyph.X, Glyph.Y, Glyph.X + Glyph.Width, Glyph.Y + Glyph.Height, Glyph.Width, Glyph.Height);

  If (Italics) Then
    S.Skew := 5.0;
End;

Procedure FontPage.SetImage(Source: Image);
Begin
  If (_Texture = Nil) Then
  Begin
    _Texture := TERRA_Texture.Texture.Create();
    _Texture.CreateFromImage(Self._Font.Name+'_page'+IntTostring(Self._ID), Source);
  End Else
  If (_Texture.Width<>Source.Width) Or (_Texture.Height<>Source.Height) Then
  Begin
    _Texture.Release();
    _Texture := Nil;
  End;

  _Texture.MipMapped := False;
  _OptimizedWidth := 0;
  _OptimizedHeight := 0;

  If Assigned(_Image) Then
    _Image.Release();

  _Image := Image.Create(Source);
End;


{ Font }
Function Font.Load(Source: Stream): Boolean;
Var
  Loader:FontLoader;
  Pos:Integer;
Begin
  Result := False;
  If (Source = Nil) Then
    Exit;

  _GlyphCount := 0;

  Pos := Source.Position;
  Loader := GetFontLoader(Source);
  If (Not Assigned(Loader)) Then
    Exit;

  Source.Seek(Pos);
  _Loading := True;
  Result := Loader(Source, Self);
  _Loading := False;

  If (Not Result) Then
    Exit;

  RecalculateMetrics();
End;

Procedure Font.RecalculateMetrics();
Var
  Glyph:FontGlyph;
  Page:FontPage;
  I, Pos:Integer;
Begin
  _AvgHeight := 0.0;
  For I:=0 To Pred(_GlyphCount) Do
  Begin
    Glyph := _Glyphs[I];
    _AvgHeight := _AvgHeight + Glyph.Height;

    Page := Self.GetPage(Glyph.Page);
    If Assigned(Page) Then
    Begin
      If (Glyph.X + Glyph.Width > Page._OptimizedWidth) Then
        Page._OptimizedWidth := Glyph.X + Glyph.Width;

      If (Glyph.Y + Glyph.Height > Page._OptimizedHeight) Then
        Page._OptimizedHeight := Glyph.Y + Glyph.Height;
    End;
  End;

  If (_GlyphCount>0) Then
    _AvgHeight := _AvgHeight / _GlyphCount;
End;

Class Function Font.GetManager:Pointer;
Begin
  Result := FontManager.Instance;
End;

Function Font.GetGlyph(ID:Cardinal; CreatedIfNeeded:Boolean = True):FontGlyph;
Var
  A,B, Mid:Integer;
  I:Integer;
  F:FontGlyphFactory;
Begin
  Result := Nil;

  If (Self._NeedsRebuild) Or (_AddingGlyph) Then
  Begin // not sorted yet, so cannot do a binary search
    For I:=0 To Pred(_GlyphCount) Do
    If (_Glyphs[I].ID = ID) Then
    Begin
      Result := _Glyphs[I];
      Exit;
    End;
  End Else
  Begin
    {If (ID=49) Then
      IntToString(2);}

    A := 0;
    B := Pred(_GlyphCount);
    Mid := 0;
    While (A<B) Do
    Begin
      Mid := A + ((B-A) Shr 1);

      If (_Glyphs[Mid].ID < ID) Then
      Begin
        A := Mid + 1;
        If (_Glyphs[A].ID = ID) Then // hack
        Begin
          Inc(Mid);
          Break;
        End;
      End Else
      If (_Glyphs[Mid].ID > ID) Then
      Begin
        B := Mid;
      End Else
        Break;
    End;

    {$IFDEF DEBUG_FONTS}Log(logDebug, 'Texture', 'Testing glyph '+IntToString(Mid));{$ENDIF}
    {$IFDEF DEBUG_FONTS}Log(logDebug, 'Texture', 'Glyph count '+IntToString(_GlyphCount)+'/'+IntToString(Length(_Glyphs)));{$ENDIF}

    If (Mid<_GlyphCount) And (_GlyphCount>0) And (_Glyphs[Mid]<>Nil) And (_Glyphs[Mid].ID = ID) Then
    Begin
      Result := _Glyphs[Mid];
      If (Assigned(Result)) Then
        Exit;
    End;
  End;

  If (Not CreatedIfNeeded) Then
    Exit;

  // not found yet? well, lets try to stream it from one of the glyph factories
  F := _Factory;
  While Assigned(F) Do
  Begin
    Result := F.InitGlyph(Self, ID, Trunc(_TextSize * F._Scale));
    If Assigned(Result) Then
    Begin
      Result._Factory := F;
      Exit;
    End;

    F := F._Next;
  End;

  Log(logWarning, 'Font', 'Glyph '+IntToString(ID)+' was not found!');
End;


Function Font.Unload: Boolean;
Var
  I:Integer;
Var
  F:FontGlyphFactory;
Begin
  F := _Factory;
  While Assigned(F) Do
  Begin
    _Factory := F;
    F := F._Next;
    _Factory.Release();
  End;
  _Factory := Nil;

  For I:=0 To Pred(_PageCount) Do
    _Pages[I].Release();
    
  _PageCount := 0;
  SetLength(_Pages, 0);

  For I:=0 To Pred(_GlyphCount) Do
    _Glyphs[I].Release();
    
  _GlyphCount := 0;
  SetLength(_Glyphs, 0);

  Result := True;
End;

Function Font.Update:Boolean;
Begin
  Inherited Update();

  If (_NeedsRebuild) Then
    Self.Rebuild();

  If (_AvgHeight<=0) Then
    Self.RecalculateMetrics();

	Result := True;
End;

Function Font.GetPage(Index:Integer):FontPage;
Begin
  If (Index<0) Or (Index>=_PageCount) Then
    Result := Nil
  Else
    Result := _Pages[Index];
End;


Function Font.AddPage():FontPage;
Var
  N:Integer;
Begin
  N := _PageCount;
  Inc(_PageCount);
  SetLength(_Pages, _PageCount);
  _Pages[N] := FontPage.Create(N);
  _Pages[N]._Texture := Nil;
  _Pages[N]._Image := Nil;
  _Pages[N]._Font := Self;
  Result := _Pages[N];
End;

Procedure Font.SortGlyphs;
Var
  I,J:Integer;
  Temp:FontGlyph;
Begin
//  GlyphSort.Sort(Self, _GlyphCount);
  For I:= Pred(_GlyphCount) DownTo 0 Do
    For J:=1 To I Do
    If (_Glyphs[J-1].ID > _Glyphs[J].ID) Then
    Begin
     Temp := _Glyphs[J-1];
     _Glyphs[J-1] := _Glyphs[j];
     _Glyphs[J] := Temp;
    End;
End;

Procedure Font.AddGlyphFactory(Factory: FontGlyphFactory; Scale:Single);
Var
  F:FontGlyphFactory;
Begin
  If (Factory = Nil) Then
    Exit;
    
  Factory._Scale := Scale;

  If (_Factory = Nil) Then
  Begin
    _Factory := Factory;
    Exit;
  End;

  F := _Factory;
  While Assigned(F._Next) Do
  Begin
    If (F = Factory) Or (F._Next = Factory) Then
      Exit;

    F := F._Next;
  End;

  F._Next := Factory;
  Factory._Next := Nil;
End;

Function Font.AddEmptyGlyph:FontGlyph;
Begin
  Inc(_GlyphCount);
  SetLength(_Glyphs, _GlyphCount);
  Result := FontGlyph.Create();
  Result._Font := Self;
  _Glyphs[Pred(_GlyphCount)] := Result;
End;

Function Font.AddGlyph(ID:Cardinal; FileName:TERRAString; XOfs, YOfs, XAdvance: SmallInt):FontGlyph;
Var
  Source: Image;
Begin
  {$IFDEF DISTANCEFIELDFONTS}
  FileName := GetFileName(FileName, True) + '_.' + GetFileExtension(FileName);
  {$ENDIF}

  Source := Image.Create(FileName);
  If (Source.Width>0) Then
    Result := Self.AddGlyph(ID, Source, XOfs, YOfs, XAdvance)
  Else
    Result := Nil;
  Source.Release();
End;

Function Font.AddGlyph(ID: Cardinal; Source: Image; XOfs, YOfs, XAdvance: SmallInt):FontGlyph;
Begin
  //Self.Prefetch();

  Result := Nil;

  If (Source = Nil) Or (Source.Width<=0) Then
    Exit;

  _AddingGlyph := True;
  Result := Self.GetGlyph(ID, False);
  _AddingGlyph := False;

  If (Assigned(Result)) Then
  Begin
    Result.XOfs := XOfs;
    Result.YOfs := YOfs;
    Result.XAdvance := XAdvance;
    Exit;
  End;

  Result := Self.AddEmptyGlyph();

  {If (Id=32) Then
    IntToString(2);}

  Result.ID := ID;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.XOfs := XOfs;
  Result.YOfs := YOfs;
  Result._Temp := Image.Create(Source);

  If (XAdvance<=0) Then
    XAdvance := Source.Width;

  Result.XAdvance := XAdvance;

  _NeedsRebuild := True;
End;

Procedure Font.Rebuild();
Var
  X,Y:Integer;
  I,K:Integer;
  Packer:RectanglePacker;
  Pics:Array Of Image;
  Temp:Image;
  S:TERRAString;

  Function Finished():Boolean;
  Var
    I:Integer;
  Begin
    For I:=0 To Pred(_GlyphCount) Do
    If (Pics[I]<>Nil) Then
    Begin
      Result := False;
      Exit;
    End;

    Result := True;
  End;

Begin
  _NeedsRebuild := False;
  Log(logDebug,'Font', 'Updating font: '+ Self.Name);

  Self.SortGlyphs();

  // rebuild the whole font atlas
  SetLength(Pics, _GlyphCount);
  For I:=0 To Pred(_GlyphCount) Do
  Begin
    If (_Glyphs[I]._Temp<>Nil) Then
    Begin
      Pics[I] := _Glyphs[I]._Temp;
      _Glyphs[I]._Temp := Nil;
    End Else
    Begin
      Log(logDebug,'Font', 'Generating glyph '+IntToString(I));
      Pics[I] := _Pages[_Glyphs[I].Page]._Image.SubImage(_Glyphs[I].X, _Glyphs[I].Y, _Glyphs[I].X + _Glyphs[I].Width, _Glyphs[I].Y + _Glyphs[I].Height);
    End;

    If (Assigned(_Glyphs[I]._Source)) And (_Glyphs[I]._Source <> Pics[I]) Then
    Begin
      _Glyphs[I]._Source.Release();
    End;
    
    _Glyphs[I]._Source := Pics[I];
  End;

  K := 0;
  While Not Finished() Do
  Begin
    If (K>=_PageCount) Then
    Begin
      Self.AddPage();
      _Pages[K]._Image := Image.Create(DefaultFontPageWidth, DefaultFontPageHeight);
    End Else
    Begin
      _Pages[K]._Image.Release;
      _Pages[K]._Image := Image.Create(DefaultFontPageWidth, DefaultFontPageHeight);
    End;

    If (Application.Instance<>Nil) Then
    Begin
      If (_Pages[K]._Texture = Nil) Then
      Begin
        _Pages[K]._Texture := Texture.Create();
        _Pages[K]._Texture.CreateFromSize(Self.Name+'_page'+IntToString(K), _Pages[K]._Image.Width, _Pages[K]._Image.Height);
      End;
      
      _Pages[K]._Texture.IsReady();
    End;

    Temp := _Pages[K]._Image;
    //Temp.Process(IMP_FillColor, ColorNull);
    Packer := RectanglePacker.Create();

    For I:=0 To Pred(_GlyphCount) Do
    If (Pics[I]<>Nil) Then
    Begin
      Packer.AddRect(Pics[I].Width+3, Pics[I].Height+3, I);
    End;

    Packer.Pack(Temp.Width, Temp.Height);
    For I:=0 To Pred(_GlyphCount) Do
    Begin
      If (Pics[I] = Nil) Then
        Continue;

      X := 0;
      Y := 0;
      If (Packer.GetRect(I, X, Y)) Then
      Begin
        _Glyphs[I].X := X;
        _Glyphs[I].Y := Y;
        _Glyphs[I].Page := K;
        //Log(logDebug,'Font', 'Bliting glyph '+IntToString(I));

        Temp.Blit(X,Y, 0, 0, Pics[I].Width, Pics[I].Height, Pics[I]);
        //Pics[I].Release();
        Pics[I] := Nil;
      End;
    End;

    //Temp.Save('koo_'+IntToString(K)+'.png');

    If Assigned(_Pages[K]._Texture) Then
      _Pages[K]._Texture.UpdateRect(Temp, 0, 0);

    ReleaseObject(Packer);

    Inc(K);
  End;

  RecalculateMetrics();
End;


Procedure Font.OnContextLost;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_PageCount) Do
    ReleaseObject(_Pages[I]._Texture);

  Self._ContextID := Application.Instance.ContextID;
  _NeedsRebuild := True;
End;

Function ConvertFontCodes(S:TERRAString):TERRAString;
Var
  S2:TERRAString;
  C:TERRAChar;
  It:StringIterator;
Begin
  Result := '';

  While StringCharPosIterator(Ord('\'), S, It) Do
  Begin
    It.Split(S2, S);

    Result := Result + S2;

    C := CharLower(StringFirstChar(S));
    S := StringCopy(S, 2, MaxInt);

    Case C Of
    Ord('n'):  Result := Result + StringFromChar(fontControlNewLine);
    Ord('t'):  Result := Result + StringFromChar(fontControlTab);
    Ord('b'):  Result := Result + StringFromChar(fontControlBlink);
    Ord('i'):  Result := Result + StringFromChar(fontControlItalics);
    Ord('w'):  Result := Result + StringFromChar(fontControlWave);

    Ord('p'):
      Begin
        S2 := StringGetNextSplit(S, Ord('}'));
        S2 := StringCopy(S2, 2, MaxInt);
        Result := Result + StringFromChar(fontControlSprite) + S2 + StringFromChar(fontControlEnd);
      End;

    Ord('c'):
      Begin
        S2 := StringGetNextSplit(S, Ord('}'));
        S2 := StringCopy(S2, 2, MaxInt);
        Result := Result + StringFromChar(fontControlColor) + S2 + StringFromChar(fontControlEnd);
      End;

    Else
      Result := Result + StringFromChar(C);

    End;
  End;

  Result := Result + S;
End;

Function UnconvertFontCodes(S:TERRAString):TERRAString;
Var
  S2:TERRAString;
  C:TERRAChar;
  It:StringIterator;
Begin
  Result := '';

  StringCreateIterator(S, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    Case C Of
    fontControlNewLine: Result := Result + '\n';
    fontControlTab: Result := Result + '\t';
    fontControlBlink: Result := Result + '\b';
    fontControlItalics: Result := Result + '\i';
    fontControlWave: Result := Result + '\w';

    fontControlSprite:
      Begin
        Result := Result + '\p{';
        Repeat
          C := It.GetNext();
          If C = fontControlEnd Then
            Break;

          StringAppendChar(Result, C);
        Until False;
        Result := Result + '}';
      End;

    fontControlColor:
      Begin
        Result := Result + '\c{';
        Repeat
          C := It.GetNext();
          If C = fontControlEnd Then
            Break;

          StringAppendChar(Result, C);
        Until False;
        Result := Result + '}';
      End;

    Else
      StringAppendChar(Result, C);
    End;
  End;
End;

Function Font.SelectPage(Index, Slot: Integer):FontPage;
Begin
  If (Index<0) Or (Index>=_PageCount) Then
    Result := Nil
  Else
  Begin
    Result := _Pages[Index];
    Result._Texture.Bind(Slot);
  End;
End;

End.
