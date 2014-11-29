{
TERRA_Font
GraphicsManager for bitmap fonts created with AngelCodeFontTool
Fnt binary file only, supports multi-page fonts

History
  7/02/10   - Binary FNT loader and GraphicsManager

ToDo
  - Add support to unicode strings
  - Fix multi-page (only page 0 is used now)
  - Fix kerning
}

Unit TERRA_Font;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Resource, TERRA_IO, TERRA_Image, TERRA_Color, TERRA_Vector2D,
  TERRA_Math, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Texture, TERRA_SpriteManager,
  TERRA_ResourceManager, TERRA_Matrix, TERRA_Matrix2D, TERRA_Classes;


Const
  TabSize = 100;

  FontQuality = 1; // currently bugged

  DefaultFontPageWidth = 256;
  DefaultFontPageHeight = 512;

  gradientNone      = 0;
  gradientHorizontal= 1;
  gradientVertical  = 2;

  fontmode_Sprite   = 0;
  fontmode_Measure  = 1;
  fontmode_Offscreen=2;

Type
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

      Destructor Destroy; Override;

      Function GetAdvance(Next:Cardinal):Integer;
      Procedure AddKerning(Next:Cardinal; Ammount:SmallInt);

      Function GetImage():Image;
  End;

  FontPage = Class(TERRAObject)
    Protected
      _ID:Integer;
      _Texture:Texture;
      _Image:Image;
      _Font:Font;

      _OptimizedWidth:Cardinal;
      _OptimizedHeight:Cardinal;

      Procedure DrawGlyph(X,Y,Z:Single; Const Transform:Matrix2D; Scale:Single; Glyph:FontGlyph; Outline, A,B,C,D:Color; Clip:ClipRect; Italics:Boolean);

    Public
      Constructor Create(ID:Integer);
      Destructor Destroy; Override;

      Procedure SetImage(Source:Image);

      Property Texture:Texture Read _Texture;
  End;

  FontEffect = Record
    Effect:AnsiChar;
    Arg:AnsiString;
  End;

  FontRenderer = Object
    Protected
      _Text:AnsiString;
      _Mode:Integer;
      _Index:Integer;
      _Next:Cardinal;
      _Started:Boolean;

      _Font:Font;

      _StartPosition:Vector2D;
      _TargetPosition:Vector2D;

      _CurrentPosition:Vector2D;
      _CurrentGlyph:FontGlyph;

      _Layer:Single;

      _Color1:Color;
      _Color2:Color;

      _Effects:Array[0..15] Of FontEffect;
      _EffectCount:Integer;

      _Blink:Boolean;
      _WavyText:Boolean;
      _GradientMode:Integer;
      _Count:Cardinal;

      _DropShadow:Boolean;
      _Italics:Boolean;

      _RevealActive:Boolean;
      _RevealCount:Integer;
      _RevealDec:Integer;

      _Width:Single;
      _Height:Single;

      _MaxX:Single;
      _MaxY:Single;
      _AdvanceX:Single;
      _Scale:Single;

      _ClipRect:ClipRect;

      Function GetNextChar:Cardinal;
      Function GetNextArg:AnsiString;

      Procedure DoEffects();

    Public
      Procedure Start(S:AnsiString; Mode:Integer; X,Y, Layer, Scale:Single; MyFont:Font; MyClipRect:ClipRect; DropShadow:Boolean = False);
      Function RenderNext():Boolean;

      Procedure SetGradient(A,B:Color; Width, Height:Single; GradientMode:Integer);
      Procedure SetColor(FontColor:Color);

      Procedure GetColors(Var A,B,C,D:Color);

      Property Position:Vector2D Read _TargetPosition;
      Property Blink:Boolean Read _Blink;
      Property Glyph:FontGlyph Read _CurrentGlyph;
      Property Next:Cardinal Read _Next;

      Property MaxX:Single Read _MaxX;
      Property MaxY:Single Read _MaxY;

      Property Italics:Boolean Read _Italics;

      Property RevealCount:Integer Read _RevealCount Write _RevealCount;
  End;

  FontImageResolver = Function (Fnt:Font; Const ImageName:AnsiString):AnsiString; CDecl;

  FontGlyphFactory = Class
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
      BilinearFilter:Boolean;

      Function Load(Source:Stream):Boolean; Override;
      //Function Save(FileName:AnsiString):Boolean;

      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;
      Procedure OnContextLost; Override;

      Function AddPage():FontPage;
      Function GetPage(Index:Integer):FontPage;

      Function AddGlyph(ID:Cardinal; Source:Image; XOfs,YOfs:SmallInt; XAdvance:SmallInt = -1):FontGlyph; Overload;
      Function AddGlyph(ID:Cardinal; Const FileName:AnsiString; XOfs,YOfs:SmallInt; XAdvance:SmallInt = -1):FontGlyph; Overload;
      Function AddEmptyGlyph():FontGlyph;
      Function GetGlyph(ID:Cardinal; CreatedIfNeeded:Boolean = True):FontGlyph;
      Procedure SortGlyphs();
      Procedure CacheGlyphs(Const S:AnsiString);

      Procedure RecalculateMetrics();

      Class Function GetManager:Pointer; Override;

      Procedure DrawText(Const X,Y,Layer:Single; Const Text:AnsiString; Const Color:Color; Const Scale:Single = 1.0; Const DropShadow:Boolean=False; Const Clip:ClipRect = Nil);
      Procedure DrawTextWithTransform(Const X,Y,Layer:Single; Const Text:AnsiString; Const Color:Color; Const Transform:Matrix2D; Const Scale:Single = 1.0; Const DropShadow:Boolean=False; Const Clip:ClipRect = Nil);
      Procedure DrawTextWithOutline(Const X,Y,Layer:Single; Const Text:AnsiString; Const Color, Outline:Color; Const Transform:Matrix2D; Const Scale:Single = 1.0; Const DropShadow:Boolean=False; Const Clip:ClipRect = Nil);
      Procedure DrawTextWithGradient(X,Y,Layer:Single; Const Text:AnsiString; Const Color1, Color2, Outline:Color; Const Transform:Matrix2D; Const GradientMode:Integer; Scale:Single = 1.0; DropShadow:Boolean=False; Const Clip:ClipRect = Nil);

      Procedure DrawTextToImage(Target:Image; X,Y:Integer; Const Text:AnsiString; ForceBlend:Boolean = True);

      Function AutoWrapText(S:AnsiString; Width:Single; Scale:Single = 1.0):AnsiString;

      Function GetTextWidth(Const Text:AnsiString; Scale:Single = 1.0):Single;
      Function GetTextHeight(Const Text:AnsiString; Scale:Single = 1.0):Single;
      Function GetTextRect(Const Text:AnsiString; Scale:Single = 1.0):Vector2D;
      Function GetLength(Const Text:AnsiString):Integer;

      Procedure AddGlyphFactory(Factory:FontGlyphFactory; Scale:Single = 1.0);

      Property TextSize:Integer Read _TextSize;

      Property PageCount:Integer Read _PageCount;

      Property NewLineOffset:Single Read _AvgHeight Write _AvgHeight;
  End;

  FontStreamValidateFunction = Function(Source:Stream):Boolean;
  FontLoader = Function(Source:Stream; Font:Font):Boolean;

  FontClassInfo = Record
    Name:AnsiString;
    Validate:FontStreamValidateFunction;
    Loader:FontLoader;
  End;

  FontManager = Class(ResourceManager)
    Protected
      _DefaultFont:Font;

      Function GetDefaultFont: Font;

    Public
      Destructor Destroy; Override;

      Class Function Instance:FontManager;

      Function GetFont(Name:AnsiString; ValidateError:Boolean = True):Font;

      Property DefaultFont:Font Read GetDefaultFont;
   End;

  Function GetFontLoader(Source:Stream):FontLoader;
  Procedure RegisterFontFormat(Name:AnsiString; Validate:FontStreamValidateFunction; Loader:FontLoader);

Var
  _FontImageResolver:FontImageResolver;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Application, TERRA_Sort,
  TERRA_PNG, TERRA_Log, TERRA_FileUtils,
  TERRA_GraphicsManager, TERRA_FileManager, TERRA_Packer;

Var
  _FontExtensions:Array Of FontClassInfo;
  _FontExtensionCount:Integer;
  _FontManager_Instance:ApplicationObject;

Function DefaultFontImageResolver(Fnt:Font; Const ImageName:AnsiString):AnsiString; CDecl;
Begin
  Result := ImageName;
End;

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

Procedure RegisterFontFormat(Name:AnsiString; Validate:FontStreamValidateFunction; Loader:FontLoader);
Var
  I,N:Integer;
Begin
  Name := LowStr(Name);

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
    Name:AnsiString;
    TextSize:Integer;
  End;

Function SearchFontByNameAndSize(P:ListObject; UserData:Pointer):Boolean; CDecl;
Begin
  Result := (Resource(P).Name = PFontSearch(Userdata).Name) And (Font(P).TextSize = PFontSearch(Userdata).TextSize);
End;

Function FontManager.GetFont(Name:AnsiString; ValidateError:Boolean):Font;
Var
  FontName, FileName, S:AnsiString;
  I:Integer;
  Size:Integer;
  Params:FontSearch;
Begin
  Name := TrimLeft(TrimRight(Name));
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  I := PosRev('@', Name);
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
    Result._Name := UpStr(Name);
    Self.AddResource(Result);
  End Else
  If ValidateError Then
    RaiseError('Could not find font. ['+Name +']');
End;

Destructor FontManager.Destroy;
Begin
  Inherited;

  DestroyObject(@_DefaultFont);
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
  Src.Destroy();
  Img.Destroy();

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

Destructor FontGlyph.Destroy;
Begin
  If (Assigned(_Temp)) And (_Temp<>_Source) Then
    _Temp.Destroy();

  If (Assigned(_Source)) Then
    _Source.Destroy();
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

{ FontPage }
Constructor FontPage.Create(ID: Integer);
Begin
  Self._ID := ID;
End;

Destructor FontPage.Destroy;
Begin
  If Assigned(_Texture) Then
    _Texture.Destroy();

  If Assigned(_Image) Then
    _Image.Destroy();
End;

Procedure FontPage.DrawGlyph(X,Y,Z:Single; Const Transform:Matrix2D; Scale:Single; Glyph:FontGlyph; Outline, A,B,C,D:Color; Clip:ClipRect; Italics:Boolean);
Var
  S:Sprite;
Begin
  S := SpriteManager.Instance.AddSpriteWithOutline(X + Glyph.XOfs * Scale, Y + Glyph.YOfs* Scale, Z, _Texture, Outline, Nil, blendBlend, 1.0, Self._Font.BilinearFilter, True);
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
    _Texture := TERRA_Texture.Texture.New(Self._Font.Name+'_page'+IntTostring(Self._ID), Source);
  End Else
  If (_Texture.Width<>Source.Width) Or (_Texture.Height<>Source.Height) Then
  Begin
    _Texture.Destroy();
    _Texture := Nil;
  End;

  _Texture.MipMapped := False;
  _OptimizedWidth := 0;
  _OptimizedHeight := 0;

  If Assigned(_Image) Then
    _Image.Destroy();

  _Image := Image.Create(Source);
End;

{ FontRenderer }
Procedure FontRenderer.Start(S:AnsiString; Mode:Integer; X, Y, Layer, Scale: Single; MyFont:Font; MyClipRect:ClipRect; DropShadow:Boolean);
Begin
  _Font := MyFont;

  If (Assigned(_Font)) And (_Font._AvgHeight<=0) Then
    _Font.RecalculateMetrics();

  _Mode := Mode;
  _Index := 0;
  _DropShadow := DropShadow;
  _Text := S;
  _Layer := Layer;
  _StartPosition.X := X;
  _StartPosition.Y := Y;
  _Scale := Scale;
  _ClipRect := MyClipRect;

  _RevealCount := 99999;
  _Count := 0;
  _Italics := False;

  _EffectCount := 0;
  _WavyText := False;
  _Blink := False;

  _Width := 1.0;
  _Height := 1.0;

  _CurrentGlyph := Nil;
  _CurrentPosition := _StartPosition;

  _Color1 := ColorWhite;
  _Color2 := _Color1;

  _RevealDec := -1;
  _Started := True;

  _MaxX := X;
  _MaxY := Y;
  _Next := GetNextChar;
End;

Function FontRenderer.GetNextArg:AnsiString;
Var
  I:Integer;
Begin
  Result := Copy(_Text, (_Index+2), MaxInt);
  I := Pos('}', Result);
  Result := Copy(Result, 1, Pred(I));
  Inc(_Index, Length(Result) + 2);
End;

Function FontRenderer.GetNextChar:Cardinal;
Var
  Len:Integer;
  Before, After:AnsiChar;
Begin
  Result := 0;
  Inc(_Index);

  Len := Length(_Text);
  If (_Index>Len) Then
  Begin
    //IntToString(_Index+Len);
    Exit;
  End;

  If (_Index>1) Then
    Before := _Text[Pred(_Index)]
  Else
    Before := #0;

  If (_Index<Len) Then
    After := _Text[Succ(_Index)]
  Else
    After := #0;

  If (_Text[_Index]='\') Then
  Begin
    If (After='\') Then
    Begin
      Result := GetNextChar();
      Exit;
    End Else
    If (Before<>'\') Then
    Begin
      Inc(_Index);
      If (_Index<Length(_Text)) Then
      Begin
        _Effects[_EffectCount].Effect := UpCase(_Text[_Index]);

        If (_Index<Length(_Text)) And (_Text[Succ(_Index)]='{') Then
        _Effects[_EffectCount].Arg := GetNextArg()
        Else
          _Effects[_EffectCount].Arg := '';

        Inc(_EffectCount);
      End;

      Result := GetNextChar();
      Exit;
    End;
  End;

  Result := Byte(_Text[_Index]);

  If (Result=255) Then
  Begin
    Inc(_Index);
    If (_Index<=Len) Then
    Begin
      Result := Byte(_Text[_Index]);
      Inc(_Index);
      If (_Index<=Len) Then
        Result := Result * 256 + Byte(_Text[_Index]);
    End;
  End;
End;

Function FontRenderer.RenderNext(): Boolean;
Var
  ID, K:Cardinal;
  H:Single;
Begin
  ID := _Next;
  _Next := GetNextChar;

  {If (ID=61443) Then
    IntToString(2);}

  //Log(logDebug,'AdWall', 'Nextchar: ' + IntToString(_Next));

  If (ID=0) Or (_Font = Nil) Then
  Begin
    Result := False;
    DoEffects();
    Exit;
  End;

  _CurrentGlyph := _Font.GetGlyph(ID);
  If Not Assigned(_CurrentGlyph) Then
  Begin
    Result := RenderNext;
    Exit;
  End;

  Inc(_Count);
  Dec(_RevealCount);

  If (_RevealCount<=0) Then
  Begin
    If (_RevealDec<0) Then
      _RevealDec := Succ(_Color1.A) Shr 2;

    If (_Color1.A > _RevealDec) Then
      Dec(_Color1.A, _RevealDec)
    Else
    Begin
      Result := False;
      Exit;
    End;
  End;

  If (_Started) Then
    DoEffects();

  //Log(logDebug,'AdWall', 'Calculating advance');
  _TargetPosition := _CurrentPosition;
  _AdvanceX := _CurrentGlyph.GetAdvance(_Next) * _Scale;
  _CurrentPosition.X := _CurrentPosition.X + _AdvanceX;

  //Log(logDebug,'AdWall', 'Testing effects');
  If (_Started) Then
    _Started := False;

  If (Not _Started) Then
    DoEffects();

  //Log(logDebug,'AdWall', 'Testing wavy test');
  If (_WavyText) Then
  Begin
    K := (GetTime Div 3);
    K := K + _Count * 8;
    _TargetPosition.Y := _TargetPosition.Y + Sin((K Mod 360) * RAD) * 4.5;
  End;

  If (_CurrentPosition.X >_MaxX) Then
    _MaxX := _CurrentPosition.X;

  H := _CurrentPosition.Y + _CurrentGlyph.Height * _Scale;
  If (H > _MaxY) Then
    _MaxY := H;

  //Log(logDebug,'AdWall', 'Finisedh ok');
  Result := True;
End;

Procedure FontRenderer.SetColor(FontColor: Color);
Begin
  _Color1 := FontColor;
  _Color2 := FontColor;
  _GradientMode := gradientNone;
End;

Procedure FontRenderer.SetGradient(A,B:Color; Width, Height:Single; GradientMode:Integer);
Begin
  If (Width<=0.0) Then
    Width := 1.0;

  If (Height<=0.0) Then
    Height := 1.0;

  _Width := Width;
  _Height := Height;

  _Color1 := A;
  _Color2 := B;
  _GradientMode := GradientMode;
End;

Procedure FontRenderer.GetColors(Var A, B, C, D:Color);
Var
  N, Delta1, Delta2:Single;
Begin
  Case _GradientMode Of
    gradientNone:
      Begin
        A := _Color1;
        B := _Color1;
        C := _Color1;
        D := _Color1;
      End;

    gradientHorizontal:
      Begin
        N := Self._CurrentPosition.X - Self._StartPosition.X;
        Delta1 := N / _Width;
        Delta2 := (N + _AdvanceX) / _Width;

        If (Delta2>1.0) Then
        Begin
          Delta2 := 1.0;
          //FloatToString(N);
        End;

        A := ColorMix(_Color2, _Color1, Delta1);
        B := ColorMix(_Color2, _Color1, Delta2);
        C := ColorMix(_Color2, _Color1, Delta1);
        D := ColorMix(_Color2, _Color1, Delta2);
      End;

    gradientVertical:
      Begin
        N := Self._CurrentPosition.Y  - Self._StartPosition.Y;
        Delta1 := N / _Height;
        Delta2 := (N + Glyph.Height * _Scale) / _Height;

        If (Delta2>1.0) Then
          Delta2 := 1.0;

        A := ColorMix(_Color2, _Color1, Delta1);
        B := ColorMix(_Color2, _Color1, Delta1);
        C := ColorMix(_Color2, _Color1, Delta2);
        D := ColorMix(_Color2, _Color1, Delta2);
      End;
  End;
End;

Procedure FontRenderer.DoEffects;
Var
  I:Integer;
  Tex:Texture;
  S:Sprite;
  SS:AnsiString;
  C:Color;
Begin
  For I:=0 To Pred(_EffectCount) Do
  Case _Effects[I].Effect Of
  'C':
      If (_Effects[I].Arg<>'') Then
      Begin
        C := ColorCreate(_Effects[I].Arg);
        Self._Color1 := C;
        Self._Color2 := C;
      End;

  'P':
      If (_Effects[I].Arg<>'') Then
      Begin
        SS := _Effects[I].Arg;
        If (SS[1]='#') Then
        Begin
          SS := _FontImageResolver(Self._Font, Copy(SS, 2, MaxInt));
        End;

        Tex := TextureManager.Instance.GetTexture(SS);
        If Assigned(Tex) Then
        Begin
          Tex.Prefetch();

          _CurrentPosition.X := _CurrentPosition.X + 4;

          If (_Mode = fontmode_Sprite) Then
          Begin
            If _DropShadow Then
            Begin
              S := SpriteManager.Instance.AddSprite(_CurrentPosition.X - 1, _CurrentPosition.Y - Tex.Height + 1, Self._Layer-1, Tex);
              S.SetColor(ColorGrey(0, _Color1.A));
              S.ClipRect := Self._ClipRect;
            End;

            S := SpriteManager.Instance.AddSprite(_CurrentPosition.X, _CurrentPosition.Y - Tex.Height, Self._Layer, Tex);
            S.SetColor(ColorGrey(255, _Color1.A));
            S.ClipRect := Self._ClipRect;
          End;

          _CurrentPosition.X := _CurrentPosition.X + Tex.Width + 4;

          If (_CurrentPosition.X >_MaxX) Then
            _MaxX := _CurrentPosition.X;

          If (_MaxY<=0) Then
            _MaxY := Tex.Height;
        End;
      End;

      '#':  Begin
            // do nothing
            End;

      'W':  Begin
              _WavyText := Not _WavyText;
            End;

      'I':  Begin
              _Italics := Not _Italics;
            End;

      'B':  Begin
              If (_RevealActive) Then
                _Blink := False
              Else
                _Blink := Not _Blink;
            End;

      'T':  Begin
              _CurrentPosition.X := (Trunc(_CurrentPosition.X/TabSize)+1)*TabSize*_Scale;
            End;

      'N':  Begin
              _CurrentPosition.X := _StartPosition.X;
              If Assigned(_Font) Then
                _CurrentPosition.Y := _CurrentPosition.Y + 4.0 + _Font._AvgHeight * _Scale;
            End;
  End;

  _EffectCount := 0;
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
        If (_Glyphs[Mid+1].ID = ID) Then // hack
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
    _Factory.Destroy();
  End;
  _Factory := Nil;

  For I:=0 To Pred(_PageCount) Do
    _Pages[I].Destroy();
    
  _PageCount := 0;
  SetLength(_Pages, 0);

  For I:=0 To Pred(_GlyphCount) Do
    _Glyphs[I].Destroy();
    
  _GlyphCount := 0;
  SetLength(_Glyphs, 0);

  Result := True;
End;

Function Font.Update:Boolean;
Begin
  Inherited Update();
	Result := True;
End;

Function Font.GetPage(Index:Integer):FontPage;
Begin
  If (Index<0) Or (Index>=_PageCount) Then
    Result := Nil
  Else
    Result := _Pages[Index];
End;

Function Font.GetTextWidth(Const Text:AnsiString; Scale:Single):Single;
Begin
  Result := GetTextRect(Text, Scale).X ;
End;

Function Font.GetTextHeight(Const Text:AnsiString; Scale:Single):Single;
Begin
  Result := GetTextRect(Text, Scale).Y ;
End;

Function Font.GetTextRect(Const Text:AnsiString; Scale:Single):Vector2D;
Var
  FR:FontRenderer;
Begin
  If (Not Self.IsReady) Then
  Begin
    Result.X := 0;
    Result.Y := 0;
  End;

  //Log(logDebug,'AdWall', 'Starting gettextrect');
  FR.Start(Text, fontmode_Measure, 0, 0, -1, Scale, Self, Nil);
  While (FR.RenderNext()) Do;

  //Log(logDebug,'AdWall', 'Finished textrect');
  Result.X := FR.MaxX / FontQuality;
  Result.Y := FR.MaxY / FontQuality;
End;

Function Font.GetLength(Const Text:AnsiString):Integer;
Var
  FR:FontRenderer;
Begin
  Result := 0;
  FR.Start(Text, fontmode_Measure, 0, 0, -1,  1.0, Self, Nil);
  While (FR.RenderNext) Do
    Inc(Result);
End;

Function GetNextWord(Var S:AnsiString):AnsiString;
Var
  I,J:Integer;
Begin
  S:=TrimLeft(S);
  If S='' Then
  Begin
    Result:='';
    Exit;
  End;

  I := Pos(' ',S);
  J := Pos('\n',S);
  If (J>1) And (J<I) Then
  Begin
    Result := Copy(S,1,Pred(J));
    S := Copy(S,J,MaxInt);
  End Else
  If I<=0 Then
  Begin
    Result:=S;
    S:='';
  End Else
  Begin
    Result:=Copy(S,1,Pred(I));
    S:=Copy(S,Succ(I),MaxInt);
  End;
  S:=TrimLeft(S);
End;

Function Font.AutoWrapText(S:AnsiString; Width, Scale: Single):AnsiString;
Var
  Temp, Temp2, S2:AnsiString;
  I:Integer;
  X:Single;
Begin
  //Log(logDebug,'AdWall', 'Starting autowrap');
  If (Self.Status<>rsReady) Then
    Self.Prefetch();

  //Log(logDebug,'AdWall', 'Isready ok');

  Result := '';
  Temp := '';
  While (S<>'') Do
  Begin
    S2 := GetNextWord(S);
    //Log(logDebug,'AdWall', 'Next word: '+S2);
    Temp2 := Temp;
    Temp := Temp + S2;

    If (S<>'') Then
      Temp := Temp + ' ';

    X := Self.GetTextWidth(Temp, Scale);
    //Log(logDebug,'AdWall', 'TexWidth ok');
    If (X>Width) Then
    Begin
      Temp := Temp2;
      Result := Result + Temp + '\n';
      Temp := S2+' ';
    End;
  End;

  Result := Result + Temp;
    //Log(logDebug,'AdWall', 'Autowrap done!');
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

Procedure Font.CacheGlyphs(Const S:AnsiString);
Begin
  Self.GetTextRect(S, 1.0);
End;

Function Font.AddEmptyGlyph:FontGlyph;
Begin
  Inc(_GlyphCount);
  SetLength(_Glyphs, _GlyphCount);
  Result := FontGlyph.Create();
  Result._Font := Self;
  _Glyphs[Pred(_GlyphCount)] := Result;
End;

Function Font.AddGlyph(ID: Cardinal; Const FileName:AnsiString; XOfs, YOfs, XAdvance: SmallInt):FontGlyph;
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
  Source.Destroy();
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
  S:AnsiString;

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
  Log(logDebug,'Font', 'Updating font: '+_Name);

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
      _Glyphs[I]._Source.Destroy();
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
      _Pages[K]._Image.Destroy;
      _Pages[K]._Image := Image.Create(DefaultFontPageWidth, DefaultFontPageHeight);
    End;

    If (Application.Instance<>Nil) Then
    Begin
      If (_Pages[K]._Texture = Nil) Then
        _Pages[K]._Texture := Texture.New(_Name+'_page'+IntToString(K), _Pages[K]._Image.Width, _Pages[K]._Image.Height);

      _Pages[K]._Texture.IsReady();
    End;

    Temp := _Pages[K]._Image;
    Temp.Process(IMP_FillColor, ColorGrey(255, 0));
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
        //Pics[I].Destroy();
        Pics[I] := Nil;
      End;
    End;

    //Temp.Save('koo_'+IntToString(K)+'.png');

    If (Application.Instance<>Nil) Then
    Begin
      _Pages[K]._Texture.UpdateRect(Temp, 0, 0);
    End;

    Packer.Destroy;

    Inc(K);
  End;

  RecalculateMetrics();
End;

Procedure Font.DrawText(Const X,Y, Layer:Single; Const Text:AnsiString; Const Color:Color; Const Scale:Single; Const DropShadow:Boolean; Const Clip:ClipRect);
Begin
  Self.DrawTextWithGradient(X, Y, Layer, Text, Color, Color, ColorNull, MatrixIdentity2D, gradientNone, Scale, DropShadow, Clip);
End;

Procedure Font.DrawTextWithTransform(Const X,Y,Layer:Single; Const Text:AnsiString; Const Color:Color; Const Transform:Matrix2D; Const Scale:Single = 1.0; Const DropShadow:Boolean=False; Const Clip:ClipRect = Nil);
Begin
  Self.DrawTextWithGradient(X, Y, Layer, Text, Color, Color, ColorNull, Transform, gradientNone, Scale, DropShadow, Clip);
End;

Procedure Font.DrawTextWithOutline(Const X,Y,Layer:Single; Const Text:AnsiString; Const Color, Outline:Color; Const Transform:Matrix2D; Const Scale:Single = 1.0; Const DropShadow:Boolean=False; Const Clip:ClipRect = Nil);
Begin
  Self.DrawTextWithGradient(X, Y, Layer, Text, Color, Color, Outline, Transform, gradientNone, Scale, DropShadow, Clip);
End;

Procedure Font.DrawTextWithGradient(X,Y,Layer:Single; Const Text:AnsiString;
                                    Const Color1, Color2, Outline:Color;
                                    Const Transform:Matrix2D; Const GradientMode:Integer;
                                    Scale:Single = 1.0; DropShadow:Boolean=False; Const Clip:ClipRect = Nil);
Var
  Glyph:FontGlyph;
  FR:FontRenderer;
  Alpha:Integer;
  DropShadowColor:TERRA_Color.Color;
  Projection:Matrix;
  A,B,C,D:Color;
  Size:Vector2D;
  I:Integer;
Begin
  If (Not Self.IsReady) Then
    Exit;

  If (_NeedsRebuild) Then
    Self.Rebuild();

  Scale := Scale / FontQuality;

  Alpha := IntMin(Color1.A, Color2.A);
  Alpha := Alpha - 55;
  If (Alpha<0) Then
    Alpha := 0;

  If (Alpha<=0) Then
    DropShadow := False
  Else
    DropShadowColor := ColorGrey(0, Alpha);

  Layer := -(99-Layer);

  Glyph := Self.GetGlyph(Ord('E'));
  If Assigned(Glyph) Then
    Y := Y - Glyph.YOfs * Scale;

  Size := Self.GetTextRect(Text, Scale);

  For I:=0 To Pred(_PageCount) Do
  Begin
    _Pages[I]._Texture.Bind(0);

    FR.Start(Text, fontmode_Sprite, X, Y, Layer, Scale, Self, Clip, DropShadow);
    If (GradientMode = gradientNone) Then
      FR.SetColor(Color1)
    Else
      FR.SetGradient(Color1, Color2, Size.X, Size.Y, GradientMode);

    While (FR.RenderNext()) Do
    Begin
      If (FR.Glyph = Nil) Then
      Begin
        IntToString(FR._Index);
        Continue;
      End;

      If (FR.Glyph.Page<>I) Or (FR.Glyph._Temp<>Nil) Then
        Continue;

      FR.GetColors(A,B,C,D);

      If (DropShadow) Then
        _Pages[I].DrawGlyph(FR.Position.X-Scale, FR.Position.Y+Scale, Layer-1, Transform, Scale, FR.Glyph, DropShadowColor, DropShadowColor, DropShadowColor, DropShadowColor, DropShadowColor, Clip, FR.Italics);

      _Pages[I].DrawGlyph(FR.Position.X, FR.Position.Y, Layer, Transform, Scale, FR.Glyph, Outline, A,B,C,D, Clip, FR.Italics);
    End;
  End;
End;

Procedure Font.DrawTextToImage(Target:Image; X, Y: Integer; const Text:AnsiString; ForceBlend:Boolean);
Var
  Next:Cardinal;
  Glyph:FontGlyph;
  FR:FontRenderer;
  Alpha:Integer;
  DropShadowColor:TERRA_Color.Color;
  Projection:Matrix;
  A,B,C,D:Color;
  I:Integer;
  GG:Image;
Begin
  If Target = Nil Then
    Exit;

  Self.Prefetch();

  If (Not Self.IsReady) Then
    Exit;

  If (_NeedsRebuild) Then
    Self.Rebuild();

  {If (Length(_Pages)<=0) Then
    Exit;}

  Glyph := Self.GetGlyph(Ord('E'));
  If Assigned(Glyph) Then
    Y := Trunc(Y - Glyph.YOfs);

  Self.GetTextRect(Text, 1.0); // TODO CHECK REALLY NECESSARY HERE?

  FR.Start(Text, fontmode_Offscreen, X, Y, 0, 1.0, Self, Nil);
  FR.SetColor(ColorWhite);

  While (FR.RenderNext()) Do
  Begin
    If (FR.Glyph = Nil) Then
      Continue;

    GG := FR.Glyph.GetImage();
    If (GG = Nil) Then
    Begin
    //  IntToString(2);
      Continue;
    End;

    Target.BlitWithAlpha(Trunc(FR.Position.X + FR.Glyph.XOfs), Trunc(FR.Position.Y + FR.Glyph.YOfs), 0, 0, GG.Width, GG.Height, GG, ForceBlend);
  End;
End;

Procedure Font.OnContextLost;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_PageCount) Do
  Begin
    If Assigned(_Pages[I]._Texture) Then
    Begin
      _Pages[I]._Texture.Destroy();
      _Pages[I]._Texture := Nil;
    End;
  End;

  Self._ContextID := Application.Instance.ContextID;
  _NeedsRebuild := True;
End;

Initialization
  _FontImageResolver := DefaultFontImageResolver;
  RegisterResourceClass(Font);
End.
