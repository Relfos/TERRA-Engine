Unit TERRA_FontRenderer;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Color, TERRA_Vector2D, TERRA_Matrix3x3, TERRA_Matrix4x4,
  TERRA_Resource, TERRA_Texture, TERRA_Font, TERRA_Sprite, TERRA_ClipRect, TERRA_Image, TERRA_Viewport;

Const
  fontEffectBegin = '[';
  fontEffectEnd = ']';

  MaxStackedFontEffects = 16;

Type
  TERRAFontCharStyle = Record
    WavyText:Boolean;
    Bold:Boolean;
    Italics:Boolean;
    IsLink:Boolean;
    A, B, C, D:ColorRGBA;
  End;

  TERRAFontCommand = Record
    Value:TERRAChar;
    X:Single;
    Y:Single;
    Width:Single;
    Height:Single;
    Style:TERRAFontCharStyle;
    Glyph:FontGlyph;
  End;

  TERRAFontRenderer = Class(TERRAObject)
    Protected
      _InitColor1, _InitColor2:ColorRGBA;
      _Outline:ColorRGBA;
      _Glow:ColorRGBA;

      _Transform:Matrix3x3;

      _ClipRect:TERRAClipRect;

      _Mode:Integer;

      _Font:TERRAFont;
      _FontOffset:Single;

      _ColorA:ColorRGBA;
      _ColorB:ColorRGBA;
      _ColorC:ColorRGBA;
      _ColorD:ColorRGBA;

      _Width:Single;
      _Height:Single;

      _MaxWidth:Single;
      _MaxHeight:Single;

      _MaxX:Single;
      _MaxY:Single;

      _NewLineOffset:Single;

      _AutoWrap:Boolean;

      _View:TERRAViewport;

      _LastAllocFrame:Cardinal;
      _Sprites:Array Of FontSprite;
      _SpriteCount:Integer;

      _Size:Single;
      _Scale:Single;

      _CharList:Array Of TERRAFontCommand;
      _CharCount:Integer;

      Function AllocSprite():FontSprite;

      Procedure DrawSprite(Const TextureName:TERRAString);
      Function ResolveTexture(Const TextureName:TERRAString):TERRATexture; Virtual;
      Procedure TransformSprite(S:TERRASprite); Virtual;

      Procedure QueueChar(Const Value:TERRAChar; Const X, Y:Single);

      Function DrawGlyph(View:TERRAViewport; X,Y:Single; Glyph:FontGlyph; A,B,C,D:ColorRGBA; Italics:Boolean; Var DestSprite:FontSprite):Boolean;

    Public
      Constructor Create();
      Procedure Release; Override;

      Function Reset():TERRAFontRenderer;

      Function SetColor(Const Value:ColorRGBA):TERRAFontRenderer;
      Function SetVerticalGradient(Const Top, Bottom:ColorRGBA):TERRAFontRenderer;
      Function SetHorizontalGradient(Const Left, Right:ColorRGBA):TERRAFontRenderer;

      Function SetOutline(Const Value:ColorRGBA):TERRAFontRenderer;
      Function SetGlow(Const Value:ColorRGBA):TERRAFontRenderer;

      Function SetAutoWrap(Const Enabled:Boolean):TERRAFontRenderer;

      Function SetFont(Const TargetFont:TERRAFont):TERRAFontRenderer;

      Function SetSize(Const Size:Single):TERRAFontRenderer;

      Function SetAreaLimit(Const Width, Height:Single):TERRAFontRenderer;

      Function SetTransform(Const Transform:Matrix3x3):TERRAFontRenderer;
      ///Function  SetDropShadow(Const DropShadowColor:ColorRGBA):TERRAFontRenderer;

      Function  SetClipRect(Const Clip:TERRAClipRect):TERRAFontRenderer;

      Function Compile(Const S:TERRAString; Mode:Integer; X,Y, Layer:Single):Boolean;

      Function DrawText(View:TERRAViewport; X,Y,Layer:Single; Const Text:TERRAString):TERRAFontRenderer;
      Function DrawTextToSprite(View:TERRAViewport; X,Y,Layer:Single; Const Text:TERRAString; Var DestSprite:FontSprite):TERRAFontRenderer;
      //Function DrawTextToImage(Target:TERRAImage; X,Y:Integer; Const Text:TERRAString; ForceBlend:Boolean = True):TERRAFontRenderer;

      Procedure GetColors(Const U, V:Single; Const CommandID:Integer);

      Function GetTextWidth(Const Text:TERRAString):Single;
      Function GetTextHeight(Const Text:TERRAString):Single;
      Function GetTextRect(Const Text:TERRAString ):Vector2D;
      Function GetLength(Const Text:TERRAString):Integer;

      Property MaxX:Single Read _MaxX;
      Property MaxY:Single Read _MaxY;

      Property NewLineOffset:Single Read _NewLineOffset Write _NewLineOffset;

      Property Font:TERRAFont Read _Font;
  End;

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_EngineManager, TERRA_GraphicsManager, TERRA_Math, TERRA_DebugDraw, TERRA_FontManager, TERRA_TextureAtlas;

{ FontRenderer }
Constructor TERRAFontRenderer.Create;
Begin
  Reset();
End;

Function TERRAFontRenderer.Reset():TERRAFontRenderer;
Begin
  SetColor(ColorWhite);
  SetTransform(Matrix3x3_Identity);
  SetSize(DefaultFontSize);
  Result := Self;
End;

Procedure TERRAFontRenderer.QueueChar(Const Value:TERRAChar; Const X, Y:Single);
Begin
  If (Length(_CharList) <= _CharCount) Then
    SetLength(_CharList, Succ(_CharCount));

  _CharList[_CharCount].Value := Value;
  _CharList[_CharCount].X := X;
  _CharList[_CharCount].Y := Y;
  Inc(_CharCount);
End;

Function TERRAFontRenderer.Compile(Const S:TERRAString; Mode:Integer; X,Y, Layer:Single):Boolean;
Var
  It:StringIterator;
  C, TempChar, NextChar:TERRAChar;

  WasWord, IsWord:Boolean;

  State:StringIteratorState;

  Glyph:FontGlyph;

  StartX, StartY, AdvanceX, TargetHeight, H:Single;
  N, K:Integer;

  Tag, Arg:TERRAString;
  ArgMode, Active:Boolean;

  Style:TERRAFontCharStyle;

  SkipSpaces:Boolean;

  Procedure ApplyLineBreak();
  Begin
    X := StartX;
    Y := Y + _NewLineOffset + (_Font.NewLineOffset + FontPadding * 2) * FontInvScale *  _Scale;

    SkipSpaces := True;
  End;
Begin
  Result := False;

  If (_Font = Nil) Then
    SetFont(Engine.Fonts.DefaultFont);

  If (_Font = Nil) Or (Not _Font.IsReady()) Then
    Exit;

  _Mode := Mode;

  _CharCount := 0;

  Style.Italics := False;
  Style.WavyText := False;
  Style.IsLink := False;

  _Width := 1.0;
  _Height := 1.0;

  _MaxX := X;
  _MaxY := Y;

  StartX := X;
  StartY := Y;
  WasWord := False;
  SkipSpaces := False;

  StringCreateIterator(S, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();

    If (C = fontEffectBegin) Then
    Begin
      Tag := '';
      Arg := '';
      Active := True;
      ArgMode := False;
      While It.HasNext() Do
      Begin
        C := It.GetNext();
        If (C = fontEffectEnd) Then
          Break
        Else
        If (C = '/') And (Tag ='') Then
          Active := False
        Else
        If (C = '=') Then
          ArgMode := True
        Else
        If (ArgMode) Then
          StringAppendChar(Arg, C)
        Else
          StringAppendChar(Tag, C);
     End;

      If (Tag = 'i') Then
      Begin
        Style.Italics := Active;
      End Else
      If (Tag = 'b') Then
      Begin
        Style.Bold := Active;
      End Else
      If (Tag = 'w') Then
      Begin
        Style.WavyText := Active;
      End Else
      If (Tag = 'color') Then
      Begin
        Self.SetColor(ColorCreateFromString(Arg));
      End Else
      If (Tag = 'img') Then
      Begin
        //AddCommand(fontCommand_Image, Active, Arg);
      End Else
      If (Tag = 'url') Then
      Begin
        Style.IsLink := Active;

        If Style.IsLink Then
          Self.SetColor(ColorCreate(128, 128, 255))
        Else
          Self.SetColor(ColorWhite);
      End Else
        Log(logWarning, 'Font', 'Unknown font effect tag: '+Tag);

      Continue;
    End;

    If (C = NewLineChar) Then
    Begin
      ApplyLineBreak();
      Continue;
    End;

    IsWord := (CharIsAlphaNumeric(C)) Or (CharIsPunctuation(C));
    If (_AutoWrap) And (Self._MaxWidth>0) And ((Not IsWord) Or ((IsWord) And (Not WasWord))) Then
    Begin
      It.SaveState(State);
      TempChar := C;
      AdvanceX := 0.0;
      While (It.HasNext) Do
      Begin
        NextChar := It.GetNext();
        Glyph := _Font.GetGlyph(C);

        If Assigned(Glyph) Then
          AdvanceX := AdvanceX + Glyph.GetAdvance(NextChar) * FontInvScale * _Scale;

        C := NextChar;

        If (Not CharIsAlphaNumeric(C)) Then
          Break;
      End;

      C := TempChar;
      It.RestoreState(State);

      If  (X + AdvanceX >= StartX+  Self._MaxWidth) Then
        ApplyLineBreak();
    End;

    WasWord := IsWord;

    Glyph := _Font.GetGlyph(C);
    If (Glyph = Nil) Then
      Continue;

    AdvanceX := Glyph.GetAdvance(NextChar) * FontInvScale * _Scale;


    TargetHeight := (Glyph.Height + FontPadding) * FontInvScale * _Scale;

    N := _CharCount;
    QueueChar(C, X, Y);
    _CharList[N].Width := AdvanceX;
    _CharList[N].Height := TargetHeight;
    _CharList[N].Glyph := Glyph;
    _CharList[N].Style := Style;

    //GetColors((X-StartX)/MaxWidth, (Y-StartY)/MaxHeight, A,B,C,D);
    GetColors(0, 0, N);

    If (SkipSpaces) Then
    Begin
      If (C = #32) Then
        AdvanceX := 0
      Else
        SkipSpaces := False;
    End;

    If (X >_MaxX) Then
      _MaxX := X;

    H := Y + TargetHeight;
    If (H > _MaxY) Then
      _MaxY := H;

    // don't wast time rendering spaces...
    (*If (C = #32) Then
      _CurrentGlyph := Nil;*)

    X := X + AdvanceX;

  End;

  Result := True;
End;

Function TERRAFontRenderer.SetColor(Const Value:ColorRGBA):TERRAFontRenderer;
Begin
  _ColorA := Value;
  _ColorB := Value;
  _ColorC := Value;
  _ColorD := Value;
  Result := Self;
End;

Function TERRAFontRenderer.SetVerticalGradient(Const Top, Bottom:ColorRGBA):TERRAFontRenderer;
Begin
  _ColorA := Top;
  _ColorB := Bottom;
  _ColorC := Top;
  _ColorD := Bottom;
  Result := Self;
End;

Function TERRAFontRenderer.SetHorizontalGradient(Const Left, Right:ColorRGBA):TERRAFontRenderer;
Begin
  _ColorA := Left;
  _ColorB := Right;
  _ColorC := Left;
  _ColorD := Right;
  Result := Self;
End;

Procedure TERRAFontRenderer.GetColors(Const U, V:Single; Const CommandID:Integer);
Var
  A, B, C, D:ColorRGBA;
Begin
  A := ColorMix(_ColorA, _ColorB, U);
  B := ColorMix(_ColorA, _ColorB, U);
  C := ColorMix(_ColorA, _ColorB, V);
  D := ColorMix(_ColorA, _ColorB, V);

  _CharList[CommandID].Style.A := A;
  _CharList[CommandID].Style.B := B;
  _CharList[CommandID].Style.C := C;
  _CharList[CommandID].Style.D := D;
End;

Function TERRAFontRenderer.GetTextWidth(Const Text:TERRAString):Single;
Begin
  Result := GetTextRect(Text).X ;
End;

Function TERRAFontRenderer.GetTextHeight(Const Text:TERRAString):Single;
Begin
  Result := GetTextRect(Text).Y ;
End;

Function TERRAFontRenderer.GetTextRect(Const Text:TERRAString):Vector2D;
Begin
  If (_Font = Nil) Or (Not _Font.IsReady) Then
  Begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  End;

  //Log(logDebug,'AdWall', 'Starting gettextrect');
  Compile(Text, fontmode_Measure, 0, 0, -1);

  //Log(logDebug,'AdWall', 'Finished textrect');
  Result.X := Self.MaxX;
  Result.Y := Self.MaxY;
End;

Function TERRAFontRenderer.GetLength(Const Text:TERRAString):Integer;
Begin
  Self.Compile(Text, fontmode_Measure, 0, 0, -1);
  Result := Self._CharCount;
End;

(*Function TERRAFontRenderer.AutoWrapText(Const Text:TERRAString; Width, Scale: Single):TERRAString;
Var
  Temp, Temp2, S, S2:TERRAString;
  I:Integer;
  X:Single;
  Separator:TERRAChar;
Begin
  If (_Font =  Nil) Then
  Begin
    Result := Text;
    Exit;
  End;

  S := ConvertFontCodes(Text);

  //Log(logDebug,'AdWall', 'Starting autowrap');
  If (_Font.Status<>rsReady) Then
    _Font.Prefetch();

  //Log(logDebug,'AdWall', 'Isready ok');

  Result := '';
  Temp := '';
  While (S<>'') Do
  Begin
    S2 := StringExtractNextWord(S, Separator);
    //Log(logDebug,'AdWall', 'Next word: '+S2);
    Temp2 := Temp;
    Temp := Temp + S2;

    If (Separator<>NullChar) Then
    Begin
      StringAppendChar(Temp, Separator);
    End;

    X := Self.GetTextWidth(Temp, Scale);
    //Log(logDebug,'AdWall', 'TexWidth ok');
    If (X>Width) Then
    Begin
      Temp := Temp2;
      Result := Result + Temp;

      Temp := S2;

      If Separator <> NewLineChar Then
        StringAppendChar(Result, NewLineChar);
    End;
  End;

  Result := Result + Temp;
End;*)

Function TERRAFontRenderer.DrawText(View:TERRAViewport; X,Y,Layer:Single; Const Text:TERRAString):TERRAFontRenderer;
Var
  Dest:FontSprite;
Begin
  If (_LastAllocFrame <> Engine.Graphics.FrameID) Then
    _SpriteCount := 0;

  Dest := Self.AllocSprite();
  Dest.Flags := Dest.Flags Or Sprite_Font;
  Result := DrawTextToSprite(View, X,Y,Layer, Text, Dest);
  View.SpriteRenderer.QueueSprite(Dest);
End;

Function TERRAFontRenderer.DrawTextToSprite(View:TERRAViewport; X,Y,Layer:Single; Const Text:TERRAString; Var DestSprite:FontSprite):TERRAFontRenderer;
Var
  Size:Vector2D;
  I:Integer;
  Color:ColorRGBA;
  TargetX, TargetY:Single;
  K:Integer;
Begin
  Result := Self;

  If (DestSprite = Nil) Then
    Exit;
    //DestSprite := FontSprite.Create();

  If (_Font = Nil) Or (Not _Font.IsReady()) Then
    Exit;

  DestSprite.Clear();

  Self._View := View;

  Y := Y - _FontOffset * FontInvScale;

  Size := Self.GetTextRect(Text);

  DestSprite.Flags := DestSprite.Flags Or Sprite_Font;
  DestSprite.Layer := Layer;
  DestSprite.SetTransform(_Transform);
  DestSprite.ClipRect := _ClipRect;
  DestSprite.Outline := _Outline;
  DestSprite.Glow := _Glow;
  DestSprite.Smoothing := (2.5 / _Scale)/16.0;

  Compile(Text, fontmode_Sprite, X, Y, Layer);

  For I:=0 To Pred(_CharCount) Do
  Begin
    TargetX := _CharList[I].X;
    TargetY := _CharList[I].Y;

    If (_CharList[I].Style.WavyText) Then
    Begin
      K := (Application.GetTime() Div 3);
      K := K + I* 8;
      TargetY := TargetY + Sin((K Mod 360) * RAD) * 4.5;
    End;


    DrawGlyph(View, TargetX, TargetY, _CharList[I].Glyph, _CharList[I].Style.A, _CharList[I].Style.B, _CharList[I].Style.C, _CharList[I].Style.D, _CharList[I].Style.Italics, DestSprite);
  End;

  //DrawClipRect(View, _ClipRect, ColorYellow);
End;

Function TERRAFontRenderer.DrawGlyph(View:TERRAViewport; X,Y:Single; Glyph:FontGlyph; A,B,C,D:ColorRGBA; Italics:Boolean; Var DestSprite:FontSprite):Boolean;
Var
  Item:TextureAtlasItem;
  Target:FontSprite;
  Tex:TERRATexture;
  Skew:Single;
Begin
  Result := False;

  If DestSprite = Nil Then
    Exit;

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

  DestSprite.Texture := Tex;

  DestSprite.AddGlyph(X, Y, Glyph, A, B, C, D, Skew, _Scale);

  Result := True;
End;

(*Function TERRAFontRenderer.DrawTextToImage(Target:TERRAImage; X, Y: Integer; const Text:TERRAString; ForceBlend:Boolean):TERRAFontRenderer;
Var
  Next:Cardinal;
  Alpha:Integer;
  DropShadowColor:ColorRGBA;
  Projection:Matrix4x4;
  A,B,C,D:ColorRGBA;
  I:Integer;
  GG:TERRAImage;
Begin
  Result := Self;

  If Target = Nil Then
    Exit;

  If (_Font = Nil) Then
    Exit;

  If (_Font.Status<>rsReady) Then
    _Font.Prefetch();

  _Font.Update();

  GetTextRect(Text); // TODO CHECK REALLY NECESSARY HERE?

  Y := Trunc(Y - _FontOffset);

  BeginRender(Text, fontmode_Offscreen, X, Y, 0);
  While (RenderNext()) Do
  Begin
    If (_CurrentGlyph = Nil) Then
      Continue;

    GG := _CurrentGlyph.GetImage();
    If (GG = Nil) Then
    Begin
    //  IntToString(2);
      Continue;
    End;

    Target.BlitWithAlpha(Trunc(Position.X + _CurrentGlyph.XOfs * FontInvScale), Trunc(Position.Y + _CurrentGlyph.YOfs * FontInvScale), 0, 0, GG.Width, GG.Height, GG, ForceBlend);
  End;
  EndRender();
End;*)

Function TERRAFontRenderer.SetClipRect(const Clip:TERRAClipRect):TERRAFontRenderer;
Begin
  Self._ClipRect := Clip;
  Result := Self;
End;

(*Function TERRAFontRenderer.SetDropShadow(Const DropShadowColor:ColorRGBA):TERRAFontRenderer;
Begin
  _DropShadow := DropShadowColor.A>0;

  If _DropShadow Then
  Begin
    _InitDropColor := DropShadowColor;
  End Else
  Begin
    _InitDropColor := ColorNull;
  End;

  {$IFDEF DISTANCEFIELDFONTS}
  Self.SetOutline(DropShadowColor);
  {$ENDIF}

  Result := Self;
End;*)

Function TERRAFontRenderer.SetSize(const Size: Single):TERRAFontRenderer;
Begin
  _Size := Size;
  _Scale := _Size / DefaultFontSize;

  Result := Self;
End;

Function TERRAFontRenderer.SetFont(const TargetFont:TERRAFont):TERRAFontRenderer;
Var
  Glyph:FontGlyph;
Begin
  Result := Self;
  _Font := TargetFont;
  If _Font = Nil Then
    Exit;

  Glyph := _Font.GetGlyph('E');
  If Assigned(Glyph) Then
    _FontOffset := Glyph.YOfs
  Else
    _FontOffset := 0;
End;

Function TERRAFontRenderer.SetGlow(Const Value:ColorRGBA):TERRAFontRenderer;
Begin
  Self._Outline := Value;
  Result := Self;
End;

Function TERRAFontRenderer.SetOutline(const Value:ColorRGBA):TERRAFontRenderer;
Begin
  Self._Outline := Value;
  Result := Self;
End;

Function TERRAFontRenderer.SetTransform(const Transform: Matrix3x3):TERRAFontRenderer;
Begin
  _Transform := Transform;
  Result := Self;
End;

Procedure TERRAFontRenderer.DrawSprite(const TextureName:TERRAString);
Var
  Tex:TERRATexture;
  S:TERRASprite;
Begin
  If (TextureName = '') Then
    Exit;

  Tex := Self.ResolveTexture(TextureName);
  If Tex = Nil Then
    Exit;

  Tex.Prefetch();

  {
  _CurrentPosition.X := _CurrentPosition.X + 4;

  If (_Mode = fontmode_Sprite) Then
  Begin
    (*S := _View.SpriteRenderer.DrawSprite(Self._Layer + 0.1, Tex);
    S.Translate(_CurrentPosition.X, _CurrentPosition.Y - Tex.Height);
    S.SetColor(ColorGrey(255, _Color1.A));
    Self.TransformSprite(S);*)
  End;

  _CurrentPosition.X := _CurrentPosition.X + Tex.Width + 4;

  If (_CurrentPosition.X >_MaxX) Then
    _MaxX := _CurrentPosition.X;

  If (_MaxY<=0) Then
    _MaxY := Tex.Height;}
End;

Function TERRAFontRenderer.ResolveTexture(Const TextureName: TERRAString): TERRATexture;
Begin
  Result := Engine.Textures.GetItem(TextureName);
End;

Procedure TERRAFontRenderer.TransformSprite(S: TERRASprite);
Begin
  S.ClipRect := Self._ClipRect;
End;

Function TERRAFontRenderer.AllocSprite: FontSprite;
Begin
  _LastAllocFrame := Engine.Graphics.FrameID;

  If (Length(_Sprites)<=_SpriteCount) Then
  Begin
    If (Length(_Sprites)<=0) Then
      SetLength(_Sprites, 64)
    Else
      SetLength(_Sprites, Length(_Sprites) * 2);
  End;

  If _Sprites[_SpriteCount] = Nil Then
  Begin
    _Sprites[_SpriteCount] := FontSprite.Create();
  End;

  Result := _Sprites[_SpriteCount];
  Inc(_SpriteCount);
End;

Procedure TERRAFontRenderer.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Length(_Sprites)) Do
    ReleaseObject(_Sprites[I]);
End;


Function TERRAFontRenderer.SetAutoWrap(Const Enabled: Boolean): TERRAFontRenderer;
Begin
  Self._AutoWrap := Enabled;
End;

Function TERRAFontRenderer.SetAreaLimit(const Width, Height: Single): TERRAFontRenderer;
Begin
  _MaxWidth := Width;
  _MaxHeight := Height;
End;

End.
