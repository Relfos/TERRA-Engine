Unit TERRA_FontRenderer;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Color, TERRA_Vector2D, TERRA_Matrix3x3, TERRA_Matrix4x4,
  TERRA_Resource, TERRA_Texture, TERRA_Font, TERRA_SpriteManager, TERRA_ClipRect, TERRA_Image;

Type
  FontRenderer = Class(TERRAObject)
    Protected
      _InitColor1, _InitColor2:Color;
      _Outline:Color;

      _Transform:Matrix3x3;

      _ClipRect:ClipRect;

      _Text:TERRAString;
      _Iterator:StringIterator;
      _Mode:Integer;
      _Next:TERRAChar;
      _Started:Boolean;

      _Font:TERRAFont;
      _FontOffset:Single;

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
      _GradientMode:FontGradient;
      _Count:Cardinal;

      _DropShadow:Boolean;
      _InitDropColor:Color;

      _Italics:Boolean;

      _Width:Single;
      _Height:Single;

      _MaxX:Single;
      _MaxY:Single;
      _AdvanceX:Single;

      Function GetNextChar:TERRAChar;
      Function GetNextArg:TERRAString;

      Procedure DoEffects();

      Procedure DrawSprite(Const TextureName:TERRAString);
      Function ResolveTexture(Const TextureName:TERRAString):TERRATexture; Virtual;
      Procedure TransformSprite(S:Sprite); Virtual;

      Procedure UpdateGradient(Width, Height:Single);

    Public
      Constructor Create();

      Function Reset():FontRenderer;

      Function SetColor(Const TextColor:Color):FontRenderer;
      Function SetGradient(Const A,B:Color; GradientMode:FontGradient):FontRenderer;
      Function SetOutline(Const OutlineColor:Color):FontRenderer;

      Function  SetFont(Const TargetFont:TERRAFont):FontRenderer;

      Function SetTransform(Const Transform:Matrix3x3):FontRenderer;
      Function  SetDropShadow(Const DropShadowColor:Color):FontRenderer;

      Function  SetClipRect(Const Clip:ClipRect):FontRenderer;

      Procedure BeginRender(Const S:TERRAString; Mode:Integer; X,Y, Layer:Single);
      Procedure EndRender();
      Function RenderNext():Boolean;

      Function DrawText(X,Y,Layer:Single; Const Text:TERRAString):FontRenderer;
      Function DrawTextToImage(Target:Image; X,Y:Integer; Const Text:TERRAString; ForceBlend:Boolean = True):FontRenderer;

      Procedure GetColors(Out A,B,C,D:Color);

      Function AutoWrapText(Const Text:TERRAString; Width:Single; Scale:Single = 1.0):TERRAString;
      Function GetTextWidth(Const Text:TERRAString; Scale:Single = 1.0):Single;
      Function GetTextHeight(Const Text:TERRAString; Scale:Single = 1.0):Single;
      Function GetTextRect(Const Text:TERRAString; Scale:Single = 1.0):Vector2D;
      Function GetLength(Const Text:TERRAString):Integer;

      Property Position:Vector2D Read _TargetPosition;
      Property Blink:Boolean Read _Blink;
      //Property Glyph:FontGlyph Read _CurrentGlyph;
      Property Next:Cardinal Read _Next;

      Property MaxX:Single Read _MaxX;
      Property MaxY:Single Read _MaxY;

      Property Italics:Boolean Read _Italics;

      Property Font:TERRAFont Read _Font;
  End;

Implementation
Uses TERRA_OS, TERRA_Math;

{ FontRenderer }
Constructor FontRenderer.Create;
Begin
  Reset();
End;

Function FontRenderer.Reset():FontRenderer;
Begin
  SetColor(ColorWhite);
  SetTransform(MatrixIdentity3x3);
  Result := Self;
End;

Procedure FontRenderer.BeginRender(Const S:TERRAString; Mode:Integer; X,Y, Layer:Single);
Begin
  If (_Font = Nil) Then
  Begin
    SetFont(FontManager.Instance.DefaultFont);
  End;

  If (Not _Font.IsReady()) Then
    Exit;

  _Mode := Mode;
  _Text := S;

  StringCreateIterator(_Text, _Iterator);

  _Layer := Layer;
  _StartPosition.X := X;
  _StartPosition.Y := Y;

  _Count := 0;
  _Italics := False;

  _EffectCount := 0;
  _WavyText := False;
  _Blink := False;

  _Width := 1.0;
  _Height := 1.0;

  _CurrentGlyph := Nil;
  _CurrentPosition := _StartPosition;

  _Started := True;

  _MaxX := X;
  _MaxY := Y;
  _Next := GetNextChar;
End;

Function FontRenderer.GetNextArg:TERRAString;
Var
  C:TERRAChar;
Begin
  Result := '';

  While _Iterator.HasNext() Do
  Begin
    C := _Iterator.GetNext();
    If C = fontControlEnd Then
      Break;

    StringAppendChar(Result, C);
  End;
End;

Function FontRenderer.GetNextChar:Cardinal;
Var
  Len:Integer;
  Current, Before, After:TERRAChar;
Begin
  If (Not _Iterator.HasNext()) Then
  Begin
    Result := NullChar;
    Exit;
  End;

  Result := _Iterator.GetNext();

  If (Result>0) And (Result<32) Then
  Begin
    _Effects[_EffectCount].Effect := Result;

    If (Result>fontControlNewLine) And (Result<fontControlEnd) Then
      _Effects[_EffectCount].Arg := GetNextArg()
    Else
      _Effects[_EffectCount].Arg := '';
    Inc(_EffectCount);

    Result := GetNextChar();
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

  {If (_RevealCount<=0) Then
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
  End;}

  If (_Started) Then
    DoEffects();

  //Log(logDebug,'AdWall', 'Calculating advance');
  _TargetPosition := _CurrentPosition;
  _AdvanceX := _CurrentGlyph.GetAdvance(_Next);
  _CurrentPosition.X := _CurrentPosition.X + _AdvanceX;

  //Log(logDebug,'AdWall', 'Testing effects');
  If (_Started) Then
    _Started := False;

  If (Not _Started) Then
    DoEffects();

  //Log(logDebug,'AdWall', 'Testing wavy test');
  If (_WavyText) Then
  Begin
    K := (Application.GetTime() Div 3);
    K := K + _Count * 8;
    _TargetPosition.Y := _TargetPosition.Y + Sin((K Mod 360) * RAD) * 4.5;
  End;

  If (_CurrentPosition.X >_MaxX) Then
    _MaxX := _CurrentPosition.X;

  H := _CurrentPosition.Y + _CurrentGlyph.Height;
  If (H > _MaxY) Then
    _MaxY := H;

  //Log(logDebug,'AdWall', 'Finisedh ok');
  Result := True;
End;

Function FontRenderer.SetColor(Const TextColor:Color):FontRenderer;
Begin
  _Color1 := TextColor;
  _Color2 := TextColor;
  _GradientMode := gradientNone;
  Result := Self;
End;

Function FontRenderer.SetGradient(Const A,B:Color; GradientMode:FontGradient):FontRenderer;
Begin
  _Color1 := A;
  _Color2 := B;
  _GradientMode := GradientMode;
  Result := Self;
End;

Procedure FontRenderer.UpdateGradient(Width, Height:Single);
Begin
  If (Width<=0.0) Then
    Width := 1.0;

  If (Height<=0.0) Then
    Height := 1.0;

  _Width := Width;
  _Height := Height;
End;

Procedure FontRenderer.GetColors(Out A, B, C, D:Color);
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
        Delta2 := (N + _CurrentGlyph.Height) / _Height;

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
  SS:TERRAString;
  C:Color;
Begin
  For I:=0 To Pred(_EffectCount) Do
  Case _Effects[I].Effect Of
  fontControlColor:
      If (_Effects[I].Arg<>'') Then
      Begin
        C := ColorCreateFromString(_Effects[I].Arg);
        Self._Color1 := C;
        Self._Color2 := C;
      End;

  fontControlSprite:
    Self.DrawSprite(_Effects[I].Arg);

  fontControlWave:
            Begin
              _WavyText := Not _WavyText;
            End;

  fontControlItalics:
            Begin
              _Italics := Not _Italics;
            End;

  fontControlBlink:
            Begin
              _Blink := Not _Blink;
            End;

  fontControlTab:
            Begin
              _CurrentPosition.X := (Trunc(_CurrentPosition.X/TabSize)+1)*TabSize;
            End;

  fontControlNewLine:
            Begin
              _CurrentPosition.X := _StartPosition.X;
              If Assigned(_Font) Then
                _CurrentPosition.Y := _CurrentPosition.Y + 4.0 + _Font.NewLineOffset;
            End;
  End;

  _EffectCount := 0;
End;

Function FontRenderer.GetTextWidth(Const Text:TERRAString; Scale:Single):Single;
Begin
  Result := GetTextRect(Text, Scale).X ;
End;

Function FontRenderer.GetTextHeight(Const Text:TERRAString; Scale:Single):Single;
Begin
  Result := GetTextRect(Text, Scale).Y ;
End;

Function FontRenderer.GetTextRect(Const Text:TERRAString; Scale:Single):Vector2D;
Begin
  If (_Font = Nil) Or (Not _Font.IsReady) Then
  Begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  End;

  //Log(logDebug,'AdWall', 'Starting gettextrect');
  BeginRender(Text, fontmode_Measure, 0, 0, -1);
  While (RenderNext()) Do;
  EndRender();

  //Log(logDebug,'AdWall', 'Finished textrect');
  Result.X := Self.MaxX;
  Result.Y := Self.MaxY;
End;

Function FontRenderer.GetLength(Const Text:TERRAString):Integer;
Begin
  Result := 0;
  BeginRender(Text, fontmode_Measure, 0, 0, -1);
  While (RenderNext()) Do
    Inc(Result);
  EndRender();
End;

Function FontRenderer.AutoWrapText(Const Text:TERRAString; Width, Scale: Single):TERRAString;
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
End;

Function FontRenderer.DrawText(X,Y, Layer:Single; Const Text:TERRAString):FontRenderer;
Var
  Alpha:Integer;
  Projection:Matrix4x4;
  A,B,C,D:Color;
  Size:Vector2D;
  I:Integer;
  DropColor:Color;
  FM:FontManager;
Begin
  Result := Self;

  If (_Font = Nil) Or (Not _Font.IsReady()) Then
    Exit;

  Alpha := IntMin(_Color1.A, _Color2.A);
  Alpha := Alpha - 55;
  If (Alpha<0) Then
    Alpha := 0;

  Y := Y - _FontOffset;

  Size := Self.GetTextRect(Text);

  If (_DropShadow) Then
  Begin
    DropColor := _InitDropColor;
    DropColor.A := Trunc(DropColor.A * (Alpha/255));
  End Else
    DropColor := ColorNull;

  BeginRender(Text, fontmode_Sprite, X, Y, Layer);
  If (_GradientMode <> gradientNone) Then
    UpdateGradient(Size.X, Size.Y);

  FM := FontManager.Instance;

  While (RenderNext()) Do
  Begin
    If (_CurrentGlyph = Nil) Then
    Begin
      Continue;
    End;

    GetColors(A,B,C,D);

    {$IFNDEF DISTANCEFIELDFONTS}
    If (_DropShadow) Then
      FM.DrawGlyph(Position.X - 1.0, Position.Y + 1.0, Layer - 0.1, _Transform, _CurrentGlyph, DropColor, DropColor, DropColor, DropColor, DropColor, _ClipRect, _Italics);
    {$ENDIF}

    FM.DrawGlyph(Position.X, Position.Y, Layer, _Transform, _CurrentGlyph, _Outline, A,B,C,D, _ClipRect, _Italics);
  End;

  EndRender();
End;

Function FontRenderer.DrawTextToImage(Target:Image; X, Y: Integer; const Text:TERRAString; ForceBlend:Boolean):FontRenderer;
Var
  Next:Cardinal;
  Alpha:Integer;
  DropShadowColor:TERRA_Color.Color;
  Projection:Matrix4x4;
  A,B,C,D:Color;
  I:Integer;
  GG:Image;
Begin
  Result := Self;
  
  If Target = Nil Then
    Exit;

  If (_Font = Nil) Then
    Exit;

  If (_Font.Status<>rsReady) Then
    _Font.Prefetch();

  _Font.Update();

  GetTextRect(Text, 1.0); // TODO CHECK REALLY NECESSARY HERE?

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

    Target.BlitWithAlpha(Trunc(Position.X + _CurrentGlyph.XOfs), Trunc(Position.Y + _CurrentGlyph.YOfs), 0, 0, GG.Width, GG.Height, GG, ForceBlend);
  End;
  EndRender();
End;

Procedure FontRenderer.EndRender;
Begin
  _Started := False;
End;

Function FontRenderer.SetClipRect(const Clip: ClipRect):FontRenderer;
Begin
  Self._ClipRect := Clip;
  Result := Self;
End;

Function FontRenderer.SetDropShadow(Const DropShadowColor:Color):FontRenderer;
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
End;

Function FontRenderer.SetFont(const TargetFont:TERRAFont):FontRenderer;
Var
  Glyph:FontGlyph;
Begin
  Result := Self;
  _Font := TargetFont;
  If _Font = Nil Then
    Exit;

  Glyph := _Font.GetGlyph(Ord('E'));
  If Assigned(Glyph) Then
    _FontOffset := Glyph.YOfs
  Else
    _FontOffset := 0;
End;

Function FontRenderer.SetOutline(const OutlineColor: Color):FontRenderer;
Begin
  Self._Outline := OutlineColor;
  Result := Self;
End;

Function FontRenderer.SetTransform(const Transform: Matrix3x3):FontRenderer;
Begin
  _Transform := Transform;
  Result := Self;
End;

Procedure FontRenderer.DrawSprite(const TextureName:TERRAString);
Var
  Tex:TERRATexture;
  S:Sprite;
Begin
  If (TextureName = '') Then
    Exit;

  Tex := Self.ResolveTexture(TextureName);
  If Tex = Nil Then
    Exit;

  Tex.Prefetch();

  _CurrentPosition.X := _CurrentPosition.X + 4;

  If (_Mode = fontmode_Sprite) Then
  Begin
    If _DropShadow Then
    Begin
      S := SpriteManager.Instance.DrawSprite(_CurrentPosition.X - 1, _CurrentPosition.Y - Tex.Height + 1, Self._Layer, Tex);
      S.SetColor(ColorGrey(0, _Color1.A));
      Self.TransformSprite(S);
    End;

    S := SpriteManager.Instance.DrawSprite(_CurrentPosition.X, _CurrentPosition.Y - Tex.Height, Self._Layer + 0.1, Tex);
    S.SetColor(ColorGrey(255, _Color1.A));
    Self.TransformSprite(S);
  End;

  _CurrentPosition.X := _CurrentPosition.X + Tex.Width + 4;

  If (_CurrentPosition.X >_MaxX) Then
    _MaxX := _CurrentPosition.X;

  If (_MaxY<=0) Then
    _MaxY := Tex.Height;
End;

Function FontRenderer.ResolveTexture(Const TextureName: TERRAString): TERRATexture;
Begin
  Result := TextureManager.Instance.GetTexture(TextureName);
End;

Procedure FontRenderer.TransformSprite(S: Sprite);
Begin
  S.ClipRect := Self._ClipRect;
End;

End.
