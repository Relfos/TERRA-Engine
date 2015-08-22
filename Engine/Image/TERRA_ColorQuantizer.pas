Unit TERRA_ColorQuantizer;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_Stream, TERRA_FileStream, TERRA_Math, TERRA_Color, TERRA_Image, TERRA_ColorDither, TERRA_Sort;

Type
  ColorTable = Class(TERRAObject)
    Protected
      _Palette:ColorPalette;
      _Size:Integer;

      Function ColorDistance(Const A,B:ColorRGBA):Integer;
      Function ColorFindInterpolation(Const Val, A,B:ColorRGBA):Single;

    Public

      Procedure Apply(Target:Image; DitherMode:Integer);

      Procedure LoadFromStream(Source:Stream);
      Procedure LoadFromFile(Const FileName:TERRAString);

      Procedure GetNearestDitherIndices(Const C:ColorRGBA; Out First, Second:Integer);
      Function GetNearestPaletteIndex(Const C:ColorRGBA):Integer;
      Function GetNearestPaletteColor(Const C:ColorRGBA):ColorRGBA;

      Function GetColorByIndex(Const Index:Integer):ColorRGBA;

      Function GetImage(Const Width, Height:Integer):Image;
  End;

  ColorOctreeNode = Class;

  ReducibleNodes = Array[0..7] OF ColorOctreeNode;

  ColorOctreeNode = Class(TERRAObject)
        IsLeaf      :Boolean;
        PixelCount  :Integer;
        RedSum      :Integer;
        GreenSum    :Integer;
        BlueSum:Integer;
        Next:ColorOctreeNode;
        Child:ReducibleNodes;

        Constructor Create (Const Level         :  Integer;
                            Const ColorBits     :  Integer;
                            Var   LeafCount     :  Integer;
                            Var   ReducibleNodes:  ReducibleNodes);
        Procedure Release;  Override;
      End;

  ColorQuantizer = Class(TERRAObject)
    Protected
          _Tree:ColorOctreeNode;
          _LeafCount:Integer;
          _ReducibleNodes:ReducibleNodes;
          _MaxColors:Integer;
          _ColorBits:Integer;

          Procedure AddColor(Var   Node:ColorOctreeNode; Const Color:ColorRGBA; Const ColorBits:Integer; Const Level:Integer; Var LeafCount:Integer; Var ReducibleNodes:ReducibleNodes);
          Procedure DeleteTree(Var Node:ColorOctreeNode);
          Procedure GetPaletteColors(Const Node:ColorOctreeNode; Var ColorPalette:ColorPalette; Var Index:Integer);
          Procedure   ReduceTree(Const ColorBits:Integer; Var LeafCount:Integer; Var ReducibleNodes:ReducibleNodes);

    Public
          Constructor Create(Const MaxColors:  Integer; Const ColorBits:  Integer);
          Procedure  Release();  Override;

          Function GetColorTableFromImage(Target:Image):ColorTable;

          Property ColorCount:Integer Read _LeafCount;
      End;

Implementation

{ ColorOctreeNode }
Constructor ColorOctreeNode.Create (Const Level:Integer; Const ColorBits:Integer; Var LeafCount:Integer; Var ReducibleNodes: ReducibleNodes);
Var
  i:Integer;
Begin
  PixelCount  := 0;
  RedSum      := 0;
  GreenSum    := 0;
  BlueSum     := 0;
  For I := Low(Child) To High(Child) Do
    Child[i] := Nil;

  IsLeaf := (Level = ColorBits);
  If IsLeaf Then
  Begin
    Next := NIL;
    Inc(LeafCount);
  End Else
  Begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := SELF
  End;
End;

Procedure ColorOctreeNode.Release;
Var
  i:Integer;
Begin
  For i := Low(Child) TO High(Child) Do
    ReleaseObject(Child[i]);
End;


{ ColorQuantizer }
Constructor ColorQuantizer.Create(Const MaxColors:  Integer; Const ColorBits:  Integer);
Var
  i:Integer;
Begin
  ASSERT (ColorBits <= 8);

  _Tree := NIL;
  _LeafCount := 0;

  // Initialize all nodes even though only ColorBits+1 of them are needed
  For I:=Low(_ReducibleNodes) To High(_ReducibleNodes) Do
    _ReducibleNodes[i] := NIL;

  _MaxColors := MaxColors;
  _ColorBits := ColorBits
End;

Procedure ColorQuantizer.Release;
Begin
  ReleaseObject(_Tree);
End;


Procedure ColorQuantizer.AddColor(Var Node:ColorOctreeNode; Const Color:ColorRGBA; Const ColorBits:Integer; Const Level:Integer; Var LeafCount:Integer; Var ReducibleNodes: ReducibleNodes);
Const
  Mask: Array[0..7] OF BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);
Var
  Index, Shift:Integer;
Begin
  // If the node doesn't exist, create it.
  If Node = Nil Then
    Node := ColorOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  If Node.IsLeaf Then
  Begin
      Inc(Node.PixelCount);
      Inc(Node.RedSum,   Color.R);
      Inc(Node.GreenSum, Color.G);
      Inc(Node.BlueSum,  Color.B);
  End Else
  Begin
    // Recurse a level deeper if the node is not a leaf.
    Shift := 7 - Level;

    Index :=
      (((Color.r AND mask[Level]) SHR Shift) SHL 2)  OR
      (((Color.g AND mask[Level]) SHR Shift) SHL 1)  OR
      ((Color.b AND mask[Level]) SHR Shift);

    AddColor(Node.Child[Index], Color, ColorBits, Level+1, LeafCount, ReducibleNodes)
  End;
End;

Procedure ColorQuantizer.DeleteTree(Var Node:  ColorOctreeNode);
Var
  I:Integer;
Begin
  For I := Low(ReducibleNodes) TO High(ReducibleNodes) Do
  Begin
    If Assigned(Node.Child[i]) Then
      DeleteTree(Node.Child[i]);
  End;

  ReleaseObject(Node);

  Node := Nil;
End;


Procedure ColorQuantizer.GetPaletteColors(Const Node:ColorOctreeNode; Var ColorPalette:ColorPalette; Var Index:Integer);
Var
  i:Integer;
Begin
  If Node.IsLeaf Then
  Begin
    With ColorPalette[Index] Do
    Begin
      R   := BYTE(Node.RedSum   DIV Node.PixelCount);
      G := BYTE(Node.GreenSum DIV Node.PixelCount);
      B  := BYTE(Node.BlueSum  DIV Node.PixelCount);
    End;

    Inc(Index)
  End Else
  Begin
    For i := Low(Node.Child) TO High(Node.Child) Do
    Begin
      If Assigned(Node.Child[i]) Then
        GetPaletteColors(Node.Child[i], ColorPalette, Index)
    End;
  End;
End;

Procedure ColorQuantizer.ReduceTree(Const ColorBits:  Integer; Var   LeafCount:Integer; Var ReducibleNodes: ReducibleNodes);
Var
  BlueSum :Integer;
  Children:Integer;
  GreenSum:Integer;
  i:Integer;
  Node:ColorOctreeNode;
  RedSum:Integer;
Begin
  // Find the deepest level containing at least one reducible node
  i := Colorbits - 1;
  While (i > 0) AND (ReducibleNodes[i] = NIL) DO
    Dec(i);

  // Reduce the node most recently added to the list at level i.
  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  RedSum   := 0;
  GreenSum := 0;
  BlueSum  := 0;
  Children := 0;

  For i := Low(ReducibleNodes) TO High(ReducibleNodes) DO
  Begin
    If Assigned(Node.Child[i]) Then
    Begin
      Inc(RedSum,          Node.Child[i].RedSum);
      Inc(GreenSum,        Node.Child[i].GreenSum);
      Inc(BlueSum,         Node.Child[i].BlueSum);
      Inc(Node.PixelCount, Node.Child[i].PixelCount);
      ReleaseObject(Node.Child[i]);
      Node.Child[i] := NIL;
      Inc(Children);
    End;
  End;

  Node.IsLeaf   := True;
  Node.RedSum   := RedSum;
  Node.GreenSum := GreenSum;
  Node.BlueSum  := BlueSum;
  Dec(LeafCount, Children-1);
End;

Function ColorQuantizer.GetColorTableFromImage(Target:Image):ColorTable;
Var
  i,Index:Integer;
  It:ImageIterator;
Begin
  Result := Nil;

  It := Target.Pixels([image_Read, image_Write]);
  While It.HasNext Do
  Begin
    If It.Value.A<=0 Then
      Continue;

    AddColor(_Tree, It.Value, _ColorBits, 0, _LeafCount, _ReducibleNodes);

    While _LeafCount > _MaxColors DO
      ReduceTree(_Colorbits, _LeafCount, _ReducibleNodes);
  End;
  ReleaseObject(It);

  Result := ColorTable.Create();
  Index := 0;
  GetPaletteColors(_Tree, Result._Palette, Index);
  Result._Size := Succ(Index);
End;


{ ColorTable }
Procedure ColorTable.Apply(Target:Image; DitherMode:Integer);
Var
  First, Second, Alpha:Integer;
  It:ImageIterator;
  Val:Single;
  Current, A, B:ColorRGBA;
  DitherVal:Integer;
Begin
  It := Target.Pixels([image_Read, image_Write]);
  While It.HasNext Do
  Begin
    If It.Value.A<=0 Then
      Continue;

    If (DitherMode<=1) Then
    Begin
      It.Value := GetNearestPaletteColor(Current);
      Continue;
    End;

    Current := It.Value;

    Alpha := Current.A;

    Self.GetNearestDitherIndices(Current, First, Second);
    A := _Palette[First];
    B := _Palette[Second];

    Val := Self.ColorFindInterpolation(Current, A, B);

    Case DitherMode Of
      2:  DitherVal := ColorDither2x2(It.X, It.Y, Val);
      8:  DitherVal := ColorDither8x8(It.X, It.Y, Val);
    Else
      DitherVal := ColorDither4x4(It.X, It.Y, Val);
    End;

    If DitherVal>0 Then
      Current := B
    Else
      Current := A;

      // apply separate alpha dither

    Val := Alpha / 255.0;
    Case DitherMode Of
      4:  DitherVal := ColorDither4x4(It.X, It.Y, Val);
      8:  DitherVal := ColorDither8x8(It.X, It.Y, Val);
      Else
        DitherVal := ColorDither2x2(It.X, It.Y, Val);
    End;

    Current.A := Alpha * DitherVal;

    //Current := ColorGrey(255 * DitherVal);

    It.Value := Current;
  End;

  ReleaseObject(It);
End;

Function ColorTable.GetNearestPaletteColor(const C: ColorRGBA): ColorRGBA;
Var
  Index:Integer;
Begin
  Index := Self.GetNearestPaletteIndex(C);
  Result := Self.GetColorByIndex(Index);
End;

Function ColorTable.GetNearestPaletteIndex(const C: ColorRGBA): Integer;
Var
  Current:ColorRGBA;
  I, Best, Dist:Integer;
Begin
  Result := -1;

  Best := 999999;

  For I:=0 To Pred(_Size) Do
  Begin
    Current := Self._Palette[I];
    Dist := Self.ColorDistance(Current, C);
    If (Dist<Best) Then
    Begin
      Best := Dist;
      Result := I;
    End;
  End;
End;

Procedure ColorTable.GetNearestDitherIndices(const C: ColorRGBA; out First, Second: Integer);
Var
  Current:ColorRGBA;
  I, Best, Dist:Integer;
Begin
  First := -1;

  Best := 999999;
  For I:=0 To Pred(_Size) Do
  Begin
    Current := Self._Palette[I];
    Dist := Self.ColorDistance(Current, C);
    If (Dist<Best) Then
    Begin
      Best := Dist;
      First := I;
    End;
  End;

  Second := First;

  Best := 999999;
  For I:=0 To Pred(_Size) Do
  If (I <> First) Then
  Begin
    Current := Self._Palette[I];
    Dist := Self.ColorDistance(Current, C);
    If (Dist<Best) Then
    Begin
      Best := Dist;
      Second := I;
    End;
  End;
End;

Function ColorTable.ColorDistance(const A, B: ColorRGBA):Integer;
Var
  DR, DG, DB:Integer;
  DH, DS, DL:Integer;
  TempA, TempB:ColorHSL;

  Dist1, Dist2:Integer;
Begin
  DR := A.R - B.R;
  DG := A.G - B.G;
  DB := A.B - B.B;
  Dist1 := (Sqr(DR) + Sqr(DG) + Sqr(DB));

(*  TempA := ColorRGBToHSL(A);
  TempB := ColorRGBToHSL(B);

  DH := TempA.H - TempB.H;
  DS := TempA.S - TempB.S;
  DL := TempA.L - TempB.L;

  Dist2 := Trunc(Sqrt(Sqr(DH) + Sqr(DS) + Sqr(DL)));

  Result := IntMin(Dist1, Dist2);*)

  Result := Dist1;
End;

Function ColorTable.ColorFindInterpolation(const Val, A, B: ColorRGBA): Single;
Var
  DistA, DistB:Integer;
Begin
  DistA := Self.ColorDistance(Val, A);
  DistB := Self.ColorDistance(Val, B);

  If (DistA = DistB) Then
  Begin
    Result := 0;
  End Else
  If (DistA<DistB) Then
  Begin
//  A--C------B
    Result := DistA / (DistA + DistB);
  End Else
  Begin
//  A------C--B
    Result := 0.5 + (DistB / (DistA + DistB));
  End;
End;

Function ColorTable.GetColorByIndex(const Index: Integer): ColorRGBA;
Begin
  If (Index>=0) And (Index<_Size) Then
    Result := Self._Palette[Index]
  Else
    Result := ColorNull;
End;

Function ColorTable.GetImage(Const Width, Height:Integer):Image;
Var
  I:Integer;
Begin
  Result := Image.Create(Width, Height);
  For I:=0 To Pred(_Size) Do
    Result.SetPixel(I Mod Width, I Div Width, _Palette[I]);
End;


Procedure ColorTable.LoadFromFile(const FileName: TERRAString);
Var
  Src:Stream;
Begin
  Src := FileStream.Open(FileName);
  Self.LoadFromStream(Src);
  ReleaseObject(Src);
End;

Procedure ColorTable.LoadFromStream(Source: Stream);
Var
  S:TERRAString;
  I, R, G, B:Integer;
Begin
  Source.ReadLine(S); //JASC
  Source.ReadLine(S); //0100

  Source.ReadLine(S);
  _Size := StringToInt(S);

  For I:=0 To Pred(_Size) Do
  If (Source.EOF) Then
    Break
  Else
  Begin
    Source.ReadLine(S);

    R := StringToInt(StringGetNextSplit(S, Ord(' ')));
    G := StringToInt(StringGetNextSplit(S, Ord(' ')));
    B := StringToInt(StringGetNextSplit(S, Ord(' ')));
    Self._Palette[I] := ColorCreate(R, G, B);
  End;
End;

End.