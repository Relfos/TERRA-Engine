Unit TERRA_UISkin;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D, TERRA_Color, TERRA_ClipRect, TERRA_Matrix3x3,
  TERRA_TextureAtlas, TERRA_Texture, TERRA_Font, TERRA_Stream, TERRA_ObjectTree;

Const
  layoutHorizontal = 0;
  layoutVertical   = 1;

  ExtendedPressDuration = 2000;

  System_Name_Wnd = '@UI_Window';
  System_Name_Text = '@UI_Text';
  System_Name_Btn = '@UI_BTN';
  System_Name_BG = '@UI_BG';

Type
  UISkinTile = Record
    X:Single;
    Y:Single;
    Width:Single;
    Height:Single;

    U1:Single;
    V1:Single;

    U2:Single;
    V2:Single;
  End;

  UIQuad = Record
    Pos:Vector2D;
    Size:Vector2D;

    StartUV:Vector2D;
    EndUV:Vector2D;

    PageID:Integer;
  End;

  UIQuadList = Record
    QuadCount:Integer;
    Quads:Array Of UIQuad;
  End;

  UISkinProperty = Record
    Saturation:Single;
    ColorTable:TERRATexture;
    QuadColor:Color;
    TextColor:Color;
    Clip:ClipRect;
    TextFont:TERRAFont;
  End;

  UISkinComponent = Class(TERRAObject)
    Protected
      _ID:Integer;
      _State:Integer;
      _Parent:UISkinComponent;
      _Name:TERRAString;

      _ChildrenList:Array Of UISkinComponent;
      _ChildrenCount:Integer;

      Procedure Release; Override;

      Procedure AddComponent(Component:UISkinComponent);
      Function LoadComponentFromNode(Source:TERRAObjectNode):UISkinComponent;

      Procedure InitFromSource(Source:TERRAObjectNode);

      Procedure AddQuad(Var Target:UIQuadList; Item:TextureAtlasItem; X, Y:Single; U1,V1, U2,V2:Single; Width, Height:Single);

    Public
      Constructor Create(Source:TERRAObjectNode; Parent:UISkinComponent);

      Function GetChildByName(Const Name:TERRAString):UISkinComponent;

      Function GetWidth(ID, State:Integer):Integer; Virtual;
      Function GetHeight(ID, State:Integer):Integer; Virtual;

      Procedure GetProperties(Var Props:UISkinProperty; ID, CurrentState, DefaultState:Integer); Virtual;

      Procedure Draw(Var Target:UIQuadList; Const X, Y, U1, V1, U2, V2:Single; Const Width, Height, ID, CurrentState, DefaultState:Integer); Virtual;

      Property Name:TERRAString Read _Name;
  End;

  UISkinRect = Class(UISkinComponent)
    Protected
      _Tiles:Array[0..2, 0..2] Of UISkinTile;
      _Texture:TextureAtlasItem;

      Function GetTile(I,J:Integer):UISkinTile;

    Public
      Constructor Create(Source:TERRAObjectNode; Parent:UISkinComponent);

      Procedure Draw(Var Target:UIQuadList; Const X, Y, U1, V1, U2, V2:Single; Const Width, Height, ID, CurrentState, DefaultState:Integer); Override;
  End;

  UISkinImage = Class(UISkinComponent)
    Protected
      _Texture:TextureAtlasItem;

    Public
      Constructor Create(Source:TERRAObjectNode; Parent:UISkinComponent);

      Function GetWidth(ID, State:Integer):Integer; Override;
      Function GetHeight(ID, State:Integer):Integer; Override;

      Procedure Draw(Var Target:UIQuadList; Const X, Y, U1, V1, U2, V2:Single; Const Width, Height, ID, CurrentState, DefaultState:Integer); Override;
  End;

  UISkinColor = Class(UISkinComponent)
    Protected
      _Color:Color;

    Public
      Constructor Create(Source:TERRAObjectNode; Parent:UISkinComponent);

      Procedure GetProperties(Var Props:UISkinProperty; ID, CurrentState, DefaultState:Integer); Override;
  End;

  UISkinTextColor = Class(UISkinColor)
    Public
      Procedure GetProperties(Var Props:UISkinProperty; ID, CurrentState, DefaultState:Integer); Override;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_Image, TERRA_FileUtils, TERRA_UI;

{ UISkinComponent }
Procedure UISkinComponent.AddQuad(Var Target:UIQuadList; Item:TextureAtlasItem; X, Y:Single; U1,V1, U2,V2:Single; Width, Height:Single);
Var
  P:Vector2D;
  ShadowOffset:Vector2D;
  Source:Image;
  MyTextureAtlas:TextureAtlas;
  Quad:UIQuad;
  US, VS:Single;
Begin
  If (Item = Nil) Then
    Exit;

  MyTextureAtlas := UIManager.Instance.TextureAtlas;
  Source := Item.Buffer;

  P.X := X;
  P.Y := Y;

//  Width := Width + Frac(X);
//  Height := Height + Frac(Y);

  US := Item.U2 - Item.U1;
  VS := Item.V2 - Item.V1;

  Quad.Pos := P;
  Quad.Size := VectorCreate2D(Width, Height);
  Quad.StartUV.X := Item.U1 +  U1*US;
  Quad.StartUV.Y := Item.V1 +  V1*VS;
  Quad.EndUV.X := Item.U1 +  U2*US;
  Quad.EndUV.Y := Item.V1 +  V2*VS;
  Quad.PageID := Item.PageID;

  Inc(Target.QuadCount);
  SetLength(Target.Quads, Target.QuadCount);
  Target.Quads[Pred(Target.QuadCount)] := Quad;
End;

Constructor UISkinComponent.Create(Source:TERRAObjectNode; Parent:UISkinComponent);
Var
  I:Integer;
  Component:UISkinComponent;
Begin
  _ChildrenCount := 0;
  _Parent := Parent;

  If Source = Nil Then
    Exit;

  Self.InitFromSource(Source);

  For I:=0 To Pred(Source.ChildCount) Do
  Begin
    Component := LoadComponentFromNode(Source.GetChildByIndex(I));
    AddComponent(Component);
  End;
End;

Procedure UISkinComponent.AddComponent(Component:UISkinComponent);
Begin
  If Component = Nil Then
    Exit;

  Inc(_ChildrenCount);
  SetLength(_ChildrenList, _ChildrenCount);
  _ChildrenList[Pred(_ChildrenCount)] := Component;
End;

Function UISkinComponent.LoadComponentFromNode(Source:TERRAObjectNode):UISkinComponent;
Var
  I:Integer;
Begin
  Result := Nil;
  If Source = Nil Then
    Exit;

  If StringEquals(Source.Name, 'component') Then
  Begin
    Result := UISkinComponent.Create(Source, Self);
  End Else
  If StringEquals(Source.Name, 'rect') Then
  Begin
    Result := UISkinRect.Create(Source, Self);
  End Else
  If StringEquals(Source.Name, 'image') Then
  Begin
    Result := UISkinImage.Create(Source, Self);
  End Else
  If StringEquals(Source.Name, 'color') Then
  Begin
    Result := UISkinColor.Create(Source, Self);
  End Else
  If StringEquals(Source.Name, 'textcolor') Then
  Begin
    Result := UISkinTextColor.Create(Source, Self);
  End Else
    Result := Nil;

End;

Procedure UISkinComponent.Draw(Var Target:UIQuadList; Const X, Y, U1, V1, U2, V2:Single; Const Width, Height, ID, CurrentState, DefaultState:Integer);
Var
  I, Count:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._State<0) And ((_ChildrenList[I]._ID = ID) Or (_ChildrenList[I]._ID <0)) Then
  Begin
    _ChildrenList[I].Draw(Target, X, Y, U1, V1, U2, V2, Width, Height, ID, CurrentState, DefaultState);
  End;

  If CurrentState<0 Then
    Exit;

  Count := Target.QuadCount;
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._State = CurrentState) And ((_ChildrenList[I]._ID = ID) Or (_ChildrenList[I]._ID <0)) Then
  Begin
    _ChildrenList[I].Draw(Target, X, Y, U1, V1, U2, V2, Width, Height, ID, CurrentState, DefaultState);
    Inc(Count);
  End;

  If (Count<Target.QuadCount) Then
    Exit;

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._State = DefaultState) And ((_ChildrenList[I]._ID = ID) Or (_ChildrenList[I]._ID <0)) Then
    _ChildrenList[I].Draw(Target, X, Y, U1, V1, U2, V2, Width, Height, ID, CurrentState, DefaultState);
End;

Procedure UISkinComponent.GetProperties(var Props: UISkinProperty; ID, CurrentState, DefaultState:Integer);
Var
  I, Count:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._State<0) And ((_ChildrenList[I]._ID = ID) Or (_ChildrenList[I]._ID <0)) Then
    _ChildrenList[I].GetProperties(Props, ID, CurrentState, DefaultState);

  If CurrentState<0 Then
    Exit;

  Count := 0;
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._State = CurrentState) And ((_ChildrenList[I]._ID = ID) Or (_ChildrenList[I]._ID <0)) Then
  Begin
    _ChildrenList[I].GetProperties(Props, ID, CurrentState, DefaultState);
    Inc(Count);
  End;

  If (Count>0) Then
    Exit;

  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._State = DefaultState) And ((_ChildrenList[I]._ID = ID) Or (_ChildrenList[I]._ID <0)) Then
    _ChildrenList[I].GetProperties(Props, ID, CurrentState, DefaultState);
End;

Procedure UISkinComponent.Release;
Begin
  // do nothing
End;

Function UISkinComponent.GetChildByName(Const Name: TERRAString): UISkinComponent;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildrenCount) Do
  If (StringEquals(_ChildrenList[I]._Name, Name)) Then
  Begin
    Result := _ChildrenList[I];
    Exit;
  End;

  Result := Nil;
End;

Procedure UISkinComponent.InitFromSource(Source:TERRAObjectNode);
Var
  S:TERRAString;
Begin
  If Source = Nil Then
    Exit;

  Source.GetString('name', _Name);
  Source.GetInteger('id', _ID, -1);
  Source.GetInteger('state', _State, -1);
End;

Function UISkinComponent.GetWidth(ID, State: Integer): Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._ID = ID) And (_ChildrenList[I]._State = State) Then
    Result := IntMax(Result, _ChildrenList[I].GetWidth(ID, State));
End;

Function UISkinComponent.GetHeight(ID, State: Integer): Integer;
Var
  I:Integer;
Begin
  Result := 0;
  For I:=0 To Pred(_ChildrenCount) Do
  If (_ChildrenList[I]._ID = ID) And (_ChildrenList[I]._State = State) Then
    Result := IntMax(Result, _ChildrenList[I].GetHeight(ID, State));
End;

{ UISkinRect }
Constructor UISkinRect.Create(Source:TERRAObjectNode; Parent:UISkinComponent);
Var
  I, J:Integer;
  W, H:Integer;
  SrcFile:TERRAString;

  X1, Y1, X2, Y2:Integer;
  U1, V1, U2, V2:Single;
Begin
  _ChildrenCount := 0;
  _Parent := Parent;

  Self.InitFromSource(Source);

  Source.GetString('src', SrcFile);
  _Texture := UIManager.Instance.GetUI(0).LoadImage(SrcFile);

  W := _Texture.Buffer.Width;
  H := _Texture.Buffer.Height;

  Source.GetInteger('x1', X1, W Div 3);
  Source.GetInteger('y1', Y1, H Div 3);
  Source.GetInteger('x2', X2, (W Div 3) * 2 );
  Source.GetInteger('y2', Y2, (H Div 3) * 2);

  U1 := X1 / W;
  V1 := Y1 / H;
  U2 := X2 / W;
  V2 := Y2 / H;

  For J:=0 To 2 Do
    For I:=0 To 2 Do
    Begin
      Case I Of
      0:Begin
          _Tiles[I, J].U1 := 0.0;
          _Tiles[I, J].U2 := U1;
        End;
      1:Begin
          _Tiles[I, J].U1 := U1;
          _Tiles[I, J].U2 := U2;
        End;
      2:Begin
          _Tiles[I, J].U1 := U2;
          _Tiles[I, J].U2 := 1.0;
        End;
      End;

      Case J Of
      0:Begin
          _Tiles[I, J].V1 := 0.0;
          _Tiles[I, J].V2 := V1;
        End;
      1:Begin
          _Tiles[I, J].V1 := V1;
          _Tiles[I, J].V2 := V2;
        End;
      2:Begin
          _Tiles[I, J].V1 := V2;
          _Tiles[I, J].V2 := 1.0;
        End;
      End;
    End;

  For J:=0 To 2 Do
    For I:=0 To 2 Do
    Begin
      _Tiles[I, J].X := _Tiles[I, J].U1 * W;
      _Tiles[I, J].Y := _Tiles[I, J].V1 * H;

      _Tiles[I, J].Width := ((_Tiles[I, J].U2 * W) - _Tiles[I, J].X);
      _Tiles[I, J].Height := ((_Tiles[I, J].V2 * H) - _Tiles[I, J].Y);
    End;
End;

Function UISkinRect.GetTile(I, J: Integer): UISkinTile;
Begin
  Result := _Tiles[I,J];
End;

Procedure UISkinRect.Draw(Var Target:UIQuadList; Const X, Y, U1, V1, U2, V2:Single; Const Width, Height, ID, CurrentState, DefaultState:Integer);
Var
  CompScaleX, CompScaleY, CompSizeX, CompSizeY, CompX, CompY:Single;
  I,J, CountX, CountY:Integer;
  LX,LY:Single;
  NW, NH:Single;
  BottomX, BottomY:Single;
  T:UISkinTile;
Begin
  LX := Width - (Self.GetTile(0, 0).Width + Self.GetTile(2, 0).Width);
  LY := Height - (Self.GetTile(0, 0).Height + Self.GetTile(0, 2).Height);

  CompSizeX := Self.GetTile(1, 1).Width;
  CountX := Trunc(LX / CompSizeX);
  If CountX<=0 Then
  Begin
    CountX := 1;
    CompScaleX := LX/CompSizeX;
  End Else
  Begin
    NW := CountX * CompSizeX;
    CompScaleX := LX/NW;
  End;
  CompSizeX := Trunc(CompSizeX * CompScaleX);

  CompSizeY := Self.GetTile(1, 1).Height;
  CountY := Trunc(LY / CompSizeY);
  If CountY<=0 Then
  Begin
    CountY := 1;
    CompScaleY := LY/CompSizeY;
  End Else
  Begin
    NH := CountY * CompSizeY;
    CompScaleY := LY/NH;
  End;
  CompSizeY := Trunc(CompSizeY * CompScaleY);

  CompX := Self.GetTile(0, 0).Width;
  For I:=1 To CountX Do
    CompX := CompX + CompSizeX;
  BottomX := CompX;

  CompY := Self.GetTile(0, 0).Height;
  For I:=1 To CountY Do
    CompY := CompY + CompSizeY;
  BottomY := CompY;

  CompX := Self.GetTile(0, 0).Width;
  For I:=1 To CountX Do
  Begin
    T := Self.GetTile(1, 0);
    Self.AddQuad(Target, _Texture, X + CompX, Y, T.U1, T.V1, T.U2, T.V2, CompSizeX, T.Height);

    T := Self.GetTile(1, 2);
    Self.AddQuad(Target, _Texture, X + CompX, Y + BottomY, T.U1, T.V1, T.U2, T.V2, CompSizeX, T.Height);

    CompX := CompX + CompSizeX;
  End;

  CompY := Self.GetTile(0, 0).Height;
  For I:=1 To CountY Do
  Begin
    T := Self.GetTile(0, 1);
    Self.AddQuad(Target, _Texture, X, Y + CompY, T.U1, T.V1, T.U2, T.V2, T.Width, CompSizeY);

    T := Self.GetTile(2, 1);
    Self.AddQuad(Target, _Texture, X + BottomX, Y + CompY, T.U1, T.V1, T.U2, T.V2, T.Width, CompSizeY);

    CompY := CompY + CompSizeY;
  End;

  T := Self.GetTile(0, 0);
  Self.AddQuad(Target, _Texture, X, Y, T.U1, T.V1, T.U2, T.V2, T.Width, T.Height);

  T := Self.GetTile(2, 0);
  Self.AddQuad(Target, _Texture, X + BottomX, Y, T.U1, T.V1, T.U2, T.V2, T.Width, T.Height);

  T := Self.GetTile(0, 2);
  Self.AddQuad(Target, _Texture, X, Y + BottomY, T.U1, T.V1, T.U2, T.V2, T.Width, T.Height);

  T := Self.GetTile(2, 2);
  Self.AddQuad(Target, _Texture, X + BottomX, Y + BottomY, T.U1, T.V1, T.U2, T.V2, T.Width, T.Height);

  CompY := Self.GetTile(0, 0).Height;
  For J:=1 To CountY Do
  Begin
    CompX := Self.GetTile(0, 0).Width;

    T := Self.GetTile(1, 1);
    For I:=1 To CountX Do
    Begin
      Self.AddQuad(Target, _Texture, X + CompX, Y + CompY, T.U1, T.V1, T.U2, T.V2, CompSizeX, CompSizeY);
      CompX := CompX + CompSizeX;
    End;

    CompY := CompY + CompSizeY;
  End;
End;

{ UISkinImage }
Constructor UISkinImage.Create(Source:TERRAObjectNode; Parent: UISkinComponent);
Var
  SrcFile:TERRAString;
Begin
  _Parent := Parent;

  Self.InitFromSource(Source);

  Source.GetString('src', SrcFile);
  _Texture := UIManager.Instance.GetUI(0).LoadImage(SrcFile);
End;

Procedure UISkinImage.Draw(Var Target:UIQuadList; Const X, Y, U1, V1, U2, V2:Single; Const Width, Height, ID, CurrentState, DefaultState:Integer);
Begin
  Self.AddQuad(Target, _Texture, X, Y, U1, V1, U2, V2, Width, Height);
End;


Function UISkinImage.GetWidth(ID, State: Integer): Integer;
Begin
  If (Assigned(_Texture)) And (Assigned(_Texture.Buffer)) Then
    Result := _Texture.Buffer.Width
  Else
    Result := 0;
End;

Function UISkinImage.GetHeight(ID, State: Integer): Integer;
Begin
  If (Assigned(_Texture)) And (Assigned(_Texture.Buffer)) Then
    Result := _Texture.Buffer.Height
  Else
    Result := 0;
End;

{ UISkinColor }
Constructor UISkinColor.Create(Source:TERRAObjectNode; Parent: UISkinComponent);
Var
  S:TERRAString;
Begin
  _Parent := Parent;

  Self.InitFromSource(Source);

  Source.GetString('color', S, '#FFFFFF');
  _Color := ColorCreateFromString(S);

  Source.GetString('alpha', S, '');
  If S<>'' Then
    _Color.A := StringToInt(S);
End;

Procedure UISkinColor.GetProperties(var Props: UISkinProperty; ID, CurrentState, DefaultState:Integer);
Begin
  Props.QuadColor := Self._Color;
End;

{ UISkinTextColor }
Procedure UISkinTextColor.GetProperties(var Props: UISkinProperty; ID,CurrentState, DefaultState: Integer);
Begin
  Props.TextColor := Self._Color;
End;

End.
