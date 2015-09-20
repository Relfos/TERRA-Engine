Unit TERRA_UIGraph;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_UIWidget, TERRA_Color,
  TERRA_UIDimension, TERRA_Sprite, TERRA_Texture, TERRA_Renderer, TERRA_Vector2D, TERRA_Viewport;

Type
  UIGraph = Class(UIWidget)
    Protected
      _Values:Array Of Single;
      _ValueCount:Integer;

      _MaximumRange:Single;

      Procedure UpdateSprite(); Override;

      Class Function GetObjectType:TERRAString; Override;

    Public
      Constructor Create(Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);

      Procedure SetGraphValue(ID:Integer; Const Value:Single);
  End;

  UISampledGraph = Class(UIGraph)
    Protected
      _SampleFrequency:Cardinal;
      _SampleAccum:Single;
      _SampleCount:Cardinal;
      _LastSampleUpdate:Cardinal;

      Procedure UpdateSprite(); Override;

    Public
      Target:TERRAObject; // can be any numberic property

      Constructor Create(Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);

      Procedure AddSample(Const Value:Single);
  End;

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_Engine, TERRA_DebugDraw;

{ UIImage }
Constructor UIGraph.Create(Name:TERRAString; Parent:UIWidget; Const X,Y:UIDimension; Const Layer:Single; Const Width, Height:UIDimension);
Begin
  Inherited Create(Name, Parent);

  Self.Left := X;
  Self.Top := Y;
  Self.Layer := Layer;

  Self.Width := Width;
  Self.Height := Height;

  Self.Pivot := Vector2D_Create(0, 0);
End;


Class Function UIGraph.GetObjectType: TERRAString;
Begin
  Result := 'UIGraph';
End;

Procedure UIGraph.SetGraphValue(ID: Integer; const Value: Single);
Begin
  If (ID<0) Then
    Exit;

  If (ID>=_ValueCount) Then
  Begin
    _ValueCount := Succ(ID);
    SetLength(_Values, _ValueCount);
  End;

  _Values[ID] := Value;
End;

Procedure UIGraph.UpdateSprite();
Var
  I:Integer;
  StartPos, EndPos:Vector2D;
  MinY, MaxY, ValY, ValRange:Single;
Begin
  _FullSize := CurrentSize;

  If _Sprite = Nil Then
    _Sprite := TERRASprite.Create()
  Else
    _Sprite.Clear();

  _Sprite.Texture := Engine.Textures.WhiteTexture;

  _Sprite.Layer := Self.GetLayer();
  _Sprite.Saturation := Self.GetSaturation();
  _Sprite.Glow := Self.GetGlow();
  _Sprite.SetColor(Self.Color);

  MinY := 99999;
  MaxY := -99999;
  For I:=0 To Pred(_ValueCount) Do
  Begin
    If (_Values[I] < MinY) Then
      MinY := _Values[I];

    If (_Values[I] > MaxY) Then
      MaxY := _Values[I];
  End;

  ValRange := (MaxY - MinY);
  If (ValRange < _MaximumRange) Then
    ValRange := _MaximumRange
  Else
    _MaximumRange := ValRange;

  StartPos := Vector2D_Zero;
  For I:=0 To Pred(_ValueCount) Do
  Begin
    If (I>0) And (I<Pred(_ValueCount)) And (_Values[I] = _Values[I-1]) And (_Values[I] = _Values[I+1])  Then
      Continue;

    EndPos := StartPos;
    ValY := ((_Values[I] - MinY)/ ValRange) * _FullSize.Y;
    StartPos := Vector2D_Create((I/_ValueCount) * _FullSize.X, _FullSize.Y - ValY);

    If (I = 0) Then
      Continue;

    _Sprite.AddLine(StartPos, EndPos, 0.0, 2.0);
  End;

  _Sprite.ClipRect := Self.ClipRect;
  _Sprite.SetTransform(_Transform);
End;

{ UISampledGraph }
Constructor UISampledGraph.Create(Name: TERRAString; Parent: UIWidget; Const X, Y:UIDimension; Const Layer:Single; const Width,Height:UIDimension);
Begin
  Inherited Create(Name, Parent, X, Y, Layer, Width, Height);
  _SampleFrequency := 250;

  Self.SetGraphValue(Trunc((1000/_SampleFrequency) * 30), 0.0);

  _LastSampleUpdate := Application.GetTime();
End;

Procedure UISampledGraph.AddSample(const Value: Single);
Var
  I:Integer;
Begin
  If (Application.GetTime - _LastSampleUpdate >= _SampleFrequency) Then
  Begin
    For I:=0 To Pred(_ValueCount) Do
      _Values[I] := _Values[I+1];

    If _SampleCount>0 Then
      _SampleAccum := _SampleAccum / _SampleCount
    Else
      _SampleAccum := 0.0;

    _Values[Pred(_ValueCount)] := _SampleAccum;

    _SampleAccum := 0;
    _SampleCount := 0;

    _LastSampleUpdate := Application.GetTime;
  End;

  _SampleAccum := _SampleAccum + Value;
  Inc(_SampleCount);
End;

Procedure UISampledGraph.UpdateSprite();
Var
  S:TERRAString;
Begin
  If Assigned(Target) Then
  Begin
    S := Target.GetBlob();
    Self.AddSample(StringToFloat(S));
  End;

  Inherited;
End;

End.