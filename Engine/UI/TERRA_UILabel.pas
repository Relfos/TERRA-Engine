Unit TERRA_UILabel;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_Vector2D, TERRA_Color, TERRA_Font, TERRA_UICaption;

Type
  UILabel = Class(UICaption)
    Protected
      _Width:Single;
      _Height:Single;

    Public
      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Caption:TERRAString; TabIndex:Integer=-1);

      Function SupportDrag(Mode:UIDragMode):Boolean; Override; 

      Procedure Render; Override;
      Procedure UpdateRects; Override;
  End;


Implementation
Uses TERRA_OS;

Constructor UILabel.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Caption:TERRAString; TabIndex:Integer);
Begin
  Inherited Create(Name, Parent, '');

  Self.TabIndex := TabIndex;

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;
  Self._Width := 0;
  Self._Height := 0;

  Self.Caption.Value := Caption;
  _NeedsUpdate := True;
End;

(*Function UILabel.OnMouseMove(X,Y:Integer):Boolean;
Var
  Pos:Vector2D;
  AO, OH:Boolean;
Begin
  Pos := Self.GetPosition;
  OH := _HighLight;
  AO := (Assigned(OnMouseClick));
  _HighLight := (AO) And (OnRegion(X,Y)) And (Not Self.HasTweens);
  Result := False;
  If _HighLight  Then
    Result := True;
  {$IFDEF IPHONE}
  If (_Highlight) And (Not OH) And (Not DisableSlideTouch) Then
    Self.OnMouseDown(X, Y, 99);
  {$ENDIF}
End;*)

{Procedure UILabel.Reveal(DurationPerLetter, Delay:Cardinal);
Begin
  Self._RevealTime := Delay + GetTime;

  If Not Assigned(Self.Font) Then
    Exit;

  Self._RevealDuration := Self.Font.GetLength(Caption);
  Self._RevealDuration := DurationPerLetter * Self._RevealDuration;
End;}

Procedure UILabel.UpdateRects();
Begin
  Inherited;
  _Size := _TextRect;
End;

Procedure UILabel.Render();
Var
  S:TERRAString;
  Time:Cardinal;
  Delta:Single;
  P:Vector2D;
Begin
  Self.ClearProperties();
  Self.UpdateRects;

  If (Caption.Value = '') Then
    Exit;

  Self.UpdateTransform();

  _Width := Self._TextRect.X;
  _Height := Self._TextRect.Y;

(*  {$IFDEF MOBILE}
  Color := Self.GetColor;
  {$ELSE}
  If (Not Assigned(OnMouseClick)) Or (_HighLight) Then
    Color := Self.GetColor
  Else
    Color := ColorGrey(200, Self.Color.A);
  {$ENDIF}*)

  If (Not DisableHighlights) Then
    Self.UpdateHighlight();

  Color := Self.GetColor;

  Self.DrawText(Caption.Value, 0, 0, 0, _TextRect, Scale, 0, False, Color);

  Inherited;
End;





Function UILabel.SupportDrag(Mode: UIDragMode): Boolean;
Begin
  Result := (Mode = UIDrag_Move);
End;

End.