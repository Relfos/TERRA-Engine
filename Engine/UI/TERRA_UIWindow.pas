Unit TERRA_UIWindow;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_UI, TERRA_UISkin, TERRA_UIDimension, TERRA_Vector2D, TERRA_Color;

Type
  UIWindow = Class(Widget)
    Protected
      _Caption:TERRAString;

      Function IsSelectable():Boolean; Override;

    Public
      Selectable:Boolean;
      Frameless:Boolean;
      CloseButton:Widget;

      Procedure Render; Override;

//      Procedure EnableHighlights();

      Procedure SetCaption(Const Value:TERRAString);

      Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;
			Procedure OnMouseUp(X,Y:Integer;Button:Word); Override;
			Procedure OnMouseWheel(X,Y:Integer; Delta:Integer); Override;

      Constructor Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Width, Height:UIDimension; Const ComponentName:TERRAString);

      Property Caption:TERRAString Read _Caption Write SetCaption;
  End;


Implementation
Uses TERRA_OS;

{ UIWindow }
Constructor UIWindow.Create(Name:TERRAString; Parent:Widget; X,Y,Z:Single; Width, Height:UIDimension; Const ComponentName:TERRAString);
Begin
  Inherited Create(Name, Parent, ComponentName);

  Self.SetRelativePosition(VectorCreate2D(X,Y));
  Self.Layer := Z;
  Self.Width := Width;
  Self.Height := Height;
  Self._Dragging := False;

  Self.Selectable := True;

  Self.UpdateRects();
End;

Procedure UIWindow.OnMouseDown(X,Y:Integer;Button:Word);
Begin
  If (FrameLess) Then
    Exit;


  If (Assigned(OnMouseClick)) And (Not Self.HasPropertyTweens()) Then
  Begin
    Self._HitTime := Application.GetTime();
    Self._Hitting := True;
    Exit;
  End;

  Inherited OnMouseDown(X,Y, Button);
End;

Procedure UIWindow.OnMouseUp(X,Y:Integer;Button:Word);
Begin
  If (_Hitting) Then
  Begin
    _Hitting := False;
    Self.OnHit(Self.OnMouseClick);
    Exit;
  End;

  Inherited OnMouseUp(X,Y, Button);
End;

{Procedure UIWindow.EnableHighlights;
Var
  I:Integer;
  W:Widget;
Begin
  For I:=0 To Pred(ChildrenCount) Do
  Begin
    W := GetChild(I);
    If (Assigned(W)) And (W.CanHighlight) Then
    Begin
      While (W.UpControl<>Nil) And (W.UpControl.Position.Y<W.Position.Y) Do
        W := W.UpControl;
      UI.Highlight := W;
      Exit;
    End;
  End;
End;}

                                                         
Procedure UIWindow.Render;
Begin
  Self.ClearProperties();
  Self.UpdateRects();
  Self.UpdateTransform();
  Self.UpdateHighlight();

  {If (Not Self.HasTweens) And (Self.Parent = Nil) Then
  Begin
    Pos := Self.GetAbsolutePosition();
    Size := Self.Size;
    If (Pos.X + Size.X<=0) Or (Pos.Y + Size.Y<=0) Or (Pos.X + Size.X>=UIManager.Instance.Width) Or (Pos.Y + Size.Y>=UIManager.Instance.Height) Then
    Begin
      Self.Visible := False;
      Exit;
    End;
  End;}

  If (Not Frameless) Then
  Begin
    Self.DrawComponent( 0, 0, 0, Self.Width, Self.Height, 0, False);
  End;

  Inherited Render();
End;

Function UIWindow.IsSelectable: Boolean;
Begin
  Result := False;
End;

Procedure UIWindow.SetCaption(const Value:TERRAString);
Begin
  _Caption := Value;
End;

Procedure UIWindow.OnMouseWheel(X,Y:Integer; Delta: Integer);
Var
  I:Integer;
  Add:Single;
//  S:UIScrollbar;
Begin
(*  Result := False;

  RemoveHint(X+Y); //TODO - check this stupid hint

  For I:=0 To Pred(Self._ChildrenCount) Do
  If (_ChildrenList[I] Is UIScrollbar) Then
  Begin
    S := UIScrollbar(_ChildrenList[I]);

    Add := (Delta/Abs(Delta)) * -0.1;

    If (Not S.Horizontal) Then
      S.Slide(Add * S.Max);

    Result := True;
  End;*)
End;

End.