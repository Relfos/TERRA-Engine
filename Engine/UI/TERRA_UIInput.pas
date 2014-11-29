Unit TERRA_UIInput;
{$I terra.inc}
Interface
Uses TERRA_UI, TERRA_Vector3D, TERRA_Vector2D, TERRA_Color, TERRA_Math;

Type
  UIJoystick = Class(Widget)
    Protected
      _Width:Integer;
      _Height:Integer;
      _PickWidth:Integer;
      _PickHeight:Integer;
      _Dragging:Boolean;
      _JX,_JY:Integer;

      Procedure Render; Override;

      Procedure DoKey(ID:Integer; Value:Boolean);

    Public
      KeyUp, KeyDown, KeyLeft, KeyRight:Integer;

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;
      Function OnMouseUp(X,Y:Integer;Button:Word):Boolean; Override;
      Function OnMouseMove(X,Y:Integer):Boolean; Override;

      Constructor Create(Name:AnsiString; Parent:Widget; X,Y,Z:Single; Prefix:AnsiString = 'joystick'; TabIndex:Integer=-1);
  End;

  UIVirtualKey = Class(Widget)
    Protected
      _Width:Integer;
      _Height:Integer;
      _Dragging:Boolean;
      _Key:Integer;

      Procedure Render; Override;

    Public
      AllowSliding:Boolean;

      Function OnMouseDown(X,Y:Integer;Button:Word):Boolean; Override;
      Function OnMouseUp(X,Y:Integer;Button:Word):Boolean; Override;
      Function OnMouseMove(X,Y:Integer):Boolean; Override;

      Constructor Create(Name:AnsiString; Parent:Widget; X,Y,Z:Single; KeyIndex:Integer; SourceName:AnsiString; AllowSliding:Boolean = True; TabIndex:Integer=-1);
  End;

Implementation
Uses TERRA_Application, TERRA_OS, TERRA_XML;

{ UIJoystick }
Constructor UIJoystick.Create(Name:AnsiString; Parent:Widget; X, Y,Z: Single; Prefix:AnsiString; TabIndex:Integer);
Begin
  Self._Visible := True;
  Self._Name := Name;
  Self._Parent := Parent;
  Self._Position := VectorCreate2D(X,Y);
  Self._Layer := Z;
  Self._Color := ColorWhite;
  Self._TabIndex := TabIndex;
  Self.LoadComponent(Prefix+'_base.png');
  Self.LoadComponent(Prefix+'_point.png');

  Self.KeyUp := TERRA_OS.keyUp;
  Self.KeyDown := TERRA_OS.keyDown;
  Self.KeyLeft := TERRA_OS.keyLeft;
  Self.KeyRight := TERRA_OS.keyRight;

  If (Length(_ComponentList)>0) And (Assigned(_ComponentList[0])) And (Assigned(Self._ComponentList[0].Buffer)) Then
  Begin
    _Width := Self._ComponentList[0].Buffer.Width;
    _Height := Self._ComponentList[0].Buffer.Height;
  End;

  If (Length(_ComponentList)>1) And (Assigned(_ComponentList[1])) And (Assigned(Self._ComponentList[1].Buffer)) Then
  Begin
    _PickWidth := Self._ComponentList[1].Buffer.Width;
    _PickHeight := Self._ComponentList[1].Buffer.Height;
  End;

  UI.Instance.AddWidget(Self);
End;

Function UIJoystick.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Var
  P:Vector2D;
Begin
  P := Self.GetPosition;
  If (X>=P.X) And (X<=P.X+_Width) And (Y>=P.Y) And (Y<=P.Y+_Height) Then
  Begin
    _Dragging := True;
    OnMouseMove(X,Y);
    Result := True;
  End Else
    Result := False;
End;


Procedure UIJoystick.DoKey(ID:Integer; Value:Boolean);
Begin
  If (Application.Instance.Input.Keys[ID]=Value) Then
    Exit;

  Application.Instance.Input.Keys[ID] := Value;
End;

Function UIJoystick.OnMouseMove(X, Y: Integer): Boolean;
Var
  P:Vector2D;
  PX, PY:Integer;
  Dist:Single;
Begin
  If (_Dragging) Then
  Begin
    P := Self.GetPosition;
    PX := Trunc(X - P.X - (_Width * 0.5));
    PY := Trunc(Y - P.Y- (_Height * 0.5));
    Dist := Sqrt(Sqr(PX)+Sqr(PY));
    If (Dist<(_Width*0.4)) Then
    Begin
      _JX := PX;
      _JY := PY;

      DoKey(keyUp, _JY<-(_Height*0.2));
      DoKey(keyDown, _JY>+(_Height*0.2));
      DoKey(keyLeft, _JX<-(_Width*0.2));
      DoKey(keyRight, _JX>+(_Width*0.2));
    End;

    Result := True;
  End Else
    Result := False;
End;

Function UIJoystick.OnMouseUp(X, Y: Integer; Button: Word): Boolean;
Begin
  If (_Dragging) Then
  Begin
    _Dragging := False;
    DoKey(keyUp, False);
    DoKey(keyDown, False);
    DoKey(keyLeft, False);
    DoKey(keyRight, False);
  End;

  Result := False;
End;

Procedure UIJoystick.Render;
Begin
  _Size := VectorCreate2D(_Width, _Height);
  Self.DrawComponent(0, VectorZero, 0.0, 0.0, 0.975, 0.975, Self.GetColor, False);
  Self.DrawComponent(1, VectorCreate(_JX + (_Width - _PickWidth) *0.5, _JY + (_Height - _PickHeight) * 0.5, 5.0), 0.0, 0.0, 0.95, 0.95, Self.GetColor, False);
End;

{ UIVirtualKey }
Constructor UIVirtualKey.Create(Name:AnsiString; Parent: Widget; X, Y, Z: Single; KeyIndex: Integer; SourceName:AnsiString;  AllowSliding:Boolean ;TabIndex: Integer);
Begin
  Self._Visible := True;
  Self._Name := Name;
  Self._Parent := Parent;
  Self._Position := VectorCreate2D(X,Y);
  Self._Layer := Z;
  Self._Color := ColorWhite;
  Self._TabIndex := TabIndex;
  Self._Key := KeyIndex;
  Self.AllowSliding := AllowSliding;
  Self.LoadComponent('key_'+SourceName+'.png');

  If (Length(_ComponentList)>0) And (Assigned(_ComponentList[0])) And (Assigned(Self._ComponentList[0].Buffer)) Then
  Begin
    _Width := Self._ComponentList[0].Buffer.Width;
    _Height := Self._ComponentList[0].Buffer.Height;
  End;

  UI.Instance.AddWidget(Self);
End;

Var
  LastKey:UIVirtualKey = Nil;
  Sliding:Boolean = False;

Function UIVirtualKey.OnMouseDown(X, Y: Integer; Button: Word): Boolean;
Var
  P:Vector2D;
Begin
  P := Self.GetPosition;
  If (X>=P.X) And (Y>=P.Y) And (X<=P.X + _Width) And (Y<=P.Y + _Height) Then
  Begin
    Application.Instance.Input.Keys[_Key] := True;
    LastKey := Self;
    Sliding := True;
    Result := True;
  End Else
    Result := False;
End;

Function UIVirtualKey.OnMouseMove(X, Y: Integer): Boolean;
Var
  P:Vector2D;
Begin
  P := Self.GetPosition;
  If (LastKey<>Self) And (Sliding) And (AllowSliding) And (X>=P.X) And (Y>=P.Y) And (X<=P.X + _Width) And (Y<=P.Y + _Height) Then
  Begin
    If Assigned(LastKey) Then
      Application.Instance.Input.Keys[LastKey._Key] := False;

    Application.Instance.Input.Keys[_Key] := True;
    LastKey := Self;
    Result := True;
  End Else
    Result := False;
End;

Function UIVirtualKey.OnMouseUp(X, Y: Integer; Button: Word): Boolean;
Begin
  If (Self = LastKey) Then
  Begin
    Application.Instance.Input.Keys[_Key] := False;
    LastKey := Nil;
    Sliding := False;
  End;
  Result := False;
End;

Procedure UIVirtualKey.Render;
Begin
  _Size := VectorCreate2D(_Width, _Height);
  Self.DrawComponent(0, VectorZero, 0.0, 0.0, 1.0, 1.0, Self.GetColor, False);
End;


End.