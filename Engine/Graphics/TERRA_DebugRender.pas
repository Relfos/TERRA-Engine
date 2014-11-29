Unit TERRA_DebugRender;
{$I terra.inc}

Interface
Uses TERRA_GraphicsManager, TERRA_BoundingBox, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Color, TERRA_Vector3D, TERRA_Matrix,
  TERRA_Texture;

Type
  PDebugLine = ^DebugLine;
  DebugLine = Record
    A,B:Vector3D;
    Color:TERRA_Color.Color;
    Visible:Boolean;
  End;

  DebugCollection = Class(Renderable)
    Protected
      _Lines:Array Of DebugLine;
      _LineCount:Integer;

    Public
      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render; Override;

      Function GetLine(ID:Integer):PDebugLine;
  End;

Implementation

{ DebugCollection }
Function DebugCollection.GetBoundingBox: BoundingBox;
Var
  I:Integer;
Begin
  Result.Reset;
  For I:=0 To Pred(_LineCount) Do
  If (_Lines[I].Visible) Then
  Begin
    Result.Add(_Lines[I].A);
    Result.Add(_Lines[I].B);
  End;
End;

Function DebugCollection.GetLine(ID: Integer): PDebugLine;
Begin
  If (ID<0) Then
    Result := Nil
  Else
  If (ID>=_LineCount) Then
  Begin
    _LineCount := Succ(ID);
    SetLength(_Lines, _LineCount);
    _Lines[ID].Color := ColorWhite;
    _Lines[ID].Visible := True;
    Result := @(_Lines[ID]);
  End Else
    Result := @(_Lines[ID]);
End;

Procedure DebugCollection.Render;
Var
  I:Integer;
Begin
  Texture.Bind(Nil, 1);
  Texture.Bind(Nil, 0);

{$IFDEF PC}
  GraphicsManager.Instance.EnableColorShader(ColorWhite, MatrixIdentity);
  glLineWidth(3.0);

  For I:=0 To Pred(_LineCount) Do
  Begin
    glColor3ub(_Lines[I].Color.R, _Lines[I].Color.G, _Lines[I].Color.B);
    glBegin(GL_LINE_STRIP);
    With _Lines[I].A Do
      glVertex3f(X,Y,Z);
    With _Lines[I].B Do
      glVertex3f(X,Y,Z);
    glEnd;
  End;
{$ENDIF}
End;

End.