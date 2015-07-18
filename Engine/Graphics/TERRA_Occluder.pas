Unit TERRA_Occluder;

{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_Utils, TERRA_Vector3D, TERRA_Vecto2D, TERRA_Viewport, TERRA_Renderable;

Type
  Occluder = Class(Renderable)
    Protected
      _Next:Occluder;
      _P1,_P2,_P3,_P4:Vector3D;
      _StartVertex, _EndVertex:Vector3D;
      _BoundingBox:BoundingBox;

      Function OccluderOccluded(Occ:Occluder):Boolean;

    Public
      Function IsVisible:Boolean;

      Procedure Update(View:TERRAViewport); Override;
      
      Procedure SetTransform(Transform:Matrix4x4; Width,Height:Single);

      Function PointOccluded(P:Vector3D):Boolean;
      Function BoxOccluded(Box:BoundingBox; V:TERRAViewport):Boolean;

      Procedure Render(View:TERRAViewport; Const Bucket:Cardinal); Override;
      Function GetBoundingBox:BoundingBox; Override;
  End;

Implementation

{ Occluder }
Procedure Occluder.SetTransform(Transform:Matrix4x4; Width,Height:Single);
Var
  X1,X2,Y1,Y2:Single;
Begin
  X1 := -Width*0.5;
  X2 := -X1;
  Y1 := 0.0;
  Y2 := Height;

  _P1 := Transform.Transform(VectorCreate(X1, Y1, 0.0));
  _P2 := Transform.Transform(VectorCreate(X2, Y1, 0.0));
  _P3 := Transform.Transform(VectorCreate(X2, Y2, 0.0));
  _P4 := Transform.Transform(VectorCreate(X1, Y2, 0.0));

  _BoundingBox.Reset;
  _BoundingBox.Add(_P1);
  _BoundingBox.Add(_P2);
  _BoundingBox.Add(_P3);
  _BoundingBox.Add(_P4);
End;

Function Occluder.IsVisible:Boolean;
Begin
  Result:=Not ((_EndVertex.X<0) Or (_StartVertex.X>GraphicsManager.Instance.Width) Or
          (_EndVertex.Y<0) Or (_StartVertex.Y>GraphicsManager.Instance.Height) Or
          ((_StartVertex.Z>1) And (_EndVertex.Z>1)));
End;

Procedure Occluder.Update(View:TERRAViewport);
Var
   T1,T2,T3,T4:Vector3D;
Begin
  T1 := View.ProjectPoint(_P1);
  T2 := View.ProjectPoint(_P2);
  T3 := View.ProjectPoint(_P3);
  T4 := View.ProjectPoint(_P4);

  _StartVertex := VectorMin(T1, VectorMin(T2, VectorMin(T3, T4)));
  _EndVertex := VectorMax(T1, VectorMax(T2, VectorMax(T3, T4)));
End;

Function Occluder.PointOccluded(P:Vector3D):Boolean;
Begin
    Result := (P.X>_StartVertex.X) And (P.X<_EndVertex.X) And
            (P.Y>_StartVertex.Y) And (P.Y<_EndVertex.Y) And
            (P.Z>FloatMin(_StartVertex.Z, _EndVertex.Z));
End;

Function Occluder.OccluderOccluded(Occ:Occluder):Boolean;
Begin
  Result := (Occ._StartVertex.X>_StartVertex.X) And (Occ._StartVertex.X<_EndVertex.X) And (Occ._StartVertex.Z>FloatMin(_StartVertex.Z, _EndVertex.Z))
          And (Occ._EndVertex.X>_StartVertex.X) And (Occ._EndVertex.X<_EndVertex.X) And (Occ._EndVertex.Z>FloatMin(_StartVertex.Z, _EndVertex.Z));
End;

Function Occluder.BoxOccluded(Box:BoundingBox; V:TERRAViewport):Boolean;
Var
  K:Single;
  A,B:Vector3D;
  {
  Image:TERRA_Image.Image;
  Snap:Boolean;}
Begin
  If (_StartVertex.Z>1) Or (_EndVertex.Z>1) Then
  Begin
    Result := False;
    Exit;
  End;

  {Snap := Application.Instance.Input.Keys.WasPressed(Ord('J'));
  If Snap Then
  Begin
    Image := TERRA_Image.Image.Create;
    Image.New(GraphicsManager.Instance.Width, GraphicsManager.Instance.Height);
  End;}

  Box := GraphicsManager.Instance.ProjectBoundingBox(Box, V);
  A := Box.StartVertex;
  B := Box.EndVertex;

  If A.X>B.X Then
  Begin
    K := A.X;
    A.X := B.X;
    B.X := K;
  End;

  If A.Y>B.Y Then
  Begin
    K:=A.Y;
    A.Y:=B.Y;
    B.Y:=K;
  End;

  If ((B.X<0) Or (A.X>Application.Instance.Width) Or (B.Y<0) Or (A.Y>Application.Instance.Height) Or
      ((A.Z>1) And (B.Z>1))) Then
  Begin
    Result:=True;
    Exit;
  End;

  {If Snap Then
  Begin
    Image.FillRectangle(Integer(Round(_StartVertex.X)), Integer(Round(_StartVertex.Y)), Integer(Round(_EndVertex.X)), Integer(Round(_EndVertex.Y)), ColorRed);
    Image.FillRectangle(Integer(Round(A.X)), Integer(Round(A.Y)), Integer(Round(B.X)), Integer(Round(B.Y)), ColorBlue);
    Image.Save('occlusion.png');
    ReleaseObject(Image)
    Halt;
  End;}

  Result:=(Self.PointOccluded(A) And Self.PointOccluded(B));
End;

Function Occluder.GetBoundingBox:BoundingBox;
Begin
  Result := _BoundingBox;
End;

Procedure Occluder.Render(View:TERRAViewport; Const Bucket:Cardinal);
Begin
{$IFDEF PC_X}
  If (TranslucentPass) Then
    Exit;

  GraphicsManager.Instance.EnableColorShader(ColorWhite, Matrix4x4Identity);
  glLineWidth(3);
//  glLineStipple(1, $FF);
//  glEnable(GL_LINE_STIPPLE);

  TextureManager.Instance.WhiteTexture.Bind(0);

  glDisable(GL_CULL_FACE);
   glBegin(GL_LINE_STRIP);
  With _P1 Do
    glVertex3f(X,Y,Z);
  With _P2 Do
    glVertex3f(X,Y,Z);
  With _P3 Do
    glVertex3f(X,Y,Z);
  With _P4 Do
    glVertex3f(X,Y,Z);
  With _P1 Do
    glVertex3f(X,Y,Z);
  glEnd;


//  glLineStipple(1, $FFFF);
//  glDisable(GL_LINE_STIPPLE);
{$ENDIF}
End;


End.