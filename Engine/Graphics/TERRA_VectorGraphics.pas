{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_VectorGraphics
 * Implements vector graphic rendering with gradients
 ***********************************************************************************************************************
}
Unit TERRA_VectorGraphics;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_OS, TERRA_Matrix, TERRA_Color, TERRA_Vector3D, TERRA_Vector2D,
  TERRA_Collision2D, TERRA_Shader
  {$IFDEF FULLDEBUG}, TERRA_Debug{$ENDIF};

Type
  VectorBrush = Class
    Protected
      _Shader:Shader;
      _PositionHandle:Integer;
      _UVHandle:Integer;
      _FillHandle:Integer;

      Procedure BeginShader; Virtual;
  End;

  SolidFill = Class(VectorBrush)
    Protected
      Procedure BeginShader; Override;

    Public
      FillColor:Color;

      Constructor Create(MyColor:Color);
  End;

  RadialGradient = Class(VectorBrush)
    Protected
      _Ofs:Vector2D;

      Procedure BeginShader; Override;

    Public
      A,B:Color;

      Constructor Create(A,B:Color; OfsX, OfsY:Single);
  End;

  LinearGradient = Class(VectorBrush)
    Protected
      _Start:Vector2D;
      _End:Vector2D;

      Procedure BeginShader; Override;

    Public
      A,B:Color;

      Constructor Create(A,B:Color; X1,Y1,X2,Y2:Single);
  End;

  VectorVertex = Packed Record
    Position:Vector3D;
    FillCoord:Vector2D;
    TexCoord:Vector2D;
  End;

Procedure DrawCircle(X,Y,Layer:Single; Radius:Single; Outline,Fill:VectorBrush; Width:Single = 1.0);
Procedure DrawRect(X1,Y1,X2,Y2,Layer:Single; Outline,Fill:VectorBrush; Width:Single = 1.0);
Procedure DrawPolygon(Poly:Polygon2D; Layer:Single; Outline,Fill:VectorBrush; Width:Single = 1.0);
Procedure DrawPath(Poly:Polygon2D; Layer:Single; Fill:VectorBrush; Width:Single = 1.0);

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_GraphicsManager, TERRA_Math;

Function GetShader_SolidVector:AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
  Line('  attribute highp vec4 terra_position;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  uniform mat4 transformMatrix;');
	Line('void main()	{');
  Line('  gl_Position = projectionMatrix * transformMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
  Line('  uniform lowp vec4 fillColor;');
	Line('  void main()	{');
  Line('    gl_FragColor = fillColor;');
  Line(' }');
  Line('}  ');
  Result := S;
End;

Function GetShader_RadialVector:AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying highp vec2 fill;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec4 terra_fill;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  uniform mat4 transformMatrix;');
	Line('void main()	{');
  Line('  gl_Position = projectionMatrix * transformMatrix * terra_position;');
  Line('  fill = terra_fill.xy;}');
  Line('}');
  Line('fragment {');
	Line('  varying highp vec2 fill;');
	Line('  uniform lowp vec4 color1;');
	Line('  uniform lowp vec4 color2;');
	Line('  uniform highp vec2 ofs;');
	Line('  void main()	{');
  Line('    mediump float delta = min(length(fill.xy - ofs.xy), 1.0);');
  Line('    gl_FragColor = mix(color1, color2, delta);');
  Line(' }');
  Line('}  ');
  Result := S;
End;

Function GetShader_LinearVector:AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
	Line('  varying highp vec2 fill;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec4 terra_fill;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  uniform mat4 transformMatrix;');
	Line('void main()	{');
  Line('  gl_Position = projectionMatrix * transformMatrix * terra_position;');
  Line('  fill = terra_fill.xy;}');
  Line('}');
  Line('fragment {');
	Line('  varying highp vec2 fill;');
	Line('  uniform lowp vec4 color1;');
	Line('  uniform lowp vec4 color2;');
	Line('  uniform highp vec2 lineStart;');
	Line('  uniform highp vec2 lineEnd;');
	Line('  uniform highp float mag;');
	Line('  void main()	{');
  Line('    highp vec2 ofs = lineEnd - lineStart;');
  Line('    highp float delta = fill.x * (lineEnd.x - lineStart.x) - lineEnd.x * lineStart.x + lineStart.x*lineStart.x + fill.y * (lineEnd.y - lineStart.y) - lineEnd.y * lineStart.y + lineStart.y*lineStart.y;');
  Line('    delta = min(delta/mag, 1.0);');
  Line('    gl_FragColor = mix(color1, color2, delta);');
  Line(' }');
  Line('}  ');
  Result := S;
End;

Procedure VectorBrush.BeginShader;
Var
  Transform:Matrix;
Begin
  Transform := MatrixIdentity;

  ShaderManager.Instance.Bind(_Shader);
  _Shader.SetUniform('texture', 0);
  _Shader.SetUniform('projectionMatrix', GraphicsManager.Instance.ProjectionMatrix);
  _Shader.SetUniform('transformMatrix', Transform);

  _PositionHandle := _Shader.GetAttribute('terra_position');
  _UVHandle := _Shader.GetAttribute('terra_UV0');
  _FillHandle := _Shader.GetAttribute('terra_fill');
End;


Var
  _SolidFillShader:Shader = Nil;

Constructor SolidFill.Create(MyColor:Color);
Begin
  If Not Assigned(_SolidFillShader) Then
  Begin
    _SolidFillShader := TERRA_Shader.Shader.CreateFromString(GetShader_SolidVector(), 'Vectorsolid');
    ShaderManager.Instance.AddShader(_SolidFillShader);
  End;

  _Shader := _SolidFillShader;
  FillColor := MyColor;
End;

Procedure SolidFill.BeginShader;
Begin
  Inherited;
  _Shader.SetUniform('fillColor', FillColor);
End;


Var
  _RadialShader:Shader = Nil;

Constructor RadialGradient.Create(A,B:Color; OfsX, OfsY:Single);
Begin
  If Not Assigned(_RadialShader) Then
  Begin
    _RadialShader := TERRA_Shader.Shader.CreateFromString(GetShader_RadialVector(), 'Vectorradial');
    ShaderManager.Instance.AddShader(_RadialShader);
  End;

  _Shader := _RadialShader;
  Self.A := A;
  Self.B := B;
  Self._Ofs.X := OfsX;
  Self._Ofs.Y := OfsY;
End;

Procedure RadialGradient.BeginShader;
Begin
  Inherited;
  _Shader.SetUniform('color1', A);
  _Shader.SetUniform('color2', B);
  _Shader.SetUniform('ofs', _Ofs);
End;

Var
  _LinearShader:Shader = Nil;

Constructor LinearGradient.Create(A,B:Color; X1,Y1,X2,Y2:Single);
Begin
  If Not Assigned(_LinearShader) Then
  Begin
    _LinearShader := TERRA_Shader.Shader.CreateFromString(GetShader_LinearVector(), 'Vectorlinear');
    ShaderManager.Instance.AddShader(_LinearShader);
  End;

  _Shader := _LinearShader;
  Self.A := A;
  Self.B := B;
  Self._Start := VectorCreate2D(X1, Y1);
  Self._End := VectorCreate2D(X2, Y2);
End;

Procedure LinearGradient.BeginShader;
Var
  Mag:Single;
Begin
  Inherited;

  Mag := Sqrt(Sqr(_End.X - _Start.X) + Sqr(_End.Y - _Start.Y));

  _Shader.SetUniform('color1', A);
  _Shader.SetUniform('color2', B);
  _Shader.SetUniform('lineStart', _Start);
  _Shader.SetUniform('lineEnd', _End);
  _Shader.SetUniform('mag', Mag);
End;

Const
  Segments = 64;
Var
  _VertexList:Array[0..Succ(Segments*2)] Of VectorVertex;

Procedure DrawCircle(X,Y,Layer:Single; Radius:Single; Outline,Fill:VectorBrush; Width:Single);
Var
  I,J:Integer;
  Angle, Delta:Single;
  C, S:Single;
  PX, PY:Single;
Begin
  GraphicsManager.Instance.SetBlendMode(blendBlend);

  Angle := 0;
  Delta := (360*RAD) / Segments;
  Layer := -Layer;

  _VertexList[0].Position := VectorCreate(X, Y, Layer);
  _VertexList[0].FillCoord := VectorCreate2D(0, 0);

  If Assigned(Fill) Then
  Begin
    Fill.BeginShader;
    glVertexAttribPointer(Fill._PositionHandle, 3, GL_FLOAT, False, 28, @(_VertexList[0].Position));    
    If (Fill._UVHandle>=0) Then
      glVertexAttribPointer(Fill._UVHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].TexCoord));  
    If (Fill._FillHandle>=0) Then
      glVertexAttribPointer(Fill._FillHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].FillCoord));         

    For I:=0 To Segments Do
    Begin
      C := Cos(Angle);
      S := Sin(Angle);
      PX := X + Radius * C;
      PY := Y + Radius * S;
      _VertexList[Succ(I)].Position := VectorCreate(PX, PY, Layer);
      _VertexList[Succ(I)].FillCoord := VectorCreate2D(C, S);
      Angle := Angle - Delta;
    End;

    glDrawArrays(GL_TRIANGLE_FAN, 0, Segments+2);                                                 
    GraphicsManager.Instance.Internal(0, Segments);
  End;

  If (Assigned(Outline)) And (Width>0) Then
  Begin
    Outline.BeginShader;
    glVertexAttribPointer(Outline._PositionHandle, 3, GL_FLOAT, False, 28, @(_VertexList[0].Position));    
    If (Outline._UVHandle>=0) Then
      glVertexAttribPointer(Outline._UVHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].TexCoord));  
    If (Outline._FillHandle>=0) Then
      glVertexAttribPointer(Outline._FillHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].FillCoord));         

    J := 0;
    For I:=0 To Segments Do
    Begin
      C := Cos(Angle);
      S := Sin(Angle);
      PX := X + (Radius) * C;
      PY := Y + (Radius) * S;
      _VertexList[J].Position := VectorCreate(PX, PY, Layer);
      _VertexList[J].FillCoord := VectorCreate2D(C, S);
      Inc(J);

      PX := X + (Radius + Width) * C;
      PY := Y + (Radius + Width) * S;
      _VertexList[J].Position := VectorCreate(PX, PY, Layer);
      _VertexList[J].FillCoord := VectorCreate2D(C, S);
      Inc(J);

      Angle := Angle - Delta;
    End;

    glDrawArrays(GL_TRIANGLE_STRIP, 0, J);                                                 
  End;
End;

Var
  Index:Integer;
  OX1,OY1,OX2,OY2:Single;
  IndexList:Array[0..8*3] Of Word;

Function NewVertex(X,Y, Layer:Single):Integer; {$IFDEF FPC}Inline;{$ENDIF}
Begin
  _VertexList[Index].Position := VectorCreate(X, Y, Layer);
  _VertexList[Index].FillCoord := VectorCreate2D((((X-OX1)/(OX2-OX1))-0.5)*2, (((Y-OY1)/(OY2-OY1))-0.5)*2);
  Result := Index;
  Inc(Index);
End;

Procedure NewTriangle(A,B,C:Integer); {$IFDEF FPC}Inline;{$ENDIF}
Begin
  IndexList[Index+0] := A;
  IndexList[Index+1] := B;
  IndexList[Index+2] := C;
  Inc(Index, 3);
End;

Procedure DrawRect(X1,Y1,X2,Y2,Layer:Single; Outline,Fill:VectorBrush; Width:Single);
Var
  PX, PY:Single;
  A,B,C,D,E,F,G,H, L, M, N, O, Q:Integer;
Begin
  Layer := -Layer;

  If (Assigned(Fill)) Then
  Begin
    PX := (X1+X2) * 0.5;
    PY := (Y1+Y2) * 0.5;
    _VertexList[0].Position := VectorCreate(PX, PY, Layer);
    _VertexList[0].FillCoord := VectorCreate2D(0.0, 0.0);

    _VertexList[1].Position := VectorCreate(X1, Y1, Layer);
    _VertexList[1].FillCoord := VectorCreate2D(-1.0, -1.0);

    _VertexList[2].Position := VectorCreate(X1, Y2, Layer);
    _VertexList[2].FillCoord := VectorCreate2D(-1.0, 1.0);

    _VertexList[3].Position := VectorCreate(X2, Y2, Layer);
    _VertexList[3].FillCoord := VectorCreate2D(1.0, 1.0);

    _VertexList[4].Position := VectorCreate(X2, Y1, Layer);
    _VertexList[4].FillCoord := VectorCreate2D(1.0, -1.0);

    _VertexList[5] := _VertexList[1];

    Fill.BeginShader;

    If (Fill._PositionHandle>=0) Then
    Begin
      glVertexAttribPointer(Fill._PositionHandle, 3, GL_FLOAT, False, 28, @(_VertexList[0].Position));    
      If (Fill._UVHandle>=0) Then
        glVertexAttribPointer(Fill._UVHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].TexCoord));  
      If (Fill._FillHandle>=0) Then
        glVertexAttribPointer(Fill._FillHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].FillCoord));         

      glDrawArrays(GL_TRIANGLE_FAN, 0, 6);                                                 
      GraphicsManager.Instance.Internal(0, 2);
    End;
  End;

  If (Assigned(Outline)) And (Width>0) Then
  Begin
    Index := 0;
    OX1 := X1 - Width;
    OY1 := Y1 - Width;
    OX2 := X2 + Width;
    OY2 := Y2 + Width;

    A := NewVertex(OX1, OY1, Layer);
    B := NewVertex(OX1, Y1, Layer);
    C := NewVertex(OX2, OY1, Layer);
    D := NewVertex(OX2, Y1, Layer);
    E := NewVertex(X2, Y1, Layer);
    F := NewVertex(X2, Y2, Layer);
    G := NewVertex(OX2, Y2, Layer);
    H := NewVertex(OX1, OY2, Layer);
    L := NewVertex(OX2, OY2, Layer);
    M := NewVertex(OX1, Y2, Layer);
    N := NewVertex(X1, Y2, Layer);
    O := NewVertex(X1, Y1, Layer);

    Index := 0;
    NewTriangle(A,B,C);
    NewTriangle(D,C,B);
    NewTriangle(D,E,F);
    NewTriangle(D, F, G);
    NewTriangle(H,L,G);
    NewTriangle(G,M,H);

    NewTriangle(M, N, O);
    NewTriangle(M, O, B);

    Outline.BeginShader;

    If (Outline._PositionHandle>=0) Then
    Begin
      glVertexAttribPointer(Outline._PositionHandle, 3, GL_FLOAT, False, 28, @(_VertexList[0].Position));    
      If (Outline._UVHandle>=0) Then
        glVertexAttribPointer(Outline._UVHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].TexCoord));  
      If (Outline._FillHandle>=0) Then
        glVertexAttribPointer(Outline._FillHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].FillCoord));         

      glDrawElements(GL_TRIANGLES, Index, GL_UNSIGNED_SHORT, @IndexList[0]);                      

      GraphicsManager.Instance.Internal(0, Index Div 3);
    End;
  End;
End;


Procedure DrawPolygon(Poly:Polygon2D; Layer:Single; Outline,Fill:VectorBrush; Width:Single);
Var
  I:Integer;
  NX, NY:Single;
  X1, Y1:Single;
  X2, Y2:Single;
  PX, PY:Single;
  CX, CY:Single;
  W,H:Single;
  _VertexList:Array Of VectorVertex;
Begin
  If (Poly.VertexCount<3) Then
    Exit;

  If (Assigned(Outline)) And (Width>0) Then
    DrawPath(Poly, Layer + 0.1, Outline, Width);

  Layer := -Layer;

  SetLength(_VertexList, Succ(Poly.VertexCount));

  X1 := 9999;
  Y1 := 9999;
  X2 := -9999;
  Y2 := -9999;
  For I:=0 To Pred(Poly.VertexCount) Do
  Begin
    PX := Poly.Vertices[I].X;
    PY := Poly.Vertices[I].Y;

    If (PX<X1) Then
      X1 := PX;
    If (PY<Y1) Then
      Y1 := PY;

    If (PX>X2) Then
      X2 := PX;
    If (PY>Y2) Then
      Y2 := PY;
  End;
  W := (X2-X1) * 0.5;
  H := (Y2-Y1) * 0.5;
  CX := X1 + W;
  CY := Y1 + H;

  For I:=0 To Pred(Poly.VertexCount) Do
  Begin
    PX := Poly.Vertices[I].X;
    PY := Poly.Vertices[I].Y;

    NX := PX - CX;
    NY := PY - CY;

    _VertexList[I].Position.X := PX;
    _VertexList[I].Position.Y := PY;
    _VertexList[I].Position.Z := Layer;
    _VertexList[I].FillCoord := VectorCreate2D(NX/W, NY/H);
  End;
  _VertexList[Poly.VertexCount] := _VertexList[0];

  Fill.BeginShader;

  If (Fill._PositionHandle<0) Then
    Exit;

  glDisable(GL_CULL_FACE);

  glVertexAttribPointer(Fill._PositionHandle, 3, GL_FLOAT, False, 28, @(_VertexList[0].Position));    
  If (Fill._UVHandle>=0) Then
    glVertexAttribPointer(Fill._UVHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].TexCoord));  
  If (Fill._FillHandle>=0) Then
    glVertexAttribPointer(Fill._FillHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].FillCoord));         

  glDrawArrays(GL_TRIANGLE_STRIP, 0, Succ(Poly.VertexCount));                                                 

  glEnable(GL_CULL_FACE);
End;

Procedure DrawPath(Poly:Polygon2D; Layer:Single; Fill:VectorBrush; Width:Single = 1.0);
Var
  I,K:Integer;
  NX, NY:Single;
  TX, TY:Single;
  LX, LY:Single;
  SX, SY:Single;
  X1, Y1:Single;
  X2, Y2:Single;
  PX, PY:Single;
  CX, CY:Single;
  W,H, L:Single;
  Index:Integer;
  Left, Right:Vector2D;
  _VertexList:Array Of VectorVertex;

  Procedure AddPoint(TX, TY:Single);
  Begin
    NX := TX - CX;
    NY := TY - CY;
    _VertexList[Index].Position.X := TX;
    _VertexList[Index].Position.Y := TY;
    _VertexList[Index].Position.Z := Layer;
    _VertexList[Index].FillCoord := VectorCreate2D(NX/W, NY/H);
    Inc(Index);
  End;
Begin
  If (Poly.VertexCount<2) Then
    Exit;

  Layer := -Layer;
  Width := Width * 0.5;

  SetLength(_VertexList, Succ(Poly.VertexCount) * 4);

  X1 := 9999;
  Y1 := 9999;
  X2 := -9999;
  Y2 := -9999;
  For I:=0 To Pred(Poly.VertexCount) Do
  Begin
    PX := Poly.Vertices[I].X;
    PY := Poly.Vertices[I].Y;

    If (PX<X1) Then
      X1 := PX;
    If (PY<Y1) Then
      Y1 := PY;

    If (PX>X2) Then
      X2 := PX;
    If (PY>Y2) Then
      Y2 := PY;
  End;
  W := (X2-X1) * 0.5;
  H := (Y2-Y1) * 0.5;
  CX := X1 + W;
  CY := Y1 + H;

  Index := 0;
  For I:=0 To Poly.VertexCount Do
  Begin
    K := I Mod Poly.VertexCount;
    PX := Poly.Vertices[K].X;
    PY := Poly.Vertices[K].Y;

    K := Succ(I) Mod Poly.VertexCount;
    LX := Poly.Vertices[K].X;
    LY := Poly.Vertices[K].Y;

    SX := LX-PX;
    SY := LY-PY;

    L := Sqrt(Sqr(SX) + Sqr(SY));
    SX := SX / L;
    SY := SY / L;

    TX := PX + SY * Width;
    TY := PY + SX * Width;
    AddPoint(TX, TY);

    TX := PX - SY * Width;
    TY := PY - SX * Width;
    AddPoint(TX, TY);

    TX := LX + SY * Width;
    TY := LY + SX * Width;
    AddPoint(TX, TY);

    TX := LX - SY * Width;
    TY := LY - SX * Width;
    AddPoint(TX, TY);

  End;

  Fill.BeginShader;

  If (Fill._PositionHandle<0) Then
    Exit;

  glDisable(GL_CULL_FACE);

  glVertexAttribPointer(Fill._PositionHandle, 3, GL_FLOAT, False, 28, @(_VertexList[0].Position));    
  If (Fill._UVHandle>=0) Then
    glVertexAttribPointer(Fill._UVHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].TexCoord));  
  If (Fill._FillHandle>=0) Then
    glVertexAttribPointer(Fill._FillHandle, 2, GL_FLOAT, False, 28, @(_VertexList[0].FillCoord));         

  glDrawArrays(GL_TRIANGLE_STRIP, 0, Index);                                                 
  GraphicsManager.Instance.Internal(0, Index - 2);

  glEnable(GL_CULL_FACE);
End;

End.