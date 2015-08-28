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
 * TERRA_UITransition
 * Implements UI transitions (fades, etc)
 ***********************************************************************************************************************
}
Unit TERRA_UITransition;

{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Utils, TERRA_Math, TERRA_Vector2D, TERRA_Vector3D, TERRA_Texture, TERRA_Resource,
  {$IFDEF POSTPROCESSING}TERRA_ScreenFX, {$ENDIF}
  TERRA_Image, TERRA_Color, TERRA_Matrix4x4, TERRA_Matrix3x3, TERRA_VertexFormat;

Const
  FadeVertexFormat = [vertexFormatPosition, vertexFormatUV0];

Type
  FadeCallback = Procedure(Arg:Pointer);  CDecl;

{  FadeVertex = Packed Record
    Position:Vector2D;
    UV:Vector2D;
  End;}

  UITransition = Class(TERRAObject)
    Protected
      _ID:Cardinal;
      _Time:Cardinal;
      _Duration:Integer;
      _Delay:Integer;
      _Active:Boolean;
      _Running:Boolean;
      _CallOnStart:Boolean;
      _Callback:FadeCallback;
      _Arg:Pointer;
      _EaseType:Integer;
      _FinishValue:Single;
      _Transform:Matrix3x3;

      _FadeVertices:VertexData;


      Procedure Render(Alpha:Single); Virtual; Abstract;

      Procedure InitVertices(OfsX, OfsY:Single);

    Public
      Function Update:Boolean;

      Procedure SetCallback(Callback:FadeCallback; UserData:Pointer = Nil; OnStart:Boolean=False);

      Procedure Release; Override;

      Property FinishValue:Single Read _FinishValue Write _FinishValue;
      Property Duration:Integer Read _Duration;
      Property ID:Cardinal Read _ID;
      Property EaseType:Integer Read _EaseType Write _EaseType;

      Property Transform:Matrix3x3 Read _Transform Write _Transform;
  End;

  UIFade = Class(UITransition)
    Protected
      _FadeTexture:Texture;
      _FadeOut:Boolean;

      Procedure Render(Alpha:Single); Override;

    Public
      Color:TERRA_Color.Color;

      Constructor Create(FadeTexture:Texture; Duration, Delay:Cardinal; Invert:Boolean);
  End;

  UISlide = Class(UITransition)
    Protected
      _Direction:Vector2D;
      _Texture:Texture;

      Procedure Render(Alpha:Single); Override;

    Public
      Constructor Create(Direction:Vector2D; Duration, Delay:Cardinal);
      Procedure Release; Override;
  End;

Implementation
Uses TERRA_OS, TERRA_ResourceManager, TERRA_GraphicsManager, TERRA_Renderer, TERRA_Tween, TERRA_UI;

Var
  _FadeShader:ShaderInterface;
  _SlideShader:ShaderInterface;

Function GetShader_UIFade:TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
  Line('  attribute vec4 terra_position;');
  Line('  attribute vec4 terra_UV0;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying mediump vec4 texCoord;');
	Line('void main()	{');
  Line('  texCoord = terra_UV0;');
  Line('  gl_Position = projectionMatrix * terra_position;}');
	Line('}');
  Line('fragment {');
	Line('  uniform sampler2D texture;');
	Line('  uniform highp float alpha;');
  Line('  uniform lowp vec4 fadeColor;');
  Line('  varying mediump vec4 texCoord;');
  Line('	void main()	{');
  Line('    highp float t = texture2D(texture, texCoord.xy).a;');
  Line('    highp float p; if (t<alpha) p = 0.0; else p = 1.0;');
  Line('    gl_FragColor = vec4(fadeColor.rgb, p);}');
  Line('}');
  Result := S;
End;

Function GetShader_UISlide:TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
  Line('  attribute vec4 terra_position;');
  Line('  attribute vec4 terra_UV0;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying mediump vec4 texCoord;');
	Line('void main()	{');
  Line('  texCoord = terra_UV0;');
  Line('  gl_Position = projectionMatrix * terra_position;}');
	Line('}');
  Line('fragment {');
	Line('  uniform sampler2D texture;');
  Line('  varying mediump vec4 texCoord;');
  Line('	void main()	{');
  Line('    highp vec4 p = texture2D(texture, texCoord.xy);');
  Line('    gl_FragColor = p;}');
  Line('}');
  Result := S;
End;

{ UITransition }
Procedure UITransition.InitVertices(OfsX, OfsY:Single);
Var
  W,H:Single;
  P:Vector2D;
  I:Integer;
Begin
  W := UIManager.Instance.Width;
  H := UIManager.Instance.Height;

  If (_FadeVertices = Nil) Then
    _FadeVertices := VertexData.Create(FadeVertexFormat, 6);

  _FadeVertices.SetVector2D(0, vertexUV0, VectorCreate2D(0.0, 0.0));
  _FadeVertices.SetVector2D(1, vertexUV0, VectorCreate2D(0.0, 1.0));
  _FadeVertices.SetVector2D(2, vertexUV0, VectorCreate2D(1.0, 1.0));
  _FadeVertices.SetVector2D(4, vertexUV0, VectorCreate2D(1.0, 0.0));

  _FadeVertices.SetVector3D(0, vertexPosition, VectorCreate(OfsX, OfsY, 0.0));
  _FadeVertices.SetVector3D(1, vertexPosition, VectorCreate(OfsX, OfsY + H, 0.0));
  _FadeVertices.SetVector3D(2, vertexPosition, VectorCreate(OfsX + W, OfsY + H, 0.0));
  _FadeVertices.SetVector3D(4, vertexPosition, VectorCreate(OfsX + W, OfsY, 0.0));

  _FadeVertices.CopyVertex(2, 3);
  _FadeVertices.CopyVertex(0, 5);

  For I:=0 To 5 Do
  Begin
    _FadeVertices.GetVector2D(I, vertexPosition, P);
    P := _Transform.Transform(P);

    _FadeVertices.SetVector3D(I, vertexPosition, VectorCreate(P.X, P.Y, 0.0));
  End;
End;

Procedure UITransition.SetCallback(Callback:FadeCallback; UserData:Pointer = Nil; OnStart:Boolean=False);
Begin
  _Callback := Callback;
  _Arg := UserData;
  _CallOnStart := OnStart;
End;

Procedure UITransition.Release;
Begin
  ReleaseObject(_FadeVertices);
End;

Function UITransition.Update:Boolean;
Var
  Alpha:Single;
Begin
  {$IFDEF DISABLETRANSITIONS}
  _Callback(_Arg);
  Result := False;
  Exit;
  {$ENDIF}

  If Not _Running Then
  Begin
    _Running := True;
    _Time := Application.GetTime + _Delay;
    If (_CallOnStart) And (Assigned(_Callback)) Then
      _Callback(_Arg);
  End;

  Alpha := Application.GetTime;
  If (Alpha>=_Time) Then
  Begin
    Alpha := Alpha - _Time;
    Alpha := Alpha / _Duration;

    If (Alpha>1.0) Then
      Alpha := 1.0;
  End Else
    Alpha := 0.0;

  If (Alpha>=0.0) And (Alpha<_FinishValue) Then
  Begin
    _Active := True;

    Result := True;
    Render(Alpha);
  End Else
  Begin
    Render(Alpha);

    If (_Active) Then
    Begin
      _Active := False;
      If (Not _CallOnStart) And (Assigned(_Callback)) Then
        _Callback(_Arg);

      Result := False;
    End Else
      Result := True;
  End;
End;

{ UIFade }
Constructor UIFade.Create(FadeTexture:Texture; Duration, Delay:Cardinal; Invert:Boolean);
Begin
  If Not Assigned(FadeTexture) Then
    Exit;

  FadeTexture.PreserveQuality := True;

  _ID := Random(36242);
  _FinishValue := 1.0;
  _FadeTexture := FadeTexture;
  _Duration := Duration;
  _Delay := Delay;
  _FadeOut := Invert;
  _Active := False;
  Color := ColorBlack;

  If (Not Assigned(_FadeShader)) Then
  Begin
    _FadeShader := GraphicsManager.Instance.Renderer.CreateShader();
    _FadeShader.Generate('ui_fade', GetShader_UIFade());
  End;
End;

Procedure UIFade.Render(Alpha:Single);
Var
  I:Integer;
  X,Y:Single;
  M:Matrix4x4;
  StencilID:Byte;
  Delta:Single;
  _Shader:ShaderInterface;
  Graphics:GraphicsManager;
Begin
  If (_FadeOut) Then
    Alpha := 1.0 - Alpha;

  Delta := GetEase(Alpha, _EaseType);

  Graphics := GraphicsManager.Instance;

  If Not _FadeTexture.IsReady() Then
    Exit;

  _FadeTexture.Bind(0);
  If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
    Graphics.Renderer.SetBlendMode(blendZero)
  Else
    Graphics.Renderer.SetBlendMode(blendBlend);

  _Shader := _FadeShader;

  Graphics.Renderer.BindShader(_Shader);
  //ShaderManager.Instance.Bind(_Shader);

  M := Graphics.ProjectionMatrix;
  Graphics.Renderer.SetModelMatrix(Matrix4x4Identity);
  Graphics.Renderer.SetProjectionMatrix(M);
  _Shader.SetIntegerUniform('texture', 0);
  _Shader.SetFloatUniform('alpha', Delta);
  _Shader.SetColorUniform('fadeColor', Color);

  Self.InitVertices(0, 0);

  Graphics.Renderer.SetDepthTest(False);


  (*If (Not GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable) Then
  Begin
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@M);
    glMatrixMode(GL_MODELVIEW);
    M := Matrix4x4Identity;
    glLoadMatrixf(@M);
    glMatrixMode(GL_TEXTURE);
    glLoadMatrixf(@M);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glVertexPointer(2, GL_FLOAT, SizeOf(FadeVertex), @FadeVertices[0].Position);
    glTexCoordPointer(2, GL_FLOAT, SizeOf(FadeVertex), @FadeVertices[0].UV);
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, Delta);

    StencilID := GraphicsManager.Instance.GenerateStencilID();

    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS, StencilID, $FFFFFFFF);
    glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
    glColorMask(False, False, False, False);
  End Else
  {$ENDIF}
  Begin
  	StencilID := 0;
    glVertexAttribPointer(PositionHandle, 2, GL_FLOAT, False, 16, @(FadeVertices[0].Position));
    glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, 16, @(FadeVertices[0].UV));
  End;

  glVertexAttribPointer(PositionHandle, 2, GL_FLOAT, False, 16, @(FadeVertices[0].Position));
  glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, 16, @(FadeVertices[0].UV));
  *);

{  Graphics.Renderer.SetSourceVertexSize(16);
  Graphics.Renderer.SetAttributeSource('terra_position', vertexPosition, typeVector2D, @(FadeVertices[0].Position));
  Graphics.Renderer.SetAttributeSource('terra_UV0', vertexUV0, typeVector2D, @(FadeVertices[0].UV));}

  Graphics.Renderer.SetVertexSource(_FadeVertices);
  Graphics.Renderer.DrawSource(renderTriangles, 6);

  (*
  If (Not GraphicsManager.Instance.Renderer.Features.Shaders.Avaliable) Then
  Begin
    TextureManager.Instance.WhiteTexture.Bind(0);

    glColorMask(True, True, True, True);
    glStencilFunc(GL_EQUAL, StencilID, $FFFFFFFF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);

    glColor4f(Color.R/255, Color.G/255, Color.B/255, 1.0);
    glDisable(GL_ALPHA_TEST);
    GraphicsManager.Instance.Renderer.SetBlendMode(blendOne);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisable(GL_STENCIL_TEST);
  End;
  *)

  Graphics.Renderer.SetDepthTest(True);
End;

{ UISlide }
Constructor UISlide.Create(Direction: Vector2D; Duration,Delay: Cardinal);
Var
  Src:Image;
Begin
  _Direction := Direction;
  _Duration := Duration;
  _Delay := Delay;
  _Active := False;
  _FinishValue := 1.0;
  _ID := Random(36242);

  {$IFDEF POSTPROCESSING}
  Src := GraphicsManager.Instance.ActiveViewport.GetRenderTarget(captureTargetColor).GetImage();
  _Texture := Texture.Create(rtDynamic, 'ui_slide');
  _Texture.InitFromSize(Src.Width, Src.Height, ColorWhite);
  _Texture.UpdateRect(Src);
  ReleaseObject(Src);
  {$ELSE}
  _Texture := Nil;
  {$ENDIF}

  If Not Assigned(_SlideShader) Then
  Begin
    _SlideShader := GraphicsManager.Instance.Renderer.CreateShader();
    _SlideShader.Generate('ui_slide', GetShader_UISlide()); 
  End;
End;

Procedure UISlide.Release;
Begin
  ReleaseObject(_Texture);
  Inherited;
End;

Procedure UISlide.Render(Alpha: Single);
Var
  X,Y, W, H:Single;
  _Shader:ShaderInterface;
  StencilID:Integer;
  Delta:Single;
  Graphics:GraphicsManager;
  I:Integer;
  P:Vector2D;
Begin
  Delta := GetEase(Alpha, _EaseType);

  Graphics := GraphicsManager.Instance;

  _Texture.Bind(0);

  _Shader := _SlideShader;

  Graphics.Renderer.BindShader(_Shader);
  //ShaderManager.Instance.Bind(_Shader);
  Graphics.Renderer.SetModelMatrix(Matrix4x4Identity);
  Graphics.Renderer.SetProjectionMatrix(Graphics.ProjectionMatrix);
  _Shader.SetIntegerUniform('texture', 0);

  Graphics.Renderer.SetBlendMode(blendNone);

  W := UIManager.Instance.Width;
  H := UIManager.Instance.Height;

  X := _Direction.X * Delta * W;
  Y := _Direction.Y * Delta * H;

  Self.InitVertices(X, Y);

  Graphics.Renderer.SetDepthTest(False);

{  Graphics.Renderer.SetSourceVertexSize(16);
  Graphics.Renderer.SetAttributeSource('terra_position', vertexPosition, typeVector2D, @(FadeVertices[0].Position));
  Graphics.Renderer.SetAttributeSource('terra_UV0', vertexUV0, typeVector2D, @(FadeVertices[0].UV));}

  Graphics.Renderer.SetVertexSource(_FadeVertices);
  Graphics.Renderer.DrawSource(renderTriangles, 6);

  Graphics.Renderer.SetDepthTest(True);
End;

End.