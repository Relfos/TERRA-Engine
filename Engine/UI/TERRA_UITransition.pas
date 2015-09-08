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
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D, TERRA_Texture,
  TERRA_Image, TERRA_Color, TERRA_Renderer, TERRA_Viewport, TERRA_Tween, TERRA_Sprite,
  TERRA_UIDimension, TERRA_UIWidget, TERRA_UIView;

Const
  TransitionLayer = 99.95;

Type
  UITransitionState = (
    UITransition_Waiting,
    UITransition_FadingOut,
    UITransition_FadingIn,
    UITransition_Finished
  );

  UITransition = Class(UIWidget)
    Protected
      _Time:Cardinal;
      _Duration:Integer;
      _Delay:Integer;

      _TransitionState:UITransitionState;
      _TransitionValue:Single;

      _EaseType:TweenEaseType;
      _Invert:Boolean;

      Procedure Render(View:TERRAViewport; Const Stage:RendererStage); Override;

    Public
      Constructor Create(Owner:UIView; Const Duration:Cardinal; Const Delay:Cardinal = 0; Const Invert:Boolean = False);

      Property Duration:Integer Read _Duration;
      Property EaseType:TweenEaseType Read _EaseType Write _EaseType;
  End;

  UIFade = Class(UITransition)
    Protected
      _FadeTexture:TERRATexture;

      Procedure UpdateSprite(View:TERRAViewport); Override;

    Public
      Constructor Create(View:UIView; FadeTexture:TERRATexture; Const Duration:Cardinal; Const Delay:Cardinal = 0; Const Invert:Boolean = False);
  End;

  UISlide = Class(UITransition)
    Protected
      _View:TERRAViewport;
      _Direction:Vector2D;
      _Texture:TERRATexture;

      Procedure UpdateSprite(View:TERRAViewport); Override;

    Public
      Constructor Create(View:UIView; TargetView:TERRAViewport; Direction:Vector2D; Duration, Delay:Cardinal);
      Procedure Release; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_ResourceManager, TERRA_Resource, TERRA_Engine;


{ UITransition }
Constructor UITransition.Create(Owner: UIView; Const Duration:Cardinal; Const Delay:Cardinal; Const Invert:Boolean);
Begin
  Inherited Create('fade', Owner);

  _Duration := Duration;
  _Delay := Delay;
  _Invert := Invert;
  _TransitionState := UITransition_Waiting;

  If Assigned(Owner) Then
    Owner.Modal := Self;
End;

Procedure UITransition.Render(View:TERRAViewport; Const Stage:RendererStage);
Var
  Alpha:Single;
Begin
  {$IFDEF DISABLETRANSITIONS}
  _Callback(_Arg);
  Result := False;
  Exit;
  {$ENDIF}

  If _TransitionState = UITransition_Waiting Then
  Begin
    _Time := Application.GetTime + _Delay;

    Self.TriggerEvent(widgetEvent_Show);

    _TransitionState := UITransition_FadingOut;
  End;

  Alpha := Application.GetTime;
  If (Alpha>=_Time) Then
  Begin
    Alpha := Alpha - _Time;
    Alpha := Alpha / _Duration;
  End Else
    Alpha := 0.0;

  If (Alpha>1.0) Then
  Begin
    Alpha := 1.0;
    
    If _TransitionState = UITransition_FadingOut Then
    Begin
      _Time := Application.GetTime + _Delay;

      Self.TriggerEvent(widgetEvent_Show);

      _TransitionState := UITransition_FadingIn;
    End Else
    If _TransitionState = UITransition_FadingIn Then
    Begin
      _TransitionState := UITransition_Finished;

      Self.TriggerEvent(widgetEvent_Hide);

      Self.Delete;
      Exit;
    End;
  End;

  If ((_TransitionState = UITransition_FadingIn) = _Invert) Then
    Alpha := 1.0 - Alpha;

  _TransitionValue := GetEase(Alpha, _EaseType);

  Inherited Render(View, Stage);
End;

{ UIFade }
Constructor UIFade.Create(View:UIView; FadeTexture:TERRATexture; Const Duration:Cardinal; Const Delay:Cardinal; Const Invert:Boolean);
Begin
  Inherited Create(View, Duration, Delay, Invert);

  Self.Width := UIPercent(100);
  Self.Height := UIPercent(100);

  If Not Assigned(FadeTexture) Then
    FadeTexture := Engine.Textures.WhiteTexture;

  FadeTexture.PreserveQuality := True;
  FadeTexture.Filter := filterLinear;

  _FadeTexture := FadeTexture;

  Color := ColorBlack;
End;

Procedure UIFade.UpdateSprite(View:TERRAViewport);
Var
  I:Integer;
  X,Y:Single;
  StencilID:Byte;
Begin
  _FullSize := CurrentSize;

  If _Sprite = Nil Then
    _Sprite := TERRASprite.Create()
  Else
    _Sprite.Clear();

  _Sprite.Texture := Engine.Textures.WhiteTexture;
  _Sprite.SetDissolve(Self._FadeTexture, Self._TransitionValue);

  _Sprite.Layer := TransitionLayer;

  _Sprite.SetUVs(0, 0, 1, 1);
  _Sprite.SetColor(Self.Color);

  _Sprite.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0,0), 0.0, _FullSize.X, _FullSize.Y);

  _Sprite.ClipRect := Self.ClipRect;
  _Sprite.SetTransform(_Transform);

  If (Not Engine.Graphics.Renderer.Features.Shaders.Avaliable) Then
    _Sprite.BlendMode := blendZero
  Else
    _Sprite.BlendMode := blendBlend;

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

(*  Graphics.Renderer.SetVertexSource(_FadeVertices);
  Graphics.Renderer.DrawSource(renderTriangles, 6);*)

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

  Graphics.Renderer.SetDepthTest(True);
  *)
End;

{ UISlide }
Constructor UISlide.Create(View:UIView; TargetView:TERRAViewport; Direction:Vector2D; Duration,Delay: Cardinal);
Var
  Src:TERRAImage;
Begin
  _View := TargetView;
  _Direction := Direction;
  _Duration := Duration;
  _Delay := Delay;

  {$IFDEF POSTPROCESSING}
  Src := _View.GetRenderTarget(captureTargetColor).GetImage();
  _Texture := TERRATexture.Create(rtDynamic);
  _Texture.InitFromSize(Src.Width, Src.Height, ColorWhite);
  _Texture.UpdateRect(Src);
  ReleaseObject(Src);
  {$ELSE}
  _Texture := Nil;
  {$ENDIF}
End;

Procedure UISlide.Release;
Begin
  ReleaseObject(_Texture);
  Inherited;
End;

Procedure UISlide.UpdateSprite(View:TERRAViewport);
Var
  X,Y, W, H:Single;
  _Shader:ShaderInterface;
  StencilID:Integer;
  Delta:Single;
  I:Integer;
  P:Vector2D;
Begin
(*  Delta := GetEase(Alpha, _EaseType);

  Graphics := Engine.Graphics;

  _Texture.Bind(0);

  _Shader := _SlideShader;

  Graphics.Renderer.BindShader(_Shader);
  //ShaderManager.Instance.Bind(_Shader);
  Graphics.Renderer.SetModelMatrix(Matrix4x4_Identity);

  RaiseError('needs fix');
  Graphics.Renderer.SetProjectionMatrix(View.Camera.Projection);
  _Shader.SetIntegerUniform('texture', 0);

  Graphics.Renderer.SetBlendMode(blendNone);

  W := View.Width;
  H := View.Height;

  X := _Direction.X * Delta * W;
  Y := _Direction.Y * Delta * H;

  Self.InitVertices(X, Y);

  Graphics.Renderer.SetDepthTest(False);

{  Graphics.Renderer.SetSourceVertexSize(16);
  Graphics.Renderer.SetAttributeSource('terra_position', vertexPosition, typeVector2D, @(FadeVertices[0].Position));
  Graphics.Renderer.SetAttributeSource('terra_UV0', vertexUV0, typeVector2D, @(FadeVertices[0].UV));}

  Graphics.Renderer.SetVertexSource(_FadeVertices);
  Graphics.Renderer.DrawSource(renderTriangles, 6);

  Graphics.Renderer.SetDepthTest(True);*)
End;

End.
