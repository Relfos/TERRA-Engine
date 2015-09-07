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
 * TERRA_Sprite
 * Implements the global sprite manager
 ***********************************************************************************************************************
}
Unit TERRA_SpriteRenderer;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Color, TERRA_Texture, TERRA_Renderer,
  TERRA_Application, TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_ClipRect,
  TERRA_VertexFormat, TERRA_Sprite;

Const
  MaxSpriteShaders = 64;

Type
  TERRASpriteRenderer = Class;

  SpriteBatch = Class(TERRAObject)
    Protected
      _ID:Cardinal;
      _First:TERRASprite;
      _SpriteCount:Integer;
      _Texture:TERRATexture;

      _DissolveTexture:TERRATexture;
      _DissolveValue:Single;

      _Pattern:TERRATexture;

      _Manager:TERRASpriteRenderer;

      {$IFNDEF DISABLECOLORGRADING}
      _ColorTable:TERRATexture;
      {$ENDIF}

      _BlendMode:Integer;
      _Layer:Single;
      _Saturation:Single;
      _Glow:ColorRGBA;
      _Outline:ColorRGBA;
      _Smoothing:Single;

      _ShaderID:Cardinal;
      _Vertices:VertexData;

      _RequiredVertices:Integer;
      _RequiredIndices:Integer;

      _Indices:Array Of Word;
      _IndexCount:Integer;

      _Ready:Boolean;

      Procedure AddSprite(P:TERRASprite);

      Procedure Prepare();
      Procedure Render(Const ProjectionMatrix, TransformMatrix:Matrix4x4; Stage:RendererStage);
      Procedure Clear();

    Public
      Constructor Create(ID:Cardinal; Manager:TERRASpriteRenderer);
      Procedure Release(); Override;
      //Procedure SetupSaturationCombiners(Var Slot:Integer);
  End;

  TERRASpriteRenderer = Class(TERRAObject)
    Protected
      _Index:Integer;
      _SpriteCount:Integer;
      _Sprites:Array Of TERRASprite;

      _Batches:Array Of SpriteBatch;
      _BatchCount:Integer;

   Public
      Procedure InitBatches();
      Procedure Release; Override;

      Procedure Prepare;
      Procedure Render(Const ProjectionMatrix, TransformMatrix:Matrix4x4; Stage:RendererStage);
      Procedure Clear;

      Procedure QueueSprite(S:TERRASprite);

      { Fetches a temporary sprite that will be disposed in the next frame }
      Function FetchSprite():TERRASprite; //Const Layer:Single; SpriteTexture:TERRATexture; ColorTable:TERRATexture = Nil; BlendMode:Integer = blendBlend;  Saturation:Single = 1.0; Filter:TextureFilterMode = filterLinear; Shader:ShaderInterface = Nil):TERRASprite;
  End;


Implementation
Uses TERRA_ResourceManager, TERRA_Engine, TERRA_InputManager, TERRA_GraphicsManager, TERRA_Log, TERRA_Image, TERRA_OS, TERRA_Math, TERRA_Font, TERRA_ShaderManager
  {$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Const
  MaxVerticesPerBatch = 1024 * 32;

Var
  _SpriteShaders:Array[0..Pred(MaxSpriteShaders)] Of ShaderInterface;


{ TERRASpriteRenderer }
Procedure TERRASpriteRenderer.InitBatches;
Var
    I:Integer;
Begin
  _SpriteCount := 64;
  SetLength(_Sprites, _SpriteCount);
  For I:=0 To Pred(_SpriteCount) Do
    _Sprites[I] := TERRASprite.Create();
  _Index := -1;

  _BatchCount := 50;
  SetLength(_Batches, _BatchCount);
  For I:=0 To Pred(_BatchCount) Do
    _Batches[I] := SpriteBatch.Create(Succ(I), Self);
End;

Procedure TERRASpriteRenderer.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SpriteCount) Do
    ReleaseObject(_Sprites[I]);
  _SpriteCount := 0;

  For I:=0 To Pred(_BatchCount) Do
    ReleaseObject(_Batches[I]);
  _BatchCount := 0;

  For I:=0 To Pred(MaxSpriteShaders) Do
    ReleaseObject(_SpriteShaders[I]);
End;

//Function TERRASpriteRenderer.DrawSprite(Const Layer:Single; SpriteTexture:TERRATexture; ColorTable:TERRATexture; BlendMode:Integer;  Saturation:Single; Filter:TextureFilterMode; Shader:ShaderInterface):QuadSprite;
Function TERRASpriteRenderer.FetchSprite():TERRASprite;
Var
  I:Integer;
Begin
  If (_SpriteCount<=0) Then
    Self.InitBatches();

(*  {$IFDEF WINDOWS}
  If (Layer<0) Then
    DebugBreak;
  {$ENDIF}*)

  Inc(_Index);
  If (_Index>=_SpriteCount) Then
  Begin
    _SpriteCount := _SpriteCount * 2;
    SetLength(_Sprites, _SpriteCount);
    For I:=_Index To Pred(_SpriteCount) Do
        _Sprites[I] := TERRASprite.Create();
  End;

  If (_Sprites[_Index] = Nil) Then
    _Sprites[_Index] := TERRASprite.Create();

  Result := _Sprites[_Index];

  Result.Clear();
  Result.Reset();

(*  Result.Layer := Layer;
  Result.Rect.Width := 0;
  Result.Rect.Height := 0;
  {$IFNDEF DISABLECOLORGRADING}
  Result.ColorTable := ColorTable;
  {$ENDIF}

  Result.Flip := False;
  Result.Mirror := False;
  Result.ClipRect.Style := clipNothing;
  Result.Saturation := Saturation;
  Result.Shader := Shader;*)

(*  SpriteTexture.Filter := Filter;
//  SpriteTexture.Wrap := True;
  SpriteTexture.MipMapped := False;

  Result.SetColor(ColorWhite);
  Result.Texture := SpriteTexture;
  Result.BlendMode := BlendMode;*)

//  Self.QueueSprite(Result);
End;

Procedure TERRASpriteRenderer.QueueSprite(S:TERRASprite);
Var
  N, I:Integer;
  TargetLayer:Single;
  HasShaders, ResetBatch:Boolean;
Begin
  If (S = Nil) Then
    Exit;

  If (S.Texture = Nil) Or (S.BatchID>0) Or (S.IndexCount<=0) Then
    Exit;

  If (_BatchCount<=0) Then
    Self.InitBatches();

  HasShaders := Engine.Graphics.Renderer.Features.Shaders.Avaliable;

  ResetBatch := True;

  TargetLayer := S.Layer * 0.01;
  N := -1;

  For I:=0 To Pred(_BatchCount) Do
  If (_Batches[I]._Texture = S.Texture) And (_Batches[I]._Pattern = S.Pattern)
  And (_Batches[I]._DissolveTexture = S.DissolveTexture) And (_Batches[I]._DissolveValue = S.DissolveValue) And (_Batches[I]._BlendMode = S.BlendMode)
  {$IFNDEF DISABLECOLORGRADING}And (_Batches[I]._ColorTable = S.ColorTable){$ENDIF}
  And (_Batches[I]._ShaderID = S.Flags)
  And ( (HasShaders) Or (_Batches[I]._Saturation = S.Saturation))
  And (Cardinal(_Batches[I]._Outline) = Cardinal(S.Outline))
  And (_Batches[I]._RequiredVertices + S.Vertices.Count < MaxVerticesPerBatch)
  And (_Batches[I]._Smoothing = S.Smoothing) And (_Batches[I]._Layer = TargetLayer) Then
  Begin
    N := I;
    ResetBatch := False;
    Break;
  End;
  
  If (N<0) Then
  Begin
    For I:=0 To Pred(_BatchCount) Do
    If (_Batches[I]._SpriteCount <=0) Then
    Begin
      N := I;
      Break;
    End;
  End;

  If (N<0) Then
  Begin
    N := _BatchCount;
    Inc(_BatchCount);
    SetLength(_Batches, _BatchCount);
  End;

  If ResetBatch Then
  Begin
    _Batches[N]._SpriteCount := 0;
    _Batches[N]._BlendMode := S.BlendMode;
    _Batches[N]._Texture := S.Texture;
    _Batches[N]._Pattern  := S.Pattern;
    _Batches[N]._DissolveTexture  := S.DissolveTexture;
    _Batches[N]._DissolveValue  := S.DissolveValue;
    _Batches[N]._Layer := TargetLayer;
    {$IFNDEF DISABLECOLORGRADING}
    _Batches[N]._ColorTable := S.ColorTable;
    {$ENDIF}
    _Batches[N]._Saturation := S.Saturation;
    _Batches[N]._Glow := S.Glow;
    _Batches[N]._Smoothing := S.Smoothing;
    _Batches[N]._ShaderID := S.Flags;
    _Batches[N]._Outline := S.Outline;
    _Batches[N]._First := Nil;
    _Batches[N]._Manager := Self;
    _Batches[N]._RequiredVertices := 0;
    _Batches[N]._RequiredIndices := 0;
  End;

  _Batches[N].AddSprite(S);
End;

Procedure TERRASpriteRenderer.Prepare;
Var
  I:Integer;
  Graphics:GraphicsManager;
Begin
  Graphics := Engine.Graphics;

(*  If (_SpriteShaderWithoutGrading = Nil) Then
  Begin
    _SpriteShaderWithoutGrading := Graphics.Renderer.CreateShader();
    _SpriteShaderWithoutGrading.Generate('Sprite', GetShader_Sprite(False, False, False));
  End;

  If (_SpriteShaderSolid = Nil) Then
  Begin
    _SpriteShaderSolid := Graphics.Renderer.CreateShader();
    _SpriteShaderSolid.Generate('SpriteSolid', GetShader_Sprite(False, False, True));
  End;


  {$IFNDEF DISABLECOLORGRADING}
  If (_SpriteShaderWithGrading = Nil) Then
  Begin
    _SpriteShaderWithGrading := Graphics.Renderer.CreateShader();
    _SpriteShaderWithGrading.Generate('SpriteGrading', GetShader_Sprite(True, False, False));
  End;
  {$ENDIF}

  If (_FontShader = Nil) Then
  Begin
    _FontShader := Graphics.Renderer.CreateShader();
    _FontShader.Generate('Font', GetShader_Sprite(False, True, False));
  End;*)

  For I:=0 To Pred(_BatchCount) Do
  Begin
    _Batches[I].Prepare();
  End;

  _Index := -1;
End;

Procedure TERRASpriteRenderer.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_BatchCount) Do
  If (Assigned(_Batches[I]._First)) Then
  Begin
    _Batches[I].Clear();
  End;

  _Index := -1;
End;

Procedure TERRASpriteRenderer.Render(Const ProjectionMatrix, TransformMatrix:Matrix4x4; Stage:RendererStage);
Var
  I,K:Integer;
  Min:Single;
  Total, Index, Count:Integer;
  M:Matrix4x4;
  Graphics:GraphicsManager;
Begin
  Graphics := Engine.Graphics;
  Graphics.Renderer.SetBlendMode(blendNone);

  //glEnable(GL_DEPTH_TEST); {FIXME}
  Graphics.Renderer.SetDepthFunction(compareLessOrEqual);

  Graphics.Renderer.SetCullMode(cullNone);
//  Graphics.Renderer.SetColorMask(True, True, True, False);

  (*If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    Projection := Graphics.ProjectionMatrix;

    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@Projection);

    M := Matrix4x4Identity;

    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@M);

    glMatrixMode(GL_TEXTURE);
    glLoadMatrixf(@M);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  End;
  BIBI
  *)


  Total := 0;
  For I:=0 To Pred(_BatchCount) Do
  If (Assigned(_Batches[I]._First)) And (_Batches[I]._Ready) Then
    Inc(Total);

  Count := 0;
  While (Total>0) Do
  Begin
    Index := -1;
    Min := 9999;

    For I:=0 To Pred(_BatchCount) Do
    If (_Batches[I]._Ready) And (_Batches[I]._Layer<Min) Then
    Begin
      Min := _Batches[I]._Layer;
      Index := I;
    End;

    If (Index>=0) Then
    Begin
      _Batches[Index].Render(ProjectionMatrix, TransformMatrix, Stage);
      Dec(Total);

      Inc(Count); //If Count>1 Then break;
    End Else
    Break;
  End;


  Graphics.Renderer.SetDepthTest(True);    //BIBI

  (*If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;*)

  For I:=0 To Pred(_BatchCount) Do
    _Batches[I]._Ready := (_Batches[I]._SpriteCount>0);
End;

{ SpriteBatch }
Constructor SpriteBatch.Create(ID:Cardinal; Manager:TERRASpriteRenderer);
Begin
  _Manager := Manager;
  _ID := ID;
  _Vertices := CreateSpriteVertexData(128);
  SetLength(_Indices, 4);
End;

Procedure SpriteBatch.AddSprite(P:TERRASprite);
Var
  S, Prev:TERRASprite;
Begin
  If (P = Nil) Or (P.Vertices=Nil) Then
    Exit;

  Inc(_SpriteCount);
  P.Next := _First;
  P.BatchID := Self._ID;
  _First := P;

  Inc(_RequiredVertices, P.Vertices.Count);
  Inc(_RequiredIndices, P.IndexCount);
End;

Procedure SpriteBatch.Release;
Begin
  ReleaseObject(_Vertices);
  SetLength(_Indices, 0);
  _IndexCount := 0;
End;

Procedure SpriteBatch.Clear();
Begin
  _SpriteCount := 0;
  _IndexCount := 0;
  _Texture := Nil;
  _First := Nil;
End;

Procedure SpriteBatch.Render(Const ProjectionMatrix, TransformMatrix:Matrix4x4; Stage:RendererStage);
Var
  Graphics:GraphicsManager;
  TargetShader:ShaderInterface;
Begin
  _Ready := False;

  If (_SpriteCount<=0) Then
  Begin
    _First := Nil;
    Exit;
  End;

  Graphics := Engine.Graphics;

{  Graphics.Renderer.SetSourceVertexSize(SizeOf(SpriteVertex));
  Graphics.Renderer.SetAttributeSource(TERRA_POSITION_ATTRIBUTE, typeVector3D,  @_Vertices[0].Position);
  Graphics.Renderer.SetAttributeSource(TERRA_UV0_ATTRIBUTE, typeVector2D, @_Vertices[0].TexCoord);
  Graphics.Renderer.SetAttributeSource(TERRA_COLOR_ATTRIBUTE, typeColor,  @_Vertices[0].Color);

  If (_Saturation<1.0) Then
    Graphics.Renderer.SetAttributeSource(TERRA_SATURATION_ATTRIBUTE, typeFloat, @_Vertices[0].Saturation);}


  If (_SpriteShaders[Self._ShaderID] = Nil) Then
  Begin
    _SpriteShaders[Self._ShaderID] := Graphics.Renderer.CreateShader();
    _SpriteShaders[Self._ShaderID].Generate('sprite_'+CardinalToString(Self._ShaderID), GetShader_Sprite(_ShaderID));
  End;

  TargetShader := _SpriteShaders[Self._ShaderID];
  Graphics.Renderer.BindShader(TargetShader);

  If (_ShaderID And Sprite_GUI = 0) Then
    Graphics.Renderer.SetModelMatrix(TransformMatrix);

  Graphics.Renderer.SetProjectionMatrix(ProjectionMatrix);

  If (_ShaderID And Sprite_SolidColor = 0) Then
  Begin
    TargetShader.SetIntegerUniform('texture', 0);
    _Texture.Bind(0);
  End;

  If (_ShaderID And Sprite_Dissolve<>0) Then
  Begin
    TargetShader.SetIntegerUniform('dissolve_texture', 1);
    _DissolveTexture.WrapMode := wrapNothing;
    _DissolveTexture.Filter := filterLinear;
    _DissolveTexture.Bind(1);
    TargetShader.SetFloatUniform('dissolve_value', _DissolveValue);
  End;

  If (_ShaderID And Sprite_Pattern<>0) Then
  Begin
    TargetShader.SetIntegerUniform('pattern_texture', 1);
    _Pattern.Bind(1);
  End;

  If (_ShaderID And Sprite_Font<>0) Then
  Begin
    TargetShader.SetColorUniform('outlineColor', _Outline);
    TargetShader.SetVec2Uniform('shadowOffset', Vector2D_Create(0.1, 0.1));
    TargetShader.SetFloatUniform('smoothing', Self._Smoothing); //);
  End Else
  {$IFNDEF DISABLECOLORGRADING}
    ColorTableBind(_ColorTable, 1);
  {$ENDIF}

  Graphics.Renderer.SetBlendMode(_BlendMode);
//  Ratio := UIManager.Instance.Ratio;

  Graphics.Renderer.SetVertexSource(_Vertices);
  Graphics.Renderer.DrawIndexedSource(renderTriangles, _IndexCount, @_Indices[0]);

  (*
  If (Not Graphics.Renderer.Features.Shaders.Avaliable) Then
  Begin
    If (Slot>0) Then
    Begin
      Slot := Graphics.Renderer.Features.MaxTextureUnits;
      For I:=Pred(Slot) DownTo 1 Do
      Begin
        glActiveTexture(GL_TEXTURE0 + I);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
        glDisable(GL_TEXTURE_2D);
      End;
    End;
  End;
  BIBI *)
End;


Procedure SpriteBatch.Prepare();
Var
  I, J:Integer;
  S:TERRASprite;
  K:Single;
  MaxX,MinX, MaxY,MinY:Single;
  M:Matrix3x3;
  C:ColorRGBA;
  InIt, OutIt:VertexIterator;
  Src, Dest:SpriteVertex;
  FullyClipped:Boolean;
  Ratio:Single;
  BaseID:Integer;
  Pos:Vector3D;
  Graphics:GraphicsManager;
  CurrentClip:Vector4D;
Begin
  If (_SpriteCount<=0) Then
  Begin
    _First := Nil;
    Exit;
  End;

  _RequiredVertices := 0;
  S := _First;
  While Assigned(S) Do
  Begin
    Inc(_RequiredVertices, S.Vertices.Count);
    S := S.Next;
  End;

  If (_Vertices.Count<_RequiredVertices) Then
    _Vertices.Resize(_RequiredVertices);

  SetLength(_Indices, _RequiredIndices);

  Graphics := Engine.Graphics;

  OutIt := _Vertices.GetIteratorForClass(SpriteVertex);

  BaseID := 0;
  Self._IndexCount := 0;
  S := _First;
  While Assigned(S) Do
  Begin
    S.BatchID := 0;
    
    If (S.ClipRect.Style = clipEverything) Or (S.Vertices = Nil) Or (S.Vertices.Count <= 0) Then
    Begin
      S := S.Next;
      Continue;
    End;

    If (S.ClipRect.Style = clipNothing) Then
      CurrentClip := Vector4D_Create(0, 0, 9999, 9999)
    Else
      CurrentClip := Vector4D_Create(S.ClipRect.X1, S.ClipRect.Y1, S.ClipRect.X2, S.ClipRect.Y2);

    MinX := 9999;
    MaxX := -9999;
    MinY := 9999;
    MaxY := -9999;

(*    If Not OutIt.Seek(Self._IndexCount) Then
      Break;*)

    InIt := S.Vertices.GetIteratorForClass(SpriteVertex);
    While (InIt.HasNext()) Do
    Begin
      If (Not OutIt.HasNext()) Then
      Begin
        Log(logWarning, 'Sprite', 'Failed batch merge of sprite vertices...');
        Break;
      End;

      Src := SpriteVertex(InIt.Value);
      Dest := SpriteVertex(OutIt.Value);

      Pos := Src.Position;

      Pos := S.Transform.Transform(Pos);

      //Pos.X := Trunc(Pos.X);
      //Pos.Y := Trunc(Pos.Y);
      //Pos.Z := Pos.Z + S.Layer;

      Dest.Position := Pos;
      Dest.Saturation := S.Saturation;
      Dest.Glow := S.Glow;
      Dest.TexCoord := Src.TexCoord;
      Dest.Color := Src.Color;

      Dest.ClipRect := CurrentClip;

      MinX := FloatMin(MinX, Pos.X);
      MinY := FloatMin(MinY, Pos.Y);
      MaxX := FloatMax(MaxX, Pos.X);
      MaxY := FloatMax(MaxY, Pos.Y);
    End;
    ReleaseObject(InIt);

    For I:=0 To Pred(S.IndexCount) Do
      Self._Indices[Self._IndexCount + I] := S.GetIndex(I) + BaseID;

    Inc(BaseID, S.Vertices.Count);
    Inc(Self._IndexCount, S.IndexCount);

    S := S.Next;
  End;
  ReleaseObject(OutIt);

  _Ready := True;
End;

End.
