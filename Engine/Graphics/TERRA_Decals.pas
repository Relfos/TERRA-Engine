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
 * TERRA_Decals
 * Implements decal rendering (eg: footprints, tire trails)
 ***********************************************************************************************************************
}
Unit TERRA_Decals;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_Color, TERRA_Vector2D, TERRA_Vector3D,
  TERRA_TextureAtlas, TERRA_Texture, TERRA_Renderer, TERRA_Matrix4x4, TERRA_MeshFilter, TERRA_Lights,
  TERRA_Viewport, TERRA_VertexFormat;

Const
  DecalDecayDuration = 2000;

Type
  Decal = Record
    Position:Vector3D;
    Normal:Vector3D;
    Size:Single;
    Rotation:Single;
    Color:TERRA_Color.Color;
    Time:Cardinal;
    Life:Cardinal;
    Item:TextureAtlasItem;
  End;

  DecalManager = Class(ApplicationComponent)
    Protected
      _Decals:Array Of Decal;
      _DecalCount:Integer;

      _TextureAtlas:TextureAtlas;
      _NeedTextureAtlasRebuild:Boolean;
      _NeedGeometryRebuild:Boolean;
      _Shader:ShaderInterface;

      _LightBatch:LightBatch;

      _Buffer:VertexData;


      Procedure BuildGeometry;

    Public
      Procedure Init; Override;

      Procedure Release; Override;

      Class Function Instance:DecalManager;


      Function AddDecal(Const TextureName:TERRAString; Position, Normal:Vector3D; DecalColor:Color; Size:Single; Rotation:Single = 0; Duration:Integer=20000):Boolean;

      Procedure Render(View:TERRAViewport);
  End;

Implementation
Uses TERRA_OS, TERRA_Stream, TERRA_FileStream, TERRA_FileUtils, TERRA_BoundingBox,
  TERRA_Image, TERRA_GraphicsManager, TERRA_ShaderFactory, TERRA_Log, TERRA_FileManager;

Var
  _DecalInstance:ApplicationObject;

{ DecalManager }
Function DecalManager.AddDecal(Const TextureName:TERRAString; Position,Normal:Vector3D; DecalColor:Color; Size:Single; Rotation:Single; Duration:Integer):Boolean;
Var
  N:Integer;
  S, Name:TERRAString;
  Src:Stream;
  I:Integer;
  Img:Image;
  Info:ImageClassInfo;
  Item:TextureAtlasItem;
Begin
  Name := GetFileName(TextureName, True);
  Item := _TextureAtlas.Get(Name);
  If Item = Nil Then
  Begin
    Log(logDebug, 'Game', 'Seaching decals');
    S := '';
    I := 0;
    While (S='') And (I<GetImageExtensionCount()) Do
    Begin
      Info := GetImageExtension(I);
      S := FileManager.Instance.SearchResourceFile(Name+'.'+Info.Name);
      Inc(I);
    End;

    If (S<>'') Then
    Begin
      Log(logDebug, 'Game', 'Got '+S);

      Src := FileStream.Open(S);
      Img := Image.Create(Src);
      ReleaseObject(Src);
      Item := _TextureAtlas.Add(Img, S);
      _NeedTextureAtlasRebuild := True;
      ReleaseObject(Img);
    End Else
    Begin
      Result := False;
      Exit;
    End;
  End;

  N := _DecalCount;
  Inc(_DecalCount);
  If (Length(_Decals)<_DecalCount) Then
    SetLength(_Decals, _DecalCount);

  Position.Add(VectorScale(Normal, 0.01));
  _Decals[N].Item := Item;
  _Decals[N].Position := Position;
  _Decals[N].Normal := Normal;
  _Decals[N].Color := DecalColor;
  _Decals[N].Size := Size;
  _Decals[N].Rotation := Rotation;
  _Decals[N].Time := Application.GetTime;
  _Decals[N].Life := Duration;
  _NeedGeometryRebuild := True;
  Result := True;
End;

Procedure DecalManager.BuildGeometry;
Var
  I, Ofs:Integer;
  Angle:Single;
  Pos:Vector3D;
  U1,V1,U2,V2:Single;
Begin
  _Buffer.Resize(_DecalCount * 6);

  Ofs := 0;
  For I:=0 To Pred(_DecalCount) Do
  Begin
    U1 := _Decals[I].Item.U1;
    V1 := _Decals[I].Item.V1;
    U2 := _Decals[I].Item.U2;
    V2 := _Decals[I].Item.V2;

    Angle := _Decals[I].Rotation + (PI/4);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;

    _Buffer.SetVector3D(Ofs + 0, vertexNormal, _Decals[I].Normal);
    _Buffer.SetVector2D(Ofs + 0, vertexUV0, VectorCreate2D(U2, V1));
    _Buffer.SetColor(Ofs + 0, vertexColor, _Decals[I].Color);
    _Buffer.SetVector3D(Ofs + 0, vertexPosition,  Pos);
    _Buffer.CopyVertex(Ofs + 0, Ofs + 5);

    Angle := _Decals[I].Rotation + ((PI/4)*3);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;

    _Buffer.SetVector3D(Ofs + 1, vertexNormal, _Decals[I].Normal);
    _Buffer.SetVector2D(Ofs + 1, vertexUV0, VectorCreate2D(U1, V1));
    _Buffer.SetColor(Ofs + 1, vertexColor, _Decals[I].Color);
    _Buffer.SetVector3D(Ofs + 1, vertexPosition, Pos);

    Angle := _Decals[I].Rotation + ((PI/4)*5);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;

    _Buffer.SetVector3D(Ofs + 2, vertexNormal, _Decals[I].Normal);
    _Buffer.SetVector2D(Ofs + 2, vertexUV0, VectorCreate2D(U1, V2));
    _Buffer.SetColor(Ofs + 2, vertexColor, _Decals[I].Color);
    _Buffer.SetVector3D(Ofs + 2, vertexPosition, Pos);
    _Buffer.CopyVertex(Ofs + 2, Ofs + 3);

    Angle := _Decals[I].Rotation + ((PI/4)*7);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;

    _Buffer.SetVector3D(Ofs + 4, vertexNormal, _Decals[I].Normal);
    _Buffer.SetVector2D(Ofs + 4, vertexUV0, VectorCreate2D(U2, V2));
    _Buffer.SetColor(Ofs + 4, vertexColor, _Decals[I].Color);
    _Buffer.SetVector3D(Ofs + 4, vertexPosition, Pos);

    Inc(Ofs, 6);
  End;
End;

Procedure DecalManager.Init;
Begin
  _NeedTextureAtlasRebuild := False;
  _TextureAtlas := TextureAtlas.Create('decal', 256, 256);
End;

Procedure DecalManager.Release;
Begin
  ReleaseObject(_TextureAtlas);
  _DecalInstance := Nil;
End;

Class Function DecalManager.Instance: DecalManager;
Begin
  If _DecalInstance = Nil Then
    _DecalInstance := InitializeApplicationComponent(DecalManager, TextureManager);

  Result := DecalManager(_DecalInstance.Instance);
End;


Procedure DecalManager.Render(View:TERRAViewport);
Var
  I:Integer;
  T, OutFlags, FxFlags:Cardinal;
  CutOff, LightCount:Integer;
  Delta, Alpha:Single;
  Center:Vector3D;
  Box:BoundingBox;
  Slot:Integer;
  Graphics:GraphicsManager;
Begin
  If (_DecalCount<=0) Then
    Exit;

  I := 0;
  T := Application.GetTime;

  Center := View.Camera.Position;
  Box.StartVertex := VectorAdd(Center, VectorCreate(-100, -100, -100));
  Box.EndVertex := VectorAdd(Center, VectorCreate(100, 100, 100));

  While (I<_DecalCount) Do
  Begin
    Delta := T - _Decals[I].Time;
    If (Delta>_Decals[I].Life) And (_Decals[I].Life>0) Then
    Begin
      _Decals[I] := _Decals[Pred(_DecalCount)];
      Dec(_DecalCount);
    End Else
    Begin
      CutOff := _Decals[I].Life - DecalDecayDuration;
      If (Delta>=CutOff) Then
      Begin
        Delta := Delta - CutOff;
        Alpha := (Delta/DecalDecayDuration);
        Alpha := (1-Alpha)*255;
        If (_Decals[I].Color.A>Alpha) Then
        Begin
          _Decals[I].Color.A := Trunc(Alpha);
          _NeedGeometryRebuild := True;
        End;
      End;

      Center.Add(_Decals[I].Position);
      Inc(I);
    End;
  End;

  Center.Scale(1/_DecalCount);

  If (_NeedTextureAtlasRebuild) Then
  Begin
    _TextureAtlas.Update;
    _NeedTextureAtlasRebuild := False;
  End;

  FxFlags := shaderAlphaTest;
  OutFlags := 0;

  LightManager.Instance.SortLights(Center, Box, _LightBatch);
  _Shader := ShaderFactory.Instance.GetShader(FxFlags, OutFlags, GraphicsManager.Instance.Renderer.Settings.FogMode, _LightBatch);
  Slot := 1;
  LightManager.Instance.SetupUniforms(@_LightBatch, Slot);

  Graphics := GraphicsManager.Instance;

  Graphics.Renderer.BindShader(_Shader);

  If (_NeedGeometryRebuild) Then
  Begin
    Self.BuildGeometry();
    _NeedGeometryRebuild := False;
  End;

  View.Camera.SetupUniforms;

  _Shader.SetIntegerUniform('texture0', 0);

  Graphics.Renderer.SetModelMatrix(Matrix4x4Identity);
  Graphics.Renderer.SetTextureMatrix(Matrix4x4Identity);

  _Shader.SetColorUniform('diffuse_color', ColorWhite);
  _Shader.SetFloatUniform('specular_power', 0);
  _Shader.SetFloatUniform('emission_factor', 0);
  _Shader.SetColorUniform('specular_color', ColorWhite);

  Graphics.Renderer.SetBlendMode(blendNone);
  Graphics.Renderer.SetBlendMode(blendBlend);

  Graphics.Renderer.SetDepthMask(False);
  _TextureAtlas.GetTexture(0).Bind(0);

{  Graphics.Renderer.SetAttributeSource(TERRA_POSITION_ATTRIBUTE, typeVector3D, @(_Buffer[0].Position));
  Graphics.Renderer.SetAttributeSource(TERRA_UV0_ATTRIBUTE, typeVector2D, @(_Buffer[0].TextureCoords));
  Graphics.Renderer.SetAttributeSource(TERRA_COLOR_ATTRIBUTE, typeColor, @(_Buffer[0].Color));}


  Graphics.Renderer.SetVertexSource(_Buffer);
  Graphics.Renderer.DrawSource(renderTriangles, _DecalCount * 6);

  Graphics.Renderer.SetDepthMask(True);
End;

End.
