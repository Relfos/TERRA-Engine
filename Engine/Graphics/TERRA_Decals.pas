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
  TERRA_Color, TERRA_Vector3D, TERRA_TextureAtlas, TERRA_Shader, TERRA_Texture,
  TERRA_Matrix4x4, TERRA_MeshFilter, TERRA_Lights, TERRA_Application;

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
      _Shader:Shader;

      _LightBatch:LightBatch;

      _Buffer:Array Of MeshVertex;


      Procedure BuildGeometry;

    Public
      Procedure Init; Override;

      Destructor Destroy; Override;

      Class Function Instance:DecalManager;


      Function AddDecal(TextureName:AnsiString; Position, Normal:Vector3D; DecalColor:Color; Size:Single; Rotation:Single = 0; Duration:Integer=20000):Boolean;

      Procedure Render;
  End;

Implementation
Uses TERRA_OS, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_IO, TERRA_FileIO, TERRA_FileUtils, TERRA_BoundingBox,
  TERRA_Image, TERRA_GraphicsManager, TERRA_ShaderFactory, TERRA_Log, TERRA_FileManager;

Var
  _DecalInstance:ApplicationObject;

{ DecalManager }
Function DecalManager.AddDecal(TextureName:AnsiString; Position,Normal:Vector3D; DecalColor:Color; Size:Single; Rotation:Single; Duration:Integer):Boolean;
Var
  N:Integer;
  S, Name:AnsiString;
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
      Src.Destroy;
      Item := _TextureAtlas.Add(Img, S);
      _NeedTextureAtlasRebuild := True;
      Img.Destroy();
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
  _Decals[N].Time := GetTime;
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
  If Length(_Buffer)<_DecalCount * 6 Then
    SetLength(_Buffer, _DecalCount * 6);

  Ofs := 0;
  For I:=0 To Pred(_DecalCount) Do
  Begin
    U1 := _Decals[I].Item.X / Self._TextureAtlas.Width;
    V1 := _Decals[I].Item.Y / Self._TextureAtlas.Height;
    U2 := (_Decals[I].Item.X + _Decals[I].Item.Buffer.Width) / Self._TextureAtlas.Width;
    V2 := (_Decals[I].Item.Y + _Decals[I].Item.Buffer.Height) / Self._TextureAtlas.Height;

    Angle := _Decals[I].Rotation + (PI/4);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;

    _Buffer[Ofs + 0].Normal := _Decals[I].Normal;
    _Buffer[Ofs + 0].TextureCoords.X := U2;
    _Buffer[Ofs + 0].TextureCoords.Y := V1;
    _Buffer[Ofs + 0].Color := _Decals[I].Color;
    _Buffer[Ofs + 0].Position := Pos;
    _Buffer[Ofs + 5] := _Buffer[Ofs + 0];

    Angle := _Decals[I].Rotation + ((PI/4)*3);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;
    _Buffer[Ofs + 1].Normal := _Decals[I].Normal;
    _Buffer[Ofs + 1].TextureCoords.X := U1;
    _Buffer[Ofs + 1].TextureCoords.Y := V1;
    _Buffer[Ofs + 1].Color := _Decals[I].Color;
    _Buffer[Ofs + 1].Position := Pos;

    Angle := _Decals[I].Rotation + ((PI/4)*5);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;
    _Buffer[Ofs + 2].Normal := _Decals[I].Normal;
    _Buffer[Ofs + 2].TextureCoords.X := U1;
    _Buffer[Ofs + 2].TextureCoords.Y := V2;
    _Buffer[Ofs + 2].Color := _Decals[I].Color;
    _Buffer[Ofs + 2].Position := Pos;
    _Buffer[Ofs + 3] := _Buffer[Ofs + 2];

    Angle := _Decals[I].Rotation + ((PI/4)*7);
    Pos := _Decals[I].Position;
    Pos.X := Pos.X + Cos(Angle) * _Decals[I].Size;
    Pos.Z := Pos.Z - Sin(Angle) * _Decals[I].Size;
    _Buffer[Ofs + 4].Normal := _Decals[I].Normal;
    _Buffer[Ofs + 4].TextureCoords.X := U2;
    _Buffer[Ofs + 4].TextureCoords.Y := V2;
    _Buffer[Ofs + 4].Color := _Decals[I].Color;
    _Buffer[Ofs + 4].Position := Pos;

    Inc(Ofs, 6);
  End;
End;

Procedure DecalManager.Init;
Begin
  _NeedTextureAtlasRebuild := False;
  _TextureAtlas := TextureAtlas.Create('decal', 256, 256);
End;

Destructor DecalManager.Destroy;
Begin
  If Assigned(_TextureAtlas) Then
    _TextureAtlas.Destroy;

  _DecalInstance := Nil;
End;

Class Function DecalManager.Instance: DecalManager;
Begin
  If _DecalInstance = Nil Then
    _DecalInstance := InitializeApplicationComponent(DecalManager, TextureManager);

  Result := DecalManager(_DecalInstance.Instance);
End;


Procedure DecalManager.Render;
Var
  I:Integer;
  T, OutFlags, FxFlags:Cardinal;
  CutOff, LightCount:Integer;
  Delta, Alpha:Single;
  PositionHandle, ColorHandle, UVHandle:Integer;
  Center:Vector3D;
  Box:BoundingBox;
  Slot:Integer;
Begin
  If (_DecalCount<=0) Then
    Exit;

  I := 0;
  T := GetTime;

  Center := GraphicsManager.Instance.ActiveViewport.Camera.Position;
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

  _LightBatch := LightManager.Instance.SortLights(Center, Box);
  _Shader := ShaderFactory.Instance.GetShader(FxFlags, OutFlags, GraphicsManager.Instance.Settings.FogMode, GraphicsManager.Instance.LightModel, _LightBatch);
  Slot := 1;
  LightManager.Instance.SetupUniforms(@_LightBatch, Slot);

  ShaderManager.Instance.Bind(_Shader);

  PositionHandle := _Shader.GetAttribute('terra_position');
  UVHandle := _Shader.GetAttribute('terra_UV0');
  ColorHandle := _Shader.GetAttribute('terra_color');

  If (PositionHandle<0) Then
    Exit;

  If (_NeedGeometryRebuild) Then
  Begin
    Self.BuildGeometry();
    _NeedGeometryRebuild := False;
  End;

  GraphicsManager.Instance.ActiveViewport.Camera.SetupUniforms;

  _Shader.SetUniform('texture0', 0);
  _Shader.SetUniform('modelMatrix', Matrix4x4Identity);
  _Shader.SetUniform('textureMatrix', Matrix4x4Identity);

  _Shader.SetUniform('diffuse_color', ColorWhite);
  _Shader.SetUniform('specular_power', 0);
  _Shader.SetUniform('emission_factor', 0);
  _Shader.SetUniform('specular_color', ColorWhite);

  GraphicsManager.Instance.SetBlendMode(blendNone);
  GraphicsManager.Instance.SetBlendMode(blendBlend);

  glDepthMask(False);                                                               
  _TextureAtlas.GetTexture(0).Bind(0);

  glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(_Buffer[0]), @(_Buffer[0].Position));    
  glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, SizeOf(_Buffer[0]), @(_Buffer[0].TextureCoords));    
  glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(_Buffer[0]), @(_Buffer[0].Color));    

  glDrawArrays(GL_TRIANGLES, 0, _DecalCount * 6);
  GraphicsManager.Instance.Internal(0, _DecalCount * 2);

  glDepthMask(True);                                            
  GraphicsManager.Instance.SetBlendMode(blendNone);
End;

End.
