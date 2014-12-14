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
 * TERRA_VertexBufferObject
 * Implements a vertex buffer class
 ***********************************************************************************************************************
}
Unit TERRA_VertexBufferObject;

{$I terra.inc}

{-$DEFINE MAPBUFFERS}

{$DEFINE VBO}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF} TERRA_Utils;

Type
  TriangleEdgesState = Record
    Visible:Array[0..2] Of Boolean;
  End;

  PTriangleEdgesStateArray = ^TriangleEdgesStateArray;
  TriangleEdgesStateArray = Array[0..1024*64] Of TriangleEdgesState;

  Attribute = Record
    Name:AnsiString;
    Handle:Integer;
    Count:Integer;
    Size:Integer;
    Format:Integer;
    Normalized:Boolean;
    Offset:PtrUInt;
  End;

  VBO = Class(TERRAObject)
    Protected
      _Handle:Integer;
      _Dynamic:Boolean;
      _Buffer:Pointer;
      _VertexCount:Integer;
      _VertexSize:Integer;
      _TriangleCount:Integer;
      _EdgeCount:Integer;
      _IndexList:PTriangleArray;
      _EdgeList:PTriangleEdgesStateArray;
      _WireframeIndices:PWordArray;
      _CurrentOffset:Integer;

      _Attributes:Array Of Attribute;
      _AttributeCount:Integer;

      Procedure Build;

    Public
      Constructor Create(VertexData, IndexData, EdgeData:Pointer; VertexCount, TriangleCount:Integer; VertexSize:Integer; DynamicUsage:Boolean);
      Destructor Destroy; Override;

      Procedure AddAttribute(Name:AnsiString; Count:Integer; Format:Integer; Normalized:Boolean; Skip:Boolean = False);

      Procedure Draw(Wireframe:Boolean);

      Procedure Update(Data:PByte);

      Procedure SetIndexList(IndexList:Pointer; TriangleCount:Integer);

      (*Function Lock:Pointer;
      Procedure Unlock;*)
  End;



Implementation
Uses TERRA_GraphicsManager, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Shader, TERRA_Log;

{ VBO }
Constructor VBO.Create(VertexData, IndexData, EdgeData:Pointer; VertexCount, TriangleCount:Integer; VertexSize:Integer; DynamicUsage:Boolean);
Begin
  _IndexList := IndexData;
  _Buffer := VertexData;
  _EdgeList := EdgeData;
  _Dynamic := DynamicUsage;
  _TriangleCount := TriangleCount;
  _VertexCount := VertexCount;
  _VertexSize := VertexSize;
  _AttributeCount := 0;
  _CurrentOffset := 0;
  _EdgeCount := 0;
  _WireframeIndices := Nil;
End;

Procedure VBO.AddAttribute(Name:AnsiString; Count:Integer; Format:Integer; Normalized, Skip:Boolean);
Var
  Size:Integer;
Begin
  Case Format Of
  GL_FLOAT: Size := Count * 4;
  GL_UNSIGNED_BYTE: Size := Count;
  {$IFNDEF ANDROID}
  GL_UNSIGNED_INT:  Size := Count * 4;
  {$ENDIF}
  Else
    Log(logWarning, 'VBO', 'Invalid VBO attribute type ['+IntToString(Format)+']');
  End;

  If (Not Skip) Then
  Begin
    Inc(_AttributeCount);
    SetLength(_Attributes, _AttributeCount);
    _Attributes[Pred(_AttributeCount)].Name := Name;
    _Attributes[Pred(_AttributeCount)].Size := Size;
    _Attributes[Pred(_AttributeCount)].Count := Count;
    _Attributes[Pred(_AttributeCount)].Format := Format;
    _Attributes[Pred(_AttributeCount)].Normalized := Normalized;
    _Attributes[Pred(_AttributeCount)].Handle := -1;
    _Attributes[Pred(_AttributeCount)].Offset := _CurrentOffset;
  End;

  Inc(_CurrentOffset, Size);
End;

Procedure VBO.Build;
Var
  Index:Single;
  I, N:Integer;
  Flags:Integer;
  P:Pointer;
Begin
  If _Dynamic Then
    Flags := GL_DYNAMIC_DRAW
  Else
    Flags := GL_STATIC_DRAW;

  {$IFDEF VBO}
  glGenBuffers(1, @_Handle);                              
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);                 
  glBufferData(GL_ARRAY_BUFFER, _VertexSize * _VertexCount, _Buffer, Flags); 
  glBindBuffer(GL_ARRAY_BUFFER, 0);                            
  {$ENDIF}
End;

Destructor VBO.Destroy;
Begin
  {$IFDEF VBO}
  If (_Handle<>0) Then
  Begin
    glDeleteBuffers(1, @_Handle); 
    _Handle := 0;
  End;
  {$ENDIF}

  If Assigned(_WireframeIndices) Then
  Begin
    FreeMem(_WireframeIndices);
    _WireframeIndices := Nil;
  End;
End;

Procedure VBO.Draw(Wireframe:Boolean);
Var
  S:Shader;
  I, J, K, Ofs:Integer;
Begin
  If (_Handle = 0) Or (_TriangleCount<=0) Then
    Build;

  If (Wireframe) And (_WireframeIndices = Nil) Then
  Begin
    If (Assigned(_EdgeList)) Then
    Begin
      _EdgeCount := 0;
      For I:=0 To Pred(_TriangleCount) Do
        For J:=0 To 2 Do
        If (_EdgeList[I].Visible[J]) Then
          Inc(_EdgeCount);
    End Else
      _EdgeCount := 3 * _TriangleCount;

    GetMem(_WireframeIndices, SizeOf(Word) * 2 * _EdgeCount);
    Ofs := 0;
    For I:=0 To Pred(_TriangleCount) Do
      For J:=0 To 2 Do
      If (_EdgeList = Nil) Or (_EdgeList[I].Visible[J]) Then
      Begin
        K := (J+1) Mod 3;
        _WireframeIndices[Ofs] := _IndexList[I].Indices[J]; Inc(Ofs);
        _WireframeIndices[Ofs] := _IndexList[I].Indices[K]; Inc(Ofs);
      End;
  End;

  S := ShaderManager.Instance.ActiveShader;
  If Not Assigned(S) Then
  Begin
    Log(logDebug, 'VBO', 'No shader!');
    Exit;
  End;

  For I:=0 To Pred(_AttributeCount) Do
  Begin
    _Attributes[I].Handle := S.GetAttribute(_Attributes[I].Name);
    {If (_Attributes[I].Handle<0) Then
      Log(logDebug, 'VBO', 'Attribute '+_Attributes[I].Name+' is missing from the shader.');}
  End;

  {$IFDEF VBO}
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);
  {$ELSE}
  Ofs := Cardinal(_Buffer);
  {$ENDIF}

  For I:=0 To Pred(_AttributeCount) Do
  If (_Attributes[I].Handle>=0) Then
    glVertexAttribPointer(_Attributes[I].Handle, _Attributes[I].Count, _Attributes[I].Format, _Attributes[I].Normalized, _VertexSize, Pointer(_Attributes[I].Offset));

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Mesh', 'glDrawElements: '+IntToString(_TriangleCount*3));
  {$ENDIF}

  If WireFrame Then
  Begin
    glDrawElements(GL_LINES, _EdgeCount * 2, GL_UNSIGNED_SHORT, @(_WireframeIndices[0]));
  End Else
  Begin
    glDrawElements(GL_TRIANGLES, _TriangleCount * 3, GL_UNSIGNED_SHORT, _IndexList);
    GraphicsManager.Instance.Internal(0 , _TriangleCount);
  End;

  {$IFDEF VBO}
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  {$ENDIF}
End;

(*Function VBO.Lock: Pointer;
Begin
{$IFDEF VBO}
  If (_Handle<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;
{$ENDIF}

{$IFDEF MAPBUFFERS}
{$IFDEF VBO}
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);                                   
  Result := glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);                  
{$ENDIF}
{$ELSE}
  If Not Assigned(_Temp) Then
  Begin
    GetMem(_Temp, _VertexCount * _VertexSize);
    Move(_Buffer^, _Temp^, _VertexCount * _VertexSize);
  End;
  Result := _Temp;
{$ENDIF}
End;

Procedure VBO.Unlock;
Begin
  If (_Handle<=0) Then
    Exit;

{$IFDEF VBO}
{$IFDEF MAPBUFFERS}
  glUnmapBuffer(GL_ARRAY_BUFFER);
{$ELSE}
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);
  glBufferSubData(GL_ARRAY_BUFFER, 0, _VertexCount * _VertexSize, _Temp);
{$ENDIF}

  glBindBuffer(GL_ARRAY_BUFFER, 0);
{$ENDIF}
End;*)

Procedure VBO.Update(Data:PByte);
Begin
  If (_Handle<=0) Then
    Exit;

  glBindBuffer(GL_ARRAY_BUFFER, _Handle);
  glBufferSubData(GL_ARRAY_BUFFER, 0, _VertexCount * _VertexSize, Data);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
End;

Procedure VBO.SetIndexList(IndexList: Pointer; TriangleCount:Integer);
Begin
  _IndexList := IndexList;
  _TriangleCount := TriangleCount;
End;

End.
