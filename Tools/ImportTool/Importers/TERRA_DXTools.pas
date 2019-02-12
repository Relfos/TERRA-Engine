{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_DXTools
 * Implements DirectX mesh utilities
 ***********************************************************************************************************************
}
Unit TERRA_DXTools;

{$I terra.inc}
Interface
Uses Windows, DXTypes, Direct3D9, D3DX9, DXErr9, TERRA_Utils,
 TERRA_Vector3D, TERRA_Vector2D;

Type
  DXMeshVertex = Packed Record
    Position:Vector3D;
    Normal:Vector3D;
    UV:Vector2D;
  End;

  DXMesh = Class(TERRAObject)
    Protected
      _pMesh: ID3DXMesh;                  // Encapsulated D3DX Mesh
      _pLODMesh:ID3DXPMesh;
      _pAdjacency: PDWORD;

      _VertexList:Array Of DXMeshVertex;
      _VertexCount:Integer;
      _IndexList:Array Of Integer;
      _IndexCount:Integer;

      _MinVertexCount:Integer;
      _MaxVertexCount:Integer;

      Procedure ClearMesh;

    Public
      Constructor Create(Handle:HWND);
      Procedure Release;

      Procedure SetVertexCount(Count:Integer);
      Procedure SetTriangleCount(Count:Integer);

      Procedure AddVertex(Index:Integer; Pos,Normal:Vector3D; UV:Vector2D);
      Procedure AddTriangle(Index:Integer; A,B,C:Integer);

      Function GetVertexCount:Integer;
      Function GetTriangleCount:Integer;

      Function GetVertex(Index:Integer):DXMeshVertex;
      Function GetTriangleIndex(TriangleIndex, VertexIndex:Integer):Integer;

      Procedure BuildGeometry;
      Procedure ReadGeometry;
      Procedure WriteGeometry;

      Procedure BuildAtlas(Width,Height:Integer; Callback:ProgressNotifier=Nil);

      Procedure BuildNormals;

      Function GetTriangleAdjancency(Index:Integer):TriangleAdjancency;

      Procedure BuildLOD;
      Procedure SetLOD(VertexCount:Integer);
  End;

Implementation
Uses TERRA_Error, TERRA_Application, TERRA_Unwrap;

Var
  _pd3dDevice: IDirect3DDevice9;      // Direct3D Device object 

Function CreateNULLRefDevice(Handle:HWND): IDirect3DDevice9;
var
  hr: HRESULT;
  pD3D: IDirect3D9;
  Mode: TD3DDisplayMode;
  pp: TD3DPresentParameters;
  pd3dDevice: IDirect3DDevice9;
begin
  Result:= nil;

  pD3D := Direct3DCreate9( D3D_SDK_VERSION);
  if (pD3D = nil) then Exit;

  pD3D.GetAdapterDisplayMode(0, Mode);

  ZeroMemory(@pp, SizeOf(TD3DPresentParameters));
  pp.BackBufferWidth  := 1;
  pp.BackBufferHeight := 1;
  pp.BackBufferFormat := Mode.Format;
  pp.BackBufferCount  := 1;
  pp.SwapEffect       := D3DSWAPEFFECT_COPY;
  pp.Windowed         := True;

  hr := pD3D.CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_NULLREF, Handle,
                          D3DCREATE_HARDWARE_VERTEXPROCESSING, @pp, pd3dDevice);
  pD3D := nil;
  if FAILED(hr) or (pd3dDevice = nil) then
  Begin
    RaiseError('Error creating D3D device');
    Exit;
  End;

  Result:= pd3dDevice;
end;

Const
  // Vertex declaration
  VERTEX_DECL: array[0..3] of TD3DVertexElement9 =
  (
    (Stream: 0; Offset: 0;  _Type: D3DDECLTYPE_FLOAT3; Method: D3DDECLMETHOD_DEFAULT; Usage: D3DDECLUSAGE_POSITION; UsageIndex: 0),
    (Stream: 0; Offset: 12; _Type: D3DDECLTYPE_FLOAT3; Method: D3DDECLMETHOD_DEFAULT; Usage: D3DDECLUSAGE_NORMAL;   UsageIndex: 0),
    (Stream: 0; Offset: 24; _Type: D3DDECLTYPE_FLOAT2; Method: D3DDECLMETHOD_DEFAULT; Usage: D3DDECLUSAGE_TEXCOORD; UsageIndex: 0),
    {D3DDECL_END()}(Stream:$FF; Offset:0; _Type:D3DDECLTYPE_UNUSED; Method:TD3DDeclMethod(0); Usage:TD3DDeclUsage(0); UsageIndex:0)
  );

Constructor DXMesh.Create(Handle:HWND);
Begin
  If Not Assigned(_pd3dDevice) Then
    _pd3dDevice := CreateNULLRefDevice(Handle);
  _pMesh := nil;
End;

Procedure DXMesh.Release;
Begin
  ClearMesh;
  _pMesh := nil;
  _pLODMesh := Nil;
End;

Procedure DXMesh.ClearMesh;
Begin
  If Assigned(_pAdjacency) Then
  Begin
    FreeMem(_pAdjacency);
    _pAdjacency := Nil;
  End;

  If Assigned(_pMesh) then
    _pMesh := Nil;
End;

Procedure DXMesh.SetVertexCount(Count:Integer);
Begin
  _VertexCount := Count;
  SetLength(_VertexList, Count);
End;

Procedure DXMesh.SetTriangleCount(Count:Integer);
Begin
  _IndexCount := Count*3;
  SetLength(_IndexList, Count*3);
End;

Procedure DXMesh.AddVertex(Index:Integer; Pos,Normal:Vector3D; UV:Vector2D);
Begin
  _VertexList[Index].Position := Pos;
  _VertexList[Index].Normal := Normal;
  _VertexList[Index].UV := UV;
End;

Procedure DXMesh.AddTriangle(Index:Integer; A,B,C:Integer);
Var
  N:Integer;
Begin
  If (A=B) Or (A=C) Or (B=C) Then
  Begin
    RaiseError('Degenerate triangle!');
    Exit;
  End;

  N := Index * 3;
  _IndexList[N+0] := A;
  _IndexList[N+1] := B;
  _IndexList[N+2] := C;
End;

Procedure DXMesh.BuildGeometry;
Var
  Result:HRESULT;
Begin
  // Create the encapsulated mesh
  Result := D3DXCreateMesh((_IndexCount Div 3), _VertexCount,
                           D3DXMESH_MANAGED or D3DXMESH_32BIT, @VERTEX_DECL,
                           _pd3dDevice, _pMesh);
  If (Failed(Result)) Then
  Begin
    RaiseError('Error creating D3D mesh');
    Exit;
  End;

  Result:= S_OK;
End;

Procedure DXMesh.WriteGeometry;
Var
  Result:HRESULT;
  pTempMesh:ID3DXMesh;
  Epsilons: TD3DXWeldEpsilons;
  Warnings:ID3DXBUFFER;
  pVert: ^DXMeshVertex;
  pIndex: PDWORD;
Begin
  If (Assigned(_pAdjacency)) Then
    FreeMem(_pAdjacency);

  // Copy the vertex data
  Result := _pMesh.LockVertexBuffer(0, Pointer(pVert));
  if Failed(Result) then
  Begin
    RaiseError('Error locking D3D vertex buffer');
    Exit;
  End;

  CopyMemory(pVert, @(_VertexList[0]), _VertexCount*SizeOf(DXMeshVertex));
  _pMesh.UnlockVertexBuffer;

  // Copy the index data
  Result := _pMesh.LockIndexBuffer(0, Pointer(pIndex));
  if Failed(Result) then
  Begin
    RaiseError('Error locking D3D index buffer');
    Exit;
  End;
  CopyMemory(pIndex, @(_IndexList[0]), _IndexCount*SizeOf(DWORD));
  _pMesh.UnlockIndexBuffer;

  // Reorder the vertices according to subset and optimize the mesh for this graphics
  // card's vertex cache. When rendering the mesh's triangle list the vertices will
  // cache hit more often so it won't have to re-execute the vertex shader.
  Try
    GetMem(_pAdjacency, SizeOf(DWORD)*_pMesh.GetNumFaces*3);
  Except
    Result:= E_OUTOFMEMORY;
    Exit;
  End;

  _pMesh.GenerateAdjacency(1e-6, _pAdjacency);

  // Perform a weld to try and remove excess vertices.
  // Weld the mesh using all epsilons of 0.0f.  A small epsilon like 1e-6 works well too
  ZeroMemory(@Epsilons, SizeOf(TD3DXWeldEpsilons));
  Epsilons.Position := 0.01;
  Result := D3DXWeldVertices(_pMesh, 0, @Epsilons, _pAdjacency, _pAdjacency, Nil, Nil);
  if Failed(Result) then
  Begin
    RaiseError('Error welding D3D mesh');
    Exit;
  End;

  // Perform simple cleansing operations on mesh
  Result := D3DXCleanMesh(D3DXCLEAN_SIMPLIFICATION, _pMesh, _pAdjacency, pTempMesh, _pAdjacency, @Warnings);
  if Not Failed(Result) then
    _pMesh := pTempMesh
  Else
    RaiseError(PTERRAChar(Warnings.GetBufferPointer));

  _pMesh.OptimizeInplace(D3DXMESHOPT_VERTEXCACHE Or D3DXMESHOPT_DEVICEINDEPENDENT, _pAdjacency, _pAdjacency, nil, nil);
End;

Procedure DXMesh.ReadGeometry;
Var
  Result:HRESULT;
  pVert: ^DXMeshVertex;
  pIndex: PDWORD;
Begin
  _IndexCount := _pMesh.GetNumFaces * 3;
  SetLength(_IndexList, _IndexCount);

  _VertexCount := _pMesh.GetNumVertices;
  SetLength(_VertexList, _VertexCount);

  // Copy the vertex data
  Result := _pMesh.LockVertexBuffer(0, Pointer(pVert));
  if Failed(Result) then
  Begin
    RaiseError('Error locking D3D vertex buffer');
    Exit;
  End;

  CopyMemory(@(_VertexList[0]), pVert, _VertexCount*SizeOf(DXMeshVertex));
  _pMesh.UnlockVertexBuffer;

  // Copy the index data
  Result := _pMesh.LockIndexBuffer(0, Pointer(pIndex));
  if Failed(Result) then
  Begin
    RaiseError('Error locking D3D index buffer');
    Exit;
  End;
  CopyMemory(@(_IndexList[0]), pIndex, _IndexCount*SizeOf(DWORD));
  _pMesh.UnlockIndexBuffer;
End;

Var
  MyCallback:ProgressNotifier = Nil;

Function UVAtlasCallback(PercentDone: Single; lpUserContext: Pointer): HRESULT; stdcall;
begin
  If Assigned(MyCallback) Then
    MyCallback.Notify(PercentDone);
  Result:= S_OK;
end;

Procedure DXMesh.BuildAtlas(Width,Height:Integer; Callback:ProgressNotifier=Nil);
Var
  Result:HRESULT;
  pMeshResult:ID3DXMesh;
  pFacePartitioning, pVertexRemapArray: ID3DXBuffer;
  stretchOut: Single;
  numchartsOut: LongWord;
Begin
  WriteGeometry;

  MyCallback := Callback;
  Result := D3DXUVAtlasCreate(_pMesh,
                              0, //maxcharts
                              0.067, //maxstretch
                              Width,
                              Height,
                              1.0, //gutter
                              0,
                              _pAdjacency,
                              Nil,
                              Nil,
                              UVAtlasCallback,
                              0.001,
                              nil,
                              D3DXUVATLAS_DEFAULT,
                              pMeshResult,
                              @pFacePartitioning,
                              @pVertexRemapArray,
                              @stretchOut,
                              @numchartsOut);
  if Failed(Result) then
  Begin
    RaiseError('Error generating D3D UV atlas');
    Exit;
  End;

  _pMesh := pMeshResult;
  pFacePartitioning := Nil;
  pVertexRemapArray := Nil;

  ReadGeometry;
End;

Procedure DXMesh.BuildLOD;
Var
  Result:HRESULT;
Begin
  WriteGeometry;

  // Verify validity of mesh for simplification
  Result := D3DXValidMesh(_pMesh, _pAdjacency, nil);
  if Failed(Result) then
  Begin
    RaiseError('Error D3D mesh not valid');
    Exit;
  End;

  Result := D3DXGeneratePMesh(_pMesh, _pAdjacency, nil, nil, 1, D3DXMESHSIMP_FACE, _pLODMesh);
  if Failed(Result) then
  Begin
    RaiseError('Error generating D3D progressive mesh');
    Exit;
  End;

  _MinVertexCount := _pLODMesh.GetMinVertices;
  _MaxVertexCount := _pLODMesh.GetMaxVertices;
End;

Procedure DXMesh.SetLOD(VertexCount:Integer);
Var
  Result:HRESULT;
  pVert: ^DXMeshVertex;
  pIndex: PDWORD;
Begin
  VertexCount := IntMin(IntMax(VertexCount, _MinVertexCount), _MaxVertexCount);
  _pLODMesh.SetNumVertices(VertexCount);

  _IndexCount := _pLODMesh.GetNumFaces * 3;
  SetLength(_IndexList, _IndexCount);

  _VertexCount := _pLODMesh.GetNumVertices;
  SetLength(_VertexList, _VertexCount);

  // Copy the vertex data
  Result := _pLODMesh.LockVertexBuffer(D3DLOCK_READONLY, Pointer(pVert));
  if Failed(Result) then
  Begin
    RaiseError('Error locking D3D vertex buffer');
    Exit;
  End;

  CopyMemory(@(_VertexList[0]), pVert, _VertexCount*SizeOf(DXMeshVertex));
  _pLODMesh.UnlockVertexBuffer;

  // Copy the index data
  Result := _pLODMesh.LockIndexBuffer(D3DLOCK_READONLY, Pointer(pIndex));
  if Failed(Result) then
  Begin
    RaiseError('Error locking D3D index buffer');
    Exit;
  End;
  CopyMemory(@(_IndexList[0]), pIndex, _IndexCount*SizeOf(DWORD));
  _pLODMesh.UnlockIndexBuffer;
End;

Procedure DXMesh.BuildNormals;
Begin
  WriteGeometry;

  D3DXComputeNormals(_pMesh, _pAdjacency);

  ReadGeometry;
End;

Function DXMesh.GetVertexCount:Integer;
Begin
  Result := _VertexCount;
End;

Function DXMesh.GetTriangleCount:Integer;
Begin
  Result := _IndexCount Div 3;
End;

Function DXMesh.GetVertex(Index:Integer):DXMeshVertex;
Begin
  Result := _VertexList[Index];
End;

Function DXMesh.GetTriangleIndex(TriangleIndex, VertexIndex:Integer):Integer;
Begin
  Result := _IndexList[(TriangleIndex*3) + VertexIndex];
End;

Function DXMesh.GetTriangleAdjancency(Index:Integer):TriangleAdjancency;
Begin
  Move(Pointer(Integer(_pAdjacency) + (Index*(4*3)))^, Result, 4*3);
End;

Initialization
  _pd3dDevice := Nil;
Finalization
  _pd3dDevice := Nil;
End.