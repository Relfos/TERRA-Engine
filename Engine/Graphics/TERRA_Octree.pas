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
 * TERRA_Octree
 * Implements a Octree class
 ***********************************************************************************************************************
}
Unit TERRA_Octree;

{
WARNING - When calling Octree.Intersect
Make sure the paramter T is initialized to a very long number (eg: 9999)
}

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Vector3D, TERRA_GraphicsManager, TERRA_BoundingBox, TERRA_Ray, TERRA_Color;

Const
  MaxLevel = 6;

Type
  OctreeElement = Class(TERRAObject)
    Public
      Box:BoundingBox;

      Procedure Release; Override;

      Procedure Render; Virtual;
      Function Intersect(Const R:Ray; Var T:Single):Boolean; Virtual;
  End;

  Octree = Class(TERRAObject)
    Protected
      _Size:BoundingBox;
      _Level:Integer;
      _Children:Array[0..7] Of Octree;

      _Elements:Array Of OctreeElement;
      _ElementCount:Integer;

      Function GetChildrenBoundingBox(Index:Integer):BoundingBox;

    Public
      Constructor Create(Box:BoundingBox; Level:Integer = 0);
      Procedure Release; Override;

      Procedure AddElement(Element:OctreeElement);
      Procedure RemoveElement(Element:OctreeElement);

      Function Intersect(Const R:Ray; Var T:Single):OctreeElement;

      Procedure Render;
  End;

Implementation
{$IFDEF PC}
Uses TERRA_DebugDraw;
{$ENDIF}

{ Octree }
Constructor Octree.Create(Box:BoundingBox; Level:Integer);
Var
  I:Integer;
Begin
  Self._Level := Level;
  Self._Size := Box;

  Inc(Level);
  If Level<MaxLevel Then
  For I:=0 To 7 Do
    _Children[I] := Octree.Create(Self.GetChildrenBoundingBox(I), Level);
End;

Procedure Octree.AddElement(Element:OctreeElement);
Var
  I:Integer;
Begin
  For I:=0 To 7 Do
  If (Assigned(_Children[I])) And (Element.Box.Inside(_Children[I]._Size)) Then
  Begin
    _Children[I].AddElement(Element);
    Exit;
  End;

  Inc(_ElementCount);
  SetLength(_Elements, _ElementCount);
  _Elements[Pred(_ElementCount)] := Element;
End;

Function Octree.GetChildrenBoundingBox(Index: Integer): BoundingBox;
Var
  Center:Vector3D;
Begin
  Center := _Size.Center;
  Case Index Of
  0 : Begin
        Result.StartVertex := VectorCreate(_Size.StartVertex.X, _Size.StartVertex.Y, _Size.StartVertex.Z);
        Result.EndVertex := VectorCreate(Center.X, Center.Y, Center.Z);
      End;
  1 : Begin
        Result.StartVertex := VectorCreate(_Size.StartVertex.X, _Size.StartVertex.Y, Center.Z);
        Result.EndVertex := VectorCreate(Center.X, Center.Y, _Size.EndVertex.Z);
      End;
  2 : Begin
        Result.StartVertex := VectorCreate(Center.X, _Size.StartVertex.Y, _Size.StartVertex.Z);
        Result.EndVertex := VectorCreate(_Size.EndVertex.X, Center.Y, Center.Z);
      End;
  3 : Begin
        Result.StartVertex := VectorCreate(Center.X, _Size.StartVertex.Y, Center.Z);
        Result.EndVertex := VectorCreate(_Size.EndVertex.X, Center.Y, _Size.EndVertex.Z);
      End;

  4 : Begin
        Result.StartVertex := VectorCreate(_Size.StartVertex.X, Center.Y, _Size.StartVertex.Z);
        Result.EndVertex := VectorCreate(Center.X, _Size.EndVertex.Y, Center.Z);
      End;
  5 : Begin
        Result.StartVertex := VectorCreate(_Size.StartVertex.X, Center.Y, Center.Z);
        Result.EndVertex := VectorCreate(Center.X, _Size.EndVertex.Y, _Size.EndVertex.Z);
      End;
  6 : Begin
        Result.StartVertex := VectorCreate(Center.X, Center.Y, _Size.StartVertex.Z);
        Result.EndVertex := VectorCreate(_Size.EndVertex.X, _Size.EndVertex.Y, Center.Z);
      End;
  7 : Begin
        Result.StartVertex := VectorCreate(Center.X, Center.Y, Center.Z);
        Result.EndVertex := VectorCreate(_Size.EndVertex.X, _Size.EndVertex.Y, _Size.EndVertex.Z);
      End;
  Else
  	Begin
  		Result.StartVertex := VectorZero;
  		FillChar(Result, SizeOf(Result), 0);
  	End;
  End;
End;

Procedure Octree.RemoveElement(Element:OctreeElement);
Var
  I:Integer;
Begin
  I:=0;
  While I<_ElementCount Do
  If (_Elements[I] = Element) Then
  Begin
    ReleaseObject(_Elements[I]);
    _Elements[I] := _Elements[Pred(_ElementCount)];
    Dec(_ElementCount);
    Exit;
  End Else
    Inc(I);

  For I:=0 To 7 Do
  If Assigned(_Children[I]) Then
    _Children[I].RemoveElement(Element);
End;

Procedure Octree.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ElementCount) Do
    ReleaseObject(_Elements[I]);

  For I:=0 To 7 Do
  If Assigned(_Children[I]) Then
    ReleaseObject(_Children[I]);
End;

Function Octree.Intersect(Const R: Ray; Var T:Single):OctreeElement;
Var
  K:Single;
  I:Integer;
  Ok:Boolean;
  Obj:OctreeElement;
Begin
  Result := Nil;
  K := 99999;
  If (R.Intersect(_Size, K)) Then
  Begin
    For I:=0 To Pred(_ElementCount) Do
    Begin
      Ok := _Elements[I].Intersect(R, K);
      If (Ok) Then
      Begin
        If (K<T) Then
        Begin
          Result := _Elements[I];
          T := K;
        End;
      End;
    End;

    For I:=0 To 7 Do
    If Assigned(_Children[I]) Then
    Begin
      Obj := _Children[I].Intersect(R, T);
      If (Obj<>Nil) Then
      Begin
        Result := Obj;
        Exit;
      End;
    End;
  End;
End;

Procedure Octree.Render;
Var
  I:Integer;
Begin
  If (Not GraphicsManager.Instance.ActiveViewport.Camera.Frustum.BoxVisible(_Size)) Then
    Exit;

  For I:=0 To Pred(_ElementCount) Do
    _Elements[I].Render();

  For I:=0 To 7 Do
  If Assigned(_Children[I]) Then
    _Children[I].Render;
End;

{ OctreeElement }
Procedure OctreeElement.Release;
Begin
  // do nothing
End;

Function OctreeElement.Intersect(const R: Ray; var T: Single): Boolean;
Begin
  Result := R.Intersect(Box, T);
End;

Procedure OctreeElement.Render;
Begin
End;

End.