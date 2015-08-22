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
 * TERRA_Sort
 * Implements generic quick sort
 ***********************************************************************************************************************
}
Unit TERRA_Sort;

{$I terra.inc}

Interface
Uses TERRA_Object;

Type
  Sort = Class
    Protected
      Class Function Partition(Data:TERRAObject; L, R:Integer):Integer;
      Class Procedure QuickSort(Data:TERRAObject; L,R:Integer);

    Public
      // should return 1 if A < B
      // should return -1 if A > B
      // should return 0 if A = B
      Class Procedure SetPivot(Data:TERRAObject; A:Integer); Virtual; Abstract;
      Class Function Compare(Data:TERRAObject; A:Integer):Integer; Virtual; Abstract;
      Class Procedure Swap(Data:TERRAObject; A,B:Integer); Virtual; Abstract;

      Class Procedure Sort(Data:TERRAObject; Count:Integer);
  End;

Implementation

{ Sort }
Class Function Sort.Partition(Data:TERRAObject; L, R:Integer):Integer;
Var
  I,J:Integer;
Begin
  I := L;
  J := R;

  SetPivot(Data, (R + L) Shr 1);

  While (i <= j) Do
  Begin
    While (Compare(Data, I)>0) Do
      Inc(I);

    While (Compare(Data, J)<0) Do
      Dec(J);

    If (i <= j) Then
    Begin
      Swap(Data, I, J);
      Inc(I);
      Dec(J);
    End;
  End;

  Result := I;
End;

Class Procedure Sort.QuickSort(Data:TERRAObject; L,R:Integer);
Var
  index:Integer;
Begin
  Index := Partition(Data, L, R);

  If (L < Pred(index)) Then
    QuickSort(Data, L, Pred(index));

  If (index < R) Then
    QuickSort(Data, Index, R);
End;

Class procedure Sort.Sort(Data: TERRAObject; Count: Integer);
Begin
  QuickSort(Data, 0, Pred(Count));
End;

End.

