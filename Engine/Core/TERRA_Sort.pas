Unit TERRA_Sort;

{$I terra.inc}

Interface

Type
  Sort = Class
    Protected
      Class Procedure QuickSort(Data:Pointer; L,R:Integer);

    Public
      // should return 1 if A < B
      // should return -1 if A > B
      // should return 0 if A = B
      Class Procedure SetPivot(Data:Pointer; A:Integer); Virtual; Abstract;
      Class Function Compare(Data:Pointer; A:Integer):Integer; Virtual; Abstract;
      Class Procedure Swap(Data:Pointer; A,B:Integer); Virtual; Abstract;

      Class Procedure Sort(Data:Pointer; Count:Integer);
  End;

Implementation

{ Sort }
Class Procedure Sort.QuickSort(Data:Pointer; L,R:Integer);
  Procedure _QuickSort(iLo,iHi:Integer);
  Var
    Lo, Hi: Integer;
  Begin
    Lo := iLo;
    Hi := iHi;
    SetPivot(Data, (Lo + Hi) Shr 1);
    //WriteLn('Begin: ',Lo, ' -> ',Hi);
    Repeat
      While (Compare(Data, Lo)>0) Do
        Inc(Lo);

      While (Compare(Data, Hi)<0) Do
        Dec(Hi);

      If Lo <= Hi Then
      Begin
      //WriteLn('Swapping: ',Lo, ' -> ',Hi);
        Swap(Data, Lo, Hi);
        Inc(Lo);
        Dec(Hi);
      End;
    Until Lo > Hi;
    If Hi > iLo Then
      _QuickSort(iLo, Hi);
    If Lo < iHi Then
      _QuickSort(Lo, iHi);
  End;

Begin
  If R<L Then
    Exit;
  _QuickSort(L, R);
End;

Class procedure Sort.Sort(Data: Pointer; Count: Integer);
Begin
  QuickSort(Data, 0, Pred(Count));
End;

End.

