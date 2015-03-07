Unit TERRA_TestCore;

{$I terra.inc}

Interface
Uses TERRA_TestSuite;

Type
  TERRACore_TestList = class(TestCase)
    Procedure Run; Override;
   End;

  TERRACore_TestHashTable = class(TestCase)
    Procedure Run; Override;
   End;

  TERRACore_TestSort = class(TestCase)
    Procedure Run; Override;
   End;

Implementation
Uses TERRA_Utils, TERRA_Sort, TERRA_Collections;

Type
  IntegerArraySort = Class(Sort)
    Public
      Class Procedure Swap(Data:Pointer; A,B:Integer); Override;
      Class Procedure SetPivot(Data:Pointer; A:Integer); Override;
      Class Function Compare(Data:Pointer; A:Integer):Integer; Override;
  End;

Var
  Items, Temp:Array Of Integer;
  Pivot:Integer;

Class Procedure IntegerArraySort.Swap(Data:Pointer; A,B:Integer);
Var
  P:PIntegerArray;
  Temp:Integer;
Begin
  P := Data;
  Temp := P[A];
  P[A] := P[B];
  P[B] := Temp;
End;

Class Procedure IntegerArraySort.SetPivot(Data:Pointer; A: Integer);
Var
  P:PIntegerArray;
Begin
  P := Data;
  Pivot := P[A];
End;

Class Function IntegerArraySort.Compare(Data:Pointer; A:Integer):Integer;
Var
  P:PIntegerArray;
Begin
  //WriteLn('Compare: ',A,' -> ', B);
  P := Data;
  If (P[A] < Pivot) Then
    Result := 1
  Else
  If (P[A] > Pivot) Then
    Result := -1
  Else
    Result := 0;
End;

 procedure QuickSort(var A: array of Integer; iLo, iHi: Integer) ;
 var
   Lo, Hi, Pivot, N,T: Integer;
   Function Compare1():Boolean;
   Begin
    //WriteLn('Compare: ',Lo,' -> ', N);
    Result :=A[Lo] < Pivot;
   End;
   Function Compare2():Boolean;
   Begin
    //WriteLn('Compare: ',Hi,' -> ', N);
    Result :=A[Hi] > Pivot;
   End;
 begin
   Lo := iLo;
   Hi := iHi;
   N := (Lo + Hi) div 2;
   Pivot := A[N];
   //WriteLn('Begin: ',Lo, ' -> ',Hi);
   repeat

     while Compare1 do
      Inc(Lo) ;
     while Compare2 do
      Dec(Hi) ;
     if Lo <= Hi then
     begin
     //WriteLn('Swapping: ',Lo, ' -> ',Hi);
       T := A[Lo];
       A[Lo] := A[Hi];
       A[Hi] := T;
       Inc(Lo) ;
       Dec(Hi) ;
     end;
   until Lo > Hi;
   if Hi > iLo then QuickSort(A, iLo, Hi) ;
   if Lo < iHi then QuickSort(A, Lo, iHi) ;
 end;


Procedure TERRACore_TestSort.Run();
Var
  I, J, Count:Integer;
  //Seed:Integer;
Begin
  For J:=1 To 500 Do
  Begin
//    Seed := Integer(RandSeed);

    Count := 500 + Random(1500);
    SetLength(Items, Count);
    SetLength(Temp, Count);
    For I:=0 To Pred(Count) Do
    Begin
      Items[I] := Random(9999);
      Temp[I] := Items[I];
    End;

    QuickSort(Temp,0, Pred(Count)) ;
    IntegerArraySort.Sort(@Items[0], Count);

    //WriteLn('seed: ',Seed);


    For I:=1 To Pred(Count) Do
    Begin
      //WriteLn(I);
      Check((Items[I]>=Items[I-1]), 'Quick sort error!');
      Check((Items[I]=Temp[I]), 'Quick sort error!');
    End;
  End;
End;

Procedure TERRACore_TestList.Run;
Var
  L:List;
  I,J,N, Prev:Integer;
  It:Iterator;
  Int:IntegerObject;
  Table:HashTable;
begin
  For J:=1 To 100 Do
  Begin
    L := List.Create();
    N := 500+Random(1500);
    For I:=1 To N Do
    Begin
      IntToString(I);
      L.Add(IntegerObject.Create(Random(200)));
    End;

    Check(L.Count=N, 'Invalid list count, got '+IntToString(L.Count)+', expected '+IntToString(N));

    It := L.CreateIterator();
    While It.HasNext Do
    Begin
      Check(It.GetNext()<>Nil, 'List iterator error!');
    End;
    It.Release;

    L.Release;
  End;

  //WriteLn('List sort test...');
  L := List.Create(coSorted);
  N := 2000;
  For I:=1 To N Do
  Begin
    IntToString(I);
    L.Add(IntegerObject.Create(Random(20000)));
  End;
  It := L.CreateIterator;
  Prev := -1;
  While It.HasNext Do
  Begin
    Int := IntegerObject(It.GetNext());
    //Write(Int.Value, ' ');
    Check(Prev<=Int.Value, 'List ascending sort error!');
    Prev := Int.Value;
  End;
  It.Release;
  L.Release;

  //WriteLn('List descending sort test...');
  L := List.Create(coSortedInverted);
  N := 2000;
  For I:=1 To N Do
  Begin
    IntToString(I);
    L.Add(IntegerObject.Create(Random(20000)));
  End;
  It := L.CreateIterator;
  Prev := 99999999;
  While It.HasNext Do
  Begin
    Int := IntegerObject(It.GetNext());
    //Write(Int.Value, ' ');
    Check(Prev>=Int.Value, 'List descending sort error!');
    Prev := Int.Value;
  End;
  It.Release;
  L.Release;
End;

Procedure TERRACore_TestHashTable.Run();
Var
  I,J,N:Integer;
  IT:Iterator;
  Table:HashTable;
Begin
  For J:=1 To 5 Do
  Begin
    Table := HashTable.Create(256);
    N := 500+Random(1500);
    For I:=1 To N Do
    Begin
      IntToString(I);
      Table.Add(IntegerObject.Create(Random(200)));
    End;

    I := 0;
    It := Table.CreateIterator();
    While It.HasNext Do
    Begin
      Check(It.GetNext()<>Nil, 'Hash table iterator error!');
      Inc(I);
    End;
    It.Release;
    Check(Table.Count=I, 'Invalid hashtable count, got '+IntToString(I)+', expected '+IntToString(Table.Count));
    Table.Release;
  End;

end;

End.