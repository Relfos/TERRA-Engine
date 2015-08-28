Unit TERRA_TestCore;

{$I terra.inc}

Interface
Uses TERRA_TestSuite;

Type
  TERRACore_TestList = class(TestCase)
    Procedure Run; Override;
   End;

  TERRACore_TestHashMap = class(TestCase)
    Procedure Run; Override;
   End;

  TERRACore_TestSort = class(TestCase)
    Procedure Run; Override;
   End;

   TERRACore_TestObjectArray = class(TestCase)
    Procedure Run; Override;
   End;

Implementation
Uses TERRA_Utils, TERRA_Sort, TERRA_Collections, TERRA_CollectionObjects,
  TERRA_HashMap, TERRA_ObjectArray, TERRA_KeyPairObjects;

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
  I,J,N, Prev, Count:Integer;
  It:Iterator;
  Int:IntegerObject;
  Table:HashMap;
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

    It := L.GetIterator();
    Count := 0;
    While It.HasNext Do
    Begin
      Check(It.Value<>Nil, 'List iterator error!');
      Inc(Count);
    End;
    Check(Count = L.Count, 'Iterator did not iterate full list!');

    ReleaseObject(L);
  End;

  //WriteLn('List sort test...');
  L := List.Create(collection_Sorted_Ascending);
  N := 2000;
  For I:=1 To N Do
  Begin
    IntToString(I);
    L.Add(IntegerObject.Create(Random(20000)));
  End;

  It := L.GetIterator();
  Prev := -1;
  While It.HasNext Do
  Begin
    Int := IntegerObject(It.Value);
    //Write(Int.Value, ' ');
    Check(Prev<=Int.Value, 'List ascending sort error!');
    Prev := Int.Value;
  End;
  ReleaseObject(L);

  //WriteLn('List descending sort test...');
  L := List.Create(collection_Sorted_Descending);
  N := 2000;
  For I:=1 To N Do
  Begin
    IntToString(I);
    L.Add(IntegerObject.Create(Random(20000)));
  End;

  It := L.GetIterator();
  Prev := 99999999;
  While It.HasNext Do
  Begin
    Int := IntegerObject(It.Value);
    //Write(Int.Value, ' ');
    Check(Prev>=Int.Value, 'List descending sort error!');
    Prev := Int.Value;
  End;
  ReleaseObject(L);

  L := List.Create();
  For I:=0 To 10 Do
    L.Add(IntegerObject.Create(I));

  It := L.GetIterator();
  While It.HasNext Do
  Begin
    Int := IntegerObject(It.Value);

    If (Odd(Int.Value)) Then
      Int.Discard();
  End;

  Check(L.Count = 6, 'List discard error!');

{  It := L.CreateIterator();
  While It.HasNext Do
  Begin
    Int := IntegerObject(It.Value);

    WriteLn(Int.ToString());
  End;
  ReleaseObject(It);}
End;

Procedure TERRACore_TestHashMap.Run();
Var
  I,J,N, Count:Integer;
  Item:StringKeyPair;
  It:Iterator;
  Table:HashMap;
Begin
  For J:=1 To 5 Do
  Begin
    Table := HashMap.Create(256);
    N := 1500+Random(1500);

    For I:=1 To N Do
    Begin
      Table.Add(StringKeyPair.Create(IntToString(I), IntToString(Random(200))));
    End;

    Count := 0;
    It := Table.GetIterator();
    While It.HasNext Do
    Begin
      Item := StringKeyPair(It.Value);

      Check(Assigned(Item), 'Hash table iterator error!');

      Inc(Count);
      {If Assigned(Item) Then
        WriteLn(Item.ToString);}
    End;
    Check(Count = Table.Count, 'Iterator did not iterate full hash table!');

    ReleaseObject(Table);
  End;

  Table := HashMap.Create(256);
  For I:=1 To 100 Do
  Begin
    Table.Add(StringKeyPair.Create('BOO_'+IntToString(I), IntToString(Sqr(I))));
  End;

  Item := StringKeyPair(Table['BOO_2']);
  Check((Assigned(Item)) And (Item.Value = '4'), 'Hash table direct acess error!');

  Item := StringKeyPair(Table['BOO_4']);
  Check((Assigned(Item)) And (Item.Value = '16'), 'Hash table direct acess error!');

  ReleaseObject(Table);

End;

Procedure TERRACore_TestObjectArray.Run();
Var
  I,J,N, Count:Integer;
  Item:IntegerObject;
  It:Iterator;
  V:ObjectArray;
Begin
  V := ObjectArray.Create(0, Nil);

  N := 30+ Random(100);
  For J:=0 To Pred(N) Do
  Begin
    V.Add(IntegerObject.Create(Random(200)));
  End;

  It := V.GetIterator();
  Count := 0;
  While It.HasNext() Do
  Begin
    Item := IntegerObject(It.Value);
    Inc(Count);
  End;
  Check(Count = V.Count, 'Iterator did not iterate full list!');
  ReleaseObject(It);

  ReleaseObject(V);
End;


End.