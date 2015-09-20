Unit TERRA_TestString;

{$I terra.inc}

Interface
Uses TERRA_TestSuite, TERRA_Object;

Type
  TERRAString_TestIterator = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAString_TestReverseIterator = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAString_TestUnicodeIterator = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAString_TestUnicodeReverseIterator = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAString_TestGetChar = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAString_TestRegex = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestSubStrPos = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestCharPos = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestCopy = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestSplits = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestIteratorSplits = class(TestCase)
    Procedure Run; Override;
   End;

(*   TERRAString_TestWordExtract = class(TestCase)
    Procedure Run; Override;
   End;*)

   TERRAString_TestPad = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestReplace = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestReverse = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestTrim = class(TestCase)
    Procedure Run; Override;
   End;

   TERRAString_TestConversions = class(TestCase)
    Procedure Run; Override;
   End;


Implementation
Uses TERRA_String, TERRA_Utils;

Function GetCatInCyrillic():TERRAString;
Begin
  Result := '';
  StringAppendChar(Result, #1082);
  StringAppendChar(Result, #1086);
  StringAppendChar(Result, #1096);
  StringAppendChar(Result, #1082);
  StringAppendChar(Result, #1072);
End;

Function GetDogInCyrillic():TERRAString;
Begin
  Result := '';
  StringAppendChar(Result, #1089);
  StringAppendChar(Result, #1086);
  StringAppendChar(Result, #1073);
  StringAppendChar(Result, #1072);
  StringAppendChar(Result, #1082);
  StringAppendChar(Result, #1072);
End;

Procedure TERRAString_TestIterator.Run();
Var
  S:TERRAString;
  It:StringIterator;
  N:Integer;

  Procedure Expect(C:TERRAChar);
  Var
    B:TERRAChar;
  Begin
    Inc(N);
    B := It.GetNext();
    Check(B = C, 'Expected char "'+ C + '" at position '+IntegerProperty.Stringify(N)+' got "'+ B +'"');
  End;
Begin
  S := 'TEST1';
  N := 0;
  It := StringCreateIterator(S);
  Expect('T');
  Expect('E');
  Expect('S');
  Expect('T');
  Expect('1');
  Check(It.HasNext = False, 'End of string expected!');
  ReleaseObject(It);
End;

Procedure TERRAString_TestReverseIterator.Run();
Var
  S:TERRAString;
  It:StringIterator;
  N:Integer;

  Procedure Expect(C:TERRAChar);
  Var
    B:TERRAChar;
  Begin
    Inc(N);
    B := It.GetNext();
    Check(B = C, 'Expected char "'+ C + '" at position '+IntegerProperty.Stringify(N)+' got "'+ B +'"');
  End;
Begin
  S := 'ABCX3';
  N := 0;
  It := StringCreateReverseIterator(S);
  Expect('3');
  Expect('X');
  Expect('C');
  Expect('B');
  Expect('A');
  Check(It.HasNext = False, 'End of string expected!');
  ReleaseObject(It);
End;

Procedure TERRAString_TestUnicodeIterator.Run();
Var
  S:TERRAString;
  It:StringIterator;
  N:Integer;

  Procedure Expect(C:TERRAChar);
  Var
    B:TERRAChar;
  Begin
    Inc(N);
    B := It.GetNext();
    Check(B = C, 'Expected char "'+ StringFromChar(C) + '" at position '+IntegerProperty.Stringify(N)+' got "'+StringFromChar(B)+'"');
  End;
Begin
  S := GetCatInCyrillic();

  N := 0;
  It := StringCreateIterator(S);
  Expect(#1082);
  Expect(#1086);
  Expect(#1096);
  Expect(#1082);
  Expect(#1072);
  Check(It.HasNext = False, 'End of string expected!');
  ReleaseObject(It);
End;

Procedure TERRAString_TestUnicodeReverseIterator.Run();
Var
  S:TERRAString;
  It:StringIterator;
  N:Integer;

  Procedure Expect(C:TERRAChar);
  Var
    B:TERRAChar;
  Begin
    Inc(N);
    B := It.GetNext();
    Check(B = C, 'Expected char "'+ StringFromChar(C) + '" at position '+IntegerProperty.Stringify(N)+' got "'+StringFromChar(B)+'"');
  End;
Begin
  S := GetDogInCyrillic();

  N := 0;
  It := StringCreateReverseIterator(S);
  Expect(#1072);
  Expect(#1082);
  Expect(#1072);
  Expect(#1073);
  Expect(#1086);
  Expect(#1089);
  Check(It.HasNext = False, 'End of string expected!');
  ReleaseObject(It);
End;

Procedure TERRAString_TestGetChar.Run();
Var
  S:TERRAString;

  Procedure Expect(C:TERRAChar; N:Integer);
  Var
    B:TERRAChar;
  Begin
    B := StringGetChar(S, N);
    Check(B = C, 'Expected char "'+ C + '" at position '+IntegerProperty.Stringify(N)+' got "'+ B +'"');
  End;
Begin
  S := 'PIPO5';

  Expect('P', 3);
  Expect('O', 4);
  Expect('I', -4);
  Expect('5', 5);
End;

Procedure TERRAString_TestRegex.Run();
Var
  A, B:TERRAString;


  Procedure ExpectMatch(Const S, Expr:TERRAString; Value:Boolean);
  Var
    S2:String;
    Result:Boolean;
  Begin
    If Value Then
      S2 := 'match'
    Else
      S2 := 'fail';

    Result := StringMatchRegex(S, Expr);
    Check(Result = Value, 'Expected "'+ S + '" to '+S2+' expression '+Expr);
  End;
Begin
  A := 'leaf.png';
  B := 'sound2.wav';

  ExpectMatch(A, '*', True);
  ExpectMatch(B, '*', True);

  ExpectMatch(A, '*.png', True);
  ExpectMatch(A, '*.wav', False);
  ExpectMatch(A, 'l*', True);
  ExpectMatch(A, 'x*', False);
  ExpectMatch(A, '*u*', False);

  ExpectMatch(B, '*.png', False);
  ExpectMatch(B, '*.wav', True);
  ExpectMatch(B, '*l*', False);
  ExpectMatch(B, '*u*', True);
End;

Procedure TERRAString_TestSubStrPos.Run();
Var
  S:TERRAString;

  Procedure Expect(Const SubStr:TERRAString; N:Integer; IgnoreCase:Boolean);
  Var
    P:Integer;
  Begin
    P := StringPos(SubStr, S, IgnoreCase);
    Check(P = N, 'Expected "'+ SubStr + '" at position '+IntegerProperty.Stringify(N)+' but found in '+IntegerProperty.Stringify(P));
  End;

  Procedure ExpectReverse(Const SubStr:TERRAString; N:Integer; IgnoreCase:Boolean);
  Var
    P:Integer;
  Begin
    P := StringPosReverse(SubStr, S, IgnoreCase);
    Check(P = N, 'Expected "'+ SubStr + '" at position '+IntegerProperty.Stringify(N)+' but found in '+IntegerProperty.Stringify(P));
  End;
Begin
  S := 'GELLY4LYFE';

  Expect('ge', 1, True);
  ExpectReverse('LY', 7, False);
  Expect('LY', 4, False);
  Expect('GE', 1, False);
End;

Procedure TERRAString_TestCharPos.Run();
Var
  S:TERRAString;

  Procedure Expect(Const C:TERRAChar; N:Integer; IgnoreCase:Boolean);
  Var
    P:Integer;
  Begin
    P := StringCharPos(C, S, IgnoreCase);
    Check(P = N, 'Expected "'+ StringFromChar(C) + '" at position '+IntegerProperty.Stringify(N)+' but found in '+IntegerProperty.Stringify(P));
  End;

  Procedure ExpectReverse(Const C:TERRAChar; N:Integer; IgnoreCase:Boolean);
  Var
    P:Integer;
  Begin
    P := StringCharPosReverse(C, S, IgnoreCase);
    Check(P = N, 'Expected "'+ StringFromChar(C) + '" at position '+IntegerProperty.Stringify(N)+' but found in '+IntegerProperty.Stringify(P));
  End;
Begin
  S := 'TEST6BATt3rY';

  Expect('t', 1, True);
  ExpectReverse('B', 6, False);
  Expect('3', 10, False);
  Expect('t', 9, False);
End;

Procedure TERRAString_TestCopy.Run;
Var
  X,Y,Z:TERRAString;
  Dog, Cat, S:TERRAString;

  Procedure Expect(Const A, B: TERRAString);
  Begin
    Check(A = B, 'Expected "'+ B + '" but got "'+A+'"');
  End;

Begin
  X := 'hello world';
  Y := StringCopy(X, 1, 5);
  Z := StringCopy(X, 7, MaxInt);

  Expect(Y, 'hello');
  Expect(Z, 'world');

  Cat := GetCatInCyrillic();
  Dog := GetDogInCyrillic();

  S := Cat + ' ' + Dog;

  Y := StringCopy(S, 1, 5);
  Z := StringCopy(S, -6, 6);
  Expect(Y, Cat);
  Expect(Z, Dog);

  X := 'negative number';
  Y := StringCopy(X, -6, 6);
  Z := StringCopy(X, 1, -7);

  Expect(Y, 'number');
  Expect(Z, 'negative');
End;

Procedure TERRAString_TestSplits.Run;
Var
  A,B, C, D, E:TERRAString;
  Dog, Cat, S:TERRAString;

  Procedure Expect(Const X, Y: TERRAString);
  Begin
    Check(X = Y, 'Expected "'+ Y + '" but got "'+X+'"');
  End;

Begin
  A := 'hello|world|again';
  StringSplitByChar(A, B, C, '|');

  Expect(B, 'hello');

  StringSplitByChar(C, D, E, '|');

  Expect(D, 'world');
  Expect(E, 'again');

  Cat := GetCatInCyrillic();
  Dog := GetDogInCyrillic();
  A := Cat + ',lion,'+Dog;

  StringSplitByChar(A, B, C, ',');

  Expect(B, Cat);

  StringSplitByChar(C, D, E, ',');

  Expect(D, 'lion');
  Expect(E, Dog);

  A := 'oranges,apples,pears';

  Expect(StringGetNextSplit(A, ','), 'oranges');
  Expect(StringGetNextSplit(A, ','), 'apples');
  Expect(StringGetNextSplit(A, ','), 'pears');
  Expect(A, '');
End;

Procedure TERRAString_TestIteratorSplits.Run;
Var
  S:TERRAString;
  It:StringIterator;

  Procedure Expect(Result:Boolean; Const X, Y: TERRAString);
  Var
    A,B:TERRAString;
  Begin
    Check(Result, 'Expected iterator.split to return true for "'+ S +'"');
    It.Split(A, B);
    Check(X = A, 'Expected "'+ X + '" but got "'+A+'"');
    Check(Y = B, 'Expected "'+ Y + '" but got "'+B+'"');
  End;

Begin
  S := 'integer:->string';
  It := StringPosIterator(':->', S);
  Expect(Assigned(It), 'integer', 'string');
  ReleaseObject(It);
End;

(*Procedure TERRAString_TestWordExtract.Run;
Var
  Dog, Cat, S:TERRAString;

  Procedure Expect(Const Y: TERRAString; Const Separator:TERRAChar);
  Var
    X:TERRAString;
    C:TERRAChar;
  Begin
    X := StringExtractNextWord(S, C);
    Check(X = Y, 'Expected "'+ Y + '" but got "'+X+'"');
    Check(C = Separator, 'Expected "'+ StringFromChar(Separator) + '" as separator but got "'+StringFromChar(C)+'"');
  End;

Begin
  Cat := GetCatInCyrillic();
  Dog := GetDogInCyrillic();

  S := 'hello, '+Dog+' again! Got '+Cat+' in blue?';

  Expect('hello', ',');
  Expect('', ' ');
  Expect(Dog, ' ');
  Expect('again', '!');
  Expect('', ' ');
  Expect('Got', ' ');
  Expect(Cat, ' ');
  Expect('in', ' ');
  Expect('blue', '?');
  Check(S = '', 'End of string expected!');
End;*)

Procedure TERRAString_TestPad.Run;
  Procedure Expect(Const X, Y: TERRAString);
  Begin
    Check(X = Y, 'Expected "'+ Y + '" but got "'+X+'"');
  End;

Begin
  Expect(StringPadLeft(IntegerProperty.Stringify(2), 2, 'x'), 'x2');
  Expect(StringPadLeft(IntegerProperty.Stringify(2), 4, '0'), '0002');
  Expect(StringPadLeft(IntegerProperty.Stringify(2), 1, '0'), '2');
  Expect(StringPadRight(IntegerProperty.Stringify(2), 3,' '), '2  ');
End;


Procedure TERRAString_TestReplace.Run;
Var
  S, Dog, Cat:TERRAString;

  Procedure Expect(Const X, Y: TERRAString);
  Begin
    Check(X = Y, 'Expected "'+ Y + '" but got "'+X+'"');
  End;

Begin
  Cat := GetCatInCyrillic();
  Dog := GetDogInCyrillic();

  S := 'come $1 and $2';
  StringReplaceText('$1', Cat, S);
  StringReplaceText('$2', Dog, S);
  Expect(S, 'come '+Cat+' and '+Dog);

  S := '$1 and another $1';
  StringReplaceText('$1', Cat, S);
  Expect(S, Cat+' and another '+Cat);

  S := 'X and Y';
  StringReplaceChar('X', '3', S);
  StringReplaceChar('y', '4', S);
  Expect(S, '3 and 4');
End;

Procedure TERRAString_TestReverse.Run;
Var
  S:TERRAString;

  Procedure Expect(Const X, Y: TERRAString);
  Begin
    Check(X = Y, 'Expected "'+ Y + '" but got "'+X+'"');
  End;

Begin
  S := '1234X';
  Expect(StringReverse(S), 'X4321');
End;

Procedure TERRAString_TestTrim.Run;
Var
  S, Dog, Cat:TERRAString;

  Procedure Expect(Const X, Y: TERRAString);
  Begin
    Check(X = Y, 'Expected "'+ Y + '" but got "'+X+'"');
  End;

Begin
  S := '   <A2>';
  Expect(StringTrimLeft(S), '<A2>');

  S := '<ABC>   ';
  Expect(StringTrimRight(S), '<ABC>');

  S := '   <XYZ>   ';
  Expect(StringTrim(S), '<XYZ>');
End;

Procedure TERRAString_TestConversions.Run;
  Procedure Expect(Const X, Y: TERRAString);
  Begin
    Check(X = Y, 'Expected "'+ Y + '" but got "'+X+'"');
  End;

Begin
  Expect(FloatProperty.Stringify(3.14159, 4), '3.1415');
  Expect(FloatProperty.Stringify(3.14159, 5), '3.14159');
  Expect(FloatProperty.Stringify(3.14159, 6), '3.141590');

  Expect(FloatProperty.Stringify(-7.85679, 4), '-7.8567');
  Expect(FloatProperty.Stringify(-7.85679, 5), '-7.85679');
  Expect(FloatProperty.Stringify(-7.85679, 6), '-7.856790');

  Expect(FloatProperty.Stringify(-0.6632), '-0.66320');

  Expect(FloatProperty.Stringify(0.0156), '0.01560');

  Expect(FloatProperty.Stringify(0.00123, 4), '0.0012');
  Expect(FloatProperty.Stringify(0.00123, 5), '0.00123');
  Expect(FloatProperty.Stringify(0.00123, 6), '0.001230');
End;

End.