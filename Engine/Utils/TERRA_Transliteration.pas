Unit TERRA_Transliteration;

Interface
Uses TERRA_Object, TERRA_String;

Function ConvertLatinToCyrillic(Const S:TERRAString):TERRAString;
Function ConvertLatinToKorean(Const S:TERRAString):TERRAString;

Function StringLocalize(Const S:TERRAString):TERRAString;

Implementation
Uses TERRA_Localization, TERRA_Engine;

Function StringLocalize(Const S:TERRAString):TERRAString;
Begin
    {Else
      Result := ConvertLatinToKorean(Result);}

  If Engine.Localization.Language = language_Russian Then
    Result := ConvertLatinToCyrillic(S)
  Else
    Result := S;
End;


Type
  CharTransliteratorFunction = Function(C:TERRAChar):TERRAChar;

Function TransliterateString(Const S:TERRAString; Converter:CharTransliteratorFunction):TERRAString;
Var
  C:TERRAChar;
  It:StringIterator;
Begin
  Result := '';

  It := StringCreateIterator(S);
  While It.HasNext() Do
  Begin
    C := It.GetNext();

    StringAppendChar(Result, Converter(C));
  End;
  ReleaseObject(It);
End;

Function LatinToCyrillicTransliterator(C:TERRAChar):TERRAChar;
Begin
  If (C>#128) Then
    Result := C
  Else
  Case AnsiChar(C) Of
    'A': Result := #1040;
    'B': Result := #1041;
    'V': Result := #1042;
    'G': Result := #1043;
    'D': Result := #1044;
    'J': Result := #1046;
    'Z': Result := #1047;
    'I': Result := #1048;
    'C', 'K': Result := #1050;
    'L': Result := #1051;
    'M': Result := #1052;
    'N': Result := #1053;
    'O': Result := #1054;
    'P': Result := #1055;
    'R': Result := #1056;
    'S': Result := #1057;
    'T': Result := #1058;
    'U': Result := #1059;
    'F': Result := #1060;
    'H': Result := #1061;

    'a': Result := #1072;
    'b': Result := #1073;
    'v': Result := #1074;
    'g': Result := #1075;
    'd': Result := #1076;
    'j': Result := #1078;
    'z': Result := #1079;
    'i': Result := #1080;
    'c', 'k': Result := #1082;
    'l': Result := #1083;
    'm': Result := #1084;
    'n': Result := #1085;
    'o': Result := #1086;
    'p': Result := #1087;
    'r': Result := #1088;
    's': Result := #1089;
    't': Result := #1090;
    'u': Result := #1091;
    'f': Result := #1092;
    'h': Result := #1093;
  Else
    Result := C;
  End;
End;

Function ConvertLatinToCyrillic(Const S:TERRAString):TERRAString;
Begin
  Result := S;
  StringReplaceText('ye', StringFromChar(#1076), Result);
  StringReplaceText('ts', StringFromChar(#1094), Result);
  StringReplaceText('ch', StringFromChar(#1095), Result);
  StringReplaceText('sh', StringFromChar(#1096), Result);
  StringReplaceText('yu', StringFromChar(#1102), Result);
  StringReplaceText('ya', StringFromChar(#1103), Result);

  Result := TransliterateString(Result, LatinToCyrillicTransliterator);
End;

Function LatinToKoreanTransliterator(C:TERRAChar):TERRAChar;
Begin
  If (C>#128) Then
    Result := C
  Else
  Case AnsiChar(C) Of
    'A': Result := #1040;
    'B': Result := #1041;
  Else
    Result := C;
  End;
End;

//https://code.google.com/p/conv2kr/source/browse/conv2kr.pm
//http://sori.org/hangul/conv2kr.cgi
//http://en.wikipedia.org/wiki/Korean_language_and_computers#Hangul_Syllables_Area
Function ConvertLatinToKorean(Const S:TERRAString):TERRAString;
Begin
  Result := S;
//  StringReplaceText('ye', StringFromChar(1076), Result);

  Result := TransliterateString(Result, LatinToKoreanTransliterator);
End;

End.