Unit TERRA_Localization;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_IO;

Const
  language_English   = 'EN';
  language_German    = 'DE';
  language_French    = 'FR';
  language_Portuguese= 'PT';
  language_Spanish   = 'ES';
  language_Italian   = 'IT';
  language_Japanese  = 'JP';
  language_Chinese   = 'ZH';
  language_Russian   = 'RU';
  language_Korean    = 'KO';

  invalidString = '???';

  MaxPinyinSuggestions = 64;

Type
  StringEntry = Record
    Key:AnsiString;
    Value:AnsiString;
    Group:Integer;
  End;

  StringManager = Class(TERRAObject)
    Protected
      _Lang:AnsiString;
      _Strings:Array Of StringEntry;
      _StringCount:Integer;

      Function GetLang:AnsiString;

      Function FormatString(Const Text:AnsiString):AnsiString;

    Public
      Constructor Create;
      
      Class Function Instance:StringManager;
      Procedure SetLanguage(Lang:AnsiString);

      Function GetString(Key:AnsiString; Group:Integer = -1):AnsiString;
      Function HasString(Key:AnsiString):Boolean;
      Procedure SetString(Key, Value:AnsiString; Group:Integer = -1);
      Function EmptyString():AnsiString;

      Procedure Reload();

      Procedure RemoveGroup(GroupID:Integer);
      Procedure MergeGroup(Source:Stream; GroupID:Integer);


      Property Language:AnsiString Read GetLang Write SetLanguage;
  End;

  PinyinSuggestion = Record
    ID:Word;
    Text:AnsiString;
  End;

  PinyinConverter = Class
    Protected
      _Suggestions:Array Of PinyinSuggestion;
      _SuggestionCount:Integer;

      Procedure Match(S:AnsiString);

    Public
      Constructor Create();

      Procedure GetSuggestions(Text:AnsiString);

      Function GetResult(Index:Integer):Word;

      Function Replace(Var Text:AnsiString; Index:Integer):Boolean;

      Property Results:Integer Read _SuggestionCount;
  End;

Function GetKoreanInitialJamo(N:Word):Integer;
Function GetKoreanMedialJamo(N:Word):Integer;
Function GetKoreanFinalJamo(N:Word):Integer;

Function MemoryToString(Const N:Cardinal):AnsiString;

Function IsSupportedLanguage(Const Lang:AnsiString):Boolean;

Function GetCurrencyForCountry(Const Country:AnsiString):AnsiString;

Function GetLanguageDescription(Lang:AnsiString):AnsiString;

Implementation
Uses TERRA_Application, TERRA_FileManager, TERRA_Log, TERRA_Unicode;

Var
  _Manager:StringManager = Nil;

Function IsSupportedLanguage(Const Lang:AnsiString):Boolean;
Begin
  Result := (Lang = language_English) Or (Lang = language_German)
             Or (Lang = language_French)  Or (Lang = language_Portuguese)
              Or (Lang = language_Spanish)  Or (Lang = language_Italian)
               Or (Lang = language_Japanese)  Or (Lang = language_Chinese)
                Or (Lang = language_Russian)  Or (Lang = language_Korean);
End;

Function GetCurrencyForCountry(Const Country:AnsiString):AnsiString;
Begin
  If (Country = 'GB') Then
  Begin
    Result := 'GBP';
  End Else
  If (Country = 'RU') Then
  Begin
    Result := 'RUB';
  End Else
  If (Country = 'BR') Then
  Begin
    Result := 'BRL';
  End Else
  If (Country = 'US') Then
  Begin
    Result := 'USD';
  End Else
  If (Country = 'JP') Then
  Begin
    Result := 'JPY';
  End Else
  If (Country = 'KR') Then
  Begin
    Result := 'KRW';
  End Else
  If (Country = 'UA') Then
  Begin
    Result := 'UAH';
  End Else
  If (Country = 'AU') Then
  Begin
    Result := 'AUD';
  End Else
  If (Country = 'CA') Then
  Begin
    Result := 'CAD';
  End Else
  If (Country = 'ID') Then
  Begin
    Result := 'IDR';
  End Else
  If (Country = 'MY') Then
  Begin
    Result := 'MYR';
  End Else
  If (Country = 'MX') Then
  Begin
    Result := 'MXN';
  End Else
  If (Country = 'NZ') Then
  Begin
    Result := 'NZD';
  End Else
  If (Country = 'NO') Then
  Begin
    Result := 'NOK';
  End Else
  If (Country = 'PH') Then
  Begin
    Result := 'PHP';
  End Else
  If (Country = 'SG') Then
  Begin
    Result := 'SGD';
  End Else
  If (Country = 'TH') Then
  Begin
    Result := 'THB';
  End Else
  If (Country = 'TR') Then
  Begin
    Result := 'TRY';
  End Else
  Begin
    Result := 'USD';
  End;
End;

Function GetKoreanInitialJamo(N:Word):Integer;
Begin
  Case N Of
	12593: Result := 0;
	12594: Result := 1;
	12596: Result := 2;
	12599: Result := 3;
	12600: Result := 4;
	12601: Result := 5;
	12609: Result := 6;
	12610: Result := 7;
	12611: Result := 8;
	12613: Result := 9;
	12614: Result := 10;
	12615: Result := 11;
	12616: Result := 12;
	12617: Result := 13;
	12618: Result := 14;
	12619: Result := 15;
	12620: Result := 16;
	12621: Result := 17;
	12622: Result := 18;
  Else
    Result := -1;
  End;
End;

Function GetKoreanMedialJamo(N:Word):Integer;
Begin
  Case N Of
	12623: Result := 0;
	12624: Result := 1;
	12625: Result := 2;
	12626: Result := 3;
	12627: Result := 4;
	12628: Result := 5;
	12629: Result := 6;
	12630: Result := 7;
	12631: Result := 8;
	12632: Result := 9;
	12633: Result := 10;
	12634: Result := 11;
	12635: Result := 12;
	12636: Result := 13;
	12637: Result := 14;
	12638: Result := 15;
	12639: Result := 16;
	12640: Result := 17;
	12641: Result := 18;
	12642: Result := 19;
	12643: Result := 20;
  Else
    Result := -1;
  End;
End;

Function GetKoreanFinalJamo(N:Word):Integer;
Begin
  Case N Of
	12593: Result := 1;
	12594: Result := 2;
	12595: Result := 3;
	12596: Result := 4;
	12597: Result := 5;
	12598: Result := 6;
	12599: Result := 7;
	12601: Result := 8;
	12602: Result := 9;
	12603: Result := 10;
	12604: Result := 11;
	12605: Result := 12;
	12606: Result := 13;
	12607: Result := 14;
	12608: Result := 15;
	12609: Result := 16;
	12610: Result := 17;
	12612: Result := 18;
	12613: Result := 19;
	12614: Result := 20;
	12615: Result := 21;
	12616: Result := 22;
	12618: Result := 23;
	12619: Result := 24;
	12620: Result := 25;
	12621: Result := 26;
	12622: Result := 27;
  Else
    Result := -1;
  End;
End;


Function MemoryToString(Const N:Cardinal):AnsiString;
Var
  Ext:AnsiChar;
  X:Single;
  Int,Rem:Integer;
Begin
  If (N>=1 Shl 30)Then
  Begin
    X:=N/(1 Shl 30);
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='G';
  End Else
  If (N>=1 Shl 20)Then
  Begin
    X:=N/(1 Shl 20);
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='M';
  End Else
  If (N>=1 Shl 10)Then
  Begin
    X:=N/(1 Shl 10);
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext:='K';
  End Else
  Begin
    Int:=N;
    Rem:=0;
    Ext:=#0;
  End;

  Result:=IntToString(Int);
  If Rem>0 Then
  Result:=Result+'.'+IntToString(Rem);
  Result:=Result+' ';
  If Ext<>#0 Then
    Result:=Result+Ext;

  If (Application.Instance<>Nil) And (Application.Instance.Language = language_Russian) Then
    Result := Result + UnicodeToUCS2(1073)
  Else
    Result := Result + 'b';
End;

{ StringManager }
Constructor StringManager.Create;
Begin
  _Lang := '';
  If Assigned(Application.Instance()) Then
    SetLanguage(Application.Instance.Language)
  Else
    SetLanguage('EN');
End;

Function StringManager.EmptyString:AnsiString;
Begin
  Result := InvalidString;
End;

Function StringManager.GetLang:AnsiString;
Begin
  If (_Lang ='') And (Assigned(Application.Instance())) Then
    SetLanguage(Application.Instance.Language);

  Result := _Lang;
End;

Procedure StringManager.SetString(Key, Value:AnsiString; Group:Integer = -1);
Var
  I:Integer;
Begin
  Key := UpStr(Key);
  For I:=0 To Pred(_StringCount) Do
  If (_Strings[I].Key = Key) Then
  Begin
    _Strings[I].Value := Value;
    _Strings[I].Group := Group;
    Exit;
  End;

  Inc(_StringCount);
  SetLength(_Strings, _StringCount);
  _Strings[Pred(_StringCount)].Key := UpStr(Key);
  _Strings[Pred(_StringCount)].Value := FormatString(Value);
  _Strings[Pred(_StringCount)].Group := Group;
End;

Function StringManager.GetString(Key:AnsiString; Group:Integer):AnsiString;
Var
  I:Integer;
Begin
  If (_Lang ='') And (Assigned(Application.Instance())) Then
    SetLanguage(Application.Instance.Language);

  Key := UpStr(Key);
  For I:=0 To Pred(_StringCount) Do
  If (_Strings[I].Key = Key) And ((_Strings[I].Group = Group) Or (Group<0)) Then
  Begin
    Result := _Strings[I].Value;
    Exit;
  End;

  Log(logWarning, 'Strings', 'String value for ['+Key+'] not found!');
  Result := Self.EmptyString;
End;

Class Function StringManager.Instance:StringManager;
Begin
  If Not Assigned(_Manager) Then
    _Manager := StringManager.Create;

  Result := _Manager;
End;

Procedure StringManager.MergeGroup(Source: Stream; GroupID:Integer);
Var
  Ofs, I:Integer;
  S, S2:AnsiString;
Begin
  If (Source = Nil ) Then
    Exit;

  Log(logDebug, 'Strings', 'Merging strings from '+Source.Name);
  Ofs := _StringCount;

  S := '';
  While Not Source.EOF Do
  Begin
    Source.ReadUnicodeLine(S);
    S := TrimLeft(S);
    If S='' Then
      Continue;
    I := Pos(',', S);
    S2 := Copy(S, 1, Pred(I));
    S2 := TrimRight(S2);

    S := Copy(S, Succ(I), MaxInt);
    S := TrimLeft(S);

    Inc(_StringCount);
    SetLength(_Strings, _StringCount);
    _Strings[Pred(_StringCount)].Key := UpStr(S2);
    _Strings[Pred(_StringCount)].Value := S;
    _Strings[Pred(_StringCount)].Group := GroupID;

    //Log(logDebug, 'Strings', 'Found '+S2 +' = '+S);
  End;

  For I:=Ofs To Pred(_StringCount) Do
    _Strings[I].Value := FormatString(_Strings[I].Value);
End;

Procedure StringManager.SetLanguage(Lang:AnsiString);
Var
  S, S2:AnsiString;
  Source:Stream;
  I:Integer;
Begin
  Lang := UpStr(Lang);
  If (Lang = 'CH') Or (Lang='CN') Then
    Lang := 'ZH';
  If (Lang = 'JA') Then
    Lang := 'JP';

  If (_Lang = Lang) Then
    Exit;

  S := 'translation_'+Lang+'.txt';
  S := LowStr(S);
  S := FileManager.Instance.SearchResourceFile(S);
  If S='' Then
  Begin
    Log(logWarning, 'Strings', 'Could not find translation file for lang='+Lang);

    If (Lang<>language_English) Then
      SetLanguage(language_English);

    Exit;
  End;

  _StringCount := 0;
  Source := FileManager.Instance.OpenFileStream(S);
  _Lang := Lang;
  Self.MergeGroup(Source, -1);
  Source.Destroy;

  If Application.Instance<>Nil Then
    Application.Instance.Language := Lang;
End;

Procedure StringManager.Reload();
Var
  S:AnsiString;
Begin
  S := _Lang;
  _Lang := '';

  SetLanguage(S);
End;

Procedure StringManager.RemoveGroup(GroupID: Integer);
Var
  I:Integer;
Begin
  I := 0;
  While (I<_StringCount) Do
  If (_Strings[I].Group = GroupID) Then
  Begin
    _Strings[I] := _Strings[Pred(_StringCount)];
    Dec(_StringCount);
  End Else
    Inc(I);
End;

Function StringManager.HasString(Key:AnsiString): Boolean;
Begin
  Result := GetString(Key)<>Self.EmptyString;
End;

Function StringManager.FormatString(Const Text:AnsiString):AnsiString;
Var
  I,J,N:Integer;
  Len:Integer;
  S2, S3, S:AnsiString;
  C:AnsiChar;
Begin
  I := ucs2_Pos('@', Text);
  If I<=0 Then
  Begin
    Result := Text;
    Exit;
  End;

  S := Text;
  S2 := ucs2_Copy(S, 1, Pred(I));
  S := ucs2_Copy(S, Succ(I), MaxInt);
  N := 0;
  J := 1;
  Len := ucs2_Length(S);
  While J<=Len Do
  Begin
    C := ucs2_ascii(S, J);
    If ((C>='a') And (C<='z')) Or ((C>='A') And (C<='Z')) Or ((C>='0') And (C<='9')) Or (C='_') Then
    Begin
      N := J;
      Inc(J);
    End Else
      Break;
  End;

  If (N>0) Then
  Begin
    S3 := ucs2_Copy(S, 1, N);
    S := ucs2_Copy(S, Succ(N), MaxInt);
    S3 := Self.GetString(S3);
  End Else
    S3 := '';

  Result := S2 + S3 + S;

  I := ucs2_Pos('@', Result);
  If I>0 Then
  Begin
    Result := FormatString(Result);
    StringToInt(result+text);
  End;
End;

Type
  PinyinEntry = Record
    ID:Word;
    Text:AnsiString;
  End;

Var
  _PinyinCount:Integer;
  _PinyinData:Array Of PinyinEntry;

{ PinyinConverter }
Constructor PinyinConverter.Create;
Var
  Src:Stream;
  I:Integer;
Begin
  If (_PinyinCount>0) Then
    Exit;

  Src := FileManager.Instance.OpenFileStream('pinyin.dat');
  If Src = Nil Then
    Exit;

  Src.Read(@_PinyinCount, 4);
  SetLength(_PinyinData ,_PinyinCount);

  I := 0;
  While (Not Src.EOF) And (I<_PinyinCount) Do
  Begin
    Src.Read(@_PinyinData[I].ID, 2);
    Src.ReadString(_PinyinData[I].Text);
    Inc(I);
  End;
End;

Procedure PinyinConverter.GetSuggestions(Text:AnsiString);
Const
  MaxLength = 7;
Var
  N, I:Integer;
  Temp:AnsiString;
Begin
  _SuggestionCount :=0 ;

  N := -1;
  I := Length(Text);
  While I>=1 Do
  If (Text[I]=#255) Then
  Begin
    N := I + 3;
    Break;
  End Else
    Dec(I);

  If (N>0) Then
    Text := Copy(Text, N, MaxInt);

  If (Text='') Then
    Exit;

  If Length(Text)>MaxLength Then
    Text := Copy(Text, Length(Text)-MaxLength, MaxInt);

  Text := LowStr(Text);

  Temp := Text;
  While Text<>'' Do
  Begin
    Match(Text);
    Text := Copy(Text, 2, MaxInt);
  End;

  Text := Temp;
  While Text<>'' Do
  Begin
    Match(Text);
    Text := Copy(Text, 1, Pred(Length(Text)));
  End;

End;

Function PinyinConverter.GetResult(Index: Integer): Word;
Begin
  If (Index>=0) And (Index<Self.Results) Then
    Result := _Suggestions[Index].ID
  Else
    Result := 0;
End;

Procedure PinyinConverter.Match(S:AnsiString);
Var
  I:Integer;
Begin
  If (_SuggestionCount>=MaxPinyinSuggestions) Then
    Exit;

  For I:=0 To Pred(_PinyinCount) Do
  If (_PinyinData[I].Text = S) Then
  Begin
    Inc(_SuggestionCount);
    SetLength(_Suggestions, _SuggestionCount);
    _Suggestions[Pred(_SuggestionCount)].ID := _PinyinData[I].ID;
    _Suggestions[Pred(_SuggestionCount)].Text := S;
  End;
End;

Function PinyinConverter.Replace(var Text:AnsiString; Index: Integer):Boolean;
Var
  I:Integer;
  S,S2:AnsiString;
Begin
  Result := False;
  I := PosRev(_Suggestions[Index].Text, Text);
  If (I<=0) Then
    Exit;

  S := Copy(Text, 1, Pred(I));
  S2 := Copy(Text, I+Length(_Suggestions[Index].Text), MaxInt);

  Text := S + UnicodeToUCS2(_Suggestions[Index].ID) + S2;

  Result := True;
End;

Function GetLanguageDescription(Lang:AnsiString):AnsiString;
Begin
  Lang := UpStr(Lang);

  If Lang = language_English Then
    Result := 'English'
  Else
  If Lang = language_German Then
    Result := 'Deutsch'
  Else
  If Lang = language_Spanish Then
    Result := 'Español'
  Else
  If Lang = language_Portuguese Then
    Result := 'Português'
  Else
  If Lang = language_French Then
    Result := 'Français'
  Else
  If Lang = language_Italian Then
    Result := 'Italiano'
  Else
  If Lang = language_Russian Then
    Result := #255#4#32#255#4#67#255#4#65#255#4#65#255#4#58#255#4#56#255#4#57
  Else
  If Lang = language_Korean Then
    Result := #255#213#92#255#174#0
  Else
  If Lang = language_Japanese Then
    Result := #255#101#229#255#103#44#255#48#110
  Else
  If Lang = language_Chinese Then
    Result := #255#78#45#255#86#253
  Else
    Result := invalidString;
End;

Initialization
Finalization
  If Assigned(_Manager) Then
    _Manager.Destroy;
End.