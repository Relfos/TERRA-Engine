Unit TERRA_Localization;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Application, TERRA_Utils, TERRA_Stream, TERRA_FileUtils,
  TERRA_Collections, TERRA_Hashmap;

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

  Translation_Extension = 'ltx';
  Translation_Header: FileHeader = 'LTX1';

  MaxPinyinSuggestions = 64;

Type
  StringEntry = Class(CollectionObject)
    Protected
      _Value:TERRAString;
      _Group:Integer;

    Public
      Constructor Create(Const Key, Value:TERRAString; Group:Integer);
  End;

  LocalizationManager = Class(ApplicationComponent)
    Protected
      _Lang:TERRAString;
      _Strings:Hashmap;

      Function GetLang:TERRAString;

    Public
      Procedure Init; Override;
      Procedure Release; Override;

      Class Function Instance:LocalizationManager;
      Procedure SetLanguage(Lang:TERRAString);

      Function GetString(Const Key:TERRAString):TERRAString;
      Function HasString(Const Key:TERRAString):Boolean;
      Procedure SetString(Const Key, Value:TERRAString; Group:Integer = -1);
      Function EmptyString():TERRAString;

      Procedure Reload();

      Procedure RemoveGroup(GroupID:Integer);
      Procedure MergeGroup(Source:Stream; GroupID:Integer; Const Prefix:TERRAString);


      Property Language:TERRAString Read GetLang Write SetLanguage;

      Property Strings[Const Key:TERRAString]:TERRAString Read GetString; Default;
  End;

  PinyinSuggestion = Record
    ID:Word;
    Text:TERRAString;
  End;

  PinyinConverter = Class(TERRAObject)
    Protected
      _Suggestions:Array Of PinyinSuggestion;
      _SuggestionCount:Integer;

      Procedure Match(S:TERRAString);

    Public
      Constructor Create();

      Procedure GetSuggestions(Text:TERRAString);

      Function GetResult(Index:Integer):Word;

      Function Replace(Var Text:TERRAString; Index:Integer):Boolean;

      Property Results:Integer Read _SuggestionCount;
  End;

Function GetKoreanInitialJamo(N:TERRAChar):Integer;
Function GetKoreanMedialJamo(N:TERRAChar):Integer;
Function GetKoreanFinalJamo(N:TERRAChar):Integer;

Function MemoryToString(Const N:Cardinal):TERRAString;

Function IsSupportedLanguage(Const Lang:TERRAString):Boolean;

Function GetCurrencyForCountry(Const Country:TERRAString):TERRAString;

Function GetLanguageDescription(Lang:TERRAString):TERRAString;

Implementation
Uses TERRA_FileManager, TERRA_Log, TERRA_OS;

Var
  _LocalizationManager_Instance:ApplicationObject = Nil;

Function IsSupportedLanguage(Const Lang:TERRAString):Boolean;
Begin
  Result := (Lang = language_English) Or (Lang = language_German)
             Or (Lang = language_French)  Or (Lang = language_Portuguese)
              Or (Lang = language_Spanish)  Or (Lang = language_Italian)
               Or (Lang = language_Japanese)  Or (Lang = language_Chinese)
                Or (Lang = language_Russian)  Or (Lang = language_Korean);
End;

Function GetCurrencyForCountry(Const Country:TERRAString):TERRAString;
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

Function GetKoreanInitialJamo(N:TERRAChar):Integer;
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

Function GetKoreanMedialJamo(N:TERRAChar):Integer;
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

Function GetKoreanFinalJamo(N:TERRAChar):Integer;
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


Function MemoryToString(Const N:Cardinal):TERRAString;
Var
  Ext:Char;
  X:Single;
  Int,Rem:Integer;
Begin
  If (N>=1 Shl 30)Then
  Begin
    X:=N/(1 Shl 30);
    Int:=Trunc(X);
    Rem:=Trunc(Frac(X)*10);
    Ext := 'G';
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

  If (Application.Instance.Language = language_Russian) Then
    StringAppendChar(Result, 1073)
  Else
    Result := Result + 'b';
End;

{ LocalizationManager }
Procedure LocalizationManager.Init();
Begin
  _Lang := '';
  _Strings := HashMap.Create(1024);

  If Assigned(Application.Instance()) Then
    SetLanguage(Application.Instance.Language)
  Else
    SetLanguage('EN');
End;

Procedure LocalizationManager.Release();
Begin
  ReleaseObject(_Strings);
End;

Function LocalizationManager.EmptyString:TERRAString;
Begin
  Result := InvalidString;
End;

Function LocalizationManager.GetLang:TERRAString;
Begin
  If (_Lang ='') And (Assigned(Application.Instance())) Then
    SetLanguage(Application.Instance.Language);

  Result := _Lang;
End;

Procedure LocalizationManager.SetString(Const Key, Value:TERRAString; Group:Integer = -1);
Var
  I:Integer;
  Entry:StringEntry;
Begin
  Entry := StringEntry(_Strings.GetItemByKey(Key));
  If Assigned(Entry) Then
  Begin
    Entry._Value := Value;
    Entry._Group := Group;
    Exit;
  End;

  Entry := StringEntry.Create(Key, Value, Group);
  _Strings.Add(Entry);
End;

Function LocalizationManager.GetString(Const Key:TERRAString):TERRAString;
Var
  Entry:StringEntry;
Begin
  If (_Lang ='') And (Assigned(Application.Instance())) Then
    SetLanguage(Application.Instance.Language);

  Entry := StringEntry(_Strings.GetItemByKey(Key));
  If Assigned(Entry) Then
  Begin
    Result := Entry._Value;
    Exit;
  End;

  Log(logWarning, 'Strings', 'String value for ['+Key+'] not found!');
  Result := Self.EmptyString;
End;

Class Function LocalizationManager.Instance:LocalizationManager;
Begin
  If _LocalizationManager_Instance = Nil Then
    _LocalizationManager_Instance := InitializeApplicationComponent(LocalizationManager, Nil);

  Result := LocalizationManager(_LocalizationManager_Instance.Instance);
End;

Procedure LocalizationManager.MergeGroup(Source: Stream; GroupID:Integer; Const Prefix:TERRAString);
Var
  Ofs, Count, I:Integer;
  Header:FileHeader;
  Value, Key:TERRAString;
Begin
  If (Source = Nil ) Then
    Exit;

  Log(logDebug, 'Strings', 'Merging strings from '+Source.Name);
  Ofs := _Strings.Count;

  Source.ReadHeader(Header);
  If Not CompareFileHeader(Header, Translation_Header) Then
  Begin
    Log(logError, 'Strings', 'Invalid  file header in '+Source.Name);
    Exit;
  End;

  Source.ReadInteger(Count);

  For I:=0 To Pred(Count) Do
  Begin
    Source.ReadString(Key);
    Source.ReadString(Value);

    If Prefix<>'' Then
      Key := Prefix + Key;

    Self.SetString(Key, Value, GroupID);
    //Log(logDebug, 'Strings', 'Found '+_Strings[I+Ofs].Key +' = '+_Strings[I+Ofs].Value);
  End;
End;

Procedure LocalizationManager.SetLanguage(Lang:TERRAString);
Var
  S, S2:TERRAString;
  Source:Stream;
  I:Integer;
Begin
  Lang := StringUpper(Lang);
  If (Lang = 'CH') Or (Lang='CN') Then
    Lang := 'ZH';
  If (Lang = 'JA') Then
    Lang := 'JP';

  If (_Lang = Lang) Then
    Exit;

  S := 'translation_'+ StringLower(Lang)+'.'+ Translation_Extension;
  S := FileManager.Instance.SearchResourceFile(S);
  If S='' Then
  Begin
    Log(logWarning, 'Strings', 'Could not find translation file for lang='+Lang);

    If (Lang<>language_English) Then
      SetLanguage(language_English);

    Exit;
  End;

  _Strings.Clear();
  Source := FileManager.Instance.OpenStream(S);
  _Lang := Lang;
  Self.MergeGroup(Source, -1, '');
  ReleaseObject(Source);
End;

Procedure LocalizationManager.Reload();
Var
  S:TERRAString;
Begin
  S := _Lang;
  _Lang := '';

  SetLanguage(S);
End;

Procedure LocalizationManager.RemoveGroup(GroupID: Integer);
Var
  It:Iterator;
  Entry:StringEntry;
Begin
  It := _Strings.GetIterator();
  While (It.HasNext) Do
  Begin
    Entry := StringEntry(It.Value);
    If (Entry._Group = GroupID) Then
      Entry.Discard();
  End;
End;

Function LocalizationManager.HasString(Const Key:TERRAString): Boolean;
Var
  Temp:StringEntry;
Begin
  Temp := StringEntry(_Strings.GetItemByKey(Key));
  Result := (Assigned(Temp)) And (Temp._Value<>'');
End;
     
Type
  PinyinEntry = Record
    ID:Word;
    Text:TERRAString;
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

  Src := FileManager.Instance.OpenStream('pinyin.dat');
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

Procedure PinyinConverter.GetSuggestions(Text:TERRAString);
Const
  MaxLength = 7;
Var
  It:StringIterator;
  N:Integer;
  Temp:TERRAString;
  C:TERRAChar;
Begin
  _SuggestionCount :=0 ;

  N := -1;
  StringCreateIterator(Text, It);
  While It.HasNext() Do
  Begin
    C := It.GetNext();
    If (C>255) Then
    Begin
      N := It.Position + 1;
      Break;
    End;
  End;

  If (N>0) Then
    Text := StringCopy(Text, N, MaxInt);

  If (Text='') Then
    Exit;

  If StringLength(Text)>MaxLength Then
    Text := StringCopy(Text, StringLength(Text )- MaxLength, MaxInt);

  Text := StringLower(Text);

  Temp := Text;
  While Text<>'' Do
  Begin
    Match(Text);
    Text := StringCopy(Text, 2, MaxInt);
  End;

  Text := Temp;
  While Text<>'' Do
  Begin
    Match(Text);
    Text := StringCopy(Text, 1, Pred(Length(Text)));
  End;

End;

Function PinyinConverter.GetResult(Index: Integer): Word;
Begin
  If (Index>=0) And (Index<Self.Results) Then
    Result := _Suggestions[Index].ID
  Else
    Result := 0;
End;

Procedure PinyinConverter.Match(S:TERRAString);
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

Function PinyinConverter.Replace(var Text:TERRAString; Index: Integer):Boolean;
Var
  I:Integer;
  S,S2:TERRAString;
Begin
  Result := False;
  I := StringPosReverse(_Suggestions[Index].Text, Text);
  If (I<=0) Then
    Exit;

  S := Copy(Text, 1, Pred(I));
  S2 := Copy(Text, I+Length(_Suggestions[Index].Text), MaxInt);

  Text := S;
  StringAppendChar(Text, _Suggestions[Index].ID);
  Text := Text + S2;

  Result := True;
End;

Function GetLanguageDescription(Lang:TERRAString):TERRAString;
Begin
  Lang := StringUpper(Lang);

  If Lang = language_English Then
    Result := 'English'
  Else
  If Lang = language_German Then
    Result := 'Deutsch'
  Else
  If Lang = language_Spanish Then
    Result := 'Espa'+StringFromChar(Ord('ñ'))+'ol'
  Else
  If Lang = language_Portuguese Then
    Result := 'Portugu'+StringFromChar(Ord('ê'))+'s'
  Else
  If Lang = language_French Then
    Result := 'Fran'+StringFromChar(Ord('ç'))+'ais'
  Else
  If Lang = language_Italian Then
    Result := 'Italiano'
  Else
  If Lang = language_Russian Then
    Result := StringFromChar(1056)+ StringFromChar(1091) + StringFromChar(1089) + StringFromChar(1089) + StringFromChar(1082) + StringFromChar(1080) + StringFromChar(1081)
  Else
  If Lang = language_Korean Then
    Result := StringFromChar(54620) + StringFromChar(44544)
  Else
  If Lang = language_Japanese Then
    Result := StringFromChar(26085) + StringFromChar(26412) + StringFromChar(35486)
  Else
  If Lang = language_Chinese Then
    Result := StringFromChar(20013) + StringFromChar(22269)
  Else
    Result := invalidString;
End;

{ StringEntry }
Constructor StringEntry.Create(const Key, Value: TERRAString; Group: Integer);
Begin
  Self._ObjectName := Key;
  Self._Value := Value;
  Self._Group := Group;
End;

End.