Unit TERRA_LocalizationImport;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream;


Type
  TranslationEntry = Record
    Key:TERRAString;
    Value:TERRAString;
  End;

  TranslationImporter = Class(TERRAObject)
    Protected
      _Suffix:TERRAString;
      _FileName:TERRAString;
      _Strings:Array Of TranslationEntry;
      _StringCount:Integer;

      Function FormatString(Const Text:TERRAString):TERRAString;
      Function GetString(Const Key:TERRAString):TERRAString;

    Public
      Constructor Create(Const FileName:TERRAString);

      Procedure Convert(Dest:Stream);
  End;

Function ImportTranslation(SourceFile, TargetDir:TERRAString):TERRAString;

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_Error, TERRA_MemoryStream, TERRA_FileStream, TERRA_FileUtils,
  TERRA_FileImport, TERRA_Localization;


Function ImportTranslation(SourceFile, TargetDir:TERRAString):TERRAString;
Var
  Dest, Temp:Stream;
  DestFile:TERRAString;
  Importer:TranslationImporter;
Begin
  Log(logConsole, 'Importer', 'Importing localization: '+SourceFile+'...');

  TargetDir := TargetDir + PathSeparator + 'system';

  DestFile := TargetDir + PathSeparator + GetFileName(SourceFile, True) + '.' + Translation_Extension;
  Result := DestFile;

  If (Not AssetModified(SourceFile, Result)) Then
  Begin
    Log(logConsole, 'Import', 'Skipping '+SourceFile+'...[Not modified]');
    Result := DestFile;
    Exit;
  End;

  Importer := TranslationImporter.Create(SourceFile);

  Dest := MemoryStream.Create();
  Importer.Convert(Dest);
  Dest.Truncate();

  Log(logConsole, 'Import', 'Writing '+DestFile+'...');
  
  Temp := FileStream.Create(DestFile);
  Dest.Seek(0);
  Dest.Copy(Temp);
  Temp.Release();
  Dest.Release();

  Importer.Release();
End;

{ TranslationImporter }
Constructor TranslationImporter.Create(Const FileName:TERRAString);
Var
  S:TERRAString;
  Src: Stream;
  I:Integer;
Begin
  _FileName := FileName;

  S := GetFileName(_FileName, True);
  _Suffix := StringGetNextSplit(S, Ord('_'));

  Log(logConsole, 'Import', 'Loading '+FileName+'...');
  Src := MemoryStream.Create(FileName);
  While Not Src.EOF Do
  Begin
    Src.ReadLine(S);

    Inc(_StringCount);
    SetLength(_Strings, _StringCount);
    _Strings[Pred(_StringCount)].Key := StringGetNextSplit(S, Ord(','));
    _Strings[Pred(_StringCount)].Value := S;
  End;
  Src.Release();

  For I:=0 To Pred(_StringCount) Do
    _Strings[I].Value := FormatString(_Strings[I].Value);
End;

Procedure TranslationImporter.Convert(Dest: Stream);
Var
  I:Integer;
  Header:FileHeader;
Begin
  Header := Translation_Header;
  Dest.WriteHeader(Header);
  Dest.WriteInteger(_StringCount);
  For I:=0 To Pred(_StringCount) Do
  Begin
    Dest.WriteString(_Strings[I].Key);
    Dest.WriteString(_Strings[I].Value);
  End;
End;

Function TranslationImporter.FormatString(Const Text:TERRAString):TERRAString;
Var
  S1, S2, S3, S4, Replacement:TERRAString;
  C:TERRAChar;
  It:StringIterator;
  IsValidChar:Boolean;
Begin
  If Not StringSplitByChar(Text, S1, S2, Ord('@')) Then
  Begin
    Result := Text;
    Exit;
  End;

  Replacement := '';
  S4 := '';

  StringCreateIterator(S2, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();
    IsValidChar := ((C>=Ord('a')) And (C<=Ord('z'))) Or ((C>=Ord('A')) And (C<=Ord('Z'))) Or ((C>=Ord('0')) And (C<=Ord('9'))) Or (C=Ord('_'));

    If Not IsValidChar Then
    Begin
      It.Split(S3, S4);
      StringPrependChar(S4, C);

      Replacement := Self.GetString(S3);

      If (Replacement = '') And (_Suffix<>'') Then
      Begin
        Replacement := Self.GetString(_Suffix+'_'+S3);
      End;

      If (Replacement = '') And (_Suffix<>'') Then
      Begin
        Replacement := Self.GetString(_Suffix+S3);
      End;

      If (Replacement = '') Then
      Begin
        RaiseError('Could not find replacement string for "'+S3+'" in '+_FileName);
      End;

      Break;
    End;
  End;

  Result := S1 + Replacement + FormatString(S4);
End;

Function TranslationImporter.GetString(const Key: TERRAString): TERRAString;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_StringCount) Do
  If (StringEquals(_Strings[I].Key, Key)) Then
  Begin
    Result := _Strings[I].Value;
    Exit;
  End;

  Result := '';
End;

End.
