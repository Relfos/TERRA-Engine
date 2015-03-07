Unit TERRA_Session;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_MemoryStream, TERRA_OS, TERRA_ProgressNotifier;

{-$DEFINE ALLOWBACKUPS}

Const
  DefaultSessionFileName = 'session';
  
Type
  KeyValue = Record
    Key:TERRAString;
    Value:TERRAString;
  End;

  Session = Class(TERRAObject)
    Protected
      _Path:TERRAString;
      _FileName:TERRAString;

      _Data:Array Of KeyValue;
      _DataCount:Integer;

      _Read:Boolean;

      _Backup:Boolean;


      Function getKeyID(Const Key:TERRAString):Integer;

      Function GetDefaultFilePath():TERRAString;

      Function FixPath(S:TERRAString):TERRAString;

      Function GetDataCount: Integer;

      Function GetBackUpFileName():TERRAString;

    Public

      Constructor Create(FileName:TERRAString; Backup:Boolean = False);

      Procedure Release; Override;

      Procedure SetValue(Const Key, Value:TERRAString);
      Function GetValue(Const Key:TERRAString):TERRAString;

      Function HasKey(Const Key:TERRAString):Boolean;

      Function LoadFromFile(SourceFile:TERRAString):Boolean;
      Function LoadFromStream(Source:MemoryStream):Boolean;

      Function GetKeyByIndex(Index:Integer):TERRAString;
      Function GetValueByIndex(Index:Integer):TERRAString;

      Function GetSaveFileName():TERRAString;

      Procedure Clear;

      Procedure SetFileName(FileName:TERRAString);
      Procedure SetPath(Path:TERRAString);

      Procedure CopyKeys(Other:Session);

      Function Save(Target:Stream = Nil; Callback:ProgressNotifier = Nil):Boolean;

      Function Restore():Boolean;

      Property KeyCount:Integer Read GetDataCount;
      Property Path:TERRAString Read _Path;
      Property FileName:TERRAString Read _FileName;
  End;

Implementation
Uses TERRA_FileStream, TERRA_Application, TERRA_FileUtils, TERRA_Log, TERRA_ZLib;

Const
  SessionHeader:FileHeader = 'TES2';
  OldSessionHeader:FileHeader = 'TESS';

Function IsNumber(S:TERRAString):Boolean;
Var
  I:Integer;
Begin
  For I:=1 To Length(S) Do
  If (S[I]>='0') And (S[I]<='9') Then
  Begin
    IntToString(1);
  End Else
  Begin
    Result := False;
    Exit;
  End;

  Result := True;
End;

Function OldSessionCypher(const Str:TERRAString):TERRAString;
Var
  I:Integer;
Begin
  Result := '';
  For I:=1 To Length(Str) Do
    result := Result + Char(255 - Ord(Str[I]));
End;

Function InvalidString(Const S:TERRAString):Boolean;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  StringCreateIterator(S, It);
  While It.HasNext() Do
  Begin
    C := It.GetNext();
    If (C<Ord(' ')) Then
    Begin
      Result := True;
      Exit;
    End;
  End;

  Result := False;
End;

Const
  ZBufferSize = 1024 * 64;

Function Session.LoadFromStream(Source:MemoryStream):Boolean;
Var
  I,J ,N ,Count, Len:Integer;
  Key, Path, S, S2:TERRAString;
  Pref:MemoryStream;
  Data:Array Of Byte;
  Header:FileHeader;
  OldSession:Boolean;
  ZLIB:z_stream;
  IsOldFile:Boolean;

    Procedure ReadOldString(Out S:TERRAString);
    Var
      Len2:Word;
      B:Byte;
    Begin
      S := '';

      Pref.ReadByte(B);
      If B<255 Then
        Len2 := B
      Else
        Pref.ReadWord(Len2);

      SetLength(S, Len2);
      If Len2>0 Then
        Pref.Read(@S[1], Len2);
    End;

Begin
  _DataCount := 0;
  IsOldFile := False;

  If (Source.Size<4) Then
  Begin
    Result := False;
    Log(logError,'Session','Corrupted session file');
    Exit;
  End;

  Source.Read(@Header, 4);
  If (CompareFileHeader(Header, SessionHeader)) Or (CompareFileHeader(Header, OldSessionHeader)) Then
  Begin
    OldSession := False;

    IsOldFile := (CompareFileHeader(Header, OldSessionHeader));

    Source.ReadInteger(Len);

    // Fill record
    FillChar(ZLIB,SizeOf(ZLIB),0);

    Pref := MemoryStream.Create(Len);

    ZLIB.next_in := Source.Buffer;
    Inc(ZLIB.next_in, Source.Position);
    ZLIB.avail_in := Source.Size - Source.Position;
    ZLIB.next_out := Pref.Buffer;
    ZLIB.avail_out := Pref.Size;

    inflateInit(ZLIB);
    inflate(ZLIB, Z_NO_FLUSH);
    inflateEnd(ZLIB);
  End Else
  Begin
    Pref := Source;
    Pref.Seek(0);
    OldSession := True;
  End;

  While Not Pref.EOF Do
  Begin
    If IsOldFile Then
      ReadOldString(S)
    Else
      Pref.ReadString(S);

    If (OldSession) And (S<>'') And (S[1]='@') Then
      S := OldSessionCypher(System.Copy(S, 2, MaxInt));

    //I := Self.getKeyID(S);
    Key := S;

    If Pref.EOF Then
      Break;

    If IsOldFile Then
      ReadOldString(S2)
    Else
      Pref.ReadString(S2);

    If (S2<>'') And (S2[1]='&') Then
    Begin
      Count := Length(S2) - 1;
      SetLength(Data, Count);
      If (Length(S2)>=2) Then
        Move(S2[2], Data[0], Count)
      Else
      For J:=0 To Pred(Count) Do
        Data[J] := 0;
      S2 := '';
      For J:=0 To Pred(Count) Do
        S2 := S2 + IntToString(Data[J]);
    End;

    If (Key = '') Or (InvalidString(Key)) Or (InvalidString(S2)) Then
      Continue;

    I := _DataCount;
    Inc(_DataCount);
    SetLength(_Data, _DataCount);

    _Data[I].Key := Key;
    _Data[I].Value := S2;
    //Log(logDebug,'Session','Session: '+_Data[I].Key+'='+_Data[I].Value);
  End;

  If Not OldSession Then
    Pref.Release;

  _Read := True;

  Log(logDebug,'Session','Loaded session file, '+IntToString(_DataCount)+' items found.');
  Result := True;
End;

Function Session.LoadFromFile(SourceFile:TERRAString):Boolean;
Var
  Temp:MemoryStream;
Begin
  If Not FileStream.Exists(SourceFile) Then
  Begin
    Result := False;
    Log(logError,'Session','Could not load session file: '+SourceFile);
    Exit;
  End;

  Temp := MemoryStream.Create(SourceFile);
  Result := LoadFromStream(Temp);
  Temp.Release();
End;

Function Session.GetSaveFileName:TERRAString;
Var
  S:TERRAString;
Begin
  S := FixPath(Path);
  If S<>'' Then
    S := S + PathSeparator;
  Result := S + _FileName;
End;

Function Session.Save(Target:Stream = Nil; Callback:ProgressNotifier = Nil):Boolean;
Var
  ZLIB:z_stream;
  OutBuff:Pointer;
  Ret, Rem:Integer;
  B:Byte;
  I, J, N, Len:Integer;
  Dest, Temp:MemoryStream;
  Pref:Stream;
  Key, S, S2, S3:TERRAString;
  FileName:TERRAString;
  Header:FileHeader;
Begin
  FileName := Self.GetSaveFileName();

  {$IFDEF ALLOWBACKUPS}
  If (FileStream.Exists(FileName)) And (_Backup) Then
  Begin
    FileStream.CopyFile(FileName, Self.GetBackUpFileName());
  End;
  {$ENDIF}

  Dest := MemoryStream.Create(1024, Nil, smDefault Or smLargeAlloc);

  If Assigned(Callback) Then
    Callback.Reset(2);

  For I:=0 To Pred(_DataCount) Do
  Begin
    If Assigned(Callback) Then
      Callback.Notify(I/Pred(_DataCount));

    Dest.WriteString(_Data[I].Key);
    Dest.WriteString(_Data[I].Value);
  End;

  If Assigned(Callback) Then
    Callback.NextPhase();

  Dest.Truncate();

  // Fill record
  FillChar(ZLIB,SizeOf(ZLIB),0);

  // Set internal record information
  GetMem(OutBuff, ZBufferSize);
  ZLIB.next_in := Dest.Buffer;
  ZLIB.avail_in := Dest.Size;

  // Init compression
  DeflateInit_(@zlib, Z_DEFAULT_COMPRESSION, zlib_version, SizeOf(z_Stream));


  Len := Dest.Size;
  Temp := MemoryStream.Create(Len);

//  With ZLIBStream, ZLIBStream.ZLIB Do
  // Compress all the data avaliable to compress
  Repeat
    If Assigned(Callback) Then
      Callback.Notify(ZLIB.avail_in/Dest.Size);

    ZLIB.next_out := OutBuff;
    ZLIB.avail_out := ZBufferSize;

    Ret := deflate(ZLIB, Z_FINISH);

    Rem := ZBufferSize - ZLIB.avail_out;

    If Rem>0 Then
      Temp.Write(OutBuff, Rem);
  Until (Ret<>Z_OK);

  Temp.Truncate();

  // Terminates compression
  DeflateEnd(zlib);

  /// Free internal record
  FreeMem(OutBuff);

  If (Ret = Z_STREAM_END) Then
  Begin
    Header := SessionHeader;
    Pref := FileStream.Create(FileName);
    Pref.Write(@Header, 4);
    Pref.WriteInteger(Len);
    Temp.Copy(Pref);
    Result := Pref.Size>=Temp.Size;

    If Assigned(Target) Then
    Begin
      If (Target.Size<Pref.Size) And (Target Is MemoryStream) Then
        MemoryStream(Target).Resize(Pref.Size);

      Temp.Seek(0);
      Target.Write(@Header, 4);
      Target.Write(@Len, 4);
      Temp.Copy(Target);
      Temp.Seek(0);
    End;

    Pref.Release;

    _Read := True;
  End Else
    Result := False;

  Temp.Release;
  Dest.Release;
End;


Function Session.getKeyID(Const Key:TERRAString):Integer;
Var
  I:Integer;
Begin
  Result := -1;

  For I:=0 To Pred(_DataCount) Do
  If StringEquals(_Data[I].Key, Key) Then
  Begin
    Result := I;
    Exit;
  End;

  Result := _DataCount;
  Inc(_DataCount);
  SetLength(_Data, _DataCount);

  _Data[Result].Key := Key;
End;

Procedure Session.SetValue(Const Key,Value:TERRAString);
Var
  N:Integer;
Begin
  N := Self.getKeyID(Key);
  _Data[N].Value := Value;
End;

Function Session.GetValue(Const Key:TERRAString):TERRAString;
Var
  N:Integer;
  S:TERRAString;
Begin
  If (Not _Read) Then
  Begin
    S := Self.GetSaveFileName();
    LoadFromFile(S);
  End;

  N := Self.getKeyID(Key);
  Result := _Data[N].Value;
End;

Procedure Session.Release;
Begin
  Self.Clear();
End;

Function Session.GetKeyByIndex(Index: Integer):TERRAString;
Begin
  If (Index>=0) And (Index<_DataCount) Then
  Begin
    Result := _Data[Index].Key;
  End Else
    Result := '';
End;

Function Session.GetValueByIndex(Index: Integer):TERRAString;
Begin
  If (Index>=0) And (Index<_DataCount) Then
  Begin
    Result := _Data[Index].Value;
  End Else
    Result := '';
End;

Function Session.Restore:Boolean;
Var
  FileName:TERRAString;
Begin
  Result := False;
  FileName := Self.GetBackUpFileName();

  If Not FileStream.Exists(FileName) Then
    Exit;

  Self.LoadFromFile(FileName);
  Result := True;
End;

Function Session.GetDefaultFilePath:TERRAString;
Begin
  If Assigned(Application.Instance()) Then
    Result := Application.Instance.DocumentPath + PathSeparator
  Else
    Result := '';
End;

Function Session.GetBackUpFileName:TERRAString;
Begin
(*  {$IFDEF ANDROID}
  Result := '/sdcard/';
  {$ELSE}
  Result := Self.GetDefaultFilePath();
  {$ENDIF}*)
  Result := Path + PathSeparator + Application.Instance.Client.GetAppID() +'_'+ GetFileName(_FileName, True) + '.bak';
End;

Constructor Session.Create(FileName:TERRAString; Backup:Boolean);
Begin
  If (FileName = '') Then
    FileName := DefaultSessionFileName;


  _Backup := Backup;

  Self._Path := Self.GetDefaultFilePath();
  Self._FileName := FileName;
End;

Procedure Session.SetPath(Path:TERRAString);
Begin
  Self._Path := Path;
End;

Function Session.FixPath(S:TERRAString):TERRAString;
Begin
  Result := S;
  If (Result<>'') And ((Result[Length(Result)]='/') Or (Result[Length(Result)]='\')) Then
    Result := Copy(Result, 1, Pred(Length(Result)));
End;

Procedure Session.CopyKeys(Other: Session);
Var
  I:Integer;
Begin
  If Other = Nil Then
    Exit;

  Self._DataCount := Other._DataCount;
  SetLength(Self._Data, Self._DataCount);

  For I:=0 To Pred(Self._DataCount) Do
  Begin
    _Data[I].Key := Other._Data[I].Key;
    _Data[I].Value := Other._Data[I].Value;
  End;
End;

Procedure Session.SetFileName(FileName:TERRAString);
Begin
  If FileName<>'' Then
    _FileName := StringLower(FileName);
End;

Procedure Session.Clear;
Begin
  Self._DataCount := 0;
  SetLength(_Data, 0);
  _Read := False;
End;

Function Session.GetDataCount: Integer;
Begin
  If (Not _Read) Then
  Begin
    LoadFromFile(Self.GetSaveFileName());
  End;

  Result := _DataCount;
End;

Function Session.HasKey(Const Key:TERRAString): Boolean;
Var
  I:Integer;
Begin
  If (Not _Read) Then
  Begin
    LoadFromFile(Self.GetSaveFileName());
  End;

  For I:=0 To Pred(_DataCount) Do
  If StringEquals(_Data[I].Key, Key) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Initialization
Finalization
End.
