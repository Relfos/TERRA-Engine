// iOS custom IO
Unit TERRA_FileStream;

{$I terra.inc}

Interface
Uses TERRA_Stream, TERRA_String, TERRA_FileUtils, SysUtils;

Type
  FilePointer = Pointer;

  FileStream=Class(Stream)
     Protected
        _File:FilePointer;
        _Open:Boolean;
     Public

        Constructor Create(FileName:TERRAString; StreamMode:Integer=smDefault);Overload;
        Constructor Open(FileName:TERRAString; StreamMode:Integer=smDefault);
        Procedure Release;Override;
        Procedure Delete;
        Procedure Rename(NewName:TERRAString);
        Procedure Truncate;Override;
        Function Read(Data:Pointer; Length:Cardinal):Cardinal; Override;
        Function Write(Data:Pointer; Length:Cardinal):Cardinal; Override;
        Procedure Seek(NewPosition:Cardinal);Override;

        Procedure Flush;

        Class Function Exists(Const FileName:TERRAString): Boolean;
        Class Procedure CopyFile(SourceName, DestName:TERRAString);
     End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Application, TERRA_MemoryStream, TERRA_FileManager, TERRA_Utils;

Function fopen (filename, mode :PAnsiChar):Pointer; cdecl; external;
Function fclose (stream:Pointer):Integer; cdecl; external;
Function fread(ptr:Pointer; size, count:Integer; stream:Pointer):Integer; cdecl; external;
Function fwrite(ptr:Pointer; size, count:Integer; stream:Pointer):Integer; cdecl; external;
Function ftell (stream:Pointer):Cardinal; cdecl; external;
Function fseek (stream:Pointer; offset:Cardinal; origin:Integer):Integer; cdecl; external;

Const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

Function fileExists(name:PAnsiChar):Boolean;
Var
f:Pointer;
Begin
 Log(logDebug, 'File', 'Testing file '+Name);
	f := fopen(name, 'rb');
	If (f<>Nil) Then
	Begin
    Log(logDebug, 'File', 'Found, closing file');
		fclose(f);
		Result := True;
    Log(logDebug, 'File', 'OK!!!');
	End Else
    Result := False;
End;

Function fileSize(f:Pointer):Integer;
Var
  Size,Ofs:Integer;
Begin
	ofs := ftell(f);
	fseek(f, 0, SEEK_END);
	size := ftell(f);
	fseek(f, ofs, SEEK_SET);
	Result := size;
End;

Class Function FileStream.Exists(Const FileName:TERRAString): Boolean;
Begin
  Result := FileExists(PAnsiChar(FileName));
End;

Constructor FileStream.Create(FileName:TERRAString; StreamMode:Integer=smDefault);
Begin
  Inherited Create(StreamMode);

  FileName := GetOSIndependentFileName(FileName);
  FileManager.Instance.RemoveFromCache(FileName);

  Log(logDebug,'IO','Opening '+FileName);
  If StreamMode=0 Then
    RaiseError('Invalid file mode.['+FileName+']')
  Else
  Begin
    _Name:=FileName;
    _Size:=0;
    FileMode := 2;

	  _File := fopen(PAnsiChar(_Name), PAnsiChar('wb'));

    _Open := Assigned(_File);
  End;
End;

Constructor FileStream.Open(FileName:TERRAString; StreamMode:Integer=smDefault);
Var
  FSize:Integer;
Begin
  Inherited Create(StreamMode);

  FileName := GetOSIndependentFileName(FileName);
  Log(logDebug, 'FileIO', 'File.IO:Opening '+FileName);

  _Open := False;
  _Name := FileName;

  If Not FileExists(PAnsiChar(FileName)) Then
  Begin
    RaiseError('File not found. ['+FileName+']');
    Exit;
  End;

  If StreamMode=0 Then
  Begin
    RaiseError('Invalid file mode. ['+FileName+']');
    Exit;
  End Else
  Begin
	  _File := fopen(PAnsiChar(_Name), PAnsiChar('rb'));
  	If Not Assigned(_File) Then
    Begin
      RaiseError('Cannot open file: '+FileName);
      Exit;
    End;

	  FSize := FileSize(_File);

    If FSize>=0 Then
      _Size := FSize
    Else
      _Size := 0;

    _Pos := 0;

  	Log(logDebug, 'IO', 'filesize '+IntToString(_Size));

    _Open := True;
  End;
End;

Procedure FileStream.Release;
Begin
  If Not _Open Then
    Exit;

    Log(logDebug, 'File', 'Closing file '+Name);
  fclose(_File);
End;

Procedure FileStream.Delete;
Begin
  If Not _Open Then
    Exit;
End;

Procedure FileStream.Truncate;
Begin
  If Not _Open Then
    Exit;

  _Size:=_Pos;
End;

Procedure FileStream.Rename(NewName:TERRAString);
Begin
  If Not _Open Then
    Exit;
End;

Function FileStream.Read(Data:Pointer;  Length:Cardinal):Cardinal;
Begin
   Result:=0;

  If (Length=0) Or(Not _Open) Then
  Begin
    Exit;
  End;

  If (_Mode And smRead=0)Or(_Pos>=_Size) Then
  Begin
    RaiseError('Cannot read from file. ['+_Name+']');
    Exit;
  End;

  If (_Pos+Length >_Size)Then
  Begin
    If (_Size<_Pos) Then
        Exit;
    Length := _Size - _Pos;
  End;

  If (Length<=0) Then
  Begin
    Exit;
  End;

  Length := fread(Data, 1, Length, _File);

  Inc(_Pos, Length);
  Result := Length;
End;

Function FileStream.Write(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Log(logDebug, 'App', 'Writing to file! ' + Self._Name);

  Result := 0;

  If (Not _Open) Then
    Exit;

  If (_Mode And smWrite=0)Then
  Begin
    RaiseError('File is write protected.['+_Name+']');
    Exit;
  End;

  If (_Pos+Length>_Size) And (_Mode And smDynamic=0) Then
  Begin
    RaiseError('Cannot write to file.['+_Name+']');
    Exit;
  End;

  Length := fwrite(Data, 1, Length, _File);

  Inc(_Pos, Length);
  If _Pos>_Size Then
    _Size := _Pos;

  Result := Length;
End;

Procedure FileStream.Seek(NewPosition:Cardinal);
Begin
  If _Pos>_Size Then
  Begin
    RaiseError('Cannot seek in file.['+_Name+']');
    Exit;
  End;
  _Pos := NewPosition;

	fseek(_File, _Pos, SEEK_SET);
End;


Class Procedure FileStream.CopyFile(SourceName, DestName:TERRAString);
Var
  Source, Dest:Stream;
Begin
  Source := MemoryStream.Create(SourceName);
  Dest := FileStream.Create(DestName);
  Source.Copy(Dest);
  Source.Release;
  Dest.Release;
End;

Procedure FileStream.Flush;
Begin
  If (Assigned(_File)) And (_Open) Then
  Begin
    fclose(_File);
	  _File := fopen(PAnsiChar(_Name), PAnsiChar('ab'));
    _Open := Assigned(_File);
  End;
End;

End.
