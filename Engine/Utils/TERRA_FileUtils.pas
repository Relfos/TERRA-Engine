{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}
Unit TERRA_FileUtils;
{$I terra.inc}
{$ENDIF}

Interface

Type
  FileHeader = Array[1..4] Of AnsiChar;

Function GetFilePath(FileName:AnsiString):AnsiString;
Function GetLastFilePath(FileName:AnsiString):AnsiString;
Function GetFirstFilePath(FileName:AnsiString):AnsiString;
Function GetFileName(FileName:AnsiString;Const RemoveExt:Boolean):AnsiString;
Function GetFileExtension(FileName:AnsiString):AnsiString;

Function GetOSIndependentFileName(FileName:AnsiString):AnsiString;
Function GetOSIndependentFilePath(Path:AnsiString):AnsiString;

Function CompareFileHeader(Const A, B:FileHeader):Boolean;

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_Utils;

Function CompareFileHeader(Const A, B:FileHeader):Boolean;
Begin
  Result := (A[1] = B[1]) And (A[2] = B[2])  And (A[3] = B[3]) And (A[4] = B[4]);
End;

Function GetFilePath(FileName:AnsiString):AnsiString;
Var
  I:Integer;
  S:AnsiString;
Begin
  I := Pos(PathSeparator,FileName);
  S := '';
  While I>=1 Do
  Begin
    S := S + Copy(FileName, 1, I);
    FileName := Copy(FileName,I+1, MaxInt);
    I := Pos(PathSeparator,FileName);
  End;
  Result := S;
End;

Function GetLastFilePath(FileName:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  Result := GetFilePath(FileName);
  Result := Copy(Result, 1, Length(Result)-1);
  I := Pos(PathSeparator, Result);
  While (I>=1) Do
  Begin
    Result := Copy(Result, (I+1), MaxInt);
    I := Pos(PathSeparator, Result);
  End;
End;

Function GetFirstFilePath(FileName:AnsiString):AnsiString;
Var
  I:Integer;
  S:AnsiString;
Begin
  S := GetFilePath(FileName);
  S := Copy(S, 1, Length(S)-1);
  I := Pos(PathSeparator, S);
  Result:='';
  While (I>=1) Do
  Begin
    Result := Result + Copy(S, 1, I);
    S := Copy(S, (I+1), MaxInt);
    I := Pos(PathSeparator, S);
  End;
End;

Function GetFileName(FileName:AnsiString;Const RemoveExt:Boolean):AnsiString;
Var
  I:Integer;
Begin
  I := Pos(':',FileName);
  If I>=1 Then
    FileName := Copy(FileName, (I+1), MaxInt);

  I := Pos(PathSeparator,FileName);
  While I>=1 Do
  Begin
    FileName := Copy(FileName, I+1, MaxInt);
    I := PosRev(PathSeparator, FileName);
  End;

  Result:=FileName;

  If RemoveExt Then
  Begin
    I := PosRev('.',FileName);
    If I>=1 Then 
        Result := Copy(FileName, 1, I-1);
  End;
End;

Function GetFileExtension(FileName:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  Result:='';
  Repeat
    I := Pos('.',FileName);
    If I>=1 Then
    Begin
      Result := Copy(FileName, I+1, MaxInt);
      FileName := Copy(FileName, (I+1), MaxInt);
    End;
  Until I<1;
End;

Function GetOSIndependentFileName(FileName:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  Result := '';
  For I:=1 To Length(FileName) Do
    If (FileName[I]='\') Or (FileName[I]='/') Then
      Result := Result + PathSeparator
    Else
      Result := Result + FileName[I];
End;

Function GetOSIndependentFilePath(Path:AnsiString):AnsiString;
Begin
  If (Path ='') Then
  Begin
    Result := '';
    Exit;
  End;

  If (Path[Length(Path)] = PathSeparator) Then
    Delete(Path, Length(Path), 1);

{$IFDEF PC}
{  If (Not DirectoryExists(Path)) Then
  Begin
    Path := LowStr(Path);
    If (Not DirectoryExists(Path)) Then
    Begin
      Path := CapStr(Path);
      If (Not DirectoryExists(Path)) Then
      Begin
        RaiseError('Cannot find path: '+Path);
        Exit;
      End;
    End;
  End;}
{$ENDIF}
  Result := Path;
End;

End.
