{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_FileUtils
 * Various file name/path utility functions
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}
Unit TERRA_FileUtils;
{$I terra.inc}
{$ENDIF}

Interface
Uses TERRA_String;

Type
  FileHeader = Array[1..4] Of AnsiChar;

Function GetFilePath(FileName:TERRAString):TERRAString;
Function GetLastFilePath(FileName:TERRAString):TERRAString;
Function GetFirstFilePath(FileName:TERRAString):TERRAString;
Function GetFileName(Const FileName:TERRAString;Const RemoveExt:Boolean):TERRAString;
Function GetFileExtension(FileName:TERRAString):TERRAString;

Function GetOSIndependentFileName(FileName:TERRAString):TERRAString;
Function GetOSIndependentFilePath(Path:TERRAString):TERRAString;

Function CompareFileHeader(Const A, B:FileHeader):Boolean;

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_Utils;

Function CompareFileHeader(Const A, B:FileHeader):Boolean;
Begin
  Result := (A[1] = B[1]) And (A[2] = B[2])  And (A[3] = B[3]) And (A[4] = B[4]);
End;

Function GetFilePath(FileName:TERRAString):TERRAString;
Var
  I:Integer;
  S:TERRAString;
Begin
  I := StringCharPos(Ord(PathSeparator), FileName);
  S := '';
  While I>=1 Do
  Begin
    S := S + Copy(FileName, 1, I);
    FileName := Copy(FileName,I+1, MaxInt);
    I := Pos(PathSeparator,FileName);
  End;
  Result := S;
End;

Function GetLastFilePath(FileName:TERRAString):TERRAString;
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

Function GetFirstFilePath(FileName:TERRAString):TERRAString;
Var
  I:Integer;
  S:TERRAString;
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

Function GetFileName(Const FileName:TERRAString; Const RemoveExt:Boolean):TERRAString;
Var
  I:Integer;
  List:StringArray;
Begin
  I := StringCharPos(Ord(':'), FileName);
  If I>=1 Then
    Result := StringCopy(FileName, (I+1), MaxInt)
  Else
    Result :=  FileName;


  I := StringCharPosReverse(Ord(PathSeparator), Result);
  If (I>0) Then
  Begin
    Result := StringCopy(Result, Succ(I), MaxInt);
  End;

  If RemoveExt Then
  Begin
    I := StringCharPosReverse(Ord('.'), Result);
    If I>0 Then
      Result := StringCopy(Result, 1, Pred(I));
  End;
End;

Function GetFileExtension(FileName:TERRAString):TERRAString;
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

Function GetOSIndependentFileName(FileName:TERRAString):TERRAString;
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

Function GetOSIndependentFilePath(Path:TERRAString):TERRAString;
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
    Path := StringLower(Path);
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
