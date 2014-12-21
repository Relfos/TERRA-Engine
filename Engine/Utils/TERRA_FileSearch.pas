{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
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
 * TERRA_FileSearch
 * Implements listing/searching of files inside a directory. 
 ***********************************************************************************************************************
}

Unit TERRA_FileSearch;

{$I terra.inc}

{$IFDEF WINDOWS}{$UNDEF ANDROID}{$ENDIF}

{$IFDEF ANDROID}
{$DEFINE USEJAVA}
{$ENDIF}

Interface
Uses TERRA_Utils, TERRA_Collections
{$IFDEF USEJAVA},TERRA_Java{$ENDIF}
{$IFDEF NDS}{$IFDEF GBFS},TERRA_GBFS{$ENDIF}{$IFDEF LIBFAT}Fat, CTypes{$ENDIF}
{$ELSE}, SysUtils{$ENDIF};

Type
  FileInfo= Class(ListObject)
    Public
      Name:AnsiString;
      Path:AnsiString;
      Level:Integer;
      Size:Integer;

      Function ToString():AnsiString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

  Function SearchFiles(Path:AnsiString; Filter:AnsiString; SearchSubDirectories:Boolean):List;
  Function SearchFolders(Path:AnsiString):List;

{$IFDEF USEJAVA}
  Var
    IsFolderMode:Boolean;
    CurrentPath:AnsiString;
    CurrentLevel:Integer;
    CurrentFileDir:List;
{$ENDIF}

Implementation
Uses TERRA_OS, TERRA_IO, TERRA_FileIO, TERRA_FileUtils;

Function FileInfo.ToString():AnsiString;
Begin
  Result := Path+PathSeparator+Name;
End;

Procedure FileInfo.CopyValue(Other:ListObject);
Begin
  Self.Name := FileInfo(Other).Name;
  Self.Path := FileInfo(Other).Path;
  Self.Level := FileInfo(Other).Level;
  Self.Size := FileInfo(Other).Size;
End;

Function FileInfo.Sort(Other:ListObject):Integer;
Begin
  Result := GetStringSort(Self.ToString(), FileInfo(Other).ToString());
End;

Function FileInfo.GetHashKey():HashKey;
Begin
  Result := GetStringHashKey(Self.ToString());
End;

{$UNDEF HAS_IMPLEMENTATION}

{$IFDEF NDS}
{$DEFINE HAS_IMPLEMENTATION}
{$IFDEF GBFS}
Procedure FileSearch.Search(Path:AnsiString; Filter:AnsiString; SearchSubDirectories:Boolean; Level:Integer);
Var
  I, Count, Len:Integer;
  Buffer:Array[0..30] Of AnsiChar;
  FileName:AnsiString;
  P:PFileInfo;
Begin
  Count := gbfs_count_objs(@data_gbfs);
  For I:=0 To Pred(Count) Do
  Begin
    gbfs_get_nth_obj(@data_gbfs, I, @Buffer[0], @Len);
    FileName := Buffer;

    If Not MatchRegEx(FileName, Filter) Then
      Continue;
 
    New(P);
    P.Name := FileName;
    P.Size := Len;
    P.Path := Path;
    P.Level := Level;

    Self.AddItem(P);
  End;
End;
{$ENDIF}

{$IFDEF LIBFAT}
Procedure FileSearch.Search(Path:AnsiString; Filter:AnsiString; SearchSubDirectories:Boolean; Level:Integer);
Var
  FileName:AnsiString;
  dir: PDIR_ITER;
  P:PFileInfo;
  st:Stat;
Begin
  Dir := DirOpen(PAnsiChar(Path));
  If (Not Assigned(Dir)) Then
    Exit;

  While DirNext(Dir, PAnsiChar(@Filename), @st) = 0 Do
  Begin
    If Not MatchRegEx(FileName, Filter) Then
      Continue;

    // st.st_mode & _IFDIR indicates a directory
    If (st.st_mode and $4000) <> 0 Then
    Begin
      Search(PAnsiChar(@filename), Filter, SearchSubDirectories, Succ(Level));
    End Else
    Begin
      New(P);
      P.Name := filename;
      P.Size := St.st_size;
      P.Path := Path;
      P.Level := Level;
      P.Folder := False;

      Self.AddItem(P);
    End;
  End;

  DirClose(Dir);
End;
{$ENDIF}
{$ENDIF}

{$IFDEF USEJAVA}
{$DEFINE HAS_IMPLEMENTATION}

Procedure CallJavaListing(Filter:AnsiString);
Var
  AssetsClass:JavaClass;
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Java_Begin(Frame);
  AssetsClass := JavaClass.Create(FileIOClassPath, Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(CurrentPath);
  Params.AddString(Filter);
  Params.AddBoolean(IsFolderMode);
  AssetsClass.CallStaticVoidMethod('listFiles', Params);
  Params.Destroy();
  AssetsClass.Destroy();
  Java_End(Frame);
End;

Function Search(Path:AnsiString; Filter:AnsiString; SearchSubDirectories:Boolean; Level:Integer):List;
Var
  Folders, Temp:List;
  It:ListObject;
Begin
  Result := List.Create();

  IsFolderMode := False;
  CurrentPath := Path;
  CurrentLevel := Level;
  CurrentFileDir := Result;
  CallJavaListing(Filter);

  If SearchSubDirectories Then
  Begin
    Folders := SearchFolders(Path);
    It := Folders.First;
    While Assigned(It) Do
    Begin
      Temp := Search(Path + PathSeparator + StringObject(It).Value, Filter, SearchSubDirectories, Succ(Level));

      If (Temp<>Nil) Then
      Begin
        Result.Merge(Temp);
        Temp.Destroy;
      End;
    End;
    Folders.Destroy();
  End;
End;

Function SearchFolders(Path:AnsiString):List;
Begin
  Result := List.Create();
  IsFolderMode := True;
  CurrentPath := Path;
  CurrentLevel := 0;
  CurrentFileDir := Result;
  CallJavaListing('*');
End;


{$ENDIF}

{$IFNDEF HAS_IMPLEMENTATION}
Function Search(Path:AnsiString; Filter:AnsiString; SearchSubDirectories:Boolean; Level:Integer):List;
Var
  Sr:TSearchRec;
  P:FileInfo;
  Temp:List;
Begin
  Result := List.Create();

// WriteLn('Searching: ',Path+PathSeparator+Filter);
  If FindFirst(Path+PathSeparator+Filter, faAnyFile, Sr)=0 Then
  Begin
    Repeat
      If (Sr.Name<>'.')  And  (Sr.Name<>'..') Then
      Begin   
        If (Sr.Attr And faDirectory=0) Then
        Begin
        	P := FileInfo.Create();
        	P.Name := Sr.Name;
        	P.Size := Sr.Size;
        	P.Path := Path;
        	P.Level := Level;
        	Result.Add(P);
        End;
      End;
    Until FindNext(Sr)<>0;
    FindClose(sr);
  End;

  If Not SearchSubDirectories Then
    Exit;

  If FindFirst(Path+PathSeparator+'*', faDirectory,Sr)=0 Then
  Begin
    Repeat
      If (Sr.Name='.')Or(Sr.Name='..') Then
        Continue;

        If (Sr.Attr And faDirectory<>0) Then
        Begin
//          WriteLn('Found dir :', Sr.Name);
            If (SearchSubDirectories) Then
            Begin
               Temp := Search(Path+PathSeparator+Sr.Name,Filter,SearchSubDirectories, Succ(Level));
               If (Temp<>Nil) Then
               Begin
                  Result.Merge(Temp);
                  //Temp.Destroy;
                End;
            End;

        End;
    Until FindNext(Sr)<>0;
    FindClose(sr);
  End;
End;

Function SearchFolders(Path:AnsiString):List;
Var
  Sr:TSearchRec;
  FileAttrs:Integer;
Begin
  Result := List.Create();

  FileAttrs := faDirectory;
  If FindFirst(Path+PathSeparator+'*',FileAttrs,Sr)=0 Then
  Begin
    Repeat
      If (Sr.Name='.')Or(Sr.Name='..') Then
        Continue;

      If (Sr.Attr And faDirectory<>0) Then
      Begin
        Result.Add(StringObject.Create(Sr.Name));
      End;
    Until FindNext(Sr)<>0;
    FindClose(sr);
  End;
End;
{$ENDIF}

Function SearchFiles(Path:AnsiString; Filter:AnsiString; SearchSubDirectories:Boolean):List;
Begin
  If (Path<>'') Then
    Path := GetOSIndependentFilePath(Path)
  Else
    Path:='.';

  Result := Search(Path, Filter, SearchSubDirectories, 0);
End;

End.
