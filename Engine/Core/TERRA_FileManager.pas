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
 * TERRA_FileManager
 * Implements the global file manager
 ***********************************************************************************************************************
}

Unit TERRA_FileManager;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
    TERRA_Resource, TERRA_Collections, TERRA_IO, TERRA_FileIO, TERRA_Application, TERRA_Package;

Type
  ResourceProvider = Class
    Function GetStream(Name:AnsiString): Stream; Virtual; Abstract;
    Function HasStream(Name:AnsiString):Boolean; Virtual; Abstract;
  End;

  FileLocation = Class(ListObject)
    Public
      Name:AnsiString;
      Path:AnsiString;

      Function ToString():AnsiString; Override;

      Constructor Create(Name, Path:AnsiString);
      Procedure CopyValue(Other:ListObject); Override;
      Function GetHashKey():HashKey; Override;
  End;

  FileManager = Class(ApplicationComponent)
    Protected
      _SourceList:Array Of AnsiString;
      _SourceCount:Integer;

      _PathList:Array Of AnsiString;
      _PathCount:Integer;

      _PackageList:Array Of Package;
      _PackageCount:Integer;

      _ProviderCount:Integer;
      _Providers:Array Of ResourceProvider;

      _Locations:HashTable;

    Public
      Procedure Init; Override;

      Class Function Instance:FileManager;
      Destructor Destroy; Override;

      Function SearchResourceFile(FileName:AnsiString):AnsiString;

      Procedure AddPath(Path:AnsiString);
      Procedure RemovePath(Path:AnsiString);
      Function GetPath(Index:Integer):AnsiString;
      Function AddPackage(FileName:AnsiString; Password:AnsiString='*'):Package; Overload;
      Function AddPackage(MyPackage:Package):Package; Overload;

      Procedure AddSource(Source:AnsiString);
      Procedure RemoveSource(Source:AnsiString);

      Procedure AddResourceProvider(Provider:ResourceProvider);
      Procedure DeleteResourceProvider(Provider:ResourceProvider);

      Procedure RemoveFromCache(FileName:AnsiString);

      Procedure ExcludeFileFromBackup(Source:FileStream);

      Function GetPackage(Name:AnsiString; ValidateError:Boolean = True):Package;

      Function OpenFileStream(FileName:AnsiString; Mode:Integer = smRead):Stream;

      Function OpenPackages(FileName:AnsiString):Boolean;
      Property PathCount:Integer Read _PathCount;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_OS, TERRA_Image, TERRA_GraphicsManager, TERRA_Utils, TERRA_Color,
  TERRA_FileUtils;

Var
  _FileManager:ApplicationObject = Nil;

{$IFDEF IPHONE}
Procedure ExcludeFileFromCloud(fileName:PAnsiChar);Cdecl; external;
{$ENDIF}

Function SearchFileLocation(P:ListObject; UserData:Pointer):Boolean; CDecl;
Begin
  Result := (FileLocation(P).Name = PString(Userdata)^);
End;

{ FileManager }
Class Function FileManager.Instance:FileManager;
Begin
  If (_FileManager = Nil) Then
    _FileManager := InitializeApplicationComponent(FileManager, Nil);
  Result := FileManager(_FileManager.Instance);
End;

Procedure FileManager.Init;
Begin
  _Locations := HashTable.Create(256);
  Self.AddSource('');
End;

Procedure FileManager.RemoveFromCache(FileName:AnsiString);
Var
  Location:FileLocation;
Begin
{$IFNDEF DISABLEFILECACHE}
  FileName := LowStr(FileName);
  FileName := GetFileName(FileName, False);

  Location := FileLocation(_Locations.Search(SearchFileLocation, @FileName));
  If Assigned(Location) Then
    _Locations.Delete(Location);
{$ENDIF}
End;

Procedure FileManager.ExcludeFileFromBackup(Source: FileStream);
Begin
  {$IFDEF IPHONE}
  If (Source.Name<>'') Then
    ExcludeFileFromCloud(PAnsiChar(Source.Name));
  {$ENDIF}
End;

Function FileManager.OpenPackages(FileName:AnsiString):Boolean;
Type
  FileEntry = Record
    Name:AnsiString;
    Offset:Cardinal;
    Size:Cardinal;
  End;
Var
  Source:FileStream;
  Header:FileHeader;
  I, Offset, Count:Cardinal;
  Files:Array Of FileEntry;
  P:Package;
  Stream:MemoryStream;
Begin
  Result := False;

  If Not FileStream.Exists(FileName) Then
    Exit;

  Source := FileStream.Open(FileName, smRead);
  Source.Seek(Source.Size - 8);
  Source.Read(@Header,4);
  If (Header <> TERRAHeader) Then
  Begin
    Source.Destroy;
    Exit;
  End;

  Source.Read(@Offset, 4);
  Source.Seek(Offset);

  Source.Read(@Count, 4);
  SetLength(Files, Count);
  For I:=0 To Pred(Count) Do
  Begin
    Source.ReadString(Files[I].Name);
    Source.Read(@Files[I].Offset, 4);
    Source.Read(@Files[I].Size, 4);
  End;

  For I:=0 To Pred(Count) Do
  Begin
    Stream := MemoryStream.Create(Files[I].Size);
    Source.Seek(Files[I].Offset);
    Source.Read(Stream.Buffer, Files[I].Size);
    P := Package.Open(Stream);
    Self.AddPackage(P);
  End;

  Source.Destroy;

  Result := True;
End;

Procedure FileManager.AddResourceProvider(Provider: ResourceProvider);
Begin
  Inc(_ProviderCount);
  SetLength(_Providers, _ProviderCount);
  _Providers[Pred(_ProviderCount)] := Provider;
End;

Procedure FileManager.DeleteResourceProvider(Provider:ResourceProvider);
Var
  I,N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_ProviderCount) Do
  If (_Providers[I] = Provider) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  _Providers[N] := _Providers[Pred(_ProviderCount)];
  Dec(_ProviderCount);
End;

Function FileManager.GetPath(Index: Integer):AnsiString;
Begin
  If (Index>=0) And (Index<_PathCount) Then
    Result := _PathList[Index]
  Else
    Result := '';
End;

Procedure FileManager.AddPath(Path:AnsiString);
Var
  I:Integer;
  FM:FolderManager;
Begin
  Path := GetOSIndependentFilePath(Path);
  Path := Path + PathSeparator;

  For I:=0 To Pred(_PathCount) Do
  If (_PathList[I] = Path) Then
    Exit;

  Log(logDebug, 'FileManager', 'Adding path: '+Path);
  Inc(_PathCount);
  SetLength(_PathList, _PathCount);
  _PathList[Pred(_PathCount)] := Path;

  FM := FolderManager.Instance;
  If Assigned(FM) Then
    FM.WatchFolder(Path);
End;

Procedure FileManager.RemovePath(Path:AnsiString);
Var
  I, N:Integer;
Begin
  Path := GetOSIndependentFilePath(Path);
  Path := Path + PathSeparator;

  N := -1;
  For I:=0 To Pred(_PathCount) Do
  If (_PathList[I] = Path) Then
  Begin
    N := I;
    Break;
  End;

  If (N>=0) Then
  Begin
    _PathList[N] := _PathList[Pred(_PathCount)];
    Dec(_PathCount);
  End;
End;

Function FileManager.AddPackage(MyPackage:Package):Package;
Begin
  Inc(_PackageCount);
  SetLength(_PackageList, _PackageCount);
  _PackageList[Pred(_PackageCount)] := MyPackage;
  Result := _PackageList[Pred(_PackageCount)];
  Result.Update;
End;

Function FileManager.AddPackage(FileName, Password:AnsiString):Package;
Begin
  If (Pos('.',FileName)<=0) Then
    FileName := FileName + '.terra';

  Inc(_PackageCount);
  SetLength(_PackageList, _PackageCount);
  _PackageList[Pred(_PackageCount)] := Package.Open(FileName);
  Result := _PackageList[Pred(_PackageCount)];
  Result.Password := Password;
  Result.Update;
End;

Function FileManager.SearchResourceFile(FileName:AnsiString):AnsiString;
Var
  S:AnsiString;
  I, J:Integer;
  Resource:PResourceInfo;
  Location:FileLocation;
  Procedure RegisterLocation();
  Begin
    Location := FileLocation.Create(FileName, Result);
    _Locations.Add(Location);
  End;
Begin
  Result := '';

  If (FileName='') Then
    Exit;

  FileName := LowStr(FileName);
  FileName := GetFileName(FileName, False);

  {$IFDEF DEBUG_FILECACHE}Log(logDebug, 'FileManager', 'Searching for file '+FileName+' in cache');{$ENDIF}

{$IFNDEF DISABLEFILECACHE}
  Location := FileLocation(_Locations.Search(SearchFileLocation, @FileName));
  If Assigned(Location) Then
  Begin
    {$IFDEF DEBUG_FILECACHE}Log(logDebug, 'FileManager', 'Was found in cache: '+Location.Path);{$ENDIF}
    Result := Location.Path;
    Exit;
  End;
{$ENDIF}

  {$IFDEF DEBUG_FILECACHE}Log(logDebug, 'FileManager', 'Searching for file '+FileName+' in storage');{$ENDIF}

  For I:=0 To Pred(_ProviderCount) Do
  Begin
    If (_Providers[I].HasStream(FileName)) Then
    Begin
      Result := FileName;
      RegisterLocation();
      Exit;
    End;
  End;

  If Assigned(Application.Instance()) And (Application.Instance().TempPath<>'') Then
  Begin
    S := Application.Instance().TempPath + PathSeparator + FileName;
    If FileStream.Exists(S) Then
    Begin
      Result := S;
      RegisterLocation();
      Exit;
    End;
  End;

  If FileStream.Exists(FileName) Then
  Begin
    Result := FileName;
    RegisterLocation();
    Exit;
  End;

  FileName := GetFileName(FileName, False);

  For J:=0 To Pred(_SourceCount) Do
  Begin
    If _SourceList[J]<>'' Then
      Log(logDebug,'FileManager', 'Testing source: '+_SourceList[J]);

    For I:=0 To Pred(_PathCount) Do
    Begin
      S := _SourceList[J]+_PathList[I] + FileName;
      If FileStream.Exists(S) Then
      Begin
        Result := S;
        RegisterLocation();
        Exit;
      End;
    End;
  End;
  
  For I:=0 To Pred(_PackageCount) Do
  If Assigned(_PackageList[I]) Then
  Begin
    Resource := _PackageList[I].FindResource(FileName);
    If Assigned(Resource) Then
    Begin
      Result := Resource.GetLocation;
      RegisterLocation();
      Exit;
    End;
  End;

  Result := '';
  RegisterLocation();
End;

Function FileManager.GetPackage(Name:AnsiString; ValidateError:Boolean):Package;
Var
  I:Integer;
Begin
  Name := UpStr(GetFileName(TrimLeft(TrimRight(Name)), True));
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  For I:=0 To Pred(_PackageCount) Do
  If (UpStr(_PackageList[I].Name) = Name) Then
  Begin
    Result := _PackageList[I];
    Exit;
  End;

  Result := Nil;
  If ValidateError Then
    RaiseError('Could not find package. ['+Name +']');
End;

Function FileManager.OpenFileStream(FileName:AnsiString; Mode:Integer):Stream;
Var
  I:Integer;
  PackageName,ResourceName:AnsiString;
  MyPackage:Package;
  Resource:PResourceInfo;
Begin
  Result:=Nil;

  Log(logDebug, 'FileManager', 'Searching providers for '+FileName);
  For I:=0 To Pred(_ProviderCount) Do
  Begin
    If (_Providers[I].HasStream(FileName)) Then
    Begin
      Result := _Providers[I].GetStream(FileName);
      Exit;
    End;
  End;

  I := Pos('.TERRA'+PathSeparator, UpStr(FileName));
  // load from package
  If (I>0) Then
  Begin
    PackageName := Copy(FileName,1, Pred(I+Length('.TERRA')));
    ResourceName := Copy(FileName, I+Length('.TERRA'+PathSeparator),MaxInt);

    MyPackage := Self.GetPackage(PackageName);

    Resource := MyPackage.FindResource(ResourceName);

    If Not Assigned(Resource) Then
    Begin
      RaiseError('Resource '+ResourceName+' not found in '+PackageName);
      Exit;
    End;

    Result := MyPackage.LoadResource(Resource, True);
  End Else
  Begin
    If (Application.Instance = Nil) And (FileStream.Exists(FileName)) Then
    Begin
      Result := FileStream.Open(FileName, Mode);
      Exit;
    End;

    FileName := GetFileName(FileName, False);
    FileName := Self.SearchResourceFile(FileName);
    If (FileName = '') Then
      Result := Nil
    Else
    Begin
      Log(logDebug, 'FileManager', 'Opening file '+FileName);

      I := Pos('.TERRA'+PathSeparator, UpStr(FileName));
      If (I>0) Then
        Result := Self.OpenFileStream(FileName, Mode)
      Else
        Result := MemoryStream.Create(FileName, Mode);
        //Result := FileStream.Open(FileName,Mode);

     (* If (Result.Size<=0) Then
      Begin
        {$IFDEF PC}
        Result := OpenFileStream(FileName, Mode);
        Halt;
        {$ENDIF}
      End;*)

    Log(logDebug, 'FileManager', 'Open size '+IntToString(Result.Size));
    End;
  End;
End;

Destructor FileManager.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._ProviderCount) Do
    _Providers[I].Destroy;

  For I:=0 To Pred(_PackageCount) Do
    _PackageList[I].Destroy;
  _PackageCount := 0;

  _Locations.Destroy();
  _Locations := Nil;

  _FileManager := Nil;
End;

Procedure FileManager.AddSource(Source:AnsiString);
Var
  I:Integer;
Begin
  Log(logDebug, 'FileManager', 'Adding source: '+Source);

  RemoveSource(Source);
  Inc(_SourceCount);
  SetLength(_SourceList, _SourceCount);
  _SourceList[Pred(_SourceCount)] := Source;
End;

Procedure FileManager.RemoveSource(Source:AnsiString);
Var
  I:Integer;
Begin
  I := 0;
  While I<_SourceCount Do
  If (_SourceList[I] = Source) Then
  Begin
    _SourceList[I] := _SourceList[Pred(_SourceCount)];
    Dec(_SourceCount);
  End Else
    Inc(I);
End;

{ FileLocation }
Constructor FileLocation.Create(Name, Path:AnsiString);
Begin
  Self.Name := LowStr(GetFileName(Name, False));
  Self.Path := Path;
End;

Procedure FileLocation.CopyValue(Other: ListObject);
Begin
  Self.Name := FileLocation(Other).Name;
  Self.Path := FileLocation(Other).Path;
End;

Function FileLocation.ToString:AnsiString;
Begin
  Result := Self.Name;
End;

Function FileLocation.GetHashKey: HashKey;
Begin
  Result := GetStringHashKey(Name+Path);
End;


End.
