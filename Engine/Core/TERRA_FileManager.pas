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
    TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Resource, TERRA_Collections, TERRA_Stream, TERRA_FileStream,
    TERRA_Application, TERRA_Hashmap, TERRA_Package;

Type
  ResourceProvider = Class(TERRAObject)
    Function GetStream(Const Name:TERRAString):Stream; Virtual; Abstract;
    Function HasStream(Const Name:TERRAString):Boolean; Virtual; Abstract;
  End;

  FileLocation = Class(CollectionObject)
    Public
      Path:TERRAString;

      Constructor Create(Const Name, Path:TERRAString);
      Procedure CopyValue(Other:CollectionObject); Override;
  End;

  FileManager = Class(ApplicationComponent)
    Protected
      _SourceList:Array Of TERRAString;
      _SourceCount:Integer;

      _PathList:Array Of TERRAString;
      _PathCount:Integer;

      _PackageList:Array Of Package;
      _PackageCount:Integer;

      _ProviderCount:Integer;
      _Providers:Array Of ResourceProvider;

      _Locations:HashMap;

    Public
      Procedure Init; Override;

      Class Function Instance:FileManager;
      Procedure Release; Override;

      Function SearchResourceFile(FileName:TERRAString):TERRAString;

      Procedure AddPath(Path:TERRAString);
      Procedure RemovePath(Path:TERRAString);
      Function GetPath(Index:Integer):TERRAString;
      Function AddPackage(FileName:TERRAString):Package; Overload;
      Function AddPackage(MyPackage:Package):Package; Overload;

      Procedure AddSource(Source:TERRAString);
      Procedure RemoveSource(Source:TERRAString);

      Procedure AddResourceProvider(Provider:ResourceProvider);
      Procedure DeleteResourceProvider(Provider:ResourceProvider);

      Procedure RemoveFromCache(FileName:TERRAString);

      Procedure ExcludeFileFromBackup(Source:FileStream);

      Function GetPackage(Name:TERRAString; ValidateError:Boolean = True):Package;

      Function OpenStream(FileName:TERRAString; Mode:Integer = smRead):Stream;

     // Function OpenPackages(FileName:TERRAString):Boolean;
      Property PathCount:Integer Read _PathCount;
  End;

Function IsPackageFileName(Const FileName:TERRAString):Boolean;

Implementation
Uses SysUtils, TERRA_Error, TERRA_Log, {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF} TERRA_OS, TERRA_Image,
  TERRA_GraphicsManager, TERRA_Color, TERRA_FileUtils, TERRA_MemoryStream;

Var
  _FileManager:ApplicationObject = Nil;

Function IsPackageFileName(Const FileName:TERRAString):Boolean;
Var
  I,J:Integer;
Begin
  Result := StringContains('.terra'+PathSeparator, FileName);
End;

Function SearchFileLocation(P:CollectionObject; UserData:Pointer):Boolean; CDecl;
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
  _Locations := HashMap.Create(256);
  Self.AddSource('');
End;

Procedure FileManager.RemoveFromCache(FileName:TERRAString);
Var
  Location:FileLocation;
Begin
{$IFNDEF DISABLEFILECACHE}
  FileName := StringLower(FileName);
  FileName := GetFileName(FileName, False);

  Location := FileLocation(_Locations.Search(SearchFileLocation, @FileName));
  If Assigned(Location) Then
    _Locations.Delete(Location);
{$ENDIF}
End;

Procedure FileManager.ExcludeFileFromBackup(Source: FileStream);
Begin
  {$IFDEF IPHONE}
(*  If (Source.Name<>'') Then
    ExcludeFileFromCloud(PAnsiChar(Source.Name));
 *) {$ENDIF}
End;
                                 (*
Function FileManager.OpenPackages(FileName:TERRAString):Boolean;
Type
  FileEntry = Record
    Name:TERRAString;
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
    ReleaseObject(Source);
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

  ReleaseObject(Source)

  Result := True;
End;
*)

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

Function FileManager.GetPath(Index: Integer):TERRAString;
Begin
  If (Index>=0) And (Index<_PathCount) Then
    Result := _PathList[Index]
  Else
    Result := '';
End;

Procedure FileManager.AddPath(Path:TERRAString);
Var
  I:Integer;
//  FM:FolderManager;
Begin
  Path := GetOSIndependentFilePath(Path);
  Path := Path + PathSeparator;

  {$IFDEF PC}
  If Not DirectoryExists(Path) Then
  Begin
    Log(logWarning, 'FileManager', 'The following path is missing: '+Path);
    Exit;
  End;
  {$ENDIF}

  For I:=0 To Pred(_PathCount) Do
  If (_PathList[I] = Path) Then
    Exit;

  Log(logDebug, 'FileManager', 'Adding path: '+Path);
  Inc(_PathCount);
  SetLength(_PathList, _PathCount);
  _PathList[Pred(_PathCount)] := Path;

(*  FM := FolderManager.Instance;
  If Assigned(FM) Then
    FM.WatchFolder(Path);*)
End;

Procedure FileManager.RemovePath(Path:TERRAString);
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
  Result := MyPackage;
                   
  If MyPackage = Nil Then
    Exit;

  Inc(_PackageCount);
  SetLength(_PackageList, _PackageCount);
  _PackageList[Pred(_PackageCount)] := MyPackage;
  Result := _PackageList[Pred(_PackageCount)];

  MyPackage.Load();
End;

Function FileManager.AddPackage(FileName:TERRAString):Package;
Begin
  If (Pos('.',FileName)<=0) Then
    FileName := FileName + '.terra';

  If FileStream.Exists(FileName) Then
    Result := Self.AddPackage(Package.Create(FileName))
  Else
    Result := Nil;
End;

Function FileManager.SearchResourceFile(FileName:TERRAString):TERRAString;
Var
  S:TERRAString;
  I, J:Integer;
  Resource:ResourceInfo;
  Location:FileLocation;
  Procedure RegisterLocation();
  Begin
    If IsPackageFileName(FileName) Then
      Exit;
      
    Location := FileLocation.Create(FileName, Result);
    _Locations.Add(Location);
  End;
Begin
  Result := '';

  If (FileName='') Then
    Exit;

  FileName := StringLower(FileName);
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
    Resource := _PackageList[I].FindResourceByName(FileName);
    If Assigned(Resource) Then
    Begin
      Result := Resource.GetLocation;
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


  Result := '';
  RegisterLocation();
End;

Function FileManager.GetPackage(Name:TERRAString; ValidateError:Boolean):Package;
Var
  I:Integer;
Begin
  Name := GetFileName(StringTrim(Name), True);
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  For I:=0 To Pred(_PackageCount) Do
  If (StringEquals(_PackageList[I].Name, Name)) Then
  Begin
    Result := _PackageList[I];
    Exit;
  End;

  Result := Nil;
  If ValidateError Then
    RaiseError('Could not find package. ['+Name +']');
End;

Function FileManager.OpenStream(FileName:TERRAString; Mode:Integer):Stream;
Var
  I:Integer;
  PackageName,ResourceName:TERRAString;
  MyPackage:Package;
  Resource:ResourceInfo;
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

  // load from package
  If (IsPackageFileName(FileName)) Then
  Begin
    I := StringPosReverse(PathSeparator, FileName);
    PackageName := StringCopy(FileName,1, Pred(I));
    ResourceName := StringCopy(FileName, Succ(I), MaxInt);

    MyPackage := Self.GetPackage(PackageName);

    Resource := MyPackage.FindResourceByName(ResourceName);

    If Not Assigned(Resource) Then
    Begin
      RaiseError('Resource '+ResourceName+' not found in '+PackageName);
      Exit;
    End;

    Result := MyPackage.LoadResource(Resource);
  End Else
  Begin
    If (FileStream.Exists(FileName)) Then
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

      If (IsPackageFileName(FileName)) Then
        Result := Self.OpenStream(FileName, Mode)
      Else
        Result := MemoryStream.Create(FileName, Mode);
        //Result := FileStream.Open(FileName,Mode);

     (* If (Result.Size<=0) Then
      Begin
        {$IFDEF PC}
        Result := OpenStream(FileName, Mode);
        Halt;
        {$ENDIF}
      End;*)

    Log(logDebug, 'FileManager', 'Open size '+IntToString(Result.Size));
    End;
  End;
End;

Procedure FileManager.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Self._ProviderCount) Do
    ReleaseObject(_Providers[I]);

  For I:=0 To Pred(_PackageCount) Do
    ReleaseObject(_PackageList[I]);
  _PackageCount := 0;

  ReleaseObject(_Locations);
  
  _FileManager := Nil;
End;

Procedure FileManager.AddSource(Source:TERRAString);
Var
  I:Integer;
Begin
  Log(logDebug, 'FileManager', 'Adding source: '+Source);

  RemoveSource(Source);
  Inc(_SourceCount);
  SetLength(_SourceList, _SourceCount);
  _SourceList[Pred(_SourceCount)] := Source;
End;

Procedure FileManager.RemoveSource(Source:TERRAString);
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
Constructor FileLocation.Create(Const Name, Path:TERRAString);
Begin
  Self.Name := StringLower(GetFileName(Name, False));
  Self.Path := Path;
End;

Procedure FileLocation.CopyValue(Other: CollectionObject);
Begin
  Self.Name := FileLocation(Other).Name;
  Self.Path := FileLocation(Other).Path;
End;

End.
