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
    TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections, TERRA_Stream, TERRA_FileStream,
    TERRA_Application, TERRA_Hashmap, TERRA_Package, TERRA_FileFormat;

Type
  ResourceProvider = Class(TERRAObject)
    Function GetStream(Const Name:TERRAString):TERRAStream; Virtual; Abstract;
    Function HasStream(Const Name:TERRAString):Boolean; Virtual; Abstract;
  End;

  FileManager = Class(TERRAObject)
    Protected
      _SourceList:Array Of TERRAString;
      _SourceCount:Integer;

      _FolderList:Array Of TERRAString;
      _FolderCount:Integer;

      _PackageList:Array Of TERRAPackage;
      _PackageCount:Integer;

      _ProviderCount:Integer;
      _Providers:Array Of ResourceProvider;

      _Locations:TERRAHashMap;

      Function FindLocation(FileName:TERRAString):TERRALocation;
      Function RegisterLocation(Location:TERRALocation):TERRALocation;

    Public
      Constructor Create();
      Procedure Release; Override;

      Procedure Reset();

      Function Search(FileName:TERRAString):TERRALocation;

      Procedure AddLocation(Location:TERRALocation);

      Procedure AddFolder(Path:TERRAString);
      Procedure RemoveFolder(Path:TERRAString);
      Function GetFolder(Index:Integer):TERRAString;

      Function AddPackage(FileName:TERRAString):TERRAPackage; Overload;
      Function AddPackage(MyPackage:TERRAPackage):TERRAPackage; Overload;

      Procedure AddSource(Source:TERRAString);
      Procedure RemoveSource(Source:TERRAString);

      Procedure AddResourceProvider(Provider:ResourceProvider);
      Procedure DeleteResourceProvider(Provider:ResourceProvider);

      Procedure RemoveFromCache(FileName:TERRAString);

      Procedure ExcludeFileFromBackup(Source:FileStream);

      Function GetPackage(Name:TERRAString; ValidateError:Boolean = True):TERRAPackage;

      Function OpenLocation(Location:TERRALocation):TERRAStream;
      Function OpenFile(Const FileName:TERRAString):TERRAStream;

      Function LoadFromStream(Source:TERRAStream; ObjectType:TERRAObjectType):TERRAObject;
      Function LoadFromLocation(Location:TERRALocation; ObjectType:TERRAObjectType):TERRAObject;
      Function LoadFromString(Const Data:TERRAString; ObjectType:TERRAObjectType):TERRAObject;
      Function LoadFromFile(Const FileName:TERRAString; ObjectType:TERRAObjectType):TERRAObject;

      Function SaveToStream(Obj:TERRAObject; Dest:TERRAStream; Format:TERRAFileFormat):Boolean;
      Function SaveToFile(Obj:TERRAObject; Const FileName:TERRAString; Format:TERRAFileFormat):Boolean;
      //Function SaveToString(Obj:TERRAObject; Encoding:StringEncoding):TERRAString;

     // Function OpenPackages(FileName:TERRAString):Boolean;
      Property PathCount:Integer Read _FolderCount;

      Property Files[Const Name:TERRAString]:TERRAStream Read OpenFile; Default;
  End;

  PathProperty = Class(TERRAObject)
    Protected
      _Value:TERRAString;
    Public
      Constructor Create(Const Name, InitValue:TERRAString);
      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;
  End;

Function IsPackageFileName(Const FileName:TERRAString):Boolean;

Implementation
Uses SysUtils, TERRA_Error, TERRA_Log, {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF} TERRA_OS, TERRA_Image,
  TERRA_Engine, TERRA_GraphicsManager, TERRA_Color, TERRA_FileUtils, TERRA_MemoryStream;

Type
  TERRAPackageLocation = Class(TERRALocation)
    Protected
      _Package:TERRAPackage;

      Function GetStream():TERRAStream; Override;

    Public
      Constructor Create(Const Name, Path:TERRAString; Package:TERRAPackage);
  End;

  TERRAFileLocation = Class(TERRALocation)
    Protected
      Function GetStream():TERRAStream; Override;

    Public
      Constructor Create(Const Name, Path:TERRAString);
  End;

  TERRANullLocation = Class(TERRALocation)
    Protected
      Function GetStream():TERRAStream; Override;

    Public
      Constructor Create(Const Name:TERRAString);
  End;


Function IsPackageFileName(Const FileName:TERRAString):Boolean;
Var
  I,J:Integer;
Begin
  Result := StringContains('.terra'+PathSeparator, FileName);
End;

{ FileManager }
Constructor FileManager.Create();
Begin
  _Locations := TERRAHashMap.Create(256);
  Self.AddSource('');
End;

Procedure FileManager.RemoveFromCache(FileName:TERRAString);
Var
  Location:TERRALocation;
Begin
  FileName := StringLower(FileName);
  FileName := GetFileName(FileName, False);

  Location := TERRALocation(_Locations.GetItemByKey(FileName));
  If Assigned(Location) Then
    _Locations.Delete(Location);
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

Function FileManager.GetFolder(Index: Integer):TERRAString;
Begin
  If (Index>=0) And (Index<_FolderCount) Then
    Result := _FolderList[Index]
  Else
    Result := '';
End;

Procedure FileManager.AddFolder(Path:TERRAString);
Var
  I:Integer;
//  FM:FolderManager;
Begin
  If Path = '' Then
    Exit;

  Path := GetOSIndependentFilePath(Path);
  Path := Path + PathSeparator;

  {$IFDEF PC}
  If Not DirectoryExists(Path) Then
  Begin
    Engine.Log.Write(logWarning, 'FileManager', 'The following path is missing: '+Path);
    Exit;
  End;
  {$ENDIF}

  For I:=0 To Pred(_FolderCount) Do
  If (_FolderList[I] = Path) Then
    Exit;

  Engine.Log.Write(logDebug, 'FileManager', 'Adding path: '+Path);
  Inc(_FolderCount);
  SetLength(_FolderList, _FolderCount);
  _FolderList[Pred(_FolderCount)] := Path;

(*  FM := FolderManager.Instance;
  If Assigned(FM) Then
    FM.WatchFolder(Path);*)
End;

Procedure FileManager.RemoveFolder(Path:TERRAString);
Var
  I, N:Integer;
Begin
  Path := GetOSIndependentFilePath(Path);
  Path := Path + PathSeparator;

  N := -1;
  For I:=0 To Pred(_FolderCount) Do
  If (_FolderList[I] = Path) Then
  Begin
    N := I;
    Break;
  End;

  If (N>=0) Then
  Begin
    _FolderList[N] := _FolderList[Pred(_FolderCount)];
    Dec(_FolderCount);
  End;
End;

Function FileManager.AddPackage(MyPackage:TERRAPackage):TERRAPackage;
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

Function FileManager.AddPackage(FileName:TERRAString):TERRAPackage;
Begin
  If (Pos('.',FileName)<=0) Then
    FileName := FileName + '.terra';

  If FileStream.Exists(FileName) Then
    Result := Self.AddPackage(TERRAPackage.Create(FileName))
  Else
    Result := Nil;
End;

Function FileManager.RegisterLocation(Location:TERRALocation):TERRALocation;
Begin
  Result := Location;
  _Locations.Add(Result);
End;

Function FileManager.FindLocation(FileName:TERRAString):TERRALocation;
Begin
  Result := Nil;

  If (FileName='') Then
    Exit;

  FileName := StringLower(FileName);
  FileName := GetFileName(FileName, False);

  Result := TERRALocation(_Locations.GetItemByKey(FileName));
End;

Function FileManager.Search(FileName:TERRAString):TERRALocation;
Var
  S:TERRAString;
  I, J:Integer;
  Resource:ResourceInfo;
Begin
  {$IFDEF DEBUG_FILECACHE}Engine.Log.Write(logDebug, 'FileManager', 'Searching for file '+FileName+' in cache');{$ENDIF}
  Result := Self.FindLocation(FileName);

  If Assigned(Result) Then
  Begin
    {$IFDEF DEBUG_FILECACHE}Engine.Log.Write(logDebug, 'FileManager', 'Was found in cache: '+Location.Path);{$ENDIF}

    If Result.Path = '' Then
      Result := Nil;

    Exit;
  End;

  {$IFDEF DEBUG_FILECACHE}Engine.Log.Write(logDebug, 'FileManager', 'Searching for file '+FileName+' in storage');{$ENDIF}

  (*For I:=0 To Pred(_ProviderCount) Do
  Begin
    If (_Providers[I].HasStream(FileName)) Then
    Begin
      Result := RegisterLocation(FileName, FileName);
      Exit;
    End;
  End;*)

  If FileStream.Exists(FileName) Then
  Begin
    Result := RegisterLocation(TERRAFileLocation.Create(FileName, FileName));
    Exit;
  End;

  FileName := GetFileName(FileName, False);

  For J:=0 To Pred(_SourceCount) Do
  Begin
    If _SourceList[J]<>'' Then
      Engine.Log.Write(logDebug,'FileManager', 'Testing source: '+_SourceList[J]);

    For I:=0 To Pred(_FolderCount) Do
    Begin
      S := _SourceList[J]+_FolderList[I] + FileName;
      If FileStream.Exists(S) Then
      Begin
        Result := RegisterLocation(TERRAFileLocation.Create(FileName, S));
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
      Result := RegisterLocation(TERRAPackageLocation.Create(FileName, Resource.GetLocation, _PackageList[I]));
      Exit;
    End;
  End;

  If Assigned(Application.Instance()) And (Application.Instance().TempPath<>'') Then
  Begin
    S := Application.Instance().TempPath + PathSeparator + FileName;
    If FileStream.Exists(S) Then
    Begin
      Result := RegisterLocation(TERRAFileLocation.Create(FileName, S));
      Exit;
    End;
  End;


  RegisterLocation(TERRANullLocation.Create(FileName));
  Result := Nil;
End;

Function FileManager.GetPackage(Name:TERRAString; ValidateError:Boolean):TERRAPackage;
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
    Engine.RaiseError('Could not find package. ['+Name +']');
End;

Function FileManager.OpenFile(Const FileName:TERRAString):TERRAStream;
Var
  Location:TERRALocation;
Begin
  Location := Self.Search(FileName);
  Result := Self.OpenLocation(Location);
End;

Function FileManager.OpenLocation(Location:TERRALocation):TERRAStream;
Var
  I:Integer;
  PackageName,ResourceName:TERRAString;
  MyPackage:TERRAPackage;
  Resource:ResourceInfo;
Begin
  Result := Nil;

  If Location = Nil Then
    Exit;

  Result := Location.GetStream();
End;

{
  Mode := smRead;

  Engine.Log.Write(logDebug, 'FileManager', 'Searching providers for '+FileName);
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

    Location := Self.Search(GetFileName(FileName, False));
    If (Location = Nil) Then
      Result := Nil
    Else
    Begin
      Engine.Log.Write(logDebug, 'FileManager', 'Opening file '+FileName);

      If (IsPackageFileName(FileName)) Then
        Result := Self.OpenStream(FileName)
      Else
        Result := MemoryStream.Create(FileName, Mode);
        //Result := FileStream.Open(FileName,Mode);

    Engine.Log.Write(logDebug, 'FileManager', 'Open size '+IntegerProperty.Stringify(Result.Size));
    End;
  End;
End;}

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
End;

Procedure FileManager.AddSource(Source:TERRAString);
Var
  I:Integer;
Begin
  Engine.Log.Write(logDebug, 'FileManager', 'Adding source: '+Source);

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

Procedure FileManager.Reset;
Begin
  Self._FolderCount := 1;
End;

Procedure FileManager.AddLocation(Location: TERRALocation);
Var
  Previous:TERRALocation;
Begin
  If Location = Nil Then
    Exit;

  Previous := Self.FindLocation(Location.Name);
  If Assigned(Previous) Then
    Self._Locations.Delete(Previous);

  RegisterLocation(Location);
End;

Function FileManager.LoadFromStream(Source:TERRAStream; ObjectType:TERRAObjectType):TERRAObject;
Var
  Format:TERRAFileFormat;
Begin
  Format := Engine.Formats.FindFormatFromStream(Source, ObjectType);
  If Assigned(Format) Then
  Begin
    Result := ObjectType.Create();
    Format.LoadFromStream(Result, Source);
  End Else
    Result := Nil;
End;

Function FileManager.LoadFromLocation(Location:TERRALocation; ObjectType:TERRAObjectType):TERRAObject;
Var
  Src:TERRAStream;
Begin
  If Assigned(Location) Then
  Begin
    Src := Location.GetStream();
    Result := LoadFromStream(Src, ObjectType);
    ReleaseObject(Src);
  End Else
    Result := Nil;
End;

Function FileManager.LoadFromFile(Const FileName:TERRAString; ObjectType:TERRAObjectType):TERRAObject;
Var
  Src:FileStream;
Begin
  Src := FileStream.Open(FileName);
  Result := LoadFromStream(Src, ObjectType);
  ReleaseObject(Src);
End;

Function FileManager.LoadFromString(Const Data:TERRAString; ObjectType:TERRAObjectType):TERRAObject;
Var
  Source:MemoryStream;
Begin
  Source := MemoryStream.Create(Length(Data), @Data[1]);
  Result := LoadFromStream(Source, ObjectType);
  ReleaseObject(Source);
End;

Function FileManager.SaveToStream(Obj:TERRAObject; Dest:TERRAStream; Format:TERRAFileFormat):Boolean;
Begin
  If Assigned(Format) Then
  Begin
    Result := Format.SaveToStream(Obj, Dest);
  End Else
    Result := False;
End;

Function FileManager.SaveToFile(Obj:TERRAObject; Const FileName:TERRAString; Format:TERRAFileFormat):Boolean;
Var
  Dest:FileStream;
Begin
  Dest := FileStream.Create(FileName);
  Result := SaveToStream(Obj, Dest, Format);
  ReleaseObject(Dest);
End;

(*Function FileManager.SaveToString(Encoding:StringEncoding):TERRAString;
Begin

End;*)

{ TERRAFileLocation }
Constructor TERRAFileLocation.Create(Const Name, Path:TERRAString);
Begin
  Self._ObjectName := StringLower(GetFileName(Name, False));
  Self._Path := Path;
End;

Function TERRAFileLocation.GetStream: TERRAStream;
Begin
  Result := MemoryStream.Create(_Path, smRead);
End;

{ TERRAPackageLocation }
Constructor TERRAPackageLocation.Create(const Name, Path: TERRAString; Package: TERRAPackage);
Begin
  Self._ObjectName := StringLower(GetFileName(Name, False));
  Self._Path := Path;
  Self._Package := Package;
End;

Function TERRAPackageLocation.GetStream: TERRAStream;
Var
  I:Integer;
  ResourceName:TERRAString;
  Resource:ResourceInfo;
Begin
  I := StringPosReverse(PathSeparator, _Path);
  ResourceName := StringCopy(_Path, Succ(I), MaxInt);

  Resource := _Package.FindResourceByName(ResourceName);

  If Not Assigned(Resource) Then
  Begin
    Result := Nil;
    //RaiseError('Resource '+ResourceName+' not found in '+PackageName);
    Exit;
  End;

  Result := _Package.LoadResource(Resource);
End;

{ TERRANullLocation }

Constructor TERRANullLocation.Create(const Name: TERRAString);
Begin
  Self._ObjectName := Name;
End;

Function TERRANullLocation.GetStream: TERRAStream;
Begin
  Result := Nil;
End;

{ PathProperty }
Constructor PathProperty.Create(const Name, InitValue: TERRAString);
Begin
  Self._ObjectName := Name;
  If InitValue<>'' Then
    SetBlob(InitValue);
End;

Function PathProperty.GetBlob: TERRAString;
Begin
  Result := _Value;
End;

Procedure PathProperty.SetBlob(const Blob: TERRAString);
Begin
  Self._Value := Blob;

  Engine.Files.AddFolder(Blob);
End;

End.
