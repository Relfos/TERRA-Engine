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
 * TERRA_FileImport
 * Implements file import utilities
 ***********************************************************************************************************************
}
Unit TERRA_FileImport;

{-$DEFINE SHOWSKIPS}

{$I terra.inc}

{$IFDEF WINDOWS}
	{-$DEFINE USE_ASSIMP}
{$ENDIF}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_INI, TERRA_Log, TERRA_FileStream, TERRA_FileUtils, TERRA_OS, TERRA_Image,
  TERRA_Sound, TERRA_Localization, TERRA_XML, TERRA_XMLBinary, TERRA_MemoryStream;

Type
  FileImporter = Function(SourceFile, TargetDir:TERRAString; TargetPlatform:Integer; Settings:TERRAString):TERRAString;

Function ImportFile(SourceFile, TargetDir:TERRAString; TargetPlatform:Integer; Settings, ConstantFile:TERRAString):TERRAString;

Procedure RegisterFileImporter(Extension, Path:TERRAString; Importer:FileImporter);

Function AssetModified(SourceAsset, TargetAsset:TERRAString):Boolean;

Procedure ExportArray(SourceFile, DestFile, Contents:TERRAString);

Procedure MakeDir(Dir:TERRAString);

Implementation
Uses TERRA_Error, TERRA_Stream, TERRA_Mesh, TERRA_MeshFilter, SysUtils, DateUtils, Types,
  TERRA_LocalizationImport, TERRA_Application
  {$IFDEF USE_ASSIMP}, TERRA_Assimp{$ENDIF};

Type
  Rec = Record
    Importer:FileImporter;
    Path:TERRAString;
    Ext:TERRAString;
  End;

Var
  _Importers:Array Of Rec;
  _ImporterCount:Integer;

Procedure RegisterFileImporter(Extension, Path:TERRAString; Importer:FileImporter);
Begin
  Inc(_ImporterCount);
  SetLength(_Importers, _ImporterCount);
  _Importers[Pred(_ImporterCount)].Importer := Importer;
  _Importers[Pred(_ImporterCount)].Ext := StringUpper(Extension);
  _Importers[Pred(_ImporterCount)].Path := Path;
End;

Function AssetModified(SourceAsset, TargetAsset:TERRAString):Boolean;
Var
  A,B:TDateTime;
Begin
  If (Not FileStream.Exists(TargetAsset)) Then
  Begin
    Result := True;
    Exit;
  End;

  A := FileAge(SourceAsset);
  B := FileAge(TargetAsset);
  {$IFDEF FPC}
  Result := (CompareDateTime(A, B)>0);
  {$ELSE}
  Result := (CompareDateTime(A, B)=GreaterThanValue);
  {$ENDIF}
End;

Procedure MakeDir(Dir:TERRAString);
  function Last(What:TERRAString; Where:TERRAString): Integer;
  var
    Ind : Integer;

  begin
    Result := 0;

    for Ind := (Length(Where)-Length(What)+1) downto 1 do
        if Copy(Where, Ind, Length(What)) = What then begin
           Result := Ind;
           Break;
        end;
  end;

var
  PrevDir :TERRAString;
  Ind     : Integer;

begin
  if not DirectoryExists(Dir) then
  begin
     // if directory don't exist, get name of the previous directory

     Ind     := Last('\', Dir);         //  Position of the last '\'
     PrevDir := Copy(Dir, 1, Ind-1);    //  Previous directory

     // if previous directoy don't exist,
     // it's passed to this procedure - this is recursively...
     if (PrevDir<>'') And (not DirectoryExists(PrevDir)) then
        MakeDir(PrevDir);

     // In thats point, the previous directory must be exist.
     // So, the actual directory (in "Dir" variable) will be created.
     Log(logConsole, 'Import', 'Creating dir: '+Dir);
     CreateDir(Dir);
     If (Not DirectoryExists(Dir)) Then
     Begin
     	RaiseError('Could not create dir: '+Dir);
     End;
  end;
End;

Procedure ExportArray(SourceFile, DestFile, Contents:TERRAString);
Var
  N:Byte;
  S:TERRAString;
  Source, Dest:Stream;
Begin
  Source := MemoryStream.Create(SourceFile);
  Dest := FileStream.Create(DestFile);
  Dest.WriteLine('Unit '+GetFileName(DestFile,True)+';');
  Dest.WriteLine('Interface');
  Dest.WriteLine('  Const');
  Dest.WriteLine('    '+GetFileName(SourceFile,True)+'_Size =  '+IntToString(Source.Size)+';   // '+MemoryToString(Source.Size));
  Dest.WriteLine('    '+GetFileName(SourceFile,True)+'_Data: Array[0..'+IntToString(Pred(Source.Size))+'] Of Byte=(');

  S :='    ';
  While Not Source.EOF Do
  Begin
    Source.Read(@N,1);
    S := S + '$'+HexStr(N)+',';
    If Source.EOF Then
      S := Copy(S, 1, Pred(Length(S)));
    If (Length(S)>70) Then
    Begin
      Dest.WriteLine(S);
      S :='    ';
    End;
  End;

  Dest.WriteLine(S);
  Dest.WriteLine(');');
  Dest.WriteLine('Implementation');
  If Contents<>'' Then
  Begin
    Dest.WriteLine('  // contents:');
    Dest.WriteLine(Contents);
  End;
  Dest.WriteLine('End.');
  Dest.Release;

  Source.Release;
End;

Function ImportXML(SourceFile, DestFile, TargetDir, TargetFolder, TargetExt, ConstantFile:TERRAString):TERRAString;
Begin
  TargetDir := TargetDir + PathSeparator + TargetFolder;
  MakeDir(TargetDir);
  Result := TargetDir + PathSeparator + GetFileName(SourceFile, True)+'.'+TargetExt;

  If (Not AssetModified(SourceFile, Result)) Then
  Begin
    Log(logConsole, 'Import', 'Skipping '+SourceFile+'...[Not modified]');
    Result := DestFile;
    Exit;
  End;

  Log(logConsole, 'Import', 'Importing '+SourceFile+' into ' + Result);

  If (ConstantFile<>'') And (Not FileStream.Exists(ConstantFile)) Then
  Begin
    RaiseError('File not found: '+ConstantFile);
  End Else
  Begin
    XMLConvertToBinary(SourceFile, Result, ConstantFile);
  End;
End;


Function ImportFile(SourceFile, TargetDir:TERRAString; TargetPlatform:Integer; Settings, ConstantFile:TERRAString):TERRAString;
Var
  I:Integer;
  Ext, DestFile, OutDir:TERRAString;
  PreProcess:Boolean;
  Source, Dest:Stream;
  INI:INIParser;
  Filter:MeshFilter;
  MyMesh:Mesh;
  ImgInfo:ImageClassInfo;
Begin
  SourceFile := StringLower(SourceFile);

//WriteLn('Source: ', SourceFile);

  If (TargetDir<>'') And (Not DirectoryExists(TargetDir)) Then
  Begin
    Log(logConsole, 'Import', 'Target directory does not exist: ' + TargetDir);
    Result := '';
    Exit;
  End;

  Ext := StringUpper(GetFileExtension(SourceFile));
  PreProcess := True;

  OutDir := '';
  If (Settings<>'') Then
  Begin
    INI := INIParser.Create;
    INI.ParseCommas := True;
    INI.AddToken('path', tkString, @OutDir);
    INI.AddToken('preprocess', tkBoolean, @PreProcess);
    INI.LoadFromString(Settings);
    INI.Release;
  End;

  If OutDir<>'' Then
    TargetDir := TargetDir + PathSeparator + OutDir;

  If (StringUpper(GetFileExtension(SourceFile))='FX') Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'mesh';
    PreProcess := False;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='LINK') Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'textures';
    PreProcess := False;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='FNT') Or (StringUpper(GetFileExtension(SourceFile))='TTF') Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'system';
    PreProcess := False;
  End;

  If {(StringUpper(GetFileExtension(SourceFile))='TMX') Or} (StringUpper(GetFileExtension(SourceFile))='MAP') Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'maps';
    PreProcess := False;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='OGG') Or (StringUpper(GetFileExtension(SourceFile))='M4A')
  Or (StringUpper(GetFileExtension(SourceFile))='MID') Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'sound';
    PreProcess := False;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='DAT') Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'system';
    PreProcess := False;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='MESH') Or (StringUpper(GetFileExtension(SourceFile))='ANIM') Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'mesh';
    PreProcess := False;
  End;

  If (Not PreProcess) Then
  Begin
    DestFile := TargetDir + PathSeparator + GetFileName(SourceFile, False);
    If (Not AssetModified(SourceFile, DestFile)) Then
    Begin
      Log(logConsole, 'Import', 'Skipping '+SourceFile+'...[Not modified]');
      Result := DestFile;
      Exit;
    End;

    MakeDir(TargetDir);
    Log(logConsole, 'Import', 'Copying '+SourceFile+' to '+TargetDir);
    Source := MemoryStream.Create(SourceFile);
    Dest := FileStream.Create(DestFile);
    Source.Copy(Dest);
    Source.Release;
    Dest.Release;
    Exit;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='XML') Then
  Begin
    Result := ImportXML(SourceFile, DestFile, TargetDir, 'system', 'bin', ConstantFile);
    Exit;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='TMX') Then
  Begin
    Result := ImportXML(SourceFile, DestFile, TargetDir, 'maps', 'bin', ConstantFile);
    Exit;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='TXT') Then
  Begin
    Result := ImportTranslation(SourceFile, TargetDir);
    Exit;
  End;


  I := 0;
  While (I<GetImageExtensionCount()) Do
  Begin
    ImgInfo := GetImageExtension(I);

    If (StringUpper(Ext) = StringUpper(ImgInfo.Name)) Then
    Begin
      If (OutDir='') Then
        TargetDir := TargetDir + PathSeparator + 'textures';

      MakeDir(TargetDir);
      DestFile := TargetDir + PathSeparator + GetFileName(SourceFile, False);

      If (Not AssetModified(SourceFile, DestFile)) Then
      Begin
        Log(logConsole, 'Import', 'Skipping '+SourceFile+'...[Not modified]');
        Result := DestFile;
        Exit;
      End;

    	Log(logConsole, 'Import', 'Copying '+SourceFile+' to '+TargetDir);
      Source := MemoryStream.Create(SourceFile);
      Dest := FileStream.Create(DestFile);
      Source.Copy(Dest);
      Source.Release;
      Dest.Release;
      Exit;
    End;

    Inc(I);
  End;

  I := 0;
  While (I<_SoundExtensionCount) Do
  Begin
    If (StringUpper(Ext) = StringUpper(_SoundExtensions[I].Name)) Then
    Begin
      If (OutDir='') Then
        TargetDir := TargetDir + PathSeparator + 'Sound';

      MakeDir(TargetDir);
      DestFile := TargetDir + PathSeparator + GetFileName(SourceFile, False);

      If (Not AssetModified(SourceFile, DestFile)) Then
      Begin
        Log(logConsole, 'Import', 'Skipping '+SourceFile+'...[Not modified]');
        Result := DestFile;
        Exit;
      End;

    Log(logConsole, 'Import', 'Copying '+SourceFile+' to '+TargetDir);
      Source := MemoryStream.Create(SourceFile);
      Dest := FileStream.Create(DestFile);
      Source.Copy(Dest);
      Source.Release;
      Dest.Release;
      Exit;
    End;

    Inc(I);
  End;

  For I:= 0 To Pred(_ImporterCount) Do
  If (_Importers[I].Ext = Ext) Then
  Begin
    If (_Importers[I].Path<>'') And (OutDir='') Then
      TargetDir := TargetDir + PathSeparator + _Importers[I].Path;
    MakeDir(TargetDir);
    Result := _Importers[I].Importer(SourceFile, TargetDir, TargetPlatform, Settings);
    Exit;
  End;

	{$IFDEF USE_ASSIMP}
  If aiIsExtensionSupported(PTERRAChar('.'+GetFileExtension(SourceFile))) Then
  Begin
    TargetDir := TargetDir + PathSeparator + 'Mesh';
    Result := ASSIMP_Import(SourceFile, TargetDir);
    Exit;
  End;
  {$ENDIF}

  Filter := CreateMeshFilter(SourceFile);
  If Assigned(Filter) Then
  Begin
    MyMesh := Mesh.CreateFromFilter(Filter);
    MakeDir(TargetDir);
    Result := TargetDir + PathSeparator + GetFileName(SourceFile, True)+'.mesh';
    Dest := FileStream.Create(Result);
    MyMesh.Save(Dest);
    Dest.Release;
    Exit;
  End;

  If (StringUpper(GetFileExtension(SourceFile))='X') Then
  Begin
    Result := '';
    Exit;
  End;

  Log(logConsole, 'Import', 'No file handler found for: ' + SourceFile);
  Result := '';
End;

End.

