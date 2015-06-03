Unit TERRA_PackageBuilder;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_FileUtils, TERRA_Stream, TERRA_MemoryStream, TERRA_Collections,
  TERRA_Resource, TERRA_Package;

Type
  ResourceBuilderInfo = Class(ResourceInfo)
    Protected
      _Data:MemoryStream;

    Public
      Constructor Create(Const FileName:TERRAString; Owner:Package);
      Procedure Release(); Override;
  End;

  PackageBuilder = Class(Package)
    Private
      // Writes the package header to the file
      Procedure WriteHeader(Dest:Stream);

      // Rebuild internal file table
      Procedure WriteTable(Dest:Stream);

      // Writes resources to package file
      Procedure WriteResources(Dest:Stream);

      // Sorts package file table
      Procedure SortFileTable();
      
    Public
      // Save package contents
      Function Save(Const FileName:TERRAString):Boolean;

      //Adds a resource to the package
      Function AddResource(Const ResourceFileName:TERRAString):ResourceInfo;

      Procedure DeleteResource(Resource:ResourceInfo);
    End;

Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_CRC32, TERRA_Application, TERRA_OS, TERRA_Log, TERRA_ResourceManager,
  TERRA_FileStream, TERRA_FileManager;

Constructor ResourceBuilderInfo.Create(Const FileName:TERRAString; Owner:Package);
Begin
  _Owner := Owner;
  _Data := MemoryStream.Create(FileName);
  _FileName := StringLower(GetFileName(FileName, False));
  _CRC := GetCRC32(_Data.Buffer, _Data.Size);
  _Size := _Data.Size;
End;


Procedure ResourceBuilderInfo.Release();
Begin
  _Data.Release();
End;

Procedure PackageBuilder.WriteHeader(Dest:Stream);
Var
  Header:FileHeader;
Begin
  Header := TERRAHeader;
  Dest.Seek(0);
  Dest.WriteHeader(Header); //Write the tagheader to the file
  Dest.WriteInteger(Resources.Count); //Write tablesize
  Dest.WriteCardinal(_TableOffset); //Write table offset
End;

Procedure PackageBuilder.WriteTable(Dest:Stream);
Var
  It:Iterator;
  Resource:ResourceInfo;
Begin
  SortFileTable();

  _TableOffset := Dest.Position;
  It := Self.Resources.GetIterator();
  Begin
    Resource := ResourceInfo(It.Value);
    Dest.WriteString(Resource.FileName);  // Write resource name
    Dest.WriteCardinal(Resource.Offset); // Write offset of the resource
    Dest.WriteCardinal(Resource.Size);   // Write size of the resource
    Dest.WriteCardinal(Resource.CRC); // Write CRC
  End;
  ReleaseObject(It);
End;

Function PackageBuilder.Save(Const FileName:TERRAString):Boolean;
Var
   Dest:Stream;
Begin
  Log(logDebug,'Package', 'Creating package '+FileName);

  _Location := FileName;
  _Name := GetFileName(FileName, True);

  Dest := FileStream.Create(FileName);
  WriteHeader(Dest); // write empty header
  WriteResources(Dest);
  WriteTable(Dest);
  WriteHeader(Dest);  // now write true header
  Dest.Release();

  Result := True;
End;

(*Procedure PackageBuilder.QuickSort(iLo,iHi:Integer);
Var
  Lo, Hi: Integer;
  Temp, Mid:ResourceInfo;
Begin
    If iHi<iLo Then
      Exit;
    Lo := iLo;
    Hi := iHi;
    Mid := _Resources[(Lo + Hi) Shr 1];
    Repeat
      While Mid.Offset>_Resources[Lo].Offset Do Inc(Lo);
      While _Resources[Hi].Offset>Mid.Offset Do Dec(Hi);
      If Lo <= Hi Then
      Begin
        Temp := _Resources[Lo];
        _Resources[Lo] := _Resources[Hi];
        _Resources[Hi] := Temp;
        Inc(Lo);
        Dec(Hi);
      End;
    Until Lo > Hi;
    If Hi > iLo Then
      QuickSort(iLo, Hi);
    If Lo < iHi Then
      QuickSort(Lo, iHi);
End;*)

// Sort files in package
// First textures, then models and materials
Procedure PackageBuilder.SortFileTable;
Begin
  // Sort by offsets
//  QuickSort(0, Pred(_ResourceCount));
End;

// Adds a resource to the package
Function PackageBuilder.AddResource(Const ResourceFileName:TERRAString):ResourceInfo;
Var
  Buffer:PByte;
  I, N, Count:Integer;
  CRC:Cardinal;

  Source:MemoryStream;
  Name:TERRAString;
Begin 
  Log(logDebug,'Package', 'Adding resource '+ResourceFileName);

  Result := FindResourceByName(ResourceFileName);
  If Assigned(Result) Then
     DeleteResource(Result);

  Result := ResourceBuilderInfo.Create(ResourceFileName, Self);

  Resources.Add(Result)
End;

//Removes a resource from the package
Procedure PackageBuilder.DeleteResource(Resource:ResourceInfo);
Var
  It:Iterator;
  Res:ResourceInfo;
Begin
  Log(logDebug,'Package', 'Deleting resource '+Resource.FileName);

  It := Self.Resources.GetIterator();
  While It.HasNext() Do
  Begin
    Res := ResourceInfo(It.Value);
    If StringEquals(Res.FileName, Resource.FileName) Then
      Res.Discard();
  End;
  ReleaseObject(It);
End;

Procedure PackageBuilder.WriteResources(Dest: Stream);
Var
  It:Iterator;
  Resource:ResourceBuilderInfo;
Begin
  It := Self.Resources.GetIterator();
  While It.HasNext() Do
  Begin
    Resource := ResourceBuilderInfo(It.Value);
    Resource.Offset := Dest.Position;
    Resource._Data.Copy(Dest);
  End;
  ReleaseObject(It);
End;

End.
