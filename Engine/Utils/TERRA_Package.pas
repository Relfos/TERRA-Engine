Unit TERRA_Package;
{$I terra.inc}

{-$DEFINE ALLOWEXTERNAL}

Interface
Uses TERRA_String, TERRA_FileUtils, TERRA_Stream, TERRA_Collections, TERRA_Resource;

Const
  terraHeader:FileHeader = 'TePK';

  resCompressed = 1;
  resEncrypted  = 2;

Type
  Package = Class;

  PResourceInfo = ^ResourceInfo;
  ResourceInfo=Object
    Protected
      _Owner:Package;
      _TypeClass:ResourceClass;    // Resource type tag
      _Name:TERRAString;                // Resource name

      _FileName:TERRAString;            // Resource filename
      _Offset:Cardinal;            // Offset of the resource
      _Size:Cardinal;              // Size of the resource (bytes)
      _Flags:Cardinal;             // Flags
      _CRC:Cardinal;

      _ExternalPath:TERRAString;        // External path/override

      Function GetName:TERRAString;

    Public
      Function GetLocation:TERRAString;

      Property Name:TERRAString Read GetName;
      Property FileName:TERRAString Read _FileName;
      Property Size:Cardinal Read _Size;
      Property Flags:Cardinal Read _Flags;
  End;

  Package = Class
    Private
      _Name:TERRAString;
      _Location:TERRAString;
      _File:Stream;
      _DataOffset:Cardinal;  // Header size, files data starts here
      _TableOffset:Cardinal; // Table position in the file
      _CRC:Cardinal;
      _EditMode:Boolean;

      _ResourceList:Array Of ResourceInfo; // List of all resources within the file
      _ResourceCount:Integer;    // Number of resources in the table

      // Writes the package header to the file
      Procedure RebuildHeader;

      // Rebuild internal file table
      Procedure RebuildFileTable;

      // Sorts package file table
      Procedure SortFileTable;
      Procedure QuickSort(iLo,iHi:Integer);

      // Read package contents
      Function Load:Boolean;

      // unloads resources
      Function Unload:Boolean;

      Function GetNewOffset(Size:Integer):Integer;

    Public
      Path:TERRAString;  // File override path
      Password:TERRAString;

      // Create a new empty package
      Constructor New(FileName:TERRAString);

      // Load a package from disk
      Constructor Open(FileName:TERRAString; EditMode:Boolean = False); Overload;
      Constructor Open(Source:Stream); Overload;

      Destructor Destroy; Reintroduce;

      // Save package contents
      Function Save:Boolean;

      // Register all resources into manager
      Function Update:Boolean;

      // Searches for a resource inside a package
      Function FindResource(ResourceName:TERRAString):PResourceInfo;

      // Loads a resource into a stream
      // Note: If resource file is found in search path the is loaded from there
      // This can be used for patches/mods
      Function LoadResource(Resource:PResourceInfo; AllowPatches:Boolean = False):Stream;

      //Adds a resource to the package
      Function AddResource(ResourceFileName:TERRAString; Flags:Cardinal=0):PResourceInfo;

      Procedure DeleteResource(Resource:PResourceInfo);

      // Gets package CRC
      Function GetCRC:Cardinal;

      Function GetResource(Index:Integer):PResourceInfo;

      Function CreateIterator:Iterator;

      // Package name
      Property Name:TERRAString Read _Name;
      Property ResourceCount:Integer Read _ResourceCount;
    End;

Implementation
Uses TERRA_Error, TERRA_Utils, TERRA_CRC32, TERRA_Application, TERRA_OS, TERRA_Log, TERRA_ResourceManager,
  TERRA_FileStream, TERRA_FileManager, TERRA_MemoryStream;

Type
  PackageIterator=Class(Iterator)
    Protected
      _Package:Package;
      _Index:Integer;

    Public
      Function HasNext:Boolean; Override;
      Function GetNext:ListObject; Override;
  End;

Function ResourceInfo.GetName:TERRAString;
Begin
  If Assigned(_TypeClass) Then
    Result := _TypeClass.ClassName+'.'+_Name
  Else
    Result := 'Resource.'+_Name;
  Result := StringUpper(Result);
End;

Function ResourceInfo.GetLocation:TERRAString;
Begin
   Result := _Owner._Location + PathSeparator + _Filename;
End;

//Package class
Function Package.Update:Boolean;
Var
  I:Integer;
  MyResource:Resource;
  NewLocation:TERRAString;
  Manager:ResourceManager;
Begin
  For I:=0 To Pred(_ResourceCount) Do
  Begin
    NewLocation := _ResourceList[I].GetLocation;
    {$IFDEF GLOBALDEBUG}
    Log(logDebug,'PackageManager', 'Processing ' + NewLocation);
    {$ENDIF}

    If Assigned(_ResourceList[I]._TypeClass) Then
    Begin
      MyResource := _ResourceList[I]._TypeClass.Create(NewLocation);
      //Log(logDebug, 'Package', _ResourceList[I]._Name);
      Manager := ResourceManager(MyResource.GetManager());
      Manager.AddResource(MyResource);
    End;
  End;

  Result := True;
End;

Function Package.GetResource(Index:Integer):PResourceInfo;
Begin
  Result := @(_ResourceList[Index]);
End;

Procedure Package.RebuildHeader;
Var
  Header:FileHeader;
Begin
  Header := TERRAHeader;
  _File.Seek(0);
  _File.Write(@Header, 4); //Write the tagheader to the file
  _File.WriteString(_Name);     //Write the name of the package
  _File.Write(@_ResourceCount, SizeOf(_ResourceCount)); //Write tablesize
  _File.Write(@_TableOffset, SizeOf(_TableOffset)); //Write table offset
End;

Procedure Package.RebuildFileTable;
Var
  I:Integer;
  Resource:PResourceInfo;
Begin
  SortFileTable;

  _File.Seek(_TableOffset);
  For I:=0 To Pred(_ResourceCount) Do
  Begin
    Resource := @(_ResourceList[I]);
    _File.WriteString(Resource._FileName);  // Write resource name
    _File.Write(@Resource._Offset, SizeOf(Resource._Offset)); // Write offset of the resource
    _File.Write(@Resource._Size, SizeOf(Resource._Size));   // Write size of the resource
    _File.Write(@Resource._Flags, SizeOf(Resource._Flags)); // Write flags
    _File.Write(@Resource._CRC, SizeOf(Resource._CRC)); // Write CRC
  End;
End;

Constructor Package.New(FileName:TERRAString);
Begin
  Log(logDebug,'Package', 'Creating package '+FileName);

  _Location := FileName;
  _Name := StringUpper(GetFileName(FileName, True));
  _EditMode := True;
  _ResourceCount := 0;
  _TableOffset := 4 + Succ(Length(_Name)) + SizeOf(_ResourceCount) + SizeOf(_TableOffset);
  Password := '*';

  _File := FileStream.Create(FileName);
  RebuildHeader;
  _DataOffset := _File.Position;
End;

Destructor Package.Destroy;
Begin
  Unload;
End;

Function Package.Unload:Boolean;
Begin
  _ResourceCount := 0;
  If Assigned(_File) Then
  Begin
    _File.Destroy;
    _File := Nil;
  End;
    Result := True;
End;

Procedure Package.QuickSort(iLo,iHi:Integer);
Var
  Lo, Hi: Integer;
  Temp, Mid:ResourceInfo;
Begin
    If iHi<iLo Then
      Exit;
    Lo := iLo;
    Hi := iHi;
    Mid := _ResourceList[(Lo + Hi) Shr 1];
    Repeat
      While Mid._Offset>_ResourceList[Lo]._Offset Do Inc(Lo);
      While _ResourceList[Hi]._Offset>Mid._Offset Do Dec(Hi);
      If Lo <= Hi Then
      Begin
        Temp := _ResourceList[Lo];
        _ResourceList[Lo] := _ResourceList[Hi];
        _ResourceList[Hi] := Temp;
        Inc(Lo);
        Dec(Hi);
      End;
    Until Lo > Hi;
    If Hi > iLo Then
      QuickSort(iLo, Hi);
    If Lo < iHi Then
      QuickSort(Lo, iHi);
End;

// Sort files in package
// First textures, then models and materials
Procedure Package.SortFileTable;
Begin
  // Sort by offsets
  QuickSort(0, Pred(_ResourceCount));

{  Repeat
    Done:=True;
    For I:=0 To _TableSize-2 Do
    If (_Table[I].Offset>_Table[Succ(I)].Offset) Then
    Begin
      Temp := _Table[I];
      _Table[I] := _Table[Succ(I)];
      _Table[Succ(I)] := Temp;
      Done := False;
    End;
  Until Done;}

  // Print table to log
{  Log.Write(logDebug,'Package','SortTable','File:'+Self._FileName+' DataOffset:'+LStr(Self._DataOffset) +' TableOffset:'+LStr(Self._TableOffset));
  For I:=0 To Pred(_TableSize) Do
  Begin
    If I>0 Then
      J:=_Table[I].Offset - (_Table[Pred(I)].Offset+_Table[Pred(I)].Size)
    Else
      J:=_Table[I].Offset - _DataOffset;
    Log.Write(logDebug,'Package','SortTable',LStr(I)+'  '+_Table[I].FileName+' Offset:'+lstr(_Table[I].Offset)+' Size:'+lstr(_Table[I].Size)+' Free:'+LStr(J));
  End;}
End;

Function Package.Load:Boolean;
Var
  I,J:Integer;
  S:TERRAString;
  Header:FileHeader;
  Resource:PResourceInfo;
Begin
  Result := False;

  _File.Read(@Header, SizeOf(Header));
  If Header<>TERRAHeader Then
  Begin
    RaiseError('Invalid header. ['+_File.Name+']');
    Exit;
  End;

  _File.ReadString(_Name);
  _File.Read(@_ResourceCount, SizeOf(_ResourceCount)); //Read filetable info
  _File.Read(@_TableOffset,SizeOf(_TableOffset));
  _DataOffset := _File.Position;

  _File.Seek(_TableOffset);
  SetLength(_ResourceList, _ResourceCount);
  For I:=0 To Pred(_ResourceCount) Do
  Begin
    Resource := @(_ResourceList[I]);
    Resource._Owner := Self;
    _File.ReadString(S); //Read resource name
    //Log(logDebug, 'Package', 'Read '+S);
    Resource._FileName := S;
    Resource._Name := StringUpper(GetFileName(S,True));

    S := FindResourceClass(Resource._FileName);
    Resource._TypeClass := GetResourceClass(S);

    For J:=0 To Pred(I) Do
    If (_ResourceList[J].GetName = Resource.GetName) Then
    Begin
      RaiseError('Duplicated resource ['+Resource.Name+'] in package '+Self._Name+'.');
      Exit;
    End;
    _File.Read(@Resource._Offset, SizeOf(Resource._Offset));       // Read offset of the resource
    _File.Read(@Resource._Size, SizeOf(Resource._Size));           // Size of the resource
    _File.Read(@Resource._Flags, SizeOf(Resource._Flags));         // Read flags
    _File.Read(@Resource._CRC, SizeOf(Resource._CRC));         // Read CRC

    {$IFDEF ALLOWEXTERNAL}
    Resource._ExternalPath := FileManager.Instance.SearchResourceFile(GetFileName(Resource._FileName, False));
    {$ELSE}
    Resource._ExternalPath := '';
    {$ENDIF}
  End;

  SortFileTable;
  Result := True;
End;

Function Package.Save:Boolean;
Begin
	RebuildHeader;
	_TableOffset := _File.Position;
	RebuildFileTable;
	Result := True;
End;

Constructor Package.Open(Source:Stream);
Begin
  _Location := Source.Name;
  _EditMode := False;
  _File := Source;
  Self.Load;

  If (_Location='') Then
    _Location := _Name+'.leaf';
End;

Constructor Package.Open(FileName:TERRAString; EditMode:Boolean);
Begin
  _Location := FileName;
  _Name := GetFileName(FileName, True);
  _EditMode := EditMode;
  Password := '*';

  If (EditMode) Then
    _File := FileStream.Open(FileName, smDefault Or smAppend)
  Else
    _File := FileStream.Open(FileName, smRead);

  Self.Load;
End;


//Searches for a resource within the file table
//If not found returns nil
Function Package.FindResource(ResourceName:TERRAString):PResourceInfo;
Var
  I:Integer;
Begin
  ResourceName := StringUpper(FindResourceClass(ResourceName)+'.'+GetFileName(ResourceName,True));

  Result := Nil;
  For I:=0 To Pred(_ResourceCount) Do
   If (_ResourceList[I].Name = ResourceName) Then
    Begin
      Result := @(_ResourceList[I]);
      Break;
    End;

  If Not Assigned(Result)Then
    Log(logDebug,'Package', 'Resource not found.['+ResourceName+']');
End;

//Loads a resource from the package into a stream
Function Package.LoadResource(Resource:PResourceInfo; AllowPatches:Boolean = False):Stream;
Var
  Buffer:PByte;
  I, N, Count:Integer;
  CRC:Cardinal;
Begin
	Result := Nil;
	
  If Not Assigned(Resource) Then
  Begin
    RaiseError('Package.LoadResource(): Null resource');
    Exit;
  End;

//  Log(logDebug,'Package', 'Loading resource '+Resource._Name);

  If (AllowPatches) And (Resource._ExternalPath<>'') Then
  Begin
    Result := MemoryStream.Create(Resource._ExternalPath);
    Result.Name := Resource._ExternalPath;
    Exit;
  End;

  Result := MemoryStream.Create(Resource._Size);
  Result.Name := Resource.GetLocation;
  _File.Copy(Result, Resource._Offset, Resource._Size);

  If (Resource._Flags And resEncrypted<>0) Then
  Begin
    Buffer := MemoryStream(Result).Buffer;
    Count := Result.Size;
    I := 1;
    While (Count>0) Do
    Begin
      N := ((Buffer^) + (256-Ord(Password[I]))) Mod 256;
      If (I>Length(Password)) Then
        I := 1;
      Buffer^ := N;
      Inc(Buffer);
      Dec(Count);
    End;

    CRC := GetCRC32(MemoryStream(Result).Buffer, Result.Size);
    If (CRC<>Resource._CRC) Then
    Begin
      RaiseError('Cannot load resource, CRC failed. ['+Resource._Name+']');
    End;
  End;

  Result.Seek(0);
End;

// Adds a resource to the package
Function Package.AddResource(ResourceFileName:TERRAString; Flags:Cardinal):PResourceInfo;
Var
  Buffer:PByte;
  I, N, Count:Integer;
  CRC:Cardinal;
  MyResourceClass:ResourceClass;

  Source:MemoryStream;
  Name:TERRAString;
Begin
  Log(logDebug,'Package', 'Adding resource '+ResourceFileName);

  Name := FindResourceClass(ResourceFileName);
  MyResourceClass := GetResourceClass(Name);

  Name := StringUpper(GetFileName(ResourceFileName, True));
  If Assigned(MyResourceClass) Then
    Name := StringUpper(MyResourceClass.ClassName) + '.' + Name
  Else
    Name := 'Resource.'+Name;

  Result := FindResource(ResourceFileName);
  If Assigned(Result) Then
    DeleteResource(Result);
  SortFileTable;

  Source := MemoryStream.Create(ResourceFileName);
  ResourceFileName := GetFileName(ResourceFileName, False);
  CRC := GetCRC32(Source.Buffer, Source.Size);

  If (Flags And resEncrypted<>0) Then
  Begin
    Buffer := Source.Buffer;
    Count := Source.Size;
    I := 1;
    While (Count>0) Do
    Begin
      N := ((Buffer^) + Ord(Password[I])) Mod 256;
      If (I>Length(Password)) Then
        I := 1;
      Buffer^ := N;
      Inc(Buffer);
      Dec(Count);
    End;
  End;

  SetLength(_ResourceList, Succ(_ResourceCount));
  Result := @(_ResourceList[_ResourceCount]);
  Result._Owner := Self;
  Result._FileName := ResourceFileName;
  Result._Name := Name;
  Result._TypeClass := MyResourceClass;
  Result._Size := Source.Size;
  Result._Offset := GetNewOffset(Result._Size);
  Result._Flags := Flags;
  Result._CRC := CRC;

  //Start copying the resource into the package file
  _File.Seek(Result._Offset);
  Source.Copy(_File);
  Source.Destroy;

  // Update table
  Inc(_ResourceCount);

  _TableOffset := _File.Position;

  // Filetable was overwritten by new resource, so rebuild it
  RebuildHeader;
  RebuildFileTable;
End;

//Removes a resource from the package
Procedure Package.DeleteResource(Resource:PResourceInfo);
Var
  I:Integer;
Begin
  Assert(Assigned(Resource),'Package.DeleteResource(): Null resource.');

  Log(logDebug,'Package', 'Deleting resource '+Resource._Name);

  I:=0;
  While I<_ResourceCount Do
  If _ResourceList[I]._Name = Resource._Name Then
  Begin
    _ResourceList[I] := _ResourceList[Pred(_ResourceCount)];
    Dec(_ResourceCount);
    Break;
  End Else
    Inc(I);

  RebuildFileTable;
End;

Function Package.GetCRC:Cardinal;
Var
  Source:Stream;
Begin
  If _CRC=0 Then
  Begin
    Source := FileManager.Instance.OpenStream(_Location);
    _CRC := GetCRC32(Source);
    Source.Destroy;
  End;
  Result:=_CRC;
End;

Function Package.GetNewOffset(Size: Integer): Integer;
Var
  DataStart,I:Integer;
Begin
  If _ResourceCount = 0 Then
  Begin
    Result := _DataOffset;
    Exit;
  End;

  // Check for fragmented holes to fill
  For I:=0 To Pred(_ResourceCount) Do
  Begin
    If I=0 Then
      DataStart := _DataOffset
    Else
      DataStart := _ResourceList[Pred(I)]._Offset+ _ResourceList[Pred(I)]._Size;
    If (_ResourceList[I]._Offset-DataStart >= Size) Then
    Begin
      Result:=DataStart;
      Exit;
    End;
  End;

  //Otherwise calculate new offset from last file in table
  Result := _ResourceList[Pred(_ResourceCount)]._Offset + _ResourceList[Pred(_ResourceCount)]._Size;
End;


{ PackageIterator }
Function PackageIterator.GetNext: ListObject;
Begin
  Result := @(_Package._ResourceList[_Index]);
  Inc(_Index);
End;

Function PackageIterator.HasNext: Boolean;
Begin
  Result := (_Index<_Package._ResourceCount);
End;

Function Package.CreateIterator: Iterator;
Begin
  Result := PackageIterator.Create;
  PackageIterator(Result)._Package := Self;
  PackageIterator(Result)._Index := 0;
End;

End.
