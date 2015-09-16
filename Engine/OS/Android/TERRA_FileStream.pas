// Android custom IO
Unit TERRA_FileStream;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Error, TERRA_String, TERRA_Stream, TERRA_MemoryStream, TERRA_FileUtils, TERRA_Java;

Const
  FileIOClassPath = 'com.pascal.terra.TERRAFileIO';

Type
  FilePointer = Pointer;
  FileStream=Class(MemoryStream)
     Protected
        _File:FilePointer;
        _Open:Boolean;
     Public

        Constructor Create(FileName:TERRAString; StreamMode:Integer=smDefault);Overload;
        Constructor Open(FileName:TERRAString; StreamMode:Integer=smDefault);
        Procedure Release;Override;

        Procedure Truncate();Override;

        Procedure Flush;

        Function Read(Data:Pointer; Length:Cardinal):Cardinal;Override;
        Function Write(Data:Pointer; Length:Cardinal):Cardinal;Override;
        Procedure Seek(NewPosition:Cardinal);Override;

        Class Function Exists(FileName:TERRAString): Boolean;
        Class Procedure CopyFile(SourceName, DestName:TERRAString);
     End;

Implementation
Uses JNI, TERRA_Log, TERRA_OS, TERRA_Application, TERRA_FileManager, TERRA_Utils;

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

Type
  PAAssetManager = Pointer;
  PAAssetDir = Pointer;
  PAAsset = Pointer;

//Available modes for opening assets
Const
  AASSET_MODE_UNKNOWN = 0;
  AASSET_MODE_RANDOM = 1;
  AASSET_MODE_STREAMING = 2;
  AASSET_MODE_BUFFER = 3;

{$IFDEF USE_ASSET_MANAGER}
{ Open the named directory within the asset hierarchy.
The directory can then be inspected with the AAssetDir functions.
To open the top-level directory,  pass in "" as the dirName.
The object returned here should be freed by calling AAssetDir_close().}
Function AAssetManager_openDir(mgr: PAAssetManager; dirName: Pchar): PAAssetDir; cdecl; external;

// Open an asset.
// The object returned here should be freed by calling AAsset_close().
Function AAssetManager_open(mgr: PAAssetManager; filename: Pchar; mode:Integer): PAAsset; cdecl; external;

{ Iterate over the files in an asset directory.
A NULL string is returned when all the file names have been returned.
The returned file name is suitable for passing to AAssetManager_open().
The string returned here is owned by the AssetDir implementation and is not guaranteed to remain valid if any other calls are made on this AAssetDir instance.}
Function AAssetDir_getNextFileName(assetDir:PAAssetDir): Pchar; cdecl; external;

// Reset the iteration state of AAssetDir_getNextFileName() to the beginning.
Procedure AAssetDir_rewind(assetDir: PAAssetDir); cdecl; external;

// Close an opened AAssetDir, freeing any related resources.
Procedure AAssetDir_close(assetDir: PAAssetDir); cdecl; external;

// Attempt to read 'count' bytes of data from the current offset.
// Returns the number of bytes read, zero on EOF, or < 0 on error.
Function AAsset_read(asset: PAAsset; buf: Pointer; count:Cardinal):Integer; cdecl; external;

// Seek to the specified offset within the asset data.  'whence' uses the same constants as lseek()/fseek().
//Returns the new position on success, or (off_t) -1 on error.
function AAsset_seek(asset: PAAsset; offset:Int64; whence: Cardinal):Int64; cdecl; external;

// Close the asset, freeing all associated resources.
Procedure AAsset_close(asset: PAAsset); cdecl; external;

// Get a pointer to a buffer holding the entire contents of the assset.
// Returns NULL on failure.
Function AAsset_getBuffer(asset: PAAsset): Pointer; cdecl; external;

// Report the total size of the asset data.
Function AAsset_getLength(asset: PAAsset): Int64; cdecl; external;

// Report the total amount of asset data that can be read from the current position.
Function AAsset_getRemainingLength(asset: PAAsset): Int64; cdecl; external;

// Open a new file descriptor that can be used to read the asset data.
// Returns < 0 if direct fd access is not possible (for example, if the asset is compressed).
Function AAsset_openFileDescriptor(asset:PAAsset; Var outStart, outLength:Int64):Integer; cdecl; external;

// Returns whether this asset's internal buffer is allocated in ordinary RAM (i.e. not mmapped).
Function AAsset_isAllocated(asset: PAAsset):Integer; Cdecl; External;

Function AAssetManager_fromJava(Env:PJNIEnv; assetManager:JObject):PAAssetManager; Cdecl; External;


(*Function AAssetManager_fromJava():;
Const
  AssetManagerPath = 'android/content/res/AssetManager';
Var
  amClass:JClass;
  mObject:JfieldID;
  javaAssetManager:JObject;
  Frame:JavaFrame;
  Utils:JavaClass;
Begin
  Java_Begin(Frame);

  Log(logDebug, 'App', 'Obtaining asset manager class');
  Utils := JavaClass.Create(ActivityClassPath, Frame);
  javaAssetManager := Utils.CallStaticObjectMethod(Frame, 'getAssetManager', AssetManagerPath, Nil);
  ReleaseObject(Utils);

  Log(logDebug, 'App', 'Obtaining asset manager object');
  amClass := JClass(Frame^^.FindClass(Frame, AssetManagerPath));
  mObject := Frame^^.GetFieldID(Frame, amClass, 'mObject', 'I');
  Result := PAAssetManager(Frame^^.GetIntField(Frame, javaAssetManager, mObject));

  Java_End(Frame);
End;*)

Var
  _GlobalAssetManager:PAAssetManager = Nil;

Function GetAssetManager():PAAssetManager;
Const
  AssetManagerPath = 'android/content/res/AssetManager';
Var
  Frame:JavaFrame;
  Utils:JavaClass;
  Obj:JavaObject;
Begin
  Log(logDebug, 'FileIO', 'Getting asset manager');

  If Assigned(_GlobalAssetManager) Then
  Begin
    Result := _GlobalAssetManager;
    Exit;
  End;

  Java_Begin(Frame);
  Log(logDebug, 'App', 'Obtaining asset manager class');
  Utils := JavaClass.Create(ActivityClassPath, Frame);
  Obj := Utils.CallStaticObjectMethod(Frame, 'getAssetManager', AssetManagerPath, Nil);
  _GlobalAssetManager := AAssetManager_fromJava(Frame, Obj);
  ReleaseObject(Utils);
  Java_End(Frame);

  Log(logDebug, 'FileIO', 'Asset manager was obtained: '+HexStr(Cardinal(Result)));
End;
{$ENDIF}

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

Class Function FileStream.Exists(FileName:TERRAString): Boolean;
Var
  Assets:JavaClass;
  Params:JavaArguments;
  Frame:JavaFrame;

  AssetManager:PAAssetManager;
  Asset:PAAsset;

  F:Pointer;
Begin
  FileName := GetOSIndependentFileName(FileName);
  Log(logDebug, 'FileIO', 'File.Exists: '+FileName);

  {$IFDEF USE_ASSET_MANAGER}
  AssetManager := GetAssetManager();
  If Assigned(AssetManager) Then
  Begin
    Log(logDebug, 'File', 'Opening asset: '+FileName);
    asset := AAssetManager_open(AssetManager, PAnsiChar(FileName), AASSET_MODE_UNKNOWN);
    Result:= Assigned(Asset);
    If Result Then
    Begin
      Log(logDebug, 'File', 'Asset was found: '+FileName);
      AAsset_close(Asset);
      Exit;
    End;

    Log(logDebug, 'File', 'Asset was not found, trying something else');
  End;
  {$ENDIF}

  Java_Begin(Frame);
  Assets := JavaClass.Create(FileIOClassPath, Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(FileName);
  Result := Assets.CallStaticBoolMethod(Frame, 'fileExists', Params);
  ReleaseObject(Params);
  ReleaseObject(Assets);
  Java_End(Frame);

  If Result Then
  Begin
    Log(logDebug, 'File', 'Asset was found!');
    Exit;
  End;

  // not found in assets, try in app path
  Log(logDebug, 'File', 'Testing file '+FileName);
	f := fopen(PAnsiChar(FileName), 'rb');
	If (f<>Nil) Then
	Begin
    Log(logDebug, 'File', 'OK!!!');
		fclose(f);
		Result := True;
	End Else
    Result := False;
End;

Constructor FileStream.Create(FileName:TERRAString; StreamMode:Integer=smDefault);
Begin
  _Name := '';
  _Mode := StreamMode;
  _Pos := 0;
  _Buffer := Nil;

  FileName := GetOSIndependentFileName(FileName);
  FileManager.Instance.RemoveFromCache(FileName);

  Log(logDebug,'IO','Creating '+FileName);
  If StreamMode=0 Then
    RaiseError('Invalid file mode.['+FileName+']')
  Else
  Begin
    _Name := FileName;
    _Size := 0;

	  _File := fopen(PAnsiChar(_Name), PAnsiChar('wb'));

    _Open := Assigned(_File);

    If Not _Open Then
      Log(logError, 'App', 'Could not create ' + FileName)
    Else
      Log(logDebug, 'App', 'Created ' + FileName);
  End;
End;

Constructor FileStream.Open(FileName:TERRAString; StreamMode:Integer=smDefault);
Var
  Assets:JavaObject;
  Params:JavaArguments;
  Frame:JavaFrame;

  FSize:Integer;

  AssetManager:PAAssetManager;
  Asset:PAAsset;
Begin
  Inherited Create(StreamMode);

  If StreamMode=0 Then
  Begin
    RaiseError('Invalid file mode. ['+FileName+']');
    Exit;
  End;

  {$IFDEF USE_ASSET_MANAGER}
  AssetManager := GetAssetManager();
  If Assigned(AssetManager) Then
  Begin
    asset := AAssetManager_open(AssetManager, PAnsiChar(FileName), AASSET_MODE_UNKNOWN);
    If (Assigned(asset)) Then
    Begin
      _Size := AAsset_getLength(asset);

    	Log(logDebug, 'IO', 'filesize '+IntToString(_Size));

      Create(_Size, Nil, StreamMode);

      AAsset_read(Asset, _Buffer, _Size);
      AAsset_close(Asset);

      _Open := True;
      Exit;
    End;
  End;
  {$ENDIF}

  _Buffer := Nil;
  FileName := GetOSIndependentFileName(FileName);
  Log(logDebug, 'FileIO', 'File.IO:Opening '+FileName);
  _Name := FileName;

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(FileName);
  Assets := JavaObject.Create(FileIOClassPath, Params, Frame);
  ReleaseObject(Params);

  FSize := Assets.CallIntMethod(Frame, 'getSize', Nil);
  If FSize >=0 Then
  Begin
    _Size := FSize;
  	Log(logDebug, 'IO', 'Asset Size '+IntToString(_Size));
    Create(_Size, Nil, StreamMode);

  	Log(logDebug, 'IO', 'Loading byte data into stream');
    _Buffer := Assets.CallByteArrayMethod(Frame, 'getContents', Nil, _Size);

    _File := Nil;
    Log(logDebug, 'IO', 'all ok!');
  End;

  ReleaseObject(Assets);
  Java_End(Frame);

  If (FSize>=0) Then
  Begin
    _Open := True;
    Exit;
  End;

  Log(logDebug, 'FileIO', 'Not an asset, loading normally.');
  // not an asset, use normal file functions
  _Open := False;

	_File := fopen(PAnsiChar(_Name), PAnsiChar('rb'));
	If Not Assigned(_File) Then
  Begin
    RaiseError('File not found. ['+FileName+']');
    Exit;
  End;

	FSize := FileSize(_File);

  If (FSize>=0) Then
    _Size := Cardinal(FSize)
  Else
    _Size := 0;

  Log(logDebug, 'IO', 'filesize '+IntToString(_Size));

  _Buffer := Nil;
  _Open:=True;
End;

Procedure FileStream.Release;
Begin
  Inherited;

  If (Assigned(_File)) And (_Open) Then
    fclose(_File);
End;

Function FileStream.Read(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Result:=0;

  If (Not _Open) Or (Length=0) Then
  Begin
    Exit;
  End;

  If Assigned(_Buffer) Then
  Begin
    //Log(logDebug,'FileIO', 'Reading from memory '+IntToString(Length)+' bytes');
    Result := Inherited Read(Data, Length);
    Exit;
  End;

  //Log(logDebug,'FileIO', 'Position '+IntToString(_pos));

  If (_Pos>=_Size) Then
  Begin
    Result := 0;
    RaiseError('Cannot read from file. ['+_Name+']');
    Exit;
  End;

  If (_Pos+Length>_Size)Then
    Length := _Size-_Pos;

  If (Length<=0) Then
  Begin
    Result:=0;
    Exit;
  End;

  //Log(logDebug,'FileIO', 'Length= '+IntToString(Length)+' bytes');
  //Log(logDebug,'FileIO', 'Reading from disk '+IntToString(Length)+' bytes');
  Length := fread(Data, 1, Length, _File);

  Inc(_Pos, Length);
  Result := Length;
  //Log(logDebug,'FileIO', 'New Position '+IntToString(_pos));
End;

Function FileStream.Write(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  Result := 0;

  If (Not _Open) Or (Length=0) Then
  Begin
    Exit;
  End;

  If Assigned(_Buffer) Then
  Begin
    Log(logDebug, 'App', 'Writing to assets not supported! ' + Self._Name);
    Exit;
  End;

  //Log(logDebug, 'App', 'Writing to file! ' + Self._Name);

  If (_Mode And smWrite=0)Then
  Begin
    RaiseError('File is write protected.['+_Name+']');
    Exit;
  End;

  If (_Pos+Length>_Size)And(_Mode And smDynamic=0) Then
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
  If (Not _Open) Then
    Exit;

  If Assigned(_Buffer) Then
  Begin
    Inherited Seek(NewPosition);
    Exit;
  End;

  If _Pos>_Size Then
  Begin
    RaiseError('Cannot seek in file.['+_Name+']');
    Exit;
  End;

  _Pos := NewPosition;

	fseek(_File, _Pos, SEEK_SET);
End;


Procedure FileStream.Truncate();
Begin
  _Size := _Pos;
End;

Class procedure FileStream.CopyFile(SourceName, DestName:TERRAString);
Var
  Source, Dest:Stream;
Begin
  Source := MemoryStream.Create(SourceName);
  Dest := FileStream.Create(DestName);
  Source.Copy(Dest);
  ReleaseObject(Source);
  ReleaseObject(Dest);
End;

Procedure FileStream.Flush;
Begin
  If (Not _Open) Then
    Exit;

  {If (Application.Instance = Nil) Or (Not Application.Instance.CanReceiveEvents) Then
    Exit;}

  If (Assigned(_File)) And (_Open) Then
  Begin
    fclose(_File);
	  _File := fopen(PAnsiChar(_Name), PAnsiChar('ab'));
    _Open := Assigned(_File);
  End;
End;

End.
