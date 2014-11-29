{
@abstract(File and Memory Streams)
@author(Sergio Flores <relfos@gmail.com>)
@created(March 15, 2005)
@lastmod(January 4, 2006)
The IO unit provides stream classes for reading/writing data.

Version History
   15/3/05  • Implemented TStream object
               • Implemented TFileStream object
               • Implemented TMemoryStream object
            • Implemented TResourceStream object
   16/3/05  • Implemented TList and TQueueList objects
               • FIFO like stream for use with fixed size items
            • Added CopyStream() function - not implemented yet
            • Implemented TCompressedStream
               • Uses a combination of LZSS compression and adaptative Huffman coding
   17/3/05  • Implemented CopyStream()
            • CopyStream replaced with Copy method of TStream
   12/6/05  • Create() overloaded method of LFileStream renamed to Open()
            • Added Delete() destructor to LFileStream
            • Stream.Copy overload version now allows offset and size arguments
            . Added Rename method to LFileStream
   27/6/05  • Fixed bug in Stream.Copy that caused segmentation fault
   28/6/05  • Fixed another bug in Stream.Copy
   30/7/05  • Added ReadString and ReadLine methods
-            • Optimized MemoryStream for single byte,word and dword reads/writes
   11/8/05  • Implemented CopyText()
               • Allows to copy a stream into another, formated as text
               • Useful for reading text files
   14/8/05  • Implemented Stream.Skip()
   1/11/05  • Implemented LBitStream class
    7/7/06  • Extended LFileStream.Open method
               • Now supports following sintax
                  [Path\][Package:]Filename
    8/7/06  • Fixed bug with FileStream size when offset>0
    9/7/06  • Resource file Loading now supports load external resources
   18/8/06  • Added tests to FileStream to prevent some IO errors when file not exists
   22/8/06  • Added byteorder properties, to simplify reading BigEndian/LittleEndian streams
            • Added support for writing with byteorder

}
Unit TERRA_FileIO;

{$I terra.inc}

Interface
Uses TERRA_IO, TERRA_FileUtils;

Type
  FilePointer=File;

  FileStream=Class(Stream)
     Protected
        _File:FilePointer;
        _Open:Boolean;
     Public

        Constructor Create(FileName:AnsiString; StreamMode:Integer=smDefault);Overload;
        Constructor Open(FileName:AnsiString; StreamMode:Integer=smDefault; Offset:Integer=0; MaxSize:Integer=-1);
        Destructor Destroy;Override;
        Procedure Delete;
        Procedure Rename(NewName:AnsiString);
        Procedure Truncate;Override;
        Function Read(Data:Pointer; Length:Cardinal):Cardinal; Override;
        Function Write(Data:Pointer; Length:Cardinal):Cardinal; Override;
        Procedure Seek(NewPosition:Cardinal);Override;

        Procedure Flush;

        Class Function Exists(FileName:AnsiString): Boolean;
        Class Procedure CopyFile(SourceName, DestName:AnsiString);
     End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Application, TERRA_Utils, TERRA_FileManager, SysUtils;

{$I-}

Procedure FileSeek(Var F:FilePointer; Offset:Integer);
Begin
  System.Seek(F, Offset);
End;

Procedure FileTruncate(Var F:FilePointer);
Begin
  System.Truncate(F);
End;


Procedure FileRename(Var F:FilePointer; Name:AnsiString);
Begin
  System.Rename(F, Name);
End;

{$IFDEF WINDOWS}
Class Function FileStream.Exists(FileName:AnsiString): Boolean;
Begin
  Log(logDebug, 'File', 'Testing file '+FileName);
  FileMode := 0;
  FileName := GetOSIndependentFileName(FileName);
  Result:=(FileAge(FileName)<>-1);
End;
{$ELSE}
Class Function FileStream.Exists(FileName:AnsiString): Boolean;
Begin
  {$IFDEF DEBUG_FILECACHE}Log(logDebug, 'FileManager', 'Testing for file '+FileName);{$ENDIF}

  FileName := GetOSIndependentFileName(FileName);
  Result := FileExists(FileName);
End;
{$ENDIF}


{**************************
   TFileStream Object
 **************************}

Constructor FileStream.Create(FileName:AnsiString; StreamMode:Integer=smDefault);
Begin
  Inherited Create(StreamMode);

  FileName := GetOSIndependentFileName(FileName);
  FileManager.Instance.RemoveFromCache(FileName);

  Log(logDebug,'IO','Opening '+FileName);
  If StreamMode=0 Then
    RaiseError('Invalid file mode.['+FileName+']')
  Else
  Begin
    _Name:=FileName;
    _Size:=0;
    FileMode := 2;

    AssignFile(_File,_Name);
    Rewrite(_File,1);

    _Open:=True;
  End;
End;

Constructor FileStream.Open(FileName:AnsiString; StreamMode:Integer=smDefault; Offset:Integer=0; MaxSize:Integer=-1);
Begin
  Inherited Create(StreamMode);

  FileName := GetOSIndependentFileName(FileName);

  _Open := False;
  _Name := FileName;

  {$IFNDEF NDS}
  If Not FileExists(PAnsiChar(FileName)) Then
  Begin
    RaiseError('File not found. ['+FileName+']');
    Exit;
  End;
  {$ENDIF}

  If StreamMode=0 Then
  Begin
    RaiseError('Invalid file mode. ['+FileName+']');
    Exit;
  End Else
  Begin
    _Offset := Offset;

    If (_Mode And smWrite <>0) Then
      FileMode := 2
    Else
      FileMode := 0;

    AssignFile(_File,_Name);
    Reset(_File,1);
    _Size := FileSize(_File);

    If (MaxSize>0) And (MaxSize<_Size) Then
      _Size := MaxSize;

    If _Offset>0 Then
    Begin
      Self.Seek(_Offset);
      _Pos := 0;
    End;

    _Open:=True;
  End;
End;

Destructor FileStream.Destroy;
Begin
  If Not _Open Then
    Exit;

  CloseFile(_File);
End;

Procedure FileStream.Delete;
Begin
  If Not _Open Then
    Exit;

  CloseFile(_File);
  Erase(_File);

  _Open := False;
End;

Procedure FileStream.Truncate;
Begin
  If Not _Open Then
    Exit;

  FileTruncate(_File);

  _Size:=_Pos;
End;

Procedure FileStream.Rename(NewName:AnsiString);
Begin
  If Not _Open Then
    Exit;

{$IFNDEF PC}
  RaiseError('File.Rename() not implemented!');
{$ELSE}
  _Name:=NewName;
  CloseFile(_File);
  Erase(_File);
  FileRename(_File,_Name);
  AssignFile(_File,_Name);
  Reset(_File,1);

  Seek(Position);
{$ENDIF}
End;

Function FileStream.Read(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  If (Length=0) Or (Not _Open) Then
  Begin
    Result := 0;
    Exit;
  End;

  If (_Mode And smRead=0) Or (_Pos>=_Size) Then
  Begin
    Result := 0;
    {$IFDEF PC}
    RaiseError('Cannot read from file: '+Self._Name+' ('+IntToString(_Pos)+'/'+IntToString(_Size)+')');
    Exit;
    {$ENDIF}
    FillChar(Data^, Length, 0);
    Exit;
  End;

  If (_Pos + Length > _Size)Then
    Length := _Size -_Pos;

  If (Length<=0) Then
  Begin
    Result:=0;
    Exit;
  End;
    
  BlockRead(_File, Data^, Length);

  Inc(_Pos, Length);
  Result:=Length;
End;

Function FileStream.Write(Data:Pointer; Length:Cardinal):Cardinal;
Begin
  {$IFDEF MOBILE}
  Log(logDebug, 'App', 'Writing to file! ' + Self._Name);
  {$ENDIF}

  Result := 0;
  If (Not _Open) Then
    Exit;

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

  BlockWrite(_File,Data^,Length);

  Inc(_Pos, Length);
  If _Pos>_Size Then
    _Size:=_Pos;

  Result := Length;
End;

Procedure FileStream.Seek(NewPosition:Cardinal);
Begin
  If _Pos>_Size Then
  Begin
    RaiseError('Cannot seek in file.['+_Name+']');
    Exit;
  End;
  _Pos := NewPosition;

  FileSeek(_File, _Pos+_Offset);
End;

Class procedure FileStream.CopyFile(SourceName, DestName:AnsiString);
Var
  Source, Dest:Stream;
Begin
  Source := MemoryStream.Create(SourceName);
  Dest := FileStream.Create(DestName);
  Source.Copy(Dest);
  Source.Destroy;
  Dest.Destroy;
End;

Procedure FileStream.Flush;
Begin
  CloseFile(_File);
  FileMode := 2;
  AssignFile(_File,_Name);
  Reset(_File,1);
  FileSeek(_File, _Pos+_Offset);
End;

End.
