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
 * TERRA_FileStream
 * Implements a generic file input stream
 ***********************************************************************************************************************
}

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
            • Added Delete() Procedure to LFileStream
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
Unit TERRA_FileStream;

{$I terra.inc}

Interface
Uses TERRA_Stream, TERRA_Object, TERRA_FileUtils, TERRA_String;

Type
  FilePointer=File;

  FileStream=Class(Stream)
     Protected
        _File:FilePointer;
        _Open:Boolean;
     Public

        Constructor Create(FileName:TERRAString; StreamMode:Integer=smDefault);Overload;
        Constructor Open(FileName:TERRAString; StreamMode:Integer=smDefault);
        Procedure Release;Override;
        Procedure Delete;
        Procedure Rename(NewName:TERRAString);
        Procedure Truncate;Override;
        Function Read(Data:Pointer; Length:Cardinal):Cardinal; Override;
        Function Write(Data:Pointer; Length:Cardinal):Cardinal; Override;
        Procedure Seek(NewPosition:Cardinal);Override;

        Procedure Flush;

        Class Function Exists(FileName:TERRAString): Boolean;
        Class Procedure CopyFile(SourceName, DestName:TERRAString);
     End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Application, TERRA_Utils, TERRA_FileManager, TERRA_MemoryStream, SysUtils;

{$I-}

Procedure FileSeek(Var F:FilePointer; Offset:Integer);
Begin
  System.Seek(F, Offset);
End;

Procedure FileTruncate(Var F:FilePointer);
Begin
  System.Truncate(F);
End;


Procedure FileRename(Var F:FilePointer; Name:TERRAString);
Begin
  System.Rename(F, Name);
End;

{$IFDEF WINDOWS}
Class Function FileStream.Exists(FileName:TERRAString): Boolean;
Begin
  Log(logDebug, 'File', 'Testing file '+FileName);
  FileMode := 0;
  FileName := GetOSIndependentFileName(FileName);
  Result:=(FileAge(FileName)<>-1);
End;
{$ELSE}
Class Function FileStream.Exists(FileName:TERRAString): Boolean;
Begin
  {$IFDEF DEBUG_FILECACHE}Log(logDebug, 'FileManager', 'Testing for file '+FileName);{$ENDIF}

  FileName := GetOSIndependentFileName(FileName);
  Result := FileExists(FileName);
End;
{$ENDIF}


{**************************
   TFileStream Object
 **************************}

Constructor FileStream.Create(FileName:TERRAString; StreamMode:Integer=smDefault);
Begin
  Inherited Create(StreamMode);

  FileName := GetOSIndependentFileName(FileName);
  FileManager.Instance.RemoveFromCache(FileName);

  Log(logDebug,'IO','Creating '+FileName);
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

Constructor FileStream.Open(FileName:TERRAString; StreamMode:Integer=smDefault);
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
    If (_Mode And smWrite <>0) Then
      FileMode := 2
    Else
      FileMode := 0;

    IOResult();

  Log(logDebug,'IO','Opening '+FileName);
    AssignFile(_File,_Name);
    Reset(_File,1);
    _Size := FileSize(_File);

    _Open:=True;
  End;
End;

Procedure FileStream.Release;
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

Procedure FileStream.Rename(NewName:TERRAString);
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
    Log(logWarning, 'IO', 'Cannot read from file: '+Self._Name+' ('+IntToString(_Pos)+'/'+IntToString(_Size)+')');
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

  FileSeek(_File, _Pos);
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
  CloseFile(_File);
  FileMode := 2;
  AssignFile(_File,_Name);
  Reset(_File,1);
  FileSeek(_File, _Pos);
End;

End.
