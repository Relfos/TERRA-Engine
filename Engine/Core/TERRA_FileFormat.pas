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
 * TERRA_FileFormats
 * Implements a list of file formats
 ***********************************************************************************************************************
}

Unit TERRA_FileFormat;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Stream, TERRA_FileManager;

Type
  TERRAFileFormat = Class(TERRAObject)
    Protected
      _Extension:TERRAString;
      _Kind:TERRAObjectType;

      Function Identify(Source:TERRAStream):Boolean; Virtual;

    Public
      Constructor Create(Const Kind:TERRAObjectType; Const Extension:TERRAString);

      Function LoadFromStream(Target:TERRAObject; Source:TERRAStream):Boolean; Virtual;
      Procedure LoadFromString(Target:TERRAObject; Const Data:TERRAString; Encoding:StringEncoding);
      Procedure LoadFromLocation(Target:TERRAObject; Location:TERRALocation; Encoding:StringEncoding = encodingUnknown);
      Procedure LoadFromFile(Target:TERRAObject; Const FileName:TERRAString; Encoding:StringEncoding = encodingUnknown);

      Function SaveToStream(Target:TERRAObject; Dest:TERRAStream):Boolean; Virtual;
      Procedure SaveToFile(Target:TERRAObject; Const FileName:TERRAString);
      Function SaveToString(Target:TERRAObject; Encoding:StringEncoding):TERRAString;


      Property Extension:TERRAString Read _Extension;
      Property Kind:TERRAObjectType Read _Kind;
  End;


  FormatManager = Class(TERRAObject)
    Public
      _Formats:Array Of TERRAFileFormat;
      _FormatCount:Integer;

    Public
      Procedure Add(Format:TERRAFileFormat);
      Function GetFormatByIndex(Index:Integer):TERRAFileFormat;

      Function FindLocationFromName(Const Name:TERRAString; Kind:TERRAObjectType; Out Location:TERRALocation):TERRAFileFormat;
      Function FindFormatFromExtension(Const Ext:TERRAString):TERRAFileFormat;
      Function FindFormatFromStream(Source:TERRAStream; Kind:TERRAObjectType):TERRAFileFormat;

      Property FormatCount:Integer Read _FormatCount;

      Property Formats[Const Name:TERRAString]:TERRAFileFormat Read FindFormatFromExtension; Default;
  End;

Implementation
Uses TERRA_Engine, TERRA_FileUtils, TERRA_FileStream, TERRA_MemoryStream;

{ TERRAFileFormat }
Constructor TERRAFileFormat.Create(Const Kind:TERRAObjectType; Const Extension:TERRAString);
Begin
  _Kind := Kind;
  _Extension := Extension;
End;

Function TERRAFileFormat.Identify(Source: TERRAStream): Boolean;
Begin
  Result := StringEquals(GetFileExtension(Source.Name), Self.Extension);
End;

Function TERRAFileFormat.LoadFromStream(Target:TERRAObject; Source:TERRAStream):Boolean;
Begin
  Result := False;
End;

Function TERRAFileFormat.SaveToStream(Target:TERRAObject; Dest:TERRAStream):Boolean;
Begin
  Result := False;
End;

Procedure TERRAFileFormat.SaveToFile(Target:TERRAObject; Const FileName:TERRAString);
Var
  Dest:FileStream;
Begin
  Dest := FileStream.Create(FileName);
  SaveToStream(Target, Dest);
  ReleaseObject(Dest);
End;

Function TERRAFileFormat.SaveToString(Target:TERRAObject; Encoding:StringEncoding):TERRAString;
Var
  Dest:MemoryStream;
Begin
  Dest := MemoryStream.Create();
  SaveToStream(Target, Dest);
  SetLength(Result, Dest.Size);
  Move(Dest.Buffer^, Result[1], Dest.Size);
  ReleaseObject(Dest);
End;

Procedure TERRAFileFormat.LoadFromFile(Target:TERRAObject; Const FileName:TERRAString; Encoding:StringEncoding);
Var
  Source:FileStream;
Begin
  Source := FileStream.Open(FileName);

  If Encoding <> encodingUnknown Then
    Source.Encoding := Encoding;

  LoadFromStream(Target, Source);
  ReleaseObject(Source);
End;

Procedure TERRAFileFormat.LoadFromString(Target:TERRAObject; Const Data:TERRAString; Encoding:StringEncoding);
Var
  Source:MemoryStream;
Begin
  Source := MemoryStream.Create(Length(Data), @Data[1]);
  Source.Encoding := Encoding;
  LoadFromStream(Target, Source);
  ReleaseObject(Source);
End;

Procedure TERRAFileFormat.LoadFromLocation(Target: TERRAObject; Location: TERRALocation; Encoding: StringEncoding);
Var
  Source:TERRAStream;
Begin
  Source := Location.GetStream();
  If Assigned(Source) Then
  Begin
    LoadFromStream(Target, Source);
    ReleaseObject(Source);
  End;
End;

{ FormatManager }
Procedure FormatManager.Add(Format:TERRAFileFormat);
Var
  I:Integer;
Begin
  If Format = Nil Then
    Exit;

  For I:=0 To Pred(_FormatCount) Do
  If (_Formats[I].ClassType = Format.ClassType) Then
    Exit;

  Inc(_FormatCount);
  SetLength(_Formats, _FormatCount);
  _Formats[Pred(_FormatCount)] := Format;
End;

Function FormatManager.GetFormatByIndex(Index:Integer):TERRAFileFormat;
Begin
  If (Index<0) Or (Index>=_FormatCount) Then
    Result := Nil
  Else
    Result := _Formats[Index];
End;

Function FormatManager.FindLocationFromName(Const Name:TERRAString; Kind:TERRAObjectType; Out Location:TERRALocation):TERRAFileFormat;
Var
  I:Integer;
Begin
  Result := Nil;
  Location := Nil;
  For I:=0 To Pred(_FormatCount) Do
  If (_Formats[I].Kind = Kind) Then
  Begin
    Location := Engine.Files.Search(Name + '.' + _Formats[I].Extension);
    If (Assigned(Location)) Then
    Begin
      Result := _Formats[I];
      Exit;
    End;
  End;
End;

Function FormatManager.FindFormatFromStream(Source:TERRAStream; Kind:TERRAObjectType):TERRAFileFormat;
Var
  I:Integer;
  Ofs:Cardinal;
  Ok:Boolean;
Begin
  Ofs := Source.Position;
  Result := Nil;

  For I:=0 To Pred(_FormatCount) Do
  If (_Formats[I].Kind = Kind) Then
  Begin
    Ok := _Formats[I].Identify(Source);
    Source.Seek(Ofs);
    If (Ok) Then
    Begin
      Result := _Formats[I];
      Break;
    End;
  End;
End;

Function FormatManager.FindFormatFromExtension(Const Ext:TERRAString): TERRAFileFormat;
Var
  I:Integer;
Begin
  Result := Nil;
  For I:=0 To Pred(_FormatCount) Do
  If (StringEquals(_Formats[I].Extension, Ext)) Then
  Begin
    Result := _Formats[I];
    Exit;
  End;
End;

End.
