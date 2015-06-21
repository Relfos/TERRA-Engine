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
 * TERRA_Callback
 * Implements callstack debug info (tested with Delphi 7 only, will provide empty info in other compilers)
 ***********************************************************************************************************************
}
Unit TERRA_Callstack;

{$I terra.inc}

Interface
Uses SysUtils;

Type
  CallInfo = Record
    Name:^String;
    Line:Cardinal;
  End;

Procedure GetCurrentCall(Var Info:CallInfo);

//Function TextualDebugInfoForAddress(Const TheAddress: Cardinal):TERRAString;

Function DumpCallstack:String;
Function DumpExceptionCallStack(E: Exception):String;

//Function GetLibraryAddress():Cardinal;

Var
  SettingCallStack:Boolean = False;

Implementation

//Uses TERRA_Utils, TERRA_String, TERRA_Stream, TERRA_FileStream
{$IFNDEF FPC}
//Uses Windows;
{$ENDIF}


Const
{$IFNDEF FPC}
  SkillCalls = 0;
  LineEnding = #13#10;
	StoredCallStackDepth = 26; 	// Size of the call stack we store when GetMem is called, must be an EVEN number
{$ELSE}
	StoredCallStackDepth = 20;
{$ENDIF}

Type
	CallStackArray = Array[0..StoredCallStackDepth] of Pointer;

Var
  _CurrentCallstack:CallStackArray;
  _CurrentCallstackSize:Integer;


(*Function GetLibraryAddress():Cardinal;
Const
  libname = 'libterra.so';
Var
  S, Range,Perms, Ofs, Dev, Inode, Path:TERRAString;
  StartPos, EndPos, OfsValue:Cardinal;
  Src:FileStream;
Begin
  Result := 0;

  Src := FileStream.Open('/proc/self/maps');

  While Not Src.EOF Do
  Begin
    Src.ReadLine(S);

    Range := StringGetNextSplit(S, Ord(' '));
    Perms := StringGetNextSplit(S, Ord(' '));
    Ofs := StringGetNextSplit(S, Ord(' '));
    Dev := StringGetNextSplit(S, Ord(' '));
    INode := StringGetNextSplit(S, Ord(' '));
    Path := StringGetNextSplit(S, Ord(' '));

    If StringContains(LibName, Path) Then
    Begin
      StartPos := StringToCardinal(StringGetNextSplit(Range, Ord('-')));
      OfsValue := StringToCardinal(Ofs);

      If (StringContainsChar(Ord('r'), Perms)) And (StringContainsChar(Ord('x'), Perms)) Then
      Begin
        Result := StartPos - OfsValue;
      End;
    End;
  End;

  Src.Release();
End;

Procedure AdjustCallstack();
Var
  BaseAddr:Cardinal;
  I:Integer;
Begin
  BaseAddr := GetLibraryAddress();

  For I:=0 To Pred(_CurrentCallstackSize) Do
    Dec(Cardinal(_CurrentCallstack[I]), BaseAddr);
End;*)
  
{$IFDEF DELPHI}
Type
	AddressToLine = Class
	  Public
		  Address: Cardinal;
		  Line: Cardinal;

		  Constructor Create(const AAddress, ALine: Cardinal);
	  End;

	UnitDebugInfos = Class
	  Public
  		Name:String;
	  	Addresses: array of AddressToLine;
      ShowOnCallstack:Boolean;

		  Constructor Create(const AName:String; const NbLines: Cardinal);

  		Function LineWhichContainsAddress(const Address: Cardinal):Integer;
	  End;

  RoutineDebugInfos = Class
	  Public
		  Name:String;
  		StartAddress: Cardinal;
	  	EndAddress: Cardinal;

		  Constructor Create(const AName:String; const AStartAddress: Cardinal; const ALength: Cardinal);
	  End;


Constructor UnitDebugInfos.Create(const AName:String; const NbLines: Cardinal);
Begin
	Name := AName;

  ShowOnCallstack := {(Pos('TERRA_', AName)>0) And} (Pos('Callstack', AName)<=0) And (Pos('MemoryManager', AName)<=0);

	SetLength(Addresses, NbLines);
End;

constructor RoutineDebugInfos.Create(const AName:String; const AStartAddress: Cardinal; const ALength: Cardinal);
Var
  I:Integer;
begin
	Name := Copy(AName, 2, MaxInt);
  Name := Copy(Name, Succ(Pos('@', Name)), MaxInt);

  For I:=1 To Length(Name) Do
  If (Name[I]='@') Then
    Name[I] := '.';

	StartAddress := AStartAddress;
	EndAddress := StartAddress + ALength - 1;
end;

constructor AddressToLine.Create(const AAddress, ALine: Cardinal);
begin
	Address := AAddress;
	Line := ALine
end;

function UnitDebugInfos.LineWhichContainsAddress(const Address: Cardinal):Integer;
var
	Start, Finish, Pivot: Cardinal;
begin
	if Addresses[0].Address > Address then
		Result := 0
	else
		begin
			Start := 0;
			Finish := Length(Addresses) - 1;

			while Start < Finish - 1 do
				begin
					Pivot := Start + (Finish - Start) div 2;

					if Addresses[Pivot].Address = Address then
						begin
							Start := Pivot;
							Finish := Start
						end
					else
						if Addresses[Pivot].Address > Address then
							Finish := Pivot
						else
							Start := Pivot
				end;

			Result := Addresses[Start].Line;
		end;
end;

Const
	MaxListSize = MaxInt div 16 - 1;

Type
	SRCMODHDR = packed record
		_cFile: Word;
		_cSeg: Word;
		_baseSrcFile: array[0..MaxListSize] of Integer;
	end;

	SRCFILE = packed record
		_cSeg: Word;
		_nName: Integer;
		_baseSrcLn: array[0..MaxListSize] of Integer;
	end;

	SRCLN = packed record
		_Seg: Word;
		_cPair: Word;
		_Offset: array[0..MaxListSize] of Integer;
	end;

	PSRCMODHDR = ^SRCMODHDR;
	PSRCFILE = ^SRCFILE;
	PSRCLN = ^SRCLN;

	TArrayOfByte = array[0..MaxListSize] of Byte;
	TArrayOfWord = array[0..MaxListSize] of Word;
	PArrayOfByte = ^TArrayOfByte;
	PArrayOfWord = ^TArrayOfWord;
	PArrayOfPointer = ^TArrayOfPointer;
	TArrayOfPointer = array[0..MaxListSize] of PArrayOfByte;

Var
	Routines: array of RoutineDebugInfos;
	RoutinesCount: integer;
	Units: array of UnitDebugInfos;
	UnitsCount: integer;

Function Max(A,B:Integer):Integer;
Begin
  If (A>B) Then
    Result := A
  Else
    Result := B;
End;

procedure AddRoutine(const Name:String; const Start, Len: Cardinal);
begin
	if Length(Routines) <= RoutinesCount then
		SetLength(Routines, Max(RoutinesCount * 2, 1000));

	Routines[RoutinesCount] := RoutineDebugInfos.Create(Name, Start, Len);
	RoutinesCount := RoutinesCount + 1;
end;

procedure AddUnit(const U: UnitDebugInfos);
begin
	if Length(Units) <= UnitsCount then
		SetLength(Units, Max(UnitsCount * 2, 1000));

	Units[UnitsCount] := U;
	UnitsCount := UnitsCount + 1;
end;

procedure dumpsymbols(NameTbl: PArrayOfPointer; sstptr: PArrayOfByte; size: integer);
//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
var
	len, sym: integer;
begin
	while size > 0 do
		begin
			len := PWord(@sstptr^[0])^;
			sym := PWord(@sstptr^[2])^;

			INC(len, 2);

			if ((sym = $205) or (sym = $204)) and (PInteger(@sstptr^[40])^ > 0) then
				AddRoutine(PAnsiChar(NameTbl^[PInteger(@sstptr^[40])^ - 1]), PInteger(@sstptr^[28])^, PInteger(@sstptr^[16])^);

			if (len = 2) then
				size := 0
			else
				begin
					sstptr := PArrayOfByte(@sstptr^[len]);
					DEC(size, len);
				end;
		end;
end;

Const
  MAX_PATH = 260;

  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;
  IMAGE_SIZEOF_FILE_HEADER         = 20;

Type
  PImageFileHeader = ^TImageFileHeader;
  TImageFileHeader = Packed Record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp:Cardinal;
    PointerToSymbolTable:Cardinal;
    NumberOfSymbols:Cardinal;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  End;

  PImageDataDirectory = ^TImageDataDirectory;
  TImageDataDirectory = record
    VirtualAddress:Cardinal;
    Size:Cardinal;
  End;

  PImageOptionalHeader = ^TImageOptionalHeader;
  TImageOptionalHeader = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: Cardinal;
    SizeOfInitializedData: Cardinal;
    SizeOfUninitializedData: Cardinal;
    AddressOfEntryPoint: Cardinal;
    BaseOfCode: Cardinal;
    BaseOfData: Cardinal;
    { NT additional fields. }
    ImageBase: Cardinal;
    SectionAlignment: Cardinal;
    FileAlignment: Cardinal;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: Cardinal;
    SizeOfImage: Cardinal;
    SizeOfHeaders: Cardinal;
    CheckSum: Cardinal;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Cardinal;
    SizeOfStackCommit: Cardinal;
    SizeOfHeapReserve: Cardinal;
    SizeOfHeapCommit: Cardinal;
    LoaderFlags: Cardinal;
    NumberOfRvaAndSizes: Cardinal;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  End;

  PImageDosHeader = ^TImageDosHeader;
  TImageDosHeader = packed record        { DOS .EXE header                  }
      e_magic: Word;                     { Magic number                     }
      e_cblp: Word;                      { Bytes on last page of file       }
      e_cp: Word;                        { Pages in file                    }
      e_crlc: Word;                      { Relocations                      }
      e_cparhdr: Word;                   { Size of header in paragraphs     }
      e_minalloc: Word;                  { Minimum extra paragraphs needed  }
      e_maxalloc: Word;                  { Maximum extra paragraphs needed  }
      e_ss: Word;                        { Initial (relative) SS value      }
      e_sp: Word;                        { Initial SP value                 }
      e_csum: Word;                      { Checksum                         }
      e_ip: Word;                        { Initial IP value                 }
      e_cs: Word;                        { Initial (relative) CS value      }
      e_lfarlc: Word;                    { File address of relocation table }
      e_ovno: Word;                      { Overlay number                   }
      e_res: array [0..3] of Word;       { Reserved words                   }
      e_oemid: Word;                     { OEM identifier (for e_oeminfo)   }
      e_oeminfo: Word;                   { OEM information; e_oemid specific}
      e_res2: array [0..9] of Word;      { Reserved words                   }
      _lfanew: LongInt;                  { File address of new exe header   }
  End;

Var
	Displ: Cardinal;
	{Displ is the displacement of the code in the executable file. The code in SetDispl was written by Juan Vidal Pich}

procedure SetDispl;
var
	NTHeader: PImageFileHeader;
	NTOptHeader: PImageOptionalHeader;
begin
	//-> If you have a compilation error in this routine and you are compiling with delphi 4, I'd say that you did not install the Delphi update pack 3

	NTHeader := PImageFileHeader(Cardinal(PImageDosHeader(HInstance)._lfanew) + HInstance + 4); {SizeOf(IMAGE_NT_SIGNATURE) = 4}
	NTOptHeader := PImageOptionalHeader(Cardinal(NTHeader) + IMAGE_SIZEOF_FILE_HEADER);
	Displ := HInstance + NTOptHeader.BaseOfCode;
	//Result := HInstance + PImageNtHeaders(LongInt(HInstance)+PImageDosHeader(HInstance)^._lfanew)^.OptionalHeader.BaseOfCode;
end;

Const
  PathSeparator = '\';
  
Function ExtractFileName(FileName:String):String;
Var
  I, Len:Integer;
Begin
  Len := Length(FileName);
  For I:=Len DownTo 1 Do
  If (FileName[I] = PathSeparator) Then
  Begin
    Result := Copy(FileName, Succ(I), MaxInt);
    Exit;
  End;

  Result := FileName;
End;

procedure dumplines(NameTbl: PArrayOfPointer; sstptr: PArrayOfByte; size: integer);
//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
var
	srcmodhdr: PSRCMODHDR;
	i: Word;
	srcfile: PSRCFILE;
	srcln: PSRCLN;
	k: Word;
	CurrentUnit: UnitDebugInfos;
begin
	if size > 0 then
		begin
			srcmodhdr := PSRCMODHDR(sstptr);

			for i := 0 to pred(srcmodhdr^._cFile) do
				begin
					srcfile := PSRCFILE(@sstptr^[srcmodhdr^._baseSrcFile[i]]);

					if srcfile^._nName > 0 then
						//note: I assume that the code is always in segment #1. If this is not the case, Houston !  - VM
						begin
							srcln := PSRCLN(@sstptr^[srcfile^._baseSrcLn[0]]);

							CurrentUnit := UnitDebugInfos.Create(ExtractFileName(PAnsiChar(NameTbl^[srcfile^._nName - 1])), srcln^._cPair);
							AddUnit(CurrentUnit);

							for k := 0 to pred(srcln^._cPair) do
								CurrentUnit.Addresses[k] := AddressToLine.Create(Integer(PArrayOfPointer(@srcln^._Offset[0])^[k]), Integer(PArrayOfWord(@srcln^._Offset[srcln^._cPair])^[k]));
						end;
				end;
		end;
end;

Function GetModuleFileName(hModule: HINST; lpFilename: PAnsiChar; nSize: Cardinal):Cardinal; stdcall; External 'Kernel32.dll' Name 'GetModuleFileNameA';

Var
  InfoInitialized:Boolean = False;

Procedure GetProjectInfos();
//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
var
	AHeader: packed record
		Signature: array[0..3] Of AnsiChar;
		AnInteger: Integer;
	end;
	k: integer;
	j: Word;
	lfodir: Integer;
	SstFrameSize: integer;
	SstFrameElem: PArrayOfByte;
	ssttype, sstsize, sstbase: Integer;
	x, y, z: Integer;
	sstbuf: PArrayOfByte;
	OldFileMode: integer;
	AFileOfByte: file of Byte;
	Names: PArrayOfByte;
	NameTbl: PArrayOfPointer;
	SstFrame: PArrayOfByte;
	ifabase: Integer;
	cdir, cbdirentry: word;
	FileName:String;
  DebugOffset:Integer;
  I, Size:Integer;
  Id:Integer;
  Buf:Array Of AnsiChar;
begin
  If InfoInitialized Then
    Exit;

  InfoInitialized := True;

  SetDispl;

	RoutinesCount := 0;
	UnitsCount := 0;
	OldFileMode := FileMode;
	FileMode := 0;
	SetLength(FileName, MAX_PATH + 1);
	SetLength(FileName, GetModuleFileName(HInstance, PAnsiChar(FileName), MAX_PATH));
	AssignFile(AFileOfByte, FileName);
	Reset(AFileOfByte);
	Names := nil;
	NameTbl := nil;

  Size := FileSize(AFileOfByte);
  SetLength(Buf, Size);
  BlockRead(AFileOfByte, Buf[0], Size);
  ID := 4;
  DebugOffset := -1;
  I := Pred(Size);
  Repeat
    Case ID Of
    4:  If (Buf[I]='9') Or (Buf[I]='A') Then
          Dec(ID);
    3:  If (Buf[I]='0') Then
          Dec(ID)
        Else
          ID := 4;
    2:  If (Buf[I]='B') Then
          Dec(ID)
        Else
          ID := 4;
    1:  If (Buf[I]='F') Then
        Begin
          DebugOffset := I;
          Break;
        End Else
          ID := 4;
    End;
    Dec(I);
  Until (I<0);

  SetLength(Buf, 0);
  If DebugOffset >= 0 Then
  Begin
	Seek(AFileOfByte, DebugOffset);
	BlockRead(AFileOfByte, AHeader, SizeOf(AHeader));
	if (AHeader.Signature = 'FB09') or (AHeader.Signature = 'FB0A') then
		begin
			ifabase := FilePos(AFileOfByte) - AHeader.AnInteger;
			Seek(AFileOfByte, ifabase);
			BlockRead(AFileOfByte, AHeader, SizeOf(AHeader));
			if (AHeader.Signature = 'FB09') or (AHeader.Signature = 'FB0A') then
				begin
					lfodir := ifabase + AHeader.AnInteger;
					if lfodir >= ifabase then
						begin
							Seek(AFileOfByte, lfodir);
							BlockRead(AFileOfByte, j, SizeOf(Word));
							BlockRead(AFileOfByte, cbdirentry, SizeOf(Word));
							BlockRead(AFileOfByte, cdir, SizeOf(Word));
							Seek(AFileOfByte, lfodir + j);
							SstFrameSize := cdir * cbdirentry;
							getmem(SstFrame, SstFrameSize);
							BlockRead(AFileOfByte, SstFrame^, SstFrameSize);
							for k := 0 to pred(cdir) do
								begin
									SstFrameElem := PArrayOfByte(@SstFrame^[k * cbdirentry]);
									ssttype := PWord(@SstFrameElem^[0])^;
									if (ssttype = $0130) then
										begin
											sstbase := ifabase + PInteger(@SstFrameElem^[4])^;
											sstsize := PInteger(@SstFrameElem^[8])^;
											getmem(Names, sstsize);
											Seek(AFileOfByte, sstbase);
											BlockRead(AFileOfByte, Names^, sstsize);
											y := PInteger(@Names^[0])^;
											getmem(NameTbl, sizeof(Pointer) * y);
											z := 4;
											for x := 0 to pred(y) do
												begin
													NameTbl^[x] := PArrayOfByte(@Names^[z + 1]);
													z := z + Names^[z] + 2;
												end;
										end;
								end;
							for k := 0 to pred(cdir) do
								begin
									SstFrameElem := PArrayOfByte(@SstFrame^[k * cbdirentry]);
									ssttype := PWord(@SstFrameElem^[0])^;
									sstbase := ifabase + PInteger(@SstFrameElem^[4])^;
									sstsize := PInteger(@SstFrameElem^[8])^;
									getmem(sstbuf, sstsize);
									Seek(AFileOfByte, sstbase);
									BlockRead(AFileOfByte, sstbuf^, sstsize);
									if (ssttype = $0125) then
										dumpsymbols(NameTbl, PArrayOfByte(@sstbuf^[4]), sstsize - 4);
									if (ssttype = $0127) then
										dumplines(NameTbl, sstbuf, sstsize);
									FreeMem(sstbuf);
								end;
							FreeMem(Names);
							FreeMem(NameTbl);
							FreeMem(SstFrame);
						end;
				end;
		end;
  End;
	CloseFile(AFileOfByte);
	FileMode := OldFileMode;
End;

Function UnitWhichContainsAddress(const Address: Cardinal):UnitDebugInfos;
var
	Start, Finish, Pivot: integer;
begin
	Start := 0;
	Finish := UnitsCount - 1;
	Result := nil;

  If (UnitsCount<=0) Then
    Exit;

	while Start <= Finish do
		begin
			Pivot := Start + (Finish - Start) div 2;

			if (Pivot>=Length(Units)) Or (Length(UnitDebugInfos(Units[Pivot]).Addresses)<=0) Then
        Break
      Else
      If (UnitDebugInfos(Units[Pivot]).Addresses[0].Address > Address) then
				Finish := Pivot - 1
			else
				if UnitDebugInfos(Units[Pivot]).Addresses[Length(UnitDebugInfos(Units[Pivot]).Addresses) - 1].Address < Address then
					Start := Pivot + 1
				else
					begin
						Result := Units[Pivot];
						Start := Finish + 1;
					end;
		end;
end;

Function RoutineWhichContainsAddress(const Address: Cardinal):RoutineDebugInfos;
Var
	Start, Finish, Pivot: integer;
Begin
	Start := 0;
	Result := Nil;
	Finish := RoutinesCount - 1;

	while Start <= Finish do
		begin
			Pivot := Start + (Finish - Start) div 2;

			if RoutineDebugInfos(Routines[Pivot]).StartAddress > Address then
				Finish := Pivot - 1
			else
				if RoutineDebugInfos(Routines[Pivot]).EndAddress < Address then
					Start := Pivot + 1
				else
					begin
						Result := RoutineDebugInfos(Routines[Pivot]);
						Start := Finish + 1;
					end;
		end;
End;

Function BackTraceStrFunc(Const TargetAddress:Pointer):String;
var
	U:UnitDebugInfos;
  R:RoutineDebugInfos;
	AddressInDebugInfos: Cardinal;
  Num:String;
  TheAddress:Cardinal;
begin
	If UnitsCount<=0 Then
    GetProjectInfos();

  TheAddress := Cardinal(TargetAddress);

	If (TheAddress > Displ) then
  Begin
    AddressInDebugInfos := TheAddress - Displ;

		U := UnitWhichContainsAddress(AddressInDebugInfos);
    R := RoutineWhichContainsAddress(AddressInDebugInfos);

    If Assigned(R) Then
    Begin
      Result := R.Name;
      If Assigned(U) Then
      Begin
        Str(U.LineWhichContainsAddress(AddressInDebugInfos), Num);
        Result := Result + ' (line '+Num+' of '+U.Name+')';
      End;
      Exit;
    End;
  End;


  Str(TheAddress, Num);
	Result := Num + ' (No debug info)';
End;

Procedure FillCallStackFromAddress(StackStart, StackMax:Pointer);
	{Works only with stack frames - Without, St contains correct info, but is not as deep as it should
	I just don't know a general rule for walking the stack when they are not there}
Var
	//the stack can never go beyond - stackmax http://msdn.microsoft.com/library/periodic/period96/S2CE.htm
	CurrentFrame:Pointer;
	Count:Integer;
begin
	FillChar(_CurrentCallstack, SizeOf(_CurrentCallstack), 0);

	CurrentFrame := StackStart;
	Count := 0;

	While (longint(CurrentFrame) >= longint(StackStart)) and (longint(CurrentFrame) < longint(StackMax)) and (Count <= StoredCallStackDepth) Do
  Begin
    _CurrentCallstack[Count] := Pointer(PInteger(longint(CurrentFrame) + 4)^ - 4);
		Inc(Count);

    CurrentFrame := Pointer(PInteger(CurrentFrame)^);
  End;

  _CurrentCallstackSize := Count;
End;

Procedure FillCurrentCallStack();
Var
  StackStart, StackMax:Pointer;
Begin
	Asm
		mov EAX, FS:[4]
		mov StackMax, EAX
		mov StackStart, EBP
	End;

  FillCallStackFromAddress(StackStart, StackMax);
End;

Procedure GetCurrentCall(Var Info:CallInfo);
Var
	I, Count:Integer;
  TheAddress:Cardinal;
	AddressInDebugInfos: Cardinal;
	U:UnitDebugInfos;
  Routine:RoutineDebugInfos;
Begin
  SettingCallStack := True;
	If (UnitsCount<=0) Then
    GetProjectInfos();

  FillCurrentCallstack();
  SettingCallStack := False;

	I := 0;
	Info.Name := Nil;
  Info.Line := 0;

  Count := SkillCalls;

	While (i <= StoredCallStackDepth) and (_CurrentCallstack[i] <> nil) Do
  Begin
    TheAddress := Cardinal(_CurrentCallstack[i]);

	  If (TheAddress > Displ) Then
    Begin
      AddressInDebugInfos := TheAddress - Displ;

		  U := UnitWhichContainsAddress(AddressInDebugInfos);

      If (Assigned(U)) And (U.ShowOnCallstack) Then
      Begin
        Routine := RoutineWhichContainsAddress(AddressInDebugInfos);
        If Assigned(Routine) Then
        Begin
          Dec(Count);
          Info.Name := @Routine.Name;
          Info.Line := U.LineWhichContainsAddress(AddressInDebugInfos);
          If (Count<0) Then
            Exit;
        End;
      End;
    End;

		Inc(I);
  End;
End;

Procedure FillExceptionCallStack(E:Exception);
Begin
  FillCurrentCallStack();
End;
{$ELSE}

Procedure GetCurrentCall(Var Info:CallInfo);
Begin
	Info.Name := Nil;
  Info.Line := 0;
End;

Procedure FillCurrentCallStack();
Var
  I:Integer;
  prevbp: Pointer;
  CallerFrame, CallerAddress, bp:Pointer;
Begin
  _CurrentCallstackSize := 0;
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  Try
    prevbp := bp - 1;
    I := 0;
    While bp > prevbp Do
    Begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;

       _CurrentCallstack[_CurrentCallstackSize] := CallerAddress;
       Inc(_CurrentCallstackSize);

       Inc(I);

       if (I >= StoredCallStackDepth) or (CallerFrame = nil) then
         Break;

       prevbp := bp;
       bp := CallerFrame;
     End;

    {$IFDEF ANDROID}
    //AdjustCallstack();
    {$ENDIF}
   Except
     // prevent endless dump if an exception occured
   End;
End;

Procedure FillExceptionCallStack(E:Exception);
Var
  I:Integer;
  Frames: PPointer;
Begin
  _CurrentCallstack[0] := ExceptAddr;

  Frames := ExceptFrames;
  I := 0;
  While (I<ExceptFrameCount) And (Succ(I)<StoredCallStackDepth) Do
  Begin
    _CurrentCallstack[Succ(I)] := Frames[I];
    Inc(I);
  End;

  {$IFDEF ANDROID}
  //AdjustCallstack();
  {$ENDIF}

  _CurrentCallstackSize := Succ(I);
End;
{$ENDIF}

Function GetCallstackString():String;
Var
  I:Integer;
Begin
  If _CurrentCallstackSize<=0 Then
  Begin
    Result := 'No call stack info present.';
    Exit;
  End;

  Result := '';
  For I:=0 To Pred(_CurrentCallstackSize) Do
    Result := Result + BackTraceStrFunc(_CurrentCallstack[I]) + LineEnding;
End;

Function DumpCallstack:String;
Begin
  FillCurrentCallStack();
  Result := GetCallstackString();
End;

Function DumpExceptionCallStack(E: Exception):String;
Begin
  FillExceptionCallStack(E);
  Result := GetCallstackString();
End;



End.
