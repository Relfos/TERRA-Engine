Unit TERRA_DebugInfo;

{$I terra.inc}

Interface

Type
	AddressToLine = Class
	  Public
		  Address:Cardinal;
		  Line:Cardinal;

		  Constructor Create(const AAddress, ALine: Cardinal);
	  End;

        { TERRAUnitInfo }

 TERRAUnitInfo = Class
    Protected
  		_Name:String;
	  	_Addresses:Array Of AddressToLine;
      _AddressCount:Integer;
      _ShowOnCallstack:Boolean;

	  Public
		  Constructor Create(Const UnitName:String; Const LineCount:Cardinal);

  		Function FindLineWithAddress(const Address: Cardinal):Integer;

  		Property Name:String Read _Name;
      Property ShowOnCallstack:Boolean Read _ShowOnCallstack;
	  End;

  TERRARoutineInfo = Class
    Protected
		  _Name:String;
  		_StartAddress: Cardinal;
	  	_EndAddress: Cardinal;

	  Public
		  Constructor Create(const RoutineName:String; Const FirstAddress: Cardinal; Const RoutineSize:Cardinal);

      Property Name:String Read _Name;
	  End;

Var
	CodeDispl: Cardinal;
	// Displ is the displacement of the code in the executable file. The code in SetDispl was written by Juan Vidal Pich

Function FindUnitWithAddress(Const Address:Cardinal):TERRAUnitInfo;
Function FindRoutineWithAddress(Const Address:Cardinal):TERRARoutineInfo;

Procedure InitProjectInfos();

Implementation

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

  ReleaseObject(Src);
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

constructor TERRAUnitInfo.Create(const UnitName: String;
  const LineCount: Cardinal);
Begin
	_Name := UnitName;
  _ShowOnCallstack := {(Pos('TERRA_', AName)>0) And} (Pos('Callstack', UnitName)<=0) And (Pos('MemoryManager', UnitName)<=0);
  _AddressCount := LineCount;
	SetLength(_Addresses, _AddressCount);
End;

constructor TERRARoutineInfo.Create(Const RoutineName:String; const FirstAddress:Cardinal; Const RoutineSize:Cardinal);
Var
  I:Integer;
begin
	_Name := Copy(RoutineName, 2, MaxInt);
  _Name := Copy(_Name, Succ(Pos('@', _Name)), MaxInt);

  For I:=1 To Length(_Name) Do
  If (_Name[I]='@') Then
    _Name[I] := '.';

	Self._StartAddress := FirstAddress;
	Self._EndAddress := Self._StartAddress + RoutineSize - 1;
end;

constructor AddressToLine.Create(const AAddress, ALine: Cardinal);
begin
	Address := AAddress;
	Line := ALine
end;

{$IFNDEF FPC}
function TERRAUnitInfo.FindLineWithAddress(const Address: Cardinal):Integer;
Var
	Start, Finish, Pivot: Cardinal;
Begin
  Result := 0;

	If _Addresses[0].Address > Address Then
    Exit;

  Start := 0;
	Finish := _AddressCount - 1;

	While Start < Finish - 1 Do
  Begin
    Pivot := Start + (Finish - Start) Shr 1;

		If _Addresses[Pivot].Address = Address Then
    Begin
      Start := Pivot;
			Finish := Start;
    End Else
    If _Addresses[Pivot].Address > Address Then
      Finish := Pivot
    Else
      Start := Pivot
  End;

  Result := _Addresses[Start].Line;
End;

Const
	MaxListSize = (MaxInt div 16) - 1;

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
	Routines: array of TERRARoutineInfo;
	RoutinesCount: integer;
	Units: array of TERRAUnitInfo;
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

	Routines[RoutinesCount] := TERRARoutineInfo.Create(Name, Start, Len);
	RoutinesCount := RoutinesCount + 1;
end;

procedure AddUnit(const U: TERRAUnitInfo);
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

procedure SetDispl;
var
	NTHeader: PImageFileHeader;
	NTOptHeader: PImageOptionalHeader;
begin
	//-> If you have a compilation error in this routine and you are compiling with delphi 4, I'd say that you did not install the Delphi update pack 3

	NTHeader := PImageFileHeader(Cardinal(PImageDosHeader(HInstance)._lfanew) + HInstance + 4); {SizeOf(IMAGE_NT_SIGNATURE) = 4}
	NTOptHeader := PImageOptionalHeader(Cardinal(NTHeader) + IMAGE_SIZEOF_FILE_HEADER);
	CodeDispl := HInstance + NTOptHeader.BaseOfCode;
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

Procedure dumplines(NameTbl: PArrayOfPointer; sstptr: PArrayOfByte; size: integer);
Var
	srcmodhdr: PSRCMODHDR;
	i: Word;
	srcfile: PSRCFILE;
	srcln: PSRCLN;
	k: Word;
	CurrentUnit: TERRAUnitInfo;
Begin
	If size > 0 Then
  Begin
    srcmodhdr := PSRCMODHDR(sstptr);

		For I := 0 To (srcmodhdr^._cFile - 1) Do
    Begin
      srcfile := PSRCFILE(@sstptr^[srcmodhdr^._baseSrcFile[i]]);

			If srcfile^._nName > 0 Then //note: I assume that the code is always in segment #1. If this is not the case, Houston !  - VM
      Begin
        srcln := PSRCLN(@sstptr^[srcfile^._baseSrcLn[0]]);

				CurrentUnit := TERRAUnitInfo.Create(ExtractFileName(PAnsiChar(NameTbl^[srcfile^._nName - 1])), srcln^._cPair);
				AddUnit(CurrentUnit);

				For k := 0 To (srcln^._cPair - 1) Do
				  CurrentUnit._Addresses[k] := AddressToLine.Create(Integer(PArrayOfPointer(@srcln^._Offset[0])^[k]), Integer(PArrayOfWord(@srcln^._Offset[srcln^._cPair])^[k]));
      End;
    End;
  End;
End;

Function GetModuleFileName(hModule: HINST; lpFilename: PAnsiChar; nSize: Cardinal):Cardinal; stdcall; External 'Kernel32.dll' Name 'GetModuleFileNameA';

Var
  InfoInitialized:Boolean = False;

Procedure InitProjectInfos();
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

Function FindUnitWithAddress(Const Address:Cardinal):TERRAUnitInfo;
Var
	Start, Finish, Pivot: integer;
Begin
	Start := 0;
	Finish := UnitsCount - 1;
	Result := nil;

  If (UnitsCount<=0) Then
    Exit;

	While Start <= Finish Do
  Begin
    Pivot := Start + (Finish - Start) div 2;

		If (Pivot>=Length(Units)) Or (Length(TERRAUnitInfo(Units[Pivot])._Addresses)<=0) Then
      Break
    Else
    If (TERRAUnitInfo(Units[Pivot])._Addresses[0].Address > Address) then
		  Finish := Pivot - 1
    Else
    If TERRAUnitInfo(Units[Pivot])._Addresses[Length(TERRAUnitInfo(Units[Pivot])._Addresses) - 1].Address < Address then
      Start := Pivot + 1
    Else
      Begin
        Result := Units[Pivot];
				Start := Finish + 1;
      End;
  End;
End;

Function FindRoutineWithAddress(Const Address:Cardinal):TERRARoutineInfo;
Var
	Start, Finish, Pivot: integer;
Begin
	Start := 0;
	Result := Nil;
	Finish := RoutinesCount - 1;

	While Start <= Finish Do
  Begin
    Pivot := Start + (Finish - Start) div 2;

		If TERRARoutineInfo(Routines[Pivot])._StartAddress > Address Then
      Finish := Pivot - 1
    Else
    If TERRARoutineInfo(Routines[Pivot])._EndAddress < Address Then
      Start := Pivot + 1
    Else
      Begin
        Result := TERRARoutineInfo(Routines[Pivot]);
				Start := Finish + 1;
      End;
  End;
End;

{$ELSE}
function TERRAUnitInfo.FindLineWithAddress(const Address: Cardinal): Integer;
begin
  Result := 0;
end;

Function FindUnitWithAddress(Const Address:Cardinal):TERRAUnitInfo;
Begin
  Result := Nil;
End;

Function FindRoutineWithAddress(Const Address:Cardinal):TERRARoutineInfo;
Begin
  Result := Nil;
End;

Procedure InitProjectInfos();
Begin
End;

{$ENDIF}

End.
