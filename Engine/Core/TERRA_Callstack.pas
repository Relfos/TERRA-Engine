Unit TERRA_Callstack;

{$I terra.inc}

Interface

Type
  CallInfo = Record
    Name:^AnsiString;
    Line:Cardinal;
  End;

//Procedure FillCallStack(Const NbLevelsToExclude: integer);

Procedure GetCurrentCall(Var Info:CallInfo);

//Function TextualDebugInfoForAddress(Const TheAddress: Cardinal):AnsiString;

Var
  SettingCallStack:Boolean = False;

Implementation
{$IFNDEF FPC}
Uses Windows;
{$ENDIF}

Const
  SkillCalls = 0;
	StoredCallStackDepth = 26;
	{Size of the call stack we store when GetMem is called, must be an EVEN number}

Type
	CallStackArray = Array[0..StoredCallStackDepth] of Pointer;

Var
  _St: CallStackArray;

{$IFNDEF FPC}
{$IFDEF VER150}
{$DEFINE SUPPORT_CALLSTACK}
{$ENDIF}
{$ENDIF}

{$IFDEF SUPPORT_CALLSTACK}
Type
	AddressToLine = Class     
	  Public
		  Address: Cardinal;
		  Line: Cardinal;

		  Constructor Create(const AAddress, ALine: Cardinal);
	  End;

	UnitDebugInfos = class
	  Public
  		Name:AnsiString;
	  	Addresses: array of AddressToLine;
      ShowOnCallstack:Boolean;

		  Constructor Create(const AName:AnsiString; const NbLines: Cardinal);

  		Function LineWhichContainsAddress(const Address: Cardinal):Integer;
	  End;

  RoutineDebugInfos = class
	  Public
		  Name:AnsiString;
  		StartAddress: Cardinal;
	  	EndAddress: Cardinal;

		  Constructor Create(const AName:AnsiString; const AStartAddress: Cardinal; const ALength: Cardinal);
	  End;


Constructor UnitDebugInfos.Create(const AName:AnsiString; const NbLines: Cardinal);
Begin
	Name := AName;

  ShowOnCallstack := {(Pos('TERRA_', AName)>0) And} (Pos('Callstack', AName)<=0) And (Pos('MemoryManager', AName)<=0);

	SetLength(Addresses, NbLines);
End;

constructor RoutineDebugInfos.Create(const AName:AnsiString; const AStartAddress: Cardinal; const ALength: Cardinal);
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

procedure AddRoutine(const Name:AnsiString; const Start, Len: Cardinal);
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
  
Function ExtractFileName(FileName:AnsiString):AnsiString;
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
	FileName:AnsiString;
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

function UnitWhichContainsAddress(const Address: Cardinal):UnitDebugInfos;
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

Procedure FillCallStack(Const NbLevelsToExclude: integer);
	{Works only with stack frames - Without, St contains correct info, but is not as deep as it should
	I just don't know a general rule for walking the stack when they are not there}
Var
	StackStart: Pointer;
	StackMax: Pointer;	//the stack can never go beyond - http://msdn.microsoft.com/library/periodic/period96/S2CE.htm
	CurrentFrame: Pointer;
	Count, SkipCount: integer;
begin
	FillChar(_St, SizeOf(_St), 0);
	asm
		mov EAX, FS:[4]
		mov StackMax, EAX
		mov StackStart, EBP
	end;
	CurrentFrame:= StackStart;
	Count:= 0;
	SkipCount:= 0;
	while (longint(CurrentFrame) >= longint(StackStart)) and (longint(CurrentFrame) < longint(StackMax)) and (Count <= StoredCallStackDepth) do
		begin
			if SkipCount >= NbLevelsToExclude then
				begin
					_St[Count]:= Pointer(PInteger(longint(CurrentFrame) + 4)^ - 4);
					Count:= Count + 1;
				end;
			CurrentFrame:= Pointer(PInteger(CurrentFrame)^);
			SkipCount:= SkipCount + 1;
		end;
End;

(*Function TextualDebugInfoForAddress(Const TheAddress: Cardinal):AnsiString;
var
	U:UnitDebugInfos;
	AddressInDebugInfos: Cardinal;
begin
	If UnitsCount<=0 Then
    GetProjectInfos();

	If (TheAddress > Displ) then
  Begin
    AddressInDebugInfos := TheAddress - Displ;

		U := UnitWhichContainsAddress(AddressInDebugInfos);

    If U <> nil then
//				Result := 'Module ' + U.Name + RoutineWhichContainsAddress(AddressInDebugInfos) + U.LineWhichContainsAddress(AddressInDebugInfos)
      Result := RoutineWhichContainsAddress(AddressInDebugInfos)
    Else
      Result := RoutineWhichContainsAddress(AddressInDebugInfos);
  End Else
		Result := NoDebugInfo;

	//Result := Result + ' Find error: ' + HexStr(TheAddress);
End;*)

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

  FillCallstack(0);
  SettingCallStack := False;

	I := 0;
	Info.Name := Nil;
  Info.Line := 0;

  Count := SkillCalls;

	While (i <= StoredCallStackDepth) and (_St[i] <> nil) Do
  Begin
    TheAddress := Cardinal(_St[i]);

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

{$ELSE}

Procedure GetCurrentCall(Var Info:CallInfo);
Begin
	Info.Name := Nil;
  Info.Line := 0;
End;
{$ENDIF}


End.