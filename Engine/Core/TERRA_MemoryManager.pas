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
 * TERRA_Memory_Manager
 * Implements a custom memory allocator, used mostly for finding leaks/unnecessary allocations
 ***********************************************************************************************************************
}
Unit TERRA_MemoryManager;

{$I terra.inc}
Interface

{$IFDEF ANDROID}
Uses cmem;
{$ENDIF}

{$IFNDEF FPC}
Uses {FastMM4, }Windows;

{.$DEFINE USE_MSVCTR}
{$ENDIF}

Type
{$IFDEF FPC}
  {$IFDEF CPU64}
  SizeType = QWord;
  {$ELSE}
  SizeType = LongWord;
  {$ENDIF}
{$ELSE}
  {$IF CompilerVersion >= 22}
  SizeType = NativeInt;
  {$ELSE}
  SizeType = Integer;
  {$IFEND}
{$ENDIF}

  PMemoryAllocStats = ^MemoryAllocStats;
  MemoryAllocStats = Record
    Name:^String;
    Line:Cardinal;
    AllocCount:Cardinal;
    AllocSize:SizeType;
  End;

  MemoryManager = Class
    Protected

    Public
      Class Procedure BeginFrame();

      Class Function GetAllocCountPerFrame():Cardinal;
      Class Function GetAllocSizePerFrame():Cardinal;

      Class Function GetAllocStatsCount():Integer;
      Class Function GetAllocStatsInfo(Index:Cardinal):PMemoryAllocStats;

      Class Procedure EnableStats(Value:Boolean);
  End;

Implementation
Uses TERRA_Callstack, TERRA_Error;

{$IFDEF USE_MSVCTR}
Const
  msvcrtDLL = 'msvcrt.dll';

function malloc(Size: SizeType): Pointer; cdecl; external msvcrtDLL;
function realloc(P: Pointer; Size: SizeType): Pointer; cdecl; external msvcrtDLL;
procedure free(P: Pointer); cdecl; external msvcrtDLL;
{$ENDIF}

Var
  _MemoryManager_Instance:MemoryManager;
  _AllocMemCount:Cardinal;
  _AllocMemSize:SizeType;
  _UsedRAM:Cardinal;
  PrevManager:TMemoryManager;

Const
  MaxAllocStats = 256;

Var
  _AllocStatsEnabled:Boolean;
  _AllocStats:Array[0..Pred(MaxAllocStats)] Of MemoryAllocStats;
  _AllocStatsCount:Cardinal;

  _PrevAllocStats:Array[0..Pred(MaxAllocStats)] Of MemoryAllocStats;
  _PrevAllocStatsCount:Cardinal;

Function TERRA_GetMem(Size: SizeType): Pointer;{$IFDEF FPC} {$IFDEF CPUARM }Stdcall;  {$ELSE}Register;{$ENDIF}{$ENDIF}
Var
  I,N:Integer;
  Info:CallInfo;
  Temp:Boolean;
Begin
  {$IFDEF USE_MSVCTR}
  Result := malloc(size);
  {$ELSE}
  Result := PrevManager.GetMem(Size);
  {$ENDIF}

  If (Size>0) And (Result = Nil) Then
  Begin
    RaiseError('Out of memory!');
    Exit;
  End;

  {If Assigned(Result) Then
  Begin
    FillChar(Result^, Size, 0);
  End;}

  If (Not _AllocStatsEnabled) Then
    Exit;

  Inc(_AllocMemCount);
  Inc(_AllocMemSize, Size);

  {$IFDEF DEBUG_ALLOCS}
  If (_AllocStatsCount>=MaxAllocStats) Then
    Exit;

  If (Info.Name=Nil)  Then
    Exit;

  Temp := _AllocStatsEnabled;
  _AllocStatsEnabled := False;
  GetCurrentCall(Info);

  N := -1;
  For I:=0 To Pred(_AllocStatsCount) Do
  If (Cardinal(_AllocStats[I].Name) = Cardinal(Info.Name)) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    N := _AllocStatsCount;
    Inc(_AllocStatsCount);

    Cardinal(_AllocStats[N].Name) := Cardinal(Info.Name);
    _AllocStats[N].Line := Info.Line;
    _AllocStats[N].AllocCount := 0;
    _AllocStats[N].AllocSize := 0;
  End;

  Inc(_AllocStats[N].AllocCount);
  Inc(_AllocStats[N].AllocSize, Size);

  _AllocStatsEnabled := Temp;
  {$ENDIF}
End;

{$IFDEF FPC}
Function TERRA_FreeMem(P: Pointer):SizeType; {$IFDEF CPUARM }Stdcall;  {$ELSE} Register;{$ENDIF}
{$ELSE}
Function TERRA_FreeMem(P: Pointer):Integer;
{$ENDIF}
Begin
  {$IFDEF USE_MSVCTR}
  free(P);
  {$ELSE}
  PrevManager.FreeMem(P);
  {$ENDIF}
  Result := 0;
End;

{$IFDEF FPC}
Function TERRA_ReallocMem(Var P: Pointer; Size: SizeType): Pointer;{$IFDEF FPC} {$IFDEF CPUARM }Stdcall;  {$ELSE}Register;{$ENDIF}{$ENDIF}
Begin
  Result := PrevManager.ReallocMem(P, Size);

  If (Size>0) And (Result = Nil) Then
  Begin
    RaiseError('Out of memory!');
    Exit;
  End;
End;
{$ELSE}
Function TERRA_ReallocMem(P: Pointer; Size: SizeType): Pointer;
Begin
  {$IFDEF USE_MSVCTR}
  Result := realloc(P, Size);
  {$ELSE}
  Result := PrevManager.ReallocMem(P, Size);
  {$ENDIF}

  If (Size>0) And (Result = Nil) Then
  Begin
    RaiseError('Out of memory!');
    Exit;
  End;
End;
{$ENDIF}

{ MemoryManager }
Class Procedure MemoryManager.BeginFrame;
Var
  I, Count, MaxCount, MaxIndex:Integer;
Begin
  {$IFDEF DEBUG_ALLOCS}
  Count := 0;
  While Count<_AllocStatsCount Do
  Begin
    MaxIndex := -1;
    MaxCount := 0;

    For I:=0 To Pred(_AllocStatsCount) Do
    If (_AllocStats[I].AllocCount>MaxCount) Then
    Begin
      MaxIndex := I;
      MaxCount := _AllocStats[I].AllocCount;
    End;

    _PrevAllocStats[Count] := _AllocStats[MaxIndex];
    Inc(Count);
    _AllocStats[MaxIndex].AllocCount := 0;
  End;

  _PrevAllocStatsCount := _AllocStatsCount;
  {$ELSE}
  _PrevAllocStatsCount := 0;
  {$ENDIF}

  _AllocMemCount := 0;
  _AllocMemSize := 0;
  _AllocStatsCount := 0;
  _AllocStatsEnabled := True;
End;

Class Procedure MemoryManager.EnableStats(Value: Boolean);
Begin
  _AllocStatsEnabled := Value;
End;

Class Function MemoryManager.GetAllocCountPerFrame: Cardinal;
Begin
  Result := _AllocMemCount ;
End;

Class Function MemoryManager.GetAllocSizePerFrame: Cardinal;
Begin
  Result := _AllocMemSize;
End;

{Class function MemoryManager.Instance: MemoryManager;
Begin
  If _MemoryManager_Instance = Nil Then
    _MemoryManager_Instance := MemoryManager.Create();

  Result := _MemoryManager_Instance;
End;}

Class Function MemoryManager.GetAllocStatsCount:Integer;
Begin
  Result := _PrevAllocStatsCount;
End;

Class function MemoryManager.GetAllocStatsInfo(Index: Cardinal): PMemoryAllocStats;
Begin
  If (Index<_PrevAllocStatsCount) Then
    Result := @_PrevAllocStats[Index]
  Else
    Result := Nil;
End;

Var
  TERRAMemoryManager: TMemoryManager;

Initialization
  GetMemoryManager(PrevManager);

  {$IFNDEF DEBUG_LEAKS}
  {$IFNDEF DISABLEMEMORYMANAGER}
  TERRAMemoryManager := PrevManager;

  TERRAMemoryManager.GetMem := TERRA_GetMem;
  TERRAMemoryManager.FreeMem := TERRA_FreeMem;
  TERRAMemoryManager.ReallocMem := TERRA_ReallocMem;

  SetMemoryManager(TERRAMemoryManager);
  //MemoryManager.Instance();
  {$ENDIF}
  {$ENDIF}
End.
