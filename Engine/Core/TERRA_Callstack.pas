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
Uses SysUtils, TERRA_Object, TERRA_DebugInfo;

Const
{$IFNDEF FPC}
  SkillCalls = 0;
  LineEnding = #13#10;
	StoredCallStackDepth = 26; 	// Size of the call stack we store when GetMem is called, must be an EVEN number
{$ELSE}
	StoredCallStackDepth = 20;
{$ENDIF}

Type
  CallInfo = Record
    Routine:TERRARoutineInfo;
    Line:Cardinal;
  End;

	CallStackArray = Array[0..StoredCallStackDepth] of Pointer;

  { TERRACallstack }

  TERRACallstack = Class(TERRAObject)
    Protected
      _CurrentCallstack:CallStackArray;
      _CurrentCallstackSize:Integer;

      Procedure FillCallStackFromAddress(StackStart, StackMax:Pointer);

    Public
      Procedure FillCurrentCallStack();
      Procedure FillExceptionCallStack(E: Exception);

      Procedure GetCurrentCall(Out Info:CallInfo);

      Function GetDescription():TERRAString;
  End;

//Function GetLibraryAddress():Cardinal;

Var
  SettingCallStack:Boolean = False;

Implementation

{ TERRACallstack }

{$IFDEF DELPHI}
Function BackTraceStrFunc(Const TargetAddress:Pointer):String;
var
	U:TERRAUnitInfo;
  R:TERRARoutineInfo;
	AddressInDebugInfos: Cardinal;
  Num:String;
  TheAddress:Cardinal;
begin
  InitProjectInfos();

  TheAddress := Cardinal(TargetAddress);

	If (TheAddress > CodeDispl) then
  Begin
    AddressInDebugInfos := TheAddress - CodeDispl;

		U := FindUnitWithAddress(AddressInDebugInfos);
    R := FindRoutineWithAddress(AddressInDebugInfos);

    If Assigned(R) Then
    Begin
      Result := R.Name;
      If Assigned(U) Then
      Begin
        Str(U.FindLineWithAddress(AddressInDebugInfos), Num);
        Result := Result + ' (line '+Num+' of '+U.Name+')';
      End;
      Exit;
    End;
  End;


  Str(TheAddress, Num);
	Result := Num + ' (No debug info)';
End;

Procedure TERRACallstack.FillCallStackFromAddress(StackStart, StackMax:Pointer);
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

Procedure TERRACallstack.FillCurrentCallStack();
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

Procedure TERRACallstack.FillExceptionCallStack(E:Exception);
Begin
  FillCurrentCallStack();
  //FillCallStackFromAddress(ExceptAddr, Pointer(Cardinal(ExceptAddr) + 16));
End;

Procedure TERRACallstack.GetCurrentCall(Out Info:CallInfo);
Var
	I, Count:Integer;
  TheAddress:Cardinal;
	AddressInDebugInfos: Cardinal;
	U:TERRAUnitInfo;
  Routine:TERRARoutineInfo;
Begin
  SettingCallStack := True;

  InitProjectInfos();

  FillCurrentCallstack();
  SettingCallStack := False;

	I := 0;
	Info.Routine := Nil;
  Info.Line := 0;

  Count := SkillCalls;

	While (i <= StoredCallStackDepth) and (_CurrentCallstack[i] <> nil) Do
  Begin
    TheAddress := Cardinal(_CurrentCallstack[i]);

	  If (TheAddress > CodeDispl) Then
    Begin
      AddressInDebugInfos := TheAddress - CodeDispl;

		  U := FindUnitWithAddress(AddressInDebugInfos);

      If (Assigned(U)) And (U.ShowOnCallstack) Then
      Begin
        Routine := FindRoutineWithAddress(AddressInDebugInfos);
        If Assigned(Routine) Then
        Begin
          Dec(Count);
          Info.Routine := Routine;
          Info.Line := U.FindLineWithAddress(AddressInDebugInfos);
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
     Info.Routine := Nil;
     Info.Line := 0;
End;

procedure TERRACallstack.FillCallStackFromAddress(StackStart, StackMax: Pointer);
begin

end;

procedure TERRACallstack.FillCurrentCallStack;
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

procedure TERRACallstack.FillExceptionCallStack(E: Exception);
begin

end;

procedure TERRACallstack.GetCurrentCall(out Info: CallInfo);
begin

end;

Procedure FillExceptionCallStack(E:Exception);
Var
  I:Integer;
  Frames: PPointer;
Begin
  (*FIXME
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
  *)
End;
{$ENDIF}

function TERRACallstack.GetDescription: TERRAString;
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

End.
