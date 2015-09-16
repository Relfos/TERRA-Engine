Unit TERRA_Profiler;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Object, TERRA_Threads, TERRA_DebugInfo;

Type
  ProfilerSample = Record
    //Address:PtrUInt;
    Routine:TERRARoutineInfo;
    Samples:Int64;
  End;

  TERRAProfiler = Class(TERRAThread)
    Protected
      _Samples:ProfilerSample;
      _SampleCount:Integer;

    Public
      TargetThread:ThreadHandle;

      Procedure Execute; Override;
  End;


Implementation
Uses Windows;

{ TERRAProfiler }
Procedure TERRAProfiler.Execute;
Var
  TargetContext:_CONTEXT;
  Routine:TERRARoutineInfo;
Begin
  TargetContext.ContextFlags := CONTEXT_FULL;
  If SuspendThread(TargetThread) = -1 then
    Exit;

  If Not GetThreadContext(TargetThread, TargetContext) then
    Exit;

  Routine := FindRoutineWithAddress(TargetContext.Eip);

  If ResumeThread(TargetThread) = -1 Then
    Exit;
End;

End.