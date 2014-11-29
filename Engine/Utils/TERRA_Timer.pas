Unit TERRA_Timer;
{$I terra.inc}

(*
/* Code to check whether RDTSC is available */
DWORD timerAvailable = 0x1;

_asm{
		;	Get CPU ID information
		mov eax, 0
		cpuid

		;	check if  CPU is GenuineIntel
		cmp ebx, 'uneG'
		jne not_intel
		cmp edx, 'Ieni'
		jne not_intel
		cmp ecx, 'letn'
		jne not_intel
			jmp test_RDTSC

	not_intel:
			;	check is CPU is AuthenticAMD
		cmp ebx, 'htuA'
		jne not_available
		cmp edx, 'itne'
		jne not_available
		cmp ecx, 'DMAc'		
		jne not_available

	test_RDTSC:
			;	get CPU supported features
		mov eax, 1
		cpuid

		;	check if TSC supported
		bt edx, 4
		jnc not_available

		;	TSC supported
		xor timerAvailable, 1h

	not_available:
			xor timerAvailable, 1h
}

/* Code to read Time Stamp Counter */
int64 time;
int64* addr = &time;
__emit__(0x0f);
__emit__(0x31);
_asm{
		;	read TSC
		mov esi, addr
		mov [esi], eax
		mov [esi + 4], edx
}
 *)

Interface
Uses TERRA_Utils;

Const
  MAX_SAMPLE_COUNT = 50;

Type
  Timer = Class(TERRAObject)
    Protected
      frameTimes:Array[0..Pred(MAX_SAMPLE_COUNT)] Of Single;
      timeScale:Single;
      actualElapsedTimeSec:Single;
      lastTime:Int64;
      sampleCount:Integer;

    Public
	    Constructor Create;
      Destructor Destroy; Override;

	    Function GetElapsedTime:Single;
  End;

Implementation
Uses Windows;

Var
  QFreq:Int64;

Constructor Timer.Create;
Begin
  timeScale := 0.0;
  actualElapsedTimeSec := 0.0;
  lastTime := 0;
  sampleCount := 0;

	QueryPerformanceCounter(lastTime);
  timeScale := 1.0 / QFreq;
	Self.GetElapsedTime;
End;

Destructor Timer.Destroy;
Begin
  // do nothing
End;

Function Timer.GetElapsedTime:Single;
Var
  I:Integer;
  time:Int64;
  elapsedTimeSec:Single;
Begin
  Time := 0;
  QueryPerformanceCounter(time);
  elapsedTimeSec := (time - lastTime) * timeScale;
  lastTime := time;

  If (Abs(elapsedTimeSec - actualElapsedTimeSec) < 1.0) Then
  Begin
    Move(frameTimes[0], frameTimes[1], SizeOf(frameTimes) - Sizeof(frameTimes[0]));
    frameTimes[0] := elapsedTimeSec;

    If (sampleCount < MAX_SAMPLE_COUNT) Then
      Inc(sampleCount);
  End;

  actualElapsedTimeSec := 0.0;

  For i := 0 To Pred(sampleCount) Do
    actualElapsedTimeSec := actualElapsedTimeSec + frameTimes[i];

  If (sampleCount > 0) Then
    actualElapsedTimeSec := actualElapsedTimeSec / sampleCount;

  Result := actualElapsedTimeSec;
End;


Initialization
  QFreq := 0;
	QueryPerformanceFrequency(QFreq);
End.

