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


Type
  Timer = Class
    Class Function GetElapsedTime:Single;
  End;

Implementation
Uses Windows;

Const
  MAX_SAMPLE_COUNT = 50;

Var
  QFreq:Int64;
  timeScale:Single;
  startTime:Int64;

Class Function Timer.GetElapsedTime:Single;
Var
  time:Int64;
Begin
  Time := 0;
  QueryPerformanceCounter(time);
  Result := (time - startTime);
  Result := Result * timeScale;
End;


Initialization
  QFreq := 0;
	QueryPerformanceFrequency(QFreq);

  startTime := 0;

	QueryPerformanceCounter(startTime);
  timeScale := 1.0 / QFreq;
End.

