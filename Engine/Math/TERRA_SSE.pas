{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores 
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
 * TERRA_SSE
 * Implements SSE optimized operations
 ***********************************************************************************************************************
}
// provides useful consts to SSE calculations
// all constants are 16 byte aligned
Unit TERRA_SSE;
{$i terra.inc}

Interface
Uses TERRA_Utils, TERRA_Log;

Function ArcCos(Const V:Single):Single;
Function Sin(Const V:Single):Single;

Var
  SIMD_SP_sin_c0:PSingle;
  SIMD_SP_sin_c1:PSingle;
  SIMD_SP_sin_c2:PSingle;
  SIMD_SP_sin_c3:PSingle;
  SIMD_SP_sin_c4:PSingle;
  SIMD_SP_arccos_c0:PSingle;
  SIMD_SP_arccos_c1:PSingle;
  SIMD_SP_arccos_c2:PSingle;
  SIMD_SP_arccos_c3:PSingle;
  SIMD_SP_arccos_c4:PSingle;
  SIMD_SP_arccos_c5:PSingle;
  SIMD_SP_halfPI:PSingle;
  SIMD_SP_one:PSingle;
  SIMD_SP_Epsilon:PSingle;
  SIMD_SP_OneMinusEpsilon:PSingle;
  SIMD_SP_signBit:PSingle;

  SIMD_SP_atan_c0:PSingle;
  SIMD_SP_atan_c1:PSingle;
  SIMD_SP_atan_c2:PSingle;
  SIMD_SP_atan_c3:PSingle;
  SIMD_SP_atan_c4:PSingle;
  SIMD_SP_atan_c5:PSingle;
  SIMD_SP_atan_c6:PSingle;
  SIMD_SP_atan_c7:PSingle;

Implementation

Function Sin(Const V:Single):Single;
Asm
  movss xmm0, V
  shufps xmm0, xmm0, 0

	movaps xmm1, xmm0
	mulps xmm1, xmm1
  mov eax, SIMD_SP_sin_c0
	movaps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_sin_c1
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_sin_c2
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_sin_c3
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_sin_c4
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_one
	addps xmm2, [eax]
	mulps xmm2, xmm0
  movss result, xmm2
End;


Function ArcCos(Const V:Single):Single;
Asm
  movss xmm0, V
  shufps xmm0, xmm0, 0

  mov eax, SIMD_SP_one
  movaps xmm1, [eax]
  movaps xmm3, xmm1
  addps xmm3, xmm3 // 2.0
  subps xmm1, xmm0  // Y = 1.0 - X

  movaps xmm2, xmm1
  mulps xmm2, xmm3    // (Y*2.0)
  rsqrtps xmm2, xmm2  // 1.0/Sqr(Y*2.0)

  // calculate polynomial
  mov eax, SIMD_SP_arccos_c0
  movaps xmm0, [eax]

  mov eax, SIMD_SP_arccos_c1
  movaps xmm3, [eax]
  mulps xmm3, xmm1
  addps xmm0, xmm3

  mov eax, SIMD_SP_arccos_c2
  movaps xmm3, [eax]
  mulps xmm1, xmm1
  mulps xmm3, xmm1
  addps xmm0, xmm3

  mov eax, SIMD_SP_arccos_c3
  movaps xmm3, [eax]
  mulps xmm1, xmm1
  mulps xmm3, xmm1
  addps xmm0, xmm3

  mov eax, SIMD_SP_arccos_c4
  movaps xmm3, [eax]
  mulps xmm1, xmm1
  mulps xmm3, xmm1
  addps xmm0, xmm3

  mov eax, SIMD_SP_arccos_c5
  movaps xmm3, [eax]
  mulps xmm1, xmm1
  mulps xmm3, xmm1
  addps xmm0, xmm3

  // calculate result
  mulps xmm2, xmm0
  movss result, xmm2
End;

Var
  _Buffer:PSingle;
  _BufferIndex:PSingle;

Procedure AlignInit(Var Name:PSingle; Value:Single);
Begin
  Name := PSingle((Cardinal(_BufferIndex)+$0F) And $FFFFFFF0);
  _BufferIndex := Name;
  PSingle(_BufferIndex)^ := Value;
  Inc(_BufferIndex);
  PSingle(_BufferIndex)^ := Value;
  Inc(_BufferIndex);
  PSingle(_BufferIndex)^ := Value;
  Inc(_BufferIndex);
  PSingle(_BufferIndex)^ := Value;
  Inc(_BufferIndex);
End;

Var
  S:Cardinal;
  P:Single;

  cntrReg, saveCntr:Cardinal;
  SSE_Enabled:Boolean;

Procedure InitSSE;
Begin
  Try
    SSE_Enabled := False;
    Asm
      mov eax, 1
      db $0F,$A2               /// cpuid
      test edx,(1 shl 25)
      jnz @SSEFound
      mov SSE_Enabled,0
      jmp @END_SSE
    @SSEFound:
      mov SSE_Enabled,1
    @END_SSE:
    End;
  Except
    SSE_Enabled := False;
  End;

  If (Not SSE_Enabled) Then
  Begin
    Log(logError,'Core','SSE extension required!');
    Halt;
  End;

//  MaskAllFPUExceptions;
{  Asm
    stmxcsr [cntrReg]        //Get MXCSR register
  End;
    saveCntr := cntrReg;
  Asm
    or [cntrReg], 32768 // enable FZ mode
    and [cntrReg], 7808 //bit 7 - invalid instruction mask
                              //bit 9 – divide-by-zero mask
                              //bit 10 - overflow mask
                              //bit 11 – underflow mask
     ldmxcsr [cntrReg]        //Load MXCSR register
  End;
 }
  {
  If (EnableSSE) Then
  Begin
    Enable3DNow := False;
  End Else
  Begin
    Enable3DNow := False;
    // check for 3DNow!
    Asm
      //test whether extended function 80000001h is supported
      mov eax, 80000000h      //call extended function 80000000h
      db $0F,$A2               /// cpuid                   //reports back highest supported ext. function
      cmp eax, 80000000h      //supports functions > 80000000h?
      jbe @NO_EXTENDED        //no 3DNow! support, either
      //test if function 80000001h indicates 3DNow! support
      mov eax, 80000001h      //call extended function 80000001h
      db $0F,$A2               /// cpuid                   //reports back extended feature flags
      test edx, 80000000h     //bit 31 in extended features
      jz @NO_3DNow            //if set, 3DNow! is supported
      mov Enable3DNow,1
      jmp @END_3DNow
      @NO_EXTENDED:
      @NO_3DNow:
        mov Enable3DNow,0
        jmp @END_3DNow
      @END_3DNow:
    End;
  End;}

  GetMem(_Buffer, 1024);
  _BufferIndex := _Buffer;

  AlignInit(SIMD_SP_sin_c0, -2.39e-08);
  AlignInit(SIMD_SP_sin_c1, 2.7526e-06);
  AlignInit(SIMD_SP_sin_c2, -1.98409e-04);
  AlignInit(SIMD_SP_sin_c3, 8.3333315e-03);
  AlignInit(SIMD_SP_sin_c4, -1.666666664e-01);

  AlignInit(SIMD_SP_one, 1.0 );
  AlignInit(SIMD_SP_Epsilon, 1e-6 );
  AlignInit(SIMD_SP_OneMinusEpsilon, 1.0- (1e-6) );
  AlignInit(SIMD_SP_halfPI, PI / 2.0 );

  S := $7FFFFFFF ;
  Move(S, P, 4);
  AlignInit(SIMD_SP_signBit, P );

  AlignInit(SIMD_SP_arccos_c0, -0.000007239283986332);
  AlignInit(SIMD_SP_arccos_c1, 2.000291665285952400);
  AlignInit(SIMD_SP_arccos_c2, 0.163910606547823220);
  AlignInit(SIMD_SP_arccos_c3, 0.047654245891495528);
  AlignInit(SIMD_SP_arccos_c4, 0.005516443930088506);
  AlignInit(SIMD_SP_arccos_c5, 0.015098965761299077 );

  AlignInit(SIMD_SP_atan_c0, 0.0028662257 );
  AlignInit(SIMD_SP_atan_c1, -0.0161657367 );
  AlignInit(SIMD_SP_atan_c2, 0.0429096138 );
  AlignInit(SIMD_SP_atan_c3, -0.0752896400 );
  AlignInit(SIMD_SP_atan_c4, 0.1065626393 );
  AlignInit(SIMD_SP_atan_c5, -0.1420889944 );
  AlignInit(SIMD_SP_atan_c6, 0.1999355085 );
  AlignInit(SIMD_SP_atan_c7, -0.3333314528 );
End;

Initialization
  InitSSE;
Finalization
  FreeMem(_Buffer);
End.
