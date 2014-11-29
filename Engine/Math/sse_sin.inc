//SinSSE() 
//	Input: 	xmm0: angle
//	Output: xmm2: Sin(angle)
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
