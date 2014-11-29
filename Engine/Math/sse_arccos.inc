//ArcCosSSE() 
//	Input: 	xmm0: x
//	Output: xmm2: ArcCos(x)
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
  xorps xmm0, xmm0
  maxps xmm2, xmm0
