//Atan2SSE() 
//	Input: 	xmm0: Y
//			xmm1: X
//	Output: xmm2: atan(X,Y)
	movaps xmm3, xmm0
	minps xmm0, xmm1 // xmm0 = ( y > x ) ? x : y
	maxps xmm1, xmm3 // xmm1 = ( y > x ) ? y : x
	cmpeqps xmm3, xmm0 // xmm3 = ( y > x ) ? 0xFFFFFFFF : 0x00000000
	rcpps xmm2, xmm1
	mulps xmm1, xmm2
	mulps xmm1, xmm2
	addps xmm2, xmm2
	subps xmm2, xmm1 // xmm2 = ( y > x ) ? 1 / y : 1 / x
	mulps xmm0, xmm2 // xmm0 = ( y > x ) ? x / y : y / x
	movaps xmm1, xmm3
  mov eax, SIMD_SP_signBit
	andps xmm1, [eax] // xmm1 = ( y > x ) ? 0x80000000 : 0x00000000
	xorps xmm0, xmm1 // xmm0 = ( y > x ) ? -x / y : y / x
  mov eax, SIMD_SP_halfPI
	andps xmm3, [eax] // xmm3 = ( y > x ) ? PI/2 : 0.0f
	movaps xmm1, xmm0
	mulps xmm1, xmm1
  mov eax, SIMD_SP_atan_c0
	movaps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_atan_c1
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_atan_c2
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_atan_c3
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_atan_c4
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_atan_c5
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_atan_c6
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_atan_c7
	addps xmm2, [eax]
	mulps xmm2, xmm1
  mov eax, SIMD_SP_one
	addps xmm2, [eax]
	mulps xmm2, xmm0
	addps xmm2, xmm3

