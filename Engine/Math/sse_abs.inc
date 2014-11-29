//AbsSSE() 
//	Input: 	xmm0: x
//	Output: xmm2: Abs(x)
	mov eax, SIMD_SP_signBit
	andps xmm0, [eax] // xmm0 = ( x < 0 ) ? 0x80000000 : 0x00000000
