.syntax unified		
.section __TEXT,__text,regular
.section __TEXT,__textcoal_nt,coalesced
.section __TEXT,__const_coal,coalesced
.section __TEXT,__symbol_stub4,symbol_stubs,none,12
.text

.text
.align 2		
.arm

.globl _enable_runfast
.no_dead_strip _enable_runfast
.private_extern _enable_runfast
_enable_runfast:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	movw	r3, #24672
	movt	r3, 1032
	mov	r2, #50331648
	fmrx   r3, fpscr                       
	and    r3, r3, r3                      
	orr    r3, r3, r2                      
	fmxr   fpscr, r3                       

	str	r3, [fp, #-8]
	sub	sp, fp, #0
	ldr	fp, [sp], #4
	bx	lr		
	
.globl _dot2_neon_hfp
.no_dead_strip _dot2_neon_hfp
.private_extern _dot2_neon_hfp
_dot2_neon_hfp:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]

	vld1.32                {d2}, [r3]                      
	vld1.32                {d4}, [r2]                      
	vmul.f32               d0, d2, d4                      
	vpadd.f32              d0, d0, d0                      
	
	mov	r0, r3	@ float
	sub	sp, fp, #0
	ldr	fp, [sp], #4
	bx	lr		

.globl _normalize2_neon
.no_dead_strip _normalize2_neon
.private_extern _normalize2_neon
_normalize2_neon:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]

	vld1.32                d4, [r3]                                
	vmul.f32               d0, d4, d4                              
	vpadd.f32              d0, d0                                  
	vmov.f32               d1, d0                                  
	vrsqrte.f32    d0, d0                                  
	vmul.f32               d2, d0, d1                              
	vrsqrts.f32    d3, d2, d0                              
	vmul.f32               d0, d0, d3                              
	vmul.f32               d2, d0, d1                              
	vrsqrts.f32    d3, d2, d0                              
	vmul.f32               d0, d0, d3                              
	vmul.f32               d4, d4, d0[0]                   
	vst1.32                d4, [r2]                                

	sub	sp, fp, #0
	ldr	fp, [sp], #4
	bx	lr		
	
.globl _dot3_neon_hfp
.no_dead_strip _dot3_neon_hfp
.private_extern _dot3_neon_hfp
_dot3_neon_hfp:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]

	vld1.32                {d2}, [r3]                      
	flds                   s6, [r3, #8]            
	vld1.32                {d4}, [r2]                      
	flds                   s10, [r2, #8]   
	vmul.f32               d0, d2, d4                      
	vpadd.f32              d0, d0, d0                      
	vmla.f32               d0, d3, d5                      
	
	mov	r0, r3	@ float
	sub	sp, fp, #0
	ldr	fp, [sp], #4
	bx	lr		
	
.globl _cross3_neon
.no_dead_strip _cross3_neon
.private_extern _cross3_neon
_cross3_neon:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #20
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	str	r2, [fp, #-16]
	ldr	r1, [fp, #-8]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-16]

	flds                   s3, [r1]                        
	add                    r1, r1, #4                      
	vld1.32                {d0}, [r1]                      
	vmov.f32               s2, s1                          
	flds                   s5, [r2]                        
	add                    r2, r2, #4                      
	vld1.32                {d3}, [r2]                      
	vmov.f32               s4, s7                          
	vmul.f32               d4, d0, d2                      
	vmls.f32               d4, d1, d3                      
	vmul.f32               d5, d3, d1[1]           
	vmls.f32               d5, d0, d2[1]           
	vst1.32                d4, [r3]                        
	add                    r3, r3, #8                      
	fsts                   s10, [r3]                       
	
	str	r1, [fp, #-8]
	str	r2, [fp, #-12]
	str	r3, [fp, #-16]
	sub	sp, fp, #0

	ldr	fp, [sp], #4
	bx	lr		
	

.globl _normalize3_neon
.no_dead_strip _normalize3_neon
.private_extern _normalize3_neon
_normalize3_neon:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]

	vld1.32                {d4}, [r3]                              
	flds                   s10, [r3, #8]                   
	vmul.f32               d0, d4, d4                              
	vpadd.f32              d0, d0                                  
	vmla.f32               d0, d5, d5                              
	vmov.f32               d1, d0                                  
	vrsqrte.f32    d0, d0                                  
	vmul.f32               d2, d0, d1                              
	vrsqrts.f32    d3, d2, d0                              
	vmul.f32               d0, d0, d3                              
	vmul.f32               d2, d0, d1                              
	vrsqrts.f32    d3, d2, d0                              
	vmul.f32               d0, d0, d3                              
	vmul.f32               q2, q2, d0[0]                   
	vst1.32                {d4}, [r2]                              
	fsts                   s10, [r2, #8]                   
	

	sub	sp, fp, #0

	ldr	fp, [sp], #4
	bx	lr		
	
.globl _normalize4_neon
.no_dead_strip _normalize4_neon
.private_extern _normalize4_neon
_normalize4_neon:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]

	vld1.32                {d4, d5}, [r3]                  
	vmul.f32               d0, d4, d4                              
	vmla.f32               d0, d5, d5                              
	vpadd.f32              d0, d0                                  
	vmov.f32               d1, d0                                  
	vrsqrte.f32    d0, d0                                  
	vmul.f32               d2, d0, d1                              
	vrsqrts.f32    d3, d2, d0                              
	vmul.f32               d0, d0, d3                              
	vmul.f32               d2, d0, d1                              
	vrsqrts.f32    d3, d2, d0                              
	vmul.f32               d0, d0, d3                              
	vmul.f32               q2, q2, d0[0]                   
	vst1.32                {d4, d5}, [r2]                  
	
	sub	sp, fp, #0
	ldr	fp, [sp], #4
	bx	lr		
	
.globl _dot4_neon_hfp
.no_dead_strip _dot4_neon_hfp
.private_extern _dot4_neon_hfp
_dot4_neon_hfp:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]

	vld1.32                {d2, d3}, [r3]                  
	vld1.32                {d4, d5}, [r2]                  
	vmul.f32               d0, d2, d4                              
	vmla.f32               d0, d3, d5                              
	vpadd.f32              d0, d0                                  
	
	mov	r0, r3	@ float
	sub	sp, fp, #0
	ldr	fp, [sp], #4
	bx	lr		
	
.globl _matmul3_neon
.no_dead_strip _matmul3_neon
.private_extern _matmul3_neon
_matmul3_neon:
	str	fp, [sp, #-4]!
	fstmfdd	sp!, {d8, d9, d10, d11, d12, d13, d14, d15}
	add	fp, sp, #64
	sub	sp, sp, #20
	str	r0, [fp, #-72]
	str	r1, [fp, #-76]
	str	r2, [fp, #-80]
	ldr	r1, [fp, #-72]
	ldr	r2, [fp, #-76]
	ldr	r3, [fp, #-80]

	vld1.32                {d0, d1}, [r2]!                 
	vld1.32                {d2, d3}, [r2]!                 
	flds                   s8, [r2]                                
	vld1.32                {d6, d7}, [r1]                  
	add                    r1, r1, #12                             
	vld1.32                {d8, d9}, [r1]                  
	add                    r1, r1, #12                             
	vld1.32                {d10}, [r1]                             
	add                    r1, r1, #8                              
	flds                   s22, [r1]                               
	vmul.f32               q6, q3, d0[0]                   
	vmul.f32               q7, q3, d1[1]                   
	vmul.f32               q8, q3, d3[0]                   
	vmla.f32               q6, q4, d0[1]                   
	vmla.f32               q7, q4, d2[0]                   
	vmla.f32               q8, q4, d3[1]                   
	vmla.f32               q6, q5, d1[0]                   
	vmla.f32               q7, q5, d2[1]                   
	vmla.f32               q8, q5, d4[0]                   
	vmov.f32               q0, q8                                  
	vst1.32                {d12, d13}, [r3]                
	add                    r3, r3, #12                             
	vst1.32                {d14, d15}, [r3]                
	add                    r3, r3, #12                             
	vst1.32                {d0}, [r3]                              
	add                    r3, r3, #8                              
	fsts                   s2, [r3]                                
	

	str	r1, [fp, #-72]
	str	r2, [fp, #-76]
	str	r3, [fp, #-80]
	sub	sp, fp, #64

	fldmfdd	sp!, {d8-d15}
	ldr	fp, [sp], #4
	bx	lr		
	
.globl _matvec3_neon
.no_dead_strip _matvec3_neon
.private_extern _matvec3_neon
_matvec3_neon:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #28
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	ldr	r0, [fp, #-16]
	ldr	r1, [fp, #-20]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-8]

	mov                    r3, #12                                 
	vld1.32                {d0, d1}, [r1]                  
	vld1.32                {d2, d3}, [r0], r3              
	vld1.32                {d4, d5}, [r0], r3              
	vld1.32                {d6, d7}, [r0], r3              
	vmul.f32               q9, q1, d0[0]                   
	vmla.f32               q9, q2, d0[1]                   
	vmla.f32               q9, q3, d1[0]                   
	vmov.f32               q0, q9                                  
	vst1.32                d0, [r2]!                               
	fsts                   s2, [r2]                                
	
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	str	r3, [fp, #-8]
	sub	sp, fp, #0

	ldr	fp, [sp], #4
	bx	lr		
	
.globl _matmul4_neon
.no_dead_strip _matmul4_neon
.private_extern _matmul4_neon
_matmul4_neon:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #20
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	str	r2, [fp, #-16]
	ldr	r1, [fp, #-8]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-16]

	vld1.32                {d0, d1}, [r2]!                 
	vld1.32                {d2, d3}, [r2]!                 
	vld1.32                {d4, d5}, [r2]!                 
	vld1.32                {d6, d7}, [r2]                  
	vld1.32                {d16, d17}, [r1]!               
	vld1.32                {d18, d19}, [r1]!               
	vld1.32                {d20, d21}, [r1]!               
	vld1.32                {d22, d23}, [r1]                
	vmul.f32               q12, q8, d0[0]                  
	vmul.f32               q13, q8, d2[0]                  
	vmul.f32               q14, q8, d4[0]                  
	vmul.f32               q15, q8, d6[0]                  
	vmla.f32               q12, q9, d0[1]                  
	vmla.f32               q13, q9, d2[1]                  
	vmla.f32               q14, q9, d4[1]                  
	vmla.f32               q15, q9, d6[1]                  
	vmla.f32               q12, q10, d1[0]                 
	vmla.f32               q13, q10, d3[0]                 
	vmla.f32               q14, q10, d5[0]                 
	vmla.f32               q15, q10, d7[0]                 
	vmla.f32               q12, q11, d1[1]                 
	vmla.f32               q13, q11, d3[1]                 
	vmla.f32               q14, q11, d5[1]                 
	vmla.f32               q15, q11, d7[1]                 
	vst1.32                {d24, d25}, [r3]!               
	vst1.32                {d26, d27}, [r3]!               
	vst1.32                {d28, d29}, [r3]!               
	vst1.32                {d30, d31}, [r3]                
	
	str	r1, [fp, #-8]
	str	r2, [fp, #-12]
	str	r3, [fp, #-16]
	sub	sp, fp, #0

	ldr	fp, [sp], #4
	bx	lr		
	
.globl _matvec4_neon
.no_dead_strip _matvec4_neon
.private_extern _matvec4_neon
_matvec4_neon:
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #20
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	str	r2, [fp, #-16]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]
	ldr	r1, [fp, #-16]

	vld1.32                {d0, d1}, [r2]                  
	vld1.32                {d18, d19}, [r3]!               
	vld1.32                {d20, d21}, [r3]!               
	vld1.32                {d22, d23}, [r3]!               
	vld1.32                {d24, d25}, [r3]!               
	vmul.f32               q13, q9, d0[0]                  
	vmla.f32               q13, q10, d0[1]                 
	vmla.f32               q13, q11, d1[0]                 
	vmla.f32               q13, q12, d1[1]                 
	vst1.32                {d26, d27}, [r1]                
	
	sub	sp, fp, #0
	ldr	fp, [sp], #4
	bx	lr		
