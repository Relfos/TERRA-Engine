/*
Math-NEON:  Neon Optimised Math Library based on cmath
Contact:    lachlan.ts@gmail.com
Copyright (C) 2009  Lachlan Tychsen - Smith aka Adventus

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#if defined __arm__ 
//&& defined __ARM_NEON_

// this function enables the floating point runfast mode on the ARM Cortex A8.  
void enable_runfast()
{
        static const unsigned int x = 0x04086060;
        static const unsigned int y = 0x03000000;
        int r;

    __asm__ __volatile__(
                "fmrx   %0, fpscr                       \n\t"   //r0 = FPSCR
                "and    %0, %0, %1                      \n\t"   //r0 = r0 & 0x04086060
                "orr    %0, %0, %2                      \n\t"   //r0 = r0 | 0x03000000
                "fmxr   fpscr, %0                       \n\t"   //FPSCR = r0
                : "=r"(r)
                : "r"(x), "r"(y)
        );
}


//vec2 scalar product
float dot2_neon_hfp(float v0[2], float v1[2])
{
        __asm__ __volatile__ (
        "vld1.32                {d2}, [%0]                      \n\t"   //d2={x0,y0}
        "vld1.32                {d4}, [%1]                      \n\t"   //d4={x1,y1}
        "vmul.f32               d0, d2, d4                      \n\t"   //d0 = d2*d4
        "vpadd.f32              d0, d0, d0                      \n\t"   //d0 = d[0] + d[1]
        :: "r"(v0), "r"(v1) 
    : 
        );      
}

void normalize2_neon(float v[2], float d[2])
{
        __asm__ __volatile__ (
        "vld1.32                d4, [%0]                                \n\t"   //d4 = {x0,y0}
        "vmul.f32               d0, d4, d4                              \n\t"   //d0 = d2*d2
        "vpadd.f32              d0, d0                                  \n\t"   //d0 = d[0] + d[1]
        
        "vmov.f32               d1, d0                                  \n\t"   //d1 = d0
        "vrsqrte.f32    d0, d0                                  \n\t"   //d0 = ~ 1.0 / sqrt(d0)
        "vmul.f32               d2, d0, d1                              \n\t"   //d2 = d0 * d1
        "vrsqrts.f32    d3, d2, d0                              \n\t"   //d3 = (3 - d0 * d2) / 2        
        "vmul.f32               d0, d0, d3                              \n\t"   //d0 = d0 * d3
        "vmul.f32               d2, d0, d1                              \n\t"   //d2 = d0 * d1  
        "vrsqrts.f32    d3, d2, d0                              \n\t"   //d3 = (3 - d0 * d2) / 2        
        "vmul.f32               d0, d0, d3                              \n\t"   //d0 = d0 * d3  

        "vmul.f32               d4, d4, d0[0]                   \n\t"   //d4 = d4*d0[0]
        "vst1.32                d4, [%1]                                \n\t"   //
        
        :: "r"(v), "r"(d) 
    : "d0", "d1", "d2", "d3", "d4", "memory"
        );      
}

float dot3_neon_hfp(float v0[3], float v1[3])
{
        __asm__ __volatile__(
        "vld1.32                {d2}, [%0]                      \n\t"   //d2={x0,y0}
        "flds                   s6, [%0, #8]            \n\t"   //d3[0]={z0}
        "vld1.32                {d4}, [%1]                      \n\t"   //d4={x1,y1}
        "flds                   s10, [%1, #8]   \n\t"   //d5[0]={z1}

        "vmul.f32               d0, d2, d4                      \n\t"   //d0= d2*d4
        "vpadd.f32              d0, d0, d0                      \n\t"   //d0 = d[0] + d[1]
        "vmla.f32               d0, d3, d5                      \n\t"   //d0 = d0 + d3*d5 
        :: "r"(v0), "r"(v1) 
    : "d0","d1","d2","d3","d4","d5"
        );      
}

void cross3_neon(float v0[3], float v1[3], float d[3])
{
        __asm__ __volatile__ (
        "flds                   s3, [%0]                        \n\t"   //d1[1]={x0}
        "add                    %0, %0, #4                      \n\t"   //
        "vld1.32                {d0}, [%0]                      \n\t"   //d0={y0,z0}
        "vmov.f32               s2, s1                          \n\t"   //d1[0]={z0}

        "flds                   s5, [%1]                        \n\t"   //d2[1]={x1}
        "add                    %1, %1, #4                      \n\t"   //
        "vld1.32                {d3}, [%1]                      \n\t"   //d3={y1,z1}
        "vmov.f32               s4, s7                          \n\t"   //d2[0]=d3[1]
        
        "vmul.f32               d4, d0, d2                      \n\t"   //d4=d0*d2
        "vmls.f32               d4, d1, d3                      \n\t"   //d4-=d1*d3
        
        "vmul.f32               d5, d3, d1[1]           \n\t"   //d5=d3*d1[1]
        "vmls.f32               d5, d0, d2[1]           \n\t"   //d5-=d0*d2[1]
        
        "vst1.32                d4, [%2]                        \n\t"   //
        "add                    %2, %2, #8                      \n\t"   //
        "fsts                   s10, [%2]                       \n\t"   //
        
        : "+r"(v0), "+r"(v1), "+r"(d):
    : "d0", "d1", "d2", "d3", "d4", "d5", "memory"
        );      
}

void normalize3_neon(float v[3], float d[3])
{
        __asm__ __volatile__ (
        "vld1.32                {d4}, [%0]                              \n\t"   //d4={x0,y0}
        "flds                   s10, [%0, #8]                   \n\t"   //d5[0]={z0}

        "vmul.f32               d0, d4, d4                              \n\t"   //d0= d4*d4
        "vpadd.f32              d0, d0                                  \n\t"   //d0 = d[0] + d[1]
        "vmla.f32               d0, d5, d5                              \n\t"   //d0 = d0 + d5*d5 
        
        "vmov.f32               d1, d0                                  \n\t"   //d1 = d0
        "vrsqrte.f32    d0, d0                                  \n\t"   //d0 = ~ 1.0 / sqrt(d0)
        "vmul.f32               d2, d0, d1                              \n\t"   //d2 = d0 * d1
        "vrsqrts.f32    d3, d2, d0                              \n\t"   //d3 = (3 - d0 * d2) / 2        
        "vmul.f32               d0, d0, d3                              \n\t"   //d0 = d0 * d3
        "vmul.f32               d2, d0, d1                              \n\t"   //d2 = d0 * d1  
        "vrsqrts.f32    d3, d2, d0                              \n\t"   //d4 = (3 - d0 * d3) / 2        
        "vmul.f32               d0, d0, d3                              \n\t"   //d0 = d0 * d4  

        "vmul.f32               q2, q2, d0[0]                   \n\t"   //d0= d2*d4
        "vst1.32                {d4}, [%1]                              \n\t"   //
        "fsts                   s10, [%1, #8]                   \n\t"   //
        
        :: "r"(v), "r"(d) 
    : "d0", "d1", "d2", "d3", "d4", "d5", "memory"
        );      
}

void normalize4_neon(float v[4], float d[4])
{
        __asm__ __volatile__(
        "vld1.32                {d4, d5}, [%0]                  \n\t"   //d2={x0,y0}, d3={z0, w0}
        "vmul.f32               d0, d4, d4                              \n\t"   //d0= d4*d4
        "vmla.f32               d0, d5, d5                              \n\t"   //d0 = d0 + d5*d5 
        "vpadd.f32              d0, d0                                  \n\t"   //d0 = d[0] + d[1]
        
        "vmov.f32               d1, d0                                  \n\t"   //d1 = d0
        "vrsqrte.f32    d0, d0                                  \n\t"   //d0 = ~ 1.0 / sqrt(d0)
        "vmul.f32               d2, d0, d1                              \n\t"   //d2 = d0 * d1
        "vrsqrts.f32    d3, d2, d0                              \n\t"   //d3 = (3 - d0 * d2) / 2        
        "vmul.f32               d0, d0, d3                              \n\t"   //d0 = d0 * d3
        "vmul.f32               d2, d0, d1                              \n\t"   //d2 = d0 * d1  
        "vrsqrts.f32    d3, d2, d0                              \n\t"   //d4 = (3 - d0 * d3) / 2        
        "vmul.f32               d0, d0, d3                              \n\t"   //d0 = d0 * d4  

        "vmul.f32               q2, q2, d0[0]                   \n\t"   //d0= d2*d4
        "vst1.32                {d4, d5}, [%1]                  \n\t"   //d2={x0,y0}, d3={z0, w0}
        
        :: "r"(v), "r"(d) 
    : "d0", "d1", "d2", "d3", "d4", "d5", "memory"
        );      
}


float dot4_neon_hfp(float v0[4], float v1[4])
{
        __asm__ __volatile__ (
        "vld1.32                {d2, d3}, [%0]                  \n\t"   //d2={x0,y0}, d3={z0, w0}
        "vld1.32                {d4, d5}, [%1]                  \n\t"   //d4={x1,y1}, d5={z1, w1}
        "vmul.f32               d0, d2, d4                              \n\t"   //d0= d2*d4
        "vmla.f32               d0, d3, d5                              \n\t"   //d0 = d0 + d3*d5 
        "vpadd.f32              d0, d0                                  \n\t"   //d0 = d[0] + d[1]
        :: "r"(v0), "r"(v1) : 
        );      
}

void matmul3_neon(float m0[9], float m1[9], float d[9])
{
        __asm__ __volatile__ (
        "vld1.32                {d0, d1}, [%1]!                 \n\t"   //q0 = m1
        "vld1.32                {d2, d3}, [%1]!                 \n\t"   //q1 = m1+4
        "flds                   s8, [%1]                                \n\t"   //q2 = m1+8
        
        "vld1.32                {d6, d7}, [%0]                  \n\t"   //q3[0] = m0
        "add                    %0, %0, #12                             \n\t"   //q3[0] = m0
        "vld1.32                {d8, d9}, [%0]                  \n\t"   //q4[0] = m0+12
        "add                    %0, %0, #12                             \n\t"   //q3[0] = m0
        "vld1.32                {d10}, [%0]                             \n\t"   //q5[0] = m0+24
        "add                    %0, %0, #8                              \n\t"   //q3[0] = m0
        "flds                   s22, [%0]                               \n\t"   //q2 = m1+8
        
        "vmul.f32               q6, q3, d0[0]                   \n\t"   //q12 = q3 * d0[0]
        "vmul.f32               q7, q3, d1[1]                   \n\t"   //q13 = q3 * d2[0]
        "vmul.f32               q8, q3, d3[0]                   \n\t"   //q14 = q3 * d4[0]
        "vmla.f32               q6, q4, d0[1]                   \n\t"   //q12 = q9 * d0[1]
        "vmla.f32               q7, q4, d2[0]                   \n\t"   //q13 = q9 * d2[1]
        "vmla.f32               q8, q4, d3[1]                   \n\t"   //q14 = q9 * d4[1]
        "vmla.f32               q6, q5, d1[0]                   \n\t"   //q12 = q10 * d0[0]
        "vmla.f32               q7, q5, d2[1]                   \n\t"   //q13 = q10 * d2[0]
        "vmla.f32               q8, q5, d4[0]                   \n\t"   //q14 = q10 * d4[0]

        "vmov.f32               q0, q8                                  \n\t"   //q14 = q10 * d4[0]
        "vst1.32                {d12, d13}, [%2]                \n\t"   //d = q12
        "add                    %2, %2, #12                             \n\t"   //q3[0] = m0
        "vst1.32                {d14, d15}, [%2]                \n\t"   //d+4 = q13     
        "add                    %2, %2, #12                             \n\t"   //q3[0] = m0
        "vst1.32                {d0}, [%2]                              \n\t"   //d+8 = q14     
        "add                    %2, %2, #8                              \n\t"   //q3[0] = m0
        "fsts                   s2, [%2]                                \n\t"   //d = q12       
        
        : "+r"(m0), "+r"(m1), "+r"(d): 
    : "d8", "d9", "d10", "d11", "d12", "d13", "d14", "d15", "memory"
        );      
};

//matrix vector multiplication. d = m * v
void matvec3_neon(float m[9], float v[3], float d[3])
{
        int tmp;
        __asm__ __volatile__ (
        "mov                    %3, #12                                 \n\t"   //r3 = 12
        "vld1.32                {d0, d1}, [%1]                  \n\t"   //Q0 = v
        "vld1.32                {d2, d3}, [%0], %3              \n\t"   //Q1 = m
        "vld1.32                {d4, d5}, [%0], %3              \n\t"   //Q2 = m+12
        "vld1.32                {d6, d7}, [%0], %3              \n\t"   //Q3 = m+24
        
        "vmul.f32               q9, q1, d0[0]                   \n\t"   //Q9 = Q1*Q0[0]
        "vmla.f32               q9, q2, d0[1]                   \n\t"   //Q9 += Q2*Q0[1] 
        "vmla.f32               q9, q3, d1[0]                   \n\t"   //Q9 += Q3*Q0[2] 
        "vmov.f32               q0, q9                                  \n\t"   //Q0 = q9
        
        "vst1.32                d0, [%2]!                               \n\t"   //r2 = D24      
        "fsts                   s2, [%2]                                \n\t"   //r2 = D25[0]   

        : "+r"(m), "+r"(v), "+r"(d), "+r"(tmp):
    : "q0", "q9", "q10","q11", "q12", "q13", "memory"
        );      
}


//matrix matrix multipication. d = m0 * m1;

void matmul4_neon(float m0[16], float m1[16], float d[16])
{
       __asm__ __volatile__ (
        "vld1.32                {d0, d1}, [%1]!                 \n\t"   //q0 = m1
        "vld1.32                {d2, d3}, [%1]!                 \n\t"   //q1 = m1+4
        "vld1.32                {d4, d5}, [%1]!                 \n\t"   //q2 = m1+8
        "vld1.32                {d6, d7}, [%1]                  \n\t"   //q3 = m1+12
        "vld1.32                {d16, d17}, [%0]!               \n\t"   //q8 = m0
        "vld1.32                {d18, d19}, [%0]!               \n\t"   //q9 = m0+4
        "vld1.32                {d20, d21}, [%0]!               \n\t"   //q10 = m0+8
        "vld1.32                {d22, d23}, [%0]                \n\t"   //q11 = m0+12

        "vmul.f32               q12, q8, d0[0]                  \n\t"   //q12 = q8 * d0[0]
        "vmul.f32               q13, q8, d2[0]                  \n\t"   //q13 = q8 * d2[0]
        "vmul.f32               q14, q8, d4[0]                  \n\t"   //q14 = q8 * d4[0]
        "vmul.f32               q15, q8, d6[0]                  \n\t"   //q15 = q8 * d6[0]
        "vmla.f32               q12, q9, d0[1]                  \n\t"   //q12 = q9 * d0[1]
        "vmla.f32               q13, q9, d2[1]                  \n\t"   //q13 = q9 * d2[1]
        "vmla.f32               q14, q9, d4[1]                  \n\t"   //q14 = q9 * d4[1]
        "vmla.f32               q15, q9, d6[1]                  \n\t"   //q15 = q9 * d6[1]
        "vmla.f32               q12, q10, d1[0]                 \n\t"   //q12 = q10 * d0[0]
        "vmla.f32               q13, q10, d3[0]                 \n\t"   //q13 = q10 * d2[0]
        "vmla.f32               q14, q10, d5[0]                 \n\t"   //q14 = q10 * d4[0]
        "vmla.f32               q15, q10, d7[0]                 \n\t"   //q15 = q10 * d6[0]
        "vmla.f32               q12, q11, d1[1]                 \n\t"   //q12 = q11 * d0[1]
        "vmla.f32               q13, q11, d3[1]                 \n\t"   //q13 = q11 * d2[1]
        "vmla.f32               q14, q11, d5[1]                 \n\t"   //q14 = q11 * d4[1]
        "vmla.f32               q15, q11, d7[1]                 \n\t"   //q15 = q11 * d6[1]

        "vst1.32                {d24, d25}, [%2]!               \n\t"   //d = q12       
        "vst1.32                {d26, d27}, [%2]!               \n\t"   //d+4 = q13     
        "vst1.32                {d28, d29}, [%2]!               \n\t"   //d+8 = q14     
        "vst1.32                {d30, d31}, [%2]                \n\t"   //d+12 = q15    

        : "+r"(m0), "+r"(m1), "+r"(d) : 
    : "q0", "q1", "q2", "q3", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15",
        "memory"
        );      
}

//matrix vector multiplication. d = m * v
void matvec4_neon(float m[16], float v[4], float d[4])
{
       __asm__ __volatile__ (
        "vld1.32                {d0, d1}, [%1]                  \n\t"   //Q0 = v
        "vld1.32                {d18, d19}, [%0]!               \n\t"   //Q1 = m
        "vld1.32                {d20, d21}, [%0]!               \n\t"   //Q2 = m+4
        "vld1.32                {d22, d23}, [%0]!               \n\t"   //Q3 = m+8
        "vld1.32                {d24, d25}, [%0]!               \n\t"   //Q4 = m+12     
        
        "vmul.f32               q13, q9, d0[0]                  \n\t"   //Q5 = Q1*Q0[0]
        "vmla.f32               q13, q10, d0[1]                 \n\t"   //Q5 += Q1*Q0[1] 
        "vmla.f32               q13, q11, d1[0]                 \n\t"   //Q5 += Q2*Q0[2] 
        "vmla.f32               q13, q12, d1[1]                 \n\t"   //Q5 += Q3*Q0[3]
        
        "vst1.32                {d26, d27}, [%2]                \n\t"   //Q4 = m+12     
        : 
        : "r"(m), "r"(v), "r"(d) 
    : "q0", "q9", "q10","q11", "q12", "q13", "memory"
        );      
}

#endif

