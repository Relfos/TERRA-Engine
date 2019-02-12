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
 * TERRA_NEON
 * Implements optimized math routines in ARM Neon asm
 ***********************************************************************************************************************
}
Unit TERRA_NEON;

{$I terra.inc}

Interface

Procedure enable_runfast(); Cdecl; external;
Function dot2_neon_hfp(v0, v1:PSingle):Single;  Cdecl; external;
Procedure normalize2_neon(v,d:PSingle);  Cdecl; external;
Function dot3_neon_hfp(v0,v1:PSingle):Single; Cdecl; external;
Function cross3_neon(v0,v1,d:PSingle):Single; Cdecl; external;
Procedure normalize3_neon(v,d:PSingle); Cdecl; external;
Procedure normalize4_neon(v,d:Psingle); Cdecl; external;
Function dot4_neon_hfp(v0,v1:PSingle):Single; Cdecl; external;
Procedure matmul3_neon(m0, m1,d:PSingle); Cdecl; external;
Procedure matvec3_neon(m,v,d:PSingle); Cdecl; external;
Procedure matmul4_neon(m0,m1,d:PSingle); Cdecl; external;
Procedure matvec4_neon(m,v,d:PSingle); Cdecl; external;

Implementation

End.
