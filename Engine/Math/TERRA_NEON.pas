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
