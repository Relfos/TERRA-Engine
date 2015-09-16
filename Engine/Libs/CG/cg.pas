{******************************************************************************}
{*                                                                            *}
{*  Copyright (c) 2002-2004, NVIDIA Corporation.                              *}
{*                                                                            *}
{*  Files:    cg.h, cg_datatypes.h, cg_errors.h, cg_profiles.h,               *}
{*            cgGL_profiles.h, cg_bindlocations.h, cg_enums.h                 *}
{*  Content:  NVIDIA Cg core include files                                    *}
{*                                                                            *}
{*  NVIDIA "Cg" Release 1.5 ObjectPascal adaptation by Alexey Barkovoy        *}
{*  E-Mail: clootie@clootie.ru                                                *}
{*                                                                            *}
{*  Modified: 31-Jul-2006                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*     http://www.clootie.ru                                                  *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

(*
 *
 * Copyright (c) 2002-2006, NVIDIA Corporation.
 *
 *
 *
 * NVIDIA Corporation("NVIDIA") supplies this software to you in consideration
 * of your agreement to the following terms, and your use, installation,
 * modification or redistribution of this NVIDIA software constitutes
 * acceptance of these terms.  If you do not agree with these terms, please do
 * not use, install, modify or redistribute this NVIDIA software.
 *
 *
 *
 * In consideration of your agreement to abide by the following terms, and
 * subject to these terms, NVIDIA grants you a personal, non-exclusive license,
 * under NVIDIA’s copyrights in this original NVIDIA software (the "NVIDIA
 * Software"), to use, reproduce, modify and redistribute the NVIDIA
 * Software, with or without modifications, in source and/or binary forms;
 * provided that if you redistribute the NVIDIA Software, you must retain the
 * copyright notice of NVIDIA, this notice and the following text and
 * disclaimers in all such redistributions of the NVIDIA Software. Neither the
 * name, trademarks, service marks nor logos of NVIDIA Corporation may be used
 * to endorse or promote products derived from the NVIDIA Software without
 * specific prior written permission from NVIDIA.  Except as expressly stated
 * in this notice, no other rights or licenses express or implied, are granted
 * by NVIDIA herein, including but not limited to any patent rights that may be
 * infringed by your derivative works or by other works in which the NVIDIA
 * Software may be incorporated. No hardware is licensed hereunder.
 *
 *
 *
 * THE NVIDIA SOFTWARE IS BEING PROVIDED ON AN "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
 * WITHOUT LIMITATION, WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT,
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR ITS USE AND OPERATION
 * EITHER ALONE OR IN COMBINATION WITH OTHER PRODUCTS.
 *
 *
 *
 * IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL,
 * EXEMPLARY, CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, LOST
 * PROFITS; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) OR ARISING IN ANY WAY OUT OF THE USE,
 * REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE NVIDIA SOFTWARE,
 * HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT, TORT (INCLUDING
 * NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF NVIDIA HAS BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

//////////////////////////////////////////////////////////////////////////////
// HISTORY:
// 31-Jul-06 - Alexey Barkovoy:
//   - Updated to Release 1.5 of Cg toolkit (published 06-Apr-2006)
// 12-Jul-05 - Alexey Barkovoy:
//   - Updated to Release 1.4 of Cg toolkit (published 15-Jun-2005)
// 12-Mar-05 - Alexey Barkovoy:
//   - Updated to Release 1.3 of Cg toolkit (published 18-Jan-2005)
// 04-Mar-04 - Alexey Barkovoy:
//   - Updated to Release 1.2 of Cg toolkit (published 25-Feb-2004)
// 21-Mar-03 - Alexey Barkovoy:
//   - Updated to Release 1.1 of Cg toolkit (published 04-Mar-2003)
// 11-Jan-03 - Alexey Barkovoy:
//   - Updated to Release 1.0 of Cg toolkit (published 20-Dec-2002)

{$Include JEDI.inc}
{$MINENUMSIZE 4}

unit cg;

interface

uses Windows;

(*$HPPEMIT '#include "cg.h"' *)

const
  CgLibrary = 'cg.dll';

const
  {$DEFINE CG_VERSION_1_5}
  CG_VERSION_NUM                = 1502;
  {$EXTERNALSYM CG_VERSION_NUM}

(*************************************************************************)
(*** CG Run-Time Library API                                          ***)
(*************************************************************************)

(*************************************************************************)
(*** Data types and enumerants                                         ***)
(*************************************************************************)

type
  PCGbool = ^TCGbool;
  TCGbool = Integer;
  CGbool = TCGbool;
  {$NODEFINE CGbool}
  {$NODEFINE TCGbool}
  {$HPPEMIT 'typedef CGbool TCGbool;'}

const
  CG_FALSE = TCGbool(0);
  {$EXTERNALSYM CG_FALSE}
  CG_TRUE  = TCGbool(1);
  {$EXTERNALSYM CG_TRUE}

type
  _CGcontext = record end;
  PCGcontext = ^_CGcontext;
  CGcontext = PCGcontext;
  {$NODEFINE _CGcontext}
  {$NODEFINE PCGcontext}
  {$NODEFINE CGcontext}
  {$HPPEMIT 'typedef CGcontext PCGcontext;'}

  _CGprogram = record end;
  PCGprogram = ^_CGprogram;
  CGprogram = PCGprogram;
  {$NODEFINE _CGprogram}
  {$NODEFINE PCGprogram}
  {$NODEFINE CGprogram}
  {$HPPEMIT 'typedef CGprogram PCGprogram;'}
  PPCGprogram = ^PCGprogram;

  _CGparameter = record end;
  PCGparameter = ^_CGparameter;
  CGparameter = PCGparameter;
  {$NODEFINE _CGparameter}
  {$NODEFINE PCGparameter}
  {$NODEFINE CGparameter}
  {$HPPEMIT 'typedef CGparameter PCGparameter;'}

  _CGeffect = record end;
  PCGeffect = ^_CGeffect;
  CGeffect = PCGeffect;
  {$NODEFINE _CGeffect}
  {$NODEFINE PCGeffect}
  {$NODEFINE CGeffect}
  {$HPPEMIT 'typedef CGeffect PCGeffect;'}

  _CGtechnique = record end;
  PCGtechnique = ^_CGtechnique;
  CGtechnique = PCGtechnique;
  {$NODEFINE _CGtechnique}
  {$NODEFINE PCGtechnique}
  {$NODEFINE CGtechnique}
  {$HPPEMIT 'typedef CGtechnique PCGtechnique;'}

  _CGpass = record end;
  PCGpass = ^_CGpass;
  CGpass = PCGpass;
  {$NODEFINE _CGpass}
  {$NODEFINE PCGpass}
  {$NODEFINE CGpass}
  {$HPPEMIT 'typedef CGpass PCGpass;'}

  _CGstate = record end;
  PCGstate = ^_CGstate;
  CGstate = PCGstate;
  {$NODEFINE _CGstate}
  {$NODEFINE PCGstate}
  {$NODEFINE CGstate}
  {$HPPEMIT 'typedef CGstate PCGstate;'}

  _CGstateassignment = record end;
  PCGstateassignment = ^_CGstateassignment;
  CGstateassignment = PCGstateassignment;
  {$NODEFINE _CGstateassignment}
  {$NODEFINE PCGstateassignment}
  {$NODEFINE CGstateassignment}
  {$HPPEMIT 'typedef CGstateassignment PCGstateassignment;'}

  _CGannotation = record end;
  PCGannotation = ^_CGannotation;
  CGannotation = PCGannotation;
  {$NODEFINE _CGannotation}
  {$NODEFINE PCGannotation}
  {$NODEFINE CGannotation}
  {$HPPEMIT 'typedef CGannotation PCGannotation;'}

  _CGhandle = record end;
  PCGhandle = ^_CGhandle;
  CGhandle = PCGhandle;
  {$NODEFINE _CGhandle}
  {$NODEFINE PCGhandle}
  {$NODEFINE CGhandle}
  {$HPPEMIT 'typedef CGhandle PCGhandle;'}



 (*
  * The following macro invocations define the supported CG basic data types.
  *
  * The macros have the form :
  *
  *   CG_DATATYPE_MACRO(name, compiler_name, enum_name, base_name, nrows, ncols, pc_name)
  *
  *     name          : The name of the data type.
  *     compiler_name : The name of the data type within the compiler syntax.
  *     enum_name     : The C enumerant.
  *     base_name     : The C enumerant of the base type.
  *     nrows         : Number of rows for matrix types.  Should be 0 other-wise.
  *     ncols         : Number of columns for scalar, vector, and matrix types.
  *     pc_name       : The C enumerant of the parameter class.
  *
  *)
{$IFDEF COMPILER6_UP}
  TCGtype = (
  {$IFDEF BCB}
    CG_Dummy
  {$ELSE}
    CG_UNKNOWN_TYPE,
    CG_STRUCT,
    CG_ARRAY,

    CG_TYPE_START_ENUM = 1024,

    //# define CG_DATATYPE_MACRO(name, compiler_name, enum_name, ncols, nrows) \
    //# define CG_DATATYPE_MACRO(name, compiler_name, enum_name, base_name, ncols, nrows, pc) \
    //  enum_name ,

    CG_HALF,      // CG_DATATYPE_MACRO(Half,half,CG_HALF,0,0)
    CG_HALF2,     // CG_DATATYPE_MACRO(Half2,half2,CG_HALF2,0,0)
    CG_HALF3,     // CG_DATATYPE_MACRO(Half3,half3,CG_HALF3,0,0)
    CG_HALF4,     // CG_DATATYPE_MACRO(Half4,half4,CG_HALF4,0,0)
    CG_HALF1x1,   // CG_DATATYPE_MACRO(Half1x1,half1x1,CG_HALF1x1,1,1)
    CG_HALF1x2,   // CG_DATATYPE_MACRO(Half1x2,half1x2,CG_HALF1x2,1,2)
    CG_HALF1x3,   // CG_DATATYPE_MACRO(Half1x3,half1x3,CG_HALF1x3,1,3)
    CG_HALF1x4,   // CG_DATATYPE_MACRO(Half1x4,half1x4,CG_HALF1x4,1,4)
    CG_HALF2x1,   // CG_DATATYPE_MACRO(Half2x1,half2x1,CG_HALF2x1,2,1)
    CG_HALF2x2,   // CG_DATATYPE_MACRO(Half2x2,half2x2,CG_HALF2x2,2,2)
    CG_HALF2x3,   // CG_DATATYPE_MACRO(Half2x3,half2x3,CG_HALF2x3,2,3)
    CG_HALF2x4,   // CG_DATATYPE_MACRO(Half2x4,half2x4,CG_HALF2x4,2,4)
    CG_HALF3x1,   // CG_DATATYPE_MACRO(Half3x1,half3x1,CG_HALF3x1,3,1)
    CG_HALF3x2,   // CG_DATATYPE_MACRO(Half3x2,half3x2,CG_HALF3x2,3,2)
    CG_HALF3x3,   // CG_DATATYPE_MACRO(Half3x3,half3x3,CG_HALF3x3,3,3)
    CG_HALF3x4,   // CG_DATATYPE_MACRO(Half3x4,half3x4,CG_HALF3x4,3,4)
    CG_HALF4x1,   // CG_DATATYPE_MACRO(Half4x1,half4x1,CG_HALF4x1,4,1)
    CG_HALF4x2,   // CG_DATATYPE_MACRO(Half4x2,half4x2,CG_HALF4x2,4,2)
    CG_HALF4x3,   // CG_DATATYPE_MACRO(Half4x3,half4x3,CG_HALF4x3,4,3)
    CG_HALF4x4,   // CG_DATATYPE_MACRO(Half4x4,half4x4,CG_HALF4x4,4,4)
    CG_FLOAT,     // CG_DATATYPE_MACRO(Float,float,CG_FLOAT,0,0)
    CG_FLOAT2,    // CG_DATATYPE_MACRO(Float2,float2,CG_FLOAT2,0,0)
    CG_FLOAT3,    // CG_DATATYPE_MACRO(Float3,float3,CG_FLOAT3,0,0)
    CG_FLOAT4,    // CG_DATATYPE_MACRO(Float4,float4,CG_FLOAT4,0,0)
    CG_FLOAT1x1,  // CG_DATATYPE_MACRO(Float1x1,float1x1,CG_FLOAT1x1,1,1)
    CG_FLOAT1x2,  // CG_DATATYPE_MACRO(Float1x2,float1x2,CG_FLOAT1x2,1,2)
    CG_FLOAT1x3,  // CG_DATATYPE_MACRO(Float1x3,float1x3,CG_FLOAT1x3,1,3)
    CG_FLOAT1x4,  // CG_DATATYPE_MACRO(Float1x4,float1x4,CG_FLOAT1x4,1,4)
    CG_FLOAT2x1,  // CG_DATATYPE_MACRO(Float2x1,float2x1,CG_FLOAT2x1,2,1)
    CG_FLOAT2x2,  // CG_DATATYPE_MACRO(Float2x2,float2x2,CG_FLOAT2x2,2,2)
    CG_FLOAT2x3,  // CG_DATATYPE_MACRO(Float2x3,float2x3,CG_FLOAT2x3,2,3)
    CG_FLOAT2x4,  // CG_DATATYPE_MACRO(Float2x4,float2x4,CG_FLOAT2x4,2,4)
    CG_FLOAT3x1,  // CG_DATATYPE_MACRO(Float3x1,float3x1,CG_FLOAT3x1,3,1)
    CG_FLOAT3x2,  // CG_DATATYPE_MACRO(Float3x2,float3x2,CG_FLOAT3x2,3,2)
    CG_FLOAT3x3,  // CG_DATATYPE_MACRO(Float3x3,float3x3,CG_FLOAT3x3,3,3)
    CG_FLOAT3x4,  // CG_DATATYPE_MACRO(Float3x4,float3x4,CG_FLOAT3x4,3,4)
    CG_FLOAT4x1,  // CG_DATATYPE_MACRO(Float4x1,float4x1,CG_FLOAT4x1,4,1)
    CG_FLOAT4x2,  // CG_DATATYPE_MACRO(Float4x2,float4x2,CG_FLOAT4x2,4,2)
    CG_FLOAT4x3,  // CG_DATATYPE_MACRO(Float4x3,float4x3,CG_FLOAT4x3,4,3)
    CG_FLOAT4x4,  // CG_DATATYPE_MACRO(Float4x4,float4x4,CG_FLOAT4x4,4,4)
    CG_SAMPLER1D, // CG_DATATYPE_MACRO(Sampler1D,sampler1D,CG_SAMPLER1D,0,0)
    CG_SAMPLER2D, // CG_DATATYPE_MACRO(Sampler2D,sampler2D,CG_SAMPLER2D,0,0)
    CG_SAMPLER3D, // CG_DATATYPE_MACRO(Sampler3D,sampler3D,CG_SAMPLER3D,0,0)
    CG_SAMPLERRECT, // CG_DATATYPE_MACRO(SamplerRECT,samplerRECT,CG_SAMPLERRECT,0,0)
    CG_SAMPLERCUBE, // CG_DATATYPE_MACRO(SamplerCUBE,samplerCUBE,CG_SAMPLERCUBE,0,0)
    CG_FIXED,     // CG_DATATYPE_MACRO(Fixed,fixed,CG_FIXED,0,0)
    CG_FIXED2,    // CG_DATATYPE_MACRO(Fixed2,fixed2,CG_FIXED2,0,0)
    CG_FIXED3,    // CG_DATATYPE_MACRO(Fixed3,fixed3,CG_FIXED3,0,0)
    CG_FIXED4,    // CG_DATATYPE_MACRO(Fixed4,fixed4,CG_FIXED4,0,0)
    CG_FIXED1x1,  // CG_DATATYPE_MACRO(Fixed1x1,fixed1x1,CG_FIXED1x1,1,1)
    CG_FIXED1x2,  // CG_DATATYPE_MACRO(Fixed1x2,fixed1x2,CG_FIXED1x2,1,2)
    CG_FIXED1x3,  // CG_DATATYPE_MACRO(Fixed1x3,fixed1x3,CG_FIXED1x3,1,3)
    CG_FIXED1x4,  // CG_DATATYPE_MACRO(Fixed1x4,fixed1x4,CG_FIXED1x4,1,4)
    CG_FIXED2x1,  // CG_DATATYPE_MACRO(Fixed2x1,fixed2x1,CG_FIXED2x1,2,1)
    CG_FIXED2x2,  // CG_DATATYPE_MACRO(Fixed2x2,fixed2x2,CG_FIXED2x2,2,2)
    CG_FIXED2x3,  // CG_DATATYPE_MACRO(Fixed2x3,fixed2x3,CG_FIXED2x3,2,3)
    CG_FIXED2x4,  // CG_DATATYPE_MACRO(Fixed2x4,fixed2x4,CG_FIXED2x4,2,4)
    CG_FIXED3x1,  // CG_DATATYPE_MACRO(Fixed3x1,fixed3x1,CG_FIXED3x1,3,1)
    CG_FIXED3x2,  // CG_DATATYPE_MACRO(Fixed3x2,fixed3x2,CG_FIXED3x2,3,2)
    CG_FIXED3x3,  // CG_DATATYPE_MACRO(Fixed3x3,fixed3x3,CG_FIXED3x3,3,3)
    CG_FIXED3x4,  // CG_DATATYPE_MACRO(Fixed3x4,fixed3x4,CG_FIXED3x4,3,4)
    CG_FIXED4x1,  // CG_DATATYPE_MACRO(Fixed4x1,fixed4x1,CG_FIXED4x1,4,1)
    CG_FIXED4x2,  // CG_DATATYPE_MACRO(Fixed4x2,fixed4x2,CG_FIXED4x2,4,2)
    CG_FIXED4x3,  // CG_DATATYPE_MACRO(Fixed4x3,fixed4x3,CG_FIXED4x3,4,3)
    CG_FIXED4x4,  // CG_DATATYPE_MACRO(Fixed4x4,fixed4x4,CG_FIXED4x4,4,4)
    CG_HALF1,     // CG_DATATYPE_MACRO(Half1,half1,CG_HALF1,0,0)
    CG_FLOAT1,    // CG_DATATYPE_MACRO(Float1,float1,CG_FLOAT1,0,0)
    CG_FIXED1,    // CG_DATATYPE_MACRO(Fixed1,fixed1,CG_FIXED1,0,0)
    CG_INT,       // CG_DATATYPE_MACRO(Int,int,CG_INT,0,1)
    CG_INT1,      // CG_DATATYPE_MACRO(Int1,int1,CG_INT1,0,1)
    CG_INT2,      // CG_DATATYPE_MACRO(Int2,int2,CG_INT2,0,2)
    CG_INT3,      // CG_DATATYPE_MACRO(Int3,int3,CG_INT3,0,3)
    CG_INT4,      // CG_DATATYPE_MACRO(Int4,int4,CG_INT4,0,4)
    CG_INT1x1,    // CG_DATATYPE_MACRO(Int1x1,int1x1,CG_INT1x1,1,1)
    CG_INT1x2,    // CG_DATATYPE_MACRO(Int1x2,int1x2,CG_INT1x2,1,2)
    CG_INT1x3,    // CCG_DATATYPE_MACRO(Int1x3,int1x3,CG_INT1x3,1,3)
    CG_INT1x4,    // CCG_DATATYPE_MACRO(Int1x4,int1x4,CG_INT1x4,1,4)
    CG_INT2x1,    // CCG_DATATYPE_MACRO(Int2x1,int2x1,CG_INT2x1,2,1)
    CG_INT2x2,    // CCG_DATATYPE_MACRO(Int2x2,int2x2,CG_INT2x2,2,2)
    CG_INT2x3,    // CCG_DATATYPE_MACRO(Int2x3,int2x3,CG_INT2x3,2,3)
    CG_INT2x4,    // CCG_DATATYPE_MACRO(Int2x4,int2x4,CG_INT2x4,2,4)
    CG_INT3x1,    // CCG_DATATYPE_MACRO(Int3x1,int3x1,CG_INT3x1,3,1)
    CG_INT3x2,    // CCG_DATATYPE_MACRO(Int3x2,int3x2,CG_INT3x2,3,2)
    CG_INT3x3,    // CCG_DATATYPE_MACRO(Int3x3,int3x3,CG_INT3x3,3,3)
    CG_INT3x4,    // CCG_DATATYPE_MACRO(Int3x4,int3x4,CG_INT3x4,3,4)
    CG_INT4x1,    // CCG_DATATYPE_MACRO(Int4x1,int4x1,CG_INT4x1,4,1)
    CG_INT4x2,    // CCG_DATATYPE_MACRO(Int4x2,int4x2,CG_INT4x2,4,2)
    CG_INT4x3,    // CCG_DATATYPE_MACRO(Int4x3,int4x3,CG_INT4x3,4,3)
    CG_INT4x4,    // CCG_DATATYPE_MACRO(Int4x4,int4x4,CG_INT4x4,4,4)
    CG_BOOL,      // CG_DATATYPE_MACRO(Bool,bool,CG_BOOL,0,1)
    CG_BOOL1,     // CG_DATATYPE_MACRO(Bool1,bool1,CG_BOOL1,0,1)
    CG_BOOL2,     // CG_DATATYPE_MACRO(Bool2,bool2,CG_BOOL2,0,2)
    CG_BOOL3,     // CG_DATATYPE_MACRO(Bool3,bool3,CG_BOOL3,0,3)
    CG_BOOL4,     // CG_DATATYPE_MACRO(Bool4,bool4,CG_BOOL4,0,4)
    CG_BOOL1x1,   // CG_DATATYPE_MACRO(Bool1x1,bool1x1,CG_BOOL1x1,1,1)
    CG_BOOL1x2,   // CG_DATATYPE_MACRO(Bool1x2,bool1x2,CG_BOOL1x2,1,2)
    CG_BOOL1x3,   // CG_DATATYPE_MACRO(Bool1x3,bool1x3,CG_BOOL1x3,1,3)
    CG_BOOL1x4,   // CG_DATATYPE_MACRO(Bool1x4,bool1x4,CG_BOOL1x4,1,4)
    CG_BOOL2x1,   // CG_DATATYPE_MACRO(Bool2x1,bool2x1,CG_BOOL2x1,2,1)
    CG_BOOL2x2,   // CG_DATATYPE_MACRO(Bool2x2,bool2x2,CG_BOOL2x2,2,2)
    CG_BOOL2x3,   // CG_DATATYPE_MACRO(Bool2x3,bool2x3,CG_BOOL2x3,2,3)
    CG_BOOL2x4,   // CG_DATATYPE_MACRO(Bool2x4,bool2x4,CG_BOOL2x4,2,4)
    CG_BOOL3x1,   // CG_DATATYPE_MACRO(Bool3x1,bool3x1,CG_BOOL3x1,3,1)
    CG_BOOL3x2,   // CG_DATATYPE_MACRO(Bool3x2,bool3x2,CG_BOOL3x2,3,2)
    CG_BOOL3x3,   // CG_DATATYPE_MACRO(Bool3x3,bool3x3,CG_BOOL3x3,3,3)
    CG_BOOL3x4,   // CG_DATATYPE_MACRO(Bool3x4,bool3x4,CG_BOOL3x4,3,4)
    CG_BOOL4x1,   // CG_DATATYPE_MACRO(Bool4x1,bool4x1,CG_BOOL4x1,4,1)
    CG_BOOL4x2,   // CG_DATATYPE_MACRO(Bool4x2,bool4x2,CG_BOOL4x2,4,2)
    CG_BOOL4x3,   // CG_DATATYPE_MACRO(Bool4x3,bool4x3,CG_BOOL4x3,4,3)
    CG_BOOL4x4,   // CG_DATATYPE_MACRO(Bool4x4,bool4x4,CG_BOOL4x4,4,4)
    CG_STRING,    // CG_DATATYPE_MACRO(String,string,CG_STRING,CG_STRING,0,1,CG_PARAMETERCLASS_OBJECT)
    CG_PROGRAM_TYPE, // CG_DATATYPE_MACRO(Program,program,CG_PROGRAM_TYPE,CG_PROGRAM_TYPE,0,0,CG_PARAMETERCLASS_OBJECT)
    CG_TEXTURE    // CG_DATATYPE_MACRO(Texture,texture,CG_TEXTURE,CG_TEXTURE,0,0,CG_PARAMETERCLASS_OBJECT)

  {$ENDIF}
  );
{$ELSE}
const
  CG_UNKNOWN_TYPE = 0;
  {$EXTERNALSYM CG_UNKNOWN_TYPE}
  CG_STRUCT       = 1;
  {$EXTERNALSYM CG_STRUCT}
  CG_ARRAY        = 2;
  {$EXTERNALSYM CG_ARRAY}

  CG_TYPE_START_ENUM = 1024;
  {$EXTERNALSYM CG_TYPE_START_ENUM}

  //# define CG_DATATYPE_MACRO(name, compiler_name, enum_name, ncols, nrows) \
  //  enum_name ,
  CG_HALF = 1025;      // CG_DATATYPE_MACRO(Half,half,CG_HALF,0,0)
  {$EXTERNALSYM CG_HALF}
  CG_HALF2 = 1026;     // CG_DATATYPE_MACRO(Half2,half2,CG_HALF2,0,0)
  {$EXTERNALSYM CG_HALF2}
  CG_HALF3 = 1027;     // CG_DATATYPE_MACRO(Half3,half3,CG_HALF3,0,0)
  {$EXTERNALSYM CG_HALF3}
  CG_HALF4 = 1028;     // CG_DATATYPE_MACRO(Half4,half4,CG_HALF4,0,0)
  {$EXTERNALSYM CG_HALF4}
  CG_HALF1x1 = 1029;   // CG_DATATYPE_MACRO(Half1x1,half1x1,CG_HALF1x1,1,1)
  {$EXTERNALSYM CG_HALF1x1}
  CG_HALF1x2 = 1030;   // CG_DATATYPE_MACRO(Half1x2,half1x2,CG_HALF1x2,1,2)
  {$EXTERNALSYM CG_HALF1x2}
  CG_HALF1x3 = 1031;   // CG_DATATYPE_MACRO(Half1x3,half1x3,CG_HALF1x3,1,3)
  {$EXTERNALSYM CG_HALF1x3}
  CG_HALF1x4 = 1032;   // CG_DATATYPE_MACRO(Half1x4,half1x4,CG_HALF1x4,1,4)
  {$EXTERNALSYM CG_HALF1x4}
  CG_HALF2x1 = 1033;   // CG_DATATYPE_MACRO(Half2x1,half2x1,CG_HALF2x1,2,1)
  {$EXTERNALSYM CG_HALF2x1}
  CG_HALF2x2 = 1034;   // CG_DATATYPE_MACRO(Half2x2,half2x2,CG_HALF2x2,2,2)
  {$EXTERNALSYM CG_HALF2x2}
  CG_HALF2x3 = 1035;   // CG_DATATYPE_MACRO(Half2x3,half2x3,CG_HALF2x3,2,3)
  {$EXTERNALSYM CG_HALF2x3}
  CG_HALF2x4 = 1036;   // CG_DATATYPE_MACRO(Half2x4,half2x4,CG_HALF2x4,2,4)
  {$EXTERNALSYM CG_HALF2x4}
  CG_HALF3x1 = 1037;   // CG_DATATYPE_MACRO(Half3x1,half3x1,CG_HALF3x1,3,1)
  {$EXTERNALSYM CG_HALF3x1}
  CG_HALF3x2 = 1038;   // CG_DATATYPE_MACRO(Half3x2,half3x2,CG_HALF3x2,3,2)
  {$EXTERNALSYM CG_HALF3x2}
  CG_HALF3x3 = 1039;   // CG_DATATYPE_MACRO(Half3x3,half3x3,CG_HALF3x3,3,3)
  {$EXTERNALSYM CG_HALF3x3}
  CG_HALF3x4 = 1040;   // CG_DATATYPE_MACRO(Half3x4,half3x4,CG_HALF3x4,3,4)
  {$EXTERNALSYM CG_HALF3x4}
  CG_HALF4x1 = 1041;   // CG_DATATYPE_MACRO(Half4x1,half4x1,CG_HALF4x1,4,1)
  {$EXTERNALSYM CG_HALF4x1}
  CG_HALF4x2 = 1042;   // CG_DATATYPE_MACRO(Half4x2,half4x2,CG_HALF4x2,4,2)
  {$EXTERNALSYM CG_HALF4x2}
  CG_HALF4x3 = 1043;   // CG_DATATYPE_MACRO(Half4x3,half4x3,CG_HALF4x3,4,3)
  {$EXTERNALSYM CG_HALF4x3}
  CG_HALF4x4 = 1044;   // CG_DATATYPE_MACRO(Half4x4,half4x4,CG_HALF4x4,4,4)
  {$EXTERNALSYM CG_HALF4x4}
  CG_FLOAT = 1045;     // CG_DATATYPE_MACRO(Float,float,CG_FLOAT,0,0)
  {$EXTERNALSYM CG_FLOAT}
  CG_FLOAT2 = 1046;    // CG_DATATYPE_MACRO(Float2,float2,CG_FLOAT2,0,0)
  {$EXTERNALSYM CG_FLOAT2}
  CG_FLOAT3 = 1047;    // CG_DATATYPE_MACRO(Float3,float3,CG_FLOAT3,0,0)
  {$EXTERNALSYM CG_FLOAT3}
  CG_FLOAT4 = 1048;    // CG_DATATYPE_MACRO(Float4,float4,CG_FLOAT4,0,0)
  {$EXTERNALSYM CG_FLOAT4}
  CG_FLOAT1x1 = 1049;  // CG_DATATYPE_MACRO(Float1x1,float1x1,CG_FLOAT1x1,1,1)
  {$EXTERNALSYM CG_FLOAT1x1}
  CG_FLOAT1x2 = 1050;  // CG_DATATYPE_MACRO(Float1x2,float1x2,CG_FLOAT1x2,1,2)
  {$EXTERNALSYM CG_FLOAT1x2}
  CG_FLOAT1x3 = 1051;  // CG_DATATYPE_MACRO(Float1x3,float1x3,CG_FLOAT1x3,1,3)
  {$EXTERNALSYM CG_FLOAT1x3}
  CG_FLOAT1x4 = 1052;  // CG_DATATYPE_MACRO(Float1x4,float1x4,CG_FLOAT1x4,1,4)
  {$EXTERNALSYM CG_FLOAT1x4}
  CG_FLOAT2x1 = 1053;  // CG_DATATYPE_MACRO(Float2x1,float2x1,CG_FLOAT2x1,2,1)
  {$EXTERNALSYM CG_FLOAT2x1}
  CG_FLOAT2x2 = 1054;  // CG_DATATYPE_MACRO(Float2x2,float2x2,CG_FLOAT2x2,2,2)
  {$EXTERNALSYM CG_FLOAT2x2}
  CG_FLOAT2x3 = 1055;  // CG_DATATYPE_MACRO(Float2x3,float2x3,CG_FLOAT2x3,2,3)
  {$EXTERNALSYM CG_FLOAT2x3}
  CG_FLOAT2x4 = 1056;  // CG_DATATYPE_MACRO(Float2x4,float2x4,CG_FLOAT2x4,2,4)
  {$EXTERNALSYM CG_FLOAT2x4}
  CG_FLOAT3x1 = 1057;  // CG_DATATYPE_MACRO(Float3x1,float3x1,CG_FLOAT3x1,3,1)
  {$EXTERNALSYM CG_FLOAT3x1}
  CG_FLOAT3x2 = 1058;  // CG_DATATYPE_MACRO(Float3x2,float3x2,CG_FLOAT3x2,3,2)
  {$EXTERNALSYM CG_FLOAT3x2}
  CG_FLOAT3x3 = 1059;  // CG_DATATYPE_MACRO(Float3x3,float3x3,CG_FLOAT3x3,3,3)
  {$EXTERNALSYM CG_FLOAT3x3}
  CG_FLOAT3x4 = 1060;  // CG_DATATYPE_MACRO(Float3x4,float3x4,CG_FLOAT3x4,3,4)
  {$EXTERNALSYM CG_FLOAT3x4}
  CG_FLOAT4x1 = 1061;  // CG_DATATYPE_MACRO(Float4x1,float4x1,CG_FLOAT4x1,4,1)
  {$EXTERNALSYM CG_FLOAT4x1}
  CG_FLOAT4x2 = 1062;  // CG_DATATYPE_MACRO(Float4x2,float4x2,CG_FLOAT4x2,4,2)
  {$EXTERNALSYM CG_FLOAT4x2}
  CG_FLOAT4x3 = 1063;  // CG_DATATYPE_MACRO(Float4x3,float4x3,CG_FLOAT4x3,4,3)
  {$EXTERNALSYM CG_FLOAT4x3}
  CG_FLOAT4x4 = 1064;  // CG_DATATYPE_MACRO(Float4x4,float4x4,CG_FLOAT4x4,4,4)
  {$EXTERNALSYM CG_FLOAT4x4}
  CG_SAMPLER1D = 1065; // CG_DATATYPE_MACRO(Sampler1D,sampler1D,CG_SAMPLER1D,0,0)
  {$EXTERNALSYM CG_SAMPLER1D}
  CG_SAMPLER2D = 1066; // CG_DATATYPE_MACRO(Sampler2D,sampler2D,CG_SAMPLER2D,0,0)
  {$EXTERNALSYM CG_SAMPLER2D}
  CG_SAMPLER3D = 1067; // CG_DATATYPE_MACRO(Sampler3D,sampler3D,CG_SAMPLER3D,0,0)
  {$EXTERNALSYM CG_SAMPLER3D}
  CG_SAMPLERRECT = 1068; // CG_DATATYPE_MACRO(SamplerRECT,samplerRECT,CG_SAMPLERRECT,0,0)
  {$EXTERNALSYM CG_SAMPLERRECT}
  CG_SAMPLERCUBE = 1069; // CG_DATATYPE_MACRO(SamplerCUBE,samplerCUBE,CG_SAMPLERCUBE,0,0)
  {$EXTERNALSYM CG_SAMPLERCUBE}
  CG_FIXED = 1070;     // CG_DATATYPE_MACRO(Fixed,fixed,CG_FIXED,0,0)
  {$EXTERNALSYM CG_FIXED}
  CG_FIXED2 = 1071;    // CG_DATATYPE_MACRO(Fixed2,fixed2,CG_FIXED2,0,0)
  {$EXTERNALSYM CG_FIXED2}
  CG_FIXED3 = 1072;    // CG_DATATYPE_MACRO(Fixed3,fixed3,CG_FIXED3,0,0)
  {$EXTERNALSYM CG_FIXED3}
  CG_FIXED4 = 1073;    // CG_DATATYPE_MACRO(Fixed4,fixed4,CG_FIXED4,0,0)
  {$EXTERNALSYM CG_FIXED4}
  CG_FIXED1x1 = 1074;  // CG_DATATYPE_MACRO(Fixed1x1,fixed1x1,CG_FIXED1x1,1,1)
  {$EXTERNALSYM CG_FIXED1x1}
  CG_FIXED1x2 = 1075;  // CG_DATATYPE_MACRO(Fixed1x2,fixed1x2,CG_FIXED1x2,1,2)
  {$EXTERNALSYM CG_FIXED1x2}
  CG_FIXED1x3 = 1076;  // CG_DATATYPE_MACRO(Fixed1x3,fixed1x3,CG_FIXED1x3,1,3)
  {$EXTERNALSYM CG_FIXED1x3}
  CG_FIXED1x4 = 1077;  // CG_DATATYPE_MACRO(Fixed1x4,fixed1x4,CG_FIXED1x4,1,4)
  {$EXTERNALSYM CG_FIXED1x4}
  CG_FIXED2x1 = 1078;  // CG_DATATYPE_MACRO(Fixed2x1,fixed2x1,CG_FIXED2x1,2,1)
  {$EXTERNALSYM CG_FIXED2x1}
  CG_FIXED2x2 = 1079;  // CG_DATATYPE_MACRO(Fixed2x2,fixed2x2,CG_FIXED2x2,2,2)
  {$EXTERNALSYM CG_FIXED2x2}
  CG_FIXED2x3 = 1080;  // CG_DATATYPE_MACRO(Fixed2x3,fixed2x3,CG_FIXED2x3,2,3)
  {$EXTERNALSYM CG_FIXED2x3}
  CG_FIXED2x4 = 1081;  // CG_DATATYPE_MACRO(Fixed2x4,fixed2x4,CG_FIXED2x4,2,4)
  {$EXTERNALSYM CG_FIXED2x4}
  CG_FIXED3x1 = 1082;  // CG_DATATYPE_MACRO(Fixed3x1,fixed3x1,CG_FIXED3x1,3,1)
  {$EXTERNALSYM CG_FIXED3x1}
  CG_FIXED3x2 = 1083;  // CG_DATATYPE_MACRO(Fixed3x2,fixed3x2,CG_FIXED3x2,3,2)
  {$EXTERNALSYM CG_FIXED3x2}
  CG_FIXED3x3 = 1084;  // CG_DATATYPE_MACRO(Fixed3x3,fixed3x3,CG_FIXED3x3,3,3)
  {$EXTERNALSYM CG_FIXED3x3}
  CG_FIXED3x4 = 1085;  // CG_DATATYPE_MACRO(Fixed3x4,fixed3x4,CG_FIXED3x4,3,4)
  {$EXTERNALSYM CG_FIXED3x4}
  CG_FIXED4x1 = 1086;  // CG_DATATYPE_MACRO(Fixed4x1,fixed4x1,CG_FIXED4x1,4,1)
  {$EXTERNALSYM CG_FIXED4x1}
  CG_FIXED4x2 = 1087;  // CG_DATATYPE_MACRO(Fixed4x2,fixed4x2,CG_FIXED4x2,4,2)
  {$EXTERNALSYM CG_FIXED4x2}
  CG_FIXED4x3 = 1088;  // CG_DATATYPE_MACRO(Fixed4x3,fixed4x3,CG_FIXED4x3,4,3)
  {$EXTERNALSYM CG_FIXED4x3}
  CG_FIXED4x4 = 1089;  // CG_DATATYPE_MACRO(Fixed4x4,fixed4x4,CG_FIXED4x4,4,4)
  {$EXTERNALSYM CG_FIXED4x4}
  CG_HALF1 = 1090;     // CG_DATATYPE_MACRO(Half1,half1,CG_HALF1,0,0)
  {$EXTERNALSYM CG_HALF1}
  CG_FLOAT1 = 1091;    // CG_DATATYPE_MACRO(Float1,float1,CG_FLOAT1,0,0)
  {$EXTERNALSYM CG_FLOAT1}
  CG_FIXED1 = 1092;    // CG_DATATYPE_MACRO(Fixed1,fixed1,CG_FIXED1,0,0)
  {$EXTERNALSYM CG_FIXED1}
  CG_INT = 1093;       // CG_DATATYPE_MACRO(Int,int,CG_INT,0,1)
  {$EXTERNALSYM CG_INT}
  CG_INT1 = 1094;      // CG_DATATYPE_MACRO(Int1,int1,CG_INT1,0,1)
  {$EXTERNALSYM CG_INT1}
  CG_INT2 = 1095;      // CG_DATATYPE_MACRO(Int2,int2,CG_INT2,0,2)
  {$EXTERNALSYM CG_INT2}
  CG_INT3 = 1096;      // CG_DATATYPE_MACRO(Int3,int3,CG_INT3,0,3)
  {$EXTERNALSYM CG_INT3}
  CG_INT4 = 1097;      // CG_DATATYPE_MACRO(Int4,int4,CG_INT4,0,4)
  {$EXTERNALSYM CG_INT4}
  CG_INT1x1 = 1098;    // CG_DATATYPE_MACRO(Int1x1,int1x1,CG_INT1x1,1,1)
  {$EXTERNALSYM CG_INT1x1}
  CG_INT1x2 = 1099;    // CG_DATATYPE_MACRO(Int1x2,int1x2,CG_INT1x2,1,2)
  {$EXTERNALSYM CG_INT1x2}
  CG_INT1x3 = 1100;    // CCG_DATATYPE_MACRO(Int1x3,int1x3,CG_INT1x3,1,3)
  {$EXTERNALSYM CG_INT1x3}
  CG_INT1x4 = 1101;    // CCG_DATATYPE_MACRO(Int1x4,int1x4,CG_INT1x4,1,4)
  {$EXTERNALSYM CG_INT1x4}
  CG_INT2x1 = 1102;    // CCG_DATATYPE_MACRO(Int2x1,int2x1,CG_INT2x1,2,1)
  {$EXTERNALSYM CG_INT2x1}
  CG_INT2x2 = 1103;    // CCG_DATATYPE_MACRO(Int2x2,int2x2,CG_INT2x2,2,2)
  {$EXTERNALSYM CG_INT2x2}
  CG_INT2x3 = 1104;    // CCG_DATATYPE_MACRO(Int2x3,int2x3,CG_INT2x3,2,3)
  {$EXTERNALSYM CG_INT2x3}
  CG_INT2x4 = 1105;    // CCG_DATATYPE_MACRO(Int2x4,int2x4,CG_INT2x4,2,4)
  {$EXTERNALSYM CG_INT2x4}
  CG_INT3x1 = 1106;    // CCG_DATATYPE_MACRO(Int3x1,int3x1,CG_INT3x1,3,1)
  {$EXTERNALSYM CG_INT3x1}
  CG_INT3x2 = 1107;    // CCG_DATATYPE_MACRO(Int3x2,int3x2,CG_INT3x2,3,2)
  {$EXTERNALSYM CG_INT3x2}
  CG_INT3x3 = 1108;    // CCG_DATATYPE_MACRO(Int3x3,int3x3,CG_INT3x3,3,3)
  {$EXTERNALSYM CG_INT3x3}
  CG_INT3x4 = 1109;    // CCG_DATATYPE_MACRO(Int3x4,int3x4,CG_INT3x4,3,4)
  {$EXTERNALSYM CG_INT3x4}
  CG_INT4x1 = 1110;    // CCG_DATATYPE_MACRO(Int4x1,int4x1,CG_INT4x1,4,1)
  {$EXTERNALSYM CG_INT4x1}
  CG_INT4x2 = 1111;    // CCG_DATATYPE_MACRO(Int4x2,int4x2,CG_INT4x2,4,2)
  {$EXTERNALSYM CG_INT4x2}
  CG_INT4x3 = 1112;    // CCG_DATATYPE_MACRO(Int4x3,int4x3,CG_INT4x3,4,3)
  {$EXTERNALSYM CG_INT4x3}
  CG_INT4x4 = 1113;    // CCG_DATATYPE_MACRO(Int4x4,int4x4,CG_INT4x4,4,4)
  {$EXTERNALSYM CG_INT4x4}
  CG_BOOL = 1114;      // CG_DATATYPE_MACRO(Bool,bool,CG_BOOL,0,1)
  {$EXTERNALSYM CG_BOOL}
  CG_BOOL1 = 1115;     // CG_DATATYPE_MACRO(Bool1,bool1,CG_BOOL1,0,1)
  {$EXTERNALSYM CG_BOOL1}
  CG_BOOL2 = 1116;     // CG_DATATYPE_MACRO(Bool2,bool2,CG_BOOL2,0,2)
  {$EXTERNALSYM CG_BOOL2}
  CG_BOOL3 = 1117;     // CG_DATATYPE_MACRO(Bool3,bool3,CG_BOOL3,0,3)
  {$EXTERNALSYM CG_BOOL3}
  CG_BOOL4 = 1118;     // CG_DATATYPE_MACRO(Bool4,bool4,CG_BOOL4,0,4)
  {$EXTERNALSYM CG_BOOL4}
  CG_BOOL1x1 = 1119;   // CG_DATATYPE_MACRO(Bool1x1,bool1x1,CG_BOOL1x1,1,1)
  {$EXTERNALSYM CG_BOOL1x1}
  CG_BOOL1x2 = 1120;   // CG_DATATYPE_MACRO(Bool1x2,bool1x2,CG_BOOL1x2,1,2)
  {$EXTERNALSYM CG_BOOL1x2}
  CG_BOOL1x3 = 1121;   // CG_DATATYPE_MACRO(Bool1x3,bool1x3,CG_BOOL1x3,1,3)
  {$EXTERNALSYM CG_BOOL1x3}
  CG_BOOL1x4 = 1122;   // CG_DATATYPE_MACRO(Bool1x4,bool1x4,CG_BOOL1x4,1,4)
  {$EXTERNALSYM CG_BOOL1x4}
  CG_BOOL2x1 = 1123;   // CG_DATATYPE_MACRO(Bool2x1,bool2x1,CG_BOOL2x1,2,1)
  {$EXTERNALSYM CG_BOOL2x1}
  CG_BOOL2x2 = 1124;   // CG_DATATYPE_MACRO(Bool2x2,bool2x2,CG_BOOL2x2,2,2)
  {$EXTERNALSYM CG_BOOL2x2}
  CG_BOOL2x3 = 1125;   // CG_DATATYPE_MACRO(Bool2x3,bool2x3,CG_BOOL2x3,2,3)
  {$EXTERNALSYM CG_BOOL2x3}
  CG_BOOL2x4 = 1126;   // CG_DATATYPE_MACRO(Bool2x4,bool2x4,CG_BOOL2x4,2,4)
  {$EXTERNALSYM CG_BOOL2x4}
  CG_BOOL3x1 = 1127;   // CG_DATATYPE_MACRO(Bool3x1,bool3x1,CG_BOOL3x1,3,1)
  {$EXTERNALSYM CG_BOOL3x1}
  CG_BOOL3x2 = 1128;   // CG_DATATYPE_MACRO(Bool3x2,bool3x2,CG_BOOL3x2,3,2)
  {$EXTERNALSYM CG_BOOL3x2}
  CG_BOOL3x3 = 1129;   // CG_DATATYPE_MACRO(Bool3x3,bool3x3,CG_BOOL3x3,3,3)
  {$EXTERNALSYM CG_BOOL3x3}
  CG_BOOL3x4 = 1130;   // CG_DATATYPE_MACRO(Bool3x4,bool3x4,CG_BOOL3x4,3,4)
  {$EXTERNALSYM CG_BOOL3x4}
  CG_BOOL4x1 = 1131;   // CG_DATATYPE_MACRO(Bool4x1,bool4x1,CG_BOOL4x1,4,1)
  {$EXTERNALSYM CG_BOOL4x1}
  CG_BOOL4x2 = 1132;   // CG_DATATYPE_MACRO(Bool4x2,bool4x2,CG_BOOL4x2,4,2)
  {$EXTERNALSYM CG_BOOL4x2}
  CG_BOOL4x3 = 1133;   // CG_DATATYPE_MACRO(Bool4x3,bool4x3,CG_BOOL4x3,4,3)
  {$EXTERNALSYM CG_BOOL4x3}
  CG_BOOL4x4 = 1134;   // CG_DATATYPE_MACRO(Bool4x4,bool4x4,CG_BOOL4x4,4,4)
  {$EXTERNALSYM CG_BOOL4x4}
  CG_STRING = 1135;    // CG_DATATYPE_MACRO(String,string,CG_STRING,CG_STRING,0,1,CG_PARAMETERCLASS_OBJECT)
  {$EXTERNALSYM CG_STRING}
  CG_PROGRAM_TYPE = 1136; // CG_DATATYPE_MACRO(Program,program,CG_PROGRAM_TYPE,CG_PROGRAM_TYPE,0,0,CG_PARAMETERCLASS_OBJECT)
  {$EXTERNALSYM CG_PROGRAM_TYPE}
  CG_TEXTURE = 1137;   // CG_DATATYPE_MACRO(Texture,texture,CG_TEXTURE,CG_TEXTURE,0,0,CG_PARAMETERCLASS_OBJECT)
  {$EXTERNALSYM CG_TEXTURE}

type
  TCGtype = DWORD;
{$ENDIF}
  CGtype = TCGtype;
  {$NODEFINE CGtype}
  {$NODEFINE TCGtype}
  {$HPPEMIT 'typedef CGtype TCGtype;'}


 (*
  * The following macro invocations define the supported CG basic hardware
  * bind locations.
  *
  * The macros have the form :
  *
  *   CG_BINDLOCATION_MACRO(name, compiler_name, enum_int)
  *
  *     name          : The name of the location.
  *     enum_name     : The C enumerant.
  *     compiler_name : The name of the location within the compiler syntax.
  *     int_id        : Integer enumerant associated with this bind location.
  *                       (3256 is reservered for CG_UNDEFINED)
  *     addressable   : The bind location must have an integer address
  *                     associated with it.
  *     ParamType     : the cgParamType of this register.
  *
  *)
{$IFDEF COMPILER6_UP}
  TCGresource = (
  {$IFDEF BCB}
    CG_DummyResource
  {$ELSE}
    //# define CG_BINDLOCATION_MACRO(name,enum_name,compiler_name,\
    //                               enum_int,addressable,param_type) \
    //  enum_name = enum_int,

    CG_TEXUNIT0              =  2048, // CG_BINDLOCATION_MACRO(TexUnit0,CG_TEXUNIT0,"texunit 0",2048,0,cgTexObjParam)
    CG_TEXUNIT1              =  2049, // CG_BINDLOCATION_MACRO(TexUnit1,CG_TEXUNIT1,"texunit 1",2049,0,cgTexObjParam)
    CG_TEXUNIT2              =  2050, // CG_BINDLOCATION_MACRO(TexUnit2,CG_TEXUNIT2,"texunit 2",2050,0,cgTexObjParam)
    CG_TEXUNIT3              =  2051, // CG_BINDLOCATION_MACRO(TexUnit3,CG_TEXUNIT3,"texunit 3",2051,0,cgTexObjParam)
    CG_TEXUNIT4              =  2052, // CG_BINDLOCATION_MACRO(TexUnit4,CG_TEXUNIT4,"texunit 4",2052,0,cgTexObjParam)
    CG_TEXUNIT5              =  2053, // CG_BINDLOCATION_MACRO(TexUnit5,CG_TEXUNIT5,"texunit 5",2053,0,cgTexObjParam)
    CG_TEXUNIT6              =  2054, // CG_BINDLOCATION_MACRO(TexUnit6,CG_TEXUNIT6,"texunit 6",2054,0,cgTexObjParam)
    CG_TEXUNIT7              =  2055, // CG_BINDLOCATION_MACRO(TexUnit7,CG_TEXUNIT7,"texunit 7",2055,0,cgTexObjParam)
    CG_TEXUNIT8              =  2056, // CG_BINDLOCATION_MACRO(TexUnit8,CG_TEXUNIT8,"texunit 8",2056,0,cgTexObjParam)
    CG_TEXUNIT9              =  2057, // CG_BINDLOCATION_MACRO(TexUnit9,CG_TEXUNIT9,"texunit 9",2057,0,cgTexObjParam)
    CG_TEXUNIT10             =  2058, // CG_BINDLOCATION_MACRO(TexUnit10,CG_TEXUNIT10,"texunit 10",2058,0,cgTexObjParam)
    CG_TEXUNIT11             =  2059, // CG_BINDLOCATION_MACRO(TexUnit11,CG_TEXUNIT11,"texunit 11",2059,0,cgTexObjParam)
    CG_TEXUNIT12             =  2060, // CG_BINDLOCATION_MACRO(TexUnit12,CG_TEXUNIT12,"texunit 12",2060,0,cgTexObjParam)
    CG_TEXUNIT13             =  2061, // CG_BINDLOCATION_MACRO(TexUnit13,CG_TEXUNIT13,"texunit 13",2061,0,cgTexObjParam)
    CG_TEXUNIT14             =  2062, // CG_BINDLOCATION_MACRO(TexUnit14,CG_TEXUNIT14,"texunit 14",2062,0,cgTexObjParam)
    CG_TEXUNIT15             =  2063, // CG_BINDLOCATION_MACRO(TexUnit15,CG_TEXUNIT15,"texunit 15",2063,0,cgTexObjParam)

    CG_ATTR0                 =  2113, // CG_BINDLOCATION_MACRO(Attr0,CG_ATTR0,"ATTR0",2113,0,cgConnectorMemberParam)
    CG_ATTR1                 =  2114, // CG_BINDLOCATION_MACRO(Attr1,CG_ATTR1,"ATTR1",2114,0,cgConnectorMemberParam)
    CG_ATTR2                 =  2115, // CG_BINDLOCATION_MACRO(Attr2,CG_ATTR2,"ATTR2",2115,0,cgConnectorMemberParam)
    CG_ATTR3                 =  2116, // CG_BINDLOCATION_MACRO(Attr3,CG_ATTR3,"ATTR3",2116,0,cgConnectorMemberParam)
    CG_ATTR4                 =  2117, // CG_BINDLOCATION_MACRO(Attr4,CG_ATTR4,"ATTR4",2117,0,cgConnectorMemberParam)
    CG_ATTR5                 =  2118, // CG_BINDLOCATION_MACRO(Attr5,CG_ATTR5,"ATTR5",2118,0,cgConnectorMemberParam)
    CG_ATTR6                 =  2119, // CG_BINDLOCATION_MACRO(Attr6,CG_ATTR6,"ATTR6",2119,0,cgConnectorMemberParam)
    CG_ATTR7                 =  2120, // CG_BINDLOCATION_MACRO(Attr7,CG_ATTR7,"ATTR7",2120,0,cgConnectorMemberParam)
    CG_ATTR8                 =  2121, // CG_BINDLOCATION_MACRO(Attr8,CG_ATTR8,"ATTR8",2121,0,cgConnectorMemberParam)
    CG_ATTR9                 =  2122, // CG_BINDLOCATION_MACRO(Attr9,CG_ATTR9,"ATTR9",2122,0,cgConnectorMemberParam)
    CG_ATTR10                =  2123, // CG_BINDLOCATION_MACRO(Attr10,CG_ATTR10,"ATTR10",2123,0,cgConnectorMemberParam)
    CG_ATTR11                =  2124, // CG_BINDLOCATION_MACRO(Attr11,CG_ATTR11,"ATTR11",2124,0,cgConnectorMemberParam)
    CG_ATTR12                =  2125, // CG_BINDLOCATION_MACRO(Attr12,CG_ATTR12,"ATTR12",2125,0,cgConnectorMemberParam)
    CG_ATTR13                =  2126, // CG_BINDLOCATION_MACRO(Attr13,CG_ATTR13,"ATTR13",2126,0,cgConnectorMemberParam)
    CG_ATTR14                =  2127, // CG_BINDLOCATION_MACRO(Attr14,CG_ATTR14,"ATTR14",2127,0,cgConnectorMemberParam)
    CG_ATTR15                =  2128, // CG_BINDLOCATION_MACRO(Attr15,CG_ATTR15,"ATTR15",2128,0,cgConnectorMemberParam)

    CG_C                     =  2178, // CG_BINDLOCATION_MACRO(VertUniform,CG_C,"c",2178,1,cgUniformParam)

    CG_TEX0                  =  2179, // CG_BINDLOCATION_MACRO(Tex0,CG_TEX0,"TEX0",2179,0,cgConnectorMemberParam)
    CG_TEX1                  =  2180, // CG_BINDLOCATION_MACRO(Tex1,CG_TEX1,"TEX1",2180,0,cgConnectorMemberParam)
    CG_TEX2                  =  2181, // CG_BINDLOCATION_MACRO(Tex2,CG_TEX2,"TEX2",2181,0,cgConnectorMemberParam)
    CG_TEX3                  =  2192, // CG_BINDLOCATION_MACRO(Tex3,CG_TEX3,"TEX3",2192,0,cgConnectorMemberParam)
    CG_TEX4                  =  2193, // CG_BINDLOCATION_MACRO(Tex4,CG_TEX4,"TEX4",2193,0,cgConnectorMemberParam)
    CG_TEX5                  =  2194, // CG_BINDLOCATION_MACRO(Tex5,CG_TEX5,"TEX5",2194,0,cgConnectorMemberParam)
    CG_TEX6                  =  2195, // CG_BINDLOCATION_MACRO(Tex6,CG_TEX6,"TEX6",2195,0,cgConnectorMemberParam)
    CG_TEX7                  =  2196, // CG_BINDLOCATION_MACRO(Tex7,CG_TEX7,"TEX7",2196,0,cgConnectorMemberParam)

    CG_HPOS                  =  2243, // CG_BINDLOCATION_MACRO(HPos,CG_HPOS,"HPOS",2243,0,cgConnectorMemberParam)
    CG_COL0                  =  2245, // CG_BINDLOCATION_MACRO(Col0,CG_COL0,"COL0",2245,0,cgConnectorMemberParam)
    CG_COL1                  =  2246, // CG_BINDLOCATION_MACRO(Col1,CG_COL1,"COL1",2246,0,cgConnectorMemberParam)
    CG_COL2                  =  2247, // CG_BINDLOCATION_MACRO(Col2,CG_COL2,"COL2",2247,0,cgConnectorMemberParam)
    CG_COL3                  =  2248, // CG_BINDLOCATION_MACRO(Col3,CG_COL3,"COL3",2248,0,cgConnectorMemberParam)
    CG_PSIZ                  =  2309, // CG_BINDLOCATION_MACRO(PSiz,CG_PSIZ,"PSIZ",2309,0,cgConnectorMemberParam)
    CG_CLP0                  =  2310, // CG_BINDLOCATION_MACRO(Clp0,CG_CLP0,"CLP0",2310,0,CG_VARYING_PARAM)
    CG_CLP1                  =  2311, // CG_BINDLOCATION_MACRO(Clp1,CG_CLP1,"CLP1",2311,0,CG_VARYING_PARAM)
    CG_CLP2                  =  2312, // CG_BINDLOCATION_MACRO(Clp2,CG_CLP2,"CLP2",2312,0,CG_VARYING_PARAM)
    CG_CLP3                  =  2313, // CG_BINDLOCATION_MACRO(Clp3,CG_CLP3,"CLP3",2313,0,CG_VARYING_PARAM)
    CG_CLP4                  =  2314, // CG_BINDLOCATION_MACRO(Clp4,CG_CLP4,"CLP4",2314,0,CG_VARYING_PARAM)
    CG_CLP5                  =  2315, // CG_BINDLOCATION_MACRO(Clp5,CG_CLP5,"CLP5",2315,0,CG_VARYING_PARAM)
    CG_WPOS                  =  2373, // CG_BINDLOCATION_MACRO(WPos,CG_WPOS,"WPOS",2373,0,cgConnectorMemberParam)
    CG_POINTCOORD            =  2374, // CG_BINDLOCATION_MACRO(PointCoord,CG_POINTCOORD,"POINTCOORD",2374,0,CG_VARYING_PARAM)


    CG_POSITION0             =  2437, // CG_BINDLOCATION_MACRO(Position0,CG_POSITION0,"POSITION0",2437,0,cgConnectorMemberParam)
    CG_POSITION1             =  2438, // CG_BINDLOCATION_MACRO(Position1,CG_POSITION1,"POSITION1",2438,0,cgConnectorMemberParam)
    CG_POSITION2             =  2439, // CG_BINDLOCATION_MACRO(Position2,CG_POSITION2,"POSITION2",2439,0,cgConnectorMemberParam)
    CG_POSITION3             =  2440, // CG_BINDLOCATION_MACRO(Position3,CG_POSITION3,"POSITION3",2440,0,cgConnectorMemberParam)
    CG_POSITION4             =  2441, // CG_BINDLOCATION_MACRO(Position4,CG_POSITION4,"POSITION4",2441,0,cgConnectorMemberParam)
    CG_POSITION5             =  2442, // CG_BINDLOCATION_MACRO(Position5,CG_POSITION5,"POSITION5",2442,0,cgConnectorMemberParam)
    CG_POSITION6             =  2443, // CG_BINDLOCATION_MACRO(Position6,CG_POSITION6,"POSITION6",2443,0,cgConnectorMemberParam)
    CG_POSITION7             =  2444, // CG_BINDLOCATION_MACRO(Position7,CG_POSITION7,"POSITION7",2444,0,cgConnectorMemberParam)
    CG_POSITION8             =  2445, // CG_BINDLOCATION_MACRO(Position8,CG_POSITION8,"POSITION8",2445,0,cgConnectorMemberParam)
    CG_POSITION9             =  2446, // CG_BINDLOCATION_MACRO(Position9,CG_POSITION9,"POSITION9",2446,0,cgConnectorMemberParam)
    CG_POSITION10            =  2447, // CG_BINDLOCATION_MACRO(Position10,CG_POSITION10,"POSITION10",2447,0,cgConnectorMemberParam)
    CG_POSITION11            =  2448, // CG_BINDLOCATION_MACRO(Position11,CG_POSITION11,"POSITION11",2448,0,cgConnectorMemberParam)
    CG_POSITION12            =  2449, // CG_BINDLOCATION_MACRO(Position12,CG_POSITION12,"POSITION12",2449,0,cgConnectorMemberParam)
    CG_POSITION13            =  2450, // CG_BINDLOCATION_MACRO(Position13,CG_POSITION13,"POSITION13",2450,0,cgConnectorMemberParam)
    CG_POSITION14            =  2451, // CG_BINDLOCATION_MACRO(Position14,CG_POSITION14,"POSITION14",2451,0,cgConnectorMemberParam)
    CG_POSITION15            =  2452, // CG_BINDLOCATION_MACRO(Position15,CG_POSITION15,"POSITION15",2452,0,cgConnectorMemberParam)
    CG_DIFFUSE0              =  2501, // CG_BINDLOCATION_MACRO(Diffuse0,CG_DIFFUSE0,"DIFFUSE0",2501,0,cgConnectorMemberParam)
    CG_TANGENT0              =  2565, // CG_BINDLOCATION_MACRO(Tangent0,CG_TANGENT0,"TANGENT0",2565,0,cgConnectorMemberParam)
    CG_TANGENT1              =  2566, // CG_BINDLOCATION_MACRO(Tangent1,CG_TANGENT1,"TANGENT1",2566,0,cgConnectorMemberParam)
    CG_TANGENT2              =  2567, // CG_BINDLOCATION_MACRO(Tangent2,CG_TANGENT2,"TANGENT2",2567,0,cgConnectorMemberParam)
    CG_TANGENT3              =  2568, // CG_BINDLOCATION_MACRO(Tangent3,CG_TANGENT3,"TANGENT3",2568,0,cgConnectorMemberParam)
    CG_TANGENT4              =  2569, // CG_BINDLOCATION_MACRO(Tangent4,CG_TANGENT4,"TANGENT4",2569,0,cgConnectorMemberParam)
    CG_TANGENT5              =  2570, // CG_BINDLOCATION_MACRO(Tangent5,CG_TANGENT5,"TANGENT5",2570,0,cgConnectorMemberParam)
    CG_TANGENT6              =  2571, // CG_BINDLOCATION_MACRO(Tangent6,CG_TANGENT6,"TANGENT6",2571,0,cgConnectorMemberParam)
    CG_TANGENT7              =  2572, // CG_BINDLOCATION_MACRO(Tangent7,CG_TANGENT7,"TANGENT7",2572,0,cgConnectorMemberParam)
    CG_TANGENT8              =  2573, // CG_BINDLOCATION_MACRO(Tangent8,CG_TANGENT8,"TANGENT8",2573,0,cgConnectorMemberParam)
    CG_TANGENT9              =  2574, // CG_BINDLOCATION_MACRO(Tangent9,CG_TANGENT9,"TANGENT9",2574,0,cgConnectorMemberParam)
    CG_TANGENT10             =  2575, // CG_BINDLOCATION_MACRO(Tangent10,CG_TANGENT10,"TANGENT10",2575,0,cgConnectorMemberParam)
    CG_TANGENT11             =  2576, // CG_BINDLOCATION_MACRO(Tangent11,CG_TANGENT11,"TANGENT11",2576,0,cgConnectorMemberParam)
    CG_TANGENT12             =  2577, // CG_BINDLOCATION_MACRO(Tangent12,CG_TANGENT12,"TANGENT12",2577,0,cgConnectorMemberParam)
    CG_TANGENT13             =  2578, // CG_BINDLOCATION_MACRO(Tangent13,CG_TANGENT13,"TANGENT13",2578,0,cgConnectorMemberParam)
    CG_TANGENT14             =  2579, // CG_BINDLOCATION_MACRO(Tangent14,CG_TANGENT14,"TANGENT14",2579,0,cgConnectorMemberParam)
    CG_TANGENT15             =  2580, // CG_BINDLOCATION_MACRO(Tangent15,CG_TANGENT15,"TANGENT15",2580,0,cgConnectorMemberParam)
    CG_SPECULAR0             =  2629, // CG_BINDLOCATION_MACRO(Specular0,CG_SPECULAR0,"SPECULAR0",2629,0,cgConnectorMemberParam)
    CG_BLENDINDICES0         =  2693, // CG_BINDLOCATION_MACRO(BlendIndices0,CG_BLENDINDICES0,"BLENDINDICES0",2693,0,cgConnectorMemberParam)
    CG_BLENDINDICES1         =  2694, // CG_BINDLOCATION_MACRO(BlendIndices1,CG_BLENDINDICES1,"BLENDINDICES1",2694,0,cgConnectorMemberParam)
    CG_BLENDINDICES2         =  2695, // CG_BINDLOCATION_MACRO(BlendIndices2,CG_BLENDINDICES2,"BLENDINDICES2",2695,0,cgConnectorMemberParam)
    CG_BLENDINDICES3         =  2696, // CG_BINDLOCATION_MACRO(BlendIndices3,CG_BLENDINDICES3,"BLENDINDICES3",2696,0,cgConnectorMemberParam)
    CG_BLENDINDICES4         =  2697, // CG_BINDLOCATION_MACRO(BlendIndices4,CG_BLENDINDICES4,"BLENDINDICES4",2697,0,cgConnectorMemberParam)
    CG_BLENDINDICES5         =  2698, // CG_BINDLOCATION_MACRO(BlendIndices5,CG_BLENDINDICES5,"BLENDINDICES5",2698,0,cgConnectorMemberParam)
    CG_BLENDINDICES6         =  2699, // CG_BINDLOCATION_MACRO(BlendIndices6,CG_BLENDINDICES6,"BLENDINDICES6",2699,0,cgConnectorMemberParam)
    CG_BLENDINDICES7         =  2700, // CG_BINDLOCATION_MACRO(BlendIndices7,CG_BLENDINDICES7,"BLENDINDICES7",2700,0,cgConnectorMemberParam)
    CG_BLENDINDICES8         =  2701, // CG_BINDLOCATION_MACRO(BlendIndices8,CG_BLENDINDICES8,"BLENDINDICES8",2701,0,cgConnectorMemberParam)
    CG_BLENDINDICES9         =  2702, // CG_BINDLOCATION_MACRO(BlendIndices9,CG_BLENDINDICES9,"BLENDINDICES9",2702,0,cgConnectorMemberParam)
    CG_BLENDINDICES10        =  2703, // CG_BINDLOCATION_MACRO(BlendIndices10,CG_BLENDINDICES10,"BLENDINDICES10",2703,0,cgConnectorMemberParam)
    CG_BLENDINDICES11        =  2704, // CG_BINDLOCATION_MACRO(BlendIndices11,CG_BLENDINDICES11,"BLENDINDICES11",2704,0,cgConnectorMemberParam)
    CG_BLENDINDICES12        =  2705, // CG_BINDLOCATION_MACRO(BlendIndices12,CG_BLENDINDICES12,"BLENDINDICES12",2705,0,cgConnectorMemberParam)
    CG_BLENDINDICES13        =  2706, // CG_BINDLOCATION_MACRO(BlendIndices13,CG_BLENDINDICES13,"BLENDINDICES13",2706,0,cgConnectorMemberParam)
    CG_BLENDINDICES14        =  2707, // CG_BINDLOCATION_MACRO(BlendIndices14,CG_BLENDINDICES14,"BLENDINDICES14",2707,0,cgConnectorMemberParam)
    CG_BLENDINDICES15        =  2708, // CG_BINDLOCATION_MACRO(BlendIndices15,CG_BLENDINDICES15,"BLENDINDICES15",2708,0,cgConnectorMemberParam)
    CG_COLOR0                =  2757, // CG_BINDLOCATION_MACRO(Color0,CG_COLOR0,"COLOR0",2757,0,cgConnectorMemberParam)
    CG_COLOR1                =  2758, // CG_BINDLOCATION_MACRO(Color1,CG_COLOR1,"COLOR1",2758,0,cgConnectorMemberParam)
    CG_COLOR2                =  2759, // CG_BINDLOCATION_MACRO(Color2,CG_COLOR2,"COLOR2",2759,0,cgConnectorMemberParam)
    CG_COLOR3                =  2760, // CG_BINDLOCATION_MACRO(Color3,CG_COLOR3,"COLOR3",2760,0,cgConnectorMemberParam)
    CG_COLOR4                =  2761, // CG_BINDLOCATION_MACRO(Color4,CG_COLOR4,"COLOR4",2761,0,cgConnectorMemberParam)
    CG_COLOR5                =  2762, // CG_BINDLOCATION_MACRO(Color5,CG_COLOR5,"COLOR5",2762,0,cgConnectorMemberParam)
    CG_COLOR6                =  2763, // CG_BINDLOCATION_MACRO(Color6,CG_COLOR6,"COLOR6",2763,0,cgConnectorMemberParam)
    CG_COLOR7                =  2764, // CG_BINDLOCATION_MACRO(Color7,CG_COLOR7,"COLOR7",2764,0,cgConnectorMemberParam)
    CG_COLOR8                =  2765, // CG_BINDLOCATION_MACRO(Color8,CG_COLOR8,"COLOR8",2765,0,cgConnectorMemberParam)
    CG_COLOR9                =  2766, // CG_BINDLOCATION_MACRO(Color9,CG_COLOR9,"COLOR9",2766,0,cgConnectorMemberParam)
    CG_COLOR10               =  2767, // CG_BINDLOCATION_MACRO(Color10,CG_COLOR10,"COLOR10",2767,0,cgConnectorMemberParam)
    CG_COLOR11               =  2768, // CG_BINDLOCATION_MACRO(Color11,CG_COLOR11,"COLOR11",2768,0,cgConnectorMemberParam)
    CG_COLOR12               =  2769, // CG_BINDLOCATION_MACRO(Color12,CG_COLOR12,"COLOR12",2769,0,cgConnectorMemberParam)
    CG_COLOR13               =  2770, // CG_BINDLOCATION_MACRO(Color13,CG_COLOR13,"COLOR13",2770,0,cgConnectorMemberParam)
    CG_COLOR14               =  2771, // CG_BINDLOCATION_MACRO(Color14,CG_COLOR14,"COLOR14",2771,0,cgConnectorMemberParam)
    CG_COLOR15               =  2772, // CG_BINDLOCATION_MACRO(Color15,CG_COLOR15,"COLOR15",2772,0,cgConnectorMemberParam)
    CG_PSIZE0                =  2821, // CG_BINDLOCATION_MACRO(PSize0,CG_PSIZE0,"PSIZE0",2821,0,cgConnectorMemberParam)
    CG_PSIZE1                =  2822, // CG_BINDLOCATION_MACRO(PSize1,CG_PSIZE1,"PSIZE1",2822,0,cgConnectorMemberParam)
    CG_PSIZE2                =  2823, // CG_BINDLOCATION_MACRO(PSize2,CG_PSIZE2,"PSIZE2",2823,0,cgConnectorMemberParam)
    CG_PSIZE3                =  2824, // CG_BINDLOCATION_MACRO(PSize3,CG_PSIZE3,"PSIZE3",2824,0,cgConnectorMemberParam)
    CG_PSIZE4                =  2825, // CG_BINDLOCATION_MACRO(PSize4,CG_PSIZE4,"PSIZE4",2825,0,cgConnectorMemberParam)
    CG_PSIZE5                =  2826, // CG_BINDLOCATION_MACRO(PSize5,CG_PSIZE5,"PSIZE5",2826,0,cgConnectorMemberParam)
    CG_PSIZE6                =  2827, // CG_BINDLOCATION_MACRO(PSize6,CG_PSIZE6,"PSIZE6",2827,0,cgConnectorMemberParam)
    CG_PSIZE7                =  2828, // CG_BINDLOCATION_MACRO(PSize7,CG_PSIZE7,"PSIZE7",2828,0,cgConnectorMemberParam)
    CG_PSIZE8                =  2829, // CG_BINDLOCATION_MACRO(PSize8,CG_PSIZE8,"PSIZE8",2829,0,cgConnectorMemberParam)
    CG_PSIZE9                =  2830, // CG_BINDLOCATION_MACRO(PSize9,CG_PSIZE9,"PSIZE9",2830,0,cgConnectorMemberParam)
    CG_PSIZE10               =  2831, // CG_BINDLOCATION_MACRO(PSize10,CG_PSIZE10,"PSIZE10",2831,0,cgConnectorMemberParam)
    CG_PSIZE11               =  2832, // CG_BINDLOCATION_MACRO(PSize11,CG_PSIZE11,"PSIZE11",2832,0,cgConnectorMemberParam)
    CG_PSIZE12               =  2833, // CG_BINDLOCATION_MACRO(PSize12,CG_PSIZE12,"PSIZE12",2833,0,cgConnectorMemberParam)
    CG_PSIZE13               =  2834, // CG_BINDLOCATION_MACRO(PSize13,CG_PSIZE13,"PSIZE13",2834,0,cgConnectorMemberParam)
    CG_PSIZE14               =  2835, // CG_BINDLOCATION_MACRO(PSize14,CG_PSIZE14,"PSIZE14",2835,0,cgConnectorMemberParam)
    CG_PSIZE15               =  2836, // CG_BINDLOCATION_MACRO(PSize15,CG_PSIZE15,"PSIZE15",2836,0,cgConnectorMemberParam)
    CG_BINORMAL0             =  2885, // CG_BINDLOCATION_MACRO(BiNormal0,CG_BINORMAL0,"BINORMAL0",2885,0,cgConnectorMemberParam)
    CG_BINORMAL1             =  2886, // CG_BINDLOCATION_MACRO(BiNormal1,CG_BINORMAL1,"BINORMAL1",2886,0,cgConnectorMemberParam)
    CG_BINORMAL2             =  2887, // CG_BINDLOCATION_MACRO(BiNormal2,CG_BINORMAL2,"BINORMAL2",2887,0,cgConnectorMemberParam)
    CG_BINORMAL3             =  2888, // CG_BINDLOCATION_MACRO(BiNormal3,CG_BINORMAL3,"BINORMAL3",2888,0,cgConnectorMemberParam)
    CG_BINORMAL4             =  2889, // CG_BINDLOCATION_MACRO(BiNormal4,CG_BINORMAL4,"BINORMAL4",2889,0,cgConnectorMemberParam)
    CG_BINORMAL5             =  2890, // CG_BINDLOCATION_MACRO(BiNormal5,CG_BINORMAL5,"BINORMAL5",2890,0,cgConnectorMemberParam)
    CG_BINORMAL6             =  2891, // CG_BINDLOCATION_MACRO(BiNormal6,CG_BINORMAL6,"BINORMAL6",2891,0,cgConnectorMemberParam)
    CG_BINORMAL7             =  2892, // CG_BINDLOCATION_MACRO(BiNormal7,CG_BINORMAL7,"BINORMAL7",2892,0,cgConnectorMemberParam)
    CG_BINORMAL8             =  2893, // CG_BINDLOCATION_MACRO(BiNormal8,CG_BINORMAL8,"BINORMAL8",2893,0,cgConnectorMemberParam)
    CG_BINORMAL9             =  2894, // CG_BINDLOCATION_MACRO(BiNormal9,CG_BINORMAL9,"BINORMAL9",2894,0,cgConnectorMemberParam)
    CG_BINORMAL10            =  2895, // CG_BINDLOCATION_MACRO(BiNormal10,CG_BINORMAL10,"BINORMAL10",2895,0,cgConnectorMemberParam)
    CG_BINORMAL11            =  2896, // CG_BINDLOCATION_MACRO(BiNormal11,CG_BINORMAL11,"BINORMAL11",2896,0,cgConnectorMemberParam)
    CG_BINORMAL12            =  2897, // CG_BINDLOCATION_MACRO(BiNormal12,CG_BINORMAL12,"BINORMAL12",2897,0,cgConnectorMemberParam)
    CG_BINORMAL13            =  2898, // CG_BINDLOCATION_MACRO(BiNormal13,CG_BINORMAL13,"BINORMAL13",2898,0,cgConnectorMemberParam)
    CG_BINORMAL14            =  2899, // CG_BINDLOCATION_MACRO(BiNormal14,CG_BINORMAL14,"BINORMAL14",2899,0,cgConnectorMemberParam)
    CG_BINORMAL15            =  2900, // CG_BINDLOCATION_MACRO(BiNormal15,CG_BINORMAL15,"BINORMAL15",2900,0,cgConnectorMemberParam)
    CG_FOG0                  =  2917, // CG_BINDLOCATION_MACRO(FOG0,CG_FOG0,"FOG0",2917,0,cgConnectorMemberParam)
    CG_FOG1                  =  2918, // CG_BINDLOCATION_MACRO(FOG1,CG_FOG1,"FOG1",2918,0,cgConnectorMemberParam)
    CG_FOG2                  =  2919, // CG_BINDLOCATION_MACRO(FOG2,CG_FOG2,"FOG2",2919,0,cgConnectorMemberParam)
    CG_FOG3                  =  2920, // CG_BINDLOCATION_MACRO(FOG3,CG_FOG3,"FOG3",2920,0,cgConnectorMemberParam)
    CG_FOG4                  =  2921, // CG_BINDLOCATION_MACRO(FOG4,CG_FOG4,"FOG4",2921,0,cgConnectorMemberParam)
    CG_FOG5                  =  2922, // CG_BINDLOCATION_MACRO(FOG5,CG_FOG5,"FOG5",2922,0,cgConnectorMemberParam)
    CG_FOG6                  =  2923, // CG_BINDLOCATION_MACRO(FOG6,CG_FOG6,"FOG6",2923,0,cgConnectorMemberParam)
    CG_FOG7                  =  2924, // CG_BINDLOCATION_MACRO(FOG7,CG_FOG7,"FOG7",2924,0,cgConnectorMemberParam)
    CG_FOG8                  =  2925, // CG_BINDLOCATION_MACRO(FOG8,CG_FOG8,"FOG8",2925,0,cgConnectorMemberParam)
    CG_FOG9                  =  2926, // CG_BINDLOCATION_MACRO(FOG9,CG_FOG9,"FOG9",2926,0,cgConnectorMemberParam)
    CG_FOG10                 =  2927, // CG_BINDLOCATION_MACRO(FOG10,CG_FOG10,"FOG10",2927,0,cgConnectorMemberParam)
    CG_FOG11                 =  2928, // CG_BINDLOCATION_MACRO(FOG11,CG_FOG11,"FOG11",2928,0,cgConnectorMemberParam)
    CG_FOG12                 =  2929, // CG_BINDLOCATION_MACRO(FOG12,CG_FOG12,"FOG12",2929,0,cgConnectorMemberParam)
    CG_FOG13                 =  2930, // CG_BINDLOCATION_MACRO(FOG13,CG_FOG13,"FOG13",2930,0,cgConnectorMemberParam)
    CG_FOG14                 =  2931, // CG_BINDLOCATION_MACRO(FOG14,CG_FOG14,"FOG14",2931,0,cgConnectorMemberParam)
    CG_FOG15                 =  2932, // CG_BINDLOCATION_MACRO(FOG15,CG_FOG15,"FOG15",2932,0,cgConnectorMemberParam)
    CG_DEPTH0                =  2933, // CG_BINDLOCATION_MACRO(DEPTH0,CG_DEPTH0,"DEPTH0",2933,0,cgConnectorMemberParam)
    CG_DEPTH1                =  2934, // CG_BINDLOCATION_MACRO(DEPTH1,CG_DEPTH1,"DEPTH1",2934,0,cgConnectorMemberParam)
    CG_DEPTH2                =  2935, // CG_BINDLOCATION_MACRO(DEPTH2,CG_DEPTH2,"DEPTH2",2935,0,cgConnectorMemberParam)
    CG_DEPTH3                =  2936, // CG_BINDLOCATION_MACRO(DEPTH3,CG_DEPTH3,"DEPTH3",2936,0,cgConnectorMemberParam)
    CG_DEPTH4                =  2937, // CG_BINDLOCATION_MACRO(DEPTH4,CG_DEPTH4,"DEPTH4",2937,0,cgConnectorMemberParam)
    CG_DEPTH5                =  2938, // CG_BINDLOCATION_MACRO(DEPTH5,CG_DEPTH5,"DEPTH5",2938,0,cgConnectorMemberParam)
    CG_DEPTH6                =  2939, // CG_BINDLOCATION_MACRO(DEPTH6,CG_DEPTH6,"DEPTH6",2939,0,cgConnectorMemberParam)
    CG_DEPTH7                =  2940, // CG_BINDLOCATION_MACRO(DEPTH7,CG_DEPTH7,"DEPTH7",2940,0,cgConnectorMemberParam)
    CG_DEPTH8                =  2941, // CG_BINDLOCATION_MACRO(DEPTH8,CG_DEPTH8,"DEPTH8",2941,0,cgConnectorMemberParam)
//TODO: what THE @#$% !!! ('29542')
    CG_DEPTH9                = 29542, // CG_BINDLOCATION_MACRO(DEPTH9,CG_DEPTH9,"DEPTH9",29542,0,cgConnectorMemberParam)
    CG_DEPTH10               =  2943, // CG_BINDLOCATION_MACRO(DEPTH10,CG_DEPTH10,"DEPTH10",2943,0,cgConnectorMemberParam)
    CG_DEPTH11               =  2944, // CG_BINDLOCATION_MACRO(DEPTH11,CG_DEPTH11,"DEPTH11",2944,0,cgConnectorMemberParam)
    CG_DEPTH12               =  2945, // CG_BINDLOCATION_MACRO(DEPTH12,CG_DEPTH12,"DEPTH12",2945,0,cgConnectorMemberParam)
    CG_DEPTH13               =  2946, // CG_BINDLOCATION_MACRO(DEPTH13,CG_DEPTH13,"DEPTH13",2946,0,cgConnectorMemberParam)
    CG_DEPTH14               =  2947, // CG_BINDLOCATION_MACRO(DEPTH14,CG_DEPTH14,"DEPTH14",2947,0,cgConnectorMemberParam)
    CG_DEPTH15               =  2948, // CG_BINDLOCATION_MACRO(DEPTH15,CG_DEPTH15,"DEPTH15",2948,0,cgConnectorMemberParam)
    CG_SAMPLE0               =  2949, // CG_BINDLOCATION_MACRO(SAMPLE0,CG_SAMPLE0,"SAMPLE0",2949,0,cgConnectorMemberParam)
    CG_SAMPLE1               =  2950, // CG_BINDLOCATION_MACRO(SAMPLE1,CG_SAMPLE1,"SAMPLE1",2950,0,cgConnectorMemberParam)
    CG_SAMPLE2               =  2951, // CG_BINDLOCATION_MACRO(SAMPLE2,CG_SAMPLE2,"SAMPLE2",2951,0,cgConnectorMemberParam)
    CG_SAMPLE3               =  2952, // CG_BINDLOCATION_MACRO(SAMPLE3,CG_SAMPLE3,"SAMPLE3",2952,0,cgConnectorMemberParam)
    CG_SAMPLE4               =  2953, // CG_BINDLOCATION_MACRO(SAMPLE4,CG_SAMPLE4,"SAMPLE4",2953,0,cgConnectorMemberParam)
    CG_SAMPLE5               =  2954, // CG_BINDLOCATION_MACRO(SAMPLE5,CG_SAMPLE5,"SAMPLE5",2954,0,cgConnectorMemberParam)
    CG_SAMPLE6               =  2955, // CG_BINDLOCATION_MACRO(SAMPLE6,CG_SAMPLE6,"SAMPLE6",2955,0,cgConnectorMemberParam)
    CG_SAMPLE7               =  2956, // CG_BINDLOCATION_MACRO(SAMPLE7,CG_SAMPLE7,"SAMPLE7",2956,0,cgConnectorMemberParam)
    CG_SAMPLE8               =  2957, // CG_BINDLOCATION_MACRO(SAMPLE8,CG_SAMPLE8,"SAMPLE8",2957,0,cgConnectorMemberParam)
    CG_SAMPLE9               =  2958, // CG_BINDLOCATION_MACRO(SAMPLE9,CG_SAMPLE9,"SAMPLE9",2958,0,cgConnectorMemberParam)
    CG_SAMPLE10              =  2959, // CG_BINDLOCATION_MACRO(SAMPLE10,CG_SAMPLE10,"SAMPLE10",2959,0,cgConnectorMemberParam)
    CG_SAMPLE11              =  2960, // CG_BINDLOCATION_MACRO(SAMPLE11,CG_SAMPLE11,"SAMPLE11",2960,0,cgConnectorMemberParam)
    CG_SAMPLE12              =  2961, // CG_BINDLOCATION_MACRO(SAMPLE12,CG_SAMPLE12,"SAMPLE12",2961,0,cgConnectorMemberParam)
    CG_SAMPLE13              =  2962, // CG_BINDLOCATION_MACRO(SAMPLE13,CG_SAMPLE13,"SAMPLE13",2962,0,cgConnectorMemberParam)
    CG_SAMPLE14              =  2963, // CG_BINDLOCATION_MACRO(SAMPLE14,CG_SAMPLE14,"SAMPLE14",2963,0,cgConnectorMemberParam)
    CG_SAMPLE15              =  2964, // CG_BINDLOCATION_MACRO(SAMPLE15,CG_SAMPLE15,"SAMPLE15",2964,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT0          =  3028, // CG_BINDLOCATION_MACRO(BlendWeight0,CG_BLENDWEIGHT0,"BLENDWEIGHT0",3028,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT1          =  3029, // CG_BINDLOCATION_MACRO(BlendWeight1,CG_BLENDWEIGHT1,"BLENDWEIGHT1",3029,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT2          =  3030, // CG_BINDLOCATION_MACRO(BlendWeight2,CG_BLENDWEIGHT2,"BLENDWEIGHT2",3030,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT3          =  3031, // CG_BINDLOCATION_MACRO(BlendWeight3,CG_BLENDWEIGHT3,"BLENDWEIGHT3",3031,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT4          =  3032, // CG_BINDLOCATION_MACRO(BlendWeight4,CG_BLENDWEIGHT4,"BLENDWEIGHT4",3032,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT5          =  3033, // CG_BINDLOCATION_MACRO(BlendWeight5,CG_BLENDWEIGHT5,"BLENDWEIGHT5",3033,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT6          =  3034, // CG_BINDLOCATION_MACRO(BlendWeight6,CG_BLENDWEIGHT6,"BLENDWEIGHT6",3034,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT7          =  3035, // CG_BINDLOCATION_MACRO(BlendWeight7,CG_BLENDWEIGHT7,"BLENDWEIGHT7",3035,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT8          =  3036, // CG_BINDLOCATION_MACRO(BlendWeight8,CG_BLENDWEIGHT8,"BLENDWEIGHT8",3036,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT9          =  3037, // CG_BINDLOCATION_MACRO(BlendWeight9,CG_BLENDWEIGHT9,"BLENDWEIGHT9",3037,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT10         =  3038, // CG_BINDLOCATION_MACRO(BlendWeight10,CG_BLENDWEIGHT10,"BLENDWEIGHT10",3038,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT11         =  3039, // CG_BINDLOCATION_MACRO(BlendWeight11,CG_BLENDWEIGHT11,"BLENDWEIGHT11",3039,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT12         =  3040, // CG_BINDLOCATION_MACRO(BlendWeight12,CG_BLENDWEIGHT12,"BLENDWEIGHT12",3040,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT13         =  3041, // CG_BINDLOCATION_MACRO(BlendWeight13,CG_BLENDWEIGHT13,"BLENDWEIGHT13",3041,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT14         =  3042, // CG_BINDLOCATION_MACRO(BlendWeight14,CG_BLENDWEIGHT14,"BLENDWEIGHT14",3042,0,cgConnectorMemberParam)
    CG_BLENDWEIGHT15         =  3043, // CG_BINDLOCATION_MACRO(BlendWeight15,CG_BLENDWEIGHT15,"BLENDWEIGHT15",3043,0,cgConnectorMemberParam)
    CG_NORMAL0               =  3092, // CG_BINDLOCATION_MACRO(Normal0,CG_NORMAL0,"NORMAL0",3092,0,cgConnectorMemberParam)
    CG_NORMAL1               =  3093, // CG_BINDLOCATION_MACRO(Normal1,CG_NORMAL1,"NORMAL1",3093,0,cgConnectorMemberParam)
    CG_NORMAL2               =  3094, // CG_BINDLOCATION_MACRO(Normal2,CG_NORMAL2,"NORMAL2",3094,0,cgConnectorMemberParam)
    CG_NORMAL3               =  3095, // CG_BINDLOCATION_MACRO(Normal3,CG_NORMAL3,"NORMAL3",3095,0,cgConnectorMemberParam)
    CG_NORMAL4               =  3096, // CG_BINDLOCATION_MACRO(Normal4,CG_NORMAL4,"NORMAL4",3096,0,cgConnectorMemberParam)
    CG_NORMAL5               =  3097, // CG_BINDLOCATION_MACRO(Normal5,CG_NORMAL5,"NORMAL5",3097,0,cgConnectorMemberParam)
    CG_NORMAL6               =  3098, // CG_BINDLOCATION_MACRO(Normal6,CG_NORMAL6,"NORMAL6",3098,0,cgConnectorMemberParam)
    CG_NORMAL7               =  3099, // CG_BINDLOCATION_MACRO(Normal7,CG_NORMAL7,"NORMAL7",3099,0,cgConnectorMemberParam)
    CG_NORMAL8               =  3100, // CG_BINDLOCATION_MACRO(Normal8,CG_NORMAL8,"NORMAL8",3100,0,cgConnectorMemberParam)
    CG_NORMAL9               =  3101, // CG_BINDLOCATION_MACRO(Normal9,CG_NORMAL9,"NORMAL9",3101,0,cgConnectorMemberParam)
    CG_NORMAL10              =  3102, // CG_BINDLOCATION_MACRO(Normal10,CG_NORMAL10,"NORMAL10",3102,0,cgConnectorMemberParam)
    CG_NORMAL11              =  3103, // CG_BINDLOCATION_MACRO(Normal11,CG_NORMAL11,"NORMAL11",3103,0,cgConnectorMemberParam)
    CG_NORMAL12              =  3104, // CG_BINDLOCATION_MACRO(Normal12,CG_NORMAL12,"NORMAL12",3104,0,cgConnectorMemberParam)
    CG_NORMAL13              =  3105, // CG_BINDLOCATION_MACRO(Normal13,CG_NORMAL13,"NORMAL13",3105,0,cgConnectorMemberParam)
    CG_NORMAL14              =  3106, // CG_BINDLOCATION_MACRO(Normal14,CG_NORMAL14,"NORMAL14",3106,0,cgConnectorMemberParam)
    CG_NORMAL15              =  3107, // CG_BINDLOCATION_MACRO(Normal15,CG_NORMAL15,"NORMAL15",3107,0,cgConnectorMemberParam)
    CG_FOGCOORD              =  3156, // CG_BINDLOCATION_MACRO(FogCoord,CG_FOGCOORD,"FOGCOORD",3156,0,cgConnectorMemberParam)
    CG_TEXCOORD0             =  3220, // CG_BINDLOCATION_MACRO(TexCoord0,CG_TEXCOORD0,"TEXCOORD0",3220,0,cgConnectorMemberParam)
    CG_TEXCOORD1             =  3221, // CG_BINDLOCATION_MACRO(TexCoord1,CG_TEXCOORD1,"TEXCOORD1",3221,0,cgConnectorMemberParam)
    CG_TEXCOORD2             =  3222, // CG_BINDLOCATION_MACRO(TexCoord2,CG_TEXCOORD2,"TEXCOORD2",3222,0,cgConnectorMemberParam)
    CG_TEXCOORD3             =  3223, // CG_BINDLOCATION_MACRO(TexCoord3,CG_TEXCOORD3,"TEXCOORD3",3223,0,cgConnectorMemberParam)
    CG_TEXCOORD4             =  3224, // CG_BINDLOCATION_MACRO(TexCoord4,CG_TEXCOORD4,"TEXCOORD4",3224,0,cgConnectorMemberParam)
    CG_TEXCOORD5             =  3225, // CG_BINDLOCATION_MACRO(TexCoord5,CG_TEXCOORD5,"TEXCOORD5",3225,0,cgConnectorMemberParam)
    CG_TEXCOORD6             =  3226, // CG_BINDLOCATION_MACRO(TexCoord6,CG_TEXCOORD6,"TEXCOORD6",3226,0,cgConnectorMemberParam)
    CG_TEXCOORD7             =  3227, // CG_BINDLOCATION_MACRO(TexCoord7,CG_TEXCOORD7,"TEXCOORD7",3227,0,cgConnectorMemberParam)
    CG_TEXCOORD8             =  3228, // CG_BINDLOCATION_MACRO(TexCoord8,CG_TEXCOORD8,"TEXCOORD8",3228,0,cgConnectorMemberParam)
    CG_TEXCOORD9             =  3229, // CG_BINDLOCATION_MACRO(TexCoord9,CG_TEXCOORD9,"TEXCOORD9",3229,0,cgConnectorMemberParam)
    CG_TEXCOORD10            =  3230, // CG_BINDLOCATION_MACRO(TexCoord10,CG_TEXCOORD10,"TEXCOORD10",3230,0,cgConnectorMemberParam)
    CG_TEXCOORD11            =  3231, // CG_BINDLOCATION_MACRO(TexCoord11,CG_TEXCOORD11,"TEXCOORD11",3231,0,cgConnectorMemberParam)
    CG_TEXCOORD12            =  3232, // CG_BINDLOCATION_MACRO(TexCoord12,CG_TEXCOORD12,"TEXCOORD12",3232,0,cgConnectorMemberParam)
    CG_TEXCOORD13            =  3233, // CG_BINDLOCATION_MACRO(TexCoord13,CG_TEXCOORD13,"TEXCOORD13",3233,0,cgConnectorMemberParam)
    CG_TEXCOORD14            =  3234, // CG_BINDLOCATION_MACRO(TexCoord14,CG_TEXCOORD14,"TEXCOORD14",3234,0,cgConnectorMemberParam)
    CG_TEXCOORD15            =  3235, // CG_BINDLOCATION_MACRO(TexCoord15,CG_TEXCOORD15,"TEXCOORD15",3235,0,cgConnectorMemberParam)
    CG_COMBINER_CONST0       =  3284, // CG_BINDLOCATION_MACRO(CombinerConst0,CG_COMBINER_CONST0,"COMBINER_CONST0",3284,0,cgUniformParam)
    CG_COMBINER_CONST1       =  3285, // CG_BINDLOCATION_MACRO(CombinerConst1,CG_COMBINER_CONST1,"COMBINER_CONST1",3285,0,cgUniformParam)
    CG_COMBINER_STAGE_CONST0 =  3286, // CG_BINDLOCATION_MACRO(CombinerStageConst0,CG_COMBINER_STAGE_CONST0,"COMBINER_STAGE_CONST0",3286,1,cgUniformParam)
    CG_COMBINER_STAGE_CONST1 =  3287, // CG_BINDLOCATION_MACRO(CombinerStageConst1,CG_COMBINER_STAGE_CONST1,"COMBINER_STAGE_CONST1",3287,1,cgUniformParam)
    CG_OFFSET_TEXTURE_MATRIX =  3288, // CG_BINDLOCATION_MACRO(OffsetTextureMatrix,CG_OFFSET_TEXTURE_MATRIX,"OFFSET_TEXTURE_MATRIX",3288,0,cgUniformParam)
    CG_OFFSET_TEXTURE_SCALE  =  3289, // CG_BINDLOCATION_MACRO(OffsetTextureScale,CG_OFFSET_TEXTURE_SCALE,"OFFSET_TEXTURE_SCALE",3289,0,cgUniformParam)
    CG_OFFSET_TEXTURE_BIAS   =  3290, // CG_BINDLOCATION_MACRO(OffsetTextureBias,CG_OFFSET_TEXTURE_BIAS,"OFFSET_TEXTURE_BIAS",3290,0,cgUniformParam)
    CG_CONST_EYE             =  3291, // CG_BINDLOCATION_MACRO(ConstEye,CG_CONST_EYE,"CONST_EYE",3291,0,cgUniformParam)
    CG_TESSFACTOR            =  3255, // CG_BINDLOCATION_MACRO(TessFactor,CG_TESSFACTOR,"TESSFACTOR",3255,0,cgConnectorMemberParam)

    CG_GLSL_UNIFORM          =  3300, // CG_BINDLOCATION_MACRO(GLSLUniform,CG_GLSL_UNIFORM,"glsl_uniform",3300,1,CGI_UNIFORM_PARAM)
    CG_GLSL_ATTRIB           =  3301, // CG_BINDLOCATION_MACRO(GLSLAttrib,CG_GLSL_ATTRIB,"glsl_attrib",3301,1,CG_VARYING_PARAM)

    // 256 + 1 of these
    CG_ENV                   =  3302,
    CG_ENV0                  =  3303,
    CG_ENV1                  =  3304,
    CG_ENV2                  =  3305,
    CG_ENV3                  =  3306,
    CG_ENV4                  =  3307,
    CG_ENV5                  =  3308,
    CG_ENV6                  =  3309,
    CG_ENV7                  =  3310,
    CG_ENV8                  =  3311,
    CG_ENV9                  =  3312,
    CG_ENV10                 =  3313,
    CG_ENV11                 =  3314,
    CG_ENV12                 =  3315,
    CG_ENV13                 =  3316,
    CG_ENV14                 =  3317,
    CG_ENV15                 =  3318,
    CG_ENV16                 =  3319,
    CG_ENV17                 =  3320,
    CG_ENV18                 =  3321,
    CG_ENV19                 =  3322,
    CG_ENV20                 =  3323,
    CG_ENV21                 =  3324,
    CG_ENV22                 =  3325,
    CG_ENV23                 =  3326,
    CG_ENV24                 =  3327,
    CG_ENV25                 =  3328,
    CG_ENV26                 =  3329,
    CG_ENV27                 =  3330,
    CG_ENV28                 =  3331,
    CG_ENV29                 =  3332,
    CG_ENV30                 =  3333,
    CG_ENV31                 =  3334,
    CG_ENV32                 =  3335,
    CG_ENV33                 =  3336,
    CG_ENV34                 =  3337,
    CG_ENV35                 =  3338,
    CG_ENV36                 =  3339,
    CG_ENV37                 =  3340,
    CG_ENV38                 =  3341,
    CG_ENV39                 =  3342,
    CG_ENV40                 =  3343,
    CG_ENV41                 =  3344,
    CG_ENV42                 =  3345,
    CG_ENV43                 =  3346,
    CG_ENV44                 =  3347,
    CG_ENV45                 =  3348,
    CG_ENV46                 =  3349,
    CG_ENV47                 =  3350,
    CG_ENV48                 =  3351,
    CG_ENV49                 =  3352,
    CG_ENV50                 =  3353,
    CG_ENV51                 =  3354,
    CG_ENV52                 =  3355,
    CG_ENV53                 =  3356,
    CG_ENV54                 =  3357,
    CG_ENV55                 =  3358,
    CG_ENV56                 =  3359,
    CG_ENV57                 =  3360,
    CG_ENV58                 =  3361,
    CG_ENV59                 =  3362,
    CG_ENV60                 =  3363,
    CG_ENV61                 =  3364,
    CG_ENV62                 =  3365,
    CG_ENV63                 =  3366,
    CG_ENV64                 =  3367,
    CG_ENV65                 =  3368,
    CG_ENV66                 =  3369,
    CG_ENV67                 =  3370,
    CG_ENV68                 =  3371,
    CG_ENV69                 =  3372,
    CG_ENV70                 =  3373,
    CG_ENV71                 =  3374,
    CG_ENV72                 =  3375,
    CG_ENV73                 =  3376,
    CG_ENV74                 =  3377,
    CG_ENV75                 =  3378,
    CG_ENV76                 =  3379,
    CG_ENV77                 =  3380,
    CG_ENV78                 =  3381,
    CG_ENV79                 =  3382,
    CG_ENV80                 =  3383,
    CG_ENV81                 =  3384,
    CG_ENV82                 =  3385,
    CG_ENV83                 =  3386,
    CG_ENV84                 =  3387,
    CG_ENV85                 =  3388,
    CG_ENV86                 =  3389,
    CG_ENV87                 =  3390,
    CG_ENV88                 =  3391,
    CG_ENV89                 =  3392,
    CG_ENV90                 =  3393,
    CG_ENV91                 =  3394,
    CG_ENV92                 =  3395,
    CG_ENV93                 =  3396,
    CG_ENV94                 =  3397,
    CG_ENV95                 =  3398,
    CG_ENV96                 =  3399,
    CG_ENV97                 =  3400,
    CG_ENV98                 =  3401,
    CG_ENV99                 =  3402,
    CG_ENV100                =  3403,
    CG_ENV101                =  3404,
    CG_ENV102                =  3405,
    CG_ENV103                =  3406,
    CG_ENV104                =  3407,
    CG_ENV105                =  3408,
    CG_ENV106                =  3409,
    CG_ENV107                =  3410,
    CG_ENV108                =  3411,
    CG_ENV109                =  3412,
    CG_ENV110                =  3413,
    CG_ENV111                =  3414,
    CG_ENV112                =  3415,
    CG_ENV113                =  3416,
    CG_ENV114                =  3417,
    CG_ENV115                =  3418,
    CG_ENV116                =  3419,
    CG_ENV117                =  3420,
    CG_ENV118                =  3421,
    CG_ENV119                =  3422,
    CG_ENV120                =  3423,
    CG_ENV121                =  3424,
    CG_ENV122                =  3425,
    CG_ENV123                =  3426,
    CG_ENV124                =  3427,
    CG_ENV125                =  3428,
    CG_ENV126                =  3429,
    CG_ENV127                =  3430,
    CG_ENV128                =  3431,
    CG_ENV129                =  3432,
    CG_ENV130                =  3433,
    CG_ENV131                =  3434,
    CG_ENV132                =  3435,
    CG_ENV133                =  3436,
    CG_ENV134                =  3437,
    CG_ENV135                =  3438,
    CG_ENV136                =  3439,
    CG_ENV137                =  3440,
    CG_ENV138                =  3441,
    CG_ENV139                =  3442,
    CG_ENV140                =  3443,
    CG_ENV141                =  3444,
    CG_ENV142                =  3445,
    CG_ENV143                =  3446,
    CG_ENV144                =  3447,
    CG_ENV145                =  3448,
    CG_ENV146                =  3449,
    CG_ENV147                =  3450,
    CG_ENV148                =  3451,
    CG_ENV149                =  3452,
    CG_ENV150                =  3453,
    CG_ENV151                =  3454,
    CG_ENV152                =  3455,
    CG_ENV153                =  3456,
    CG_ENV154                =  3457,
    CG_ENV155                =  3458,
    CG_ENV156                =  3459,
    CG_ENV157                =  3460,
    CG_ENV158                =  3461,
    CG_ENV159                =  3462,
    CG_ENV160                =  3463,
    CG_ENV161                =  3464,
    CG_ENV162                =  3465,
    CG_ENV163                =  3466,
    CG_ENV164                =  3467,
    CG_ENV165                =  3468,
    CG_ENV166                =  3469,
    CG_ENV167                =  3470,
    CG_ENV168                =  3471,
    CG_ENV169                =  3472,
    CG_ENV170                =  3473,
    CG_ENV171                =  3474,
    CG_ENV172                =  3475,
    CG_ENV173                =  3476,
    CG_ENV174                =  3477,
    CG_ENV175                =  3478,
    CG_ENV176                =  3479,
    CG_ENV177                =  3480,
    CG_ENV178                =  3481,
    CG_ENV179                =  3482,
    CG_ENV180                =  3483,
    CG_ENV181                =  3484,
    CG_ENV182                =  3485,
    CG_ENV183                =  3486,
    CG_ENV184                =  3487,
    CG_ENV185                =  3488,
    CG_ENV186                =  3489,
    CG_ENV187                =  3490,
    CG_ENV188                =  3491,
    CG_ENV189                =  3492,
    CG_ENV190                =  3493,
    CG_ENV191                =  3494,
    CG_ENV192                =  3495,
    CG_ENV193                =  3496,
    CG_ENV194                =  3497,
    CG_ENV195                =  3498,
    CG_ENV196                =  3499,
    CG_ENV197                =  3500,
    CG_ENV198                =  3501,
    CG_ENV199                =  3502,
    CG_ENV200                =  3503,
    CG_ENV201                =  3504,
    CG_ENV202                =  3505,
    CG_ENV203                =  3506,
    CG_ENV204                =  3507,
    CG_ENV205                =  3508,
    CG_ENV206                =  3509,
    CG_ENV207                =  3510,
    CG_ENV208                =  3511,
    CG_ENV209                =  3512,
    CG_ENV210                =  3513,
    CG_ENV211                =  3514,
    CG_ENV212                =  3515,
    CG_ENV213                =  3516,
    CG_ENV214                =  3517,
    CG_ENV215                =  3518,
    CG_ENV216                =  3519,
    CG_ENV217                =  3520,
    CG_ENV218                =  3521,
    CG_ENV219                =  3522,
    CG_ENV220                =  3523,
    CG_ENV221                =  3524,
    CG_ENV222                =  3525,
    CG_ENV223                =  3526,
    CG_ENV224                =  3527,
    CG_ENV225                =  3528,
    CG_ENV226                =  3529,
    CG_ENV227                =  3530,
    CG_ENV228                =  3531,
    CG_ENV229                =  3532,
    CG_ENV230                =  3533,
    CG_ENV231                =  3534,
    CG_ENV232                =  3535,
    CG_ENV233                =  3536,
    CG_ENV234                =  3537,
    CG_ENV235                =  3538,
    CG_ENV236                =  3539,
    CG_ENV237                =  3540,
    CG_ENV238                =  3541,
    CG_ENV239                =  3542,
    CG_ENV240                =  3543,
    CG_ENV241                =  3544,
    CG_ENV242                =  3545,
    CG_ENV243                =  3546,
    CG_ENV244                =  3547,
    CG_ENV245                =  3548,
    CG_ENV246                =  3549,
    CG_ENV247                =  3550,
    CG_ENV248                =  3551,
    CG_ENV249                =  3552,
    CG_ENV250                =  3553,
    CG_ENV251                =  3554,
    CG_ENV252                =  3555,
    CG_ENV253                =  3556,
    CG_ENV254                =  3557,
    CG_ENV255                =  3558,

    CG_UNDEFINED             = 3256
  {$ENDIF}
  );
{$ELSE}
const
  //# define CG_BINDLOCATION_MACRO(name,enum_name,compiler_name,\
  //                               enum_int,addressable,param_type) \
  //  enum_name = enum_int,

  CG_TEXUNIT0              =  2048; // CG_BINDLOCATION_MACRO(TexUnit0,CG_TEXUNIT0,"texunit 0",2048,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT0}
  CG_TEXUNIT1              =  2049; // CG_BINDLOCATION_MACRO(TexUnit1,CG_TEXUNIT1,"texunit 1",2049,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT1}
  CG_TEXUNIT2              =  2050; // CG_BINDLOCATION_MACRO(TexUnit2,CG_TEXUNIT2,"texunit 2",2050,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT2}
  CG_TEXUNIT3              =  2051; // CG_BINDLOCATION_MACRO(TexUnit3,CG_TEXUNIT3,"texunit 3",2051,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT3}
  CG_TEXUNIT4              =  2052; // CG_BINDLOCATION_MACRO(TexUnit4,CG_TEXUNIT4,"texunit 4",2052,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT4}
  CG_TEXUNIT5              =  2053; // CG_BINDLOCATION_MACRO(TexUnit5,CG_TEXUNIT5,"texunit 5",2053,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT5}
  CG_TEXUNIT6              =  2054; // CG_BINDLOCATION_MACRO(TexUnit6,CG_TEXUNIT6,"texunit 6",2054,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT6}
  CG_TEXUNIT7              =  2055; // CG_BINDLOCATION_MACRO(TexUnit7,CG_TEXUNIT7,"texunit 7",2055,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT7}
  CG_TEXUNIT8              =  2056; // CG_BINDLOCATION_MACRO(TexUnit8,CG_TEXUNIT8,"texunit 8",2056,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT8}
  CG_TEXUNIT9              =  2057; // CG_BINDLOCATION_MACRO(TexUnit9,CG_TEXUNIT9,"texunit 9",2057,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT9}
  CG_TEXUNIT10             =  2058; // CG_BINDLOCATION_MACRO(TexUnit10,CG_TEXUNIT10,"texunit 10",2058,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT10}
  CG_TEXUNIT11             =  2059; // CG_BINDLOCATION_MACRO(TexUnit11,CG_TEXUNIT11,"texunit 11",2059,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT11}
  CG_TEXUNIT12             =  2060; // CG_BINDLOCATION_MACRO(TexUnit12,CG_TEXUNIT12,"texunit 12",2060,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT12}
  CG_TEXUNIT13             =  2061; // CG_BINDLOCATION_MACRO(TexUnit13,CG_TEXUNIT13,"texunit 13",2061,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT13}
  CG_TEXUNIT14             =  2062; // CG_BINDLOCATION_MACRO(TexUnit14,CG_TEXUNIT14,"texunit 14",2062,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT14}
  CG_TEXUNIT15             =  2063; // CG_BINDLOCATION_MACRO(TexUnit15,CG_TEXUNIT15,"texunit 15",2063,0,cgTexObjParam)
  {$EXTERNALSYM CG_TEXUNIT15}

  CG_ATTR0                 =  2113; // CG_BINDLOCATION_MACRO(Attr0,CG_ATTR0,"ATTR0",2113,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR0}
  CG_ATTR1                 =  2114; // CG_BINDLOCATION_MACRO(Attr1,CG_ATTR1,"ATTR1",2114,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR1}
  CG_ATTR2                 =  2115; // CG_BINDLOCATION_MACRO(Attr2,CG_ATTR2,"ATTR2",2115,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR2}
  CG_ATTR3                 =  2116; // CG_BINDLOCATION_MACRO(Attr3,CG_ATTR3,"ATTR3",2116,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR3}
  CG_ATTR4                 =  2117; // CG_BINDLOCATION_MACRO(Attr4,CG_ATTR4,"ATTR4",2117,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR4}
  CG_ATTR5                 =  2118; // CG_BINDLOCATION_MACRO(Attr5,CG_ATTR5,"ATTR5",2118,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR5}
  CG_ATTR6                 =  2119; // CG_BINDLOCATION_MACRO(Attr6,CG_ATTR6,"ATTR6",2119,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR6}
  CG_ATTR7                 =  2120; // CG_BINDLOCATION_MACRO(Attr7,CG_ATTR7,"ATTR7",2120,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR7}
  CG_ATTR8                 =  2121; // CG_BINDLOCATION_MACRO(Attr8,CG_ATTR8,"ATTR8",2121,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR8}
  CG_ATTR9                 =  2122; // CG_BINDLOCATION_MACRO(Attr9,CG_ATTR9,"ATTR9",2122,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR9}
  CG_ATTR10                =  2123; // CG_BINDLOCATION_MACRO(Attr10,CG_ATTR10,"ATTR10",2123,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR10}
  CG_ATTR11                =  2124; // CG_BINDLOCATION_MACRO(Attr11,CG_ATTR11,"ATTR11",2124,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR11}
  CG_ATTR12                =  2125; // CG_BINDLOCATION_MACRO(Attr12,CG_ATTR12,"ATTR12",2125,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR12}
  CG_ATTR13                =  2126; // CG_BINDLOCATION_MACRO(Attr13,CG_ATTR13,"ATTR13",2126,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR13}
  CG_ATTR14                =  2127; // CG_BINDLOCATION_MACRO(Attr14,CG_ATTR14,"ATTR14",2127,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR14}
  CG_ATTR15                =  2128; // CG_BINDLOCATION_MACRO(Attr15,CG_ATTR15,"ATTR15",2128,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_ATTR15}

  CG_C                     =  2178; // CG_BINDLOCATION_MACRO(VertUniform,CG_C,"c",2178,1,cgUniformParam)
  {$EXTERNALSYM CG_C}

  CG_TEX0                  =  2179; // CG_BINDLOCATION_MACRO(Tex0,CG_TEX0,"TEX0",2179,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX0}
  CG_TEX1                  =  2180; // CG_BINDLOCATION_MACRO(Tex1,CG_TEX1,"TEX1",2180,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX1}
  CG_TEX2                  =  2181; // CG_BINDLOCATION_MACRO(Tex2,CG_TEX2,"TEX2",2181,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX2}
  CG_TEX3                  =  2192; // CG_BINDLOCATION_MACRO(Tex3,CG_TEX3,"TEX3",2192,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX3}
  CG_TEX4                  =  2193; // CG_BINDLOCATION_MACRO(Tex4,CG_TEX4,"TEX4",2193,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX4}
  CG_TEX5                  =  2194; // CG_BINDLOCATION_MACRO(Tex5,CG_TEX5,"TEX5",2194,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX5}
  CG_TEX6                  =  2195; // CG_BINDLOCATION_MACRO(Tex6,CG_TEX6,"TEX6",2195,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX6}
  CG_TEX7                  =  2196; // CG_BINDLOCATION_MACRO(Tex7,CG_TEX7,"TEX7",2196,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEX7}

  CG_HPOS                  =  2243; // CG_BINDLOCATION_MACRO(HPos,CG_HPOS,"HPOS",2243,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_HPOS}
  CG_COL0                  =  2245; // CG_BINDLOCATION_MACRO(Col0,CG_COL0,"COL0",2245,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COL0}
  CG_COL1                  =  2246; // CG_BINDLOCATION_MACRO(Col1,CG_COL1,"COL1",2246,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COL1}
  CG_COL2                  =  2247; // CG_BINDLOCATION_MACRO(Col2,CG_COL2,"COL2",2247,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COL2}
  CG_COL3                  =  2248; // CG_BINDLOCATION_MACRO(Col3,CG_COL3,"COL3",2248,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COL3}
  CG_PSIZ                  =  2309; // CG_BINDLOCATION_MACRO(PSiz,CG_PSIZ,"PSIZ",2309,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZ}
  CG_CLP0                  =  2310; // CG_BINDLOCATION_MACRO(Clp0,CG_CLP0,"CLP0",2310,0,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_CLP0}
  CG_CLP1                  =  2311; // CG_BINDLOCATION_MACRO(Clp1,CG_CLP1,"CLP1",2311,0,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_CLP1}
  CG_CLP2                  =  2312; // CG_BINDLOCATION_MACRO(Clp2,CG_CLP2,"CLP2",2312,0,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_CLP2}
  CG_CLP3                  =  2313; // CG_BINDLOCATION_MACRO(Clp3,CG_CLP3,"CLP3",2313,0,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_CLP3}
  CG_CLP4                  =  2314; // CG_BINDLOCATION_MACRO(Clp4,CG_CLP4,"CLP4",2314,0,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_CLP4}
  CG_CLP5                  =  2315; // CG_BINDLOCATION_MACRO(Clp5,CG_CLP5,"CLP5",2315,0,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_CLP5}
  CG_WPOS                  =  2373; // CG_BINDLOCATION_MACRO(WPos,CG_WPOS,"WPOS",2373,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_WPOS}
  CG_POINTCOORD            =  2374; // CG_BINDLOCATION_MACRO(PointCoord,CG_POINTCOORD,"POINTCOORD",2374,0,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_POINTCOORD}


  CG_POSITION0             =  2437; // CG_BINDLOCATION_MACRO(Position0,CG_POSITION0,"POSITION0",2437,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION0}
  CG_POSITION1             =  2438; // CG_BINDLOCATION_MACRO(Position1,CG_POSITION1,"POSITION1",2438,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION1}
  CG_POSITION2             =  2439; // CG_BINDLOCATION_MACRO(Position2,CG_POSITION2,"POSITION2",2439,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION2}
  CG_POSITION3             =  2440; // CG_BINDLOCATION_MACRO(Position3,CG_POSITION3,"POSITION3",2440,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION3}
  CG_POSITION4             =  2441; // CG_BINDLOCATION_MACRO(Position4,CG_POSITION4,"POSITION4",2441,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION4}
  CG_POSITION5             =  2442; // CG_BINDLOCATION_MACRO(Position5,CG_POSITION5,"POSITION5",2442,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION5}
  CG_POSITION6             =  2443; // CG_BINDLOCATION_MACRO(Position6,CG_POSITION6,"POSITION6",2443,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION6}
  CG_POSITION7             =  2444; // CG_BINDLOCATION_MACRO(Position7,CG_POSITION7,"POSITION7",2444,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION7}
  CG_POSITION8             =  2445; // CG_BINDLOCATION_MACRO(Position8,CG_POSITION8,"POSITION8",2445,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION8}
  CG_POSITION9             =  2446; // CG_BINDLOCATION_MACRO(Position9,CG_POSITION9,"POSITION9",2446,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION9}
  CG_POSITION10            =  2447; // CG_BINDLOCATION_MACRO(Position10,CG_POSITION10,"POSITION10",2447,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION10}
  CG_POSITION11            =  2448; // CG_BINDLOCATION_MACRO(Position11,CG_POSITION11,"POSITION11",2448,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION11}
  CG_POSITION12            =  2449; // CG_BINDLOCATION_MACRO(Position12,CG_POSITION12,"POSITION12",2449,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION12}
  CG_POSITION13            =  2450; // CG_BINDLOCATION_MACRO(Position13,CG_POSITION13,"POSITION13",2450,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION13}
  CG_POSITION14            =  2451; // CG_BINDLOCATION_MACRO(Position14,CG_POSITION14,"POSITION14",2451,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION14}
  CG_POSITION15            =  2452; // CG_BINDLOCATION_MACRO(Position15,CG_POSITION15,"POSITION15",2452,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_POSITION15}
  CG_DIFFUSE0              =  2501; // CG_BINDLOCATION_MACRO(Diffuse0,CG_DIFFUSE0,"DIFFUSE0",2501,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DIFFUSE0}
  CG_TANGENT0              =  2565; // CG_BINDLOCATION_MACRO(Tangent0,CG_TANGENT0,"TANGENT0",2565,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT0}
  CG_TANGENT1              =  2566; // CG_BINDLOCATION_MACRO(Tangent1,CG_TANGENT1,"TANGENT1",2566,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT1}
  CG_TANGENT2              =  2567; // CG_BINDLOCATION_MACRO(Tangent2,CG_TANGENT2,"TANGENT2",2567,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT2}
  CG_TANGENT3              =  2568; // CG_BINDLOCATION_MACRO(Tangent3,CG_TANGENT3,"TANGENT3",2568,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT3}
  CG_TANGENT4              =  2569; // CG_BINDLOCATION_MACRO(Tangent4,CG_TANGENT4,"TANGENT4",2569,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT4}
  CG_TANGENT5              =  2570; // CG_BINDLOCATION_MACRO(Tangent5,CG_TANGENT5,"TANGENT5",2570,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT5}
  CG_TANGENT6              =  2571; // CG_BINDLOCATION_MACRO(Tangent6,CG_TANGENT6,"TANGENT6",2571,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT6}
  CG_TANGENT7              =  2572; // CG_BINDLOCATION_MACRO(Tangent7,CG_TANGENT7,"TANGENT7",2572,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT7}
  CG_TANGENT8              =  2573; // CG_BINDLOCATION_MACRO(Tangent8,CG_TANGENT8,"TANGENT8",2573,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT8}
  CG_TANGENT9              =  2574; // CG_BINDLOCATION_MACRO(Tangent9,CG_TANGENT9,"TANGENT9",2574,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT9}
  CG_TANGENT10             =  2575; // CG_BINDLOCATION_MACRO(Tangent10,CG_TANGENT10,"TANGENT10",2575,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT10}
  CG_TANGENT11             =  2576; // CG_BINDLOCATION_MACRO(Tangent11,CG_TANGENT11,"TANGENT11",2576,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT11}
  CG_TANGENT12             =  2577; // CG_BINDLOCATION_MACRO(Tangent12,CG_TANGENT12,"TANGENT12",2577,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT12}
  CG_TANGENT13             =  2578; // CG_BINDLOCATION_MACRO(Tangent13,CG_TANGENT13,"TANGENT13",2578,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT13}
  CG_TANGENT14             =  2579; // CG_BINDLOCATION_MACRO(Tangent14,CG_TANGENT14,"TANGENT14",2579,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT14}
  CG_TANGENT15             =  2580; // CG_BINDLOCATION_MACRO(Tangent15,CG_TANGENT15,"TANGENT15",2580,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TANGENT15}
  CG_SPECULAR0             =  2629; // CG_BINDLOCATION_MACRO(Specular0,CG_SPECULAR0,"SPECULAR0",2629,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SPECULAR0}
  CG_BLENDINDICES0         =  2693; // CG_BINDLOCATION_MACRO(BlendIndices0,CG_BLENDINDICES0,"BLENDINDICES0",2693,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES0}
  CG_BLENDINDICES1         =  2694; // CG_BINDLOCATION_MACRO(BlendIndices1,CG_BLENDINDICES1,"BLENDINDICES1",2694,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES1}
  CG_BLENDINDICES2         =  2695; // CG_BINDLOCATION_MACRO(BlendIndices2,CG_BLENDINDICES2,"BLENDINDICES2",2695,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES2}
  CG_BLENDINDICES3         =  2696; // CG_BINDLOCATION_MACRO(BlendIndices3,CG_BLENDINDICES3,"BLENDINDICES3",2696,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES3}
  CG_BLENDINDICES4         =  2697; // CG_BINDLOCATION_MACRO(BlendIndices4,CG_BLENDINDICES4,"BLENDINDICES4",2697,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES4}
  CG_BLENDINDICES5         =  2698; // CG_BINDLOCATION_MACRO(BlendIndices5,CG_BLENDINDICES5,"BLENDINDICES5",2698,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES5}
  CG_BLENDINDICES6         =  2699; // CG_BINDLOCATION_MACRO(BlendIndices6,CG_BLENDINDICES6,"BLENDINDICES6",2699,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES6}
  CG_BLENDINDICES7         =  2700; // CG_BINDLOCATION_MACRO(BlendIndices7,CG_BLENDINDICES7,"BLENDINDICES7",2700,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES7}
  CG_BLENDINDICES8         =  2701; // CG_BINDLOCATION_MACRO(BlendIndices8,CG_BLENDINDICES8,"BLENDINDICES8",2701,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES8}
  CG_BLENDINDICES9         =  2702; // CG_BINDLOCATION_MACRO(BlendIndices9,CG_BLENDINDICES9,"BLENDINDICES9",2702,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES9}
  CG_BLENDINDICES10        =  2703; // CG_BINDLOCATION_MACRO(BlendIndices10,CG_BLENDINDICES10,"BLENDINDICES10",2703,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES10}
  CG_BLENDINDICES11        =  2704; // CG_BINDLOCATION_MACRO(BlendIndices11,CG_BLENDINDICES11,"BLENDINDICES11",2704,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES11}
  CG_BLENDINDICES12        =  2705; // CG_BINDLOCATION_MACRO(BlendIndices12,CG_BLENDINDICES12,"BLENDINDICES12",2705,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES12}
  CG_BLENDINDICES13        =  2706; // CG_BINDLOCATION_MACRO(BlendIndices13,CG_BLENDINDICES13,"BLENDINDICES13",2706,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES13}
  CG_BLENDINDICES14        =  2707; // CG_BINDLOCATION_MACRO(BlendIndices14,CG_BLENDINDICES14,"BLENDINDICES14",2707,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES14}
  CG_BLENDINDICES15        =  2708; // CG_BINDLOCATION_MACRO(BlendIndices15,CG_BLENDINDICES15,"BLENDINDICES15",2708,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDINDICES15}
  CG_COLOR0                =  2757; // CG_BINDLOCATION_MACRO(Color0,CG_COLOR0,"COLOR0",2757,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR0}
  CG_COLOR1                =  2758; // CG_BINDLOCATION_MACRO(Color1,CG_COLOR1,"COLOR1",2758,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR1}
  CG_COLOR2                =  2759; // CG_BINDLOCATION_MACRO(Color2,CG_COLOR2,"COLOR2",2759,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR2}
  CG_COLOR3                =  2760; // CG_BINDLOCATION_MACRO(Color3,CG_COLOR3,"COLOR3",2760,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR3}
  CG_COLOR4                =  2761; // CG_BINDLOCATION_MACRO(Color4,CG_COLOR4,"COLOR4",2761,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR4}
  CG_COLOR5                =  2762; // CG_BINDLOCATION_MACRO(Color5,CG_COLOR5,"COLOR5",2762,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR5}
  CG_COLOR6                =  2763; // CG_BINDLOCATION_MACRO(Color6,CG_COLOR6,"COLOR6",2763,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR6}
  CG_COLOR7                =  2764; // CG_BINDLOCATION_MACRO(Color7,CG_COLOR7,"COLOR7",2764,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR7}
  CG_COLOR8                =  2765; // CG_BINDLOCATION_MACRO(Color8,CG_COLOR8,"COLOR8",2765,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR8}
  CG_COLOR9                =  2766; // CG_BINDLOCATION_MACRO(Color9,CG_COLOR9,"COLOR9",2766,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR9}
  CG_COLOR10               =  2767; // CG_BINDLOCATION_MACRO(Color10,CG_COLOR10,"COLOR10",2767,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR10}
  CG_COLOR11               =  2768; // CG_BINDLOCATION_MACRO(Color11,CG_COLOR11,"COLOR11",2768,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR11}
  CG_COLOR12               =  2769; // CG_BINDLOCATION_MACRO(Color12,CG_COLOR12,"COLOR12",2769,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR12}
  CG_COLOR13               =  2770; // CG_BINDLOCATION_MACRO(Color13,CG_COLOR13,"COLOR13",2770,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR13}
  CG_COLOR14               =  2771; // CG_BINDLOCATION_MACRO(Color14,CG_COLOR14,"COLOR14",2771,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR14}
  CG_COLOR15               =  2772; // CG_BINDLOCATION_MACRO(Color15,CG_COLOR15,"COLOR15",2772,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_COLOR15}
  CG_PSIZE0                =  2821; // CG_BINDLOCATION_MACRO(PSize0,CG_PSIZE0,"PSIZE0",2821,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE0}
  CG_PSIZE1                =  2822; // CG_BINDLOCATION_MACRO(PSize1,CG_PSIZE1,"PSIZE1",2822,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE1}
  CG_PSIZE2                =  2823; // CG_BINDLOCATION_MACRO(PSize2,CG_PSIZE2,"PSIZE2",2823,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE2}
  CG_PSIZE3                =  2824; // CG_BINDLOCATION_MACRO(PSize3,CG_PSIZE3,"PSIZE3",2824,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE3}
  CG_PSIZE4                =  2825; // CG_BINDLOCATION_MACRO(PSize4,CG_PSIZE4,"PSIZE4",2825,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE4}
  CG_PSIZE5                =  2826; // CG_BINDLOCATION_MACRO(PSize5,CG_PSIZE5,"PSIZE5",2826,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE5}
  CG_PSIZE6                =  2827; // CG_BINDLOCATION_MACRO(PSize6,CG_PSIZE6,"PSIZE6",2827,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE6}
  CG_PSIZE7                =  2828; // CG_BINDLOCATION_MACRO(PSize7,CG_PSIZE7,"PSIZE7",2828,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE7}
  CG_PSIZE8                =  2829; // CG_BINDLOCATION_MACRO(PSize8,CG_PSIZE8,"PSIZE8",2829,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE8}
  CG_PSIZE9                =  2830; // CG_BINDLOCATION_MACRO(PSize9,CG_PSIZE9,"PSIZE9",2830,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE9}
  CG_PSIZE10               =  2831; // CG_BINDLOCATION_MACRO(PSize10,CG_PSIZE10,"PSIZE10",2831,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE10}
  CG_PSIZE11               =  2832; // CG_BINDLOCATION_MACRO(PSize11,CG_PSIZE11,"PSIZE11",2832,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE11}
  CG_PSIZE12               =  2833; // CG_BINDLOCATION_MACRO(PSize12,CG_PSIZE12,"PSIZE12",2833,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE12}
  CG_PSIZE13               =  2834; // CG_BINDLOCATION_MACRO(PSize13,CG_PSIZE13,"PSIZE13",2834,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE13}
  CG_PSIZE14               =  2835; // CG_BINDLOCATION_MACRO(PSize14,CG_PSIZE14,"PSIZE14",2835,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE14}
  CG_PSIZE15               =  2836; // CG_BINDLOCATION_MACRO(PSize15,CG_PSIZE15,"PSIZE15",2836,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_PSIZE15}
  CG_BINORMAL0             =  2885; // CG_BINDLOCATION_MACRO(BiNormal0,CG_BINORMAL0,"BINORMAL0",2885,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL0}
  CG_BINORMAL1             =  2886; // CG_BINDLOCATION_MACRO(BiNormal1,CG_BINORMAL1,"BINORMAL1",2886,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL1}
  CG_BINORMAL2             =  2887; // CG_BINDLOCATION_MACRO(BiNormal2,CG_BINORMAL2,"BINORMAL2",2887,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL2}
  CG_BINORMAL3             =  2888; // CG_BINDLOCATION_MACRO(BiNormal3,CG_BINORMAL3,"BINORMAL3",2888,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL3}
  CG_BINORMAL4             =  2889; // CG_BINDLOCATION_MACRO(BiNormal4,CG_BINORMAL4,"BINORMAL4",2889,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL4}
  CG_BINORMAL5             =  2890; // CG_BINDLOCATION_MACRO(BiNormal5,CG_BINORMAL5,"BINORMAL5",2890,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL5}
  CG_BINORMAL6             =  2891; // CG_BINDLOCATION_MACRO(BiNormal6,CG_BINORMAL6,"BINORMAL6",2891,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL6}
  CG_BINORMAL7             =  2892; // CG_BINDLOCATION_MACRO(BiNormal7,CG_BINORMAL7,"BINORMAL7",2892,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL7}
  CG_BINORMAL8             =  2893; // CG_BINDLOCATION_MACRO(BiNormal8,CG_BINORMAL8,"BINORMAL8",2893,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL8}
  CG_BINORMAL9             =  2894; // CG_BINDLOCATION_MACRO(BiNormal9,CG_BINORMAL9,"BINORMAL9",2894,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL9}
  CG_BINORMAL10            =  2895; // CG_BINDLOCATION_MACRO(BiNormal10,CG_BINORMAL10,"BINORMAL10",2895,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL10}
  CG_BINORMAL11            =  2896; // CG_BINDLOCATION_MACRO(BiNormal11,CG_BINORMAL11,"BINORMAL11",2896,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL11}
  CG_BINORMAL12            =  2897; // CG_BINDLOCATION_MACRO(BiNormal12,CG_BINORMAL12,"BINORMAL12",2897,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL12}
  CG_BINORMAL13            =  2898; // CG_BINDLOCATION_MACRO(BiNormal13,CG_BINORMAL13,"BINORMAL13",2898,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL13}
  CG_BINORMAL14            =  2899; // CG_BINDLOCATION_MACRO(BiNormal14,CG_BINORMAL14,"BINORMAL14",2899,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL14}
  CG_BINORMAL15            =  2900; // CG_BINDLOCATION_MACRO(BiNormal15,CG_BINORMAL15,"BINORMAL15",2900,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BINORMAL15}
  CG_FOG0                  =  2917; // CG_BINDLOCATION_MACRO(FOG0,CG_FOG0,"FOG0",2917,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG0}
  CG_FOG1                  =  2918; // CG_BINDLOCATION_MACRO(FOG1,CG_FOG1,"FOG1",2918,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG1}
  CG_FOG2                  =  2919; // CG_BINDLOCATION_MACRO(FOG2,CG_FOG2,"FOG2",2919,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG2}
  CG_FOG3                  =  2920; // CG_BINDLOCATION_MACRO(FOG3,CG_FOG3,"FOG3",2920,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG3}
  CG_FOG4                  =  2921; // CG_BINDLOCATION_MACRO(FOG4,CG_FOG4,"FOG4",2921,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG4}
  CG_FOG5                  =  2922; // CG_BINDLOCATION_MACRO(FOG5,CG_FOG5,"FOG5",2922,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG5}
  CG_FOG6                  =  2923; // CG_BINDLOCATION_MACRO(FOG6,CG_FOG6,"FOG6",2923,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG6}
  CG_FOG7                  =  2924; // CG_BINDLOCATION_MACRO(FOG7,CG_FOG7,"FOG7",2924,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG7}
  CG_FOG8                  =  2925; // CG_BINDLOCATION_MACRO(FOG8,CG_FOG8,"FOG8",2925,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG8}
  CG_FOG9                  =  2926; // CG_BINDLOCATION_MACRO(FOG9,CG_FOG9,"FOG9",2926,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG9}
  CG_FOG10                 =  2927; // CG_BINDLOCATION_MACRO(FOG10,CG_FOG10,"FOG10",2927,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG10}
  CG_FOG11                 =  2928; // CG_BINDLOCATION_MACRO(FOG11,CG_FOG11,"FOG11",2928,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG11}
  CG_FOG12                 =  2929; // CG_BINDLOCATION_MACRO(FOG12,CG_FOG12,"FOG12",2929,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG12}
  CG_FOG13                 =  2930; // CG_BINDLOCATION_MACRO(FOG13,CG_FOG13,"FOG13",2930,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG13}
  CG_FOG14                 =  2931; // CG_BINDLOCATION_MACRO(FOG14,CG_FOG14,"FOG14",2931,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG14}
  CG_FOG15                 =  2932; // CG_BINDLOCATION_MACRO(FOG15,CG_FOG15,"FOG15",2932,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOG15}
  CG_DEPTH0                =  2933; // CG_BINDLOCATION_MACRO(DEPTH0,CG_DEPTH0,"DEPTH0",2933,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH0}
  CG_DEPTH1                =  2934; // CG_BINDLOCATION_MACRO(DEPTH1,CG_DEPTH1,"DEPTH1",2934,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH1}
  CG_DEPTH2                =  2935; // CG_BINDLOCATION_MACRO(DEPTH2,CG_DEPTH2,"DEPTH2",2935,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH2}
  CG_DEPTH3                =  2936; // CG_BINDLOCATION_MACRO(DEPTH3,CG_DEPTH3,"DEPTH3",2936,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH3}
  CG_DEPTH4                =  2937; // CG_BINDLOCATION_MACRO(DEPTH4,CG_DEPTH4,"DEPTH4",2937,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH4}
  CG_DEPTH5                =  2938; // CG_BINDLOCATION_MACRO(DEPTH5,CG_DEPTH5,"DEPTH5",2938,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH5}
  CG_DEPTH6                =  2939; // CG_BINDLOCATION_MACRO(DEPTH6,CG_DEPTH6,"DEPTH6",2939,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH6}
  CG_DEPTH7                =  2940; // CG_BINDLOCATION_MACRO(DEPTH7,CG_DEPTH7,"DEPTH7",2940,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH7}
  CG_DEPTH8                =  2941; // CG_BINDLOCATION_MACRO(DEPTH8,CG_DEPTH8,"DEPTH8",2941,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH8}
//TODO: what THE @#$% !!! ('29542')
  CG_DEPTH9                = 29542; // CG_BINDLOCATION_MACRO(DEPTH9,CG_DEPTH9,"DEPTH9",29542,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH9}
  CG_DEPTH10               =  2943; // CG_BINDLOCATION_MACRO(DEPTH10,CG_DEPTH10,"DEPTH10",2943,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH10}
  CG_DEPTH11               =  2944; // CG_BINDLOCATION_MACRO(DEPTH11,CG_DEPTH11,"DEPTH11",2944,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH11}
  CG_DEPTH12               =  2945; // CG_BINDLOCATION_MACRO(DEPTH12,CG_DEPTH12,"DEPTH12",2945,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH12}
  CG_DEPTH13               =  2946; // CG_BINDLOCATION_MACRO(DEPTH13,CG_DEPTH13,"DEPTH13",2946,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH13}
  CG_DEPTH14               =  2947; // CG_BINDLOCATION_MACRO(DEPTH14,CG_DEPTH14,"DEPTH14",2947,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH14}
  CG_DEPTH15               =  2948; // CG_BINDLOCATION_MACRO(DEPTH15,CG_DEPTH15,"DEPTH15",2948,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_DEPTH15}
  CG_SAMPLE0               =  2949; // CG_BINDLOCATION_MACRO(SAMPLE0,CG_SAMPLE0,"SAMPLE0",2949,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE0}
  CG_SAMPLE1               =  2950; // CG_BINDLOCATION_MACRO(SAMPLE1,CG_SAMPLE1,"SAMPLE1",2950,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE1}
  CG_SAMPLE2               =  2951; // CG_BINDLOCATION_MACRO(SAMPLE2,CG_SAMPLE2,"SAMPLE2",2951,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE2}
  CG_SAMPLE3               =  2952; // CG_BINDLOCATION_MACRO(SAMPLE3,CG_SAMPLE3,"SAMPLE3",2952,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE3}
  CG_SAMPLE4               =  2953; // CG_BINDLOCATION_MACRO(SAMPLE4,CG_SAMPLE4,"SAMPLE4",2953,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE4}
  CG_SAMPLE5               =  2954; // CG_BINDLOCATION_MACRO(SAMPLE5,CG_SAMPLE5,"SAMPLE5",2954,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE5}
  CG_SAMPLE6               =  2955; // CG_BINDLOCATION_MACRO(SAMPLE6,CG_SAMPLE6,"SAMPLE6",2955,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE6}
  CG_SAMPLE7               =  2956; // CG_BINDLOCATION_MACRO(SAMPLE7,CG_SAMPLE7,"SAMPLE7",2956,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE7}
  CG_SAMPLE8               =  2957; // CG_BINDLOCATION_MACRO(SAMPLE8,CG_SAMPLE8,"SAMPLE8",2957,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE8}
  CG_SAMPLE9               =  2958; // CG_BINDLOCATION_MACRO(SAMPLE9,CG_SAMPLE9,"SAMPLE9",2958,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE9}
  CG_SAMPLE10              =  2959; // CG_BINDLOCATION_MACRO(SAMPLE10,CG_SAMPLE10,"SAMPLE10",2959,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE10}
  CG_SAMPLE11              =  2960; // CG_BINDLOCATION_MACRO(SAMPLE11,CG_SAMPLE11,"SAMPLE11",2960,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE11}
  CG_SAMPLE12              =  2961; // CG_BINDLOCATION_MACRO(SAMPLE12,CG_SAMPLE12,"SAMPLE12",2961,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE12}
  CG_SAMPLE13              =  2962; // CG_BINDLOCATION_MACRO(SAMPLE13,CG_SAMPLE13,"SAMPLE13",2962,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE13}
  CG_SAMPLE14              =  2963; // CG_BINDLOCATION_MACRO(SAMPLE14,CG_SAMPLE14,"SAMPLE14",2963,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE14}
  CG_SAMPLE15              =  2964; // CG_BINDLOCATION_MACRO(SAMPLE15,CG_SAMPLE15,"SAMPLE15",2964,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_SAMPLE15}
  CG_BLENDWEIGHT0          =  3028; // CG_BINDLOCATION_MACRO(BlendWeight0,CG_BLENDWEIGHT0,"BLENDWEIGHT0",3028,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT0}
  CG_BLENDWEIGHT1          =  3029; // CG_BINDLOCATION_MACRO(BlendWeight1,CG_BLENDWEIGHT1,"BLENDWEIGHT1",3029,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT1}
  CG_BLENDWEIGHT2          =  3030; // CG_BINDLOCATION_MACRO(BlendWeight2,CG_BLENDWEIGHT2,"BLENDWEIGHT2",3030,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT2}
  CG_BLENDWEIGHT3          =  3031; // CG_BINDLOCATION_MACRO(BlendWeight3,CG_BLENDWEIGHT3,"BLENDWEIGHT3",3031,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT3}
  CG_BLENDWEIGHT4          =  3032; // CG_BINDLOCATION_MACRO(BlendWeight4,CG_BLENDWEIGHT4,"BLENDWEIGHT4",3032,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT4}
  CG_BLENDWEIGHT5          =  3033; // CG_BINDLOCATION_MACRO(BlendWeight5,CG_BLENDWEIGHT5,"BLENDWEIGHT5",3033,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT5}
  CG_BLENDWEIGHT6          =  3034; // CG_BINDLOCATION_MACRO(BlendWeight6,CG_BLENDWEIGHT6,"BLENDWEIGHT6",3034,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT6}
  CG_BLENDWEIGHT7          =  3035; // CG_BINDLOCATION_MACRO(BlendWeight7,CG_BLENDWEIGHT7,"BLENDWEIGHT7",3035,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT7}
  CG_BLENDWEIGHT8          =  3036; // CG_BINDLOCATION_MACRO(BlendWeight8,CG_BLENDWEIGHT8,"BLENDWEIGHT8",3036,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT8}
  CG_BLENDWEIGHT9          =  3037; // CG_BINDLOCATION_MACRO(BlendWeight9,CG_BLENDWEIGHT9,"BLENDWEIGHT9",3037,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT9}
  CG_BLENDWEIGHT10         =  3038; // CG_BINDLOCATION_MACRO(BlendWeight10,CG_BLENDWEIGHT10,"BLENDWEIGHT10",3038,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT10}
  CG_BLENDWEIGHT11         =  3039; // CG_BINDLOCATION_MACRO(BlendWeight11,CG_BLENDWEIGHT11,"BLENDWEIGHT11",3039,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT11}
  CG_BLENDWEIGHT12         =  3040; // CG_BINDLOCATION_MACRO(BlendWeight12,CG_BLENDWEIGHT12,"BLENDWEIGHT12",3040,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT12}
  CG_BLENDWEIGHT13         =  3041; // CG_BINDLOCATION_MACRO(BlendWeight13,CG_BLENDWEIGHT13,"BLENDWEIGHT13",3041,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT13}
  CG_BLENDWEIGHT14         =  3042; // CG_BINDLOCATION_MACRO(BlendWeight14,CG_BLENDWEIGHT14,"BLENDWEIGHT14",3042,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT14}
  CG_BLENDWEIGHT15         =  3043; // CG_BINDLOCATION_MACRO(BlendWeight15,CG_BLENDWEIGHT15,"BLENDWEIGHT15",3043,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_BLENDWEIGHT15}
  CG_NORMAL0               =  3092; // CG_BINDLOCATION_MACRO(Normal0,CG_NORMAL0,"NORMAL0",3092,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL0}
  CG_NORMAL1               =  3093; // CG_BINDLOCATION_MACRO(Normal1,CG_NORMAL1,"NORMAL1",3093,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL1}
  CG_NORMAL2               =  3094; // CG_BINDLOCATION_MACRO(Normal2,CG_NORMAL2,"NORMAL2",3094,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL2}
  CG_NORMAL3               =  3095; // CG_BINDLOCATION_MACRO(Normal3,CG_NORMAL3,"NORMAL3",3095,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL3}
  CG_NORMAL4               =  3096; // CG_BINDLOCATION_MACRO(Normal4,CG_NORMAL4,"NORMAL4",3096,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL4}
  CG_NORMAL5               =  3097; // CG_BINDLOCATION_MACRO(Normal5,CG_NORMAL5,"NORMAL5",3097,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL5}
  CG_NORMAL6               =  3098; // CG_BINDLOCATION_MACRO(Normal6,CG_NORMAL6,"NORMAL6",3098,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL6}
  CG_NORMAL7               =  3099; // CG_BINDLOCATION_MACRO(Normal7,CG_NORMAL7,"NORMAL7",3099,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL7}
  CG_NORMAL8               =  3100; // CG_BINDLOCATION_MACRO(Normal8,CG_NORMAL8,"NORMAL8",3100,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL8}
  CG_NORMAL9               =  3101; // CG_BINDLOCATION_MACRO(Normal9,CG_NORMAL9,"NORMAL9",3101,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL9}
  CG_NORMAL10              =  3102; // CG_BINDLOCATION_MACRO(Normal10,CG_NORMAL10,"NORMAL10",3102,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL10}
  CG_NORMAL11              =  3103; // CG_BINDLOCATION_MACRO(Normal11,CG_NORMAL11,"NORMAL11",3103,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL11}
  CG_NORMAL12              =  3104; // CG_BINDLOCATION_MACRO(Normal12,CG_NORMAL12,"NORMAL12",3104,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL12}
  CG_NORMAL13              =  3105; // CG_BINDLOCATION_MACRO(Normal13,CG_NORMAL13,"NORMAL13",3105,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL13}
  CG_NORMAL14              =  3106; // CG_BINDLOCATION_MACRO(Normal14,CG_NORMAL14,"NORMAL14",3106,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL14}
  CG_NORMAL15              =  3107; // CG_BINDLOCATION_MACRO(Normal15,CG_NORMAL15,"NORMAL15",3107,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_NORMAL15}
  CG_FOGCOORD              =  3156; // CG_BINDLOCATION_MACRO(FogCoord,CG_FOGCOORD,"FOGCOORD",3156,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_FOGCOORD}
  CG_TEXCOORD0             =  3220; // CG_BINDLOCATION_MACRO(TexCoord0,CG_TEXCOORD0,"TEXCOORD0",3220,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD0}
  CG_TEXCOORD1             =  3221; // CG_BINDLOCATION_MACRO(TexCoord1,CG_TEXCOORD1,"TEXCOORD1",3221,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD1}
  CG_TEXCOORD2             =  3222; // CG_BINDLOCATION_MACRO(TexCoord2,CG_TEXCOORD2,"TEXCOORD2",3222,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD2}
  CG_TEXCOORD3             =  3223; // CG_BINDLOCATION_MACRO(TexCoord3,CG_TEXCOORD3,"TEXCOORD3",3223,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD3}
  CG_TEXCOORD4             =  3224; // CG_BINDLOCATION_MACRO(TexCoord4,CG_TEXCOORD4,"TEXCOORD4",3224,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD4}
  CG_TEXCOORD5             =  3225; // CG_BINDLOCATION_MACRO(TexCoord5,CG_TEXCOORD5,"TEXCOORD5",3225,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD5}
  CG_TEXCOORD6             =  3226; // CG_BINDLOCATION_MACRO(TexCoord6,CG_TEXCOORD6,"TEXCOORD6",3226,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD6}
  CG_TEXCOORD7             =  3227; // CG_BINDLOCATION_MACRO(TexCoord7,CG_TEXCOORD7,"TEXCOORD7",3227,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD7}
  CG_TEXCOORD8             =  3228; // CG_BINDLOCATION_MACRO(TexCoord8,CG_TEXCOORD8,"TEXCOORD8",3228,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD8}
  CG_TEXCOORD9             =  3229; // CG_BINDLOCATION_MACRO(TexCoord9,CG_TEXCOORD9,"TEXCOORD9",3229,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD9}
  CG_TEXCOORD10            =  3230; // CG_BINDLOCATION_MACRO(TexCoord10,CG_TEXCOORD10,"TEXCOORD10",3230,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD10}
  CG_TEXCOORD11            =  3231; // CG_BINDLOCATION_MACRO(TexCoord11,CG_TEXCOORD11,"TEXCOORD11",3231,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD11}
  CG_TEXCOORD12            =  3232; // CG_BINDLOCATION_MACRO(TexCoord12,CG_TEXCOORD12,"TEXCOORD12",3232,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD12}
  CG_TEXCOORD13            =  3233; // CG_BINDLOCATION_MACRO(TexCoord13,CG_TEXCOORD13,"TEXCOORD13",3233,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD13}
  CG_TEXCOORD14            =  3234; // CG_BINDLOCATION_MACRO(TexCoord14,CG_TEXCOORD14,"TEXCOORD14",3234,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD14}
  CG_TEXCOORD15            =  3235; // CG_BINDLOCATION_MACRO(TexCoord15,CG_TEXCOORD15,"TEXCOORD15",3235,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TEXCOORD15}
  CG_COMBINER_CONST0       =  3284; // CG_BINDLOCATION_MACRO(CombinerConst0,CG_COMBINER_CONST0,"COMBINER_CONST0",3284,0,cgUniformParam)
  {$EXTERNALSYM CG_COMBINER_CONST0}
  CG_COMBINER_CONST1       =  3285; // CG_BINDLOCATION_MACRO(CombinerConst1,CG_COMBINER_CONST1,"COMBINER_CONST1",3285,0,cgUniformParam)
  {$EXTERNALSYM CG_COMBINER_CONST1}
  CG_COMBINER_STAGE_CONST0 =  3286; // CG_BINDLOCATION_MACRO(CombinerStageConst0,CG_COMBINER_STAGE_CONST0,"COMBINER_STAGE_CONST0",3286,1,cgUniformParam)
  {$EXTERNALSYM CG_COMBINER_STAGE_CONST0}
  CG_COMBINER_STAGE_CONST1 =  3287; // CG_BINDLOCATION_MACRO(CombinerStageConst1,CG_COMBINER_STAGE_CONST1,"COMBINER_STAGE_CONST1",3287,1,cgUniformParam)
  {$EXTERNALSYM CG_COMBINER_STAGE_CONST1}
  CG_OFFSET_TEXTURE_MATRIX =  3288; // CG_BINDLOCATION_MACRO(OffsetTextureMatrix,CG_OFFSET_TEXTURE_MATRIX,"OFFSET_TEXTURE_MATRIX",3288,0,cgUniformParam)
  {$EXTERNALSYM CG_OFFSET_TEXTURE_MATRIX}
  CG_OFFSET_TEXTURE_SCALE  =  3289; // CG_BINDLOCATION_MACRO(OffsetTextureScale,CG_OFFSET_TEXTURE_SCALE,"OFFSET_TEXTURE_SCALE",3289,0,cgUniformParam)
  {$EXTERNALSYM CG_OFFSET_TEXTURE_SCALE}
  CG_OFFSET_TEXTURE_BIAS   =  3290; // CG_BINDLOCATION_MACRO(OffsetTextureBias,CG_OFFSET_TEXTURE_BIAS,"OFFSET_TEXTURE_BIAS",3290,0,cgUniformParam)
  {$EXTERNALSYM CG_OFFSET_TEXTURE_BIAS}
  CG_CONST_EYE             =  3291; // CG_BINDLOCATION_MACRO(ConstEye,CG_CONST_EYE,"CONST_EYE",3291,0,cgUniformParam)
  {$EXTERNALSYM CG_CONST_EYE}
  CG_TESSFACTOR            =  3255; // CG_BINDLOCATION_MACRO(TessFactor,CG_TESSFACTOR,"TESSFACTOR",3255,0,cgConnectorMemberParam)
  {$EXTERNALSYM CG_TESSFACTOR}

  CG_GLSL_UNIFORM          =  3300; // CG_BINDLOCATION_MACRO(GLSLUniform,CG_GLSL_UNIFORM,"glsl_uniform",3300,1,CGI_UNIFORM_PARAM)
  {$EXTERNALSYM CG_GLSL_UNIFORM}
  CG_GLSL_ATTRIB           =  3301; // CG_BINDLOCATION_MACRO(GLSLAttrib,CG_GLSL_ATTRIB,"glsl_attrib",3301,1,CG_VARYING_PARAM)
  {$EXTERNALSYM CG_GLSL_ATTRIB}

  // 256 + 1 of these
  CG_ENV                   =  3302;
  {$EXTERNALSYM CG_ENV}
  CG_ENV0                  =  3303;
  {$EXTERNALSYM CG_ENV0}
  CG_ENV1                  =  3304;
  {$EXTERNALSYM CG_ENV1}
  CG_ENV2                  =  3305;
  {$EXTERNALSYM CG_ENV2}
  CG_ENV3                  =  3306;
  {$EXTERNALSYM CG_ENV3}
  CG_ENV4                  =  3307;
  {$EXTERNALSYM CG_ENV4}
  CG_ENV5                  =  3308;
  {$EXTERNALSYM CG_ENV5}
  CG_ENV6                  =  3309;
  {$EXTERNALSYM CG_ENV6}
  CG_ENV7                  =  3310;
  {$EXTERNALSYM CG_ENV7}
  CG_ENV8                  =  3311;
  {$EXTERNALSYM CG_ENV8}
  CG_ENV9                  =  3312;
  {$EXTERNALSYM CG_ENV9}
  CG_ENV10                 =  3313;
  {$EXTERNALSYM CG_ENV10}
  CG_ENV11                 =  3314;
  {$EXTERNALSYM CG_ENV11}
  CG_ENV12                 =  3315;
  {$EXTERNALSYM CG_ENV12}
  CG_ENV13                 =  3316;
  {$EXTERNALSYM CG_ENV13}
  CG_ENV14                 =  3317;
  {$EXTERNALSYM CG_ENV14}
  CG_ENV15                 =  3318;
  {$EXTERNALSYM CG_ENV15}
  CG_ENV16                 =  3319;
  {$EXTERNALSYM CG_ENV16}
  CG_ENV17                 =  3320;
  {$EXTERNALSYM CG_ENV17}
  CG_ENV18                 =  3321;
  {$EXTERNALSYM CG_ENV18}
  CG_ENV19                 =  3322;
  {$EXTERNALSYM CG_ENV19}
  CG_ENV20                 =  3323;
  {$EXTERNALSYM CG_ENV20}
  CG_ENV21                 =  3324;
  {$EXTERNALSYM CG_ENV21}
  CG_ENV22                 =  3325;
  {$EXTERNALSYM CG_ENV22}
  CG_ENV23                 =  3326;
  {$EXTERNALSYM CG_ENV23}
  CG_ENV24                 =  3327;
  {$EXTERNALSYM CG_ENV24}
  CG_ENV25                 =  3328;
  {$EXTERNALSYM CG_ENV25}
  CG_ENV26                 =  3329;
  {$EXTERNALSYM CG_ENV26}
  CG_ENV27                 =  3330;
  {$EXTERNALSYM CG_ENV27}
  CG_ENV28                 =  3331;
  {$EXTERNALSYM CG_ENV28}
  CG_ENV29                 =  3332;
  {$EXTERNALSYM CG_ENV29}
  CG_ENV30                 =  3333;
  {$EXTERNALSYM CG_ENV30}
  CG_ENV31                 =  3334;
  {$EXTERNALSYM CG_ENV31}
  CG_ENV32                 =  3335;
  {$EXTERNALSYM CG_ENV32}
  CG_ENV33                 =  3336;
  {$EXTERNALSYM CG_ENV33}
  CG_ENV34                 =  3337;
  {$EXTERNALSYM CG_ENV34}
  CG_ENV35                 =  3338;
  {$EXTERNALSYM CG_ENV35}
  CG_ENV36                 =  3339;
  {$EXTERNALSYM CG_ENV36}
  CG_ENV37                 =  3340;
  {$EXTERNALSYM CG_ENV37}
  CG_ENV38                 =  3341;
  {$EXTERNALSYM CG_ENV38}
  CG_ENV39                 =  3342;
  {$EXTERNALSYM CG_ENV39}
  CG_ENV40                 =  3343;
  {$EXTERNALSYM CG_ENV40}
  CG_ENV41                 =  3344;
  {$EXTERNALSYM CG_ENV41}
  CG_ENV42                 =  3345;
  {$EXTERNALSYM CG_ENV42}
  CG_ENV43                 =  3346;
  {$EXTERNALSYM CG_ENV43}
  CG_ENV44                 =  3347;
  {$EXTERNALSYM CG_ENV44}
  CG_ENV45                 =  3348;
  {$EXTERNALSYM CG_ENV45}
  CG_ENV46                 =  3349;
  {$EXTERNALSYM CG_ENV46}
  CG_ENV47                 =  3350;
  {$EXTERNALSYM CG_ENV47}
  CG_ENV48                 =  3351;
  {$EXTERNALSYM CG_ENV48}
  CG_ENV49                 =  3352;
  {$EXTERNALSYM CG_ENV49}
  CG_ENV50                 =  3353;
  {$EXTERNALSYM CG_ENV50}
  CG_ENV51                 =  3354;
  {$EXTERNALSYM CG_ENV51}
  CG_ENV52                 =  3355;
  {$EXTERNALSYM CG_ENV52}
  CG_ENV53                 =  3356;
  {$EXTERNALSYM CG_ENV53}
  CG_ENV54                 =  3357;
  {$EXTERNALSYM CG_ENV54}
  CG_ENV55                 =  3358;
  {$EXTERNALSYM CG_ENV55}
  CG_ENV56                 =  3359;
  {$EXTERNALSYM CG_ENV56}
  CG_ENV57                 =  3360;
  {$EXTERNALSYM CG_ENV57}
  CG_ENV58                 =  3361;
  {$EXTERNALSYM CG_ENV58}
  CG_ENV59                 =  3362;
  {$EXTERNALSYM CG_ENV59}
  CG_ENV60                 =  3363;
  {$EXTERNALSYM CG_ENV60}
  CG_ENV61                 =  3364;
  {$EXTERNALSYM CG_ENV61}
  CG_ENV62                 =  3365;
  {$EXTERNALSYM CG_ENV62}
  CG_ENV63                 =  3366;
  {$EXTERNALSYM CG_ENV63}
  CG_ENV64                 =  3367;
  {$EXTERNALSYM CG_ENV64}
  CG_ENV65                 =  3368;
  {$EXTERNALSYM CG_ENV65}
  CG_ENV66                 =  3369;
  {$EXTERNALSYM CG_ENV66}
  CG_ENV67                 =  3370;
  {$EXTERNALSYM CG_ENV67}
  CG_ENV68                 =  3371;
  {$EXTERNALSYM CG_ENV68}
  CG_ENV69                 =  3372;
  {$EXTERNALSYM CG_ENV69}
  CG_ENV70                 =  3373;
  {$EXTERNALSYM CG_ENV70}
  CG_ENV71                 =  3374;
  {$EXTERNALSYM CG_ENV71}
  CG_ENV72                 =  3375;
  {$EXTERNALSYM CG_ENV72}
  CG_ENV73                 =  3376;
  {$EXTERNALSYM CG_ENV73}
  CG_ENV74                 =  3377;
  {$EXTERNALSYM CG_ENV74}
  CG_ENV75                 =  3378;
  {$EXTERNALSYM CG_ENV75}
  CG_ENV76                 =  3379;
  {$EXTERNALSYM CG_ENV76}
  CG_ENV77                 =  3380;
  {$EXTERNALSYM CG_ENV77}
  CG_ENV78                 =  3381;
  {$EXTERNALSYM CG_ENV78}
  CG_ENV79                 =  3382;
  {$EXTERNALSYM CG_ENV79}
  CG_ENV80                 =  3383;
  {$EXTERNALSYM CG_ENV80}
  CG_ENV81                 =  3384;
  {$EXTERNALSYM CG_ENV81}
  CG_ENV82                 =  3385;
  {$EXTERNALSYM CG_ENV82}
  CG_ENV83                 =  3386;
  {$EXTERNALSYM CG_ENV83}
  CG_ENV84                 =  3387;
  {$EXTERNALSYM CG_ENV84}
  CG_ENV85                 =  3388;
  {$EXTERNALSYM CG_ENV85}
  CG_ENV86                 =  3389;
  {$EXTERNALSYM CG_ENV86}
  CG_ENV87                 =  3390;
  {$EXTERNALSYM CG_ENV87}
  CG_ENV88                 =  3391;
  {$EXTERNALSYM CG_ENV88}
  CG_ENV89                 =  3392;
  {$EXTERNALSYM CG_ENV89}
  CG_ENV90                 =  3393;
  {$EXTERNALSYM CG_ENV90}
  CG_ENV91                 =  3394;
  {$EXTERNALSYM CG_ENV91}
  CG_ENV92                 =  3395;
  {$EXTERNALSYM CG_ENV92}
  CG_ENV93                 =  3396;
  {$EXTERNALSYM CG_ENV93}
  CG_ENV94                 =  3397;
  {$EXTERNALSYM CG_ENV94}
  CG_ENV95                 =  3398;
  {$EXTERNALSYM CG_ENV95}
  CG_ENV96                 =  3399;
  {$EXTERNALSYM CG_ENV96}
  CG_ENV97                 =  3400;
  {$EXTERNALSYM CG_ENV97}
  CG_ENV98                 =  3401;
  {$EXTERNALSYM CG_ENV98}
  CG_ENV99                 =  3402;
  {$EXTERNALSYM CG_ENV99}
  CG_ENV100                =  3403;
  {$EXTERNALSYM CG_ENV100}
  CG_ENV101                =  3404;
  {$EXTERNALSYM CG_ENV101}
  CG_ENV102                =  3405;
  {$EXTERNALSYM CG_ENV102}
  CG_ENV103                =  3406;
  {$EXTERNALSYM CG_ENV103}
  CG_ENV104                =  3407;
  {$EXTERNALSYM CG_ENV104}
  CG_ENV105                =  3408;
  {$EXTERNALSYM CG_ENV105}
  CG_ENV106                =  3409;
  {$EXTERNALSYM CG_ENV106}
  CG_ENV107                =  3410;
  {$EXTERNALSYM CG_ENV107}
  CG_ENV108                =  3411;
  {$EXTERNALSYM CG_ENV108}
  CG_ENV109                =  3412;
  {$EXTERNALSYM CG_ENV109}
  CG_ENV110                =  3413;
  {$EXTERNALSYM CG_ENV110}
  CG_ENV111                =  3414;
  {$EXTERNALSYM CG_ENV111}
  CG_ENV112                =  3415;
  {$EXTERNALSYM CG_ENV112}
  CG_ENV113                =  3416;
  {$EXTERNALSYM CG_ENV113}
  CG_ENV114                =  3417;
  {$EXTERNALSYM CG_ENV114}
  CG_ENV115                =  3418;
  {$EXTERNALSYM CG_ENV115}
  CG_ENV116                =  3419;
  {$EXTERNALSYM CG_ENV116}
  CG_ENV117                =  3420;
  {$EXTERNALSYM CG_ENV117}
  CG_ENV118                =  3421;
  {$EXTERNALSYM CG_ENV118}
  CG_ENV119                =  3422;
  {$EXTERNALSYM CG_ENV119}
  CG_ENV120                =  3423;
  {$EXTERNALSYM CG_ENV120}
  CG_ENV121                =  3424;
  {$EXTERNALSYM CG_ENV121}
  CG_ENV122                =  3425;
  {$EXTERNALSYM CG_ENV122}
  CG_ENV123                =  3426;
  {$EXTERNALSYM CG_ENV123}
  CG_ENV124                =  3427;
  {$EXTERNALSYM CG_ENV124}
  CG_ENV125                =  3428;
  {$EXTERNALSYM CG_ENV125}
  CG_ENV126                =  3429;
  {$EXTERNALSYM CG_ENV126}
  CG_ENV127                =  3430;
  {$EXTERNALSYM CG_ENV127}
  CG_ENV128                =  3431;
  {$EXTERNALSYM CG_ENV128}
  CG_ENV129                =  3432;
  {$EXTERNALSYM CG_ENV129}
  CG_ENV130                =  3433;
  {$EXTERNALSYM CG_ENV130}
  CG_ENV131                =  3434;
  {$EXTERNALSYM CG_ENV131}
  CG_ENV132                =  3435;
  {$EXTERNALSYM CG_ENV132}
  CG_ENV133                =  3436;
  {$EXTERNALSYM CG_ENV133}
  CG_ENV134                =  3437;
  {$EXTERNALSYM CG_ENV134}
  CG_ENV135                =  3438;
  {$EXTERNALSYM CG_ENV135}
  CG_ENV136                =  3439;
  {$EXTERNALSYM CG_ENV136}
  CG_ENV137                =  3440;
  {$EXTERNALSYM CG_ENV137}
  CG_ENV138                =  3441;
  {$EXTERNALSYM CG_ENV138}
  CG_ENV139                =  3442;
  {$EXTERNALSYM CG_ENV139}
  CG_ENV140                =  3443;
  {$EXTERNALSYM CG_ENV140}
  CG_ENV141                =  3444;
  {$EXTERNALSYM CG_ENV141}
  CG_ENV142                =  3445;
  {$EXTERNALSYM CG_ENV142}
  CG_ENV143                =  3446;
  {$EXTERNALSYM CG_ENV143}
  CG_ENV144                =  3447;
  {$EXTERNALSYM CG_ENV144}
  CG_ENV145                =  3448;
  {$EXTERNALSYM CG_ENV145}
  CG_ENV146                =  3449;
  {$EXTERNALSYM CG_ENV146}
  CG_ENV147                =  3450;
  {$EXTERNALSYM CG_ENV147}
  CG_ENV148                =  3451;
  {$EXTERNALSYM CG_ENV148}
  CG_ENV149                =  3452;
  {$EXTERNALSYM CG_ENV149}
  CG_ENV150                =  3453;
  {$EXTERNALSYM CG_ENV150}
  CG_ENV151                =  3454;
  {$EXTERNALSYM CG_ENV151}
  CG_ENV152                =  3455;
  {$EXTERNALSYM CG_ENV152}
  CG_ENV153                =  3456;
  {$EXTERNALSYM CG_ENV153}
  CG_ENV154                =  3457;
  {$EXTERNALSYM CG_ENV154}
  CG_ENV155                =  3458;
  {$EXTERNALSYM CG_ENV155}
  CG_ENV156                =  3459;
  {$EXTERNALSYM CG_ENV156}
  CG_ENV157                =  3460;
  {$EXTERNALSYM CG_ENV157}
  CG_ENV158                =  3461;
  {$EXTERNALSYM CG_ENV158}
  CG_ENV159                =  3462;
  {$EXTERNALSYM CG_ENV159}
  CG_ENV160                =  3463;
  {$EXTERNALSYM CG_ENV160}
  CG_ENV161                =  3464;
  {$EXTERNALSYM CG_ENV161}
  CG_ENV162                =  3465;
  {$EXTERNALSYM CG_ENV162}
  CG_ENV163                =  3466;
  {$EXTERNALSYM CG_ENV163}
  CG_ENV164                =  3467;
  {$EXTERNALSYM CG_ENV164}
  CG_ENV165                =  3468;
  {$EXTERNALSYM CG_ENV165}
  CG_ENV166                =  3469;
  {$EXTERNALSYM CG_ENV166}
  CG_ENV167                =  3470;
  {$EXTERNALSYM CG_ENV167}
  CG_ENV168                =  3471;
  {$EXTERNALSYM CG_ENV168}
  CG_ENV169                =  3472;
  {$EXTERNALSYM CG_ENV169}
  CG_ENV170                =  3473;
  {$EXTERNALSYM CG_ENV170}
  CG_ENV171                =  3474;
  {$EXTERNALSYM CG_ENV171}
  CG_ENV172                =  3475;
  {$EXTERNALSYM CG_ENV172}
  CG_ENV173                =  3476;
  {$EXTERNALSYM CG_ENV173}
  CG_ENV174                =  3477;
  {$EXTERNALSYM CG_ENV174}
  CG_ENV175                =  3478;
  {$EXTERNALSYM CG_ENV175}
  CG_ENV176                =  3479;
  {$EXTERNALSYM CG_ENV176}
  CG_ENV177                =  3480;
  {$EXTERNALSYM CG_ENV177}
  CG_ENV178                =  3481;
  {$EXTERNALSYM CG_ENV178}
  CG_ENV179                =  3482;
  {$EXTERNALSYM CG_ENV179}
  CG_ENV180                =  3483;
  {$EXTERNALSYM CG_ENV180}
  CG_ENV181                =  3484;
  {$EXTERNALSYM CG_ENV181}
  CG_ENV182                =  3485;
  {$EXTERNALSYM CG_ENV182}
  CG_ENV183                =  3486;
  {$EXTERNALSYM CG_ENV183}
  CG_ENV184                =  3487;
  {$EXTERNALSYM CG_ENV184}
  CG_ENV185                =  3488;
  {$EXTERNALSYM CG_ENV185}
  CG_ENV186                =  3489;
  {$EXTERNALSYM CG_ENV186}
  CG_ENV187                =  3490;
  {$EXTERNALSYM CG_ENV187}
  CG_ENV188                =  3491;
  {$EXTERNALSYM CG_ENV188}
  CG_ENV189                =  3492;
  {$EXTERNALSYM CG_ENV189}
  CG_ENV190                =  3493;
  {$EXTERNALSYM CG_ENV190}
  CG_ENV191                =  3494;
  {$EXTERNALSYM CG_ENV191}
  CG_ENV192                =  3495;
  {$EXTERNALSYM CG_ENV192}
  CG_ENV193                =  3496;
  {$EXTERNALSYM CG_ENV193}
  CG_ENV194                =  3497;
  {$EXTERNALSYM CG_ENV194}
  CG_ENV195                =  3498;
  {$EXTERNALSYM CG_ENV195}
  CG_ENV196                =  3499;
  {$EXTERNALSYM CG_ENV196}
  CG_ENV197                =  3500;
  {$EXTERNALSYM CG_ENV197}
  CG_ENV198                =  3501;
  {$EXTERNALSYM CG_ENV198}
  CG_ENV199                =  3502;
  {$EXTERNALSYM CG_ENV199}
  CG_ENV200                =  3503;
  {$EXTERNALSYM CG_ENV200}
  CG_ENV201                =  3504;
  {$EXTERNALSYM CG_ENV201}
  CG_ENV202                =  3505;
  {$EXTERNALSYM CG_ENV202}
  CG_ENV203                =  3506;
  {$EXTERNALSYM CG_ENV203}
  CG_ENV204                =  3507;
  {$EXTERNALSYM CG_ENV204}
  CG_ENV205                =  3508;
  {$EXTERNALSYM CG_ENV205}
  CG_ENV206                =  3509;
  {$EXTERNALSYM CG_ENV206}
  CG_ENV207                =  3510;
  {$EXTERNALSYM CG_ENV207}
  CG_ENV208                =  3511;
  {$EXTERNALSYM CG_ENV208}
  CG_ENV209                =  3512;
  {$EXTERNALSYM CG_ENV209}
  CG_ENV210                =  3513;
  {$EXTERNALSYM CG_ENV210}
  CG_ENV211                =  3514;
  {$EXTERNALSYM CG_ENV211}
  CG_ENV212                =  3515;
  {$EXTERNALSYM CG_ENV212}
  CG_ENV213                =  3516;
  {$EXTERNALSYM CG_ENV213}
  CG_ENV214                =  3517;
  {$EXTERNALSYM CG_ENV214}
  CG_ENV215                =  3518;
  {$EXTERNALSYM CG_ENV215}
  CG_ENV216                =  3519;
  {$EXTERNALSYM CG_ENV216}
  CG_ENV217                =  3520;
  {$EXTERNALSYM CG_ENV217}
  CG_ENV218                =  3521;
  {$EXTERNALSYM CG_ENV218}
  CG_ENV219                =  3522;
  {$EXTERNALSYM CG_ENV219}
  CG_ENV220                =  3523;
  {$EXTERNALSYM CG_ENV220}
  CG_ENV221                =  3524;
  {$EXTERNALSYM CG_ENV221}
  CG_ENV222                =  3525;
  {$EXTERNALSYM CG_ENV222}
  CG_ENV223                =  3526;
  {$EXTERNALSYM CG_ENV223}
  CG_ENV224                =  3527;
  {$EXTERNALSYM CG_ENV224}
  CG_ENV225                =  3528;
  {$EXTERNALSYM CG_ENV225}
  CG_ENV226                =  3529;
  {$EXTERNALSYM CG_ENV226}
  CG_ENV227                =  3530;
  {$EXTERNALSYM CG_ENV227}
  CG_ENV228                =  3531;
  {$EXTERNALSYM CG_ENV228}
  CG_ENV229                =  3532;
  {$EXTERNALSYM CG_ENV229}
  CG_ENV230                =  3533;
  {$EXTERNALSYM CG_ENV230}
  CG_ENV231                =  3534;
  {$EXTERNALSYM CG_ENV231}
  CG_ENV232                =  3535;
  {$EXTERNALSYM CG_ENV232}
  CG_ENV233                =  3536;
  {$EXTERNALSYM CG_ENV233}
  CG_ENV234                =  3537;
  {$EXTERNALSYM CG_ENV234}
  CG_ENV235                =  3538;
  {$EXTERNALSYM CG_ENV235}
  CG_ENV236                =  3539;
  {$EXTERNALSYM CG_ENV236}
  CG_ENV237                =  3540;
  {$EXTERNALSYM CG_ENV237}
  CG_ENV238                =  3541;
  {$EXTERNALSYM CG_ENV238}
  CG_ENV239                =  3542;
  {$EXTERNALSYM CG_ENV239}
  CG_ENV240                =  3543;
  {$EXTERNALSYM CG_ENV240}
  CG_ENV241                =  3544;
  {$EXTERNALSYM CG_ENV241}
  CG_ENV242                =  3545;
  {$EXTERNALSYM CG_ENV242}
  CG_ENV243                =  3546;
  {$EXTERNALSYM CG_ENV243}
  CG_ENV244                =  3547;
  {$EXTERNALSYM CG_ENV244}
  CG_ENV245                =  3548;
  {$EXTERNALSYM CG_ENV245}
  CG_ENV246                =  3549;
  {$EXTERNALSYM CG_ENV246}
  CG_ENV247                =  3550;
  {$EXTERNALSYM CG_ENV247}
  CG_ENV248                =  3551;
  {$EXTERNALSYM CG_ENV248}
  CG_ENV249                =  3552;
  {$EXTERNALSYM CG_ENV249}
  CG_ENV250                =  3553;
  {$EXTERNALSYM CG_ENV250}
  CG_ENV251                =  3554;
  {$EXTERNALSYM CG_ENV251}
  CG_ENV252                =  3555;
  {$EXTERNALSYM CG_ENV252}
  CG_ENV253                =  3556;
  {$EXTERNALSYM CG_ENV253}
  CG_ENV254                =  3557;
  {$EXTERNALSYM CG_ENV254}
  CG_ENV255                =  3558;
  {$EXTERNALSYM CG_ENV255}

  CG_UNDEFINED             =  3256;
  {$EXTERNALSYM CG_UNDEFINED}

type
  TCGresource = DWORD;
{$ENDIF}
  CGresource = TCGresource;
  {$NODEFINE CGresource}
  {$NODEFINE TCGresource}
  {$HPPEMIT 'typedef CGresource TCGresource;'}


 (*
  * The following macro invocations define the supported CG profiles.
  *
  * The macros have the form :
  *
  *   CG_PROFILE_MACRO(name, compiler_id, compiler_opt)
  *
  *     name         : The name of the profile.  Used consistently with the API.
  *     compiler_id  : The identifier string for the profile used by the compiler.
  *     compiler_id_caps : compiler_id in caps.
  *     compiler_opt : The command-line switch used to force compilation into
  *                    the profile.
  *     int_id           : Integer enumerant associated with this bind location.
  *     vertex_profile   : Non-zero if this is a vertex profile, otherwise it
  *                        is considered to be a fragment profile.
  *
  *
  *)
{$IFDEF COMPILER6_UP}
  TCGprofile = (
  {$IFDEF BCB}
    CG_Profile_Dummy
  {$ELSE}
    CG_PROFILE_START = 6144,
    CG_PROFILE_UNKNOWN,

    //# define CG_PROFILE_MACRO(name, compiler_id, compiler_id_caps, compiler_opt,int_id,vertex_profile) \
    //   CG_PROFILE_##compiler_id_caps = int_id,

    CG_PROFILE_VP20   = 6146, // CG_PROFILE_MACRO(Vertex,vp20,VP20,"vp20",6146,1)
    CG_PROFILE_FP20   = 6147, // CG_PROFILE_MACRO(Fragment20,fp20,FP20,"fp20",6147,0)
    CG_PROFILE_VP30   = 6148, // CG_PROFILE_MACRO(Vertex30,vp30,VP30,"vp30",6148,1)
    CG_PROFILE_FP30   = 6149, // CG_PROFILE_MACRO(Fragment,fp30,FP30,"fp30",6149,0)
    CG_PROFILE_ARBVP1 = 6150, // CG_PROFILE_MACRO(ARBVertex,arbvp1,ARBVP1,"arbvp1",6150,1)
    CG_PROFILE_FP40   = 6151, // CG_PROFILE_MACRO(Fragment40,fp40,FP40,"fp40",6151,0)
    CG_PROFILE_ARBFP1 = 7000, // CG_PROFILE_MACRO(ARBFragment,arbfp1,ARBFP1,"arbfp1",7000,0)
    CG_PROFILE_VP40   = 7001, // CG_PROFILE_MACRO(Vertex40,vp40,VP40,"vp40",7001,1)
    CG_PROFILE_GLSLV  = 7007, // CG_PROFILE_MACRO(GLSLVertex,glslv,GLSLV,"glslv",7007,1)
    CG_PROFILE_GLSLF  = 7008, // CG_PROFILE_MACRO(GLSLFragment,glslf,GLSLF,"glslf",7008,0)
    CG_PROFILE_GLSLC  = 7009, // CG_PROFILE_MACRO(GLSLCombined, glslc, GLSLC, "glslc", 7009, 0)

    CG_PROFILE_VS_1_1 = 6153, // CG_PROFILE_MACRO(DX9Vertex11,vs_1_1,VS_1_1,"vs_1_1",6153,1)
    CG_PROFILE_VS_2_0 = 6154, // CG_PROFILE_MACRO(DX9Vertex20,vs_2_0,VS_2_0,"vs_2_0",6154,1)
    CG_PROFILE_VS_2_X = 6155, // CG_PROFILE_MACRO(DX9Vertex2x,vs_2_x,VS_2_X,"vs_2_x",6155,1)
    CG_PROFILE_VS_2_SW =6156, // CG_PROFILE_MACRO(DX9Vertex2sw,vs_2_sw,VS_2_SW,"vs_2_sw",6156,1)
    CG_PROFILE_VS_3_0 = 6157, // CG_PROFILE_MACRO(DX9Vertex30,vs_3_0,VS_3_0,"vs_3_0",6157,1)

    CG_PROFILE_PS_1_1 = 6159, // CG_PROFILE_MACRO(DX9Pixel11,ps_1_1,PS_1_1,"ps_1_1",6159,0)
    CG_PROFILE_PS_1_2 = 6160, // CG_PROFILE_MACRO(DX9Pixel12,ps_1_2,PS_1_2,"ps_1_2",6160,0)
    CG_PROFILE_PS_1_3 = 6161, // CG_PROFILE_MACRO(DX9Pixel13,ps_1_3,PS_1_3,"ps_1_3",6161,0)
    CG_PROFILE_PS_2_0 = 6162, // CG_PROFILE_MACRO(DX9Pixel20,ps_2_0,PS_2_0,"ps_2_0",6162,0)
    CG_PROFILE_PS_2_X = 6163, // CG_PROFILE_MACRO(DX9Pixel2x,ps_2_x,PS_2_X,"ps_2_x",6163,0)
    CG_PROFILE_PS_2_SW =6164, // CG_PROFILE_MACRO(DX9Pixel2sw,ps_2_sw,PS_2_SW,"ps_2_sw",6164,0)
    CG_PROFILE_PS_3_0 = 6165, // CG_PROFILE_MACRO(DX9Pixel30,ps_3_0,PS_3_0,"ps_3_0",6165,0)

    CG_PROFILE_GENERIC = 7002, // CG_PROFILE_MACRO(Generic,  generic, GENERIC, "generic", 7002,0)

    CG_PROFILE_MAX = 7100
  {$ENDIF}
  );
{$ELSE}  
const
  CG_PROFILE_START = 6144;
  {$EXTERNALSYM CG_PROFILE_START}
  CG_PROFILE_UNKNOWN  = 6145;
  {$EXTERNALSYM CG_PROFILE_UNKNOWN}

  //# define CG_PROFILE_MACRO(name, compiler_id, compiler_id_caps, compiler_opt,int_id,vertex_profile) \
  //   CG_PROFILE_##compiler_id_caps = int_id,

  CG_PROFILE_VP20   = 6146; // CG_PROFILE_MACRO(Vertex,vp20,VP20,"vp20",6146,1)
  {$EXTERNALSYM CG_PROFILE_VP20}
  CG_PROFILE_FP20   = 6147; // CG_PROFILE_MACRO(Fragment20,fp20,FP20,"fp20",6147,0)
  {$EXTERNALSYM CG_PROFILE_FP20}
  CG_PROFILE_VP30   = 6148; // CG_PROFILE_MACRO(Vertex30,vp30,VP30,"vp30",6148,1)
  {$EXTERNALSYM CG_PROFILE_VP30}
  CG_PROFILE_FP30   = 6149; // CG_PROFILE_MACRO(Fragment,fp30,FP30,"fp30",6149,0)
  {$EXTERNALSYM CG_PROFILE_FP30}
  CG_PROFILE_ARBVP1 = 6150; // CG_PROFILE_MACRO(ARBVertex,arbvp1,ARBVP1,"arbvp1",6150,1)
  {$EXTERNALSYM CG_PROFILE_ARBVP1}
  CG_PROFILE_ARBFP1 = 7000; // CG_PROFILE_MACRO(ARBFragment,arbfp1,ARBFP1,"arbfp1",7000,0)
  {$EXTERNALSYM CG_PROFILE_ARBFP1}
  CG_PROFILE_VP40   = 7001; // CG_PROFILE_MACRO(Vertex40,vp40,VP40,"vp40",7001,1)
  {$EXTERNALSYM CG_PROFILE_VP40}
  CG_PROFILE_GLSLV  = 7007; // CG_PROFILE_MACRO(GLSLVertex,glslv,GLSLV,"glslv",7007,1)
  {$EXTERNALSYM CG_PROFILE_GLSLV}
  CG_PROFILE_GLSLF  = 7008; // CG_PROFILE_MACRO(GLSLFragment,glslf,GLSLF,"glslf",7008,0)
  {$EXTERNALSYM CG_PROFILE_GLSLF}
  CG_PROFILE_GLSLC  = 7009; // CG_PROFILE_MACRO(GLSLCombined, glslc, GLSLC, "glslc", 7009, 0)
  {$EXTERNALSYM CG_PROFILE_GLSLC}

  CG_PROFILE_VS_1_1 = 6153; // CG_PROFILE_MACRO(DX9Vertex11,vs_1_1,VS_1_1,"vs_1_1",6153,1)
  {$EXTERNALSYM CG_PROFILE_VS_1_1}
  CG_PROFILE_VS_2_0 = 6154; // CG_PROFILE_MACRO(DX9Vertex20,vs_2_0,VS_2_0,"vs_2_0",6154,1)
  {$EXTERNALSYM CG_PROFILE_VS_2_0}
  CG_PROFILE_VS_2_X = 6155; // CG_PROFILE_MACRO(DX9Vertex2x,vs_2_x,VS_2_X,"vs_2_x",6155,1)
  {$EXTERNALSYM CG_PROFILE_VS_2_X}
  CG_PROFILE_VS_2_SW =6156; // CG_PROFILE_MACRO(DX9Vertex2sw,vs_2_sw,VS_2_SW,"vs_2_sw",6156,1)
  {$EXTERNALSYM CG_PROFILE_VS_2_SW}
  CG_PROFILE_VS_3_0 = 6157; // CG_PROFILE_MACRO(DX9Vertex30,vs_3_0,VS_3_0,"vs_3_0",6157,1)
  {$EXTERNALSYM CG_PROFILE_VS_3_0}

  CG_PROFILE_PS_1_1 = 6159; // CG_PROFILE_MACRO(DX9Pixel11,ps_1_1,PS_1_1,"ps_1_1",6159,0)
  {$EXTERNALSYM CG_PROFILE_PS_1_1}
  CG_PROFILE_PS_1_2 = 6160; // CG_PROFILE_MACRO(DX9Pixel12,ps_1_2,PS_1_2,"ps_1_2",6160,0)
  {$EXTERNALSYM CG_PROFILE_PS_1_2}
  CG_PROFILE_PS_1_3 = 6161; // CG_PROFILE_MACRO(DX9Pixel13,ps_1_3,PS_1_3,"ps_1_3",6161,0)
  {$EXTERNALSYM CG_PROFILE_PS_1_3}
  CG_PROFILE_PS_2_0 = 6162; // CG_PROFILE_MACRO(DX9Pixel20,ps_2_0,PS_2_0,"ps_2_0",6162,0)
  {$EXTERNALSYM CG_PROFILE_PS_2_0}
  CG_PROFILE_PS_2_X = 6163; // CG_PROFILE_MACRO(DX9Pixel2x,ps_2_x,PS_2_X,"ps_2_x",6163,0)
  {$EXTERNALSYM CG_PROFILE_PS_2_X}
  CG_PROFILE_PS_2_SW =6164; // CG_PROFILE_MACRO(DX9Pixel2sw,ps_2_sw,PS_2_SW,"ps_2_sw",6164,0)
  {$EXTERNALSYM CG_PROFILE_PS_2_SW}
  CG_PROFILE_PS_3_0 = 6165; // CG_PROFILE_MACRO(DX9Pixel30,ps_3_0,PS_3_0,"ps_3_0",6165,0)
  {$EXTERNALSYM CG_PROFILE_PS_3_0}

  CG_PROFILE_GENERIC = 7002; // CG_PROFILE_MACRO(Generic,  generic, GENERIC, "generic", 7002,0)
  {$EXTERNALSYM CG_PROFILE_GENERIC}
  
  CG_PROFILE_MAX    = 7100;
  {$EXTERNALSYM CG_PROFILE_MAX}

type
  TCGprofile = DWORD;
{$ENDIF}
  CGprofile = TCGprofile;
  {$NODEFINE CGprofile}
  {$NODEFINE TCGprofile}
  {$HPPEMIT 'typedef CGprofile TCGprofile;'}

  
 (*
  * The following macro invocations define error codes returned by various cg
  * API functions.
  *
  * The macros have the form :
  *
  *   CG_ERROR_MACRO(code, enum_name, message)
  *
  *     code      : The integer error code associated with the error.
  *     enum_name : The name of enumerant of the error code in the API.
  *     message   : A description string associated with the error.
  *
  *)
  PCGerror = ^TCGerror;
  TCGerror = DWORD;
  CGerror = TCGerror;
  {$NODEFINE CGerror}
  {$NODEFINE TCGerror}
  {$HPPEMIT 'typedef CGerror TCGerror;'}

const
  //# define CG_ERROR_MACRO(code, enum_name, new_enum_name, message) \
  //   new_enum_name = code,

  CG_NO_ERROR                       = 0;  // "No error has occurred."
  {$EXTERNALSYM CG_NO_ERROR}
  CG_COMPILER_ERROR                 = 1;  // "The compile returned an error."
  {$EXTERNALSYM CG_COMPILER_ERROR}
  CG_INVALID_PARAMETER_ERROR        = 2;  // "The parameter used is invalid."
  {$EXTERNALSYM CG_INVALID_PARAMETER_ERROR}
  CG_INVALID_PROFILE_ERROR          = 3;  // "The profile is not supported."
  {$EXTERNALSYM CG_INVALID_PROFILE_ERROR}
  CG_PROGRAM_LOAD_ERROR             = 4;  // "The program did could not load."
  {$EXTERNALSYM CG_PROGRAM_LOAD_ERROR}
  CG_PROGRAM_BIND_ERROR             = 5;  // "The program could not bind."
  {$EXTERNALSYM CG_PROGRAM_BIND_ERROR}
  CG_PROGRAM_NOT_LOADED_ERROR       = 6;  // "The program must be loaded before this operation may be used."
  {$EXTERNALSYM CG_PROGRAM_NOT_LOADED_ERROR}
  CG_UNSUPPORTED_GL_EXTENSION_ERROR = 7;  // "An unsupported GL extension was required to perform this operation."
  {$EXTERNALSYM CG_UNSUPPORTED_GL_EXTENSION_ERROR}
  CG_INVALID_VALUE_TYPE_ERROR       = 8;  // "An unknown value type was assigned to a parameter."
  {$EXTERNALSYM CG_INVALID_VALUE_TYPE_ERROR}
  CG_NOT_MATRIX_PARAM_ERROR         = 9;  // "The parameter is not of matrix type."
  {$EXTERNALSYM CG_NOT_MATRIX_PARAM_ERROR}
  CG_INVALID_ENUMERANT_ERROR        = 10; // "The enumerant parameter has an invalid value."
  {$EXTERNALSYM CG_INVALID_ENUMERANT_ERROR}
  CG_NOT_4x4_MATRIX_ERROR           = 11; // "The parameter must be a 4x4 matrix type."
  {$EXTERNALSYM CG_NOT_4x4_MATRIX_ERROR}
  CG_FILE_READ_ERROR                = 12; // "The file could not be read."
  {$EXTERNALSYM CG_FILE_READ_ERROR}
  CG_FILE_WRITE_ERROR               = 13; // "The file could not be written."
  {$EXTERNALSYM CG_FILE_WRITE_ERROR}
  CG_NVPARSE_ERROR                  = 14; // "nvparse could not successfully parse the output from the Cg compiler backend."
  {$EXTERNALSYM CG_NVPARSE_ERROR}
  CG_MEMORY_ALLOC_ERROR             = 15; // "Memory allocation failed."
  {$EXTERNALSYM CG_MEMORY_ALLOC_ERROR}
  CG_INVALID_CONTEXT_HANDLE_ERROR   = 16; // "Invalid context handle."
  {$EXTERNALSYM CG_INVALID_CONTEXT_HANDLE_ERROR}
  CG_INVALID_PROGRAM_HANDLE_ERROR   = 17; // "Invalid program handle."
  {$EXTERNALSYM CG_INVALID_PROGRAM_HANDLE_ERROR}
  CG_INVALID_PARAM_HANDLE_ERROR     = 18; // "Invalid parameter handle."
  {$EXTERNALSYM CG_INVALID_PARAM_HANDLE_ERROR}
  CG_UNKNOWN_PROFILE_ERROR          = 19; // "The specified profile is unknown."
  {$EXTERNALSYM CG_UNKNOWN_PROFILE_ERROR}
  CG_VAR_ARG_ERROR                  = 20; // "The variable arguments were specified incorrectly."
  {$EXTERNALSYM CG_VAR_ARG_ERROR}
  CG_INVALID_DIMENSION_ERROR        = 21; // "The dimension value is invalid."
  {$EXTERNALSYM CG_INVALID_DIMENSION_ERROR}
  CG_ARRAY_PARAM_ERROR              = 22; // "The parameter must be an array."
  {$EXTERNALSYM CG_ARRAY_PARAM_ERROR}
  CG_OUT_OF_ARRAY_BOUNDS_ERROR      = 23; // "Index into the array is out of bounds."
  {$EXTERNALSYM CG_OUT_OF_ARRAY_BOUNDS_ERROR}
  CG_CONFLICTING_TYPES_ERROR        = 24; // "A type being added to the context conflicts with an existing type."
  {$EXTERNALSYM CG_CONFLICTING_TYPES_ERROR}
  CG_CONFLICTING_PARAMETER_TYPES_ERROR = 25; // "The parameters being bound have conflicting types."
  {$EXTERNALSYM CG_CONFLICTING_PARAMETER_TYPES_ERROR}
  CG_PARAMETER_IS_NOT_SHARED_ERROR  = 26; // "The parameter must be global."
  {$EXTERNALSYM CG_PARAMETER_IS_NOT_SHARED_ERROR}
  CG_INVALID_PARAMETER_VARIABILITY_ERROR = 27; // "The parameter could not be changed to the given variability."
  {$EXTERNALSYM CG_INVALID_PARAMETER_VARIABILITY_ERROR}
  CG_CANNOT_DESTROY_PARAMETER_ERROR = 28; // "Cannot destroy the parameter.  It is bound to other parameters or is not a root parameter."
  {$EXTERNALSYM CG_CANNOT_DESTROY_PARAMETER_ERROR}
  CG_NOT_ROOT_PARAMETER_ERROR       = 29; // "The parameter is not a root parameter."
  {$EXTERNALSYM CG_NOT_ROOT_PARAMETER_ERROR}
  CG_PARAMETERS_DO_NOT_MATCH_ERROR  = 30; // "The two parameters being bound do not match."
  {$EXTERNALSYM CG_PARAMETERS_DO_NOT_MATCH_ERROR}
  CG_IS_NOT_PROGRAM_PARAMETER_ERROR = 31; // "The parameter is not a program parameter."
  {$EXTERNALSYM CG_IS_NOT_PROGRAM_PARAMETER_ERROR}
  CG_INVALID_PARAMETER_TYPE_ERROR   = 32; // "The type of the parameter is invalid."
  {$EXTERNALSYM CG_INVALID_PARAMETER_TYPE_ERROR}
  CG_PARAMETER_IS_NOT_RESIZABLE_ARRAY_ERROR = 33; // "The parameter must be a resizable array."
  {$EXTERNALSYM CG_PARAMETER_IS_NOT_RESIZABLE_ARRAY_ERROR}
  CG_INVALID_SIZE_ERROR             = 34; // "The size value is invalid."
  {$EXTERNALSYM CG_INVALID_SIZE_ERROR}
  CG_BIND_CREATES_CYCLE_ERROR       = 35; // "Cannot bind the given parameters.  Binding will form a cycle."
  {$EXTERNALSYM CG_BIND_CREATES_CYCLE_ERROR}
  CG_ARRAY_TYPES_DO_NOT_MATCH_ERROR = 36; // "Cannot bind the given parameters.  Array types do not match."
  {$EXTERNALSYM CG_ARRAY_TYPES_DO_NOT_MATCH_ERROR}
  CG_ARRAY_DIMENSIONS_DO_NOT_MATCH_ERROR = 37; // "Cannot bind the given parameters.  Array dimensions do not match."
  {$EXTERNALSYM CG_ARRAY_DIMENSIONS_DO_NOT_MATCH_ERROR}
  CG_ARRAY_HAS_WRONG_DIMENSION_ERROR = 38; // "The array has the wrong dimension."
  {$EXTERNALSYM CG_ARRAY_HAS_WRONG_DIMENSION_ERROR}
  CG_TYPE_IS_NOT_DEFINED_IN_PROGRAM_ERROR = 39; // "Connecting the parameters failed because The type of the source parameter is not defined within the given program or does not match the type with the same name in the program."
  {$EXTERNALSYM CG_TYPE_IS_NOT_DEFINED_IN_PROGRAM_ERROR}
  CG_INVALID_EFFECT_HANDLE_ERROR    = 40; // "Invalid effect handle."
  {$EXTERNALSYM CG_INVALID_EFFECT_HANDLE_ERROR}
  CG_INVALID_STATE_HANDLE_ERROR     = 41; // "Invalid state handle."
  {$EXTERNALSYM CG_INVALID_STATE_HANDLE_ERROR}
  CG_INVALID_STATE_ASSIGNMENT_HANDLE_ERROR = 42; // "Invalid stateassignment handle."
  {$EXTERNALSYM CG_INVALID_STATE_ASSIGNMENT_HANDLE_ERROR}
  CG_INVALID_PASS_HANDLE_ERROR      = 43; // "Invalid pass handle."
  {$EXTERNALSYM CG_INVALID_PASS_HANDLE_ERROR}
  CG_INVALID_ANNOTATION_HANDLE_ERROR = 44; // "Invalid annotation handle."
  {$EXTERNALSYM CG_INVALID_ANNOTATION_HANDLE_ERROR}
  CG_INVALID_TECHNIQUE_HANDLE_ERROR = 45; // "Invalid technique handle."
  {$EXTERNALSYM CG_INVALID_TECHNIQUE_HANDLE_ERROR}
  CG_INVALID_PARAMETER_HANDLE_ERROR = 46; // "Invalid parameter handle."
  {$EXTERNALSYM CG_INVALID_PARAMETER_HANDLE_ERROR}
  CG_STATE_ASSIGNMENT_TYPE_MISMATCH_ERROR = 47; // "Operation is not valid for this type of stateassignment."
  {$EXTERNALSYM CG_STATE_ASSIGNMENT_TYPE_MISMATCH_ERROR}
  CG_INVALID_FUNCTION_HANDLE_ERROR  = 48; // "Invalid function handle."
  {$EXTERNALSYM CG_INVALID_FUNCTION_HANDLE_ERROR}
  CG_INVALID_TECHNIQUE_ERROR        = 49; // "Technique did not pass validation."
  {$EXTERNALSYM CG_INVALID_TECHNIQUE_ERROR}
  CG_INVALID_POINTER_ERROR          = 50; // "The supplied pointer is NULL."
  {$EXTERNALSYM CG_INVALID_POINTER_ERROR}
  CG_NOT_ENOUGH_DATA_ERROR          = 51; // "Not enough data was provided."
  {$EXTERNALSYM CG_NOT_ENOUGH_DATA_ERROR}
  CG_NON_NUMERIC_PARAMETER_ERROR    = 52; // "The parameter is not of a numeric type."
  {$EXTERNALSYM CG_NON_NUMERIC_PARAMETER_ERROR}
  CG_ARRAY_SIZE_MISMATCH_ERROR      = 53; // "The specified array sizes are not compatible with the given array."
  {$EXTERNALSYM CG_ARRAY_SIZE_MISMATCH_ERROR}
  CG_CANNOT_SET_NON_UNIFORM_PARAMETER_ERROR = 54; // "Cannot set the value of a non-uniform parameter."
  {$EXTERNALSYM CG_CANNOT_SET_NON_UNIFORM_PARAMETER_ERROR}
  CG_DUPLICATE_NAME_ERROR           = 55; // "This name is already in use."
  {$EXTERNALSYM CG_DUPLICATE_NAME_ERROR}

type
  TCGparameterclass = (
    CG_PARAMETERCLASS_UNKNOWN{ = 0},
    CG_PARAMETERCLASS_SCALAR,
    CG_PARAMETERCLASS_VECTOR,
    CG_PARAMETERCLASS_MATRIX,
    CG_PARAMETERCLASS_STRUCT,
    CG_PARAMETERCLASS_ARRAY,
    CG_PARAMETERCLASS_SAMPLER,
    CG_PARAMETERCLASS_OBJECT
  );
  CGparameterclass = TCGparameterclass;
  {$NODEFINE CGparameterclass}
  {$NODEFINE TCGparameterclass}
  {$HPPEMIT 'typedef CGparameterclass TCGparameterclass;'}

//!!! PREPROCESS END
{$IFDEF COMPILER6_UP}
type
  TCGenum = (
  //todo: Insert code from here: # include <Cg/cg_enums.h>
  //todo: FIX BCB6 issues with {$IFDEF BCB}
  {$IFDEF BCB}
    CG_Enum_Dummy
  {$ELSE}
    CG_UNKNOWN = 4096,
    CG_IN,
    CG_OUT,
    CG_INOUT,
    CG_MIXED,
    CG_VARYING,
    CG_UNIFORM,
    CG_CONSTANT,
    CG_PROGRAM_SOURCE,
    CG_PROGRAM_ENTRY,
    CG_COMPILED_PROGRAM,
    CG_PROGRAM_PROFILE,

    CG_GLOBAL,
    CG_PROGRAM,

    CG_DEFAULT,
    CG_ERROR,

    CG_SOURCE,
    CG_OBJECT,

    CG_COMPILE_MANUAL,
    CG_COMPILE_IMMEDIATE,
    CG_COMPILE_LAZY,
    CG_CURRENT,
    CG_LITERAL,
    CG_VERSION,
    CG_ROW_MAJOR,
    CG_COLUMN_MAJOR
  {$ENDIF}
  );
{$ELSE}
const
  CG_UNKNOWN             = 4096;
  {$EXTERNALSYM CG_UNKNOWN}
  CG_IN                  = 4097;
  {$EXTERNALSYM CG_IN}
  CG_OUT                 = 4098;
  {$EXTERNALSYM CG_OUT}
  CG_INOUT               = 4099;
  {$EXTERNALSYM CG_INOUT}
  CG_MIXED               = 4100;
  {$EXTERNALSYM CG_MIXED}
  CG_VARYING             = 4101;
  {$EXTERNALSYM CG_VARYING}
  CG_UNIFORM             = 4102;
  {$EXTERNALSYM CG_UNIFORM}
  CG_CONSTANT            = 4103;
  {$EXTERNALSYM CG_CONSTANT}
  CG_PROGRAM_SOURCE      = 4104;
  {$EXTERNALSYM CG_PROGRAM_SOURCE}
  CG_PROGRAM_ENTRY       = 4105;
  {$EXTERNALSYM CG_PROGRAM_ENTRY}
  CG_COMPILED_PROGRAM    = 4106;
  {$EXTERNALSYM CG_COMPILED_PROGRAM}
  CG_PROGRAM_PROFILE     = 4107;
  {$EXTERNALSYM CG_PROGRAM_PROFILE}

  CG_GLOBAL              = 4108;
  {$EXTERNALSYM CG_GLOBAL}
  CG_PROGRAM             = 4109;
  {$EXTERNALSYM CG_PROGRAM}

  CG_DEFAULT             = 4110;
  {$EXTERNALSYM CG_DEFAULT}
  CG_ERROR               = 4111;
  {$EXTERNALSYM CG_ERROR}

  CG_SOURCE              = 4112;
  {$EXTERNALSYM CG_SOURCE}
  CG_OBJECT              = 4113;
  {$EXTERNALSYM CG_OBJECT}

  CG_COMPILE_MANUAL      = 4114;
  {$EXTERNALSYM CG_COMPILE_MANUAL}
  CG_COMPILE_IMMEDIATE   = 4115;
  {$EXTERNALSYM CG_COMPILE_IMMEDIATE}
  CG_COMPILE_LAZY        = 4116;
  {$EXTERNALSYM CG_COMPILE_LAZY}
  CG_CURRENT             = 4117;
  {$EXTERNALSYM CG_CURRENT}
  CG_LITERAL             = 4118;
  {$EXTERNALSYM CG_LITERAL}
  CG_VERSION             = 4119;
  {$EXTERNALSYM CG_VERSION}
  CG_ROW_MAJOR           = 4120;
  {$EXTERNALSYM CG_ROW_MAJOR}
  CG_COLUMN_MAJOR        = 4121;
  {$EXTERNALSYM CG_COLUMN_MAJOR}

type
  TCGenum = DWORD;
{$ENDIF}
  CGenum = TCGenum;
  {$NODEFINE CGenum}
  {$NODEFINE TCGenum}
  {$HPPEMIT 'typedef CGenum TCGenum;'}

{$IFDEF COMPILER6_UP}
type
  TCGdomain = (
  {$IFDEF BCB}
    CG_Domain_Dummy
  {$ELSE}
    CG_UNKNOWN_DOMAIN = 0,
    CG_FIRST_DOMAIN   = 1,
    CG_VERTEX_DOMAIN  = 1,
    CG_FRAGMENT_DOMAIN,
    CG_GEOMETRY_DOMAIN,
    CG_NUMBER_OF_DOMAINS
  {$ENDIF}
  );
{$ELSE}
const
  CG_UNKNOWN_DOMAIN = 0;
  {$EXTERNALSYM CG_UNKNOWN_DOMAIN}
  CG_FIRST_DOMAIN   = 1;
  {$EXTERNALSYM CG_FIRST_DOMAIN}
  CG_VERTEX_DOMAIN  = 1;
  {$EXTERNALSYM CG_VERTEX_DOMAIN}
  CG_FRAGMENT_DOMAIN = 2;
  {$EXTERNALSYM CG_FRAGMENT_DOMAIN}
  CG_GEOMETRY_DOMAIN = 3;
  {$EXTERNALSYM CG_GEOMETRY_DOMAIN}
  CG_NUMBER_OF_DOMAINS = 4;
  {$EXTERNALSYM CG_NUMBER_OF_DOMAINS}

type
  TCGdomain = DWORD;
{$ENDIF}
  CGdomain = TCGdomain;
  {$NODEFINE CGdomain}
  {$NODEFINE TCGdomain}
  {$HPPEMIT 'typedef CGdomain TCGdomain;'}

type
  TCGstatecallback = function (sa: PCGstateassignment): CGbool; cdecl;
  {$NODEFINE TCGstatecallback}
  TCGerrorCallbackFunc = procedure; cdecl;
  {$NODEFINE TCGerrorCallbackFunc}
  TCGerrorHandlerFunc = procedure(ctx: PCGcontext; err: TCGerror; data: Pointer); cdecl;
  {$NODEFINE TCGerrorHandlerFunc}


(*************************************************************************)
(*** Functions                                                         ***)
(*************************************************************************)

//{$IFNDEF CG_EXPLICIT}

(*** Context functions ***)

function cgCreateContext: PCGcontext; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateContext}
procedure cgDestroyContext(ctx: PCGcontext); cdecl; external CgLibrary;
{$EXTERNALSYM cgDestroyContext}
function cgIsContext(ctx: PCGcontext): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsContext}
function cgGetLastListing(ctx: PCGcontext): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetLastListing}
procedure cgSetLastListing(handle: PCGhandle; const listing: PTERRAChar); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetLastListing}
procedure cgSetAutoCompile(ctx: PCGcontext; flag: TCGenum); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetAutoCompile}
function cgGetAutoCompile(ctx: PCGcontext): TCGenum; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetAutoCompile}

{$IFNDEF COMPILER6_UP}
type
  PPTERRAChar = ^PTERRAChar; //Clootie: It's actually pointer to array of PTERRAChar strings

{$ENDIF}

(*** Program functions ***)

function cgCreateProgram(ctx: PCGcontext;
  program_type: TCGenum; const _program: PTERRAChar;
  profile: TCGprofile; const entry: PTERRAChar;
  const args: PPTERRAChar): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateProgram}
function cgCreateProgramFromFile(ctx: PCGcontext;
  program_type: TCGenum; const program_file: PTERRAChar;
  profile: TCGprofile; const entry: PTERRAChar;
  const args: PPTERRAChar): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateProgramFromFile}
function cgCopyProgram(_program: PCGprogram): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgCopyProgram}
procedure cgDestroyProgram(_program: PCGprogram); cdecl; external CgLibrary;
{$EXTERNALSYM cgDestroyProgram}

function cgGetFirstProgram(ctx: PCGcontext): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstProgram}
function cgGetNextProgram(current: PCGprogram): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextProgram}
function cgGetProgramContext(prog: PCGprogram): PCGcontext; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProgramContext}
function cgIsProgram(_program: PCGprogram): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsProgram}

procedure cgCompileProgram(_program: PCGprogram); cdecl; external CgLibrary;
{$EXTERNALSYM cgCompileProgram}
function cgIsProgramCompiled(_program: PCGprogram): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsProgramCompiled}
function cgGetProgramString(prog: PCGprogram; pname: TCGenum): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProgramString}
function cgGetProgramProfile(prog: PCGprogram): TCGprofile; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProgramProfile}
function cgGetProgramOptions(prog: PCGprogram): PPTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProgramOptions}
procedure cgSetProgramProfile(prog: PCGprogram; profile: TCGprofile); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetProgramProfile}

procedure cgSetPassProgramParameters(prog: PCGprogram); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetPassProgramParameters}

(*** Parameter functions ***)

function cgCreateParameter(ctx: PCGcontext; type_: TCGtype): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateParameter}
function cgCreateParameterArray(ctx: PCGcontext; type_: TCGtype; length: Integer): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateParameterArray}
function cgCreateParameterMultiDimArray(ctx: PCGcontext; type_: TCGtype; dim: Integer; const lengths: PInteger): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateParameterMultiDimArray}
procedure cgDestroyParameter(param: PCGparameter); cdecl; external CgLibrary;
{$EXTERNALSYM cgDestroyParameter}
procedure cgConnectParameter(from, to_: PCGparameter); cdecl; external CgLibrary;
{$EXTERNALSYM cgConnectParameter}
procedure cgDisconnectParameter(param: PCGparameter); cdecl; external CgLibrary;
{$EXTERNALSYM cgDisconnectParameter}
function cgGetConnectedParameter(param: PCGparameter): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetConnectedParameter}

function cgGetNumConnectedToParameters(param: PCGparameter): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNumConnectedToParameters}
function cgGetConnectedToParameter(param: PCGparameter; index: Integer): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetConnectedToParameter}

function cgGetNamedParameter(prog: PCGprogram; const name: PTERRAChar): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedParameter}
function cgGetNamedProgramParameter(prog: PCGprogram; name_space: TCGenum; const name: PTERRAChar): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedProgramParameter}

function cgGetFirstParameter(prog: PCGprogram; name_space: TCGenum): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstParameter}
{$IFNDEF CG_DEPRECATED_1_1_API}
function cgGetNextParameter(current: PCGparameter): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextParameter}
{$ENDIF}
function cgGetFirstLeafParameter(prog: PCGprogram; name_space: TCGenum): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstLeafParameter}
{$IFNDEF CG_DEPRECATED_1_1_API}
function cgGetNextLeafParameter(current: PCGparameter): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextLeafParameter}
{$ENDIF}

function cgGetFirstStructParameter(param: PCGparameter): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstStructParameter}
function cgGetNamedStructParameter(param: PCGparameter; const name: PTERRAChar): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedStructParameter}

function cgGetFirstDependentParameter(param: PCGparameter): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstDependentParameter}

function cgGetArrayParameter(aparam: PCGparameter; index: Integer): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetArrayParameter}
function cgGetArrayDimension(param: PCGparameter): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetArrayDimension}
function cgGetArrayType(param: PCGparameter): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetArrayType}
function cgGetArraySize(param: PCGparameter; dimension: Integer): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetArraySize}
function cgGetArrayTotalSize(param: PCGparameter): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetArrayTotalSize}
procedure cgSetArraySize(param: PCGparameter; size: Integer); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetArraySize}
procedure cgSetMultiDimArraySize(param: PCGparameter; const sizes: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetMultiDimArraySize}

function cgGetParameterProgram(param: PCGparameter): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterProgram}
function cgGetParameterContext(param: PCGparameter): PCGcontext; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterContext}
function cgIsParameter(param: PCGparameter): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsParameter}
function cgGetParameterName(param: PCGparameter): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterName}
function cgGetParameterType(param: PCGparameter): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterType}
function cgGetParameterBaseType(param: PCGparameter): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterBaseType}
function cgGetParameterClass(param: PCGparameter): TCGparameterclass; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterClass}
function cgGetParameterRows(param: PCGparameter): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterRows}
function cgGetParameterColumns(param: PCGparameter): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterColumns}
function cgGetParameterNamedType(param: PCGparameter): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterNamedType}
function cgGetParameterSemantic(param: PCGparameter): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterSemantic}
function cgGetParameterResource(param: PCGparameter): TCGresource; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterResource}
function cgGetParameterBaseResource(param: PCGparameter): TCGresource; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterBaseResource}
function cgGetParameterResourceIndex(param: PCGparameter): LongWord; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterResourceIndex}
function cgGetParameterVariability(param: PCGparameter): TCGenum; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterVariability}
function cgGetParameterDirection(param: PCGparameter): TCGenum; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterDirection}
function cgIsParameterReferenced(param: PCGparameter): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsParameterReferenced}
function cgIsParameterUsed(param: PCGparameter; handle: PCGhandle): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsParameterUsed}
function cgGetParameterValues(param: PCGparameter; value_type: TCGenum; out nvalues: Integer): PDouble{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterValues}
procedure cgSetParameterValuedr(param: PCGparameter; n: Integer; const vals: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterValuedr}
procedure cgSetParameterValuedc(param: PCGparameter; n: Integer; const vals: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterValuedc}
procedure cgSetParameterValuefr(param: PCGparameter; n: Integer; const vals: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterValuefr}
procedure cgSetParameterValuefc(param: PCGparameter; n: Integer; const vals: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterValuefc}
procedure cgSetParameterValueir(param: PCGparameter; n: Integer; const vals: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterValueir}
procedure cgSetParameterValueic(param: PCGparameter; n: Integer; const vals: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterValueic}
function cgGetParameterValuedr(param: PCGparameter; n: Integer; const vals: PDouble): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterValuedr}
function cgGetParameterValuedc(param: PCGparameter; n: Integer; const vals: PDouble): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterValuedc}
function cgGetParameterValuefr(param: PCGparameter; n: Integer; const vals: PSingle): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterValuefr}
function cgGetParameterValuefc(param: PCGparameter; n: Integer; const vals: PSingle): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterValuefc}
function cgGetParameterValueir(param: PCGparameter; n: Integer; const vals: PInteger): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterValueir}
function cgGetParameterValueic(param: PCGparameter; n: Integer; const vals: PInteger): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterValueic}
function cgGetStringParameterValue(param: PCGparameter): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStringParameterValue}
procedure cgSetStringParameterValue(param: PCGparameter; const str: PTERRAChar); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetStringParameterValue}

function cgGetParameterOrdinalNumber(param: PCGparameter): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterOrdinalNumber}
function cgIsParameterGlobal(param: PCGparameter): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsParameterGlobal}
function cgGetParameterIndex(param: PCGparameter): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterIndex}

procedure cgSetParameterVariability(param: PCGparameter; vary: TCGenum); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterVariability}
procedure cgSetParameterSemantic(param: PCGparameter; const semantic: PTERRAChar); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameterSemantic}

procedure cgSetParameter1f(param: PCGparameter; x: Single); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter1f}
procedure cgSetParameter2f(param: PCGparameter; x, y: Single); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter2f}
procedure cgSetParameter3f(param: PCGparameter; x, y, z: Single); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter3f}
procedure cgSetParameter4f(param: PCGparameter; x, y, z, w: Single); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter4f}
procedure cgSetParameter1d(param: PCGparameter; x: Double); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter1d}
procedure cgSetParameter2d(param: PCGparameter; x, y: Double); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter2d}
procedure cgSetParameter3d(param: PCGparameter; x, y, z: Double); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter3d}
procedure cgSetParameter4d(param: PCGparameter; x, y, z, w: Double); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter4d}
procedure cgSetParameter1i(param: PCGparameter; x: Integer); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter1i}
procedure cgSetParameter2i(param: PCGparameter; x, y: Integer); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter2i}
procedure cgSetParameter3i(param: PCGparameter; x, y, z: Integer); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter3i}
procedure cgSetParameter4i(param: PCGparameter; x, y, z, w: Integer); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter4i}

procedure cgSetParameter1iv(param: PCGparameter; const v: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter1iv}
procedure cgSetParameter2iv(param: PCGparameter; const v: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter2iv}
procedure cgSetParameter3iv(param: PCGparameter; const v: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter3iv}
procedure cgSetParameter4iv(param: PCGparameter; const v: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter4iv}
procedure cgSetParameter1fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter1fv}
procedure cgSetParameter2fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter2fv}
procedure cgSetParameter3fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter3fv}
procedure cgSetParameter4fv(param: PCGparameter; const v: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter4fv}
procedure cgSetParameter1dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter1dv}
procedure cgSetParameter2dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter2dv}
procedure cgSetParameter3dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter3dv}
procedure cgSetParameter4dv(param: PCGparameter; const x: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetParameter4dv}

procedure cgSetMatrixParameterir(param: PCGparameter; const matrix: Pinteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetMatrixParameterir}
procedure cgSetMatrixParameterdr(param: PCGparameter; const matrix: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetMatrixParameterdr}
procedure cgSetMatrixParameterfr(param: PCGparameter; const matrix: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetMatrixParameterfr}
procedure cgSetMatrixParameteric(param: PCGparameter; const matrix: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetMatrixParameteric}
procedure cgSetMatrixParameterdc(param: PCGparameter; const matrix: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetMatrixParameterdc}
procedure cgSetMatrixParameterfc(param: PCGparameter; const matrix: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetMatrixParameterfc}

procedure cgGetMatrixParameterir(param: PCGparameter; const matrix: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgGetMatrixParameterir}
procedure cgGetMatrixParameterdr(param: PCGparameter; const matrix: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgGetMatrixParameterdr}
procedure cgGetMatrixParameterfr(param: PCGparameter; const matrix: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgGetMatrixParameterfr}
procedure cgGetMatrixParameteric(param: PCGparameter; const matrix: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgGetMatrixParameteric}
procedure cgGetMatrixParameterdc(param: PCGparameter; const matrix: PDouble); cdecl; external CgLibrary;
{$EXTERNALSYM cgGetMatrixParameterdc}
procedure cgGetMatrixParameterfc(param: PCGparameter; const matrix: PSingle); cdecl; external CgLibrary;
{$EXTERNALSYM cgGetMatrixParameterfc}

(*** Type Functions ***)

function cgGetTypeString(_type: TCGtype): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetTypeString}
function cgGetType(const type_string: PTERRAChar): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetType}

function cgGetNamedUserType(handle: PCGhandle; const name: PTERRAChar): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedUserType}

function cgGetNumUserTypes(handle: PCGhandle): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNumUserTypes}
function cgGetUserType(handle: PCGhandle; index: Integer): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetUserType}

function cgGetNumParentTypes(type_: TCGtype): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNumParentTypes}
function cgGetParentType(type_: TCGtype; index: Integer): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParentType}

function cgIsParentType(parent, child: TCGtype): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsParentType}
function cgIsInterfaceType(type_: TCGtype): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsInterfaceType}

(*** Resource Functions ***)

function cgGetResourceString(resource: TCGresource): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetResourceString}
function cgGetResource(const resource_string: PTERRAChar): TCGresource; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetResource}

{*** Enum Functions ***}

function cgGetEnumString(en: TCGenum): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetEnumString}
function cgGetEnum(const enum_string: PTERRAChar): TCGenum; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetEnum}

(*** Profile Functions ***)

function cgGetProfileString(profile: TCGprofile): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProfileString}
function cgGetProfile(const profile_string: PTERRAChar): TCGprofile; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProfile}

(*** Error Functions ***)

function cgGetError: TCGerror; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetError}
function cgGetFirstError: TCGerror; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstError}
function cgGetErrorString(error: TCGerror): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetErrorString}
function cgGetLastErrorString(error: PCGerror): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetLastErrorString}
procedure cgSetErrorCallback(func: TCGerrorCallbackFunc); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetErrorCallback}
function cgGetErrorCallback: TCGerrorCallbackFunc; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetErrorCallback}
procedure cgSetErrorHandler(func: TCGerrorHandlerFunc; data: Pointer); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetErrorHandler}
function cgGetErrorHandler(data: Pointer): TCGerrorHandlerFunc; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetErrorHandler}


{*** Misc Functions ***}

function cgGetString(sname: TCGenum): PTERRAChar{ const }; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetString}


{*** CgFX Functions ***}

function cgCreateEffect(ctx: PCGcontext; const code: PTERRAChar; const args: PPTERRAChar): PCGeffect; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateEffect}
function cgCreateEffectFromFile(ctx: PCGcontext; const filename: PTERRAChar; const args: PPTERRAChar): PCGeffect; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateEffectFromFile}
procedure cgDestroyEffect(effect: PCGeffect); cdecl; external CgLibrary;
{$EXTERNALSYM cgDestroyEffect}
function cgGetEffectContext(effect: PCGeffect): PCGcontext; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetEffectContext}
function cgIsEffect(effect: PCGeffect): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsEffect}

function cgGetFirstEffect(ctx: PCGcontext): PCGeffect; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstEffect}
function cgGetNextEffect(ctx: PCGcontext): PCGeffect; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextEffect}

function cgCreateProgramFromEffect(effect: PCGeffect; profile: TCGprofile;
  const entry: PTERRAChar; const args: PPTERRAChar): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateProgramFromEffect}

function cgGetFirstTechnique(effect: PCGeffect): PCGtechnique; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstTechnique}
function cgGetNextTechnique(effect: PCGeffect): PCGtechnique; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextTechnique}
function cgGetNamedTechnique(effect: PCGeffect; const name: PTERRAChar): PCGtechnique; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedTechnique}
function cgGetTechniqueName(techn: PCGtechnique): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetTechniqueName}
function cgIsTechnique(techn: PCGtechnique): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsTechnique}
function cgValidateTechnique(techn: PCGtechnique): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgValidateTechnique}
function cgIsTechniqueValidated(techn: PCGtechnique): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsTechniqueValidated}
function cgGetTechniqueEffect(techn: PCGtechnique): PCGeffect; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetTechniqueEffect}

function cgGetFirstPass(techn: PCGtechnique): PCGpass; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstPass}
function cgGetNamedPass(techn: PCGtechnique; const name: PTERRAChar): PCGpass; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedPass}
function cgGetNextPass(pass: PCGpass): PCGpass; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextPass}
function cgIsPass(pass: PCGpass): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsPass}
function cgGetPassName(pass: PCGpass): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetPassName}
function cgGetPassTechnique(pass: PCGpass): PCGtechnique; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetPassTechnique}

procedure cgSetPassState(pass: PCGpass); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetPassState}
procedure cgResetPassState(pass: PCGpass); cdecl; external CgLibrary;
{$EXTERNALSYM cgResetPassState}

function cgGetFirstStateAssignment(pass: PCGpass): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstStateAssignment}
function cgGetNamedStateAssignment(pass: PCGpass; const name: PTERRAChar): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedStateAssignment}
function cgGetNextStateAssignment(sa: PCGstateassignment): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextStateAssignment}
function cgIsStateAssignment(sa: PCGstateassignment): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsStateAssignment}
function cgCallStateSetCallback(sa: PCGstateassignment): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgCallStateSetCallback}
function cgCallStateValidateCallback(sa: PCGstateassignment): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgCallStateValidateCallback}
function cgCallStateResetCallback(sa: PCGstateassignment): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgCallStateResetCallback}
function cgGetStateAssignmentPass(sa: PCGstateassignment): PCGpass; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateAssignmentPass}
function cgGetSamplerStateAssignmentParameter(sa: PCGstateassignment): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetSamplerStateAssignmentParameter}

function cgGetFloatStateAssignmentValues(sa: PCGstateassignment; out nVals: Integer): PSingle; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFloatStateAssignmentValues}
function cgGetIntStateAssignmentValues(sa: PCGstateassignment; out nVals: Integer): PInteger; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetIntStateAssignmentValues}
function cgGetBoolStateAssignmentValues(sa: PCGstateassignment; out nVals: Integer): PCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetBoolStateAssignmentValues}
function cgGetStringStateAssignmentValue(sa: PCGstateassignment): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStringStateAssignmentValue}
function cgGetProgramStateAssignmentValue(sa: PCGstateassignment): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProgramStateAssignmentValue}
function cgGetTextureStateAssignmentValue(sa: PCGstateassignment): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetTextureStateAssignmentValue}
function cgGetSamplerStateAssignmentValue(sa: PCGstateassignment): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetSamplerStateAssignmentValue}
function cgGetStateAssignmentIndex(sa: PCGstateassignment): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateAssignmentIndex}

function cgGetNumDependentStateAssignmentParameters(sa: PCGstateassignment): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNumDependentStateAssignmentParameters}
function cgGetDependentStateAssignmentParameter(sa: PCGstateassignment; index: Integer): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetDependentStateAssignmentParameter}

function cgGetStateAssignmentState(sa: PCGstateassignment): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateAssignmentState}
function cgGetSamplerStateAssignmentState(sa: PCGstateassignment): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetSamplerStateAssignmentState}

function cgCreateState(ctx: PCGcontext; const name: PTERRAChar; type_: TCGtype): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateState}
function cgCreateArrayState(ctx: PCGcontext; const name: PTERRAChar; type_: TCGtype; nelems: Integer): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateArrayState}
procedure cgSetStateCallbacks(state: PCGstate; set_, reset, validate: TCGstatecallback); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetStateCallbacks}
function cgGetStateSetCallback(state: PCGstate): TCGstatecallback; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateSetCallback}
function cgGetStateResetCallback(state: PCGstate): TCGstatecallback; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateResetCallback}
function cgGetStateValidateCallback(state: PCGstate): TCGstatecallback; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateValidateCallback}
function cgGetStateType(state: PCGstate): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateType}
function cgGetStateName(state: PCGstate): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateName}
function cgGetNamedState(ctx: PCGcontext; const name: PTERRAChar): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedState}
function cgGetFirstState(ctx: PCGcontext): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstState}
function cgGetNextState(state: PCGstate): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextState}
function cgIsState(state: PCGstate): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsState}
procedure cgAddStateEnumerant(state: PCGstate; const name: PTERRAChar; value: Integer); cdecl; external CgLibrary;
{$EXTERNALSYM cgAddStateEnumerant}

function cgCreateSamplerState(ctx: PCGcontext; const name: PTERRAChar; type_: TCGtype): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateSamplerState}
function cgCreateArraySamplerState(ctx: PCGcontext; const name: PTERRAChar; type_: TCGtype; nelems: Integer): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateArraySamplerState}
function cgGetNamedSamplerState(ctx: PCGcontext; const name: PTERRAChar): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedSamplerState}
function cgGetFirstSamplerState(ctx: PCGcontext): PCGstate; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstSamplerState}

function cgGetFirstSamplerStateAssignment(param: PCGparameter): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstSamplerStateAssignment}
function cgGetNamedSamplerStateAssignment(param: PCGparameter; const name: PTERRAChar): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedSamplerStateAssignment}
procedure cgSetSamplerState(param: PCGparameter); cdecl; external CgLibrary;
{$EXTERNALSYM cgSetSamplerState}

function cgGetNamedEffectParameter(effect: PCGeffect; const name: PTERRAChar): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedEffectParameter}
function cgGetFirstLeafEffectParameter(effect: PCGeffect): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstLeafEffectParameter}
function cgGetFirstEffectParameter(effect: PCGeffect): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstEffectParameter}
function cgGetEffectParameterBySemantic(effect: PCGeffect; const name: PTERRAChar): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetEffectParameterBySemantic}

function cgGetFirstTechniqueAnnotation(techn: PCGtechnique): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstTechniqueAnnotation}
function cgGetFirstPassAnnotation(pass: PCGpass): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstPassAnnotation}
function cgGetFirstParameterAnnotation(param: PCGparameter): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstParameterAnnotation}
function cgGetFirstProgramAnnotation(prog: PCGprogram): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstProgramAnnotation}
function cgGetFirstEffectAnnotation(ann: PCGannotation): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFirstEffectAnnotation}
function cgGetNextAnnotation(ann: PCGannotation): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNextAnnotation}

function cgGetNamedTechniqueAnnotation(techn: PCGtechnique; name: PTERRAChar): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedTechniqueAnnotation}
function cgGetNamedPassAnnotation(pass: PCGpass; name: PTERRAChar): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedPassAnnotation}
function cgGetNamedParameterAnnotation(param: PCGparameter; name: PTERRAChar): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedParameterAnnotation}
function cgGetNamedProgramAnnotation(prog: PCGprogram; name: PTERRAChar): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedProgramAnnotation}
function cgGetNamedEffectAnnotation(prog: PCGeffect; name: PTERRAChar): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedEffectAnnotation}

function cgIsAnnotation(ann: PCGannotation): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgIsAnnotation}

function cgGetAnnotationName(ann: PCGannotation): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetAnnotationName}
function cgGetAnnotationType(ann: PCGannotation): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetAnnotationType}

function cgGetFloatAnnotationValues(ann: PCGannotation; out nvalues: Integer): PSingle; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetFloatAnnotationValues}
function cgGetIntAnnotationValues(ann: PCGannotation; out nvalues: Integer): PInteger; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetIntAnnotationValues}
function cgGetStringAnnotationValue(ann: PCGannotation): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStringAnnotationValue}
function cgGetBoolAnnotationValues(ann: PCGannotation; out nvalues: Integer): PCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetBoolAnnotationValues}
function cgGetBooleanAnnotationValues(ann: PCGannotation; out nvalues: Integer): PInteger; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetBooleanAnnotationValues}

function cgGetNumDependentAnnotationParameters(ann: PCGannotation): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNumDependentAnnotationParameters}
function cgGetDependentAnnotationParameter(ann: PCGannotation; index: Integer): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetDependentAnnotationParameter}

procedure cgEvaluateProgram(prog: PCGprogram; buf: PSingle; ncomps: Integer; nx, ny, nz: Integer); cdecl; external CgLibrary;
{$EXTERNALSYM cgEvaluateProgram}


//*** Cg 1.5 Additions ***//

function cgSetEffectName(effect: PCGeffect; const name: PTERRAChar): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetEffectName}
function cgGetEffectName(effect: PCGeffect): {const} PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetEffectName}
function cgGetNamedEffect(ctx: PCGcontext; const name: PTERRAChar): PCGeffect; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNamedEffect}
function cgCreateEffectParameter(effect: PCGeffect; const name: PTERRAChar; type_: TCGtype): PCGparameter; cdecl; external CgLibrary;  // added name parameter to Sony proposal
{$EXTERNALSYM cgCreateEffectParameter}

function cgCreateTechnique(effect: PCGeffect; const name: PTERRAChar): PCGtechnique; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateTechnique}

function cgCreateEffectParameterArray(effect: PCGeffect; const name: PTERRAChar; type_: TCGtype; length: Integer): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateEffectParameterArray}
function cgCreateEffectParameterMultiDimArray(effect: PCGeffect; const name: PTERRAChar; type_: TCGtype; dim: Integer; const lengths: PInteger): PCGparameter; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateEffectParameterMultiDimArray}

function cgCreatePass(techn: PCGtechnique; const name: PTERRAChar): PCGpass; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreatePass}

function cgCreateStateAssignment(pass: PCGpass; state: PCGstate): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateStateAssignment}
function cgCreateStateAssignmentIndex(pass: PCGpass; state: PCGstate; index: Integer): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateStateAssignmentIndex}
function cgCreateSamplerStateAssignment(param: PCGparameter; state: PCGstate): PCGstateassignment; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateSamplerStateAssignment}

function cgSetFloatStateAssignment(sa: PCGstateassignment; value: Single): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetFloatStateAssignment}
function cgSetIntStateAssignment(sa: PCGstateassignment; value: Integer): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetIntStateAssignment}
function cgSetBoolStateAssignment(sa: PCGstateassignment; value: CGbool): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetBoolStateAssignment}
function cgSetStringStateAssignment(sa: PCGstateassignment; value: PTERRAChar): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetStringStateAssignment}
function cgSetProgramStateAssignment(sa: PCGstateassignment; value: PCGprogram): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetProgramStateAssignment}
function cgSetSamplerStateAssignment(sa: PCGstateassignment; value: PCGparameter): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetSamplerStateAssignment}
function cgSetTextureStateAssignment(sa: PCGstateassignment; value: PCGparameter): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetTextureStateAssignment}

function cgSetFloatArrayStateAssignment(sa: PCGstateassignment; vals: PSingle): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetFloatArrayStateAssignment}
function cgSetIntArrayStateAssignment(sa: PCGstateassignment; vals: PInteger): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetIntArrayStateAssignment}
function cgSetBoolArrayStateAssignment(sa: PCGstateassignment; vals: PCGbool): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetBoolArrayStateAssignment}

function cgCreateTechniqueAnnotation(techn: PCGtechnique; const name: PTERRAChar; type_: TCGtype): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateTechniqueAnnotation}
function cgCreatePassAnnotation(pass: PCGpass; const name: PTERRAChar; type_: TCGtype): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreatePassAnnotation}
function cgCreateParameterAnnotation(param: PCGparameter; const name: PTERRAChar; type_: TCGtype): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateParameterAnnotation}
function cgCreateProgramAnnotation(prog: PCGprogram; const name: PTERRAChar; type_: TCGtype): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateProgramAnnotation}
function cgCreateEffectAnnotation(effect: PCGeffect; const name: PTERRAChar; type_: TCGtype): PCGannotation; cdecl; external CgLibrary;
{$EXTERNALSYM cgCreateEffectAnnotation}

function cgSetIntAnnotation(ann: PCGannotation; value: Integer): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetIntAnnotation}
function cgSetFloatAnnotation(ann: PCGannotation; value: Single): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetFloatAnnotation}
function cgSetBoolAnnotation(ann: PCGannotation; value: CGbool): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetBoolAnnotation}
function cgSetStringAnnotation(ann: PCGannotation; value: PTERRAChar): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgSetStringAnnotation}

function cgGetStateEnumerantName(state: PCGstate; value: Integer): PTERRAChar; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateEnumerantName}
function cgGetStateEnumerantValue(state: PCGstate; name: PTERRAChar): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetStateEnumerantValue}

function cgGetParameterEffect(param: PCGparameter): PCGeffect; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetParameterEffect}

function cgGetTypeClass(type_: TCGtype): TCGparameterclass; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetTypeClass}
function cgGetTypeBase(type_: TCGtype): TCGtype; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetTypeBase}
function cgGetTypeSizes(type_: TCGtype; nrows: PInteger; ncols: PInteger): TCGbool; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetTypeSizes}
procedure cgGetMatrixSize(type_: TCGtype; nrows: PInteger; ncols: PInteger); cdecl; external CgLibrary;
{$EXTERNALSYM cgGetMatrixSize}

function cgGetNumProgramDomains(program_: PCGprogram): Integer; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetNumProgramDomains}
function cgGetProfileDomain(profile: TCGprofile): TCGdomain; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProfileDomain}
function cgCombinePrograms(n: Integer; const exeList: PPCGprogram): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgCombinePrograms}
function cgCombinePrograms2(const exe1: PCGprogram; const exe2: PCGprogram): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgCombinePrograms2}
function cgCombinePrograms3(const exe1: PCGprogram; const exe2: PCGprogram; const exe3: PCGprogram): PCGprogram; cdecl; external CgLibrary;
{$EXTERNALSYM cgCombinePrograms3}
function cgGetProgramDomainProfile(program_: PCGprogram; index: Integer): TCGprofile; cdecl; external CgLibrary;
{$EXTERNALSYM cgGetProgramDomainProfile}

implementation

end.
