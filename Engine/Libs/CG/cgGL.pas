{******************************************************************************}
{*                                                                            *}
{*  Copyright (c) 2002-2004, NVIDIA Corporation.                              *}
{*                                                                            *}
{*  Files:    cgGL.h                                                          *}
{*  Content:  NVIDIA Cg OpenGL interface include files                        *}
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
 * under NVIDIAÒs copyrights in this original NVIDIA software (the "NVIDIA
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
// 04-Mar-04 - Alexey Barkovoy:
//   - Updated to Release 1.2 of Cg toolkit (published 25-Feb-2004)
// 21-Mar-03 - Alexey Barkovoy:
//   - Updated to Release 1.1 of Cg toolkit (published 04-Mar-2003)
// 11-Jan-03 - Alexey Barkovoy:
//   - Updated to Release 1.0 of Cg toolkit (published 20-Dec-2002)

{$Include JEDI.inc}

unit cgGL;

interface

uses
  OpenGL12{$IFNDEF COMPILER6_UP}, Windows{$ENDIF}, cg;

(*$HPPEMIT '#include "cgGL.h"' *)

const
  CgGLlibrary = 'cgGL.dll';

(*****************************************************************************)
(*** cgGL Type Definitions                                                 ***)
(*****************************************************************************)

type
  TCGGLenum = TGLEnum;
  CGGLenum = TCGGLenum;
  {$NODEFINE CGGLenum}
  {$NODEFINE TCGGLenum}
  {$HPPEMIT 'typedef CGGLenum TCGGLenum;'}

const
  CG_GL_MATRIX_IDENTITY          = 0;
  {$EXTERNALSYM CG_GL_MATRIX_IDENTITY}
  CG_GL_MATRIX_TRANSPOSE         = 1;
  {$EXTERNALSYM CG_GL_MATRIX_TRANSPOSE}
  CG_GL_MATRIX_INVERSE           = 2;
  {$EXTERNALSYM CG_GL_MATRIX_INVERSE}
  CG_GL_MATRIX_INVERSE_TRANSPOSE = 3;
  {$EXTERNALSYM CG_GL_MATRIX_INVERSE_TRANSPOSE}

  CG_GL_MODELVIEW_MATRIX         = 4;
  {$EXTERNALSYM CG_GL_MODELVIEW_MATRIX}
  CG_GL_PROJECTION_MATRIX        = 5;
  {$EXTERNALSYM CG_GL_PROJECTION_MATRIX}
  CG_GL_TEXTURE_MATRIX           = 6;
  {$EXTERNALSYM CG_GL_TEXTURE_MATRIX}
  CG_GL_MODELVIEW_PROJECTION_MATRIX = 7;
  {$EXTERNALSYM CG_GL_MODELVIEW_PROJECTION_MATRIX}

  CG_GL_VERTEX                   = 8;
  {$EXTERNALSYM CG_GL_VERTEX}
  CG_GL_FRAGMENT                 = 9;
  {$EXTERNALSYM CG_GL_FRAGMENT}

(******************************************************************************
 *** Profile Functions
 *****************************************************************************)

function cgGLIsProfileSupported(profile: TCGprofile): TCGbool; cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLIsProfileSupported}

procedure cgGLEnableProfile(profile: TCGprofile); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLEnableProfile}
procedure cgGLDisableProfile(profile: TCGprofile); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLDisableProfile}

function cgGLGetLatestProfile(profile_type: TCGGLenum): TCGprofile; cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetLatestProfile}
procedure cgGLSetOptimalOptions(profile: TCGprofile); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetOptimalOptions}

(******************************************************************************
 *** Program Managment Functions
 *****************************************************************************)

procedure cgGLLoadProgram(_program: PCGprogram); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLLoadProgram}
function cgGLIsProgramLoaded(_program: PCGprogram): TCGbool; cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLIsProgramLoaded}
procedure cgGLBindProgram(_program: PCGprogram); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLBindProgram}
procedure cgGLUnbindProgram(profile: TCGprofile); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLUnbindProgram}
function cgGLGetProgramID(_program: PCGprogram): GLuint; cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetProgramID}

(******************************************************************************
 *** Parameter Managment Functions
 *****************************************************************************)

procedure cgGLSetParameter1f(param: PCGparameter; x: Single); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter1f}

procedure cgGLSetParameter2f(param: PCGparameter; x, y: Single); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter2f}

procedure cgGLSetParameter3f(param: PCGparameter; x, y, z: Single); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter3f}

procedure cgGLSetParameter4f(param: PCGparameter; x, y, z, w: Single); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter4f}

procedure cgGLSetParameter1fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter1fv}

procedure cgGLSetParameter2fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter2fv}

procedure cgGLSetParameter3fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter3fv}

procedure cgGLSetParameter4fv(param: PCGparameter; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter4fv}

procedure cgGLSetParameter1d(param: PCGparameter; x: Double); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter1d}

procedure cgGLSetParameter2d(param: PCGparameter; x, y: Double); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter2d}

procedure cgGLSetParameter3d(param: PCGparameter; x, y, z: Double); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter3d}

procedure cgGLSetParameter4d(param: PCGparameter; x, y, z, w: Double); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter4d}

procedure cgGLSetParameter1dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter1dv}

procedure cgGLSetParameter2dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter2dv}

procedure cgGLSetParameter3dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter3dv}

procedure cgGLSetParameter4dv(param: PCGparameter; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameter4dv}

procedure cgGLGetParameter1f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter1f}

procedure cgGLGetParameter2f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter2f}

procedure cgGLGetParameter3f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter3f}

procedure cgGLGetParameter4f(param: PCGparameter; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter4f}

procedure cgGLGetParameter1d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter1d}

procedure cgGLGetParameter2d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter2d}

procedure cgGLGetParameter3d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter3d}

procedure cgGLGetParameter4d(param: PCGparameter; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameter4d}

procedure cgGLSetParameterArray1f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray1f}

procedure cgGLSetParameterArray2f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray2f}

procedure cgGLSetParameterArray3f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray3f}

procedure cgGLSetParameterArray4f(param: PCGparameter;
    offset, nelements: Longint; const v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray4f}

procedure cgGLSetParameterArray1d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray1d}

procedure cgGLSetParameterArray2d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray2d}

procedure cgGLSetParameterArray3d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray3d}

procedure cgGLSetParameterArray4d(param: PCGparameter;
    offset, nelements: Longint; const v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterArray4d}

procedure cgGLGetParameterArray1f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray1f}

procedure cgGLGetParameterArray2f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray2f}

procedure cgGLGetParameterArray3f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray3f}

procedure cgGLGetParameterArray4f(param: PCGparameter;
    offset, nelements: Longint; v: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray4f}

procedure cgGLGetParameterArray1d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray1d}

procedure cgGLGetParameterArray2d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray2d}

procedure cgGLGetParameterArray3d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray3d}

procedure cgGLGetParameterArray4d(param: PCGparameter;
    offset, nelements: Longint; v: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetParameterArray4d}

procedure cgGLSetParameterPointer(param: PCGparameter; fsize: GLint;
    _type: TGLenum; stride: GLsizei; const _pointer: Pointer); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetParameterPointer}

procedure cgGLEnableClientState(param: PCGparameter); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLEnableClientState}
procedure cgGLDisableClientState(param: PCGparameter); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLDisableClientState}

(******************************************************************************
 *** Matrix Parameter Managment Functions
 *****************************************************************************)

procedure cgGLSetMatrixParameterdr(param: PCGparameter; const matrix: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterdr}
procedure cgGLSetMatrixParameterfr(param: PCGparameter; const matrix: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterfr}
procedure cgGLSetMatrixParameterdc(param: PCGparameter; const matrix: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterdc}
procedure cgGLSetMatrixParameterfc(param: PCGparameter; const matrix: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterfc}

procedure cgGLGetMatrixParameterdr(param: PCGparameter; matrix: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterdr}
procedure cgGLGetMatrixParameterfr(param: PCGparameter; matrix: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterfr}
procedure cgGLGetMatrixParameterdc(param: PCGparameter; matrix: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterdc}
procedure cgGLGetMatrixParameterfc(param: PCGparameter; matrix: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterfc}

procedure cgGLSetStateMatrixParameter(param: PCGparameter;
    matrix: TCGGLenum; transform: TCGGLenum); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetStateMatrixParameter}

procedure cgGLSetMatrixParameterArrayfc(param: PCGparameter;
    offset, nelements: Longint; const matrices: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterArrayfc}

procedure cgGLSetMatrixParameterArrayfr(param: PCGparameter;
    offset, nelements: Longint; const matrices: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterArrayfr}

procedure cgGLSetMatrixParameterArraydc(param: PCGparameter;
    offset, nelements: Longint; const matrices: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterArraydc}

procedure cgGLSetMatrixParameterArraydr(param: PCGparameter;
    offset, nelements: Longint; const matrices: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetMatrixParameterArraydr}

procedure cgGLGetMatrixParameterArrayfc(param: PCGparameter;
    offset, nelements: Longint; matrices: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterArrayfc}

procedure cgGLGetMatrixParameterArrayfr(param: PCGparameter;
    offset, nelements: Longint; matrices: PSingle); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterArrayfr}

procedure cgGLGetMatrixParameterArraydc(param: PCGparameter;
    offset, nelements: Longint; matrices: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterArraydc}

procedure cgGLGetMatrixParameterArraydr(param: PCGparameter;
    offset, nelements: Longint; matrices: PDouble); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetMatrixParameterArraydr}

(******************************************************************************
 *** Texture Parameter Managment Functions
 *****************************************************************************)

procedure cgGLSetTextureParameter(param: PCGparameter; texobj: TGLuint); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetTextureParameter}
function cgGLGetTextureParameter(param: PCGparameter): TGLuint; cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetTextureParameter}
procedure cgGLEnableTextureParameter(param: PCGparameter); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLEnableTextureParameter}
procedure cgGLDisableTextureParameter(param: PCGparameter); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLDisableTextureParameter}
function cgGLGetTextureEnum(param: PCGparameter): TGLenum; cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetTextureEnum}

procedure cgGLSetManageTextureParameters(ctx: PCGcontext; flag: TCGbool); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetManageTextureParameters}
function cgGLGetManageTextureParameters(ctx: PCGcontext): TCGbool; cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLGetManageTextureParameters}

procedure cgGLSetupSampler(param: PCGparameter; texobj: TGLuint); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLSetupSampler}
procedure cgGLRegisterStates(ctx: PCGcontext); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLRegisterStates}

procedure cgGLEnableProgramProfiles(program_: CGprogram); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLEnableProgramProfiles}
procedure cgGLDisableProgramProfiles(program_: CGprogram); cdecl; external CgGLlibrary;
{$EXTERNALSYM cgGLDisableProgramProfiles}

implementation

end.
