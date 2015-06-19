{******************************************************************************}
{*                                                                            *}
{*  Copyright (c) 2002, NVIDIA Corporation.                                   *}
{*                                                                            *}
{*  Files:    cgD3D8.h                                                        *}
{*  Content:  NVIDIA Cg Direct3D interface include files                      *}
{*                                                                            *}
{*  NVIDIA "Cg" Release 1.4 ObjectPascal adaptation by Alexey Barkovoy        *}
{*  E-Mail: clootie@clootie.ru                                                *}
{*                                                                            *}
{*  Modified: 12-Jul-2005                                                     *}
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
// 21-Mar-03 - Alexey Barkovoy:
//   - Updated to Release 1.1 of Cg toolkit (published 04-Mar-2003)
// 11-Jan-03 - Alexey Barkovoy:
//   - Updated to Release 1.0 of Cg toolkit (published 20-Dec-2002)

unit cgD3D8;

interface

uses
  Windows, Direct3D8, D3DX8, cg;

(*$HPPEMIT '#include "cgD3D8.h"' *)

const
  CgDX8library ={$IFDEF DEBUG} 'cgD3D8d.dll'{$ELSE} 'cgD3D8.dll'{$ENDIF};

type
  (*---------------------------------------------------------------------------
  // CGerrors that will be fed to cgSetError
  // Use cgD3D8TranslateCGerror() to translate these errors into strings.
  ---------------------------------------------------------------------------*)
  TcgD3D8Errors = DWORD;
  cgD3D8Errors = TcgD3D8Errors;
  {$NODEFINE cgD3D8Errors}
  {$NODEFINE TcgD3D8Errors}
  {$HPPEMIT 'typedef cgD3D8Errors TcgD3D8Errors;'}

const
  cgD3D8Failed = 1000;
  {$EXTERNALSYM cgD3D8Failed}
  cgD3D8DebugTrace = 1001;
  {$EXTERNALSYM cgD3D8DebugTrace}

const
  (*---------------------------------------------------------------------------
  // HRESULTs specific to cgD3D8. When the CGerror is set to cgD3D8Failed
  // cgD3D8GetLastError will return an HRESULT that could be one these.
  // Use cgD3D8TranslateHRESULT() to translate these errors into strings.
  ---------------------------------------------------------------------------*)
  MAKE_CGD3DHRESULT_R     = (1 shl 31) or ($877 shl 16);

  CGD3D8ERR_NOTLOADED       = HRESULT(MAKE_CGD3DHRESULT_R or 1);
  {$EXTERNALSYM CGD3D8ERR_NOTLOADED}
  CGD3D8ERR_NODEVICE        = HRESULT(MAKE_CGD3DHRESULT_R or 2);
  {$EXTERNALSYM CGD3D8ERR_NODEVICE}
  CGD3D8ERR_NOTSAMPLER      = HRESULT(MAKE_CGD3DHRESULT_R or 3);
  {$EXTERNALSYM CGD3D8ERR_NOTSAMPLER}
  CGD3D8ERR_INVALIDPROFILE  = HRESULT(MAKE_CGD3DHRESULT_R or 4);
  {$EXTERNALSYM CGD3D8ERR_INVALIDPROFILE}
  CGD3D8ERR_NULLVALUE       = HRESULT(MAKE_CGD3DHRESULT_R or 5);
  {$EXTERNALSYM CGD3D8ERR_NULLVALUE}
  CGD3D8ERR_OUTOFRANGE      = HRESULT(MAKE_CGD3DHRESULT_R or 6);
  {$EXTERNALSYM CGD3D8ERR_OUTOFRANGE}
  CGD3D8ERR_NOTUNIFORM      = HRESULT(MAKE_CGD3DHRESULT_R or 7);
  {$EXTERNALSYM CGD3D8ERR_NOTUNIFORM}
  CGD3D8ERR_NOTMATRIX       = HRESULT(MAKE_CGD3DHRESULT_R or 8);
  {$EXTERNALSYM CGD3D8ERR_NOTMATRIX}
  CGD3D8ERR_INVALIDPARAM    = HRESULT(MAKE_CGD3DHRESULT_R or 9);
  {$EXTERNALSYM CGD3D8ERR_INVALIDPARAM}

  CGD3D8ERR_INVALIDSAMPLERSTATE       = HRESULT(MAKE_CGD3DHRESULT_R or 100);
  {$EXTERNALSYM CGD3D8ERR_INVALIDSAMPLERSTATE}
  CGD3D8ERR_INVALIDVEREXDECL          = HRESULT(MAKE_CGD3DHRESULT_R or 101);
  {$EXTERNALSYM CGD3D8ERR_INVALIDVEREXDECL}

(*---------------------------------------------------------------------------
// Other error return values
---------------------------------------------------------------------------*)
  CGD3D8_INVALID_REG = HRESULT($FFFFFFFF);
  {$EXTERNALSYM CGD3D8_INVALID_REG}

(*---------------------------------------------------------------------------
// Minimal Interface
---------------------------------------------------------------------------*)

function cgD3D8TypeToSize(_type: TCGtype): DWORD; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8TypeToSize}

function cgD3D8ResourceToInputRegister(resource: TCGresource): DWORD; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8ResourceToInputRegister}

function cgD3D8GetVertexDeclaration(prog: PCGprogram; out decl: TFVFDeclaration): TCGbool; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8GetVertexDeclaration}

function cgD3D8ValidateVertexDeclaration(prog: PCGprogram; const decl: PDWORD): TCGbool; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8ValidateVertexDeclaration}

(*---------------------------------------------------------------------------
// Expanded Interface
---------------------------------------------------------------------------*)

(* ----- D3D Device Control ----------- *)
function cgD3D8GetDevice: IDirect3DDevice8; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8GetDevice}

function cgD3D8SetDevice(pDevice: IDirect3DDevice8): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetDevice}

(* ----- Shader Management ----------- *)
function cgD3D8LoadProgram(prog: PCGprogram; paramShadowing: TCGbool; assemFlags: DWORD;
    vshaderUsage: DWORD; const vertexDecl: PDWORD): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8LoadProgram}

function cgD3D8UnloadProgram(prog: PCGprogram): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8UnloadProgram}

function cgD3D8IsProgramLoaded(prog: PCGprogram): TCGbool; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8IsProgramLoaded}

function cgD3D8BindProgram(prog: PCGprogram): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8BindProgram}

(* ----- Parameter Management ----------- *)
function cgD3D8SetUniform(param: PCGparameter;
    const floats: Pointer): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetUniform}

function cgD3D8SetUniformArray(param: PCGparameter; offset, numItems: DWORD;
    const values: Pointer): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetUniformArray}

function cgD3D8SetUniformMatrix(param: PCGparameter;
    const matrix: TD3DMatrix): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetUniformMatrix}

function cgD3D8SetUniformMatrixArray(param: PCGparameter; offset, numItems: DWORD;
    const matrices: PD3DMatrix): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetUniformMatrixArray}

function cgD3D8SetTexture(param: PCGparameter;
    tex: IDirect3DBaseTexture8): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetTexture}

function cgD3D8SetTextureStageState(param: PCGparameter;
    _type: TD3DTextureStageStateType; value: DWORD): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetTextureStageState}

function cgD3D8SetTextureWrapMode(param: PCGparameter;
    value: DWORD): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8SetTextureWrapMode}

(* ----- Parameter Management (Shadowing) ----------- *)
function cgD3D8EnableParameterShadowing(prog: PCGprogram; enable: TCGbool): HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8EnableParameterShadowing}

function cgD3D8IsParameterShadowingEnabled(prog: PCGprogram): TCGbool; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8IsParameterShadowingEnabled}

(* --------- Profile Options ----------------- *)
function cgD3D8GetLatestVertexProfile: TCGprofile; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8GetLatestVertexProfile}

function cgD3D8GetLatestPixelProfile: TCGprofile; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8GetLatestPixelProfile}

function cgD3D8GetOptimalOptions(profile: TCGprofile): PTERRAChar{ const }; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8GetOptimalOptions}

(* --------- Error reporting ----------------- *)
function cgD3D8GetLastError: HRESULT; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8GetLastError}

function cgD3D8TranslateCGerror(error: TCGerror): PTERRAChar{ const }; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8TranslateCGerror}

function cgD3D8TranslateHRESULT(hr: HRESULT): PTERRAChar{ const }; cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8TranslateHRESULT}

procedure cgD3D8EnableDebugTracing(enable: TCGbool); cdecl; external CgDX8library;
{$EXTERNALSYM cgD3D8EnableDebugTracing}

implementation

end.
