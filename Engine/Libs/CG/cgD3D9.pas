{******************************************************************************}
{*                                                                            *}
{*  Copyright (c) 2002, NVIDIA Corporation.                                   *}
{*                                                                            *}
{*  Files:    cgD3D9.h                                                        *}
{*  Content:  NVIDIA Cg Direct3D interface include files                      *}
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
// 12-Mar-05 - Alexey Barkovoy:
//   - Updated to Release 1.3 of Cg toolkit (published 18-Jan-2005)
// 21-Mar-03 - Alexey Barkovoy:
//   - Updated to Release 1.1 of Cg toolkit (published 04-Mar-2003)
// 11-Jan-03 - Alexey Barkovoy:
//   - Updated to Release 1.0 of Cg toolkit (published 20-Dec-2002)

{$Include JEDI.inc}

unit cgD3D9;

interface

uses
  Windows, Direct3D9, cg;

(*$HPPEMIT '#include "cgD3D9.h"' *)

const
  CgDX9library ={$IFDEF DEBUG} 'cgD3D9d.dll'{$ELSE} 'cgD3D9.dll'{$ENDIF};

type
  (*---------------------------------------------------------------------------
  // CGerrors that will be fed to cgSetError
  // Use cgD3D8TranslateCGerror() to translate these errors into strings.
  ---------------------------------------------------------------------------*)
  TcgD3D9Errors = DWORD;
  cgD3D9Errors = TcgD3D9Errors;
  {$NODEFINE cgD3D9Errors}
  {$NODEFINE TcgD3D9Errors}
  {$HPPEMIT 'typedef cgD3D9Errors TcgD3D9Errors;'}

const  
  cgD3D9Failed = 1000;
  {$EXTERNALSYM cgD3D9Failed}
  cgD3D9DebugTrace = 1001;
  {$EXTERNALSYM cgD3D9DebugTrace}

const
  (*---------------------------------------------------------------------------
  // HRESULTs specific to cgD3D9. When the CGerror is set to cgD3D9Failed
  // cgD3D9GetLastError will return an HRESULT that could be one these.
  // Use cgD3D9TranslateHRESULT() to translate these errors into strings.
  ---------------------------------------------------------------------------*)
  MAKE_CGD3DHRESULT_R     = (1 shl 31) or ($877 shl 16);

  CGD3D9ERR_NOTLOADED       = HRESULT(MAKE_CGD3DHRESULT_R or 1);
  {$EXTERNALSYM CGD3D9ERR_NOTLOADED}
  CGD3D9ERR_NODEVICE        = HRESULT(MAKE_CGD3DHRESULT_R or 2);
  {$EXTERNALSYM CGD3D9ERR_NODEVICE}
  CGD3D9ERR_NOTSAMPLER      = HRESULT(MAKE_CGD3DHRESULT_R or 3);
  {$EXTERNALSYM CGD3D9ERR_NOTSAMPLER}
  CGD3D9ERR_INVALIDPROFILE  = HRESULT(MAKE_CGD3DHRESULT_R or 4);
  {$EXTERNALSYM CGD3D9ERR_INVALIDPROFILE}
  CGD3D9ERR_NULLVALUE       = HRESULT(MAKE_CGD3DHRESULT_R or 5);
  {$EXTERNALSYM CGD3D9ERR_NULLVALUE}
  CGD3D9ERR_OUTOFRANGE      = HRESULT(MAKE_CGD3DHRESULT_R or 6);
  {$EXTERNALSYM CGD3D9ERR_OUTOFRANGE}
  CGD3D9ERR_NOTUNIFORM      = HRESULT(MAKE_CGD3DHRESULT_R or 7);
  {$EXTERNALSYM CGD3D9ERR_NOTUNIFORM}
  CGD3D9ERR_NOTMATRIX       = HRESULT(MAKE_CGD3DHRESULT_R or 8);
  {$EXTERNALSYM CGD3D9ERR_NOTMATRIX}
  CGD3D9ERR_INVALIDPARAM    = HRESULT(MAKE_CGD3DHRESULT_R or 9);
  {$EXTERNALSYM CGD3D9ERR_INVALIDPARAM}

(*---------------------------------------------------------------------------
// Other error return values
---------------------------------------------------------------------------*)
  CGD3D9_INVALID_REG = HRESULT($FF);
  {$EXTERNALSYM CGD3D9_INVALID_REG}

(*---------------------------------------------------------------------------
// Minimal Interface
---------------------------------------------------------------------------*)

function cgD3D9TypeToSize(_type: TCGtype): DWORD; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9TypeToSize}

function cgD3D9ResourceToDeclUsage(resource: TCGresource): DWORD; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9ResourceToDeclUsage}

type
  TVertexDeclaration9 = array[0..MAXD3DDECLLENGTH-1] of TD3DVertexElement9;

//todo: Check "decl: TVertexDeclaration9"
function cgD3D9GetVertexDeclaration(prog: PCGprogram;
  out decl: TVertexDeclaration9): TCGbool; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9GetVertexDeclaration}

function cgD3D9ValidateVertexDeclaration(prog: PCGprogram;
  const decl: PD3DVertexElement9): TCGbool; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9ValidateVertexDeclaration}

(*---------------------------------------------------------------------------
// Expanded Interface
---------------------------------------------------------------------------*)

(* ----- D3D Device Control ----------- *)
function cgD3D9GetDevice: IDirect3DDevice9;
{$EXTERNALSYM cgD3D9GetDevice}

function cgD3D9SetDevice(pDevice: IDirect3DDevice9): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetDevice}

(* ----- Shader Management ----------- *)
function cgD3D9LoadProgram(prog: PCGprogram;
    paramShadowing: TCGbool; assemFlags: DWORD): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9LoadProgram}

function cgD3D9UnloadProgram(prog: PCGprogram): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9UnloadProgram}

function cgD3D9IsProgramLoaded(prog: PCGprogram): TCGbool; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9IsProgramLoaded}

function cgD3D9BindProgram(prog: PCGprogram): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9BindProgram}

(* ----- Parameter Management ----------- *)
function cgD3D9SetUniform(param: PCGparameter;
    const floats: Pointer): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetUniform}

function cgD3D9SetUniformArray(param: PCGparameter; offset, numItems: DWORD;
    const values: Pointer): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetUniformArray}

function cgD3D9SetUniformMatrix(param: PCGparameter;
    const matrix: TD3DMatrix): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetUniformMatrix}

function cgD3D9SetUniformMatrixArray(param: PCGparameter; offset, numItems: DWORD;
    const matrices: PD3DMatrix): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetUniformMatrixArray}

function cgD3D9SetTexture(param: PCGparameter;
    tex: IDirect3DBaseTexture9): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetTexture}

function cgD3D9SetSamplerState(param: PCGparameter;
    _type: TD3DSamplerStateType; value: DWORD): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetSamplerState}

function cgD3D9SetTextureWrapMode(param: PCGparameter;
    value: DWORD): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetTextureWrapMode}

(* ----- Parameter Management (Shadowing) ----------- *)
function cgD3D9EnableParameterShadowing(prog: PCGprogram; enable: TCGbool): HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9EnableParameterShadowing}

function cgD3D9IsParameterShadowingEnabled(prog: PCGprogram): TCGbool; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9IsParameterShadowingEnabled}

(* --------- Profile Options ----------------- *)
function cgD3D9GetLatestVertexProfile: TCGprofile; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9GetLatestVertexProfile}

function cgD3D9GetLatestPixelProfile: TCGprofile; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9GetLatestPixelProfile}

function cgD3D9GetOptimalOptions(profile: TCGprofile): PPTERRAChar{ const }; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9GetOptimalOptions}

function cgD3D9IsProfileSupported(profile: TCGprofile): TCGbool; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9IsProfileSupported}

(* --------- Error reporting ----------------- *)
function cgD3D9GetLastError: HRESULT; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9GetLastError}

function cgD3D9TranslateCGerror(error: TCGerror): PTERRAChar{ const }; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9TranslateCGerror}

function cgD3D9TranslateHRESULT(hr: HRESULT): PTERRAChar{ const }; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9TranslateHRESULT}

procedure cgD3D9EnableDebugTracing(enable: TCGbool); cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9EnableDebugTracing}

(* --------- CgFX support -------------------- *)

procedure cgD3D9RegisterStates(ctx: CGcontext); cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9RegisterStates}

procedure cgD3D9SetupSampler(param: CGparameter; texture: IDirect3DBaseTexture9); cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetupSampler}

procedure cgD3D9SetManageTextureParameters(ctx: CGcontext; flag: TCGbool); cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetManageTextureParameters}

function cgD3D9GetManageTextureParameters(ctx: CGcontext): TCGbool; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9GetManageTextureParameters}

function cgD3D9GetTextureParameter(param: CGparameter): IDirect3DBaseTexture9; cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9GetTextureParameter}

procedure cgD3D9SetTextureParameter(param: CGparameter; tex: IDirect3DBaseTexture9); cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9SetTextureParameter}

procedure cgD3D9UnloadAllPrograms(); cdecl; external CgDX9library;
{$EXTERNALSYM cgD3D9UnloadAllPrograms}

implementation

function _cgD3D9GetDevice: Pointer; cdecl; external CgDX9library name 'cgD3D9GetDevice';

function cgD3D9GetDevice: IDirect3DDevice9;
begin
  Result:= IDirect3DDevice9(_cgD3D9GetDevice());
end;

end.
