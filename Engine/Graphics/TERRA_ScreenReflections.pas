{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
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
 * TERRA_SSAO
 * Implements screen-space screen occlusion effect
 ***********************************************************************************************************************
}
Unit TERRA_ScreenReflections;

Interface
Uses TERRA_Utils, TERRA_ScreenFX, TERRA_Texture, TERRA_Stream, TERRA_Renderer, TERRA_FileManager;

Type
  ScreenReflectionsFX = Class(ScreenFX)
      _Strength:Integer;

      Function RequiresTarget(TargetType:RenderTargetType):Boolean; Override;
      Function RequiresFunction(FXFunction:ScreenFXFunctionType):Boolean; Override;

      Function GetStrength: Single;
      Procedure SetStrength(const Value: Single);

    Public
      Constructor Create(Strength:Single);

      Procedure GenerateFunctions(); Override;
      Procedure GenerateCode(); Override;

      Property Strength:Single Read GetStrength Write SetStrength;
  End;


Implementation


{ ScreenReflectionsFX }
Constructor ScreenReflectionsFX.Create(Strength: Single);
Begin
  _Strength := Self.AddUniform('mod_snow', uniformFloat);
  Self.SetStrength(Strength);
End;

Function ScreenReflectionsFX.RequiresFunction(FXFunction: ScreenFXFunctionType): Boolean;
Begin
  Result := (FXFunction = fxCellularNoise);
End;

Function ScreenReflectionsFX.GetStrength: Single;
Begin
  Self.GetFloat(_Strength, Result);
End;

Procedure ScreenReflectionsFX.SetStrength(const Value: Single);
Begin
  Self.SetFloat(_Strength, Value);
End;

Procedure ScreenReflectionsFX.GenerateCode;
Begin
	Line('vec3 reflectedColor = vec3(0.0f);');
  Line('vec4 tempVal = texture2D(normal_texture, output_uv.xy);');
  Line('vec3 normal = (tempVal.xyz * 2.0) - 1.0;
  Line('normal = normalize(normal);');

	// Depth at current fragment
	Line('float currDepth = tempVal.w;');

	// Eye position, camera is at (0, 0, 0), we look along negative z, add near plane to correct parallax
	vec3 eyePosition = normalize( vec3(0, 0, Camera.NearPlane) );
	vec4 reflectionVector = ProjectionMatrix * reflect( vec4(-eyePosition, 0), vec4(normal, 0) ) ;

        // Call raytrace to get reflected color
	reflectedColor = raytrace(reflectionVector.xyz, currDepth);


  Line('  output_color.rgb += SSR(output_uv.xy);');
  //Line('  output_color.rgb = vec3(cellular2x2(output_uv.xy * 10.0));');
End;

Procedure ScreenReflectionsFX.GenerateFunctions;
Begin
vec3 raytrace(in vec3 reflectionVector, in float startDepth)
{
	vec3 color = vec3(0.0f);
	float stepSize = rayStepSize; 
 
	float size = length(reflectionVector.xy);
	reflectionVector = normalize(reflectionVector/size);
	reflectionVector = reflectionVector * stepSize;
        
        // Current sampling position is at current fragment
	vec2 sampledPosition = vert_UV;
        // Current depth at current fragment
	float currentDepth = startDepth;
        // The sampled depth at the current sampling position
	float sampledDepth = linearizeDepth( texture(deferredDepthTex, sampledPosition).z );
 
        // Raytrace as long as in texture space of depth buffer (between 0 and 1)
	while(sampledPosition.x <= 1.0 && sampledPosition.x >= 0.0 &&
	      sampledPosition.y <= 1.0 && sampledPosition.y >= 0.0)
	{
                // Update sampling position by adding reflection vector's xy and y components
		sampledPosition = sampledPosition + reflectionVector.xy;
                // Updating depth values
		currentDepth = currentDepth + reflectionVector.z * startDepth;
		float sampledDepth = linearizeDepth( texture(deferredDepthTex, sampledPosition).z );
                
                // If current depth is greater than sampled depth of depth buffer, intersection is found
		if(currentDepth > sampledDepth)
		{
                        // Delta is for stop the raytracing after the first intersection is found
                        // Not using delta will create "repeating artifacts"
			float delta = (currentDepth - sampledDepth);
			if(delta < 0.003f )
			{
				color = texture(deferredDiffuseTex, sampledPosition).rgb;
				break;
			}
		}
	}
 
	return color;}
End;

Function ScreenReflectionsFX.RequiresTarget(TargetType: RenderTargetType): Boolean;
Begin
  Result := (TargetType = captureTargetReflection) Or (TargetType = captureTargetNormal);
End;

End.
