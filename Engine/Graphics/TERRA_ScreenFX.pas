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
 * TERRA_ScreenFX
 * Implements screen based shader effects
 ***********************************************************************************************************************
}
Unit TERRA_ScreenFX;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_OS, TERRA_Shader, TERRA_Matrix4x4, TERRA_Texture, TERRA_RenderTarget;

Const
  MaxVignetteScale = 20.0;

  FXColor   = 0;
  FXOffset  = 1;

Type
  ScreenFXChain = Class;

  ScreenFX = Class(TERRAObject)
    Protected
      _FXType:Integer;
      _Buffer:TERRAString;
      _RequireTarget:Array[0..Pred(MaxCaptureTargets)] Of Boolean;
      _Enabled:Boolean;
      _Owner:ScreenFXChain;

      Procedure Line(S2:TERRAString);
      Procedure RequireTarget(ID:Integer);
      Procedure SetEnabled(const Value: Boolean);

    Public
      Procedure SetupUniforms(Sh:Shader; Var TextureSlot:Integer); Virtual;

      Procedure GenerateUniforms(); Virtual;
      Procedure GenerateFunctions(); Virtual;
      Procedure GenerateCode(); Virtual;

      Property Enabled:Boolean Read _Enabled Write SetEnabled;
  End;

  ScreenFXClass = Class Of ScreenFX;

  ScreenFXChain = Class(TERRAObject)
    Protected
      _FXs:Array Of ScreenFX;
      _FXCount:Integer;
      _Shader:Shader;
      _NeedTarget:Array[0..Pred(MaxCaptureTargets)] Of Boolean;

      _NeedsUpdate:Boolean;

      Function GetShaderName():TERRAString;

    Public
      Constructor Create;
      Destructor Destroy; Override;

      Procedure Clear;

      Procedure OnContextLost;

      Function GetShader():Shader;
      Procedure DrawScreen(X1,Y1,X2,Y2:Single);

      Procedure AddEffect(FX:ScreenFX);
      Procedure RemoveEffect(FX:ScreenFX);

      Property EffectCount:Integer Read _FXCount;
  End;

  OutlineFX = Class(ScreenFX)
    Public
      LineWidth:Single;

      Constructor Create(Width:Single = 1.0);

      Procedure SetupUniforms(Sh:Shader; Var TextureSlot:Integer); Override;
      Procedure GenerateUniforms(); Override;
      Procedure GenerateFunctions(); Override;
      Procedure GenerateCode(); Override;
  End;

  ColorCorrectionFX  = Class(ScreenFX)
    Public
      Constrast:Single;
      Brightness:Single;
      Saturation:Single;

      Constructor Create(Constrast:Single = 1.0; Brightness:Single = 1.0; Saturation:Single = 1.0);
      Procedure SetupUniforms(Sh:Shader; Var TextureSlot:Integer); Override;
      Procedure GenerateUniforms(); Override;
      Procedure GenerateFunctions(); Override;
      Procedure GenerateCode(); Override;
  End;

  VignetteFX  = Class(ScreenFX)
    Protected
      _Scale:Single;

    Public
      Constructor Create(InitScale:Single = 10.0);

      Procedure SetScale(Value:Single);

      Procedure SetupUniforms(Sh:Shader; Var TextureSlot:Integer); Override;
      Procedure GenerateUniforms(); Override;
      Procedure GenerateFunctions(); Override;
      Procedure GenerateCode(); Override;

      Property Scale:Single Read _Scale Write SetScale;
  End;

  BloomFX  = Class(ScreenFX)
    Public
      Brightness:Single;

      Constructor Create(Brightness:Single=1.0);

      Procedure SetupUniforms(Sh:Shader; Var TextureSlot:Integer); Override;
      Procedure GenerateUniforms(); Override;
      Procedure GenerateCode(); Override;
  End;

  RefractionFX  = Class(ScreenFX)
    Public
      Strength:Single;

      Constructor Create(Strength:Single=0.02);

      Procedure SetupUniforms(Sh:Shader; Var TextureSlot:Integer); Override;
      Procedure GenerateUniforms(); Override;
      Procedure GenerateCode(); Override;
  End;

  UnderwaterFX  = Class(ScreenFX)
    Protected
      Strength:Single;
      Caustics:Texture;

    Public
      Constructor Create(CausticsTex:Texture; Strength:Single = 0.02);

      Procedure SetupUniforms(Sh:Shader; Var TextureSlot:Integer); Override;
      Procedure GenerateUniforms(); Override;
      Procedure GenerateCode(); Override;
  End;

Implementation
Uses TERRA_Log, TERRA_Viewport, TERRA_GraphicsManager;

{ ScreenFXChain }
Constructor ScreenFXChain.Create;
Begin
  Self._FXCount := 0;
  Self._Shader := Nil;
  Self._NeedsUpdate := True;
End;


Destructor ScreenFXChain.Destroy;
Begin
  Clear();
End;

Procedure ScreenFXChain.AddEffect(FX: ScreenFX);
Var
  I:Integer;
Begin
  If (FX = Nil) Then
    Exit;

  Log(logDebug, 'Graphics', 'Adding FX to chain: '+FX.ClassName);

  For I:=0 To Pred(_FXCount) Do
  If (_FXs[I].ClassType = FX.ClassType) Then
  Begin
    Log(logWarning, 'Graphics', 'Failed adding duplicated FX: '+FX.ClassName);
    Exit;
  End;

  _NeedsUpdate := True;
  Inc(_FXCount);
  SetLength(_FXs, _FXCount);
  _FXs[Pred(_FXCount)] := FX;

  FX._Enabled := True;
  FX._Owner := Self;
End;

Procedure ScreenFXChain.RemoveEffect(FX: ScreenFX);
Var
  I:Integer;
Begin
  I := 0;
  While (I<_FXCount) Do
  If (_FXs[I] = FX) Then
  Begin
    _FXs[I].Destroy;
    _FXs[I] := _FXs[Pred(_FXCount)];
    Dec(_FXCount);
    Break;
  End Else
    Inc(I);

  _NeedsUpdate := True;
End;

Procedure ScreenFXChain.Clear();
Var
  I:Integer;
Begin
  _NeedsUpdate := True;
  For I:=0 To Pred(_FXCount) Do
    _FXs[I].Destroy();
  _FXCount := 0;

  If Assigned(_Shader) Then
  Begin
    _Shader.Destroy;
    _Shader := Nil;
  End;
End;

Function ScreenFXChain.GetShaderName():TERRAString;
Var
  I:Integer;
Begin
  Result := 'screenfx';
  For I:=0 To Pred(_FXCount) Do
    Result := Result + '_'+_FXs[I].ClassName;
End;

Function ScreenFXChain.GetShader: Shader;
Var
  S:TERRAString;
  I, J:Integer;
  Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  If (_NeedsUpdate) Then
  Begin
    If Assigned(_Shader) Then
      _Shader.Destroy();

    For I:=0 To Pred(MaxCaptureTargets) Do
      Self._NeedTarget[I] := False;
    Self._NeedTarget[captureTargetColor] := True;

    S := '';
    Line('version { 110 }');
    Line('vertex {');
    Line('  uniform mat4 projectionMatrix;');
    Line('	varying mediump vec4 texCoord;');
    Line('  attribute highp vec4 terra_position;');
    Line('  attribute mediump vec4 terra_UV0;');
	  Line('  void main()	{');
    Line('  texCoord = terra_UV0;');
    Line('  gl_Position = projectionMatrix * terra_position;}');
    Line('}');
    Line('fragment {');
    //Line('  uniform mat4 inverseProjectionMatrix;');
	  Line('  uniform sampler2D diffuse_texture;');
    Line('	varying mediump vec4 texCoord;');

    For I:=0 To Pred(_FXCount) Do
    Begin
      _FXs[I]._Buffer := '';
      _FXs[I].GenerateUniforms();
      S := S + _FXs[I]._Buffer;

      For J:=0 To Pred(MaxCaptureTargets) Do
        Self._NeedTarget[J] := Self._NeedTarget[J] Or _FXs[I]._RequireTarget[J];
    End;

    If (Self._NeedTarget[captureTargetNormal]) Then
      Line('  uniform sampler2D normal_texture;');

    For I:=0 To Pred(_FXCount) Do
    If (_FXs[I].Enabled) Then
    Begin
      _FXs[I]._Buffer := '';
      _FXs[I].GenerateFunctions();
      S := S + _FXs[I]._Buffer;
    End;

	  Line('	void main()	{');
    Line('    lowp vec4 output_color; ');
    Line('    lowp vec2 output_uv = texCoord.xy; ');

    For I:=0 To Pred(_FXCount) Do
    If (_FXs[I]._FXType = FXOffset) And (_FXs[I].Enabled) Then
    Begin
      _FXs[I]._Buffer := '';
      _FXs[I].GenerateCode();
      S := S + _FXs[I]._Buffer;
    End;

    Line('    output_color = texture2D(diffuse_texture, output_uv);');

    For I:=0 To Pred(_FXCount) Do
    If (_FXs[I]._FXType = FXColor) And (_FXs[I].Enabled) Then
    Begin
      _FXs[I]._Buffer := '';
      _FXs[I].GenerateCode();
      S := S + _FXs[I]._Buffer;
    End;

    //Line('    gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);}');
    Line('    gl_FragColor = output_color;}');
    Line('}');

    _Shader := Shader.CreateFromString(S, Self.GetShaderName());
    _NeedsUpdate := False;
  End;

  Result := _Shader;
End;

Procedure ScreenFXChain.DrawScreen(X1,Y1,X2,Y2:Single);
Var
  _SH:Shader;
  I:Integer;
  M:Matrix4x4;
  Target:RenderTarget;
  Slot:Integer;
Begin
  _SH := Self.GetShader();

  ShaderManager.Instance.Bind(_SH);

  {M := GraphicsManager.Instance.ActiveViewport.Projection;
  M := MatrixInverse(M);
  _SH.SetUniform('inverseProjectionMatrix', M);
  }

  Slot := 0;
  For I:=0 To Pred(MaxCaptureTargets) Do
  If (Self._NeedTarget[I]) Then
  Begin
    Target := GraphicsManager.Instance.ActiveViewport.GetRenderTarget(I);
    If (Target = Nil) Then
      GraphicsManager.Instance.ActiveViewport.SetRenderTargetState(I, True)
    Else
    Begin
      Target.Bind(Slot);
      Target.BilinearFilter := True;
    End;

    _SH.SetUniform(TargetTextureNames[I], Slot);
    Inc(Slot);
  End;

  For I:=0 To Pred(_FXCount) Do
    _FXs[I].SetupUniforms(_SH, Slot);

  GraphicsManager.Instance.SetBlendMode(blendNone);
  GraphicsManager.Instance.DrawFullscreenQuad(_SH, X1,Y1,X2,Y2);
End;

Procedure ScreenFXChain.OnContextLost;
Begin
  If Assigned(_Shader) Then
    _Shader.OnContextLost();
End;

{ ScreenFX }
Procedure ScreenFX.GenerateCode;
Begin
  _Buffer := '';
End;

Procedure ScreenFX.GenerateFunctions;
Begin
  _Buffer := '';
End;

Procedure ScreenFX.GenerateUniforms;
Begin
  _Buffer := '';
End;

Procedure ScreenFX.Line(S2:TERRAString);
Begin
  _Buffer := _Buffer + S2 + crLf;
End;

Procedure ScreenFX.RequireTarget(ID: Integer);
Begin
  If (ID>=0) And (ID<MaxCaptureTargets) Then
    _RequireTarget[ID] := True;
End;

Procedure ScreenFX.SetEnabled(const Value: Boolean);
Begin
  If (Self._Enabled = Value) Then
    Exit;

  Self._Enabled := Value;

  If Assigned(Self._Owner) Then
    Self._Owner._NeedsUpdate := True;
End;

Procedure ScreenFX.SetupUniforms(Sh:Shader; Var TextureSlot:Integer);
Begin
  // do nothing
End;

{ OutlineFX }
Constructor OutlineFX.Create(Width:Single);
Begin
  Self._FXType := FXColor;
  Self.LineWidth := Width;
End;

Procedure OutlineFX.GenerateFunctions;
Begin

  Line('lowp float sobelFilter(lowp vec2 texCoord){');
  Line('mediump vec3 sample;');
  //Line('lowp float depth = texture2D(normal_texture, texCoord).a;');

  //Line(' depth = (1.0 - depth) * 0.5;');
  Line(' mediump float kox = offX; ');
  Line(' mediump float koy = offY; ');
                                         
// fetch the 3x3 neighbourhood and use the RGB vector's length as intensity value
  Line('  lowp float spv0;');
  Line('  lowp float spv1;');
  Line('  lowp float spv2;');
  Line(' spv0 = length(texture2D(normal_texture, texCoord + vec2(-kox,-koy)).rgb);');
  Line(' spv1 = length(texture2D(normal_texture, texCoord + vec2(-kox, 0.0)).rgb);');
  Line(' spv2 = length(texture2D(normal_texture, texCoord + vec2(-kox, koy)).rgb);');
  Line(' lowp vec3 sample0 = vec3(spv0, spv1, spv2);');
  Line(' spv0 = length(texture2D(normal_texture, texCoord + vec2(0.0, -koy)).rgb);');
  Line(' spv1 = length(texture2D(normal_texture, texCoord).rgb);');
  Line(' spv2 = length(texture2D(normal_texture, texCoord + vec2(0.0, koy)).rgb);');
  Line(' lowp vec3 sample1 = vec3(spv0, spv1, spv2);');
  Line(' spv0 = length(texture2D(normal_texture, texCoord + vec2(kox,-koy)).rgb);');
  Line(' spv1 = length(texture2D(normal_texture, texCoord + vec2(kox, 0.0)).rgb);');
  Line(' spv2 = length(texture2D(normal_texture, texCoord + vec2(kox, koy)).rgb);');
  Line(' lowp vec3 sample2 = vec3(spv0, spv1, spv2);');

// calculate the convolution values for all the masks
	// calculate the convolution values for all the masks
  Line('lowp float conv0;');
  Line('lowp float conv1;');
  Line('mediump float dp3;');
  Line(' mediump vec3 gk0_0 = vec3(1.0, 2.0, 1.0); ');
  Line(' mediump vec3 gk0_2 = vec3(-1.0, -2.0, -1.0); ');
  Line(' mediump vec3 gk1_0 = vec3(1.0, 0.0, -1.0); ');
  Line(' mediump vec3 gk1_1 = vec3(2.0, 0.0, -2.0); ');
  Line(' mediump vec3 gk1_2 = vec3(1.0, 0.0, -1.0); ');
  Line(' dp3 =  dot(gk0_0, sample0) +  dot(gk0_2, sample2) ;');
  Line(' conv0 = dp3 * dp3;	');
  Line(' dp3 =  dot(gk1_0, sample0)  +  dot(gk1_1, sample1)  +  dot(gk1_2, sample2) ;');
  Line(' conv1 = dp3 * dp3;	');
  Line(' mediump float pp = sqrt(conv0*conv0+conv1*conv1);');

(*
  Line('mat3 G[2];');
  Line('G[0] = mat3( 1.0, 2.0, 1.0, 0.0, 0.0, 0.0, -1.0, -2.0, -1.0 );');
  Line('G[1] = mat3( 1.0, 0.0, -1.0, 2.0, 0.0, -2.0, 1.0, 0.0, -1.0 );');
  Line('float cnv[2];');
	Line('for (int i=0; i<2; i++) {');
	Line('	float dp3 = dot(G[i][0], samples[0]) + dot(G[i][1], samples[1]) + dot(G[i][2], samples[2]);');
	Line('	cnv[i] = dp3 * dp3;	}');
  Line('  mediump float pp = sqrt(cnv[0]*cnv[0]+cnv[1]*cnv[1]);');*)

  Line('	pp = 1.0 - min(1.0, 0.25 * pp);');

//  Line('  pp = pp * 0.3 + 0.7;');
  //Line('	if (pp<0.99) return 0.0;  else return 1.0; 	}');
	Line('	return pp;}');

(*	Line('float sobelFilter(vec2 texCoord){');
	Line('	vec3 s00 = texture2D(normal_texture, texCoord + vec2(-offX, -offY)).rgb;');
	Line('	vec3 s01 = texture2D(normal_texture, texCoord + vec2( 0,   -offY)).rgb;');
	Line('	vec3 s02 = texture2D(normal_texture, texCoord + vec2( offX, -offY)).rgb;');

	Line('	vec3 s10 = texture2D(normal_texture, texCoord + vec2(-offX,  0)).rgb;');
	Line('	vec3 s12 = texture2D(normal_texture, texCoord + vec2( offX,  0)).rgb;');

	Line('	vec3 s20 = texture2D(normal_texture, texCoord + vec2(-offX,  offY)).rgb;');
	Line('	vec3 s21 = texture2D(normal_texture, texCoord + vec2( 0,    offY)).rgb;');
	Line('	vec3 s22 = texture2D(normal_texture, texCoord + vec2( offX,  offY)).rgb;');

	Line('	vec3 sobelX = s00 + 2.0 * s10 + s20 - s02 - 2.0 * s12 - s22;');
	Line('	vec3 sobelY = s00 + 2.0 * s01 + s02 - s20 - 2.0 * s21 - s22;');

	Line('	vec3 edgeSqr = sobelX * sobelX + sobelY * sobelY;');
  Line('  vec4 px = texture2D(normal_texture, texCoord).rgba;');
	Line('	float p = dot(edgeSqr, edgeSqr);');


//  Line('	return px.a;	}');

	//Line('	return p * 0.3 + 0.7;	}');*)
End;

Procedure OutlineFX.GenerateUniforms;
Begin
	Line('uniform lowp float offX;');
	Line('uniform lowp float offY;');
//  Line('uniform mediump float outlineWidth;');
  Line('uniform sampler2D outline_texture;');
  Self.RequireTarget(captureTargetNormal);
  Self.RequireTarget(captureTargetOutline);
End;

Procedure OutlineFX.SetupUniforms(Sh: Shader; Var TextureSlot:Integer);
Var
  View:Viewport;
Begin
  View := GraphicsManager.Instance.ActiveViewport;
  Sh.SetUniform('offX', (1 / View.Width) {* LineWidth});
  Sh.SetUniform('offY', (1 / View.Height) {* LineWidth});
  //Sh.SetUniform('outlineWidth', LineWidth);

  GraphicsManager.Instance.ActiveViewport.BindStageTexture(captureTargetOutline, TextureSlot);
  Sh.SetUniform('outline_texture', TextureSlot);
  Inc(TextureSlot);
End;

Procedure OutlineFX.GenerateCode;
Begin
  Line('  lowp vec4 outline_color = texture2D(outline_texture, output_uv); ');
  //Line('  lowp vec4 outline_color = vec4(1.0, 0.0, 0.0 , 1.0); ');
  Line('  lowp float edgeValue = sobelFilter(output_uv);');
  //Line('  output_color *= edgeValue;');

  Line('  outline_color.rgb = mix(outline_color.rgb, output_color.rgb, edgeValue);');
  Line('  output_color.rgb = mix(output_color.rgb, outline_color.rgb, outline_color.a);');

  //Line('  output_color.rgb = vec3(edgeValue);');
End;

{ ColorCorrectionFX }
Constructor ColorCorrectionFX.Create(Constrast, Brightness, Saturation:Single);
Begin
  Self._FXType := FXColor;

  Self.Constrast := Constrast;
  Self.Brightness := Brightness;
  Self.Saturation := Saturation;
End;

Procedure ColorCorrectionFX .GenerateCode;
Begin
  Line('  output_color.rgb = ContrastSaturationBrightness(output_color.rgb);');
End;

procedure ColorCorrectionFX .GenerateFunctions;
begin
	// For all settings: 1.0 = 100% 0.5=50% 1.5 = 150%
	Line('  const lowp vec3 LumCoeff = vec3(0.2125, 0.7154, 0.0721);');
	Line('  const lowp float middleGray = 0.5;');
	Line('lowp vec3 ContrastSaturationBrightness(lowp vec3 color)	{');
  Line('  lowp vec3 AvgLumin = vec3(0.5);');
  Line('  lowp vec3 brtColor = color * mod_brightness;');
  Line('  lowp vec3 intensity = vec3(dot(brtColor, LumCoeff));');
  Line('  lowp vec3 satColor = mix(intensity, brtColor, mod_saturation);');
  Line('  lowp vec3 conColor = mix(AvgLumin, satColor, mod_constrast);');
  Line('return conColor;	}');
End;

Procedure ColorCorrectionFX .GenerateUniforms;
Begin
	Line('uniform mediump float mod_constrast;');
	Line('uniform mediump float mod_brightness;');
	Line('uniform mediump float mod_saturation;');
End;

Procedure ColorCorrectionFX.SetupUniforms(Sh: Shader; Var TextureSlot:Integer);
Begin
  Sh.SetUniform('mod_constrast', Constrast);
  Sh.SetUniform('mod_brightness', Brightness);
  Sh.SetUniform('mod_saturation', Saturation);
End;

{ VignetteFX }
Constructor VignetteFX.Create;
Begin
  Self._FXType := FXColor;
  Self.Scale := InitScale;
End;

Procedure VignetteFX.GenerateCode;
Begin
  Line('output_color.rgb *= Vignette(output_uv);');
End;

Procedure VignetteFX.GenerateFunctions;
Begin
	Line('lowp float Vignette(lowp vec2 st)	{');
  Line('st -= 0.5;');
  Line('lowp float vignette = 1.0 - dot(st, st);');
  Line('return (pow( vignette, vignetteScale)+0.05);}');
End;

Procedure VignetteFX.GenerateUniforms;
Begin
  Line('uniform mediump float vignetteScale;');
End;

Procedure VignetteFX.SetScale(Value: Single);
Begin
  If (Value<0) Then
    Value := 0;
  If (Value>MaxVignetteScale) Then
    Value := MaxVignetteScale;
  Self._Scale := Value;
End;

Procedure VignetteFX.SetupUniforms(Sh: Shader; Var TextureSlot:Integer);
Begin
  Sh.SetUniform('vignetteScale', Self.Scale);
End;

{ BloomFX }
Constructor BloomFX.Create(Brightness:Single);
Begin
  Self._FXType := FXColor;
  Self.Brightness := Brightness;
End;

Procedure BloomFX.GenerateCode;
Begin
  Inherited;
  Line('  output_color.rgb += bloomBightness * texture2D(bloom_texture, output_uv).rgb;');
End;

Procedure BloomFX.GenerateUniforms;
Begin
  Inherited;
  Line('uniform lowp float bloomBightness;');
  Line('uniform sampler2D bloom_texture;');
End;

Procedure BloomFX.SetupUniforms(Sh: Shader; Var TextureSlot:Integer);
Begin
  Inherited;
  GraphicsManager.Instance.ActiveViewport.BindBloomTexture(TextureSlot);
  Sh.SetUniform('bloom_texture', TextureSlot);
  Sh.SetUniform('bloomBightness', Self.Brightness);
  Inc(TextureSlot);
End;

{ RefractionFX }
Constructor RefractionFX.Create(Strength: Single);
Begin
  Self._FXType := FXColor;
  Self.Strength := Strength;
End;

Procedure RefractionFX.GenerateCode;
Begin
  Inherited;
  Line('  lowp vec4 refc = texture2D(refraction_texture, output_uv); ');
  Line('  output_uv = output_uv + refc.xy * refraction_strength;');
  Line('  lowp vec4 samprf = texture2D(diffuse_texture, output_uv); ');
  Line('  output_color.rgb = output_color.rgb * (1.0-refc.a) + samprf.rgb * refc.a;');
End;

Procedure RefractionFX.GenerateUniforms;
Begin
  Inherited;
  Line('uniform lowp float refraction_strength;');
  Line('uniform sampler2D refraction_texture;');
//  Line('const lowp float refraction_saturation = 1.2;');
  Line('const lowp vec4 c1 = vec4( 1.0, 0.0, 0.0, 0.0 );');
  Line('const lowp vec4 c2 = vec4( 0.0, 0.0, 1.0, 0.0 );');
End;

Procedure RefractionFX.SetupUniforms(Sh: Shader; Var TextureSlot:Integer);
Begin
  Inherited;
  GraphicsManager.Instance.ActiveViewport.BindStageTexture(captureTargetRefraction, TextureSlot);
  Sh.SetUniform('refraction_texture', TextureSlot);
  Sh.SetUniform('refraction_strength', Strength);
  Inc(TextureSlot);
End;

{ UnderwaterFX }
Constructor UnderwaterFX.Create(CausticsTex:Texture; Strength:Single);
Begin
  Self._FXType := FXOffset;
  Self.Strength := Strength;

  If CausticsTex = Nil Then
    CausticsTex := TextureManager.Instance.NullTexture;
      
  Self.Caustics := CausticsTex;
End;

Procedure UnderwaterFX.GenerateUniforms;
Begin
  Inherited;
  Line('uniform sampler2D caustics_texture;');
  Line('uniform lowp float caustics_strength;');
End;

Procedure UnderwaterFX.SetupUniforms(Sh: Shader; var TextureSlot: Integer);
Begin
  Inherited;
  Self.Caustics.Bind(TextureSlot);
  Sh.SetUniform('caustics_texture', TextureSlot);
  Sh.SetUniform('caustics_strength', Self.Strength);
  Inc(TextureSlot);
End;

Procedure UnderwaterFX.GenerateCode;
Begin
  Inherited;
  Line('  lowp vec4 caustic_ofs = texture2D(caustics_texture, output_uv); ');
  Line('  output_uv += vec2(caustic_ofs.r * caustics_strength, caustic_ofs.g * caustics_strength * -0.5);');
End;

End.
