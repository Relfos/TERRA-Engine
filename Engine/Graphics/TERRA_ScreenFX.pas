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
Uses TERRA_String, TERRA_Utils, TERRA_OS, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Matrix4x4, TERRA_Color,
  TERRA_Texture, TERRA_Renderer, TERRA_Noise;

Const
  MaxVignetteScale = 20.0;

  FXColor   = 0;
  FXOffset  = 1;

Const
  TargetNames:Array[0..Pred(TotalCaptureTargets)] Of TERRAString =
  ('alpha', 'diffuse', 'normal', 'emission', 'refraction', 'outline', 'reflection', 'shadow', 'position', 'glow', 'bloom', 'edge');

Var
  TargetTextureNames:Array[0..Pred(TotalCaptureTargets)] Of TERRAString;

Const
  MaxScreenFXFunctions = 5;

Type
  ScreenFXFunctionType = (
    fxRGBToHSL = 0,
    fxHSLToRGB = 1,
    fxColorGrading = 2,
    fxGreyScale = 3,
    fxCellularNoise = 4
  );

  UniformType = (
    uniformTexture = 0,
    uniformPalette = 1,
    uniformColor =2,
    uniformFloat =3,
    uniformVec2 = 4,
    uniformVec3 = 5,
    uniformVec4 = 6
  );

  ScreenFXChain = Class;

  ScreenFXUniform = Record
    Name:TERRAString;
    Kind:UniformType;
    Value:Vector4D;
    Tex:Texture;
    Initialized:Boolean;
  End;

  ScreenFX = Class(TERRAObject)
    Protected
      _FXType:Integer;
      _Buffer:TERRAString;
      _Enabled:Boolean;
      _Owner:ScreenFXChain;

      _Uniforms:Array Of ScreenFXUniform;
      _UniformCount:Integer;

      Procedure Line(S2:TERRAString);
      Procedure SetEnabled(const Value: Boolean);

      Function RequiresTarget(TargetType:RenderTargetType):Boolean; Virtual;
      Function RequiresFunction(FXFunction:ScreenFXFunctionType):Boolean; Virtual;

      Procedure GenerateUniforms();
      Procedure GenerateFunctions(); Virtual;
      Procedure GenerateCode(); Virtual;

      Procedure SetupUniforms(Sh:ShaderInterface; Var TextureSlot:Integer);

      Function AddUniform(Const Name:TERRAString; Kind:UniformType):Integer;

    Public
      Procedure SetTexture(Index:Integer; Value:Texture);
      Procedure SetColor(Index:Integer; Const Value:Color);
      Procedure SetFloat(Index:Integer; Const Value:Single);
      Procedure SetVec2(Index:Integer; Const Value:Vector2D);
      Procedure SetVec3(Index:Integer; Const Value:Vector3D);
      Procedure SetVec4(Index:Integer; Const Value:Vector4D);

      Procedure GetTexture(Index:Integer; Out Value:Texture);
      Procedure GetColor(Index:Integer; Out Value:Color);
      Procedure GetFloat(Index:Integer; Out Value:Single);
      Procedure GetVec2(Index:Integer; Out Value:Vector2D);
      Procedure GetVec3(Index:Integer; Out Value:Vector3D);
      Procedure GetVec4(Index:Integer; Out Value:Vector4D);

      Property Enabled:Boolean Read _Enabled Write SetEnabled;
  End;

  ScreenFXClass = Class Of ScreenFX;

  ScreenFXChain = Class(TERRAObject)
    Protected
      _FXs:Array Of ScreenFX;
      _FXCount:Integer;
      _Shader:ShaderInterface;
      _NeedTarget:Array[0..Pred(TotalCaptureTargets)] Of Boolean;
      _NeedFunction:Array[0..Pred(MaxScreenFXFunctions)] Of Boolean;

      _NeedsUpdate:Boolean;
      _Antialias:Boolean;

      Function GetShaderName():TERRAString;

      Function GetShader():ShaderInterface;

      Procedure SetAntiAlias(const Value: Boolean);

    Public
      Constructor Create;
      Procedure Release; Override;

      Procedure Clear;

      Procedure OnContextLost;

      Procedure DrawScreen(X1,Y1,X2,Y2:Single);

      Procedure AddEffect(FX:ScreenFX);
      Procedure RemoveEffect(FX:ScreenFX);

      Property Shader:ShaderInterface Read GetShader;
      Property EffectCount:Integer Read _FXCount;

      Property AntiAlias:Boolean Read _Antialias Write SetAntiAlias;
  End;

  OutlineFX = Class(ScreenFX)
    Protected
      Function RequiresTarget(TargetType:RenderTargetType):Boolean; Override;

    Public
      LineWidth:Single;

      Constructor Create(Width:Single = 1.0);

      Procedure GenerateCode(); Override;
  End;

  ColorCorrectionFX  = Class(ScreenFX)
    Protected
      _Brightness:Integer;
      _Contrast:Integer;
      _Saturation:Integer;

      Function RequiresFunction(FXFunction: ScreenFXFunctionType): Boolean; Override;

      Function GetBrightness: Single;
      Function GetContrast: Single;
      Function GetSaturation: Single;

      Procedure SetBrightness(const Value: Single);
      Procedure SetContrast(const Value: Single);
      Procedure SetSaturation(const Value: Single);
      
    Public
      Constructor Create(Contrast:Single = 1.0; Brightness:Single = 1.0; Saturation:Single = 1.0);

      Procedure GenerateFunctions(); Override;
      Procedure GenerateCode(); Override;

      Property Constrast:Single Read GetContrast Write SetContrast;
      Property Brightness:Single Read GetBrightness Write SetBrightness;
      Property Saturation:Single Read GetSaturation Write SetSaturation;
  End;

  ColorGradingFX  = Class(ScreenFX)
    Protected
      _Palette:Integer;

      Function RequiresFunction(FXFunction:ScreenFXFunctionType):Boolean; Override;

    Public

      Constructor Create(Palette:Texture);
      Procedure GenerateCode(); Override;
  End;

  VibranceFX = Class(ScreenFX)
    Protected
      _Strength:Integer;
      _Ramp:Texture;

      Function RequiresFunction(FXFunction:ScreenFXFunctionType):Boolean; Override;

      Function GetStrength: Single;
      Procedure SetStrength(const Value: Single);

    Public
      Constructor Create(Strength:Single);
      Procedure Release; Override;

      Procedure GenerateFunctions(); Override;
      Procedure GenerateCode(); Override;

      Property Strength:Single Read GetStrength Write SetStrength;
  End;

  VignetteFX  = Class(ScreenFX)
    Protected
      _Scale:Integer;

      Function GetScale():Single;
      Procedure SetScale(Value:Single);

    Public
      Constructor Create(InitScale:Single = 10.0);

      Procedure GenerateFunctions(); Override;
      Procedure GenerateCode(); Override;

      Property Scale:Single Read GetScale Write SetScale;
  End;

  GlowFX  = Class(ScreenFX)
    Protected
      _Strength:Integer;

      Procedure SetStrength(Value:Single);
      Function GetStrength():Single;

      Function RequiresTarget(TargetType:RenderTargetType):Boolean; Override;

    Public

      Constructor Create(Strength:Single=1.0);

      Procedure GenerateCode(); Override;

      Property Strength:Single Read GetStrength Write SetStrength;
  End;

  BloomFX  = Class(ScreenFX)
    Protected
      _Strength:Integer;

      Procedure SetStrength(Value:Single);
      Function GetStrength():Single;

      Function RequiresTarget(TargetType:RenderTargetType):Boolean; Override;

    Public

      Constructor Create(Strength:Single);

      Procedure GenerateCode(); Override;

      Property Strength:Single Read GetStrength Write SetStrength;
  End;

  DitherFX  = Class(ScreenFX)
    Protected
      _Pattern:Integer;
      _Palette:Integer;

      Function RequiresTarget(TargetType:RenderTargetType):Boolean; Override;

    Public

      Constructor Create(Pattern, Palette:Texture);

      Procedure GenerateCode(); Override;
      Procedure GenerateFunctions(); Override;
  End;

  RefractionFX  = Class(ScreenFX)
    Protected
      _Strength:Integer;

      Procedure SetStrength(Value:Single);
      Function GetStrength():Single;

      Function RequiresTarget(TargetType: RenderTargetType): Boolean; Override;

    Public

      Constructor Create(Strength:Single);

      Procedure GenerateCode(); Override;

      Property Strength:Single Read GetStrength Write SetStrength;
  End;

  UnderwaterFX  = Class(ScreenFX)
    Protected
      _Strength:Integer;
      _Caustics:Integer;

      Procedure SetStrength(Value:Single);
      Function GetStrength():Single;

    Public
      Constructor Create(CausticsTex:Texture; Strength:Single = 0.02);

      Procedure GenerateCode(); Override;

      Property Strength:Single Read GetStrength Write SetStrength;
  End;

Implementation
Uses TERRA_Log, TERRA_Error, TERRA_Math, TERRA_Image, TERRA_GraphicsManager, TERRA_ColorGrading, TERRA_Viewport;

{ ScreenFXChain }
Constructor ScreenFXChain.Create;
Begin
  Self._FXCount := 0;
  Self._Shader := Nil;
  Self._NeedsUpdate := True;
End;

Procedure ScreenFXChain.Release;
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
    _FXs[I].Release;
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
    _FXs[I].Release();
  _FXCount := 0;

  ReleaseObject(_Shader);
End;

Function ScreenFXChain.GetShaderName():TERRAString;
Var
  I:Integer;
Begin
  Result := 'screenfx';
  If Self.AntiAlias Then
    Result := Result + '_AA';
    
  For I:=0 To Pred(_FXCount) Do
    Result := Result + '_'+_FXs[I].ClassName;
End;

Function ScreenFXChain.GetShader:ShaderInterface;
Var
  S:TERRAString;
  I, J:Integer;
  Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  If (_NeedsUpdate) Then
  Begin
    ReleaseObject(_Shader);

    For I:=0 To Pred(TotalCaptureTargets) Do
      Self._NeedTarget[I] := False;

    For I:=0 To Pred(MaxScreenFXFunctions) Do
      Self._NeedFunction[I] := False;
        
    For I:=0 To Pred(TotalCaptureTargets) Do
    Begin
      For J:=0 To Pred(_FXCount) Do
      If (_FXs[J].Enabled) And (_FXs[J].RequiresTarget(RenderTargetType(I))) Then
      Begin
        Self._NeedTarget[I] := True;
        Break;
      End;
    End;

    For I:=0 To Pred(MaxScreenFXFunctions) Do
    Begin
      For J:=0 To Pred(_FXCount) Do
      If (_FXs[J].Enabled) And (_FXs[J].RequiresFunction(ScreenFXFunctionType(I))) Then
      Begin
        Self._NeedFunction[I] := True;
        Break;
      End;
    End;

    Self._NeedTarget[Integer(captureTargetColor)] := True;
    {$IFDEF ADVANCED_ALPHA_BLEND}
    Self._NeedTarget[Integer(captureTargetAlpha)] := True;
    {$ENDIF}

    S := '';
    Line('version { 110 }');
    Line('vertex {');
    Line('  uniform mat4 projectionMatrix;');
    Line('	varying mediump vec4 texCoord;');
    Line('  attribute highp vec4 terra_position;');
    Line('  attribute mediump vec4 terra_UV0;');

    If (Self.AntiAlias) Then
    Begin
      Line('	varying mediump vec2 v_rgbNW;');
      Line('	varying mediump vec2 v_rgbNE;');
      Line('	varying mediump vec2 v_rgbSW;');
      Line('	varying mediump vec2 v_rgbSE;');
      Line('	varying mediump vec2 v_rgbM;');

      Line('	uniform vec2 screenResolution;');
    End;

	  Line('  void main()	{');
    Line('  texCoord = terra_UV0;');

    If (Self.AntiAlias) Then
    Begin
      Line('  mediump vec2 inverseVP = vec2(1.0 / screenResolution.x, 1.0 / screenResolution.y);');
    	Line('  mediump vec2 fragCoord = texCoord.xy * screenResolution;');
    	Line('  v_rgbNW = (fragCoord + vec2(-1.0, -1.0)) * inverseVP;');
    	Line('  v_rgbNE = (fragCoord + vec2(1.0, -1.0)) * inverseVP;');
    	Line('  v_rgbSW = (fragCoord + vec2(-1.0, 1.0)) * inverseVP;');
    	Line('  v_rgbSE = (fragCoord + vec2(1.0, 1.0)) * inverseVP;');
    	Line('  v_rgbM = vec2(fragCoord * inverseVP);');
    End;

    Line('  gl_Position = projectionMatrix * terra_position;}');
    Line('}');
    Line('fragment {');

    //Line('  uniform mat4 inverseProjectionMatrix;');
    Line('	varying mediump vec4 texCoord;');

    Line('	uniform lowp vec2 screenResolution;');
    Line('	uniform lowp float globalTime;');

    For I:=0 To Pred(TotalCaptureTargets) Do
    If (Self._NeedTarget[I]) Then
    Begin
      Line('uniform sampler2D '+TargetTextureNames[I]+';');
    End;

    For I:=0 To Pred(_FXCount) Do
    If (_FXs[I].Enabled) Then
    Begin
      _FXs[I]._Buffer := '';
      _FXs[I].GenerateUniforms();
      S := S + _FXs[I]._Buffer;
    End;

    If (Self._NeedTarget[Integer(captureTargetNormal)]) Then
      Line('  uniform sampler2D normal_texture;');

    If (Self._NeedFunction[Integer(fxRGBToHSL)]) Then
    Begin
      Line('lowp vec3 rgb2hsv(vec3 c){');
      Line('lowp vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);');
      Line('lowp vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));');
      Line('lowp vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));');
      Line('lowp float d = q.x - min(q.w, q.y);');
      Line('lowp float e = 1.0e-10;');
      Line('return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);}');
    End;

    If (Self._NeedFunction[Integer(fxHSLToRGB)]) Then
    Begin
      Line('lowp vec3 hsv2rgb(vec3 c){');
      Line('lowp vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);');
      Line('lowp vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);');
      Line('return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);}');
    End;

    If (Self._NeedFunction[Integer(fxColorGrading)]) Then
    Begin
      Line(GetColorTableShaderCode());
    End;

    If (Self._NeedFunction[Integer(fxGreyScale)]) Then
    Begin
    	Line('  const lowp vec3 LumCoeff = vec3(0.2125, 0.7154, 0.0721);');
      Line('  lowp vec3 greyScale(lowp vec3 color) {');
      Line('  return vec3(dot(color, LumCoeff)); }');
    End;

    If (Self._NeedFunction[Integer(fxCellularNoise)]) Then
    Begin
      Line('uniform sampler2D cellNoiseTex;');

      Line('float cellNoise(vec2 P)	{ return texture2D(cellNoiseTex, P * 0.25);}');
    End;

    For I:=0 To Pred(_FXCount) Do
    If (_FXs[I].Enabled) Then
    Begin
      _FXs[I]._Buffer := '';
      _FXs[I].GenerateFunctions();
      S := S + _FXs[I]._Buffer;
    End;

    If (Self.AntiAlias) Then
    Begin
      Line('	varying mediump vec2 v_rgbNW;');
      Line('	varying mediump vec2 v_rgbNE;');
      Line('	varying mediump vec2 v_rgbSW;');
      Line('	varying mediump vec2 v_rgbSE;');
      Line('	varying mediump vec2 v_rgbM;');

      Line('	const float FXAA_REDUCE_MIN  = (1.0/ 128.0);');
      Line('	const float FXAA_REDUCE_MUL  = (1.0 / 8.0);');
      Line('	const float FXAA_SPAN_MAX    = 8.0;');
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

    If (Self._Antialias) Then
    Begin
      Line('    mediump vec2 fragCoord = output_uv * screenResolution;');
      Line('    mediump vec2 inverseVP = vec2(1.0 / screenResolution.x, 1.0 / screenResolution.y);');
      Line('    vec3 rgbNW = texture2D(diffuse_texture, v_rgbNW).xyz;');
      Line('    vec3 rgbNE = texture2D(diffuse_texture, v_rgbNE).xyz;');
      Line('    vec3 rgbSW = texture2D(diffuse_texture, v_rgbSW).xyz;');
      Line('    vec3 rgbSE = texture2D(diffuse_texture, v_rgbSE).xyz;');
      Line('    vec3 rgbM  = texture2D(diffuse_texture, v_rgbM).xyz;');
      Line('    vec3 luma = vec3(0.299, 0.587, 0.114);');
      Line('    float lumaNW = dot(rgbNW, luma);');
      Line('    float lumaNE = dot(rgbNE, luma);');
      Line('    float lumaSW = dot(rgbSW, luma);');
      Line('    float lumaSE = dot(rgbSE, luma);');
      Line('    float lumaM  = dot(rgbM,  luma);');
      Line('    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));');
      Line('    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));');

      Line('    mediump vec2 dir;');
      Line('    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));');
      Line('    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));');

      Line('    float dirReduce = max((lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL), FXAA_REDUCE_MIN);');

      Line('    float rcpDirMin = 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);');
      Line('    dir = min(vec2(FXAA_SPAN_MAX, FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX), dir * rcpDirMin)) * inverseVP;');

      Line('    vec3 rgbA = 0.5 * (');
      Line('    texture2D(diffuse_texture, fragCoord * inverseVP + dir * (1.0 / 3.0 - 0.5)).xyz +');
      Line('    texture2D(diffuse_texture, fragCoord * inverseVP + dir * (2.0 / 3.0 - 0.5)).xyz);');

      Line('    vec3 rgbB = rgbA * 0.5 + 0.25 * (');
      Line('    texture2D(diffuse_texture, fragCoord * inverseVP + dir * -0.5).xyz +');
      Line('    texture2D(diffuse_texture, fragCoord * inverseVP + dir * 0.5).xyz);');

      Line('    float lumaB = dot(rgbB, luma);');
      Line('    if ((lumaB < lumaMin) || (lumaB > lumaMax))');
      Line('      output_color = vec4(rgbA, 1.0);');
      Line('    else');
      Line('        output_color = vec4(rgbB, 1.0);');
    End Else
      Line('    output_color = texture2D(diffuse_texture, output_uv);');

    {$IFDEF ADVANCED_ALPHA_BLEND}
    Line('    lowp vec4 alpha_color = texture2D(alpha_texture, output_uv);');
    Line('    output_color = mix(output_color, alpha_color, alpha_color.a);');
    {$ENDIF}

    For I:=0 To Pred(_FXCount) Do
    If (_FXs[I]._FXType = FXColor) And (_FXs[I].Enabled) Then
    Begin
      _FXs[I]._Buffer := '';
      _FXs[I].GenerateCode();
      S := S + _FXs[I]._Buffer;
    End;

    {$IFDEF GAMMA_CORRECTION}
    Line('  output_color.rgb = pow(output_color.rgb, vec3(2.2));');
    {$ENDIF}
    //Line('    gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);}');
    Line('    gl_FragColor = output_color;}');
    Line('}');

    _Shader := GraphicsManager.Instance.Renderer.CreateShader();
    _Shader.Generate(Self.GetShaderName(), S);
    _NeedsUpdate := False;
  End;

  Result := _Shader;
End;

Procedure ScreenFXChain.DrawScreen(X1,Y1,X2,Y2:Single);
Var
  _SH:ShaderInterface;
  I:Integer;
  M:Matrix4x4;
  Target:Texture;
  Slot:Integer;
  View:Viewport;
Begin
  _SH := Self.GetShader();

  GraphicsManager.Instance.Renderer.BindShader(_SH);

  {M := GraphicsManager.Instance.ActiveViewport.Projection;
  M := MatrixInverse(M);
  _SH.SetUniform('inverseProjectionMatrix', M);
  }

  Slot := 0;
  View := GraphicsManager.Instance.ActiveViewport;

  For I:=0 To Pred(TotalCaptureTargets) Do
  If (Self._NeedTarget[I]) Then
  Begin
    View.SetRenderTargetState(RenderTargetType(I), True);

    Target := View.GetRenderTexture(RenderTargetType(I));
    Target.Filter := filterBilinear;
    Target.Bind(Slot);

    _SH.SetIntegerUniform(TargetTextureNames[I], Slot);
    Inc(Slot);
  End;

  //If Self.AntiAlias Then
  Begin
    _Sh.SetVec2Uniform('screenResolution', VectorCreate2D(View.Width, View.Height));
    _Sh.SetFloatUniform('globalTime', Application.Instance.GetTime() / 1000);
  End;

  For I:=0 To Pred(_FXCount) Do
    _FXs[I].SetupUniforms(_SH, Slot);

  If (Self._NeedFunction[Integer(fxCellularNoise)]) Then
  Begin
    TextureManager.Instance.CellNoise.Bind(Slot);
    _Sh.SetIntegerUniform('cellNoiseTex', Slot);
    Inc(Slot);
  End;
    
  GraphicsManager.Instance.Renderer.SetBlendMode(blendNone);
  GraphicsManager.Instance.DrawFullscreenQuad(_SH, X1,Y1,X2,Y2);
End;

Procedure ScreenFXChain.OnContextLost;
Begin
  ReleaseObject(_Shader);
  _NeedsUpdate := True;
End;

Procedure ScreenFXChain.SetAntiAlias(const Value: Boolean);
Begin
  If (_Antialias = Value ) Then
    Exit;

  _Antialias := Value;
  _NeedsUpdate := True;
End;

{ ScreenFX }
Function ScreenFX.AddUniform(const Name: TERRAString; Kind: UniformType):Integer;
Begin
  Result := _UniformCount;
  Inc(_UniformCount);
  SetLength(_Uniforms, _UniformCount);
  _Uniforms[Pred(_UniformCount)].Name := Name;
  _Uniforms[Pred(_UniformCount)].Kind := Kind;
End;

Procedure ScreenFX.GenerateCode;
Begin
  _Buffer := '';
End;

Procedure ScreenFX.GenerateFunctions;
Begin
  _Buffer := '';
End;

Procedure ScreenFX.GetColor(Index: Integer; out Value: Color);
Begin
  If (Index<0) Or (Index>=_UniformCount) Then
    Value := ColorNull
  Else
    Value := ColorCreateFromFloat(_Uniforms[Index].Value.X, _Uniforms[Index].Value.Y, _Uniforms[Index].Value.Z, _Uniforms[Index].Value.W);
End;

Procedure ScreenFX.GetFloat(Index: Integer; out Value: Single);
Begin
  If (Index<0) Or (Index>=_UniformCount) Then
    Value := 0.0
  Else
    Value := _Uniforms[Index].Value.X;
End;

Procedure ScreenFX.GetTexture(Index: Integer; out Value: Texture);
Begin
  If (Index<0) Or (Index>=_UniformCount) Then
    Value := Nil
  Else
    Value := _Uniforms[Index].Tex;
End;

Procedure ScreenFX.GetVec2(Index: Integer; out Value: Vector2D);
Begin
  If (Index<0) Or (Index>=_UniformCount) Then
    Value := VectorCreate2D(0.0, 0.0)
  Else
    Value := VectorCreate2D(_Uniforms[Index].Value.X, _Uniforms[Index].Value.Y);
End;

Procedure ScreenFX.GetVec3(Index: Integer; out Value: Vector3D);
Begin
  If (Index<0) Or (Index>=_UniformCount) Then
    Value := VectorCreate(0.0, 0.0, 0.0)
  Else
    Value := VectorCreate(_Uniforms[Index].Value.X, _Uniforms[Index].Value.Y, _Uniforms[Index].Value.Z);
End;

Procedure ScreenFX.GetVec4(Index: Integer; out Value: Vector4D);
Begin
  If (Index<0) Or (Index>=_UniformCount) Then
    Value := VectorCreate4D(0.0, 0.0, 0.0, 0.0)
  Else
    Value := VectorCreate4D(_Uniforms[Index].Value.X, _Uniforms[Index].Value.Y, _Uniforms[Index].Value.Z, _Uniforms[Index].Value.W);
End;

Procedure ScreenFX.SetColor(Index: Integer; const Value: Color);
Begin
  If (Index>=0) And (Index<_UniformCount) Then
  Begin
    _Uniforms[Index].Initialized := True;
    _Uniforms[Index].Value := VectorCreate4D(Value.R/255, Value.G/255, Value.B/255, Value.A/255);
  End;
End;

Procedure ScreenFX.SetFloat(Index: Integer; const Value: Single);
Begin
  If (Index>=0) And (Index<_UniformCount) Then
  Begin
    _Uniforms[Index].Initialized := True;
    _Uniforms[Index].Value.X := Value;
  End;
End;

Procedure ScreenFX.SetTexture(Index: Integer; Value: Texture);
Begin
  If (Index>=0) And (Index<_UniformCount) Then
  Begin
    _Uniforms[Index].Initialized := True;
    _Uniforms[Index].Tex := Value;
  End;
End;

Procedure ScreenFX.SetVec2(Index: Integer; const Value: Vector2D);
Begin
  If (Index>=0) And (Index<_UniformCount) Then
  Begin
    _Uniforms[Index].Initialized := True;
    _Uniforms[Index].Value := VectorCreate4D(Value.X, Value.Y, 0.0, 0.0);
  End;
End;

Procedure ScreenFX.SetVec3(Index: Integer; const Value: Vector3D);
Begin
  If (Index>=0) And (Index<_UniformCount) Then
  Begin
    _Uniforms[Index].Initialized := True;
    _Uniforms[Index].Value := VectorCreate4D(Value.X, Value.Y, Value.Z, 0.0);
  End;
End;

Procedure ScreenFX.SetVec4(Index: Integer; const Value: Vector4D);
Begin
  If (Index>=0) And (Index<_UniformCount) Then
  Begin
    _Uniforms[Index].Initialized := True;
    _Uniforms[Index].Value := VectorCreate4D(Value.X, Value.Y, Value.Z, Value.W);
  End;
End;

Procedure ScreenFX.Line(S2:TERRAString);
Begin
  _Buffer := _Buffer + S2 + crLf;
End;

Function ScreenFX.RequiresFunction(FXFunction: ScreenFXFunctionType): Boolean;
Begin
  Result := False;
End;

Function ScreenFX.RequiresTarget(TargetType:RenderTargetType):Boolean;
Begin
  Result := False;
End;


Procedure ScreenFX.SetEnabled(const Value: Boolean);
Begin
  If (Self._Enabled = Value) Then
    Exit;

  Self._Enabled := Value;

  If Assigned(Self._Owner) Then
    Self._Owner._NeedsUpdate := True;
End;

Procedure ScreenFX.GenerateUniforms();
Var
  I:Integer;
  TypeName:TERRAstring;
Begin
  _Buffer := '';

  For I:=0 To Pred(_UniformCount) Do
  Begin
    Case _Uniforms[I].Kind Of
    uniformFloat:   TypeName := 'float';
    uniformVec2:   TypeName := 'vec2';
    uniformVec3:   TypeName := 'vec3';

    uniformVec4,
    uniformColor:   TypeName := 'vec4';

    uniformPalette: Continue;
    
    uniformTexture: TypeName := 'sampler2D';

    Else
        Continue;
    End;

    Line('uniform highp '+ TypeName + ' ' + _Uniforms[I].Name + ';');
  End;
End;

Procedure ScreenFX.SetupUniforms(Sh:ShaderInterface; Var TextureSlot:Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(_UniformCount) Do
  Begin
    If Not _Uniforms[I].Initialized Then
    Begin
      RaiseError('Uniform '+_Uniforms[I].Name+ ' is not initialized in '+Self.ClassName);
      Exit;
    End;

    Case _Uniforms[I].Kind Of
    uniformFloat:   Sh.SetFloatUniform(_Uniforms[I].Name, _Uniforms[I].Value.X);
    uniformVec2:   Sh.SetVec2Uniform(_Uniforms[I].Name, VectorCreate2D(_Uniforms[I].Value.X, _Uniforms[I].Value.Y));
    uniformVec3:   Sh.SetVec3Uniform(_Uniforms[I].Name, VectorCreate(_Uniforms[I].Value.X, _Uniforms[I].Value.Y, _Uniforms[I].Value.Z));
    uniformVec4:   Sh.SetVec4Uniform(_Uniforms[I].Name, _Uniforms[I].Value);
    uniformColor:   Sh.SetVec4Uniform(_Uniforms[I].Name, _Uniforms[I].Value);

    uniformTexture:
      Begin
        If _Uniforms[I].Tex = Nil Then
          _Uniforms[I].Tex := TextureManager.Instance.WhiteTexture;

        _Uniforms[I].Tex.Bind(TextureSlot);

        Sh.SetIntegerUniform(_Uniforms[I].Name, TextureSlot);
        Inc(TextureSlot);
      End;

    uniformPalette:
      Begin
        If _Uniforms[I].Tex = Nil Then
          _Uniforms[I].Tex := TextureManager.Instance.DefaultColorTable;

        ColorTableBind(_Uniforms[I].Tex, TextureSlot);
        Inc(TextureSlot);
      End;

    End;
  End;
End;

{ OutlineFX }
Constructor OutlineFX.Create(Width:Single);
Begin
  Self._FXType := FXColor;
  Self.LineWidth := Width;
End;

Procedure OutlineFX.GenerateCode;
Begin
(*  Line('  lowp vec4 outline_color = texture2D(outline_texture, output_uv); ');
  //Line('  lowp vec4 outline_color = vec4(1.0, 0.0, 0.0 , 1.0); ');
  Line('  lowp float edgeValue = sobelFilter(output_uv);');
  //Line('  output_color *= edgeValue;');

  Line('  outline_color.rgb = mix(outline_color.rgb, output_color.rgb, edgeValue);');
  Line('  output_color.rgb = mix(output_color.rgb, outline_color.rgb, outline_color.a);');

  Line('  output_color.rgb = vec3(edgeValue);');*)

  Line('  lowp vec4 outline_color = texture2D(outline_texture, output_uv); ');
  Line('  lowp float edge_value = texture2D(edge_texture, output_uv).r; ');
  Line('  outline_color.rgb = mix(outline_color.rgb, output_color.rgb, edge_value);');
  Line('  output_color.rgb = mix(output_color.rgb, outline_color.rgb, outline_color.a);');
End;

Function OutlineFX.RequiresTarget(TargetType: RenderTargetType): Boolean;
Begin
  Result := (TargetType = captureTargetOutline) Or (TargetType = effectTargetEdge);
End;

{ ColorCorrectionFX }
Constructor ColorCorrectionFX.Create(Contrast, Brightness, Saturation:Single);
Begin
  Self._FXType := FXColor;

  _Contrast := Self.AddUniform('mod_constrast', uniformFloat);
  _Brightness := Self.AddUniform('mod_brightness', uniformFloat);
  _Saturation := Self.AddUniform('mod_saturation', uniformFloat);

  Self.SetContrast(Contrast);
  Self.SetBrightness(Brightness);
  Self.SetSaturation(Saturation);
End;

Procedure ColorCorrectionFX .GenerateCode;
Begin
  Line('  output_color.rgb = ContrastSaturationBrightness(output_color.rgb);');
End;

procedure ColorCorrectionFX .GenerateFunctions;
begin
	// For all settings: 1.0 = 100% 0.5=50% 1.5 = 150%
	Line('lowp vec3 ContrastSaturationBrightness(lowp vec3 color)	{');
  Line('  lowp vec3 AvgLumin = vec3(0.5);');
  Line('  lowp vec3 brtColor = color * mod_brightness;');
  Line('  lowp vec3 intensity = greyScale(brtColor);');
  Line('  lowp vec3 satColor = mix(intensity, brtColor, mod_saturation);');
  Line('  lowp vec3 conColor = mix(AvgLumin, satColor, mod_constrast);');
  Line('return conColor;	}');
End;

Function ColorCorrectionFX.RequiresFunction(FXFunction: ScreenFXFunctionType): Boolean;
Begin
  Result := (FXFunction = fxGreyScale);
End;

Function ColorCorrectionFX.GetBrightness: Single;
Begin
  Self.GetFloat(_Brightness, Result);
End;

function ColorCorrectionFX.GetContrast: Single;
Begin
  Self.GetFloat(_Contrast, Result);
End;

function ColorCorrectionFX.GetSaturation: Single;
Begin
  Self.GetFloat(_Saturation, Result);
End;

Procedure ColorCorrectionFX.SetBrightness(const Value: Single);
Begin
  Self.SetFloat(_Brightness, Value);
End;

Procedure ColorCorrectionFX.SetContrast(const Value: Single);
Begin
  Self.SetFloat(_Contrast, Value);
End;

Procedure ColorCorrectionFX.SetSaturation(const Value: Single);
Begin
  Self.SetFloat(_Saturation, Value);
End;

{ VibranceFX }
Constructor VibranceFX.Create(Strength: Single);
Var
  Exp:Image;
  C:Color;
  I:Integer;
Begin
  Exp := Image.Create(256, 1);
  For I:=0 To 255 Do
  Begin
    C := ColorGrey(Trunc(SmoothCurveWithOffset(I/255, 0.5) * 255));

    Exp.SetPixel(I, 0, C);
  End;
  Exp.Resize(256, 2);
  //Exp.Save('satramp.png');

  _Ramp := Texture.Create();
  _Ramp.CreateFromImage('vibranceramp', Exp);

  ReleaseObject(Exp);

  _Strength := Self.AddUniform('mod_vibrance', uniformFloat);
  Self.SetStrength(Strength);

  I := Self.AddUniform('vibrance_ramp', uniformTexture);
  Self.SetTexture(I, _Ramp);
End;

Procedure VibranceFX.Release;
Begin
  ReleaseObject(_Ramp);
End;

Procedure VibranceFX.GenerateCode;
Begin
  Line('  output_color.rgb = AdjustVibrance(output_color.rgb, mod_vibrance);');
End;

procedure VibranceFX.GenerateFunctions;
Begin
	Line('lowp vec3 AdjustVibrance(lowp vec3 color, lowp float strength)	{');
  Line('  lowp vec3 AvgLumin = vec3(0.5);');
  Line('  lowp vec3 intensity = greyScale(color);');
  Line('  lowp float saturation = rgb2hsv(color).y;');
  Line('  lowp float vibranceScale = texture2D(vibrance_ramp, vec2(saturation, 0.5)).r;');
  Line('  lowp vec3 satColor = mix(intensity, color, strength * vibranceScale);');
  Line('return satColor;	}');
End;

Function VibranceFX.RequiresFunction(FXFunction: ScreenFXFunctionType): Boolean;
Begin
  Result := (FXFunction = fxRGBToHSL) Or (FXFunction = fxGreyScale);
End;

Function VibranceFX.GetStrength: Single;
Begin
  Self.GetFloat(_Strength, Result);
End;

Procedure VibranceFX.SetStrength(const Value: Single);
Begin
  Self.SetFloat(_Strength, Value);
End;

{ VignetteFX }
Constructor VignetteFX.Create;
Begin
  Self._FXType := FXColor;
  _Scale := Self.AddUniform('vignetteScale', uniformFloat);
End;

Procedure VignetteFX.GenerateCode;
Begin
  Line('output_color.rgb *= vignetteFX(output_uv);');
End;

Procedure VignetteFX.GenerateFunctions;
Begin
	Line('lowp float vignetteFX(lowp vec2 st)	{');
  Line('st -= 0.5;');
  Line('lowp float vignette = 1.0 - dot(st, st);');
  Line('return (pow( vignette, vignetteScale)+0.05);}');
End;

Procedure VignetteFX.SetScale(Value: Single);
Begin
  If (Value<0) Then
    Value := 0;
  If (Value>MaxVignetteScale) Then
    Value := MaxVignetteScale;

  Self.SetFloat(_Scale, Value);
End;

Function VignetteFX.GetScale(): Single;
Begin
  Self.GetFloat(_Scale, Result);
End;

{ GlowFX }
Constructor GlowFX.Create(Strength:Single);
Begin
  Self._FXType := FXColor;
  Self._Strength := Self.AddUniform('glow_strength', uniformFloat);
  Self.SetStrength(Strength);
End;

Procedure GlowFX.GenerateCode;
Begin
  Line('  output_color.rgb += glow_strength * texture2D(glow_texture, output_uv).rgb;');
End;

Function GlowFX.RequiresTarget(TargetType: RenderTargetType): Boolean;
Begin
  Result := (TargetType = effectTargetGlow);
end;

Function GlowFX.GetStrength: Single;
Begin
  Self.GetFloat(_Strength, Result);
End;

Procedure GlowFX.SetStrength(Value: Single);
Begin
  Self.SetFloat(_Strength, Value);
End;

{ RefractionFX }
Constructor RefractionFX.Create(Strength: Single);
Begin
  Self._FXType := FXColor;
  Self._Strength := AddUniform('refraction_strength', uniformFloat);

  Self.SetStrength(Strength);
End;

Function RefractionFX.RequiresTarget(TargetType: RenderTargetType): Boolean;
Begin
  Result := (TargetType = captureTargetRefraction);
End;

{
  Line('const lowp vec4 c1 = vec4( 1.0, 0.0, 0.0, 0.0 );');
  Line('const lowp vec4 c2 = vec4( 0.0, 0.0, 1.0, 0.0 );');
}
Procedure RefractionFX.GenerateCode;
Begin
  Line('  lowp vec4 refc = texture2D(refraction_texture, output_uv); ');
  Line('  output_uv = output_uv + refc.xy * refraction_strength;');
  Line('  lowp vec4 samprf = texture2D(diffuse_texture, output_uv); ');
  Line('  output_color.rgb = output_color.rgb * (1.0-refc.a) + samprf.rgb * refc.a;');
End;

Function RefractionFX.GetStrength: Single;
Begin
  Self.GetFloat(_Strength, Result);
End;

Procedure RefractionFX.SetStrength(Value: Single);
Begin
  Self.SetFloat(_Strength, Value);
End;

{ UnderwaterFX }
Constructor UnderwaterFX.Create(CausticsTex:Texture; Strength:Single);
Begin
  Self._FXType := FXOffset;

  _Strength := Self.AddUniform('caustics_strength', uniformFloat);
  Self.SetStrength(Strength);

  _Caustics := Self.AddUniform('caustics_texture', uniformTexture);
  Self.SetTexture(_Caustics, CausticsTex);
End;

Procedure UnderwaterFX.GenerateCode;
Begin
  Line('  lowp vec4 caustic_ofs = texture2D(caustics_texture, output_uv); ');
  Line('  output_uv += vec2(caustic_ofs.r * caustics_strength, caustic_ofs.g * caustics_strength * -0.5);');
End;

Function UnderwaterFX.GetStrength: Single;
Begin
  Self.GetFloat(_Strength, Result);
End;

Procedure UnderwaterFX.SetStrength(Value: Single);
Begin
  Self.SetFloat(_Strength, Value);
End;

{ DitherFX }
Constructor DitherFX.Create(Pattern, Palette: Texture);
Begin
  _Pattern := Self.AddUniform('dither_pattern', uniformTexture);
  _Palette := Self.AddUniform('dither_palette', uniformTexture);

  Self.SetTexture(_Pattern, Pattern);
  Self.SetTexture(_Palette, Palette);
End;

Procedure DitherFX.GenerateCode;
Begin
  Line('  lowp float edge_value = texture2D(edge_texture, output_uv).r; ');
  Line('  output_color.x *= (edge_value * 0.25 + 0.75);');

  Line('  output_color.z = dither_shade(output_color.x, output_uv);');

  Line('	lowp float shade_count = 8.0;');
  Line('  lowp float shading = output_color.x * shade_count;');
  Line('	lowp float shadeA = floor(shading);');
  Line('	lowp float shadeB = max(shadeA - 1.0, 0.0);');


  Line('	vec4 greyA = texture2D(dither_palette, vec2(shadeA/ shade_count, output_color.y));');
  Line('	vec4 greyB = texture2D(dither_palette, vec2(shadeB/ shade_count, output_color.y));');

  //Line('	output_color.rgb = mix(greyB.rgb, greyA.rgb, output_color.z);');
  Line('	output_color = greyA;');
End;

Procedure DitherFX.GenerateFunctions;
Begin
  Line('float dither_shade(float shade, vec2 uv){');
  Line('float limit = texture2D(dither_pattern, uv * 32.0).r;');
  Line('if (shade < limit) return 0.0;');
  Line('return 1.0;}');
End;

Function DitherFX.RequiresTarget(TargetType: RenderTargetType): Boolean;
Begin
  Result := (TargetType = effectTargetEdge);
End;

{ BloomFX }
Constructor BloomFX.Create(Strength: Single);
Begin
  _Strength := Self.AddUniform('bloom_strength', uniformFloat);
  SetStrength(Strength);
End;

Procedure BloomFX.GenerateCode;
Begin
  //Line('  output_color.rgb += bloom_strength * texture2D(bloom_texture, output_uv).rgb;');
  Line('  lowp vec3 bloom_color = bloom_strength *texture2D(bloom_texture, output_uv).rgb;');
  Line('  output_color.rgb = 1.0 - ((1.0 - output_color.rgb) * (1.0 - bloom_color));');
End;

Function BloomFX.RequiresTarget(TargetType: RenderTargetType): Boolean;
Begin
  Result := (TargetType = effectTargetBloom);
End;

Function BloomFX.GetStrength: Single;
Begin
  Self.GetFloat(_Strength, Result);
End;

Procedure BloomFX.SetStrength(Value: Single);
Begin
  Self.SetFloat(_Strength, Value);
End;

{ ColorGradingFX }
Constructor ColorGradingFX.Create(Palette: Texture);
Begin
  _Palette := Self.AddUniform(ColorTableUniformName, uniformPalette);
  Self.SetTexture(_Palette, Palette);
End;

Procedure ColorGradingFX.GenerateCode;
Begin
  Line('output_color.rgb = ColorTableLookup(output_color.rgb);');
End;

Function ColorGradingFX.RequiresFunction(FXFunction: ScreenFXFunctionType): Boolean;
Begin
  Result := (FXFunction = fxColorGrading);
End;

Var
  I:Integer;

Initialization
  For I:=0 To Pred(TotalCaptureTargets) Do
    TargetTextureNames[I] := TargetNames[I] + '_texture';
End.
