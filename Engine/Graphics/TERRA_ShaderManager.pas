Unit TERRA_ShaderManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_String, TERRA_Renderer, TERRA_ShaderNode;

Function Get_SkyboxDiffuseShader():ShaderInterface;
Function Get_SkyboxNormalShader():ShaderInterface;

Function GetShader_Sprite(SpriteFlags:Cardinal):TERRAShaderGroup;
Function GetShader_Particles():TERRAShaderGroup;

Function GetShader_StencilVolumeShader():TERRAShaderGroup;

Function GetShader_FullscreenQuad():TERRAShaderGroup;
Function GetShader_FullscreenColor():TERRAShaderGroup;

Function GetShader_Blur():TERRAShaderGroup;
Function GetShader_Edge():TERRAShaderGroup;

Implementation
Uses TERRA_OS, TERRA_ShaderFactory, TERRA_Sprite, TERRA_Engine, TERRA_GraphicsManager
{$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Var
  _SkyboxDiffuseShader:ShaderInterface = Nil;
  _SkyboxNormalShader:ShaderInterface = Nil;

Function GetShader_Skybox(OutputMode:Integer):TERRAShaderGroup;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('skybox');

  S := '';
  Line('varying highp vec3 normal;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  uniform mat4 rotationMatrix;');
  Line('  uniform mat4 reflectionMatrix;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec3 terra_normal;');
  Line('  void main()	{');
  Line('  gl_Position = projectionMatrix * terra_position;');
  Line('  highp vec4 n = reflectionMatrix * vec4(terra_normal, 1.0);');
  Line('  normal = (rotationMatrix * n).xyz;}');

  Result.XVertexCode := S;

  S := '';
  Line('varying highp vec3 normal;');
  Line('uniform samplerCube skyTexture;');
  Line('uniform lowp vec4 skyColor;');
  Line('  void main()	{');
  Line('  highp vec3 n = normalize(normal);');
  If (OutputMode And shader_OutputNormal<>0) Then
  Begin
    Line('  n *= 0.5; n += vec3(0.5, 0.5, 0.5);');
    Line('  gl_FragColor = vec4(n, 0.0);}');
  End Else
  Begin
    Line('  lowp vec4 sky = textureCube(skyTexture, n) * skyColor; ');
    Line('  gl_FragColor = vec4(sky.rgb, 1.0);}');
    //Line('  gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0);}');
  End;

  Result.XFragmentCode := S;
End;

Function Get_SkyboxNormalShader():ShaderInterface;
Begin
  If (_SkyboxNormalShader = Nil) Then
  Begin
    _SkyboxNormalShader := Engine.Graphics.Renderer.CreateShader();
    _SkyboxNormalShader.Generate('skybox_normal', GetShader_Skybox(shader_OutputNormal));
  End;

  Result := _SkyboxNormalShader;
End;

Function Get_SkyboxDiffuseShader():ShaderInterface;
Begin
  If (_SkyboxDiffuseShader = Nil) Then
  Begin
    _SkyboxDiffuseShader := Engine.Graphics.Renderer.CreateShader();
    _SkyboxDiffuseShader.Generate('skybox', GetShader_Skybox(0));
  End;

  Result := _SkyboxDiffuseShader;
End;

Function GetSaturationAndConstrast():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
	S := '';
	Line('  const lowp vec3 LumCoeff = vec3(0.2125, 0.7154, 0.0721);');
	Line('  const lowp float middleGray = 0.5;');
	Line('lowp vec3 AdjustSaturation(lowp vec3 color, lowp float saturation)	{');
  Line('  lowp vec3 AvgLumin = vec3(0.5);');
  Line('  lowp vec3 intensity = vec3(dot(color, LumCoeff));');
  Line('  lowp vec3 satColor = mix(intensity, color, saturation);');
  Line('return satColor;	}');
  Result := S;
End;

Function GetShader_Sprite(SpriteFlags:Cardinal):TERRAShaderGroup;
Var
  S:TERRAString;
  DoColorGrading, IsFont, IsSolid, IsDissolve, IsGUI, HasPattern:Boolean;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('sprite_'+CardinalToString(SpriteFlags));

  DoColorGrading := (SpriteFlags And Sprite_ColorGrading)<>0;
  IsFont := (SpriteFlags And Sprite_Font)<>0;
  IsSolid := (SpriteFlags And Sprite_SolidColor)<>0;
  IsDissolve := (SpriteFlags And Sprite_Dissolve)<>0;
  HasPattern := (SpriteFlags And Sprite_Pattern)<>0;
  IsGUI := (SpriteFlags And Sprite_GUI)<>0;

  S := '';
	Line('  varying highp vec4 clipRect;');
	Line('  varying highp vec2 screen_position;');

	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 fillColor;');
	Line('  varying lowp vec4 color;');
  Line('  attribute highp vec3 terra_position;');
  Line('  attribute mediump vec2 terra_UV0;');
  Line('  attribute mediump vec4 terra_UV1;');
  Line('  attribute mediump vec4 terra_UV2;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  uniform mat4 projectionMatrix;');

  If Not IsGUI Then
    Line('  uniform mat4 modelMatrix;');

	Line('void main()	{');
  Line('  vec4 local_position = vec4(terra_position.x, terra_position.y, terra_position.z, 1.0);');
  Line('  screen_position = local_position.xy;');

  If IsGUI Then
    Line('  gl_Position =  projectionMatrix * local_position;')
  Else
    Line('  gl_Position =  projectionMatrix * modelMatrix * local_position;');

  Line('  texCoord = terra_UV0;');
  Line('  clipRect = terra_UV1;');
  Line('  fillColor = terra_UV2;');
  Line('  color = terra_color;}');
  Result.XVertexCode := S;

  S := '';
	Line('  varying highp vec4 clipRect;');
	Line('  varying highp vec2 screen_position;');

	Line('  varying mediump vec2 texCoord;');
	Line('  varying lowp vec4 fillColor;');
	Line('  varying lowp vec4 color;');

  If (IsDissolve) Then
  Begin
	  Line('  uniform lowp float dissolve_value;');
	  Line('  uniform sampler2D dissolve_texture;');
  End;

  If (HasPattern) Then
  Begin
	  Line('  uniform sampler2D pattern_texture;');
  End;

  If (Not IsSolid) Then
	  Line('  uniform sampler2D texture;');

  If IsFont Then
  Begin
    //Line('  const float smoothing = 3.0/16.0;');
    Line('  uniform lowp float smoothing;');
    Line('  const float outlineWidth = 5.0/16.0;');
    Line('  const float outerEdgeCenter = 0.5 - outlineWidth;');
    Line('  uniform lowp vec4 shadowOffset;');
    Line('  uniform lowp vec4 outlineColor;');
  End;

  Line(GetSaturationAndConstrast());

  {$IFNDEF DISABLECOLORGRADING}
  If (DoColorGrading) Then
    Line(GetColorTableShaderCode());
  {$ENDIF}

	Line('  void main()	{');

  Line('  lowp float alphaCut = 1.0; ');
  Line('  alphaCut *= step(clipRect.x, screen_position.x); ');
  Line('  alphaCut *= step(screen_position.x, clipRect.z); ');
  Line('  alphaCut *= step(clipRect.y, screen_position.y); ');
  Line('  alphaCut *= step(screen_position.y, clipRect.w); ');

  If (IsDissolve) Then
  Begin
    Line('    lowp float dissolve_limit = texture2D(dissolve_texture, texCoord.xy).r;');
    Line('  alphaCut *= step(dissolve_value, dissolve_limit); ');
  End;

  If (HasPattern) Then
  Begin
    Line('    lowp vec4 pattern = texture2D(pattern_texture, screen_position.xy * 0.01);');
  End;

(*  Line('  if ( screen_position.x< clipRect.x) { discard;} ');
  Line('  if ( screen_position.x> clipRect.z) { discard;} ');

  Line('  if ( screen_position.y< clipRect.y) { discard;} ');
  Line('  if ( screen_position.y> clipRect.w) { discard;} ');
*)

  If (IsSolid) Then
    Line('    lowp vec4 sourceColor = fillColor;')
  Else
    Line('    lowp vec4 sourceColor = texture2D(texture, texCoord.xy);');

  If IsFont Then
  Begin
  {$IFDEF DISTANCEFIELDFONTS}
  Line('    float colorDistance = sourceColor.a;');
  Line('    float alpha = smoothstep(outerEdgeCenter - smoothing, outerEdgeCenter + smoothing, colorDistance);');
  Line('    float border = smoothstep(0.5 - smoothing, 0.5 + smoothing, colorDistance);');
  Line('    vec4 baseColor = mix(outlineColor, color, border);');

  If HasPattern Then
    Line(' baseColor *= pattern; ');

  Line('  alpha *=  color.a;');
  Line('  alpha *= alphaCut;');
  Line('    gl_FragColor = vec4( baseColor.rgb, baseColor.a * alpha);}');

  {$ELSE}

  Line('    lowp vec4 mask = sourceColor;');
  Line('    lowp float alpha;');
  Line('    if (mask.a<0.5) alpha = 0.0; else alpha = 1.0;');

  {$IFNDEF MOBILE}
  Line('    alpha *= smoothstep(0.25, 0.75, mask.a);');// anti-aliasing
  {$ENDIF}
  Line('    lowp vec4 baseColor;');
  Line('    baseColor = color; ');

  Line('  alpha *= alphaCut;');

  If HasPattern Then
    Line(' baseColor *= pattern; ');

  Line('    baseColor.rgb = AdjustSaturation(baseColor.rgb, fillColor.a); ');
//  Line('    baseColor.rgb = mix(baseColor.rgb, outlineColor.rgb, mask.r); ');
  Line('    gl_FragColor = vec4(baseColor.r, baseColor.g, baseColor.b, alpha * color.a);}');

  //Line('    gl_FragColor = vec4(mask.r);');

  {$ENDIF}
  End Else
  Begin
    Line('    vec4 c = sourceColor * color;');
    {$IFNDEF DISABLECOLORGRADING}
    If (DoColorGrading) Then
      Line('    c.rgb = ColorTableLookup(c.rgb);');
    {$ENDIF}
    Line('    c.rgb = AdjustSaturation(c.rgb, fillColor.a); ');

    Line('  c.a *= alphaCut;');

    Line('    if (c.a<0.1) discard;');
    Line('    gl_FragColor = c;}');
  End;

  Result.XFragmentCode := S;
End;

Function GetShader_Particles():TERRAShaderGroup;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('particles');

  S := '';
  Line('	varying mediump vec4 texCoord;');
  Line('	varying lowp vec4 diffuse;');
  Line('	uniform highp vec3 cameraPosition;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec4 terra_UV0;');
  Line('  attribute mediump vec2 terra_ofs;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  attribute mediump vec2 terra_size;');
  Line('  attribute mediump vec2 terra_angle;');
  Line('	uniform mat4 cameraMatrix;');
  Line('	uniform mat4 projectionMatrix;');
  Line('  uniform mat4 reflectionMatrix;');
  Line('	uniform mediump vec3 cameraRight;');
  Line('	uniform mediump vec3 cameraUp;');
//  Line('	uniform highp float ratio;');
  Line('	void main()	{');
  Line('		texCoord = terra_UV0;');
  Line('		diffuse = terra_color;	');
  Line('		highp vec4 world_position = terra_position;');
  Line('    world_position = reflectionMatrix * world_position;');
  Line('    highp vec2 pp = terra_size * terra_ofs;');
  Line('    pp = vec2(pp.x * terra_angle.x - pp.y * terra_angle.y, pp.x * terra_angle.y + pp.y * terra_angle.x);');
  Line('		world_position.xyz += (pp.x * cameraRight + pp.y * cameraUp);');//  Line('		world_position.xyz += (ratio * pp.x * cameraRight + pp.y * cameraUp);');
  Line('		gl_Position = projectionMatrix * cameraMatrix * world_position;}');
  Result.XVertexCode := S;

  S := '';
  Line('	uniform sampler2D texture0;');
  Line('	uniform highp vec3 cameraPosition;');
  Line('	uniform lowp vec4 sunColor;');
  Line('	varying mediump vec4 texCoord;');
  Line('	varying lowp vec4 diffuse;');
  Line('	void main()	{');
  Line('	  lowp vec4 color = texture2D(texture0, texCoord.st) * diffuse;');
  Line('    if (color.a<0.1) discard;');
  Line('    color *= sunColor;');
  Line('		gl_FragColor = color;}');
  Result.XFragmentCode := S;
End;


(*Function GetShader_SimpleColor():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 110 }');
  Line('vertex {');
	Line('  uniform mat4 cameraMatrix;');
	Line('  uniform mat4 modelMatrix;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  attribute highp vec4 terra_position;');
	Line('  void main()	{');
  Line('    gl_Position = projectionMatrix * cameraMatrix * modelMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
	Line('  uniform lowp vec4 out_color;');
	Line('	void main()	{');
	Line('	gl_FragColor = out_color;}');
  Line('}');
  Result := S;
End;*)


Function GetShader_StencilVolumeShader():TERRAShaderGroup;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('stencil_shadows');

  S := '';
	Line('  uniform mat4 cameraMatrix;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  attribute highp vec4 terra_position;');
	Line('  void main()	{');
  Line('    gl_Position = projectionMatrix * cameraMatrix * terra_position;}');
  Result.XVertexCode := S;

  S := '';
	Line('	void main()	{');
	Line('	gl_FragColor = vec4(1.0, 1.0, 0.0, 0.5);}');
  Result.XFragmentCode := S;
End;

Function GetShader_FullscreenColor():TERRAShaderGroup;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('fullscreen_color');

  S := '';
  Line('vertex {');
  Line('  attribute highp vec4 terra_position;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;}');
  Result.XVertexCode := S;

  S := '';
  Line('  uniform mediump vec4 color;');
	Line('  void main()	{');
  Line('    gl_FragColor = color;}');
  Result.XFragmentCode := S;
End;

Function GetShader_FullscreenQuad():TERRAShaderGroup;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('fullscreen_quad');

  S := '';
	Line('  varying mediump vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec3 terra_UV0;');
  Line('  uniform mat4 projectionMatrix;');
	Line('void main()	{');
  Line('  gl_Position =  projectionMatrix * terra_position;');
  Line('  texCoord = terra_UV0.xy;}');

  Result.XVertexCode := S;

  S := '';
	Line('  varying mediump vec2 texCoord;');
	Line('  uniform sampler2D texture;');
	Line('  void main()	{');
  Line('    lowp vec4 c = texture2D(texture, texCoord.st);');
  {$IFDEF TESTFULLSCREENSHADER}
  Line('    gl_FragColor = vec4(0.0,1.0, 0.0, 1.0);}');
  {$ELSE}
  Line('    gl_FragColor = c;}');
  {$ENDIF}
  Result.XFragmentCode := S;
End;

Function GetShader_Blur():TERRAShaderGroup;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('fullscreen_blur');

  S := '';
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying highp vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec2 terra_UV0;');
	Line('  void main()	{');
  Line('    texCoord = terra_UV0;');
  Line('    gl_Position = projectionMatrix * terra_position;}');
  Result.XVertexCode := S;

  S := '';
  Line('varying highp vec2 texCoord;');
  Line('uniform highp float dx, dy;');
  Line('uniform sampler2D texture;');
	Line('void main()	{');
  Line('lowp vec2 st = texCoord.st;');

  // Apply 3x3 gaussian filter
	Line('lowp vec4 color	= texture2D(texture, st);');
  Line('lowp float alpha = color.a;');
  Line('color *= 4.0;');
  Line('lowp vec4 temp = texture2D(texture, st + vec2(+dx, 0.0));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(-dx, 0.0));;');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(0.0, +dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(0.0, -dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= 2.0 * temp;');
  Line('temp = texture2D(texture, st + vec2(+dx, +dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');
  Line('temp = texture2D(texture, st + vec2(-dx, +dy));;');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');
  Line('temp = texture2D(texture, st + vec2(-dx, -dy));;');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');
  Line('temp = texture2D(texture, st + vec2(+dx, -dy));');
  Line('alpha = max(alpha, temp.a);');
  Line('color		+= temp;');

  Line('color = color / 16.0;');
  Line('color.a = alpha;');
  Line('gl_FragColor = color;}');

  Result.XFragmentCode := S;
End;


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

Function GetShader_Edge():TERRAShaderGroup;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  Result := TERRAShaderGroup.Create('fullscreen_edge');

  S := '';
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying highp vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec2 terra_UV0;');

	Line('  void main()	{');
  Line('    texCoord = terra_UV0;');
  Line('    gl_Position = projectionMatrix * terra_position;}');
  Result.XVertexCode := S;

  S := '';
  Line('varying highp vec2 texCoord;');
  Line('uniform highp float dx, dy;');
  Line('uniform sampler2D texture;');

	Line('void main()	{');
	Line('lowp float offX = dx;');
  Line('lowp float offY = dy;');

  // Apply sobel filter

  Line('mediump vec3 sample;');
  //Line('lowp float depth = texture2D(normal_texture, texCoord).a;');

  //Line(' depth = (1.0 - depth) * 0.5;');
  Line(' mediump float kox = offX; ');
  Line(' mediump float koy = offY; ');

// fetch the 3x3 neighbourhood and use the RGB vector's length as intensity value
  Line('  lowp float spv0;');
  Line('  lowp float spv1;');
  Line('  lowp float spv2;');
  Line(' spv0 = length(texture2D(texture, texCoord + vec2(-kox,-koy)).rgb);');
  Line(' spv1 = length(texture2D(texture, texCoord + vec2(-kox, 0.0)).rgb);');
  Line(' spv2 = length(texture2D(texture, texCoord + vec2(-kox, koy)).rgb);');
  Line(' lowp vec3 sample0 = vec3(spv0, spv1, spv2);');
  Line(' spv0 = length(texture2D(texture, texCoord + vec2(0.0, -koy)).rgb);');
  Line(' spv1 = length(texture2D(texture, texCoord).rgb);');
  Line(' spv2 = length(texture2D(texture, texCoord + vec2(0.0, koy)).rgb);');
  Line(' lowp vec3 sample1 = vec3(spv0, spv1, spv2);');
  Line(' spv0 = length(texture2D(texture, texCoord + vec2(kox,-koy)).rgb);');
  Line(' spv1 = length(texture2D(texture, texCoord + vec2(kox, 0.0)).rgb);');
  Line(' spv2 = length(texture2D(texture, texCoord + vec2(kox, koy)).rgb);');
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

  Line('	pp = 1.0 - min(1.0, pp);');

//  Line('  pp = pp * 0.3 + 0.7;');
  //Line('	if (pp<0.99) return 0.0;  else return 1.0; 	}');

  Line('gl_FragColor = vec4(pp, pp, pp, 1.0);');

  //Line('gl_FragColor = texture2D(texture, texCoord);');
  Line('}');

  Result.XFragmentCode := S;
End;

Function GetShader_DistanceField():TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 110 }');
  Line('vertex {');
  Line('  uniform mat4 projectionMatrix;');
  Line('  varying highp vec2 texCoord;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute highp vec2 terra_UV0;');
	Line('  void main()	{');
  Line('    texCoord = terra_UV0;');
  Line('    gl_Position = projectionMatrix * terra_position;}');
  Line('}');
  Line('fragment {');
  Line('varying highp vec2 texCoord;');
  Line('uniform highp float dx, dy;');
  Line('uniform sampler2D texture;');

  Line('highp float minDist;');

  Line('void measurePixel(lowp ofst)	{');
  Line('lowp vec4 pB = texture2D(texture, st + vec2(dx, 0.0));');
  Line('}');

	Line('void main()	{');
  Line('lowp vec2 st = texCoord.st;');
  Line('  minDist = 9999.0;');

	Line('lowp vec4 pA	= texture2D(texture, st);');
  Line('lowp vec4 pB = texture2D(texture, st + vec2(dx, 0.0));');
  Line('lowp vec4 color = (pA + pB) * 0.5;');
  Line('gl_FragColor = color;');
  Line('}}');
  Result := S;
End;


End.
