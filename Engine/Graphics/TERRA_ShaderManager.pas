Unit TERRA_ShaderManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Renderer;

Function Get_SkyboxDiffuseShader():ShaderInterface;
Function Get_SkyboxNormalShader():ShaderInterface;

Function GetShader_Sprite(SpriteFlags:Cardinal):TERRAString;

Implementation
Uses TERRA_OS, TERRA_ShaderFactory, TERRA_Sprite, TERRA_GraphicsManager
{$IFNDEF DISABLECOLORGRADING},TERRA_ColorGrading {$ENDIF};

Var
  _SkyboxDiffuseShader:ShaderInterface = Nil;
  _SkyboxNormalShader:ShaderInterface = Nil;

Function GetShader_Skybox(OutputMode:Integer):TERRAString;
Var
  S:TERRAString;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
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
  Line('}');
  Line('fragment {');
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

  Line('}');
  Result := S;
End;

Function Get_SkyboxNormalShader():ShaderInterface;
Begin
  If (_SkyboxNormalShader = Nil) Then
  Begin
    _SkyboxNormalShader := GraphicsManager.Instance.Renderer.CreateShader();
    _SkyboxNormalShader.Generate('skybox_normal', GetShader_Skybox(shader_OutputNormal));
  End;

  Result := _SkyboxNormalShader;
End;

Function Get_SkyboxDiffuseShader():ShaderInterface;
Begin
  If (_SkyboxDiffuseShader = Nil) Then
  Begin
    _SkyboxDiffuseShader := GraphicsManager.Instance.Renderer.CreateShader();
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

Function GetShader_Sprite(SpriteFlags:Cardinal):TERRAString;
Var
  S:TERRAString;
  DoColorGrading, IsFont, IsSolid, IsDissolve:Boolean;
Procedure Line(S2:TERRAString); Begin S := S + S2 + crLf; End;
Begin
  DoColorGrading := (SpriteFlags And Sprite_ColorGrading)<>0;
  IsFont := (SpriteFlags And Sprite_Font)<>0;
  IsSolid := (SpriteFlags And Sprite_SolidColor)<>0;
  IsDissolve := (SpriteFlags And Sprite_Dissolve)<>0;

  S := '';
  Line('vertex {');
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
//  Line('  uniform mat4 modelMatrix;');

	Line('void main()	{');
  Line('  vec4 local_position = vec4(terra_position.x, terra_position.y, terra_position.z, 1.0);');
  Line('  screen_position = local_position.xy;');
  Line('  gl_Position =  projectionMatrix * local_position;');
  Line('  texCoord = terra_UV0;');
  Line('  clipRect = terra_UV1;');
  Line('  fillColor = terra_UV2;');
  Line('  color = terra_color;}');
  Line('}');

  Line('fragment {');

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

  Line('  alpha *=  color.a;');
  Line('  alpha *= alphaCut;');
  Line('    gl_FragColor = vec4( baseColor.rgb, alpha);');

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

    //Line('    if (c.a<0.1) discard;');
    Line('    gl_FragColor = c;}');
  End;

  Line('}  ');
  Result := S;
End;


End.
