Unit TERRA_WeatherFX;

{$I terra.inc}
Interface
Uses TERRA_ScreenFX;

Type
  SnowFallFX  = Class(ScreenFX)
    Protected
      _Strength:Integer;

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

(*


		Line('vec2 mod289(vec2 x) {  return x - floor(x * (1.0 / 289.0)) * 289.0;	}');

		Line('vec3 mod289(vec3 x) {	return x - floor(x * (1.0 / 289.0)) * 289.0;	}');

		Line('vec4 mod289(vec4 x) {	return x - floor(x * (1.0 / 289.0)) * 289.0;	}');

		Line('vec3 permute(vec3 x) { return mod289(((x*34.0)+1.0)*x);	}');

		Line('vec4 permute(vec4 x) { return mod((34.0 * x + 1.0) * x, 289.0); }');

		Line('vec4 taylorInvSqrt(vec4 r) {	return 1.79284291400159 - 0.85373472095314 * r;	}');

		Line('float snoise(vec2 v) {');
    Line('  const vec4 C = vec4(0.211324865405187,0.366025403784439,-0.577350269189626,0.024390243902439);');
		Line('  vec2 i  = floor(v + dot(v, C.yy) );');
		Line('  vec2 x0 = v -   i + dot(i, C.xx);');
    Line('  vec2 i1;');
    Line('  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);');
    Line('  vec4 x12 = x0.xyxy + C.xxzz;');
    Line('  x12.xy -= i1;');

    Line('  i = mod289(i);'); // Avoid truncation effects in permutation
    Line('  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 )) + i.x + vec3(0.0, i1.x, 1.0 ));');

    Line('  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);');
    Line('  m = m*m ;');
    Line('  m = m*m ;');

    Line('  vec3 x = 2.0 * fract(p * C.www) - 1.0;');
    Line('  vec3 h = abs(x) - 0.5;');
    Line('  vec3 ox = floor(x + 0.5);');
    Line('  vec3 a0 = x - ox;');

    Line('  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );');

    Line('  vec3 g;');
    Line('  g.x  = a0.x  * x0.x  + h.x  * x0.y;');
    Line('  g.yz = a0.yz * x12.xz + h.yz * x12.yw;');

    Line('  return 130.0 * dot(m, g);		}');

(*		Line('float cellular2x2(vec2 P)	{');
    Line('  #define K 0.142857142857'); // 1/7
    Line('  #define K2 0.0714285714285'); // K/2
    Line('  #define jitter 0.8'); // jitter 1.0 makes F1 wrong more often

    Line('  vec2 Pi = mod(floor(P), 289.0);');
    Line('  vec2 Pf = fract(P);');
    Line('  vec4 Pfx = Pf.x + vec4(-0.5, -1.5, -0.5, -1.5);');
    Line('  vec4 Pfy = Pf.y + vec4(-0.5, -0.5, -1.5, -1.5);');
    Line('  vec4 p = permute(Pi.x + vec4(0.0, 1.0, 0.0, 1.0));');
    Line('  p = permute(p + Pi.y + vec4(0.0, 0.0, 1.0, 1.0));');
    Line('  vec4 ox = mod(p, 7.0)*K+K2;');
    Line('  vec4 oy = mod(floor(p*K),7.0)*K+K2;');
    Line('  vec4 dx = Pfx + jitter*ox;');
    Line('  vec4 dy = Pfy + jitter*oy;');
    Line('  vec4 d = dx * dx + dy * dy;'); // d11, d12, d21 and d22, squared
    // Sort out the two smallest distances

		// Cheat and pick only F1
    Line('  d.xy = min(d.xy, d.zw);');
    Line('  d.x = min(d.x, d.y);');
    Line('  return d.x;}'); // F1 duplicated, F2 not computed

		Line('  float fbm(vec2 p) {');
    Line('  float f = 0.0;');
    Line('  float w = 0.5;');
    Line('  for (int i = 0; i < 5; i ++) {');
    Line('    f += w * snoise(p);');
		Line('    p *= 2.;');
    Line('    w *= 0.5;} ');
    Line('  return f;		}');
*)

{ SnowFallFX }
Constructor SnowFallFX.Create(Strength: Single);
Begin
  _Strength := Self.AddUniform('mod_snow', uniformFloat);
  Self.SetStrength(Strength);
End;

Function SnowFallFX.RequiresFunction(FXFunction: ScreenFXFunctionType): Boolean;
Begin
  Result := (FXFunction = fxCellularNoise);
End;

Function SnowFallFX.GetStrength: Single;
Begin
  Self.GetFloat(_Strength, Result);
End;

Procedure SnowFallFX.SetStrength(const Value: Single);
Begin
  Self.SetFloat(_Strength, Value);
End;

Procedure SnowFallFX.GenerateCode;
Begin
  Line('  output_color.rgb += snowFall(output_uv.xy);');
  //Line('  output_color.rgb = vec3(cellular2x2(output_uv.xy * 10.0));');
End;

Procedure SnowFallFX.GenerateFunctions;
Begin
 // This shader useds noise shaders by stegu -- http://webstaff.itn.liu.se/~stegu/
 // This is supposed to look like snow falling, for example like http://24.media.tumblr.com/tumblr_mdhvqrK2EJ1rcru73o1_500.gif
  Line('  vec3 snowFall(vec2 uv) {');
  Line('float speed = 1.0;');
  Line('uv.x *= (screenResolution.x / screenResolution.y);');
  Line('float sunsh= 0.0;');
  Line('vec2 GA;');
  Line('GA.x-= globalTime*1.8;');
  Line('GA.y+= globalTime*0.9;');
  Line('GA *= speed;');

  Line('float F1=0.0,F2=0.0,F3=0.0,F4=0.0,F5=0.0,N1=0.0,N2=0.0,N3=0.0,N4=0.0,N5=0.0;');
  Line('float A=0.0,A1=0.0,A2=0.0,A3=0.0,A4=0.0,A5=0.0;');


  // Attentuation
	Line('A = (uv.x-(uv.y*0.3));');
  Line('A = clamp(A,0.0,1.0);');

  Line('lowp float octave = 0.5;');

  // Snow layers, somewhat like an fbm with worley layers.
	Line('F1 = 1.0 - pow(cellNoise(octave * (uv+(GA*0.1))*8.0), mod_snow);');
  Line('A1 = 1.0 - (A*1.0);');
  Line('N1 = smoothstep(0.998,1.0,F1)*1.0*A1;');

  Line('F2 = 1.0 - pow(cellNoise(octave * (uv+(GA*0.2))*6.0), mod_snow);');
  Line('A2 = 1.0-(A*0.8);');
  Line('N2 = smoothstep(0.995,1.0,F2)*0.85*A2;');

	Line('F3 = 1.0 - pow(cellNoise(octave * (uv+(GA*0.4))*4.0), mod_snow);');
  Line('A3 = 1.0-(A*0.6);');
  Line('N3 = smoothstep(0.99,1.0,F3)*0.65*A3;');

  Line('F4 = 1.0 - pow(cellNoise(octave * (uv+(GA*0.6))*3.0), mod_snow);');
  Line('A4 = 1.0-(A*1.0);');
  Line('N4 = smoothstep(0.98,1.0,F4)*0.4*A4;');

  Line('F5 = 1.0 - pow(cellNoise(octave * (uv+(GA))*1.2), mod_snow);');
  Line('A5 = 1.0-(A*1.0);');
  Line('N5 = smoothstep(0.98,1.0,F5)*0.25*A5;');

  Line('float snow_out=N5;');

  Line('snow_out = (sunsh*0.6)+N1+N2+N3+N4+N5;');

  Line(' return vec3(snow_out*0.9, snow_out, snow_out*1.1);');
  Line('  }');
End;

End.