Unit TERRA_ColorTable;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Texture, TERRA_Shader, TERRA_Color, TERRA_Image, TERRA_Vector3D;

Const
  ColorTableUniformName = 'color_table_texture';

Type
  ColorTransform = Function (N:Color; Userdata:Pointer):Color; Cdecl;

  Function CreateColorTable(Size:Integer; Transform:ColorTransform = Nil; Userdata:Pointer = Nil):Image;
  Function ColorTableLookUp(ColorTable:Image; Source:Color):Color;
  Procedure ColorTableTransform(Dest, ColorTable:Image);

  Function GetColorTableShaderCode():AnsiString;
  Function ColorTableBind(ColorTableTex:Texture; Slot:Integer):Boolean;

Implementation
Uses TERRA_OS, TERRA_Log;

Function CreateColorTable(Size:Integer; Transform:ColorTransform; Userdata:Pointer):Image;
Var
  Scale:Single;
  R,G,B:Integer;
  Temp:Color;
Begin
  Result := Image.Create(Size * Size, Size);
  Scale := 255/Pred(Size);
  For R:=0 To Pred(Size) Do
    For G:=0 To Pred(Size) Do
      For B:=0 To Pred(Size) Do
      Begin
        Temp := ColorCreate(Byte(Trunc(R*Scale)), Byte(Trunc(G*Scale)), Byte(Trunc(B*Scale)), 255);
        If Assigned(Transform) Then
          Temp := Transform(Temp, Userdata);

        Result.SetPixel(R+B*Size, G, Temp);
      End;
End;

Procedure ColorTableTransform(Dest, ColorTable:Image);
Var
  I,J:Integer;
  P:Color;
Begin
  For J:=0 To Pred(Dest.Height) Do
    For I:=0 To Pred(Dest.Width) Do
    Begin
      P := Dest.GetPixel(I,J);
      P := ColorTableLookUp(ColorTable, P);
      Dest.SetPixel(I, J, P);
    End;
End;

Function ColorTableLookUp(ColorTable:Image; Source:Color):Color;
Var
  R1,G1,B1:Single;
  R2,G2,B2:Single;
  DR,DG,DB:Single;
  Scale:Single;
Begin
  If (ColorTable = Nil) Then
  Begin
    Result := Source;
    Exit;
  End;

  Scale := 1/(255/ColorTable.Height);

  DR := Frac(Source.R * Scale);
  DG := Frac(Source.G * Scale);
  DB := Frac(Source.B * Scale);

  R1 := Trunc(Source.R * Scale);
  G1 := Trunc(Source.G * Scale);
  B1 := Trunc(Source.B * Scale);

  R2 := Round(Source.R * Scale);
  G2 := Round(Source.G * Scale);
  B2 := Round(Source.B * Scale);

  Result := ColorTable.GetPixel(Trunc(R1*(1.0-DR) + R2*DR) + Trunc(B1*(1.0-DB) + B2*DB) * ColorTable.Height, Trunc(G1*(1.0-DG) + G2*DG));
End;

Function ColorTableBind(ColorTableTex:Texture; Slot:Integer):Boolean;
Var
  MyShader:Shader;
  Scale, Elements:Single;
Begin
  Result := False;

  MyShader := ShaderManager.Instance.ActiveShader;
  If (MyShader = Nil) Then
    Exit;

  If (Not MyShader.HasUniform(ColorTableUniformName)) Then
    Exit;

  If (ColorTableTex = Nil) Or (Not ColorTableTex.IsReady()) Then
  Begin
    If (ColorTableTex<>TextureManager.Instance.DefaultColorTable) Then
      ColorTableBind(TextureManager.Instance.DefaultColorTable, Slot);
    Exit;
  End;

  Scale := SafeDiv(256, ColorTableTex.Height);
  Elements := ColorTableTex.Height;

  MyShader.SetUniform(ColorTableUniformName, Slot);
  MyShader.SetUniform('color_table_elements', Elements);
  MyShader.SetUniform('color_table_scale', Scale);
  MyShader.SetUniform('color_table_clamp', VectorUniform(Elements-1));

  ColorTableTex.BilinearFilter := True;
  ColorTableTex.Wrap := False;
  ColorTableTex.MipMapped := False;
  ColorTableTex.Bind(Slot);
  Result := True;
End;

Function GetColorTableShaderCode():AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
	Line('  uniform sampler2D color_table_texture;');
  Line('  uniform mediump float color_table_elements;');
  Line('  uniform mediump float color_table_scale;');
  Line('  uniform mediump vec3 color_table_clamp;');
	Line('lowp vec3 ColorTableLookup(mediump vec3 color)	{');
  Line('  mediump float base_value = 255.0; ');
  Line('  mediump vec3 rescaler = vec3(base_value, base_value, base_value);');
  Line('  color *= rescaler;');
  Line('  color /= color_table_scale;');

  Line('  mediump vec3 delta = fract(color);');
  Line('  mediump vec3 rgb1 = floor(color);');
  Line('  mediump vec3 rgb2 = ceil(color);');
  Line('  rgb2 = min(rgb2, color_table_clamp);');

  // loop up first color
  Line('  mediump vec2 ofs = vec2(rgb1.r + rgb1.b * color_table_elements, rgb1.g);');
  Line('  ofs.x /= (color_table_elements*color_table_elements);');
  Line('  ofs.y /= color_table_elements;');
  Line('  mediump vec3 temp = texture2D(color_table_texture, ofs).rgb;');

  // loop up second color
  Line('  ofs = vec2(rgb2.r + rgb2.b * color_table_elements, rgb2.g);');
  Line('  ofs.x /= (color_table_elements*color_table_elements);');
  Line('  ofs.y /= color_table_elements;');
  Line('  mediump vec3 temp2 = texture2D(color_table_texture, ofs).rgb;');

  // interpolate
  Line('return mix(temp, temp2, delta);	}');

  Result := S;
End;

End.