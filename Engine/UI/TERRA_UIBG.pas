Unit TERRA_UIBG;

{$I terra.inc}
Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Math, TERRA_Texture, TERRA_Vector2D, TERRA_Vector3D, TERRA_Color, TERRA_Quaternion,
  TERRA_SpriteManager;
    
Type
  UIBackground = Class(TERRAObject)
    Public
      Procedure Render; Virtual; Abstract;
      Destructor Destroy; Override;
  End;

  TilingBackground = Class(UIBackground)
    Protected
      _Z:Single;
      _Texture:Texture;
      _ScrollSpeed:Single;
      _ScrollDir:Vector2D;
      _Scale:Vector2D;
      _VertexList:Array[0..5] Of SpriteVertex;

    Public
      Color:TERRA_Color.Color;

      Constructor Create(MyTexture:Texture; ScrollDirection:Vector2D; ScrollSpeed:Single; Scale:Vector2D);
      Procedure Render; Override;

      Procedure Remap(SU, SV:Single);
      Procedure SetLayer(Z:Single);

      Property Texture:TERRA_Texture.Texture Read _Texture Write _Texture;
      Property ScrollSpeed:Single Read _ScrollSpeed Write _ScrollSpeed;
      Property ScrollDirection:Vector2D Read _ScrollDir Write _ScrollDir;
  End;

  FrameBackground = Class(UIBackground)
    Protected
      _Texture:Texture;
      _VertexList:Array[0..5] Of SpriteVertex;

    Public
      BlendMode:Integer;
      Color:TERRA_Color.Color;

      Constructor Create(MyTexture:Texture);
      Procedure Render; Override;

      Property Texture:TERRA_Texture.Texture Read _Texture Write _Texture;
  End;

Implementation
Uses TERRA_UI, TERRA_Application, TERRA_GraphicsManager, TERRA_OS,
  {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Shader, TERRA_Matrix, Math;

Function GetShader_TileBG:AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('vertex {');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec4 terra_UV0;');
  Line('  attribute lowp vec4 terra_color;');
  Line('  uniform mat4 projectionMatrix;');
  Line('  uniform mat4 UVtransform;');
  Line('  varying mediump vec4 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('void main()	{');
  Line('  texCoord = UVtransform * terra_UV0;');
  Line('  gl_Position = projectionMatrix * terra_position;');
  Line('  color = terra_color;}');
  Line('}');
  Line('fragment {');
  Line('  varying mediump vec4 texCoord;');
	Line('  varying lowp vec4 color;');
	Line('  uniform sampler2D texture;');
	Line('  void main()	{');
  Line('gl_FragColor = color * texture2D(texture, fract(texCoord.xy));}');
  Line('}  ');
  Result := S;
End;

{ UIBackground }
Destructor UIBackground.Destroy;
Begin
  // do nothing
End;

{ TilingBackground }
Constructor TilingBackground.Create(MyTexture:Texture; ScrollDirection:Vector2D; ScrollSpeed:Single; Scale:Vector2D);
Begin
  Color := ColorWhite;
  _Texture := MyTexture;

  _ScrollSpeed := ScrollSpeed;
  _ScrollDir := ScrollDirection;
  _Scale := Scale;
End;

Procedure TilingBackground.SetLayer(Z:Single);
Var
  W,H:Integer;
Begin
  W := UIManager.Instance.Width;
  H := UIManager.Instance.Height;

  _VertexList[0].Position := VectorCreate(0, H, Z);
  _VertexList[1].Position := VectorCreate(W, H, Z);
  _VertexList[2].Position := VectorCreate(W, 0, Z);
  _VertexList[3] := _VertexList[2];
  _VertexList[4].Position := VectorCreate(0, 0, Z);
  _VertexList[5] := _VertexList[0];
End;

Procedure TilingBackground.Remap(SU, SV: Single);
Begin
  _VertexList[0].TexCoord := VectorCreate2D(0, SV);
  _VertexList[1].TexCoord := VectorCreate2D(SU, SV);

  _VertexList[2].TexCoord := VectorCreate2D(SU, 0);
  _VertexList[3] := _VertexList[2];

  _VertexList[4].TexCoord := VectorCreate2D(0, 0);
  _VertexList[5] := _VertexList[0];
End;

Var
  _TileBGShader:Shader = Nil;

Procedure TilingBackground.Render;
Const
  MaxOfs = 100;

Var
  Ratio, AnimationOffset:Single;
  I:Integer;
  PositionHandle, UVHandle, ColorHandle:Integer;
  _Shader:Shader;
  Scroll, Rot:Vector3D;
  Transform:Matrix;
  M:Matrix;
Begin
  If Not Assigned(_Texture) Then
    Exit;

  _Texture.BilinearFilter := False;
  _Texture.MipMapped := False;
  _Texture.Wrap := True;

  SetLayer(-99);
  Remap(_Scale.X, _Scale.Y);

  Texture.BilinearFilter := False;

  For I:=0 To 5 Do
    _VertexList[I].Color := Color;

  AnimationOffset := _ScrollSpeed * (GetTime / 1000);

  While (AnimationOffset>MaxOfs) Do
    AnimationOffset := AnimationOffset - MaxOfs;

  While (AnimationOffset<0) Do
    AnimationOffset := AnimationOffset + MaxOfs;

  _Texture.Bind(0);

  If (IsLandscapeOrientation(Application.Instance.Orientation)) Then
    Ratio := GraphicsManager.Instance.Height / GraphicsManager.Instance.Width
  Else
    Ratio := GraphicsManager.Instance.Width / GraphicsManager.Instance.Height;

  If (Not Assigned(_TileBGShader)) Then
  Begin
    _TileBGShader := Shader.CreateFromString(GetShader_TileBG(), 'UIBG');
    ShaderManager.Instance.AddShader(_TileBGShader);
  End;

  Scroll := VectorCreate(_ScrollDir.X * AnimationOffset, _ScrollDir.Y * AnimationOffset, 1.0);

{  If (GraphicsManager.Instance.LandscapeOrientation) Then
    Rot := VectorCreate(0, 0.0, -90*RAD)
  Else}
    Rot := VectorZero;

  Transform := MatrixTransform(Scroll, Rot, VectorCreate(Ratio, 1.0, 1.0));

  _Shader := _TileBGShader;
  M := GraphicsManager.Instance.ProjectionMatrix;
  ShaderManager.Instance.Bind(_Shader);
  _Shader.SetUniform('texture', 0);
  _Shader.SetUniform('projectionMatrix', M);
  _Shader.SetUniform('UVtransform', Transform);

  PositionHandle := _Shader.GetAttribute('terra_position');
  UVHandle := _Shader.GetAttribute('terra_UV0');
  ColorHandle := _Shader.GetAttribute('terra_color');

  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@M);
    glMatrixMode(GL_MODELVIEW);
    M := MatrixIdentity;
    glLoadMatrixf(@M);
    glMatrixMode(GL_TEXTURE);
    M := MatrixTranslation(Scroll.X, Scroll.Y, 1.0);
    glLoadMatrixf(@M);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glVertexPointer(3, GL_FLOAT, SizeOf(SpriteVertex), @_VertexList[0].Position);
    glColorPointer(4, GL_UNSIGNED_BYTE, SizeOf(SpriteVertex), @_VertexList[0].Color);
    glTexCoordPointer(3, GL_FLOAT, SizeOf(SpriteVertex), @_VertexList[0].TexCoord);
  End Else
  {$ENDIF}
  Begin
    glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(SpriteVertex), @(_VertexList[0].Position));    
    glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, SizeOf(SpriteVertex), @(_VertexList[0].TexCoord));  
    glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(SpriteVertex), @(_VertexList[0].Color));         
  End;

  glDrawArrays(GL_TRIANGLES, 0, 6);                                                 
  GraphicsManager.Instance.Internal(0, 2);

  {$IFDEF PC}
  If (Not GraphicsManager.Instance.Settings.Shaders.Avaliable) Then
  Begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  End;
  {$ENDIF}
End;

{ FrameBackground }
Constructor FrameBackground.Create(MyTexture:Texture);
Var
  W,H:Integer;
  Z:Single;
Begin
  _Texture := MyTexture;
  _Texture.Wrap := False;

  W := UIManager.Instance.Width;
  H := UIManager.Instance.Height;
  Z := -99;

  _VertexList[0].Position := VectorCreate(0, H, Z);
  _VertexList[0].TexCoord := VectorCreate2D(0, 1.0);
  _VertexList[0].Color := ColorWhite;

  _VertexList[1].Position := VectorCreate(W, H, Z);
  _VertexList[1].TexCoord := VectorCreate2D(1.0, 1.0);
  _VertexList[1].Color := ColorWhite;

  _VertexList[2].Position := VectorCreate(W, 0, Z);
  _VertexList[2].TexCoord := VectorCreate2D(1.0, 0);
  _VertexList[2].Color := ColorWhite;
  _VertexList[3] := _VertexList[2];

  _VertexList[4].Position := VectorCreate(0, 0, Z);
  _VertexList[4].TexCoord := VectorCreate2D(0, 0);
  _VertexList[4].Color := ColorWhite;
  _VertexList[5] := _VertexList[0];

  Color := ColorWhite;
  BlendMode := blendBlend;
End;

Procedure FrameBackground.Render;
Var
  AnimationOffset:Single;
//  PositionHandle, UVHandle, ColorHandle:Integer;
Begin
  If (Not Assigned(_Texture)) Or (Color.A<=0) Then
    Exit;

  GraphicsManager.Instance.SetBlendMode(blendNone);
  GraphicsManager.Instance.SetBlendMode(BlendMode);

(*  glColor4ub(Color.R, Color.G, Color.B, Color.A);
  _Texture.Bind(_Texture, 0);

  glActiveTexture(GL_TEXTURE0);

  glDisable(GL_DEPTH_TEST);

  glClientActiveTexture(GL_TEXTURE0);                 
  glEnableClientState( GL_TEXTURE_COORD_ARRAY );      

  glVertexPointer(3, GL_FLOAT, SizeOf(UIVertex), @(_VertexList[0].Position));    
  glTexCoordPointer(2, GL_FLOAT, SizeOf(UIVertex), @(_VertexList[0].TexCoord));  

  glDrawArrays(GL_TRIANGLES, 0, 6);                                                 
  GraphicsManager.Instance.Internal(0, 2);


  glClientActiveTexture(GL_TEXTURE0);                                           
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );                               

  glEnable(GL_DEPTH_TEST);
  *)
End;

End.
