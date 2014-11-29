Unit TERRA_Vegetation;
{$I terra.inc}

//http://ati.amd.com/developer/Samples/Grass.html

Interface
Uses TERRA_Utils, TERRA_Math, TERRA_GraphicsManager, TERRA_Vector2D, TERRA_Vector3D, TERRA_Matrix,
  TERRA_Color, TERRA_Texture, TERRA_BoundingBox, TERRA_Image, TERRA_Ray, TERRA_Mesh,
  TERRA_Camera, TERRA_Shader;

Type
  PVegetationVertex = ^VegetationVertex;
  VegetationVertex = Packed Record
    Position:Vector3D;
    UV:Vector2D;
    Color:TERRA_Color.Color;
  End;

  VegetationParticle = Record
    Position:Vector3D;
    Color:TERRA_Color.Color;
    Size:Single;
    GrassID:Integer;
    SortDistance:Single;
  End;

  Vegetation = Class(Renderable)
    Protected
      _Texture:Texture;
      _GrassDivisions:Integer;

      _Vertices:Array Of VegetationVertex;
      _VertexCount:Integer;

      _Particles:Array Of VegetationParticle;
      _ParticleCount:Integer;

      _BoundingBox:BoundingBox;

      _rSortAngleErrorTolerance:Single;
      _rSortSquareDistanceTolerance:Single;

      _vecLastSortViewDir:Vector3D;
      _vecLastSortCamPos:Vector3D;

      Procedure UpdateQuad(ParticleID, Offset:Integer; Angle:Single);
      Procedure AddParticle(P:Vector3D; GrassDivisions:Integer);

      Procedure SortParticles(Cam:Camera);
      Procedure QuickSort(iLo,iHi:Integer);

      Function GetNormalAt(MyMesh:Mesh; Var P, N:Vector3D):Boolean;

    Public
      UndulationIntensity:Single;
      UndulationSpeed:Integer;
      MinColor:Color;
      MaxColor:Color;
      MinSize:Single;
      MaxSize:Single;

      Constructor Create(MyMesh:Mesh; Density, Spread:Single; DensityMap:Image; GrassTexture:Texture; GrassDivisions:Integer);
      Destructor Destroy; Override;

      Function GetBoundingBox:BoundingBox; Override;
      Procedure Render; Override;
  End;

Implementation
Uses TERRA_OS, TERRA_UI, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF};

Var
  _VegetationShader:Shader;

Function GetShader_Vegetation():AnsiString;
Var
  S:AnsiString;
Procedure Line(S2:AnsiString); Begin S := S + S2 + crLf; End;
Begin
  S := '';
  Line('version { 120 }');
  Line('vertex {');
  Line('	varying mediump vec4 texCoord;');
  Line('	varying lowp vec4 diffuse;');
  Line('	varying highp vec4 world_position;');
  Line('	uniform highp vec3 cameraPosition;');
  Line('  attribute highp vec4 terra_position;');
  Line('  attribute mediump vec4 terra_UV0;');
  Line('  attribute lowp vec4 terra_color;');
  Line('	uniform mat4 cameraMatrix;');
  Line('	uniform mat4 projectionMatrix;');
  Line('	uniform mediump float undulation;');
  Line('	uniform mediump vec3 cameraRight;');
  Line('	uniform mediump vec3 cameraUp;');
  Line('	uniform highp float ratio;');
  Line('	void main()	{');
  Line('		texCoord = terra_UV0;');
  Line('		diffuse = terra_color;	');
  Line('		world_position = terra_position;');
  Line('		world_position.x += undulation * (1.0-texCoord.y) * diffuse.r;');
  Line('		gl_Position = projectionMatrix * cameraMatrix * world_position;}');
  Line('}');
  Line('fragment {');
  Line('	uniform sampler2D texture0;');
  Line('	uniform highp vec3 cameraPosition;');
  Line('	uniform lowp vec4 sunColor;');
  Line('	varying mediump vec4 texCoord;');
  Line('	varying lowp vec4 diffuse;');
  Line('	varying highp vec4 world_position;');
  Line('  lowp vec4 color;');
  	Line('  uniform highp float zFar;');
  	Line('  const highp float LOG2 = 1.442695;');
  	Line('  uniform lowp vec4 fogColor;');
	  Line('  uniform highp float fogDensity;');
  	Line('  uniform highp float fogStart;');
    Line('lowp vec3 Fog()	{');
    Line('  highp float z = length(world_position.xyz - cameraPosition) / zFar;');
    Line('  highp float fogFactor = exp2( -fogDensity * fogDensity * z * z * LOG2 );');
    Line('  fogFactor = clamp(fogFactor, 0.0, 1.0);');
    Line('  return mix(fogColor.rgb, color.rgb, fogFactor );');
	  Line('}');
  Line('	void main()	{');
  Line('	  color = texture2D(texture0, texCoord.st) * diffuse;');
  Line('    if (color.a<0.1) discard;');
  Line('    color *= sunColor;');
  Line('    color.rgb = Fog();');
  Line('		gl_FragColor = color;}');
  Line('}');
  Result := S;
End;


{ Vegetation }
Constructor Vegetation.Create(MyMesh:Mesh; Density, Spread:Single; DensityMap:Image; GrassTexture:Texture; GrassDivisions:Integer);
Var
  Box:BoundingBox;
  P, N, Size:Vector3D;
  I,J,K:Integer;
  TX, TY:Integer;
  C:Color;
  Valid:Boolean;
  Group:MeshGroup;
  Area:Single;
  V1,V2,V3:Vector3D;
  S,S1,S2,S3:Single;
  T:Triangle;
  B1,B2,B3:Single;
Begin
  _rSortAngleErrorTolerance := 0.8;
  _rSortSquareDistanceTolerance := 100;

  _GrassDivisions := GrassDivisions;
  _Texture := GrassTexture;
  _BoundingBox.Reset;
  Box := MyMesh.BoundingBox;
  Size := Box.Size;

  UndulationIntensity := 0.5;
  UndulationSpeed := 20;
  MinColor := ColorGrey(200);
  MaxColor := ColorGrey(255);

  MinSize := 2.5;
  MaxSize := 5.0;

  If Assigned(_Texture) Then
    _Texture.Wrap := False;

  For K:=0 To Pred(MyMesh.GroupCount) Do
  Begin
    Group := MyMesh.GetGroup(K);
    For I:=0 To Pred(Group.TriangleCount) Do
    Begin
      N := Group.GetTriangleNormal(I);
      If (N.Y<0.5) Then
        Continue;

      T := Group.GetTriangle(I);
      V1 := Group.Vertices[T.Indices[0]].Position;
      V2 := Group.Vertices[T.Indices[1]].Position;
      V3 := Group.Vertices[T.Indices[2]].Position;

      S1 := V1.Distance(V2);
      S2 := V2.Distance(V3);
      S3 := V3.Distance(V1);

      // calculate area
      S := (S1+S2+S3)/2;
      S1 := S - S1;
      S2 := S - S2;
      S3 := S - S3;
      Area := Sqrt(S*S1*S2*S3);

      If (Area>=1) Then
      Begin
        For J:=1 To 1 Do
        Begin
          B1 := RandomFloat(0, 1);
          B2 := RandomFloat(0, 1);
          B3 := 1 - (B1+B2);

          P.x := (b1 * V1.x) + (b2 * V2.x) + (b3 * V3.x);
          P.y := (b1 * V1.y) + (b2 * V2.y) + (b3 * V3.y);
          P.z := (b1 * V1.z) + (b2 * V2.z) + (b3 * V3.z);
          Self.AddParticle(P, GrassDivisions);
          _BoundingBox.Add(P);
        End;
      End;

    End;
  End;

      {Begin
        TX := Trunc(X * DensityMap.Width);
        TY := Trunc(Z * DensityMap.Height);
        C := DensityMap.GetPixel(TX, TY);
        Valid := (C.R>128);
      End;}
End;

Destructor Vegetation.Destroy;
Begin
  Inherited;
End;

Function Vegetation.GetBoundingBox: BoundingBox;
Begin
  Self._AlphaStage := renderAlpha2;
  Result := _BoundingBox;
End;

Procedure Vegetation.UpdateQuad(ParticleID, Offset:Integer; Angle:Single);
Const
  QuadOffsets:Array[0..3] Of Vector2D = (
    (X:-1.0; Y:1.0),
    (X: 1.0; Y:1.0),
    (X: 1.0; Y:0.0),
    (X:-1.0; Y:0.0)
    );
  QuadUVs:Array[0..3] Of Vector2D = (
    (X:0.0; Y:0.0),
    (X: 1.0; Y:0.0),
    (X: 1.0; Y:1.0),
    (X:0.0; Y:1.0)
    );
Var
  H:Single;
  N, I, GID:Integer;
  C:Color;
Begin
  N := ParticleID*6*3 + Offset;

  If (_VertexCount<N+1) Then
  Begin
    _VertexCount := N + 6*3;
    SetLength(_Vertices, _VertexCount);
  End;
  H := 2;

  _Vertices[N+0].UV := QuadUVs[0];
  _Vertices[N+0].Position := VectorCreate(QuadOffsets[0].X, QuadOffsets[0].Y*H, 0.0);
  _Vertices[N+5] := _Vertices[N+0] ;

  _Vertices[N+1].UV := QuadUVs[1];
  _Vertices[N+1].Position := VectorCreate(QuadOffsets[1].X, QuadOffsets[1].Y*H, 0.0);

  _Vertices[N+2].UV := QuadUVs[2];
  _Vertices[N+2].Position := VectorCreate(QuadOffsets[2].X, QuadOffsets[2].Y*H, 0.0);
  _Vertices[N+3] := _Vertices[N+2] ;

  _Vertices[N+4].UV := QuadUVs[3];
  _Vertices[N+4].Position := VectorCreate(QuadOffsets[3].X, QuadOffsets[3].Y*H, 0.0);

  For I:=N To N+5 Do
  Begin
    _Vertices[I].Color := _Particles[ParticleID].Color;

    _Vertices[I].Position.Rotate(VectorUp, Angle);

    _Vertices[I].Position.Scale(_Particles[ParticleID].Size);
    _Vertices[I].Position.Add(_Particles[ParticleID].Position);

    _Vertices[I].UV.X := _Vertices[I].UV.X/_GrassDivisions;
    _Vertices[I].UV.X := _Vertices[I].UV.X + (1/_GrassDivisions)* _Particles[ParticleID].GrassID;
  End;
End;

Procedure Vegetation.AddParticle(P:Vector3D; GrassDivisions:Integer);
Var
  N:Integer;
  C:Color;
Begin
  N := _ParticleCount;
  Inc(_ParticleCount);
  SetLength(_Particles, _ParticleCount);
  P.Y := P.Y-0.5;

  _Particles[N].Position := P;
  _Particles[N].GrassID := Random(_GrassDivisions);
  _Particles[N].Size := RandomFloat(MinSize, MaxSize);
  C.R := Trunc(RandomFloat(MinColor.R, MaxColor.R));
  C.G := Trunc(RandomFloat(MinColor.G, MaxColor.G));
  C.B := Trunc(RandomFloat(MinColor.B, MaxColor.B));
  C.A := 255;
  _Particles[N].Color := C;

  UpdateQuad(N, 0, 0*RAD);
  UpdateQuad(N, 6, 45*RAD);
  UpdateQuad(N, 12, 135*RAD);
End;

Procedure Vegetation.Render;
Var
  I:Integer;
  P:Vector3D;
  Ratio:Single;
  Right, Up:Vector3D;
  Tex:Texture;
  PositionHandle, UVHandle, ColorHandle:Integer;
  Undulation:Single;
  rSquareDistanceSinceLastSort:Single;
  rCosAngleSinceLastSort:Single;
  Cam:Camera;
Begin
  Cam := GraphicsManager.Instance.ActiveViewport.Camera;

  rCosAngleSinceLastSort := VectorDot(_vecLastSortViewDir, Cam.View); // dot product
  rSquareDistanceSinceLastSort := Cam.Position.Distance(_vecLastSortCamPos);

  If (rCosAngleSinceLastSort < _rSortAngleErrorTolerance) Or (rSquareDistanceSinceLastSort > _rSortSquareDistanceTolerance) Then
  Begin
    // sort the particles from back to front wrt the camera position.
    SortParticles(Cam);
                                      
    _vecLastSortViewDir := Cam.View;
    _vecLastSortCamPos := Cam.Position;
  End;

  If (_VegetationShader = Nil) Then
  Begin
    _VegetationShader := TERRA_Shader.Shader.CreateFromString(GetShader_Vegetation(), 'vegetation');
    ShaderManager.Instance.AddShader(_VegetationShader);
  End;

  If GraphicsManager.Instance.LandscapeOrientation Then
  Begin
    Up := GraphicsManager.Instance.ActiveViewport.Camera.Right;
    Right := GraphicsManager.Instance.ActiveViewport.Camera.Up;
    Up.Scale(-1.0);
    Ratio := UI.Instance.Width/UI.Instance.Height;
  End Else
  Begin
    Right := GraphicsManager.Instance.ActiveViewport.Camera.Right;
    Up := GraphicsManager.Instance.ActiveViewport.Camera.Up;
    Ratio := UI.Instance.Height/UI.Instance.Width;
  End;

  Up := VectorUp;
  Right := VectorCreate(1, 0, 0);

  ShaderManager.Instance.Bind(_VegetationShader);

  PositionHandle := _VegetationShader.GetAttribute('terra_position');
  UVHandle := _VegetationShader.GetAttribute('terra_UV0');
  ColorHandle := _VegetationShader.GetAttribute('terra_color');

  If (PositionHandle<0) Then
    Exit;

  GraphicsManager.Instance.ActiveViewport.Camera.SetupUniforms;

  Undulation := Cos(((GetTime Div UndulationSpeed) Mod 360)*RAD) * UndulationIntensity;

  _VegetationShader.SetUniform('cameraUp', Up);
  _VegetationShader.SetUniform('cameraRight', Right);
  _VegetationShader.SetUniform('texture0', 0);
  _VegetationShader.SetUniform('ratio', Ratio);
  _VegetationShader.SetUniform('undulation', Undulation);

  GraphicsManager.Instance.SetBlendMode(blendBlend);

  Texture.Bind(_Texture);
  glDepthMask(False);                                            
  glDisable(GL_CULL_FACE);

  glVertexAttribPointer(PositionHandle, 3, GL_FLOAT, False, SizeOf(VegetationVertex), @(_Vertices[0].Position));    
  glVertexAttribPointer(UVHandle, 2, GL_FLOAT, False, SizeOf(VegetationVertex), @(_Vertices[0].UV));    
  glVertexAttribPointer(ColorHandle, 4, GL_UNSIGNED_BYTE, True, SizeOf(VegetationVertex), @(_Vertices[0].Color));    

  glDrawArrays(GL_TRIANGLES, 0, _VertexCount);
  GraphicsManager.Instance.Internal(0, _VertexCount Div 3);

  glEnable(GL_CULL_FACE);
  glDepthMask(True);                                            
  GraphicsManager.Instance.SetBlendMode(blendNone);
End;

Function Vegetation.GetNormalAt(MyMesh: Mesh; var P,N: Vector3D): Boolean;
Var
  Group:MeshGroup;
  I:Integer;
  R:Ray;
  A,B,C:Vector3D;
  TT, U, V:Single;
  T:Triangle;
Begin
  Result := False;
  R.Origin := P;
  R.Direction := VectorCreate(0, 1, 0);
  Group := MyMesh.GetGroup(0);
  For I:=0 To Pred(Group.TriangleCount) Do
  Begin
    T := Group.GetTriangle(I);
    A := Group.GetVertex(T.Indices[0]).Position;
    B := Group.GetVertex(T.Indices[1]).Position;
    C := Group.GetVertex(T.Indices[2]).Position;
    If R.TriangleIntersect(A, B, C, TT, U, V) Then
    Begin
      P := R.IntersectionPoint(TT);
      N := Group.GetTriangleNormal(I);
      Result := True;
      Exit;
    End;
  End;
End;

Procedure Vegetation.QuickSort(iLo,iHi:Integer);
Var
  Lo, Hi: Integer;
  Mid, Temp:VegetationParticle;
Begin
    If iHi<iLo Then
      Exit;
    Lo := iLo;
    Hi := iHi;
    Mid := _Particles[(Lo + Hi) Shr 1];
    Repeat
      While Mid.SortDistance<_Particles[Lo].SortDistance Do Inc(Lo);
      While _Particles[Hi].SortDistance<Mid.SortDistance Do Dec(Hi);

      If Lo <= Hi Then
      Begin
        Temp := _Particles[Lo];
        _Particles[Lo] := _Particles[Hi];
        _Particles[Hi] := Temp;
        Inc(Lo);
        Dec(Hi);
      End;
    Until Lo > Hi;
    If Hi > iLo Then
      QuickSort(iLo, Hi);
    If Lo < iHi Then
      QuickSort(Lo, iHi);
End;

Procedure Vegetation.SortParticles(Cam: Camera);
Var
  I:Integer;
  P:Vector3D;
Begin
  P := Cam.Position;
  For I:=0 To Pred(_ParticleCount) Do
    _Particles[I].SortDistance := P.Distance(_Particles[I].Position);

  Self.QuickSort(0, Pred(_ParticleCount));

  For I:=0 To Pred(_ParticleCount) Do
  Begin
    UpdateQuad(I, 0, 0*RAD);
    UpdateQuad(I, 6, 45*RAD);
    UpdateQuad(I, 12, 135*RAD);
  End;
End;

End.