Unit TERRA_KurawasaFilter;

Interface
Uses TERRA_Image, TERRA_Color;

Function ApplyKurawasaFilter(Src:TERRAImage; Radius:Integer = 7):TERRAImage;

Implementation
Uses TERRA_Vector2D, TERRA_Vector3D;

Function GetKurawasaColor(X,Y:Integer; Src:TERRAImage; Radius:Integer):ColorRGBA;
Var
  fragCoord:Vector2D;
  I, J:Integer;
  N:Single;
  m0, m1, m2, m3:Vector3D;
  s0, s1, s2, s3:Vector3D;
  c:Vector3D;
  min_sigma2, sigma2:Single;
Begin
  fragCoord.X := X / Src.Width;
  fragCoord.Y := Y / Src.Height;

  //vec2 uv = fragCoord.xy/iResolution.xy;
  n := ((radius + 1) * (radius + 1));

  m0 := Vector3D_Zero;
  m1 := Vector3D_Zero;
  m2 := Vector3D_Zero;
  m3 := Vector3D_Zero;
  s0 := Vector3D_Zero;
  s1 := Vector3D_Zero;
  s2 := Vector3D_Zero;
  s3 := Vector3D_Zero;

  For J:=-Radius To 0 Do
    For I:=-Radius To 0 Do
    Begin
      c := ColorToVector3D( Src.GetPixel(X + i, Y + j));
      m0.Add(c);
      s0.Add(Vector3D_Multiply(c, c));
    End;

  For J:=-Radius To 0 Do
    For I:=0 To Radius Do
    Begin
      c := ColorToVector3D( Src.GetPixel(X + i, Y + j));
      m1.Add(c);
      s1.Add(Vector3D_Multiply(c, c));
    End;

  For J:=0 To Radius Do
    For I:=0 To Radius Do
    Begin
      c := ColorToVector3D( Src.GetPixel(X + i, Y + j));
      m2.Add(c);
      s2.Add(Vector3D_Multiply(c, c));
    End;

  For J:=0 To Radius Do
    For I:=-Radius To 0 Do
    Begin
      c := ColorToVector3D( Src.GetPixel(X + i, Y + j));
      m3.Add(c);
      s3.Add(Vector3D_Multiply(c, c));
    End;


    min_sigma2 := 1e+2;
    m0 := Vector3D_Scale(m0, 1/ n);
    s0 := Vector3D_Subtract(Vector3D_Scale(s0, 1 / n), Vector3D_Multiply(m0, m0));
    s0 := S0.Abs();

    sigma2 := s0.x + s0.y + s0.z;
    if (sigma2 < min_sigma2) Then
    Begin
      min_sigma2 := sigma2;
      Result := ColorCreateFromVector3D(m0);
    End;

    m1 := Vector3D_Scale(m1, 1/ n);
    s1 := Vector3D_Subtract(Vector3D_Scale(s1, 1 / n), Vector3D_Multiply(m1, m1));
    s1 := S1.Abs();

    sigma2 := s1.x + s1.y + s1.z;
    if (sigma2 < min_sigma2) Then
    Begin
      min_sigma2 := sigma2;
      Result := ColorCreateFromVector3D(m1);
    End;

    m2 := Vector3D_Scale(m2, 1/ n);
    s2 := Vector3D_Subtract(Vector3D_Scale(s2, 1 / n), Vector3D_Multiply(m2, m2));
    s2 := S2.Abs();

    sigma2 := s2.x + s2.y + s2.z;
    If (sigma2 < min_sigma2) Then
    Begin
      min_sigma2 := sigma2;
      Result := ColorCreateFromVector3D(m2);
    End;

    m3 := Vector3D_Scale(m3, 1/ n);
    s3 := Vector3D_Subtract(Vector3D_Scale(s3, 1 / n), Vector3D_Multiply(m3, m3));
    s3 := S3.Abs();

    sigma2 := s3.x + s3.y + s3.z;
    if (sigma2 < min_sigma2) Then
    Begin
      min_sigma2 := sigma2;
      Result := ColorCreateFromVector3D(m3);
    End;
End;

Function ApplyKurawasaFilter(Src:TERRAImage; Radius:Integer = 7):TERRAImage;
Var
  I,J:Integer;
Begin
  Result := TERRAImage.Create(Src.Width, Src.Height);
  For J:=0 To Pred(Src.Height) Do
    For I:=0 To Pred(Src.Width) Do
    Begin
      Result.SetPixel(I, J, GetKurawasaColor(I, J, Src, Radius));
    End;
End;

End.
