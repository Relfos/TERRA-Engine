Unit TERRA_TestImage;

{$I terra.inc}

Interface
Uses TERRA_TestSuite;

Type
  TERRAImage_TestColorBlend = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAImage_TestColorBlendWithSeparateAlpha = class(TestCase)
    Procedure Run; Override;
   End;


Implementation
Uses TERRA_Utils, TERRA_Color, TERRA_Image;

Procedure TERRAImage_TestColorBlend.Run();
Var
  I, J:Integer;
  Alpha, Expected, Delta:Byte;
  A,B,C:ColorRGBA;
  X,Y:Single;
  Function FailedBlend(Channel:String; Alpha, Got, Expected:Byte):String;
  Begin
    Result := 'Failed blend in '+Channel+' channel with alpha = '+IntToString(Alpha)+', got '+IntToString(Got)+' and expected '+IntToString(Expected);
  End;
Begin
  For J:=1 To 500 Do
  Begin
    Alpha := 128;
    A := ColorCreate(Random(255), Random(255), Random(255), Alpha);
    B := ColorCreate(Random(255), Random(255), Random(255), Alpha);

    C := ColorBlend(A, B);

    X := Alpha / 255;
    Y := 1.0 - X;

    Expected := Trunc(A.R * X + B.R * Y);
    Delta := Abs(C.R - Expected);
    Check(Delta<=2, FailedBlend('red', Alpha, C.R, Expected));

    Expected := Trunc(A.G * X + B.G * Y);
    Delta := Abs(C.G - Expected);
    Check(Delta<=2, FailedBlend('green', Alpha, C.G, Expected));
    
    Expected := Trunc(A.B * X + B.B * Y);
    Delta := Abs(C.B - Expected);
    Check(Delta<=2, FailedBlend('blue', Alpha, C.B, Expected));    
  End;
End;

Procedure TERRAImage_TestColorBlendWithSeparateAlpha.Run();
Var
  I, J:Integer;
  Alpha, Expected, Delta:Byte;
  A,B,C:ColorRGBA;
  X,Y:Single;
  Function FailedBlend(Channel:String; Alpha, Got, Expected:Byte):String;
  Begin
    Result := 'Failed blend in '+Channel+' channel with alpha = '+IntToString(Alpha)+', got '+IntToString(Got)+' and expected '+IntToString(Expected);
  End;
Begin
  For J:=1 To 500 Do
  Begin
    Alpha := 128;
    A := ColorCreate(Random(255), Random(255), Random(255), Byte(Random(255)));
    B := ColorCreate(Random(255), Random(255), Random(255), Byte(Random(255)));

    C := ColorBlend(A, B, Alpha);

    X := Alpha / 255;
    Y := 1.0 - X;

    Expected := Trunc(A.R * X + B.R * Y);
    Delta := Abs(C.R - Expected);
    Check(Delta<=2, FailedBlend('red', Alpha, C.R, Expected));

    Expected := Trunc(A.G * X + B.G * Y);
    Delta := Abs(C.G - Expected);
    Check(Delta<=2, FailedBlend('green', Alpha, C.G, Expected));
    
    Expected := Trunc(A.B * X + B.B * Y);
    Delta := Abs(C.B - Expected);
    Check(Delta<=2, FailedBlend('blue', Alpha, C.B, Expected));    
  End;
End;

End.