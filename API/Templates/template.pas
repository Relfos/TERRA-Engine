{**************************************************
*
*   TERRA ENGINE - $BUILD
*   www.pascalgameengine.com
*   https://github.com/Relfos/TERRA-Engine
*   Sérgio Flores (relfos@gmail)
*
*   $DATE
**************************************************}

Unit TERRA_Engine;
{$I terra.inc}

Interface

{$IFDEF IPHONE}
{$DEFINE STATIC_LINKING}
{$LINKLIB terra_engine}
{$ENDIF}

{$IFNDEF STATIC_LINKING}
Const
{$IFDEF WINDOWS}
  TERRA_LIB = 'terra_engine.dll';
{$ENDIF}

{$IFDEF LINUX}
  TERRA_LIB = 'terra_engine.so';
{$ENDIF}

{$IFDEF OSX}
  TERRA_LIB = 'terra_engine.dylib';
{$ENDIF}

{$ENDIF}

{$I TERRA_types.inc}

Type
  WidgetEventHandler = Procedure (Widget:TERRAWidget); Cdecl;

  PColor = ^Color;
  Color=Packed Object
    R:Byte;
    G:Byte;
    B:Byte;
    A:Byte;
  End;

  PVector2D=^Vector2D;
  Vector2D=Packed Object
    X,Y:Single;
  End;

  PVector3D = ^Vector3D;
  Vector3D = Packed Object
    X:Single;
    Y:Single;
    Z:Single;
  End;

  PVector4D = ^Vector4D;
  Vector4D = Packed Object
    X:Single;
    Y:Single;
    Z:Single;
    W:Single;
  End;

  PMatrix3x3 = ^Matrix3x3;
  Matrix3x3=Packed Object
    V:Array [0..8] Of Single;
  End;

  PMatrix4x4 = ^Matrix4x4;
  Matrix4x4=Packed Object
    V:Array [0..15] Of Single;
  End;

  PRay=^Ray;
  Ray = Packed Object
    Origin:Vector3D;
    Direction:Vector3D;
  End;

  BoundingBox = Packed Object
    StartVertex:Vector3D;
    EndVertex:Vector3D;
  End;

  Plane = Packed Object
    A,B,C,D:Single;
  End;

  Frustum = Packed Object
    Planes:Array[0..5]Of Plane;
  End;

Const
  Epsilon = 0.00001;

  Rad   = 0.017453292519;   // Pi/180
  Deg   = 57.29577951308;   // 180/Pi

// Color constants
  ColorWhite:Color=(R:255; G:255; B:255; A:255);
  ColorBlack:Color=(R:0; G:0; B:0; A:255);
  ColorNull:Color=(R:0; G:0; B:0; A:0);

  ColorRed:Color=(R:255; G:0; B:0; A:255);
  ColorBlue:Color=(R:0; G:0; B:255; A:255);
  ColorGreen:Color=(R:0; G:255; B:0; A:255);
  ColorYellow:Color=(R:255; G:255; B:0; A:255);

  blendNone     = 0;
  blendBlend    = 1;
  blendAdd      = 2;         //GL_ONE  GL_ONE
  blendFilter   = 3;      //GL_DST_COLOR GL_ZERO
  blendModulate = 4;
  blendJoin     = 5;

  tweenByte     = 0;
  tweenFloat    = 1;

  shaderSkinning    = 1;
  shaderSpecular    = 2;
  shaderLightMap    = 4;
  shaderSpecularMap = 8;
  shaderNormalMap   = 16;
  shaderReflective  = 32;
  shaderColorRamp   = 64;
  shaderFog         = 128;
  shaderAlphaTest   = 256;
  shaderAddSigned   = 512;

  camDirForward   = 0;
  camDirBackward  = 1;
  camDirLeft      = 2;
  camDirRight     = 3;

  renderFlagsSkipFrustum  = 1;
  renderFlagsSkipSorting  = 2;

  textureFilterPoint = 0;
  textureFilterBilinear = 1;

  widgetAnimateAlpha  = 1;
  widgetAnimatePosX   = 2;
  widgetAnimatePosY   = 4;
  widgetAnimateRotation = 8;
  widgetAnimateScale    = 16;
  widgetAnimateSaturation  = 32;
  widgetAnimatePosX_Bottom = 64;
  widgetAnimatePosY_Bottom = 128;

  // widget tween
  wtPositionX   = 1;
  wtPositionY   = 2;
  wtColorRed    = 3;
  wtColorGreen  = 4;

  wtColorBlue   = 5;
  wtColorAlpha  = 6;
  wtRotation    = 7;
  wtScale       = 8;
  wtSaturation  = 9;

  // custom tweens
  wtValue       = 100;

  waTopLeft     = 0;
  waTopCenter   = 1;
  waTopRight    = 2;
  waLeftCenter  = 3;
  waCenter      = 4;
  waRightCenter = 5;
  waBottomLeft     = 6;
  waBottomCenter   = 7;
  waBottomRight    = 8;

  //  Search flags
  searchCanTimeOut  = 1;
  searchDiagonal    = 2;

  //  Search results
  searchComplete      = 0;
  searchTimeOut       = 1;
  searchLimitReached  = 2;
  searchFailed        = 3;

{$1}

Const
  keyA  = Ord('A');
  keyB  = Ord('B');
  keyC  = Ord('C');
  keyD  = Ord('D');
  keyE  = Ord('E');
  keyF  = Ord('F');
  keyG  = Ord('G');
  keyH  = Ord('H');
  keyI  = Ord('I');
  keyJ  = Ord('J');
  keyK  = Ord('K');
  keyL  = Ord('L');
  keyM  = Ord('M');
  keyN  = Ord('N');
  keyO  = Ord('O');
  keyP  = Ord('P');
  keyQ  = Ord('Q');
  keyR  = Ord('R');
  keyS  = Ord('S');
  keyT  = Ord('T');
  keyU  = Ord('U');
  keyV  = Ord('V');
  keyW  = Ord('W');
  keyX  = Ord('X');
  keyY  = Ord('Y');
  keyZ  = Ord('Z');

{$IFDEF WINDOWS}
	keyBackspace  = 8;
	keyTab        = 9;
	keyEnter      = 13;
	keyShift      = 16;
	keyControl    = 17;
	keyAlt        = 18;
	keyPause      = 19;
	keyEscape     = 27;
	keySpace      = 32;
	keyPageUp     = 33;
	keyPageDown   = 34;
	keyEnd        = 35;
	keyHome       = 36;

	keyLeft       = 37;
	keyUp         = 38;
	keyRight      = 39;
	keyDown       = 40;

	keyInsert     = 45;
	keyDelete     = 46;
	keyF1         = 112;
	keyF2         = 113;
	keyF3         = 114;
	keyF4         = 115;
	keyF5         = 116;
	keyF6         = 117;
	keyF7         = 118;
	keyF8         = 119;
	keyF9         = 120;
	keyF10        = 121;
	keyF11        = 122;
	keyF12        = 123;
{$ENDIF}

{$IFDEF LINUX}
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = 22;
	keyTab        = 23;
	keyEnter      = 36;
	keyShift      = 50;
	keyControl    = 37;
	keyAlt        = 64;
	keyPause      = 127;
	keyEscape     = 9;
	keySpace      = 65;
	keyPageUp     = 112;
	keyPageDown   = 117;
	keyEnd        = 115;
	keyHome       = 110;

	keyLeft       = 113;
	keyUp         = 111;
	keyRight      = 114;
	keyDown       = 116;

	keyInsert     = 118;
	keyDelete     = 119;

	keyF1         = 67;
	keyF2         = 68;
	keyF3         = 69;
	keyF4         = 70;
	keyF5         = 71;
	keyF6         = 72;
	keyF7         = 73;
	keyF8         = 74;
	keyF9         = 75;
	keyF10        = 76;
	keyF11        = 77;
	keyF12        = 78;
{$ENDIF}

{$IFDEF IPHONE}
Const
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = $33;
	keyTab        = $30;
	keyEnter      = $24;
	keyShift      = $38;
	keyControl    = $3B;
	keyAlt        = $3A;
	keyPause      = $71;
	keyEscape     = $35;
	keySpace      = $31;
	keyPageUp     = $74;
	keyPageDown   = $79;
	keyEnd        = $77;
	keyHome       = $73;

	keyLeft       = $7B;
	keyUp         = $7E;
	keyRight      = $7C;
	keyDown       = $7D;
{$ENDIF}

  //easing types
	easeLinear = 0;
	easeInQuad = 1;
	easeOutQuad = 2;
	easeInOutQuad = 3;
	easeOutInQuad = 4;
	easeInCubic = 5;
	easeOutCubic = 6;
	easeInOutCubic = 7;
	easeOutInCubic = 8;
	easeInQuart = 9;
	easeOutQuart = 10;
	easeInOutQuart = 11;
	easeOutInQuart = 12;
	easeInSine = 17;
	easeOutSine = 18;
	easeInOutSine = 19;
	easeOutInSine = 20;
	easeInExpo = 21;
	easeOutExpo = 22;
	easeInOutExpo = 23;
	easeOutInExpo = 24;
	easeInCirc = 25;
	easeOutCirc = 26;
	easeInOutCirc = 27;
	easeOutInCirc = 28;
	easeInElastic = 29;
	easeOutElastic = 30;
	easeInOutElastic = 31;
	easeOutInElastic = 32;
	easeInBack = 33;
	easeOutBack = 34;
	easeInOutBack = 35;
	easeOutInBack = 36;
	easeInBounce = 37;
	easeOutBounce = 38;
	easeInOutBounce = 39;
	easeOutInBounce = 40;

Function IntToString(Const N:Integer):AnsiString;
Function CardinalToString(Const N:Cardinal):AnsiString;
Function FloatToString(Const N:Single; DecimalPlaces:Integer = 4):AnsiString;
Function BoolToString(Const N:Boolean):AnsiString;
  
Function RandomFloat(Const min,max:Single):Single;
Function RandomInt(Const min,max:Integer):Integer;

Function ColorCreate(Const R,G,B:Byte; A:Byte=255):Color;
Function ColorMultiply(Const A,B:Color):Color;
Function ColorScale(Const A:Color; B:Single):Color;
Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):Color;

Function VectorCreate2D(Const X,Y:Single):Vector2D;
Function VectorDot2D(Const A,B:Vector2D):Single;
Function VectorDistance2D(Const A,B:Vector2D):Single;
Function VectorLength2D(Const V:Vector2D):Single;
Procedure VectorNormalize2D(Var V:Vector2D);
Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;
Function VectorMultiply2D(Const A,B:Vector2D):Vector2D;
Function VectorScale2D(Const A:Vector2D; S:Single):Vector2D;

Function VectorCreate3D(Const X,Y,Z:Single):Vector3D;
Function VectorDistance3D(Const A,B:Vector3D):Single;
Function VectorDot3D(Const A,B:Vector3D):Single;
Function VectorLength3D(Const V:Vector3D):Single;
Procedure VectorNormalize3D(Var V:Vector3D);
Function VectorAdd3D(Const A,B:Vector3D):Vector3D;
Function VectorSubtract3D(Const A,B:Vector3D):Vector3D;
Function VectorMultiply3D(Const A,B:Vector3D):Vector3D;
Function VectorScale3D(Const A:Vector3D; S:Single):Vector3D;

Function VectorCreate4D(Const X,Y,Z,W:Single):Vector4D;


Implementation
Uses Math;

Const
  RAND_MAX = (MAXINT-1);
  INV_RAND_MAX = 1.0 / (RAND_MAX + 1);


Function RandomFloat(Const min,max:Single):Single; {$IFNDEF OXYGENE} Overload; {$ENDIF}
Begin
	Result := Min + ((max - min) * (System.Random(RAND_MAX) * INV_RAND_MAX));
End;

Function RandomInt(Const min,max:Integer):Integer;
Begin
  Result := Random(Succ(Max-Min)) + Min;
End;

Function IntToString(Const N:Integer):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result := S;
End;

Function BoolToString(Const N:Boolean):AnsiString;
Begin
  If N Then
    Result := 'true'
  Else
    Result := 'false';
End;

Function CardinalToString(Const N:Cardinal):AnsiString;
Var
  S:AnsiString;
Begin
  Str(N,S);
  Result:=S;
End;

Function FloatToString(Const N:Single; DecimalPlaces:Integer):AnsiString;
Var
  P,X:Single;
  A,B, I:Integer;
Begin
  P := N;
  A := Trunc(P);
  X := Abs(Frac(P));
  For I:=1 To DecimalPlaces Do
    X := X*10;
  B := Trunc(X);

  Result := IntToString(A)+'.'+IntToString(B);
End;

Function ColorCreate(Const R,G,B:Byte;A:Byte=255):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
End;

Function ColorMultiply(Const A,B:Color):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R:=Trunc((A.R/255)*(B.R/255)*255);
  Result.G:=Trunc((A.G/255)*(B.G/255)*255);
  Result.B:=Trunc((A.B/255)*(B.B/255)*255);
  Result.A:=Trunc((A.A/255)*(B.A/255)*255);
End;

Function ColorScale(Const A:Color; B:Single):Color;Overload;  {$IFDEF FPC} Inline;{$ENDIF}
Var
  X,Y,Z:Single;
Begin
  X := A.R*B;
  Y := A.G*B;
  Z := A.B*B;
  If (X>255) Then X := 255;
  If (Y>255) Then Y := 255;
  If (Z>255) Then Z := 255;
  Result.R := Trunc(X);
  Result.G := Trunc(Y);
  Result.B := Trunc(Z);
  Result.A := A.A;
End;

Function ColorGrey(GreyLevel:Byte; Alpha:Byte=255):Color;  {$IFDEF FPC} Inline;{$ENDIF}
Begin
  Result.R := GreyLevel;
  Result.G := GreyLevel;
  Result.B := GreyLevel;
  Result.A := Alpha;
End;

Function VectorCreate2D(Const X,Y:Single):Vector2D;
Begin
  Result.X := X;
  Result.Y := Y;
End;

Function VectorDistance2D(Const A,B:Vector2D):Single;
Begin
  Result := Sqrt(Sqr(A.X-B.X) + Sqr(A.Y-B.Y));
End;

Function VectorDot2D(Const A,B:Vector2D):Single;
Begin
  Result := (A.X * B.X) + (A.Y * B.Y);
End;

Function VectorLength2D(Const V:Vector2D):Single;
Begin
  Result := Sqrt(Sqr(V.X)+Sqr(V.Y));
End;

Procedure VectorNormalize2D(Var V:Vector2D);
Var
  K:Single;
Begin
  K := VectorLength2D(V);
  If (K<=1.0) Then
    Exit;

  V.X := V.X / K;
  V.Y := V.Y / K;
End;

Function VectorAdd2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
End;

Function VectorSubtract2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
End;

Function VectorMultiply2D(Const A,B:Vector2D):Vector2D;
Begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
End;

Function VectorScale2D(Const A:Vector2D; S:Single):Vector2D;
Begin
  Result.X := A.X * S;
  Result.Y := A.Y * S;
End;

// Vec3

Function VectorCreate3D(Const X,Y,Z:Single):Vector3D;
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
End;

Function VectorDistance3D(Const A,B:Vector3D):Single;
Begin
  Result := Sqrt(Sqr(A.X-B.X) + Sqr(A.Y-B.Y) + Sqr(A.Z-B.Z));
End;

Function VectorDot3D(Const A,B:Vector3D):Single;
Begin
  Result := (A.X * B.X) + (A.Y * B.Y) + (A.Z * B.Z);
End;

Function VectorLength3D(Const V:Vector3D):Single;
Begin
  Result := Sqrt(Sqr(V.X)+Sqr(V.Y)+Sqr(V.Z));
End;

Procedure VectorNormalize3D(Var V:Vector3D);
Var
  K:Single;
Begin
  K := VectorLength3D(V);
  If (K<=1.0) Then
    Exit;

  V.X := V.X / K;
  V.Y := V.Y / K;
  V.Z := V.Z / K;
End;

Function VectorAdd3D(Const A,B:Vector3D):Vector3D;
Begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
End;

Function VectorSubtract3D(Const A,B:Vector3D):Vector3D;
Begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
End;

Function VectorMultiply3D(Const A,B:Vector3D):Vector3D;
Begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
End;

Function VectorScale3D(Const A:Vector3D; S:Single):Vector3D;
Begin
  Result.X := A.X * S;
  Result.Y := A.Y * S;
  Result.Z := A.Z * S;
End;


Function VectorCreate4D(Const X,Y,Z,W:Single):Vector4D;
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
End;

Initialization
{$IFDEF FPC}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$ELSE}
  Set8087CW($133F);
{$ENDIF}
End.
