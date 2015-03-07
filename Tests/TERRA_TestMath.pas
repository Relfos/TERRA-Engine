Unit TERRA_TestMath;

{$I terra.inc}

Interface
Uses TERRA_TestSuite;

Type
  TERRAMath_TestLogFunctions = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAMath_TestPowFunctions = class(TestCase)
    Procedure Run; Override;
   End;

Implementation
Uses TERRA_Utils, Math, TERRA_Math;

Const
  MathEpsilon = 0.5;

Procedure TERRAMath_TestLogFunctions.Run();
Const
  Runs = 5000;
Var
  I, K:Integer;
  X:Single;
  Delta, Expected, Got:Double;

  Function Failed(FuncName:String; Value:Single):String;
  Begin
    Result := 'Failed '+FuncName + ' with arg '+ FloatToString(Value)+' with delta '+FloatToString(Delta);
  End;
Begin
  For I:=1 To Runs Do
  Begin
    X := RandomFloat(1, 100);

    Expected := System.Ln(X);
    Got := TERRA_Math.Ln(X);
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('Ln', X));
  End;

  For I:=1 To Runs Do
  Begin
    X := RandomFloat(1, 100);

    Expected := Math.Log2(X);
    Got := TERRA_Math.Log2(X);
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('Log2', X));
  End;

  For I:=1 To Runs Do
  Begin
    X := RandomFloat(1, 100);
    K := Trunc(X);

    Expected := Math.Log2(K);
    Got := TERRA_Math.Log2(K);
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('IntLog2', K));
  End;

  For I:=1 To Runs Do
  Begin
    X := RandomFloat(1, 100);

    Expected := Math.LNXP1(X);
    Got := TERRA_Math.LNXP1(X);
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('LNXP1', X));
  End;

  For I:=1 To Runs Do
  Begin
    X := RandomFloat(1, 100);

    Expected := X;
    Got := TERRA_Math.Exp(TERRA_Math.Ln(X));
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('Exp(Ln(X))', X));
  End;

End;

Procedure TERRAMath_TestPowFunctions.Run();
Const
  Runs = 5000;
  MaxBase = 5;
  MaxPower = 10;
Var
  I, A, B:Integer;
  X,Y:Single;
  Delta, Expected, Got:Double;

  Function Failed(FuncName:String; Value1, Value2:Single):String;
  Begin
    Result := 'Failed '+FuncName + ' with args '+ FloatToString(Value1)+' and '+ FloatToString(Value2)+' with delta '+FloatToString(Delta) + ' -> Got '+FloatToString(Got)+' and expected '+FloatToString(Expected);
  End;
Begin
  For I:=1 To Runs Do
  Begin
    X := RandomFloat(1, MaxBase);
    Y := RandomFloat(1, MaxPower);

    Expected := Math.Power(X, Y);
    Got := TERRA_Math.Power(X, Y);
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('Pow(F,F)', X, Y));
  End;


  For I:=1 To Runs Do
  Begin
    X := RandomFloat(1, MaxBase);
    B := Trunc(RandomFloat(1, MaxPower));

    Expected := Math.Power(X, B);
    Got := TERRA_Math.Power(X, B);
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('Pow(F,I)', X, B));
  End;

  For I:=1 To Runs Do
  Begin
    A := Trunc(RandomFloat(1, MaxBase));
    B := Trunc(RandomFloat(1, MaxPower));

    Expected := Math.Power(A, B);
    Got := TERRA_Math.Power(A, B);
    Delta := Abs(Got - Expected);
    Check(Delta<=MathEpsilon, Failed('Pow(I,I)', A, B));
  End;
End;


End.