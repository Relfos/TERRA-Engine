Unit TERRA_TestSuite;

{$I terra.inc}

Interface
Uses TERRA_Utils;

Type
  TestCaseClass = Class Of TestCase;

  TestMethod = Procedure Of Object;

  TestSuite = Class;

  TestCase = Class
    Protected
      _Suite:TestSuite;
      _FailureCount:Integer;
      _Failures:Array Of AnsiString;

      Procedure WriteLn(S:AnsiString);
      Procedure Check(Condition:Boolean; S:AnsiString);
      Procedure Error(S:AnsiString);

    Public
      Procedure Run; Virtual; Abstract;
      Function GetName:AnsiString; Virtual;
  End;

  TestSuite = Class(TERRAObject)
    Protected
      _TestCases:Array Of TestCase;
      _TestCount:Integer;

      Procedure WriteLn(S:AnsiString);

    Public
      Constructor Create;
      Destructor Destroy; Override;

      Procedure Clear;

      Procedure RegisterTest(T:TestCaseClass);
      Function Run():Integer;
   End;

Implementation
Uses TERRA_Log;

Procedure TestSuite.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_TestCount) Do
    _TestCases[I].Destroy();
  _TestCount := 0;
End;

Procedure TestSuite.RegisterTest(T:TestCaseClass);
Begin
  Inc(_TestCount);
  SetLength(_TestCases, _TestCount);
  _TestCases[Pred(_TestCount)] := T.Create();
  _TestCases[Pred(_TestCount)]._Suite := Self;
End;

Function TestSuite.Run: Integer;
Const
  Pad = 10;
Var
  I, J, N:Integer;
  Sucess, Failures:Integer;
Begin
  Result := 0;

  Failures := 0;
  Sucess := 0;

  For I:=0 To Pred(_TestCount) Do
  Begin
    WriteLn('Running test: ' + _TestCases[I].GetName());
    _TestCases[I]._FailureCount := 0;
    _TestCases[I].Run;

    If (_TestCases[I]._FailureCount>0) Then
      Inc(Failures)
    Else
      Inc(Sucess);

    Inc(Result, _TestCases[I]._FailureCount);
  End;

  WriteLn('');
  WriteLn('Test Results:');
  WriteLn(StrLPad('Run:', Pad,' ')+IntToString(_TestCount));
  WriteLn(StrLPad('Success:', Pad,' ') +IntToString(Sucess));
  WriteLn(StrLPad('Failures:', Pad,' ')+IntToString(Failures));
  WriteLn(StrLPad('Errors:', Pad,' ')+IntToString(Result));
  If (Result>0) Then
  Begin
    WriteLn('There were '+IntToString(Result)+' failures:');
    For I:=0 To Pred(_TestCount) Do
    If (_TestCases[I]._FailureCount>0) Then
    Begin
      WriteLn('');
      WriteLn(_TestCases[I].GetName+ ' had '+IntToString(_TestCases[I]._FailureCount)+' failures.');
      For J:=0 To Pred(_TestCases[I]._FailureCount) Do
        WriteLn(_TestCases[I]._Failures[J]);
    End;
  End;
End;

{ TestCase }
Procedure TestCase.Check(Condition: Boolean; S:AnsiString);
Begin
  If (Not Condition) Then
  Begin
    Inc(_FailureCount);
    SetLength(_Failures, _FailureCount);
    _Failures[Pred(_FailureCount)] :=S ;
  End;
End;

Procedure TestCase.Error(S:AnsiString);
Begin
  Check(False, S);
End;

Function TestCase.GetName:AnsiString;
Begin
  Result := Self.ClassName;
End;

Procedure TestCase.WriteLn(S:AnsiString);
Begin
  If Assigned(_Suite) Then
    _Suite.WriteLn(S);
End;

Procedure TestSuite.WriteLn(S:AnsiString);
Begin
  Log(logConsole, 'Tests', S);
End;

Constructor TestSuite.Create;
Begin
  _TestCount := 0;
End;

Destructor TestSuite.Destroy;
Begin
  Self.Clear();
End;

End.