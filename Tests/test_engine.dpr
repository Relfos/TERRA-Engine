program minimon_deploy_tests;

{$APPTYPE CONSOLE}

uses
  Windows,
  TERRA_Utils,
  TERRA_TestSuite,
  TERRA_TestCore;

Procedure MyCallback(S:String);
Begin
  WriteLn(S);
End;

begin
  TestSuite.Instance.SetCallback(MyCallback);
  ExitCode := TestSuite.Instance.Run();
  ReadLn;
end.