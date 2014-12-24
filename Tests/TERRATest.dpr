Program TERRATest;

{$I terra.inc}

Uses TERRA_MemoryManager, TERRA_Application, TERRA_Client, TERRA_Log, TERRA_Utils,
  TERRA_TestSuite, TERRA_TestCore, TERRA_TestImage, TERRA_TestMath;

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

Procedure MyLogFilter(Module, Desc:AnsiString);
Begin
  WriteLn(Desc);
End;

Var
	Tests:TestSuite;
  Errors:Integer;
Begin
	WriteLn('Testing TERRA engine: v'+VersionToString(EngineVersion));

  ApplicationStart(ConsoleClient.Create());

	//AddLogFilter(logDebug, '', MyLogIgnore);
	{$IFDEF WINDOWS}
  	AddLogFilter(logConsole, '', MyLogFilter);
	{$ENDIF}

  //ApplicationStart(ConsoleClient.Create());

	Tests := TestSuite.Create();
  	Tests.RegisterTest(TERRACore_TestList);
  	Tests.RegisterTest(TERRACore_TestHashTable);
  	Tests.RegisterTest(TERRACore_TestSort);

  	Tests.RegisterTest(TERRAImage_TestColorBlend);
  	Tests.RegisterTest(TERRAImage_TestColorBlendWithSeparateAlpha);

    Tests.RegisterTest(TERRAMath_TestLogFunctions);
    //Tests.RegisterTest(TERRAMath_TestPowFunctions);

  	Errors := Tests.Run();


  	Tests.Destroy();

    If Application.Instance.IsDebuggerPresent Then
      ReadLn;

    Halt(Errors);
End.
