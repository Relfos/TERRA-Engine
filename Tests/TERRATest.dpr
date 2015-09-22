Program TERRATest;

{$I terra.inc}
                   
{$APPTYPE CONSOLE}

Uses TERRA_MemoryManager, TERRA_OS, TERRA_Engine, TERRA_String, TERRA_Application, TERRA_ConsoleApplication, TERRA_Log, TERRA_Utils,
  TERRA_TestSuite, TERRA_TestCore, TERRA_TestImage, TERRA_TestMath, TERRA_TestString, TERRA_TestXML;

Var
	Tests:TestSuite;
  Errors:Integer;
Begin
	WriteLn('Testing TERRA engine: v'+VersionToString(EngineVersion));
  Engine.Log.Enabled := True;

  ConsoleApplication.Create();

	Tests := TestSuite.Create();
  	Tests.RegisterTest(TERRACore_TestList);
  	Tests.RegisterTest(TERRACore_TestHashMap);
    Tests.RegisterTest(TERRACore_TestObjectArray);
  	Tests.RegisterTest(TERRACore_TestSort);
  	//Tests.RegisterTest(TERRACore_TestRadixSort);

  	Tests.RegisterTest(TERRAImage_TestColorBlend);
  	Tests.RegisterTest(TERRAImage_TestColorBlendWithSeparateAlpha);

    Tests.RegisterTest(TERRAMath_TestLogFunctions);
    //Tests.RegisterTest(TERRAMath_TestPowFunctions);

    Tests.RegisterTest(TERRAString_TestIterator);
    Tests.RegisterTest(TERRAString_TestReverseIterator);

    Tests.RegisterTest(TERRAString_TestGetChar);
    Tests.RegisterTest(TERRAString_TestUnicodeIterator);
    Tests.RegisterTest(TERRAString_TestUnicodeReverseIterator);
    Tests.RegisterTest(TERRAString_TestRegex);
    Tests.RegisterTest(TERRAString_TestCharPos);
    Tests.RegisterTest(TERRAString_TestSubStrPos);
    Tests.RegisterTest(TERRAString_TestCopy);
    Tests.RegisterTest(TERRAString_TestSplits);
    Tests.RegisterTest(TERRAString_TestIteratorSplits);
//    Tests.RegisterTest(TERRAString_TestWordExtract);
    Tests.RegisterTest(TERRAString_TestPad);
    Tests.RegisterTest(TERRAString_TestReplace);
    Tests.RegisterTest(TERRAString_TestReverse);
    Tests.RegisterTest(TERRAString_TestTrim);
    Tests.RegisterTest(TERRAString_TestConversions);

    Tests.RegisterTest(TERRAXML_TestSimple);
    Tests.RegisterTest(TERRAXML_TestShortcuts);

  	Errors := Tests.Run();

  	Tests.Release();

   (* If Application.Instance.DebuggerPresent Then
      ReadLn;
     *)

    Halt(Errors);
End.
