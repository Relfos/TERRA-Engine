program xml_test;

{$APPTYPE CONSOLE}

Uses TERRA_Object, TERRA_ConsoleApplication, TERRA_ObjectTree, TERRA_Stream, TERRA_EngineManager, TERRA_XML;

Var
  Console:ConsoleApplication;

  Test:TERRAObjectNode;
  Src:TERRAStream;

Begin
  Console := ConsoleApplication.Create();
  Console.LogToConsole('Test');

  Engine.Files.AddFolder('assets');

  Src := Engine.Files['demo_map.tmx'];
  If Assigned(Src) Then
  Begin
    Test := Src.ReadObject();
    ReleaseObject(Test);
    ReleaseObject(Src);
  End;


  ReadLn;


End.
