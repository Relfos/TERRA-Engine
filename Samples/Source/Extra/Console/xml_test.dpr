program xml_test;

{$APPTYPE CONSOLE}

Uses TERRA_Object, TERRA_String, TERRA_ConsoleApplication, TERRA_ObjectTree, TERRA_Stream, TERRA_Engine, TERRA_XML;

Var
  Console:ConsoleApplication;

  Test:TERRAObjectNode;
  Src:TERRAStream;

Begin
  Console := ConsoleApplication.Create();
  Console.LogToConsole('Searching for file...');

  Engine.Files.AddFolder('assets');

  Src := Engine.Files['demo_map.tmx'];
  If Assigned(Src) Then
  Begin
    Console.LogToConsole('Reading XML...');
    Test := Src.ReadObject();

    Console.LogToConsole('Finished!');
    ReleaseObject(Test);
    ReleaseObject(Src);
  End;


  ReadLn;


End.
