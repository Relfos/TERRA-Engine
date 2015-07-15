Program lazarusconv;

{$APPTYPE CONSOLE}

Uses TERRA_OS, TERRA_String, TERRA_Utils, TERRA_FileSearch, TERRA_FileUtils, TERRA_Stream, TERRA_MemoryStream, TERRA_FileStream, TERRA_Collections;

Var
  Search:List;
  It:Iterator;
  Info:FileInfo;

  Template, S, OutName, MainFile, MainPackage:TERRAString;
  Src, Dest:Stream;

Procedure DoTarget(Target, Package:TERRAString);
Begin
    OutName := Info.Path + PathSeparator + MainFile + '_'+Target+'.lpi';
    MainPackage := 'TERRA_PACKAGE_'+StringUpper(Package);

    S := Template;
    StringReplaceText('#MAINFILE', MainFile, S);
    StringReplaceText('#MAINPACKAGE', MainPackage, S);

    Dest := FileStream.Create(OutName);
    Dest.WriteLine(S);
    ReleaseObject(Dest);
End;

Begin
  Search := SearchFiles('..\..\Samples', '*.dpr', True);
  It := Search.GetIterator();

  Src := MemoryStream.Create('template.lpi');
  Src.ReadLines(Template);
  ReleaseObject(Src);


  While It.HasNext() Do
  Begin
    Info := FileInfo(It.Value);

    WriteLn(Info.FullPath);

    MainFile := GetFileName(Info.Name, True);

    DoTarget('Win32', 'windows');
  End;
//  ReadLn;

  ReleaseObject(It);
  ReleaseObject(Search);
End.

