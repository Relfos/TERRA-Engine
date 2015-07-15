Program lazarusconv;

{$APPTYPE CONSOLE}

Uses TERRA_OS, TERRA_String, TERRA_Utils, TERRA_FileSearch, TERRA_FileUtils, TERRA_Stream, TERRA_MemoryStream, TERRA_FileStream, TERRA_Collections;

Var
  Search:List;
  It:Iterator;
  Info:FileInfo;

  Template, S, OutName, MainFile:TERRAString;
  Src, Dest:Stream;

Procedure DoTarget(Target, Package, PathSep:TERRAString);
Var
  BinPath, MainPackage:TERRAString;
Begin
    OutName := Info.Path + PathSeparator + MainFile + '_'+Target+'.lpi';
    BinPath := '..'+PathSep+'..'+PathSep+'..'+PathSep+'Binaries'+PathSep;

    MainPackage := 'TERRA_PACKAGE_'+StringUpper(Package);

    S := Template;
    StringReplaceText('#MAINFILE', MainFile, S);
    StringReplaceText('#MAINPACKAGE', MainPackage, S);
    StringReplaceText('#BINPATH', BinPath, S);

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

    DoTarget('Win32', 'windows', '\');
    DoTarget('Linux', 'linux', '/');
    DoTarget('OSX', 'osx', '/');
  End;
//  ReadLn;

  ReleaseObject(It);
  ReleaseObject(Search);
End.

