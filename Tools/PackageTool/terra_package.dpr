Program packagetool;
{$APPTYPE CONSOLE}
Uses TERRA_Utils, TERRA_Stream, TERRA_MemoryStream, TERRA_FileStream, TERRA_FileUtils,
  TERRA_PackageBuilder,
  TERRA_FileSearch, TERRA_Collections, TERRA_String, TERRA_OS;

Var
  Source:FileStream;
  Pack:PackageBuilder;
  S, SrcFilter, DestName:String;
  SrcPath:String;
  Search:List;
  I:Iterator;
  P:FileInfo;
  J,K:Integer;

Begin
  If (ParamCount<2) Then
  Begin
    WriteLn('Usage: <source files> <dest package>');
    Exit;
  End;

  DestName := ParamStr(ParamCount());

  Pack := PackageBuilder.Create(DestName);
  For K:=1 To Pred(ParamCount()) Do
  Begin
    SrcFilter := ParamStr(K);
    SrcPath := GetFilePath(SrcFilter);
    SrcFilter := GetFileName(SrcFilter, False);

    Search := SearchFiles(SrcPath, SrcFilter, True);
    I := Search.GetIterator();
    While I.HasNext Do
    Begin
      P := FileInfo(I.Value);
      S := P.Name;

      For J:=3 To ParamCount() Do
      If  (StringMatchRegEx(S, ParamStr(J))) Then
      Begin
        WriteLn('Skipping resource '+P.Name);
        Continue;
      End;

      If P.Path<>'' Then
        S := P.Path+PathSeparator+S;
      WriteLn('Adding resource '+P.Name);
      Pack.AddResource(S);
    End;
  End;

  Pack.Save(DestName);

  Search.Destroy;
  Pack.Destroy;
End.
