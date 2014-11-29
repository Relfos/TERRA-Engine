program import_tool;

{$I terra.inc}

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

{$IFDEF WINDOWS}
{$DEFINE HIDESKIPS}
{$ENDIF}

// TODO - Import sounds and other stuff!
uses
  TERRA_Utils,
  TERRA_Application,
  TERRA_IO,
  TERRA_OS,
  TERRA_Log,
  SysUtils,
  TERRA_FileManager,
  TERRA_FileIO,
  TERRA_FileUtils,
  TERRA_FileImport,
  TERRA_FileSearch,
  TERRA_Localization,
  TERRA_Strings,
  TERRA_Classes,
  TERRA_ResourceManager,
  TERRA_Package,
  TERRA_Resource,
  TERRA_Texture,
  TERRA_Mesh,
  TERRA_WAVE,
  TERRA_OGG,
  TERRA_MS3DImport,
  TERRA_SMDImport,
  TERRA_OBJImport,
  //TERRA_GilesImport,
  TERRA_Collada,
  TERRA_PNG,
  TERRA_TGA,
  TERRA_JPG,
  TERRA_PSD,
  TERRA_BMP,
  TERRA_GIF;

Const
	AppVersion = '1.3';

Type
  Rule = Record
    Target:Integer;
    Filter:String;
    Content:String;
  End;

Const
  osAny = -1;
  osPC  = -2;
  osMobile  = -3;

Function MatchPlatform(A,B:Integer):Boolean;
Begin
  If (B = osAny) Then
    Result := True
  Else
  If (B = osPC) And ((A=osWindows) Or (A=osLinux) Or (A=osMacOS)) Then
    Result := True
  Else
  If (B = osMobile) And ((A=osiOS) Or (A=osAndroid)) Then
    Result := True
  Else
    Result := (A=B);
End;

Procedure MyLogIgnore(Module, Desc:AnsiString);
Begin
End;

Procedure MyLogFilter(Module, Desc:AnsiString);
Begin
  {$IFDEF HIDESKIPS}
  If Pos('Skipping ',Desc)>0 Then
    Exit;
  {$ENDIF}

  WriteLn(Desc);
End;

Procedure MyFatalErrorHandler(S:String); CDecl;
Begin
  WriteLn(S);
  Halt(1);
End;

Var
  S, S2:String;
  Plat, N:Integer;

  Rules:Array Of Rule;
  RuleCount:Integer;
  RuleIndex:Integer;

  TargetDir:String;
  Extension:String;

  I, J:Integer;
  It:Iterator;
  FI:FileInfo;
  Search:List;
  PF, Ext, PackageName, UnitName:String;
  P:Package;
  Ok:Boolean;
  TS, SS:String;
  ConstFile:String ='';
 
  SrcList:Array Of String;
  SrcCount:Integer;
 
  FileList:String;
  Res:PResourceInfo;
  GeneratePackage:Boolean;
  GenerateUnit:Boolean;

  Source, Dest:Stream;
  CopyFile:Boolean;
Begin
  Plat := 0;
  GeneratePackage := False;
  GenerateUnit := False;
  PackageName := '';
  TargetDir := '';
	
	AddLogFilter(logDebug, '', MyLogIgnore);
	{$IFDEF WINDOWS}
  	AddLogFilter(logConsole, '', MyLogFilter);
	{$ENDIF}
  WriteLn('[TERRA Asset importer v'+AppVersion+']');

  SrcCount := 0;
  For I:=1 To ParamCount Do
  Begin
    S := ParamStr(I);

  	If (S<>'') And (S[1]<>'-') Then
  	Begin
  		Inc(SrcCount);
  		SetLength(SrcList, SrcCount);
  		SrcList[Pred(SrcCount)] := S;

 	  	 WriteLn('Target: ',S);
  		Continue;
  	End;
  	
    WriteLn('Param: ',S);

    J := Pos(':', S);
    If J>0 Then
    Begin
      S2 := Copy(S, Succ(J), MaxInt);
      S := Copy(S, 1, Pred(J));
    End Else
      S2 := '';

    If (LowStr(S)='-package') Then
    Begin
      GeneratePackage := True;
      PackageName := S2;
    End;

    If (LowStr(S)='-unit') Then
    Begin
      GenerateUnit := True;
      UnitName := S2;
    End;

    If (LowStr(S)='-dir') Then
    Begin
      TargetDir := S2;
    End;

    If (LowStr(S)='-root') Then
    Begin
      TargetDir := S2;
    End;

    If (LowStr(S)='-const') Then
    Begin
      ConstFile := S2;
    End;

    If (LowStr(S)='-path') Then
    Begin
      If DirectoryExists(S2) Then
      Begin
        FileManager.Instance.AddPath(S2);
      End Else
        WriteLn('Path not found: ', S2);
    End;

    If (LowStr(S)='-target') Then
    Begin
      If (LowStr(S2)='iphone') Or (LowStr(S2)='ios') Then
        Plat := osiOS
      Else
      If (LowStr(S2)='pc') Or (LowStr(S2)='windows') Then
        Plat := osWindows
      Else
      If (LowStr(S2)='android') Then
        Plat := osAndroid
      Else
      If (LowStr(S2)='ouya') Then
        Plat := osOUYA
      Else
        Log(logConsole, 'Import', 'Unknown target specified.');
    End;

    If (LowStr(S)='-help') Then
    Begin
      WriteLn(#9,'Usage: import_tool [options] <path>');
      WriteLn(#9,'Options:');
      WriteLn(#9#9,'-target:[pc/wii/ds/iphone] Selects target platform');
      WriteLn(#9#9,'-package:[packagename] Outputs resources into a TERRA package');
      WriteLn(#9#9,'-unit:[unitname] Outputs package into a pascal unit');
      WriteLn(#9,'Asset rules (assets.ini)');
      WriteLn(#9#9,'target:[any/pc/mobile/windows/osx/android/iphone] filter:[*.*]');
      WriteLn(#9#9,'{');
      WriteLn(#9#9#9,'option1=value1, option2=value2');
      WriteLn(#9#9,'}');
      Exit;
    End;
    

  End;


  If SrcCount<=0 Then
  Begin
    Write('Resources to import: ');
    ReadLn(S);
  		
  	Inc(SrcCount);
  	SetLength(SrcList, SrcCount);
  	SrcList[Pred(SrcCount)] := S;
  End;


 Rules := Nil;
 RuleCount := 0;
(*  S2 := GetFilePath(S)+PathSeparator+'assets.ini';
  If FileStream.Exists(S2) Then
  Begin
    Source := MemoryStream.Create(S2);
    S2 := '';
    While Not Source.EOF Do
    Begin
      Source.ReadLine(SS);
      S2 := S2 + SS + CrLf;
    End;
    Source.Destroy;

    While S2<>'' Do
    Begin
      S2 := TrimLeft(S2);
      I := Pos('target', S2);
      If (I>0) Then
      Begin
        Inc(RuleCount);
        SetLength(Rules, RuleCount);

        S2 := Copy(S2, I+Length('target:'), MaxInt);
        I := Pos(' ', S2);
        SS := Copy(S2,1, Pred(I));
        S2 := Copy(S2, Succ(I), MaxInt);
        SS := LowStr(SS);
        If (SS='any') Then
          Rules[Pred(RuleCount)].Target := osAny
        Else
        If (SS='pc') Then
          Rules[Pred(RuleCount)].Target := osPC
        Else
        If (SS='mobile') Then
          Rules[Pred(RuleCount)].Target := osMobile
        Else
        If (SS='windows') Then
          Rules[Pred(RuleCount)].Target := osWindows
        Else
        If (SS='linux') Then
          Rules[Pred(RuleCount)].Target := osLinux
        Else
        If (SS='mac') Then
          Rules[Pred(RuleCount)].Target := osMacOS
        Else
        If (SS='ouya') Then
          Rules[Pred(RuleCount)].Target := osOUYA
        Else
        If (SS='iphone') Or (SS='ios') Then
          Rules[Pred(RuleCount)].Target := osIOS
        Else
          WriteLn('Unknown rule target: ',SS);

        I := Pos('filter', S2);
        S2 := Copy(S2, I+Length('filter:'), MaxInt);
        I := Pos('{', S2);
        SS := Copy(S2, 1, Pred(I));
        S2 := Copy(S2, Succ(I), MaxInt);

        Rules[Pred(RuleCount)].Filter := TrimLeft(TrimRight(SS));

        I := Pos('}', S2);
        SS := Copy(S2, 1, Pred(I));
        SS := TrimLeft(TrimRight(SS));
        ReplaceText(CrLf, ',', SS);
        ReplaceText(#9, '', SS);
        Rules[Pred(RuleCount)].Content := SS;
        S2 := Copy(S2, Succ(I), MaxInt);
      End;

    End;
  End;*)

  {If Plat=0 Then
  Begin
      Repeat
        WriteLn('1 - Export to Windows/Linux/MacOS');
        WriteLn('2 - Export to Android');
        WriteLn('3 - Export to iPhone');
        ReadLn(Plat);
      Until (Plat>0) And (Plat<5);

      Case Plat Of
      1: Plat := osWindows;
      2: Plat := osAndroid;
      3: Plat := osiOS;
      End;
  End;}
  
  If (TargetDir<>'') And (TargetDir[Length(TargetDir)] = PathSeparator) Then
  	SetLength(TargetDir, Pred(Length(TargetDir)));

  If (Plat>0) Then
  Begin
    PF := GetOSName(Plat);
    If TargetDir = '' Then
      TargetDir := 'bin'+PathSeparator+PF;
    If (TargetDir[Length(TargetDir)] = PathSeparator) Then
      SetLength(TargetDir, Pred(Length(TargetDir)));
  End;

  If TargetDir <> '' Then
  Begin
    MakeDir(TargetDir);
  End Else
    TargetDir := '.';

  If GeneratePackage Then
  Begin
    If PackageName='' Then
      PackageName := 'package';

    PackageName := PackageName +'_'+PF;

    If Pos('.',PackageName)<=0 Then
      PackageName := PackageName + '.leaf';
    Write('Creating package ',PackageName,'...');
    P := Package.New(TargetDir+PathSeparator+PackageName);
    WriteLn('ok');
  End;

{  ApplicationSettings.FullScreen := False;
  ApplicationSettings.Handle := 0;
  ApplicationSettings.Hidden := True;
  ApplicationStart(Nil);}

//  FatalErrorHandler := MyFatalErrorHandler;

  FileList := '';

	While (SrcCount>0) Do
	Begin
		S := SrcList[Pred(SrcCount)];
		Dec(SrcCount);
		S2 := GetFilePath(S);
  		If S2<>'' Then
    		FileManager.Instance.AddPath(S2);

  Search := SearchFiles(GetFilePath(S), GetFileName(S, False), True);
  If Search.Count<=0 Then
  Begin
    Search.Destroy;
    Continue;
  End;

  It := Search.CreateIterator;
  //WriteLn('Files: ',Search.Count);
  While It.HasNext Do
  Begin
    FI := FileInfo(It.GetNext);
    Extension := LowStr(GetFileExtension(FI.Name));
    If (LowStr(FI.Name)='assets.ini') Or (Extension='settings') Then
      Continue;


    S := FI.Path + PathSeparator + FI.Name;
    
    If (PF<>'') Then
    Begin
		S2 := FI.Path + PathSeparator+GetFileName(FI.Name, True)+'_'+PF+'.'+Extension;

	    If FileStream.Exists(S2) Then
    	  S := S2
   		 Else
   		 Begin
      		S2 := FI.Path + PathSeparator+PF+ PathSeparator+FI.Name;
      		If FileStream.Exists(S2) Then
        		S := S2
    	End;
   	End;
   	
    RuleIndex := -1;
    For I:=0 To Pred(RuleCount) Do
    If (MatchRegEx(UpStr(S), UpStr(Rules[I].Filter))) And (MatchPlatform(Plat, Rules[I].Target)) Then
    Begin
      RuleIndex := I;
      Break;
    End;

    If (RuleIndex>=0) And (Pos('IGNORE=TRUE', UpStr(Rules[RuleIndex].Content))>0) Then
    Begin
      Log(logConsole, 'Import', 'Skipping '+ GetFileName(S,False));
      Continue;
    End;

    If (RuleIndex>=0) Then
      SS := Rules[RuleIndex].Content
    Else
      SS := '';

    S2 := ImportFile(S, TargetDir, Plat, SS, ConstFile);

    If (Assigned(P)) Then
    While (S2<>'') Do
    Begin
      SS := GetNextWord(S2, ',');
      Log(logConsole, 'Import', 'Adding file to package: '+ SS);
      Res := P.AddResource(SS);
      FileList := FileList + #9 + '// '+GetFileName(SS, False) + ' ' + MemoryToString(Res.Size) + crLf;
    End;
  End;

  It.Destroy;
  Search.Destroy;
  End;

  If Assigned(P) Then
    P.Destroy;

  If (GenerateUnit) And (GeneratePackage) Then
  Begin
    If UnitName = '' Then
      UnitName := GetFileName(PackageName, True);
    If Pos('.', UnitName)<=0 Then
      UnitName := UnitName + '.pas';

    Write('Generating unit ',UnitName,'...');
    ExportArray(TargetDir+PathSeparator+PackageName, UnitName, FileList);
    WriteLn('ok');
  End Else
  If (GenerateUnit) And (Not GeneratePackage) Then
  Begin
    WriteLn('Warning: Option -unit requires -package option!');
  End;

  //Application.Instance.Terminate;
End.
