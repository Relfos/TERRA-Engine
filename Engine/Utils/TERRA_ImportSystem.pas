Unit TERRA_ImportSystem;

{$I terra.inc}

Interface

Uses
  TERRA_Process,
  TERRA_Utils,
  TERRA_Application,
  TERRA_Stream,
  TERRA_OS,
  TERRA_Log,
  SysUtils,
  TERRA_FileManager,
  TERRA_FileStream,
  TERRA_FileUtils,
  TERRA_FileImport,
  TERRA_FileSearch,
  TERRA_Localization,
  TERRA_String,
  TERRA_Collections,
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
//  TERRA_GilesImport,
  TERRA_Collada,
  TERRA_PNG,
  TERRA_TGA,
  TERRA_JPG,
  TERRA_PSD,
  TERRA_BMP,
  TERRA_GIF;

Type
  ImportProcessor = Class(TERRAObject)
    Function
      // receives a input stream and returns a output stream (or Nil in case of failure)
      Function Execute(Const FileName:TERRAString):Boolean; Virtual; Abstract;
  End;

  ImportCommand = Class(ImportProcessor)
    Protected
      _Command:TERRAString;
      _Params:TERRAString;

    Public
      Constructor Create(Const Command, Params:TERRAString);
      Function Execute(Const FileName:TERRAString):Boolean; Override;
  End;

  ImportRule = Class(TERRAObject)
    Protected
      _Processors:Array Of ImportProcessor;
      _ProcessorCount:Integer;

      Function Execute(Const FileName:TERRAString):Boolean;

    Public
      // only files matching this filters will pass thorugh
      Platforms:Cardinal;
      PathFilter:TERRAString;

      Procedure AddProcessor(Proc:ImportProcessor);

      Procedure Release(); Override;
  End;

  ImportFile = Record
    FileName:TERRAString;
    Rule:ImportRule;
  End;

  ImportManager = Class(TERRAObject)
    Protected
      _Rules:Array Of ImportRule;
      _RuleCount:Integer;
      //RuleIndex:Integer;

      _FileList:Array Of ImportFile;
      _FileCount:Integer;

      Procedure Start(GeneratePackages: Boolean);

    Public
      Procedure AddRule(Target:Cardinal; Rule:ImportRule);

      Procedure AddFolder(Const Path:TERRAString);

      Function Execute(TargetPlatform:Cardinal; GeneratePackages:Boolean):Boolean;
  End;

Implementation


Function MatchPlatform(Plat, Filter:Cardinal):Boolean;
Begin
  Result := (Filter And Plat) <> 0;
  {If (B = osAny) Then
    Result := True
  Else
  If (B = osPC) And ((A=osWindows) Or (A=osLinux) Or (A=osMacOS)) Then
    Result := True
  Else
  If (B = osMobile) And ((A=osiOS) Or (A=osAndroid)) Then
    Result := True
  Else
    Result := (A=B);}
End;

{ ImportRule }

Function ImportRule.AddCommand(const Cmd, Params: TERRAString):ImportCommand;
Begin
  Result := ImportCommand.Create();
  Result.Command := Cmd;
  Result.Params := Params;

  Inc(_CommandCount);
  SetLength(_Commands, _CommandCount);
  _Commands[Pred(_CommandCount)] := Result;
End;


{ ImportManager }
Procedure ImportManager.AddRule(Target:Cardinal; Rule:ImportRule);
Begin
  Inc(_RuleCount);
  SetLength(_Rules, _RuleCount);
  _Rules[Pred(_RuleCount)] := Rule;
End;

Procedure ImportManager.AddSource(const Source: TERRAString);
Begin
  Inc(_SrcCount);
  SetLength(_SrcList, _SrcCount);
  _SrcList[Pred(_SrcCount)] := Source;
End;

Function ImportManager.Execute(TargetPlatform:Cardinal; GeneratePackages: Boolean): Boolean;
Var
  I, J, K:Integer;
  P:Package;
  Res:ResourceInfo;

  Search:List;
  It:Iterator;
  Info:FileInfo;

  FileList:TERRAString;
  FileName:TERRAString;
  Filter, Path, Extension:TERRAString;

  RuleIndex:Integer;
Begin
  Result := False;
  FileList := '';

  For I:=0 To Pred(_SrcCount) Do
	Begin
		Filter := GetFileName(_SrcList[I], False);
		Path := GetFilePath(_SrcList[I]);
    If Path<>'' Then
      FileManager.Instance.AddPath(Path);

    Search := SearchFiles(Path, Filter, True);
    If Search.Count<=0 Then
    Begin
      Search.Release;
      Continue;
    End;

    It := Search.GetIterator();
    //WriteLn('Files: ',Search.Count);
    While It.HasNext Do
    Begin
      Info := FileInfo(It.Value);
      Extension := StringLower(GetFileExtension(Info.Name));

      {If (StringLower(Info.Name)='assets.ini') Or (Extension='settings') Then
        Continue;}


      {S := FI.Path + PathSeparator + FI.Name;

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
     	End;}

      FileName := Info.Path + PathSeparator + Info.Name;
      Path := Info.Path;

      RuleIndex := -1;
      For J:=0 To Pred(_RuleCount) Do
      If (MatchPlatform(TargetPlatform, _Rules[J].TargetPlatforms))
      And (StringMatchRegEx(StringUpper(FileName), StringUpper(_Rules[J].Include)))
      And ((_Rules[J].Exclude = '') Or (Not StringMatchRegEx(StringUpper(FileName), StringUpper(_Rules[J].Exclude)))) Then
      Begin
        RuleIndex := J;
        Break;
      End;

      If (RuleIndex<0) Or (_Rules[RuleIndex]._CommandCount<=0) Then
      Begin
        Log(logConsole, 'Import', 'Skipping '+ Info.Name+'...');
        Continue;
      End;

      If Not _Rules[RuleIndex].Execute(FileName) Then
        Exit;

      //S2 := ImportFile(S, TargetDir, Plat, SS, ConstFile);

      (*If (Assigned(P)) Then
      While (S2<>'') Do
      Begin
        SS := StringExtractNextWord(S2, ',');
        Log(logConsole, 'Import', 'Adding file to package: '+ SS);
        Res := P.AddResource(SS);
        FileList := FileList + #9 + '// '+GetFileName(SS, False) + ' ' + MemoryToString(Res.Size) + crLf;
      End;*)
    End;

    ReleaseObject(It);
    ReleaseObject(Search);
  End;

  Result := True;
End;

{ ImportCommand }
Function ImportCommand.Execute(const FileName: TERRAString): Boolean;
Const
  READ_BYTES = 2048;

Var
  MyProcess:Process;
  S:TERRAString;
  NumBytes:Integer;
  BytesRead:Integer;
  Buffer:Array[0..READ_BYTES] Of TERRAChar;
Begin
  MyProcess := Process.Create();

  S := Self.Params;
  StringReplaceText('$FILE', FileName, S);

  MyProcess.Executable := Self.Command;
  MyProcess.AddOption(S);
  MyProcess.Execute();
  MyProcess.Release();
End;

End.
