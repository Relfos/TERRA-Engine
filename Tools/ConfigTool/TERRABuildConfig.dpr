program TERRABuildConfig;

{$APPTYPE CONSOLE}

uses
  TERRA_Object, TERRA_Stream, TERRA_FileStream, TERRA_String;

Type
  Build_Compile_Type = (
    FPC_WINDOWS_BATCH,
    FPC_LINUX_BATCH
  );

Var
  Paths:Array Of TERRAString;
  PathCount:Integer;

Procedure RegisterPath(Const Value:TERRAString);
Begin
  Inc(PathCount);
  SetLength(Paths, PathCount);
  Paths[Pred(PathCount)] := Value;
End;

Procedure GenerateCompileFile(BuildType:Build_Compile_Type; CompileFileName, SrcMain, OutputName:TERRAString);
Var
  Dest:TERRAStream;
  I:Integer;
  IsWindows:Boolean;
  EnginePathVar, TestsPathVar:TERRAString;
  Result, TempPath, TargetOS:TERRAString;
  Sep:TERRAString;

  Procedure PrepareString(Var S:TERRAString);
  Begin
    StringReplaceText('$TARGET_OS', TargetOS, TempPath);
    StringReplaceText('$ENGINE_PATH', EnginePathVar, S);
    StringReplaceText('$TESTS_PATH', TestsPathVar, S);
    If Not IsWindows Then
      StringReplaceText('\', '/', S);
  End;
Begin
  Case BuildType Of
    FPC_WINDOWS_BATCH:
      Begin
        EnginePathVar := '%ENGINE_PATH%';
        TestsPathVar := '%TESTS_PATH%';
      End;
    Else
      Begin
        EnginePathVar := '$ENGINE_PATH';
        TestsPathVar := '$TESTS_PATH';
      End;
  End;

  Case BuildType Of
    FPC_WINDOWS_BATCH:
      Begin
        IsWindows := True;
        TargetOS := 'Windows';
      End;
    Else
      Begin
        IsWindows := False;
        TargetOS := 'Linux';
      End;
  End;

  If IsWindows Then
    Sep := '\'
  Else
    Sep := '/';

  Result := 'fpc -Sew -Mdelphi -dUSE_CONSOLE ';
  Result := Result +' -Fi'+EnginePathVar+Sep+'Core -Fi'+EnginePathVar+Sep+'Utils ';

  For I:=0 To Pred(PathCount) Do
  Begin
    TempPath := Paths[I];

    PrepareString(TempPath);
    Result := Result + '-Fu'+EnginePathVar+Sep+TempPath + ' ';
  End;

  If OutputName<>'' Then
  Begin
    PrepareString(OutputName);
    Result := Result + '-o'+OutputName+' ';
  End;

  PrepareString(SrcMain);
  Result := Result + SrcMain;

  Dest := FileStream.Create(CompileFileName);

  If IsWindows Then
  Begin
    Dest.EOL := EOL_Windows;
  End Else
  Begin
    Dest.EOL := EOL_Unix;
    Dest.WriteLine('#!/bin/bash');
  End;

  Dest.WriteLine(Result);
  ReleaseObject(Dest);
End;

//$(PkgDir)
Begin
  RegisterPath('Core');
  RegisterPath('Math');
  RegisterPath('Utils');
  RegisterPath('Core\FileIO');
  RegisterPath('Graphics');
  RegisterPath('Graphics\Renderer');
  RegisterPath('Templates');
  RegisterPath('Image');
  RegisterPath('Physics');
  RegisterPath('Image');
  RegisterPath('Image\Formats');
  RegisterPath('OS\$TARGET_OS');
  RegisterPath('UI');
  RegisterPath('AI');
  RegisterPath('Audio');
  RegisterPath('Audio\Formats');
  RegisterPath('Audio\Renderers');
  RegisterPath('Network');
  RegisterPath('Libs\Steamworks\headers');
  RegisterPath('Network');
  RegisterPath('Network\Protocols');
  RegisterPath('..\Tests');


  GenerateCompileFile(FPC_WINDOWS_BATCH, 'compile_tests.bat', 'TERRATest.dpr', '.\TERRATest.exe');
  GenerateCompileFile(FPC_LINUX_BATCH, 'compile_tests.sh', 'TERRATest.dpr', '.\TERRATest');
End.
