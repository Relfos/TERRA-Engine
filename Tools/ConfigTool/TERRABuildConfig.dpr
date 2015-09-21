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

  EnginePathVar := '..'+Sep+'Engine';

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
    Result := Result + '-Fu'+TempPath + ' ';
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
  RegisterPath('$ENGINE_PATH\Core');
  RegisterPath('$ENGINE_PATH\Math');
  RegisterPath('$ENGINE_PATH\Utils');
  RegisterPath('$ENGINE_PATH\Core\FileIO');
  RegisterPath('$ENGINE_PATH\Graphics');
  RegisterPath('$ENGINE_PATH\Graphics\Renderer');
  RegisterPath('$ENGINE_PATH\Templates');
  RegisterPath('$ENGINE_PATH\Image');
  RegisterPath('$ENGINE_PATH\Physics');
  RegisterPath('$ENGINE_PATH\Image');
  RegisterPath('$ENGINE_PATH\Image\Formats');
  RegisterPath('$ENGINE_PATH\OS\$TARGET_OS');
  RegisterPath('$ENGINE_PATH\UI');
  RegisterPath('$ENGINE_PATH\AI');
  RegisterPath('$ENGINE_PATH\Audio');
  RegisterPath('$ENGINE_PATH\Audio\Formats');
  RegisterPath('$ENGINE_PATH\Audio\Renderers');
  RegisterPath('$ENGINE_PATH\Network');
  RegisterPath('$ENGINE_PATH\Libs\Steamworks\headers');
  RegisterPath('$ENGINE_PATH\Network');
  RegisterPath('$ENGINE_PATH\Network\Protocols');
  RegisterPath('..\Tests');


  GenerateCompileFile(FPC_WINDOWS_BATCH, 'compile_tests.bat', 'TERRATest.dpr', '.\TERRATest.exe');
  GenerateCompileFile(FPC_LINUX_BATCH, 'compile_tests.sh', 'TERRATest.dpr', '.\TERRATest');
End.
