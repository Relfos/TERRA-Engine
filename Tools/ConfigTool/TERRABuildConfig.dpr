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

Procedure GenerateCompileFile(BuildType:Build_Compile_Type; Const CompileFileName, BasePath, SrcMain, OutputName:TERRAString);
Var
  Dest:TERRAStream;
  I:Integer;
  IsWindows:Boolean;
  Result, EnginePathVar, TempPath, TargetOS:TERRAString;
  Sep:TERRAString;
Begin
  Case BuildType Of
    FPC_WINDOWS_BATCH: EnginePathVar := '%ENGINE_PATH%';
    Else
      EnginePathVar := '$ENGINE_PATH';
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

//  BasePath;

  Result := 'fpc -Sew -Mdelphi -dUSE_CONSOLE ';
  Result := Result +' -Fi'+EnginePathVar+Sep+'Core -Fi'+EnginePathVar+Sep+'Utils ';

  For I:=0 To Pred(PathCount) Do
  Begin
    TempPath := Paths[I];

    If Not IsWindows Then
      StringReplaceText('\', '/', TempPath);

    StringReplaceText('$TARGET_OS', TargetOS, TempPath);
    Result := Result + '-Fu'+EnginePathVar+Sep+TempPath + ' ';
  End;

  If OutputName<>'' Then
    Result := Result + '-o'+OutputName+' ';

  Result := Result + SrcMain;

  Dest := FileStream.Create(CompileFileName);

  If IsWindows Then
  Begin
    Dest.EOL := EOL_Windows;
  End Else
  Begin
    Dest.WriteLine('#!/bin/bash');
    Dest.EOL := EOL_Unix;
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


  GenerateCompileFile(FPC_WINDOWS_BATCH, 'compile_tests.bat', 'd:\code\TERRA-Engine\Engine\','%ENGINE_PATH%\..\Tests\TERRATest.dpr', '.\TERRATest.exe');
  GenerateCompileFile(FPC_LINUX_BATCH, 'compile_tests.sh', 'd:\code\TERRA-Engine\Engine\','%ENGINE_PATH%\..\Tests\TERRATest.dpr', '.\TERRATest');
End.
