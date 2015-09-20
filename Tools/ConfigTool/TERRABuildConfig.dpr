program TERRABuildConfig;

{$APPTYPE CONSOLE}

uses
  TERRA_Object, TERRA_Stream, TERRA_FileStream;

Var
  Paths:Array Of String;
  PathCount:Integer;

Procedure RegisterPath(Const Value:String);
Begin
  Inc(PathCount);
  SetLength(Paths, PathCount);
  Paths[Pred(PathCount)] := Value;
End;

Function GenerateCompileCommand(Const BasePath, SrcMain, OutputName:String):String;
Var
  I:Integer;
Begin
  Result := 'fpc -Sew -Mdelphi ';
  Result := Result +' -Fi'+BasePath+'\Core -Fi'+BasePath+'\Utils ';

  For I:=0 To Pred(PathCount) Do
    Result := Result + '-Fu'+BasePath+Paths[I] + ' ';

  If OutputName<>'' Then
    Result := Result + '-o'+OutputName+' ';

  Result := Result + SrcMain;
End;

Var
  Dest:TERRAStream;

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
  RegisterPath('OS\Windows');
  RegisterPath('UI');
  RegisterPath('AI');
  RegisterPath('Audio');
  RegisterPath('Audio\Formats');
  RegisterPath('Audio\Renderers');
  RegisterPath('Network');
  RegisterPath('Libs\Steamworks\headers');
  RegisterPath('Network');
  RegisterPath('Network\Protocols');

  Dest := FileStream.Create('compile_tests.bat');
  Dest.WriteLine(GenerateCompileCommand('d:\code\TERRA-Engine\Engine\','..\Tests\TERRATest.dpr', '.\TERRATest.exe'));
  ReleaseObject(Dest);
End.
