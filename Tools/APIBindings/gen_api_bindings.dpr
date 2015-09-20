// NOTE: this is quick and dirty job without any error checking, etc, use at your own risk

program genbindings;

{$APPTYPE CONSOLE}

uses
  TERRA_IO, TERRA_FileIO, TERRA_OS, TERRA_Utils,
  SysUtils, BindingGenerator, BindingCPP, BindingPas;

{$I TERRA_types.inc}


Var
  Gen:APIGenerator;
Begin
  SetCurrentDir('..');

  Gen := PasGenerator.Create;
  WriteLn('Executing ',Gen.ClassName);
  Gen.Execute();
  Gen.Destroy();

  Gen := CPPGenerator.Create;
  WriteLn('Executing ',Gen.ClassName);
  Gen.Execute();
  Gen.Destroy();

  WriteLn('Finished');
End.
