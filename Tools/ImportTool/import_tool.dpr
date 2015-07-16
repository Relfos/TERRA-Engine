program import_tool;

{$I terra.inc}

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$DEFINE HIDESKIPS}
{$ENDIF}

uses
  TERRA_String,
  TERRA_Utils,
  TERRA_Application,
  TERRA_ConsoleApplication,
  TERRA_OS,
  TERRA_Log,
  SysUtils,
  TERRA_ImportSystem;

Const
	AppVersion = '1.3';


Begin
  LoggingEnabled := True;

  Try
    ConsoleApplication.Create();
    WriteLn('[TERRA Asset importer v'+AppVersion+']');

  Except
    On E:Exception Do
    Begin
      WriteLn('Fatal Error: '+E.Message);
      Halt(1);
    End;
  End;

	WriteLn('Finished!');
  //Application.Instance.Terminate;
End.
