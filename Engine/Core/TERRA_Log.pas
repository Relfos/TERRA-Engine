{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}

Unit TERRA_Log;
{$I terra.inc}
{$ENDIF}

{$IFDEF WINDOWS}{$UNDEF ANDROID}{$ENDIF}


{$IFDEF OSX}{$DEFINE USE_SYSLOG}{$ENDIF}

Interface

Uses TERRA_FileIO
{$IFDEF ANDROID},Android_Log{$ENDIF}
{$IFDEF USE_SYSLOG},systemlog{$ENDIF}
{$IFDEF WINDOWS},Windows{$ENDIF}
;

{$IFNDEF IPHONE}
{$DEFINE USE_LOGFILE}
{$ENDIF}

{$IFDEF WINDOWS}
{-$DEFINE CONSOLEWINDOW}
{$ENDIF}

{$IFDEF OSX}
{-$DEFINE CONSOLEWINDOW}
{$ENDIF}


Const
  logDebug   = 0;
  logError   = 1;
  logWarning = 2;
  logConsole = 3;
  logFilterCount = 4;

Type
  LogFilterHandler = Procedure(ModuleName, Description:AnsiString);

  {$IFDEF OXYGENE}
  LogEntry = Class
  {$ELSE}
  LogEntry = Record
  {$ENDIF}
    ModuleName:AnsiString;
    Description:AnsiString;
  End;

  LogFilter = Record
    FilterType:Integer;
    Modules:AnsiString;
    Handler:LogFilterHandler;
  End;

Procedure Log(LogType:Integer; ModuleName, Description:AnsiString);
Procedure AddLogFilter(LogType:Integer; Modules:AnsiString; Handler:LogFilterHandler);

Var
  LoggingDisabled:Boolean;
  ForceLogFlush:Boolean;

Implementation
Uses TERRA_Utils, TERRA_OS, TERRA_Application, TERRA_FileUtils;

Var
  _LogShutdown:Boolean;

{$IFDEF USE_LOGFILE}
  {$IFNDEF USE_SYSLOG}
  _LogFile:FileStream;
{$ENDIF}
  _LogStarted:Boolean;
  _LogActive:Boolean;
{$ENDIF}


  _LogFileName:AnsiString;

  _Filters:Array Of LogFilter;
  _FilterCount:Integer;

  {$IFDEF CONSOLEWINDOW}
  _LogHasConsole:Boolean;
  {$ENDIF}

  _LastWrite:Cardinal;

Function LogFormatStr(LogType:Integer; ModuleName, Description:AnsiString):AnsiString;
Begin
  If ModuleName<>'' Then
  Begin
    Description := '['+ModuleName+'] '+Description;
  End;

  Case LogType Of
    logDebug:    Description := 'Debug: ' + Description;
    logWarning:  Description := 'Warning: ' + Description;
    logError:    Description := 'Error: '+ Description;
  End;

  Result := Description;
End;

Procedure AddLogFilter(LogType:Integer; Modules:AnsiString; Handler:LogFilterHandler);
Begin
  If (LogType<0) Or (LogType>=logFilterCount) Or (@Handler = Nil) Then
    Exit;

  Inc(_FilterCount);
  SetLength(_Filters, _FilterCount);
  _Filters[Pred(_FilterCount)].FilterType := LogType;
  _Filters[Pred(_FilterCount)].Modules := LowStr(Modules);
  _Filters[Pred(_FilterCount)].Handler := Handler;
End;

Procedure WriteToLog(S:AnsiString);
Var
  T:Cardinal;
Begin
{$IFNDEF DISABLELOG}
  {$IFDEF CONSOLEWINDOW}
  WriteLn(S);
  {$ENDIF}

  T := GetTime();

  {$IFDEF USE_LOGFILE}

  {$IFDEF USE_SYSLOG}
    syslog(log_info, PAnsiChar(S), []);
  {$ELSE}
  If Assigned(_LogFile) Then
  Begin
    _LogFile.WriteLine(S);
    If (ForceLogFlush) Or (T-_LastWrite>2000) Then
      _LogFile.Flush();
  End;
 {$ENDIF}

  _LastWrite := T;
 {$ENDIF}

{$ENDIF}
End;

{$IFNDEF OXYGENE}
Procedure Log_Shutdown();
Begin
  If _LogShutdown Then
    Exit;

  _LogShutdown := True;

  {$IFNDEF DISABLELOG}
  If (_LogStarted) Then
    WriteToLog('End of log session.');

  {$IFDEF CONSOLEWINDOW}
  {$IFDEF WINDOWS}
  If (_LogHasConsole) Then
  Begin
    FreeConsole();
    _LogHasConsole := False;
  End;

  {$ENDIF}
  {$ENDIF}
  
  {$IFDEF USE_LOGFILE}
    {$IFDEF USE_SYSLOG}
    closelog();
    {$ELSE}
  If Assigned(_LogFile) Then
  Begin
    _LogFile.Destroy();
    _LogFile := Nil;
  End;
	{$ENDIF}
  {$ENDIF}
  {$ENDIF}

End;
{$ENDIF}

Function Log_Ready():Boolean;
Var
  CurrentTime:TERRATime;
Begin
  If _LogShutdown Then
  Begin
    Result := False;
    Exit;
  End;

{$IFNDEF DISABLELOG}
  If _LogStarted Then
  Begin
    Result := True;
    Exit;
  End;

  {$IFNDEF PC}
  If (Application.Instance = Nil) Or (Application.Instance.DocumentPath='') Then
  Begin
    Result := False;
    Exit;
  End;

  _LogFileName := Application.Instance.DocumentPath + PathSeparator+ 'terra.log';
  {$ELSE}
  _LogFileName := GetFileName(ParamStr(0), True)+'.log';

  {$IFDEF WINDOWS}
  _LogFileName := GetFilePath(ParamStr(0)) + _LogFileName;
  {$ENDIF}
  {$ENDIF}


{$IFDEF USE_LOGFILE}
  {$IFDEF USE_SYSLOG}
  openlog('TERRA',LOG_NOWAIT,LOG_DEBUG);
  {$ELSE}
  _LogFile := FileStream.Create(_LogFileName);
  {$ENDIF}

  CurrentTime := GetCurrentTime();

  WriteToLog('Log session started at '+TimeToString(CurrentTime));
  WriteToLog('Engine: TERRA '+VersionToString(EngineVersion){$IFDEF FULLDEBUG}+' [Debug mode]'{$ENDIF});
{$ENDIF}

  _LogStarted := True;
  Result := True;
{$ENDIF}
End;


{$IFNDEF OXYGENE}
{$I-}
{$ENDIF}
Procedure Log(LogType:Integer; ModuleName, Description:AnsiString);
Var
  I:Integer;
  S:AnsiString;
Begin
  If _LogShutdown Then
    Exit;

{$IFNDEF DISABLELOG}
  //WriteLn(Module,':',Desc);

  {$IFNDEF MOBILE}
  If LoggingDisabled Then
    Exit;
  {$ENDIF}

  {$IFDEF CONSOLEWINDOW}
  {$IFDEF WINDOWS}
  If (Not _LogHasConsole) Then
  Begin
    _LogHasConsole := True;
    AllocConsole();
  End;
  {$ENDIF}
  {$ENDIF}

  If (LogType<0) Or (LogType>logFilterCount) Then
    LogType := 0;

  {$IFNDEF DISABLETHREADS}
  Description := CardinalToString(Cardinal(GetCurrentThreadId())) + ': '+Description;
  {$ENDIF}

  Description := LogFormatStr(LogType, ModuleName, Description);

{$IFDEF IPHONE}
  iPhoneLog(PAnsiChar(Description));
{$ENDIF}

{$IFDEF ANDROID}
  __android_log_write(ANDROID_LOG_DEBUG, PAnsiChar(ModuleName), PAnsiChar(Description));
{$ENDIF}

  {$IFDEF MOBILE}
  If LoggingDisabled Then
    Exit;
  {$ENDIF}

  S := LowStr(ModuleName);
  For I:=0 To Pred(_FilterCount) Do
  If (_Filters[I].FilterType = LogType) And ((_Filters[I].Modules='') Or (Pos(S, _Filters[I].Modules)>0)) Then
  Begin
    _Filters[I].Handler(ModuleName, Description);
    Exit;
  End;

{$IFDEF IPHONE}
  Exit;
{$ENDIF}

  If (_LogActive) Then
    Exit;

  _LogActive := True;

  If (Log_Ready()) Then
    WriteToLog(Description);
    
  _LogActive := False;
{$ENDIF}
End;

Initialization
Finalization
  Log_Shutdown();
End.
