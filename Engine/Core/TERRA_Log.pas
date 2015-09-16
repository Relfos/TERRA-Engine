{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_Log
 * Implements portable logging utilities
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}

Unit TERRA_Log;
{$I terra.inc}
{$ENDIF}

{$IFDEF WINDOWS}{$UNDEF ANDROID}{$ENDIF}


Interface

{$IFNDEF OXYGENE}
{$IFDEF IPHONE}
{$UNDEF USE_LOGFILE}
{$ELSE}
{$DEFINE USE_LOGFILE}
{$ENDIF}

{$IFDEF WINDOWS}
{-$DEFINE CONSOLEWINDOW}
{$ENDIF}

{$IFDEF LINUX}
{.$DEFINE CONSOLEWINDOW}
{$ENDIF}

{$IFDEF OSX}
{.$DEFINE USE_SYSLOG}
{$DEFINE CONSOLEWINDOW}
{$ENDIF}


Uses TERRA_Object, TERRA_FileStream, TERRA_String
{$IFDEF USE_SYSLOG},systemlog{$ENDIF}
{$IFDEF WINDOWS},Windows{$ENDIF}
;

{$ENDIF}
Const
  logDebug   = 0;
  logError   = 1;
  logWarning = 2;
  logConsole = 3;
  logFilterCount = 4;

Type
  LogFilterHandler = {$IFDEF OXYGENE}Public {$ENDIF} Procedure(ModuleName, Description:TERRAString);

  LogEntry = {$IFDEF OXYGENE}Public {$ENDIF} Record  
    ModuleName:TERRAString;
    Description:TERRAString;
  End;

  LogFilter = {$IFDEF OXYGENE}Public {$ENDIF} Record
    FilterType:Integer;
    Modules:TERRAString;
    Handler:LogFilterHandler;
  End;

  TERRALog = Class(TERRAObject)
    Protected
      _Enabled:Boolean;

    {$IFDEF USE_LOGFILE}
    {$IFNDEF USE_SYSLOG}
      _File:FileStream;
    {$ENDIF}
    {$ENDIF}

      _Active:Boolean;
      _Started:Boolean;

      _Filters:Array Of LogFilter;
      _FilterCount:Integer;

      {$IFDEF CONSOLEWINDOW}
      _HasConsole:Boolean;
      {$ENDIF}

      _LastWrite:Cardinal;

      _FileName:TERRAString;

      Function IsReady():Boolean;
      Procedure AddLine(Const S:TERRAString);

      Procedure SetEnabled(Const Value:Boolean);

    Public
      ForceLogFlush:Boolean;

      Constructor Create();
      Procedure Release(); Override;

      Procedure Write(LogType:Integer; Const ModuleName, Description:TERRAString);
      Procedure AddFilter(LogType:Integer; Const Modules:TERRAString; Handler:LogFilterHandler);

      Property Enabled:Boolean Read _Enabled Write SetEnabled;
      Property FileName:TERRAString Read _FileName;
  End;

Implementation

{$IFNDEF OXYGENE}
Uses TERRA_Utils, TERRA_OS, TERRA_Application, TERRA_FileUtils;
{$ENDIF}

{$IFDEF ANDROID}

Const
  ANDROID_LOG_UNKNOWN=0;
  ANDROID_LOG_DEFAULT=1;
  ANDROID_LOG_VERBOSE=2;
  ANDROID_LOG_DEBUG=3;
  ANDROID_LOG_INFO=4;
  ANDROID_LOG_WARN=5;
  ANDROID_LOG_ERROR=6;
  ANDROID_LOG_FATAL=7;
  ANDROID_LOG_SILENT=8;

Function __android_log_write(prio:Integer; tag,text:PAnsiChar):Integer; cdecl; external 'liblog.so' name '__android_log_write';
{$ENDIF}

Function LogFormatStr(LogType:Integer; ModuleName, Description:TERRAString):TERRAString;
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

Procedure TERRALog.AddFilter(LogType:Integer; Const Modules:TERRAString; Handler:LogFilterHandler);
Begin
  If (LogType<0) Or (LogType>=logFilterCount) Or (@Handler = Nil) Then
    Exit;

  Inc(_FilterCount);
  SetLength(_Filters, _FilterCount);
  _Filters[Pred(_FilterCount)].FilterType := LogType;
  _Filters[Pred(_FilterCount)].Modules := StringLower(Modules);
  _Filters[Pred(_FilterCount)].Handler := Handler;
End;

Procedure TERRALog.AddLine(Const S:TERRAString);
Var
  T:Cardinal;
Begin
{$IFNDEF DISABLELOG}
  {$IFDEF CONSOLEWINDOW}
  WriteLn(S);
  {$ENDIF}

  T := Application.GetTime();

  {$IFDEF USE_LOGFILE}

  {$IFDEF USE_SYSLOG}
    syslog(log_info, PAnsiChar(S), []);
  {$ELSE}
  If Assigned(_File) Then
  Begin
    _File.WriteLine(S);
    If (ForceLogFlush) Or (T-_LastWrite>2000) Then
      _File.Flush();
  End;
 {$ENDIF}

  _LastWrite := T;
 {$ENDIF}

{$ENDIF}
End;

Procedure TERRALog.Release();
Begin
  {$IFNDEF DISABLELOG}
  AddLine('End of log session.');

  {$IFDEF CONSOLEWINDOW}
  {$IFDEF WINDOWS}
  If (_LogHasConsole) Then
  Begin
    FreeConsole();
    _LogHasConsole := False;
  End;

  {$ENDIF}
  {$ENDIF}

  {$IFDEF USE_File}
    {$IFDEF USE_SYSLOG}
    closelog();
    {$ELSE}

  ReleaseObject(_File);
	{$ENDIF}
  {$ENDIF}
  {$ENDIF}
End;

Function TERRALog.IsReady():Boolean;
Var
  CurrentTime:TERRATime;
Begin
  If (Not Enabled) Then
  Begin
    Result := False;
    Exit;
  End;

{$IFNDEF DISABLELOG}
  If _Started Then
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

  _FileName := Application.Instance.DocumentPath + PathSeparator+ 'terra.log';
  {$ELSE}
  _FileName := GetFileName(ParamStr(0), True)+'.log';

  {$IFDEF WINDOWS}
  _FileName := GetFilePath(ParamStr(0)) + _FileName;
  {$ENDIF}
  {$ENDIF}


{$IFDEF USE_File}
  {$IFDEF USE_SYSLOG}
  openlog('TERRA',LOG_NOWAIT,LOG_DEBUG);
  {$ELSE}
  _File := FileStream.Create(_FileName);
  {$ENDIF}

  CurrentTime := Application.GetCurrentTime();

  WriteToLog('Log session started at '+TimeToString(CurrentTime, ':', ':', ''));
  WriteToLog('Engine: TERRA '+VersionToString(EngineVersion){$IFDEF FULLDEBUG}+' [Debug mode]'{$ENDIF});
{$ENDIF}

  _Started := True;
  Result := True;
{$ENDIF}
End;


{$IFNDEF OXYGENE}
{$I-}
{$ENDIF}
Procedure TERRALog.Write(LogType:Integer; Const ModuleName, Description:TERRAString);
Var
  I:Integer;
  S:TERRAString;
Begin
{$IFNDEF DISABLELOG}
  //WriteLn(Module,':',Desc);

  {$IFNDEF MOBILE}
  If Not Enabled Then
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

  S := LogFormatStr(LogType, ModuleName, Description);

  {$IFNDEF DISABLETHREADS}
  S := CardinalToString(PtrUInt(GetCurrentThreadId())) + ': '+ S;
  {$ENDIF}

{$IFDEF IPHONE}
  iPhoneLog(PAnsiChar(S));
{$ENDIF}

{$IFDEF ANDROID}
  __android_log_write(ANDROID_LOG_DEBUG, PAnsiChar(ModuleName), PAnsiChar(S));
{$ENDIF}

  {$IFDEF MOBILE}
  If Not LoggingEnabled Then
    Exit;
  {$ENDIF}

  For I:=0 To Pred(_FilterCount) Do
  If (_Filters[I].FilterType = LogType) And ((_Filters[I].Modules='') Or (StringContains(ModuleName, _Filters[I].Modules))) Then
  Begin
    _Filters[I].Handler(ModuleName, Description);
    Exit;
  End;

  If (LogType = logConsole) Then
  Begin
    Application.Instance.LogToConsole(Description);
    Exit;
  End;

{$IFDEF IPHONE}
  Exit;
{$ENDIF}

  If (_Active) Then
    Exit;

  _Active := True;

  If (IsReady()) Then
    AddLine(S);

  _Active := False;
{$ENDIF}
End;

Constructor TERRALog.Create();
Begin
{$IFDEF DEBUG_LOG}
  _Enabled := True;
{$ELSE}
  {$IFDEF PC}
    _Enabled := Application.GetOption('log') = '1';
  {$ELSE}
    {$IFDEF CRASH_REPORT}
      _Enabled := False;
    {$ELSE}
      _Enabled := True;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
End;

Procedure TERRALog.SetEnabled(const Value: Boolean);
Begin
  _Enabled := Value;
End;

End.
