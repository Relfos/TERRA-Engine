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
 * TERRA_Resource
 * Implements the generic Resource class
 ***********************************************************************************************************************
}
Unit TERRA_Resource;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Collections, TERRA_Hashmap, TERRA_Stream, TERRA_FileManager;

Type
  ResourceStatus = (
    rsUnloaded  = 0,
    rsBusy      = 1,
    rsInvalid   = 2,
    rsReady     = 3
  );

  ResourceType = (
    rtLoaded    = 0,
    rtStreamed  = 1,
    rtDynamic   = 2
  );

  TERRAResource = Class(TERRAObject)
    Private
      _Status:ResourceStatus;
      _Kind:ResourceType;

    Protected
      _Time:Cardinal;
      _Location:TERRALocation;
      _SizeInBytes:Cardinal;

      Procedure SetStatus(const Value:ResourceStatus);

      Function Build():Boolean; Virtual;

    Public
      Priority:Integer;

      Constructor Create(Kind:ResourceType; Location:TERRALocation = Nil);
      Procedure Release; Override;

      Procedure Touch;

      Function IsReady:Boolean;

      Class Function GetManager:Pointer; Virtual;

      Function Load(MyStream:Stream):Boolean; Virtual;Abstract;
      Function Unload:Boolean; Virtual;
      Function Update:Boolean; Virtual;

      Procedure Rebuild();

      Function GetBlob():TERRAString; Override;

      Procedure Prefetch;

      Function ShouldUnload():Boolean;

      Property Name:TERRAString Read _ObjectName;
      Property Location:TERRALocation Read _Location;
      Property Time:Cardinal Read _Time Write _Time;
      Property Status:ResourceStatus Read _Status Write SetStatus;
      Property Kind:ResourceType Read _Kind;
      Property SizeInBytes:Cardinal Read _SizeInBytes;
  End;

  ResourceClass = Class Of TERRAResource;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Utils, TERRA_ResourceManager, TERRA_FileStream, TERRA_GraphicsManager,
  TERRA_FileUtils, TERRA_Application;

Constructor TERRAResource.Create(Kind:ResourceType; Location:TERRALocation);
Var
  I:Integer;
Begin
  Self._Kind := Kind;

  If Assigned(Location) Then
    Self._ObjectName := GetFileName(Location.Path, True);

  Self._Location := Location;

  Self._SizeInBytes := 0;
  Self.SetStatus(rsUnloaded);
  Self.Priority := 50;
End;

Procedure TERRAResource.Release;
Begin
  Log(logDebug, 'Resource', 'Destroying resource '+Self.Name);
  {$IFNDEF ANDROID}
  Self.Unload();
  {$ENDIF}
End;

Class Function TERRAResource.GetManager: Pointer;
Begin
  Result := Nil;
End;

Function TERRAResource.IsReady:Boolean;
Var
  Manager:ResourceManager;
Begin
//  Log(logDebug, 'Resource', 'Calling isReady()...');

  If (Self = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

  If (Status = rsReady) Then
  Begin
    Self.Touch();
    Result := True;
    Exit;
  End;

  Result := False;
  If (Status <> rsUnloaded) Then
    Exit;

  (*If (Not _Prefetching) And (Application.Instance.FrameTime>500) Then
  Begin
    Result := False;
    Exit;
  End;*)

  Log(logDebug, 'Resource', 'Obtaining manager for '+Self.Name);
  Manager := Self.GetManager;
  If (Manager = Nil) Then
  Begin
    Log(logDebug, 'Resource', 'Failed to obtain a manager...');
    Exit;
  End;

  If (Assigned(Self.Location)) Then
  Begin
    Self.SetStatus(rsBusy);
    Log(logDebug, 'Resource', 'Loading the resource...');
    Manager.ReloadResource(Self, Manager.UseThreads);
    Result := (Status = rsReady);
  End Else
  Begin
    Log(logDebug, 'Resource', 'Updating the resource...' + Self.ClassName);

    Self.Rebuild();

    Result := True;
  End;
End;

Procedure TERRAResource.Prefetch;
Begin
  If (Self.Status<>rsUnloaded) Then
    Exit;

  Log(logDebug, 'Resource', 'Prefetching '+ Self.Name);

  If Self.IsReady() Then
    Exit;

  Log(logDebug, 'Resource', 'Prefetching '+Self.Name);

  While (Not Self.IsReady) Do
  Begin
    Application.Instance.RefreshComponents();

    If (Self.Status = rsInvalid) Then
      Break;

  End;

  If (Self.Status = rsInvalid) Then
    Log(logError, 'Resource', 'Error prefetching resource')
  Else
    Log(logDebug, 'Resource', 'Prefetching for '+Self.Name+' is done!');
End;

Function TERRAResource.GetBlob:TERRAString;
Begin
  Result := Self.Name;
End;

Function TERRAResource.Unload:Boolean;
Begin
  SetStatus(rsUnloaded);
  Result := True;
End;

Function TERRAResource.Update:Boolean;
Begin
  Result := True;
End;

Function TERRAResource.ShouldUnload: Boolean;
Begin
  Result := (Application.GetTime() - Self.Time > ResourceDiscardTime);
End;

Procedure TERRAResource.SetStatus(const Value:ResourceStatus);
Var
  Manager:ResourceManager;
Begin
  {If Value<>rsUnloaded Then
    StringToInt(Self._Key);}

  If (_Location = Nil) Then
  Begin
    _Status := Value;
    Exit;
  End;

  Manager := Self.GetManager;
  If (Manager = Nil) Then
  Begin
    Log(logDebug, 'Resource', 'Failed to obtain a manager...');
    Exit;
  End;

  Manager.Lock();
  _Status := Value;
  Manager.Unlock();
End;

Function TERRAResource.Build: Boolean;
Begin
  Result := False;
End;

Procedure TERRAResource.Rebuild;
Begin
  Self.SetStatus(rsBusy);
  If Self.Build() Then
  Begin
    Self.Update();
    Self.SetStatus(rsReady);
  End Else
    Self.SetStatus(rsUnloaded);
End;

Procedure TERRAResource.Touch;
Begin
  _Time := Application.GetTime();
End;


End.
