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
Uses TERRA_String, TERRA_Collections, TERRA_Hashmap, TERRA_Stream;

Const
  rsUnloaded  = 0;
  rsBusy      = 1;
  rsInvalid   = 2;
  rsReady     = 3;

Type
  ResourceClass = Class Of Resource;

  Resource = Class(HashMapObject)
    Protected
      _Time:Cardinal;
      _Location:TERRAString;
      _Status:Integer;
      _Size:Integer;
      _KeepStream:Boolean;
      _ContextID:Integer;

      Procedure CopyValue(Other:CollectionObject); Override;
      Function Sort(Other:CollectionObject):Integer; Override;

    Public
      Priority:Integer;

      Constructor Create(Location:TERRAString);
      Procedure Release; Override;

      Function IsReady:Boolean;

      Class Function GetManager:Pointer; Virtual;

      Function Load(MyStream:Stream):Boolean; Virtual;Abstract;
      Function LoadFromFile(Const FileName:TERRAString):Boolean;

      Function Unload:Boolean; Virtual;
      Function Update:Boolean; Virtual;
      Procedure OnContextLost(); Virtual;


      Function ToString():TERRAString; Override;

      Procedure Prefetch;

      Function ShouldUnload():Boolean;

      Property Name:TERRAString Read _Key;
      Property Location:TERRAString Read _Location;
      Property Time:Cardinal Read _Time Write _Time;
      Property Status:Integer Read _Status Write _Status;
      Property Size:Integer Read _Size;
      Property KeepStream:Boolean Read _KeepStream Write _KeepStream;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Utils, TERRA_ResourceManager, TERRA_FileStream, TERRA_GraphicsManager,
  TERRA_FileUtils, TERRA_Application, TERRA_FileManager;

Procedure Resource.CopyValue(Other: CollectionObject);
Begin
  RaiseError('Not implemented!');
End;

Constructor Resource.Create(Location:TERRAString);
Var
  I:Integer;
Begin
  If Pos('@', Location) = 1 Then
  Begin
    Self._Key := Location;
    Self._Location := '';
  End Else
  Begin
    Self._Key := GetFileName(Location,True);
    Self._Location := Location;
  End;

  Self._Size := 0;
  Self._Status := rsUnloaded;
  Self.Priority := 50;
End;

Procedure Resource.Release;
Begin
  Log(logDebug, 'Resource', 'Destroying resource '+Self.Name);
  {$IFNDEF ANDROID}
  Self.Unload();
  {$ENDIF}
End;

Class Function Resource.GetManager: Pointer;
Begin
  Result := Nil;
End;

Function Resource.IsReady:Boolean;
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
    If (Application.Instance<>Nil) And (_ContextID <> Application.Instance.ContextID) Then
    Begin
      Result := False;
      Log(logWarning, 'Resource', 'Invalid context in '+Self._Key);
      Self.OnContextLost();
    End Else
    Begin
      _Time := Application.GetTime;
      Result := True;
    End;

    Exit;
  End;

  Result := False;
  If (Status <> rsUnloaded) Then
    Exit;

  If (Not _Prefetching) And (Application.Instance<>Nil) And (Application.Instance.FrameTime>500) Then
  Begin
    Result := False;
    Exit;
  End;

  Log(logDebug, 'Resource', 'Obtaining manager for '+Self.Name);
  Manager := Self.GetManager;
  If (Manager = Nil) Then
  Begin
    Log(logDebug, 'Resource', 'Failed to obtain a manager...');
    Exit;
  End;

  Manager.Lock;
  _Status := rsBusy;
  Manager.Unlock;

  If (Self.Location<>'') Then
  Begin
    Log(logDebug, 'Resource', 'Loading the resource...');
    Manager.ReloadResource(Self, Manager.UseThreads);
    Result := (Status = rsReady);
  End Else
  Begin
    Log(logDebug, 'Resource', 'Updating the resource...');

    Self.Update();

    Manager.Lock;
    _Status := rsReady;
    Manager.Unlock;
    Result := True;
  End;
End;

Procedure Resource.Prefetch;
Begin
  If (Self._Status<>rsUnloaded) Then
    Exit;

  Log(logDebug, 'Resource', 'Prefetching '+ Self.Name);

  If Self.IsReady() Then
    Exit;

  If _Prefetching Then
  Begin
    Log(logDebug, 'Resource', 'Prefetch overflow!');
    Exit;
  End;

  If (Application.Instance = Nil) Then
  Begin
    Self._Status := rsReady;
    Exit;
  End;

  Log(logDebug, 'Resource', 'Prefetching '+Self.Name);
  _Prefetching := True;
  While (Not Self.IsReady) Do
  Begin
    Application.Instance.RefreshComponents();

    If (Self._Status = rsInvalid) Then
      Break;

  End;
  _Prefetching := False;

  If (Self._Status = rsInvalid) Then
    Log(logError, 'Resource', 'Error prefetching resource')
  Else
    Log(logDebug, 'Resource', 'Prefetching for '+Self.Name+' is done!');
End;

Function Resource.Sort(Other: CollectionObject): Integer;
Begin
  Result := GetStringSort(Self.Name, Resource(Other).Name);
End;

Function Resource.ToString:TERRAString;
Begin
  Result := Self.Name;
End;

Function Resource.Unload:Boolean;
Begin
  Result := False;
End;

Function Resource.Update:Boolean;
Begin
  Result := True;
  If (Application.Instance<>Nil) Then
    _ContextID := Application.Instance.ContextID;
End;

Procedure Resource.OnContextLost;
Begin
  Self.Unload();
End;

Function Resource.ShouldUnload: Boolean;
Begin
  Result := (Application.GetTime() - Self.Time > ResourceDiscardTime);
End;

Function Resource.LoadFromFile(const FileName: TERRAString): Boolean;
Var
  Src:Stream;
Begin
  Self.Unload();
  Src := FileManager.Instance.OpenStream(FileName);
  Result := Self.Load(Src);
  ReleaseObject(Src);
End;

End.
