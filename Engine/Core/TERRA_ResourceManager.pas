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
 * TERRA_ResourceManager
 * Implements a generic Resource Manager
 ***********************************************************************************************************************
}

Unit TERRA_ResourceManager;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
    TERRA_Object, TERRA_String, TERRA_Resource, TERRA_Collections, TERRA_Stream, TERRA_Application,
    TERRA_Threads, TERRA_Mutex, TERRA_Hashmap, TERRA_Queue;


Const
  ResourceUpdateTime = 5000;
  ResourceDiscardTime = 60000;

Type
  ResourceEntry = Class(TERRAObject)
    Public
      Value:TERRAResource;

      Function GetBlob():TERRAString; Override;

      Constructor Create(MyResource:TERRAResource);
  End;

  ResourceManager = Class(TERRAObject)
    Protected
      _LastUpdate:Cardinal;
      _Queue:TERRAQueue;

      {$IFNDEF DISABLETHREADS}
      _LockSection:CriticalSection;
      {$ENDIF}

      _Resources:TERRAHashMap;

      _Purging:Boolean;

//      Procedure OnAppDestroy; Override;

    Public
      UseThreads:Boolean;

      AutoUnload:Boolean;

      Constructor Create();
      Procedure Release; Override;

      Procedure Update; Virtual;

      Function GetResource(Const Name:TERRAString):TERRAResource;
      Procedure AddResource(MyResource:TERRAResource);
      Procedure ReloadResource(Resource:TERRAResource; InBackground:Boolean=True);

      Function GetLoadedResourceCount:Integer;

      Function Busy:Boolean;

      Procedure Lock;
      Procedure Unlock;

      Procedure Clear;

      Procedure PurgeResources;
      Procedure PreFetch(MyResource:TERRAResource);

      Property Resources:TERRAHashMap Read _Resources;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_Image, TERRA_GraphicsManager, TERRA_Utils, TERRA_Color,
  TERRA_Engine, TERRA_FileUtils, TERRA_FileStream, TERRA_FileManager;

Type
  ResourceLoader = Class(TERRATask)
    Protected
      _Target:TERRAResource;

    Public
      Constructor Create(Target:TERRAResource);
      Procedure Execute; Override;
  End;

Constructor ResourceLoader.Create(Target:TERRAResource);
Begin
  _Target := Target;
End;

Procedure ResourceLoader.Execute();
Var
  MyResource:TERRAResource;
  Source:TERRAStream;
  Result:Boolean;
  Manager:ResourceManager;
Begin
	MyResource := _Target;
  If (Not Assigned(MyResource)) Or (MyResource.Status = rsReady) Then
    Exit;

  Log(logDebug, 'ResourceManager', 'Obtaining manager for '+MyResource.Name);
  Manager := ResourceManager(MyResource.GetManager());
  If (Manager = Nil) Then
  Begin
    Log(logDebug, 'ResourceManager', 'Could not find a manager for '+MyResource.Name);
    MyResource.Status := rsInvalid;
    Exit;
  End;

  Source := Engine.Files.OpenLocation(MyResource.Location);
  If (Source=Nil) Then
  Begin
    Log(logDebug, 'ResourceManager', 'Could not open location...');
    MyResource.Status := rsInvalid;
    Exit;
  End;

  MyResource.Status := rsBusy;

  Log(logDebug, 'Resources', 'Loading '+MyResource.Name);

  If Source.Size = 0 Then
  Begin
    //Source := Engine.Files.OpenLocation(MyResource.Location);
    Log(logWarning, 'Resources', 'Empty resource stream at '+MyResource.Name);
  End;

  Result := MyResource.Load(Source);
  If (MyResource.Kind <> rtStreamed) Then
    ReleaseObject(Source);

  If (Not Result) Then
  Begin
    MyResource.Status := rsInvalid;
    Exit;
  End;

  MyResource.Time := Application.GetTime;
  Log(logDebug, 'Resource', 'Loaded '+MyResource.Name);

  If (Manager.UseThreads) Then
  Begin
    Manager.Lock;
    Manager._Queue.Push(ResourceEntry.Create(MyResource));
    Manager.Unlock;
  End Else
  Begin
    Log(logDebug, 'Resource', 'Updating '+MyResource.Name);
    MyResource.Update();
    MyResource.Status := rsReady;
  End;

  Log(logDebug, 'Resource', 'Finished '+MyResource.Name);
End;

Constructor ResourceManager.Create();
Begin
  Log(logDebug, 'Resource', 'Creating resource manager for class: '+Self.ClassName);

  _Resources := TERRAHashMap.Create(1024);
  _LastUpdate := 0;
  _Queue := TERRAQueue.Create();

{$IFNDEF DISABLETHREADS}
  _LockSection := CriticalSection.Create({Self.ClassName});
{$ENDIF}

  UseThreads := False;

  AutoUnload := False;

  Log(logDebug, 'Resource', 'This resource manager is ready to go!');
End;

Procedure ResourceManager.Release;
Var
  I:Integer;
Begin
  ReleaseObject(_Resources);

  ReleaseObject(_Queue);
{$IFNDEF DISABLETHREADS}
  ReleaseObject(_LockSection);
{$ENDIF}
End;

Procedure ResourceManager.AddResource(MyResource:TERRAResource);
Begin
  If (_Resources <> Nil) Then
    _Resources.Add(MyResource)
  Else
    Engine.RaiseError('Resource table is null!');
End;

Function ResourceManager.GetResource(Const Name:TERRAString): TERRAResource;
Var
  Temp:TERRAString;
Begin
  If _Resources = Nil Then
  Begin
    Engine.RaiseError('Resource table is null!');
    Result := Nil;
    Exit;
  End;

  If (StringContainsChar('.', Name)) Then
    Temp := GetFileName(Name, True)
  Else
    Temp := Name;

  Result := TERRAResource(_Resources.Items[Temp]);

  {If Assigned(Result) Then
    Log(logDebug, 'Resource', 'Searched for '+Name+': got '+Result.Name)
  Else
    Log(logDebug, 'Resource', 'Searched for '+Name+': got (NIL)');}
End;

Procedure ResourceManager.ReloadResource(Resource: TERRAResource; InBackground:Boolean=True);
Begin
  If Not Assigned(Resource) Then
  Begin
    Engine.RaiseError('Cannot load null resource!');
    Exit;
  End;

  If InBackground Then
    Log(logDebug, 'ResourceManager', 'Reloading '+Resource.Name+' in background')
  Else
    Log(logDebug, 'ResourceManager', 'Reloading '+Resource.Name+' in foreground');

  Engine.Tasks.RunTask(ResourceLoader.Create(Resource), (InBackground And UseThreads), Nil, Resource.Priority);
End;


{Procedure ResourceManager.OnAppDestroy;
Begin
  If (Assigned(_ResourceManager_Instance)) Then
    _ResourceManager_Instance.Release;
End;
}

Procedure ResourceManager.PurgeResources;
Var
  It:Iterator;
  MyResource:TERRAResource;
Begin
  If (Not AutoUnload) Then
    Exit;

  If (_Purging) Then
    Exit;

  _Purging := True;

  It := _Resources.GetIterator();
  While (It.HasNext) Do
  Begin
    MyResource := TERRAResource(It.Value);
    If (MyResource = Nil) Then
      Break;

    If (MyResource.Status<>rsReady) Or (MyResource.Kind <> rtLoaded) Then
      Continue;

    If (MyResource.ShouldUnload()) Then
    Begin
      MyResource.Status := rsBusy;
      Log(logDebug,'Resource','Unloaded '+MyResource.Name);
      If MyResource.Unload Then
        MyResource.Status := rsUnloaded
      Else
        MyResource.Status := rsReady;
    End;
  End;
  ReleaseObject(It);

  _Purging := False;

  _LastUpdate := Application.GetTime;
End;

Procedure ResourceManager.Update;
Var
  Entry:ResourceEntry;
Begin
  If (_Queue = Nil) Then
    Exit;

  While (_Queue.Count>0) Do
  Begin
    Self.Lock;
    Entry := ResourceEntry(_Queue.Pop());
    Self.Unlock;
    Entry.Value.Update;

    Entry.Value.Status := rsReady;

    ReleaseObject(Entry);

    Break; // only one item per frame
  End;

{$IFDEF PC}
  If (Application.GetTime-_LastUpdate<ResourceUpdateTime) Then
    Exit;

  PurgeResources();
{$ENDIF}
End;

Procedure ResourceManager.Lock;
Begin
  {$IFNDEF DISABLETHREADS}
  _LockSection.Lock();
  {$ENDIF}
End;

Procedure ResourceManager.Unlock;
Begin
  {$IFNDEF DISABLETHREADS}
  _LockSection.Unlock();
  {$ENDIF}
End;

Procedure ResourceManager.PreFetch(MyResource:TERRAResource);
Begin
  If Assigned(MyResource) Then
  Begin
    If MyResource.Status <> rsUnloaded Then
      Exit;
      
    MyResource.Prefetch();
  End;
End;

Function ResourceManager.GetLoadedResourceCount:Integer;
Var
  It:Iterator;
  MyResource:TERRAResource;
Begin
  Result := 0;
  It := _Resources.GetIterator();
  While (It.HasNext) Do
  Begin
    MyResource := TERRAResource(It.Value);
    If (MyResource.Status <> rsBusy) Then
      Inc(Result);
  End;
End;

Function ResourceManager.Busy: Boolean;
Begin
  Result := _Queue.Count>0;
End;

Procedure ResourceManager.Clear;
Var
  It:Iterator;
  N:Integer;
  MyResource:TERRAResource;
Begin
  Log(logDebug, Self.ClassName, 'Unloading all resources.');
  N := 0;
  It := _Resources.GetIterator();
  While (It.HasNext) Do
  Begin
    MyResource := TERRAResource(It.Value);

    MyResource.Status := rsBusy;
   // Log(logDebug,'Resource','Unloaded '+MyResource.Name);
    If MyResource.Unload Then
      MyResource.Status := rsUnloaded
    Else
      MyResource.Status := rsReady;
    Inc(N);
  End;

  Log(logDebug, 'Resources', 'Unloaded '+ IntegerProperty.Stringify(N) + ' resources.');

  _LastUpdate := Application.GetTime;
End;


{ ResourceEntry }
Constructor ResourceEntry.Create(MyResource: TERRAResource);
Begin
  Self.Value := MyResource;
End;

Function ResourceEntry.GetBlob:TERRAString;
Begin
  Result := Value.Name;
End;

(*Procedure ResourceManager.OnContextLost;
Var
  It:Iterator;
  MyResource:Resource;
Begin
  If (_Resources = Nil) Then
    Exit;

  It := _Resources.GetIterator();
  While (It.HasNext) Do
  Begin
    MyResource := Resource(It.Value);
    If (MyResource.Status = rsReady) Then
    Begin
      Log(logDebug, 'ResourceManager', 'Context lost: '+MyResource.Name);
      MyResource.OnContextLost();
    End;
  End;
End;*)

End.
