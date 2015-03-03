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
    TERRA_String, TERRA_Resource, TERRA_Collections, TERRA_Stream, TERRA_Application,TERRA_Threads, TERRA_Mutex;


Const
  ResourceUpdateTime = 5000;
  ResourceDiscardTime = 40000;

Type
  ResourceEntry = Class(ListObject)
    Public
      Value:Resource;

      Function ToString():TERRAString; Override;

      Constructor Create(MyResource:Resource);
      Procedure CopyValue(Other:ListObject); Override;
  End;

  ResourceManager = Class(ApplicationComponent)
    Protected
      _LastUpdate:Cardinal;
      _Queue:Queue;

      {$IFNDEF DISABLETHREADS}
      _LockSection:CriticalSection;
      {$ENDIF}

      _Resources:HashTable;

      _Purging:Boolean;

//      Procedure OnAppDestroy; Override;

    Public
      UseThreads:Boolean;

      AutoUnload:Boolean;

      Procedure Init; Override;
      Destructor Destroy; Override;

      Procedure Update; Override;
      Procedure OnContextLost; Override;

      Function GetResource(Name:TERRAString):Resource;
      Procedure AddResource(MyResource:Resource);
      Procedure ReloadResource(Resource:Resource; InBackground:Boolean=True);

      Function GetLoadedResourceCount:Integer;

      Function ResolveResourceLink(Const ResourceName:TERRAString):TERRAString;


      Function Busy:Boolean;

      Procedure Lock;
      Procedure Unlock;

      Procedure Clear;

      Procedure PurgeResources;
      Procedure PreFetch(MyResource:Resource);

      Property Resources:HashTable Read _Resources;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_OS, TERRA_Image, TERRA_GraphicsManager, TERRA_Utils, TERRA_Color,
  TERRA_FileUtils, TERRA_FileStream, TERRA_FileManager;

Type
  ResourceLoader = Class(Task)
    Protected
      _Target:Resource;

    Public
      Constructor Create(Target:Resource);
      Procedure Execute; Override;
  End;

Constructor ResourceLoader.Create(Target:Resource);
Begin
  _Target := Target;
End;

Procedure ResourceLoader.Execute();
Var
  MyResource:Resource;
  Source:Stream;
  Result:Boolean;
  Manager:ResourceManager;
Begin
	MyResource := _Target;
  If (Not Assigned(MyResource)) Or (MyResource.Status = rsReady) Then
    Exit;

  Log(logDebug, 'ResourceManager', 'Obtaining manager for '+MyResource.Name);
  Manager := MyResource.GetManager();
  If (Manager = Nil) Then
  Begin
    Log(logDebug, 'ResourceManager', 'Could not find a manager for '+MyResource.Name);
    MyResource.Status := rsInvalid;
    Exit;
  End;

  Source := FileManager.Instance.OpenStream(MyResource.Location, smRead);
  If (Source=Nil) Then
  Begin
    Log(logDebug, 'ResourceManager', 'Could not open location...');
    Manager.Lock;
    MyResource.Status := rsInvalid;
    Manager.Unlock;
    Exit;
  End;

  Manager.Lock;
  MyResource.Status := rsBusy;
  Manager.Unlock;

  Log(logDebug, 'Resources', 'Loading '+MyResource.Name);

  If Source.Size = 0 Then
  Begin
    Source := FileManager.Instance.OpenStream(MyResource.Location, smRead);
    Log(logWarning, 'Resources', 'Empty resource stream at '+MyResource.Location);
  end;

  Result := MyResource.Load(Source);
  If (Not MyResource.KeepStream) Then
    Source.Destroy;

  If (Not Result) Then
  Begin
    MyResource.Status := rsInvalid;
    Exit;
  End;

  MyResource.Time := GetTime;
  Log(logDebug, 'Resource', 'Loaded '+MyResource.Name);

  If (MyResource.InBackground) Then
  Begin
    Manager.Lock;
    Manager._Queue.Push(ResourceEntry.Create(MyResource));
    Manager.Unlock;
  End Else
  Begin
    Log(logDebug, 'Resource', 'Updating '+MyResource.Name);
    MyResource.Update;
    MyResource.Status := rsReady;
  End;

  Log(logDebug, 'Resource', 'Finished '+MyResource.Name);
End;

Procedure ResourceManager.Init;
Begin
  Log(logDebug, 'Resource', 'Creating resource manager for class: '+Self.ClassName);

  _Resources := HashTable.Create(1024);
  _LastUpdate := 0;
  _Queue := Queue.Create();

{$IFNDEF DISABLETHREADS}
  _LockSection := CriticalSection.Create(Self.ClassName);
{$ENDIF}

  UseThreads := True;
  AutoUnload := True;

  Log(logDebug, 'Resource', 'This resource manager is ready to go!');
End;

Destructor ResourceManager.Destroy;
Var
  I:Integer;
Begin
{$IFNDEF DISABLETHREADS}
  If Assigned(_LockSection) Then
    _LockSection.Destroy;
{$ENDIF}

  If Assigned(_Queue) Then
    _Queue.Destroy;

  If Assigned(_Resources) Then
    _Resources.Destroy;
End;

Procedure ResourceManager.AddResource(MyResource:Resource);
Begin
  If Pos('PAKEMON_0',MyResource.Name)>0 Then
    IntToString(2);

  If (_Resources <> Nil) Then
    _Resources.Add(MyResource)
  Else
    RaiseError('Resource table is null!');
End;

Function SearchResourceByName(P:ListObject; UserData:Pointer):Boolean; CDecl;
Begin
  Result := StringEquals(Resource(P).Name , PString(Userdata)^);
End;

Function ResourceManager.GetResource(Name:TERRAString): Resource;
Begin
  If _Resources = Nil Then
  Begin
    RaiseError('Resource table is null!');
    Result := Nil;
    Exit;
  End;

  Name := GetFileName(Name, True);
  Result := Resource(_Resources.Search(SearchResourceByName, @Name));


  If Assigned(Result) Then
    Log(logDebug, 'Resource', 'Searched for '+Name+': got '+Result.Name)
  Else
    Log(logDebug, 'Resource', 'Searched for '+Name+': got (NIL)');
End;

Procedure ResourceManager.ReloadResource(Resource: Resource; InBackground:Boolean=True);
Begin
  If Not Assigned(Resource) Then
  Begin
    RaiseError('Cannot load null resource!');
    Exit;
  End;

  Log(logDebug, 'ResourceManager', 'Reloading '+Resource.Name);
  Resource.InBackground := InBackground;

  ThreadPool.Instance.RunTask(ResourceLoader.Create(Resource), (InBackground And UseThreads), Nil, Resource.Priority);
End;


{Procedure ResourceManager.OnAppDestroy;
Begin
  If (Assigned(_ResourceManager_Instance)) Then
    _ResourceManager_Instance.Destroy;
End;
}

Procedure ResourceManager.PurgeResources;
Var
  I:Iterator;
  MyResource:Resource;
Begin
  If (Not AutoUnload) Then
    Exit;

  If (_Purging) Then
    Exit;

  _Purging := True;

  I := _Resources.CreateIterator;
  While (I.HasNext) Do
  Begin
    MyResource := Resource(I.GetNext());
    If (MyResource = Nil) Then
      Break;

    If (MyResource.Status<>rsReady) Or (MyResource.Location='') Or (MyResource.KeepStream) Then
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
  I.Destroy;

  _Purging := False;

  _LastUpdate := GetTime;
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

    Self.Lock;
    Entry.Value.Status := rsReady;
    Self.Unlock;

    Entry.Destroy;

    Break; // only one item per frame
  End;

{$IFDEF PC}
  If (GetTime-_LastUpdate<ResourceUpdateTime) Then
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

Procedure ResourceManager.PreFetch(MyResource:Resource);
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
  I:Iterator;
  MyResource:Resource;
Begin
  Result := 0;
  I := _Resources.CreateIterator;
  While (I.HasNext) Do
  Begin
    MyResource := Resource(I.GetNext());
    If (MyResource.Status <> rsBusy) Then
      Inc(Result);
  End;
  I.Destroy;
End;

Function ResourceManager.Busy: Boolean;
Begin
  Result := _Queue.Count>0;
End;

Procedure ResourceManager.Clear;
Var
  I:Iterator;
  N:Integer;
  MyResource:Resource;
Begin
  Log(logDebug, Self.ClassName, 'Unloading all resources.');
  N := 0;
  I := _Resources.CreateIterator;
  While (I.HasNext) Do
  Begin
    MyResource := Resource(I.GetNext());

    MyResource.Status := rsBusy;
   // Log(logDebug,'Resource','Unloaded '+MyResource.Name);
    If MyResource.Unload Then
      MyResource.Status := rsUnloaded
    Else
      MyResource.Status := rsReady;
    Inc(N);
  End;
  I.Destroy;

  Log(logDebug, 'Resources', 'Unloaded '+IntToString(N) + ' resources.');

  _LastUpdate := GetTime;
End;


{ ResourceEntry }
Constructor ResourceEntry.Create(MyResource: Resource);
Begin
  Self.Value := MyResource;
End;

Procedure ResourceEntry.CopyValue(Other: ListObject);
Begin
  Self.Value := ResourceEntry(Other).Value;
End;

Function ResourceEntry.ToString:TERRAString;
Begin
  Result := Value.Name;
End;

Procedure ResourceManager.OnContextLost;
Var
  I:Iterator;
  MyResource:Resource;
Begin
  If (_Resources = Nil) Then
    Exit;

  I := _Resources.CreateIterator;
  While (I.HasNext) Do
  Begin
    MyResource := Resource(I.GetNext());
    If (MyResource.Status = rsReady) Then
    Begin
      Log(logDebug, 'ResourceManager', 'Context lost: '+MyResource.Name);
      MyResource.OnContextLost();
    End;
  End;
  I.Destroy;
End;

Function ResourceManager.ResolveResourceLink(Const ResourceName: TERRAString):TERRAString;
Const
	LinkExtension = '.link';
Var
  Name:TERRAString;
  Src:Stream;
Begin
  Result := '';
  
  If Pos(LinkExtension, ResourceName)>0 Then
  	Exit;

  Src := FileManager.Instance.OpenStream(ResourceName + LinkExtension);
  If Assigned(Src) Then
  Begin
    Src.ReadLine(Name);
    Src.Destroy;

    If (StringLower(Name) = StringLower(ResourceName)) Then
      Exit;

    Result := Name;
  End;
End;

End.
