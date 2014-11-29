Unit TERRA_ResourceLoader;

{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Application, TERRA_Resource;

Type
  ResourceLoader = Class(ApplicationComponent)
    Protected
      _Resources:Array Of Resource;
      _ResourceCount:Integer;
      _Index:Integer;
      _FistTime:Boolean;
      _LastProgress:Single;

      Procedure Update; Override;
      Procedure Init; Override;

    Public
      Class Function Instance:ResourceLoader;
      Destructor Destroy; Override;

      Procedure Clear;
      Procedure AddResource(Res:Resource);
      Function GetCurrent:Resource;

      Function GetProgress:Single;
      Function IsReady:Boolean;
  End;

Implementation
Uses TERRA_ResourceManager, TERRA_Mesh, TERRA_Log;

{ LoaderManager }
Var
  _LoaderManagerInstance:ApplicationObject;

Procedure ResourceLoader.AddResource(Res: Resource);
Var
  I:Integer;
Begin
  If (Res= Nil) Then
  Begin
    IntToString(2);
    Exit;
  End;

  For I:=0 To Pred(_ResourceCount) Do
  If (_Resources[I] = Res) Then
    Exit;

  Inc(_ResourceCount);
  SetLength(_Resources, _ResourceCount);
  _Resources[Pred(_ResourceCount)] := Res;
  Log(logDebug, 'Loader', 'Adding resource: '+Res.Name);
End;

Procedure ResourceLoader.Clear;
Begin
  _Index := 0;
  _ResourceCount :=0 ;
  _FistTime := True;
  _LastProgress := 0;
End;

Procedure ResourceLoader.Init;
Begin
  Self.Clear;
End;

Destructor ResourceLoader.Destroy;
Begin
  _LoaderManagerInstance := Nil;
End;

Function ResourceLoader.GetCurrent:Resource;
Begin
  If _ResourceCount<=0 Then
    Result := Nil
  Else
    Result := _Resources[_Index];
End;

Function ResourceLoader.GetProgress:Single;
Var
  I, Count:Integer;
Begin
  Count := 0;
  For I:=0 To Pred(_ResourceCount) Do
  If (_Resources[I].Status = rsReady) Then
    Inc(Count);

  If (_ResourceCount<=0) Then
    Result := 100
  Else
    Result := (Count/_ResourceCount)*100;
  If (Result<_LastProgress) Then
    Result := _LastProgress;

  _LastProgress := Result;
End;

Class Function ResourceLoader.Instance:ResourceLoader;
Begin
  If _LoaderManagerInstance = Nil Then
    _LoaderManagerInstance := InitializeApplicationComponent(ResourceLoader, Nil);

  Result := ResourceLoader(_LoaderManagerInstance.Instance);
End;

Function ResourceLoader.IsReady: Boolean;
Begin
  Result := GetProgress>=100;
End;

Procedure ResourceLoader.Update;
Var
  J:Integer;
  Group:MeshGroup;
  MyMesh:Mesh;
Begin
  If (_ResourceCount<=0) Then
    Exit;

  If _FistTime Then
  Begin
    _FistTime := False;
  End;

  _Resources[_Index].IsReady();
  If (_Resources[_Index].Status = rsReady) Then
  Begin
      If _Resources[_Index] Is Mesh Then
      Begin
        MyMesh := Mesh(_Resources[_Index]);
        For J:=0 To Pred(MyMesh.GroupCount) Do
        Begin
          Group := MyMesh.GetGroup(J);
          Self.AddResource(Group.DiffuseMap);
          Self.AddResource(Group.NormalMap);
          Self.AddResource(Group.SpecularMap);
          Self.AddResource(Group.GlowMap);
          Self.AddResource(Group.LightMap);
          Self.AddResource(Group.ColorRamp);
        End;
      End;

    If (_Index<Pred(_ResourceCount)) Then
      Inc(_Index);
  End;
End;

End.
