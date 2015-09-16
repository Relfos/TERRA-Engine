Unit TERRA_ResourceLoader;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_Resource;

Type
  ResourceLoader = Class(TERRAObject)
    Protected
      _Resources:Array Of TERRAResource;
      _ResourceCount:Integer;
      _Index:Integer;
      _FistTime:Boolean;
      _LastProgress:Single;


    Public
      Constructor Create;

      Procedure Update;

      Procedure Clear;
      Procedure AddResource(Res:TERRAResource);
      Function GetCurrent:TERRAResource;

      Function GetProgress:Single;
      Function IsReady:Boolean;
  End;

Implementation
Uses TERRA_ResourceManager, TERRA_Mesh, TERRA_Log, TERRA_Engine;

{ ResourceLoader }
Constructor ResourceLoader.Create;
Begin
  Self.Clear;
End;

Procedure ResourceLoader.AddResource(Res: TERRAResource);
Var
  I:Integer;
Begin
  If (Res= Nil) Then
  Begin
    Exit;
  End;

  For I:=0 To Pred(_ResourceCount) Do
  If (_Resources[I] = Res) Then
    Exit;

  Inc(_ResourceCount);
  SetLength(_Resources, _ResourceCount);
  _Resources[Pred(_ResourceCount)] := Res;
  Engine.Log.Write(logDebug, 'Loader', 'Adding resource: '+Res.Name);
End;

Procedure ResourceLoader.Clear;
Begin
  _Index := 0;
  _ResourceCount :=0 ;
  _FistTime := True;
  _LastProgress := 0;
End;

Function ResourceLoader.GetCurrent:TERRAResource;
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

Function ResourceLoader.IsReady: Boolean;
Begin
  Result := GetProgress>=100;
End;

Procedure ResourceLoader.Update;
Var
  J:Integer;
  Group:MeshGroup;
  MyMesh:TERRAMesh;
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
      If _Resources[_Index] Is TERRAMesh Then
      Begin
        MyMesh := TERRAMesh(_Resources[_Index]);
        For J:=0 To Pred(MyMesh.GroupCount) Do
        Begin
          Group := MyMesh.GetGroup(J);
          Self.AddResource(Group.DiffuseMap);
          Self.AddResource(Group.NormalMap);
          Self.AddResource(Group.SpecularMap);
          Self.AddResource(Group.GlowMap);
          Self.AddResource(Group.LightMap);
          Self.AddResource(Group.ToonRamp);
        End;
      End;

    If (_Index<Pred(_ResourceCount)) Then
      Inc(_Index);
  End;
End;


End.
