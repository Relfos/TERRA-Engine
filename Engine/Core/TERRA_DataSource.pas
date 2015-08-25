Unit TERRA_DataSource;

{%I terra.inc}

Interface
Uses TERRA_Object, TERRA_ObjectTree, TERRA_String, TERRA_Collections, TERRA_List, TERRA_FileUtils;

Type
  DataSourceProperty = Class(TERRAObject)
    Protected
      _Value:TERRAString;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:TERRAString);

      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:TERRAString Read _Value Write SetBlob;
  End;

  DataSourceManager = Class(TERRAObject)
    Protected
      _Sources:Array Of TERRAObject;
      _SourceCount:Integer;

      Function TryPath(Path:TERRAString; Obj:TERRAObject):TERRAString;

    Public
      Class Function Instance:DataSourceManager;

      Procedure AddObject(Obj:TERRAObject);
      Procedure AddFromSession(FileName:TERRAString);

      Procedure Clear;

      Function GetObject(Index:Integer):TERRAObject;
      Function GetValueFromPath(Path:TERRAString):TERRAString;

      Property SourceCount:Integer Read _SourceCount;
  End;

Implementation
Uses TERRA_Session;

Var
  _DataSourceManagerInstance:DataSourceManager;

{ DataSourceProperty }
Constructor DataSourceProperty.Create(const Name, InitValue: TERRAString);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
End;

Function DataSourceProperty.GetBlob: TERRAString;
Begin
  Result := _Value;
End;

Procedure DataSourceProperty.SetBlob(const Blob: TERRAString);
Begin
  _Value := Blob;
End;

Function DataSourceProperty.GetObjectType: TERRAString;
Begin
  Result := 'datasource';
End;

{ DataSourceManager }
Class function DataSourceManager.Instance: DataSourceManager;
Begin
  If _DataSourceManagerInstance = Nil Then
    _DataSourceManagerInstance := DataSourceManager.Create();

  Result := _DataSourceManagerInstance;
End;

Procedure DataSourceManager.AddObject(Obj: TERRAObject);
Begin
  Inc(_SourceCount);
  SetLength(_Sources, _SourceCount);
  _Sources[Pred(_SourceCount)] := Obj;
End;

Procedure DataSourceManager.Clear;
Begin
  _SourceCount := 0;
End;

Function DataSourceManager.GetObject(Index: Integer): TERRAObject;
Begin
  If (Index<0) Or (Index>=_SourceCount) Then
    Result := Nil
  Else
    Result := _Sources[Index];
End;

Procedure DataSourceManager.AddFromSession(FileName: TERRAString);
Var
  SaveData:Session;
  It:Iterator;
  Item, Row:StringProperty;
  Obj:List;
  S, S2, Tag:TERRAString;
Begin
  SaveData := TERRA_Session.Session.Create(GetFileName(FileName, False));
  SaveData.Path := GetFilePath(FileName);
  If SaveData.Data.Count<=0 Then
    Exit;

  It := SaveData.Data.GetIterator();
  While It.HasNext() Do
  Begin
    Row := StringProperty(It.Value);

    Obj := List.Create();
    Obj.Name := Row.Name;
    S := Row.Value;

    While S<>'' Do
    Begin
      S2 := StringGetNextSplit(S, Ord('|'));
      Tag := StringGetNextSplit(S2, Ord('='));

      Item := StringProperty.Create(Tag, S2);
      Obj.Add(Item);
    End;

    Self.AddObject(Obj);
  End;
  ReleaseObject(It);
  ReleaseObject(SaveData);
End;

Function DataSourceManager.TryPath(Path:TERRAString; Obj:TERRAObject):TERRAString;
Var
  I:Integer;
  Key:TERRAObject;
  BaseName:TERRAString;
Begin
  BaseName := StringGetNextSplit(Path, Ord('.'));

  If (Obj = Nil) Or (Not StringEquals(Obj.Name, BaseName)) Then
    Result := ''
  Else
  If (Path = '') Then
  Begin
    Result := Obj.GetBlob();
  End Else
  Begin
    I := 0;
    Repeat
      Key := Obj.GetPropertyByIndex(I);

      Result := TryPath(Path, Key);
      If Result<>'' Then
        Exit;

      Inc(I);
    Until False;
  End;
End;

Function DataSourceManager.GetValueFromPath( Path: TERRAString): TERRAString;
Var
  I:Integer;
Begin
  Result := '';

  If Path = '' Then
    Exit;

  For I:=0 To Pred(_SourceCount) Do
  Begin
    Result := TryPath(Path, _Sources[I]);
    If (Result<>'') Then
      Exit;
  End;
End;

End.
