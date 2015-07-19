Unit TERRA_ObjectTree;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections, TERRA_Stream,
  TERRA_Color, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D;

Type
  TERRAObjectNode = Class(TERRAObject)
    Protected
      _Value:TERRAString;

      _Root:TERRAObjectNode;
      _Parent:TERRAObjectNode;
      _ChildList:Array Of TERRAObjectNode;
      _ChildCount:Integer;

      Function GetParentCount:Integer;
      Function GetRoot: TERRAObjectNode;

    Public
      Constructor Create(Const Name:TERRAString = ''; Const Value:TERRAString = '');
      Procedure Release; Override;

      Function GetPath():TERRAString;

      Function SaveToObject(Target:TERRAObject):TERRAObject; Virtual;
      Procedure SaveToStream(Dest:Stream; SaveFlags:Cardinal); Virtual; Abstract;
      Procedure SaveToFile(FileName:TERRAString; SaveFlags:Cardinal = 0);
      Function SaveToString(Encoding:StringEncoding; SaveFlags:Cardinal = 0):TERRAString;

      Procedure LoadFromObject(Source:TERRAObject); Virtual;
      Procedure LoadFromStream(Source:Stream); Virtual; Abstract;
      Procedure LoadFromString(Const Data:TERRAString; Encoding:StringEncoding);
      Procedure LoadFromFile(Const FileName:TERRAString; Encoding:StringEncoding = encodingUnknown);


      Function GetInteger(Const Name:TERRAString; Var Dest:Integer; Default:Integer = 0):Boolean;
      Function GetCardinal(Const Name:TERRAString; Var Dest:Cardinal; Default:Cardinal = 0):Boolean;
      Function GetByte(Const Name:TERRAString; Var Dest:Byte; Default:Byte):Boolean;
      Function GetBoolean(Const Name:TERRAString; Var Dest:Boolean; Default:Boolean = False):Boolean;
      Function GetString(Const Name:TERRAString; Var Dest:TERRAString; Const Default:TERRAString = ''):Boolean;
      Function GetSingle(Const Name:TERRAString; Var Dest:Single; Default:Single = 0):Boolean;

      Function GetChildByName(Const Name:TERRAString):TERRAObjectNode;
      Function GetChildByIndex(Index:Integer):TERRAObjectNode;
      Function GetChildByPath(Path:TERRAString; Const PathSeparator:TERRAChar):TERRAObjectNode;

      Procedure AddChild(Child:TERRAObjectNode);
      Function AddString(Const Name:TERRAString; Const Value:TERRAString):TERRAObjectNode;
      Function AddBoolean(Const Name:TERRAString; Const Value:Boolean):TERRAObjectNode;
      Function AddInteger(Const Name:TERRAString; Const Value:Integer):TERRAObjectNode;
      Function AddCardinal(Const Name:TERRAString; Const Value:Cardinal):TERRAObjectNode;
      Function AddSingle(Const Name:TERRAString; Const Value:Single):TERRAObjectNode;
      Function AddColor(Const Name:TERRAString; Const Value:Color):TERRAObjectNode;
      Function AddTime(Const Name:TERRAString; Const Value:TERRATime):TERRAObjectNode;
      Function AddVector2D(Const Name:TERRAString; Const Value:Vector2D):TERRAObjectNode;
      Function AddVector3D(Const Name:TERRAString; Const Value:Vector3D):TERRAObjectNode;
      Function AddVector4D(Const Name:TERRAString; Const Value:Vector4D):TERRAObjectNode;

      Property Root:TERRAObjectNode Read GetRoot;
      Property Parent:TERRAObjectNode Read _Parent;
      Property Value:TERRAString Read _Value Write _Value;
      Property ChildCount:Integer Read _ChildCount;
  End;

  TERRAObjectNodeClass = Class Of TERRAObjectNode;

Implementation
Uses TERRA_MemoryStream, TERRA_FileStream, TERRA_XML, TERRA_OS;

Procedure DumpObjectTree(Node:TERRAObjectNode; Dest:Stream; Level:Integer);
Var
  I:Integer;
  S:TERRAString;
Begin
  S := '';
  For I:=1 To Level Do
    S := S + '  ';

  Dest.WriteLine(S+Node.Name +':'+ Node.Value);
  For I:=0 To Pred(Node.ChildCount) Do
    DumpObjectTree(Node.GetChildByIndex(I), Dest, Succ(Level));
End;


Constructor TERRAObjectNode.Create(Const Name, Value:TERRAString);
Begin
  Self._ObjectName := Name;
  Self._Value := Value;
End;


Function TERRAObjectNode.GetRoot: TERRAObjectNode;
Begin
  If Assigned(_Root) Then
  Begin
    Result := _Root;
    Exit;
  End;

  If Assigned(_Parent) Then
    Result := _Parent.Root
  Else
    Result := Self;
End;

Function TERRAObjectNode.GetParentCount:Integer;
Var
  Node:TERRAObjectNode;
Begin
  Node := Self;
  Result := -1;
  While Assigned(Node) Do
  Begin
    Inc(Result);
    Node := Node._Parent;
  End;
End;

Function TERRAObjectNode.GetInteger(Const Name:TERRAString; Var Dest:Integer; Default:Integer):Boolean;
Var
  PP:TERRAObjectNode;
Begin
  PP := Self.GetChildByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function TERRAObjectNode.GetCardinal(Const Name:TERRAString; Var Dest:Cardinal; Default:Cardinal):Boolean;
Var
  PP:TERRAObjectNode;
Begin
  PP := Self.GetChildByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToCardinal(PP.Value)
  Else
    Dest := Default;
End;

Function TERRAObjectNode.GetSingle(Const Name:TERRAString; Var Dest:Single; Default:Single):Boolean;
Var
  PP:TERRAObjectNode;
Begin
  PP := Self.GetChildByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function TERRAObjectNode.GetBoolean(Const Name:TERRAString; Var Dest:Boolean; Default:Boolean):Boolean;
Var
  PP:TERRAObjectNode;
Begin
  PP := Self.GetChildByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := (PP.Value = '1') Or (StringLower(PP.Value) = 'true')
  Else
    Dest := Default;
End;

Function TERRAObjectNode.GetByte(Const Name:TERRAString; Var Dest:Byte; Default:Byte):Boolean;
Var
  PP:TERRAObjectNode;
Begin
  PP := Self.GetChildByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function TERRAObjectNode.GetString(Const Name:TERRAString; Var Dest:TERRAString; Const Default:TERRAString):Boolean;
Var
  PP:TERRAObjectNode;
Begin
  PP := Self.GetChildByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := PP.Value
  Else
    Dest := Default;
End;

Procedure TERRAObjectNode.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildCount) Do
    _ChildList[I].Release();

  SetLength(_ChildList,0);
  _ChildCount := 0;
End;


Function TERRAObjectNode.GetPath:TERRAString;
Var
  Node:TERRAObjectNode;
Begin
  Node := Self;
  Result := '';
  While Assigned(Node) Do
  Begin
    If Result<>'' Then
      Result := '.' + Result;
    Result := Node.Name + Result;
    Node := Node._Parent;
  End;
End;

Function TERRAObjectNode.GetChildByPath(Path:TERRAString; Const PathSeparator:TERRAChar): TERRAObjectNode;
Var
  First:TERRAString;
Begin
  First := StringGetNextSplit(Path, PathSeparator);

  If First = '' Then
    Result := Nil
  Else
  Begin
    Result := Self.GetChildByName(First);
    If Not Assigned(Result) Then
      Exit;

    If Path <> '' Then
      Result := Result.GetChildByPath(Path, PathSeparator);
  End;
End;

Function TERRAObjectNode.GetChildByIndex(Index:Integer):TERRAObjectNode;
Begin
  If (Index<0) Or (Index>=_ChildCount) Then
    Result := Nil
  Else
    Result := _ChildList[Index];
End;

Function TERRAObjectNode.GetChildByName(Const Name:TERRAString):TERRAObjectNode;
Var
  I:Integer;
Begin
  Result:=Nil;
  For I:=0 To Pred(_ChildCount) Do
    If StringEquals(_ChildList[I].Name, Name) Then
    Begin
      Result := _ChildList[I];
      Exit;
    End;
End;

Procedure TERRAObjectNode.AddChild(Child: TERRAObjectNode);
Begin
  If Child = Nil Then
    Exit;

  Child._Parent := Self;
  Inc(_ChildCount);
  SetLength(_ChildList, _ChildCount);
  _ChildList[Pred(_ChildCount)] := Child;
End;

Function TERRAObjectNode.AddString(Const Name:TERRAString; Const Value:TERRAString):TERRAObjectNode;
Var
  TargetClass:TERRAObjectNodeClass;
  Node:TERRAObjectNode;
Begin
  TargetClass := TERRAObjectNodeClass(Self.ClassType);
  Node := TargetClass.Create(Name, Value);
  AddChild(Node);
  Result := Node;
End;

Function TERRAObjectNode.AddBoolean(Const Name:TERRAString; Const Value:Boolean):TERRAObjectNode;
Begin
  Result := AddString(Name, BoolToString(Value));
End;

Function TERRAObjectNode.AddInteger(Const Name:TERRAString; Const Value:Integer):TERRAObjectNode;
Begin
  Result := AddString(Name, IntToString(Value));
End;

Function TERRAObjectNode.AddCardinal(Const Name:TERRAString; Const Value:Cardinal):TERRAObjectNode;
Begin
  Result := AddString(Name, CardinalToString(Value));
End;

Function TERRAObjectNode.AddSingle(Const Name:TERRAString; Const Value:Single):TERRAObjectNode;
Begin
  Result := AddString(Name, FloatToString(Value));
End;

Function TERRAObjectNode.AddVector2D(Const Name:TERRAString; Const Value:Vector2D):TERRAObjectNode;
Var
  Node:TERRAObjectNode;
Begin
  Node := TERRAObjectNode.Create(Name,'');
  Node.AddSingle('x', Value.X);
  Node.AddSingle('y', Value.Y);
  AddChild(Node);
  Result := Node;
End;

Function TERRAObjectNode.AddVector3D(Const Name:TERRAString; Const Value:Vector3D):TERRAObjectNode;
Var
  Node:TERRAObjectNode;
Begin
  Node := TERRAObjectNode.Create(Name,'');
  Node.AddSingle('x', Value.X);
  Node.AddSingle('y', Value.Y);
  Node.AddSingle('z', Value.Z);
  AddChild(Node);
  Result := Node;
End;

Function TERRAObjectNode.AddVector4D(Const Name:TERRAString; Const Value:Vector4D):TERRAObjectNode;
Var
  Node:TERRAObjectNode;
Begin
  Node := TERRAObjectNode.Create(Name,'');
  Node.AddSingle('x', Value.X);
  Node.AddSingle('y', Value.Y);
  Node.AddSingle('z', Value.Z);
  Node.AddSingle('w', Value.W);
  AddChild(Node);
  Result := Node;
End;

Function TERRAObjectNode.AddColor(Const Name:TERRAString; Const Value:Color):TERRAObjectNode;
Begin
  Result := AddString(Name, ColorToString(Value));
End;

Function TERRAObjectNode.AddTime(Const Name:TERRAString; Const Value:TERRATime):TERRAObjectNode;
Var
  Node:TERRAObjectNode;
Begin
  Node := TERRAObjectNode.Create(Name,'');
  Node.AddInteger('hour', Value.Hour);
  Node.AddInteger('minute', Value.Minute);
  Node.AddInteger('second', Value.Second);
  AddChild(Node);
  Result:=Node;
End;

Procedure TERRAObjectNode.LoadFromObject(Source:TERRAObject);
Var
  Index:Integer;
  Node:TERRAObjectNode;
  TargetClass:TERRAObjectNodeClass;
  Prop:TERRAObject;
Begin
  TargetClass := TERRAObjectNodeClass(Self.ClassType);

  If (Self.Parent = Nil) Then
    Self.Name := 'root';

  Index := 0;
  Repeat
    Prop := Source.GetPropertyByIndex(Index);
    If Prop = Nil Then
      Exit;

    If Prop.IsValueObject() Then
    Begin
      Self.AddString(Prop.Name, Prop.GetBlob());
    End Else
    Begin
      Node := TargetClass.Create(Prop.GetObjectType());
      Self.AddChild(Node);

      Node.AddString('name', Prop.Name);

      Node.LoadFromObject(Prop);
    End;

    Inc(Index);
  Until False;
End;

Function TERRAObjectNode.SaveToObject(Target: TERRAObject): TERRAObject;
Var
  TypeName, PropName:TERRAString;
  I:Integer;
  P:TERRAObjectNode;
  Prop:TERRAObject;
Begin
  Result := Nil;

  If Target = Nil Then
  Begin
    IntToString(2);
    Exit;
  End;

  TypeName := Self.Name;
  If TypeName <> '' Then
  Begin
    Self.GetString('name', PropName, '');

    If (PropName = '') Then
      PropName := TypeName;

    Prop := Target.FindProperty(PropName);
    If Assigned(Prop) Then
      Result := Prop
    Else
      Result := Target.CreateProperty(PropName, TypeName);

    If (Assigned(Result)) And (Self.Value<>'') Then
    Begin
      Result.SetBlob(Self.Value);
      Exit;
    End;
  End;

  If Result = Nil Then
    Result := Target;

  For I:=0 To Pred(ChildCount) Do
  Begin
    P := Self.GetChildByIndex(I);

    If (StringEquals(P.Name, 'name')) Then
      Continue;

    If P.Name='position' Then
      IntToString(2);

      P.SaveToObject(Result);
  End;
End;


Procedure TERRAObjectNode.SaveToFile(FileName: TERRAString; SaveFlags:Cardinal);
Var
  Dest:FileStream;
Begin
  Dest := FileStream.Create(FileName);
  SaveToStream(Dest, SaveFlags);
  ReleaseObject(Dest);
End;

Function TERRAObjectNode.SaveToString(Encoding:StringEncoding; SaveFlags:Cardinal):TERRAString;
Var
  Dest:MemoryStream;
Begin
  Dest := MemoryStream.Create();
  SaveToStream(Dest, SaveFlags);
  SetLength(Result, Dest.Size);
  Move(Dest.Buffer^, Result[1], Dest.Size);
  ReleaseObject(Dest);
End;

Procedure TERRAObjectNode.LoadFromFile(const FileName:TERRAString; Encoding: StringEncoding);
Var
  Source:FileStream;
Begin
  Source := FileStream.Open(FileName);

  If Encoding <> encodingUnknown Then
    Source.Encoding := Encoding;

  LoadFromStream(Source);
  Source.Release;
End;

Procedure TERRAObjectNode.LoadFromString(Const Data:TERRAString; Encoding:StringEncoding);
Var
  Source:MemoryStream;
Begin
  Source := MemoryStream.Create(Length(Data), @Data[1]);
  Source.Encoding := Encoding;
  LoadFromStream(Source);
  ReleaseObject(Source);
End;

End.
