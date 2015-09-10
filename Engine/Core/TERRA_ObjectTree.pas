Unit TERRA_ObjectTree;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Collections,
  TERRA_Color, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D;

Type
  TERRAObjectNode = Class(TERRAObject)
    Protected
      _Value:TERRAString;

      _Parent:TERRAObjectNode;
      _ChildList:Array Of TERRAObjectNode;
      _ChildCount:Integer;

      Function GetParentCount:Integer;

    Public
      Constructor Create(Const Name:TERRAString = ''; Const Value:TERRAString = '');
      Procedure Release; Override;

      Function GetPath():TERRAString;

      Function SaveToObject(Target:TERRAObject; Owner:TERRAObject = Nil):TERRAObject;

      Procedure LoadFromObject(Source:TERRAObject); Virtual;

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
      Function AddColor(Const Name:TERRAString; Const Value:ColorRGBA):TERRAObjectNode;
      Function AddTime(Const Name:TERRAString; Const Value:TERRATime):TERRAObjectNode;
      Function AddVector2D(Const Name:TERRAString; Const Value:Vector2D):TERRAObjectNode;
      Function AddVector3D(Const Name:TERRAString; Const Value:Vector3D):TERRAObjectNode;
      Function AddVector4D(Const Name:TERRAString; Const Value:Vector4D):TERRAObjectNode;

      Property Parent:TERRAObjectNode Read _Parent;
      Property Value:TERRAString Read _Value Write _Value;
      Property ChildCount:Integer Read _ChildCount;

      Property Children[ID:Integer]:TERRAObjectNode Read GetChildByIndex; Default;
  End;

Implementation
Uses TERRA_Log, TERRA_MemoryStream, TERRA_FileStream, TERRA_Engine, TERRA_XML, TERRA_OS;

(*Procedure DumpObjectTree(Node:TERRAObjectNode; Dest:TERRAStream; Level:Integer);
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
End;*)

Constructor TERRAObjectNode.Create(Const Name, Value:TERRAString);
Begin
  Self._ObjectName := Name;
  Self._Value := Value;
End;

(*Function TERRAObjectNode.GetRoot: TERRAObjectNode;
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
End;*)

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
  Node:TERRAObjectNode;
Begin
  Node := TERRAObjectNode.Create(Name, Value);
  AddChild(Node);
  Result := Node;
End;

Function TERRAObjectNode.AddBoolean(Const Name:TERRAString; Const Value:Boolean):TERRAObjectNode;
Begin
  Result := AddString(Name, BoolToString(Value));
End;

Function TERRAObjectNode.AddInteger(Const Name:TERRAString; Const Value:Integer):TERRAObjectNode;
Begin
  Result := AddString(Name,  IntegerProperty.Stringify(Value));
End;

Function TERRAObjectNode.AddCardinal(Const Name:TERRAString; Const Value:Cardinal):TERRAObjectNode;
Begin
  Result := AddString(Name, CardinalToString(Value));
End;

Function TERRAObjectNode.AddSingle(Const Name:TERRAString; Const Value:Single):TERRAObjectNode;
Begin
  Result := AddString(Name, FloatProperty.Stringify(Value));
End;

Function TERRAObjectNode.AddVector2D(Const Name:TERRAString; Const Value:Vector2D):TERRAObjectNode;
Begin
  Result := Self.AddString(Name,'');
  Result.AddSingle('x', Value.X);
  Result.AddSingle('y', Value.Y);
End;

Function TERRAObjectNode.AddVector3D(Const Name:TERRAString; Const Value:Vector3D):TERRAObjectNode;
Begin
  Result := Self.AddString(Name,'');
  Result.AddSingle('x', Value.X);
  Result.AddSingle('y', Value.Y);
  Result.AddSingle('z', Value.Z);
End;

Function TERRAObjectNode.AddVector4D(Const Name:TERRAString; Const Value:Vector4D):TERRAObjectNode;
Begin
  Result := Self.AddString(Name,'');
  Result.AddSingle('x', Value.X);
  Result.AddSingle('y', Value.Y);
  Result.AddSingle('z', Value.Z);
  Result.AddSingle('W', Value.W);
End;

Function TERRAObjectNode.AddColor(Const Name:TERRAString; Const Value:ColorRGBA):TERRAObjectNode;
Begin
  Result := AddString(Name, ColorProperty.Stringify(Value));
End;

Function TERRAObjectNode.AddTime(Const Name:TERRAString; Const Value:TERRATime):TERRAObjectNode;
Begin
  Result := Self.AddString(Name,'');
  Result.AddInteger('hour', Value.Hour);
  Result.AddInteger('minute', Value.Minute);
  Result.AddInteger('second', Value.Second);
End;

Procedure TERRAObjectNode.LoadFromObject(Source:TERRAObject);
Var
  Index:Integer;
  Node:TERRAObjectNode;
  Prop:TERRAObject;
  Value:TERRAString;
Begin
  If (Self.Parent = Nil) Then
    Self.Name := 'root';

  Index := 0;
  Repeat
    Prop := Source.GetPropertyByIndex(Index);
    If Prop = Nil Then
      Exit;

  (*  If (Prop.Name = 'dimension') Or (Prop.GetObjectType = 'dimension') Then
      DebugBreak;*)

    Value := Prop.GetBlob();

    If (Value <> '') Or (Prop.HasProperties) Then
    Begin
      Node := TERRAObjectNode.Create(Prop.GetObjectType());
      Self.AddChild(Node);

      Node.AddString('name', Prop.Name);
      If (Prop.HasProperties()) Then
      Begin
        Node.LoadFromObject(Prop);
      End Else
      Begin
        Node.Value := Value;
      End;
    End;

    Inc(Index);
  Until False;
End;

Function TERRAObjectNode.SaveToObject(Target:TERRAObject; Owner:TERRAObject): TERRAObject;
Var
  TypeName, PropName:TERRAString;
  CurrentType:TERRAString;
  I:Integer;
  P:TERRAObjectNode;
  Prop:TERRAObject;
Begin
  Result := Nil;

  If (Owner = Nil) Then
  Begin
    For I:=0 To Pred(ChildCount) Do
      _ChildList[I].SaveToObject(Target, Target);

    Exit;
  End;

  If Target = Nil Then
  Begin
     IntegerProperty.Stringify(2);
    Exit;
  End;

  TypeName := Self.Name;
  If TypeName <> '' Then
  Begin
    Self.GetString('name', PropName, '');

    If (PropName = '') Then
      PropName := TypeName;

    Prop := Target.FindProperty(PropName);

    If (Assigned(Prop)) Then
    Begin
      CurrentType := Prop.GetObjectType;
      If (Not StringEquals(CurrentType, TypeName)) Then
      Begin
        Engine.Log.Write(logWarning, 'Object', 'Unseralization mistmatch:' + CurrentType+ ' vs ' +TypeName);
        Prop := Nil;
      End;
    End;

    If (Assigned(Prop)) Then
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

{    If P.Name='texture' Then
       IntegerProperty.Stringify(2);}

      P.SaveToObject(Result, Target);
  End;
End;


End.
