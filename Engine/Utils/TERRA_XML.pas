Unit TERRA_XML;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Stream, TERRA_Utils, TERRA_Vector3D, TERRA_Color;

Const
  xmlSaveHeader   = 1;
  xmlSaveCompact  = 2;

Type
  XMLTagType = (xmlBeginTag,xmlEndTag,xmlData);

  XMLDocument = Class;
  XMLNode = Class;

  XMLNode = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Value:TERRAString;
      _Childs:Array Of XMLNode;
      _NodeCount:Integer;
      _Parent:XMLNode;
      _Document:XMLDocument;

      Procedure Save(Dest:Stream; SaveFlags:Cardinal);
      Function Read(Source:Stream):TERRAString;
      Function GetTagType(Const S:TERRAString):XMLTagType;
      Function GetTagName(Const S:TERRAString):TERRAString;
      Function GetPath:TERRAString;
      Function GetParentCount:Integer;

    Public
      Constructor Create(Const Name:TERRAString = ''; Const Value:TERRAString = '');

      Procedure LoadFromStream(Document:XMLDocument; Source:Stream; InitTag:TERRAString = '');
      Procedure LoadFromObject(Document:XMLDocument; Source:TERRAObject);

      Procedure Release;Reintroduce;

      Function AddTag(Const Name,Value:TERRAString):XMLNode;
      Procedure AddNode(Node:XMLNode);

      Function GetNodeByName(Const Name:TERRAString):XMLNode;
      Function GetNodeByIndex(Index:Integer):XMLNode;

      Function GetNodeByPath(Path:TERRAString; Const PathSeparator:TERRAChar):XMLNode;

      Function GetInteger(Const Name:TERRAString; Var Dest:Integer; Default:Integer = 0):Boolean;
      Function GetCardinal(Const Name:TERRAString; Var Dest:Cardinal; Default:Cardinal = 0):Boolean;
      Function GetByte(Const Name:TERRAString; Var Dest:Byte; Default:Byte):Boolean;
      Function GetBoolean(Const Name:TERRAString; Var Dest:Boolean; Default:Boolean = False):Boolean;
      Function GetString(Const Name:TERRAString; Var Dest:TERRAString; Const Default:TERRAString = ''):Boolean;
      Function GetSingle(Const Name:TERRAString; Var Dest:Single; Default:Single = 0):Boolean;

      Property Name:TERRAString Read _Name Write _Name;
      Property Value:TERRAString Read _Value Write _Value;

      Property NodeCount:Integer Read _NodeCount;
  End;

  XMLDocument = Class(TERRAObject)
    Protected
      _Root:XMLNode;
      _TempBuffer:TERRAString;

      Procedure SetRoot(const Value: XMLNode);

    Public
      Procedure Release;Reintroduce;

      Procedure LoadFromObject(Source:TERRAObject);
      Procedure LoadFromStream(Source:Stream);

      Procedure Save(Dest:Stream; SaveFlags:Cardinal = 0);

      Procedure LoadFromFile(FileName:TERRAString; Encoding:StringEncoding = encodingUnknown);
      Procedure SaveToFile(FileName:TERRAString; SaveFlags:Cardinal = 0);

      Procedure LoadFromString(Data:TERRAString; Encoding:StringEncoding);
      //Procedure SaveToFile(Var Data:TERRAString);Overload;

      Procedure AddNode(Node:XMLNode; Parent:XMLNode=Nil);
      Function GetNodeByName(Const Name:TERRAString):XMLNode;

      Function AddString(Const Name:TERRAString; Value:TERRAString=''; Parent:XMLNode=Nil):XMLNode;
      Function AddBoolean(Const Name:TERRAString; Value:Boolean; Parent:XMLNode=Nil):XMLNode;
      Function AddInteger(Const Name:TERRAString; Value:Integer; Parent:XMLNode=Nil):XMLNode;
      Function AddCardinal(Const Name:TERRAString; Value:Cardinal; Parent:XMLNode=Nil):XMLNode;
      Function AddSingle(Const Name:TERRAString; Value:Single; Parent:XMLNode=Nil):XMLNode;
      Function AddVector(Const Name:TERRAString; Value:Vector3D; Parent:XMLNode=Nil):XMLNode;
      Function AddColor(Const Name:TERRAString; Value:Color; Parent:XMLNode=Nil):XMLNode;
      Function AddTime(Const Name:TERRAString; Value:TERRATime; Parent:XMLNode=Nil):XMLNode;

      Property Root:XMLNode Read _Root Write SetRoot;
  End;

Implementation
Uses TERRA_Error, TERRA_Collections, TERRA_FileManager, TERRA_FileStream, TERRA_MemoryStream, TERRA_Log;

{ XMLNode }
Constructor XMLNode.Create(Const Name, Value:TERRAString);
Begin
  Self._Name:=Name;
  Self._Value:=Value;
End;

Function XMLNode.GetInteger(Const Name:TERRAString; Var Dest:Integer; Default:Integer):Boolean;
Var
  PP:XMLNode;
Begin
  PP := Self.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function XMLNode.GetCardinal(Const Name:TERRAString; Var Dest:Cardinal; Default:Cardinal):Boolean;
Var
  PP:XMLNode;
Begin
  PP := Self.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToCardinal(PP.Value)
  Else
    Dest := Default;
End;

Function XMLNode.GetSingle(Const Name:TERRAString; Var Dest:Single; Default:Single):Boolean;
Var
  PP:XMLNode;
Begin
  PP := Self.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function XMLNode.GetBoolean(Const Name:TERRAString; Var Dest:Boolean; Default:Boolean):Boolean;
Var
  PP:XMLNode;
Begin
  PP := Self.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := (PP.Value = '1') Or (StringLower(PP.Value) = 'true')
  Else
    Dest := Default;
End;

Function XMLNode.GetByte(Const Name:TERRAString; Var Dest:Byte; Default:Byte):Boolean;
Var
  PP:XMLNode;
Begin
  PP := Self.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function XMLNode.GetString(Const Name:TERRAString; Var Dest:TERRAString; Const Default:TERRAString):Boolean;
Var
  PP:XMLNode;
Begin
  PP := Self.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := PP.Value
  Else
    Dest := Default;
End;

Procedure XMLNode.LoadFromObject(Document:XMLDocument; Source:TERRAObject);
Var
  Index:Integer;
  Node:XMLNode;
  Prop:TERRAObject;
Begin
  Self._Name := Source.ObjectName;
  Self._Value := Source.GetBlob();

  Index := 0;
  Repeat
    Prop := Source.GetPropertyByIndex(Index);
    If Prop = Nil Then
      Exit;

    Node := XMLNode.Create();
    Self.AddNode(Node);
    Node.LoadFromObject(Document, Prop);

    Inc(Index);
  Until False;
End;

Procedure XMLNode.LoadFromStream(Document:XMLDocument; Source:Stream; InitTag:TERRAString);
Var
  S, S2:TERRAString;
  Tag, Value:TERRAString;
  It:StringIterator;
  J:Integer;
  C:TERRAChar;
  Inside, Found:Boolean;
  ShortTag:Boolean;
  Node:XMLNode;
  Temp:TERRAString;
Begin
  Self._Document := Document;

  _Value := '';
  If (Source = Nil) Then
    Exit;

  If (InitTag='') Then
  Begin
    Repeat
      If (Source.EOF) And (Self._Document._TempBuffer='') Then
        Exit;

      S := Read(Source);
      C := StringGetChar(S, 2);
    Until (C <> Ord('?')) And (C <> Ord('!'));
  End Else
    S := InitTag;

  If GetTagType(S)<>xmlBeginTag Then
  Begin
    Log(logError, 'XML', 'Invalid XML sintax!');
    Exit;
  End;

  ShortTag := (StringGetChar(S, -2) = Ord('/'));
  S := GetTagName(S);

  If (StringCharPosIterator(Ord(' '), S, It)) Then
  Begin
    It.Split(S, S2);
    Self._Name := S;

    S := S2;
    If (StringLastChar(S) = Ord('/')) Then
      StringDropChars(S, -1);

    While (S<>'') Do
    Begin
      StringCreateIterator(S, It);
      Inside := False;
      Found := False;

      While It.HasNext() Do
      Begin
        C := It.GetNext();
        If (C= Ord('"')) Then
          Inside := Not Inside
        Else
        If (C = Ord(' ')) And (Not Inside) Then
        Begin
          It.Split(S2, S);
          Found := True;
          Break;
        End;
      End;

      If Not Found Then
      Begin
        S2 := S;
        S := '';
      End;

      If StringCharPosIterator(Ord('='), S2, It) Then
      Begin
        It.Split(Tag, Value);

        Value := StringTrim(Value);
        Temp := VAlue;
        Value := StringCopy(Value, 2, -2);

        Node := XMLNode.Create(Tag, Value);
        AddNode(Node);
      End Else
        Node := Nil;
    End;
  End Else
    Self._Name := S;

  If (ShortTag) Then
    Exit;

  Repeat
    S := Read(Source);
    Case GetTagType(S) Of
      xmlBeginTag:  Begin
                      Node := XMLNode.Create();
                      Node.LoadFromStream(_Document, Source, S);

                      AddNode(Node);
                    End;

      xmlEndTag:    Break;

      xmlData:
        Begin
          If (StringLastChar(S) = Ord('"')) Then
            StringDropChars(S, -1);

          StringRemoveSpecialHTMLChars(S);
          _Value := S;
        End;
    End;
  Until (Source.EOF) And (Self._Document._TempBuffer='');
End;

Procedure XMLNode.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_NodeCount) Do
    ReleaseObject(_Childs[I]);
  SetLength(_Childs,0);
  _NodeCount:=0;
End;

Function XMLNode.GetTagType(Const S:TERRAString):XMLTagType;
Begin
  If (StringGetChar(S, 1) = Ord('<')) Then
  Begin
    If (StringGetChar(S, 2) = Ord('/')) Then
      Result := xmlEndTag
    Else
      Result := xmlBeginTag;
  End Else
    Result := xmlData;
End;

Function XMLNode.GetTagName(Const S:TERRAString):TERRAString;
Begin
  Result := StringCopy(S, 2, -2);
End;

Function XMLNode.GetParentCount:Integer;
Var
  Node:XMLNode;
Begin
  Node:=Self;
  Result:=-1;
  While Assigned(Node) Do
  Begin
    Inc(Result);
    Node:=Node._Parent;
  End;
End;

Function XMLNode.GetPath:TERRAString;
Var
  Node:XMLNode;
Begin
  Node:=Self;
  Result:='';
  While Assigned(Node) Do
  Begin
    If Result<>'' Then
      Result:='.'+Result;
    Result:=Node._Name+Result;
    Node:=Node._Parent;
  End;
End;

Function XMLNode.GetNodeByPath(Path:TERRAString; Const PathSeparator:TERRAChar): XMLNode;
Var
  First:TERRAString;
Begin
  First := StringGetNextSplit(Path, PathSeparator);

  If First = '' Then
    Result := Nil
  Else
  Begin
    Result := Self.GetNodeByName(First);
    If Not Assigned(Result) Then
      Exit;

    If Path <> '' Then
      Result := Result.GetNodeByPath(Path, PathSeparator);
  End;
End;

Function XMLNode.GetNodeByIndex(Index:Integer):XMLNode;
Begin
  If (Index<0) Or (Index>=_NodeCount) Then
    Result := Nil
  Else
    Result := _Childs[Index];
End;

Function XMLNode.GetNodeByName(Const Name:TERRAString):XMLNode;
Var
  I:Integer;
Begin
  Result:=Nil;
  For I:=0 To Pred(_NodeCount) Do
    If StringEquals(_Childs[I].Name, Name) Then
    Begin
      Result:=_Childs[I];
      Exit;
    End;
End;

Function XMLNode.Read(Source:Stream):TERRAString;
{Const
  BufferSize = 1024;}
Var
  S:TERRAString;
  I,J:Integer;
Begin
  If (_Document._TempBuffer<>'') Then
  Begin
    I := StringCharPos(Ord('<'), _Document._TempBuffer);
    J := StringCharPos(Ord('>'), _Document._TempBuffer);

    If (I=1) Then
      I := 0;

    If (J>0) And (((I>0) And (J<I)) Or (I<=0))  Then
    Begin
      I := J+1;
    End;

    If (I>1) Then
    Begin
      Result := StringCopy(_Document._TempBuffer, 1, Pred(I));
      _Document._TempBuffer := StringCopy(_Document._TempBuffer, I, MaxInt);
      _Document._TempBuffer := StringTrimLeft(_Document._TempBuffer);
    End Else
    Begin
      Result := _Document._TempBuffer;
      _Document._TempBuffer := '';
    End;


    Result := StringTrim(Result);

    If (Result='') And (Not Source.EOF) Then
    Begin
      Result := Read(Source);
      Exit;
    End;

    Exit;
  End;

  S := '';
  Source.ReadLine(S);
  S := StringTrimLeft(S);
  StringReplaceText('/ >', '/>', S);

  If (S='') And (Not Source.EOF) Then
  Begin
    Result := Read(Source);
    Exit;
  End;

  I := StringPos('</', S);
  J := StringPos('>', S);
  If (I>0) And ((I<J) Or (J<1)) Then
  Begin
    _Document._TempBuffer := StringCopy(S, I, MaxInt);
    S := StringCopy(S, 1, Pred(I));

    _Document._TempBuffer := StringTrimLeft(_Document._TempBuffer);
  End Else
  If (J>0) Then
  Begin
    _Document._TempBuffer := StringCopy(S, Succ(J), MaxInt);
    S := StringCopy(S, 1, J);

    _Document._TempBuffer := StringTrimLeft(_Document._TempBuffer);
  End;

  Result := StringTrim(S);

  If (Result='') And (Not Source.EOF) Then
  Begin
    Result := Read(Source);
    Exit;
  End;
End;

Procedure XMLNode.Save(Dest:Stream; SaveFlags:Cardinal);
Var
  Tabs, S:TERRAString;
  I, Count:Integer;
Begin
  Tabs:='';
  For I:=1 To GetParentCount Do
    Tabs:=Tabs+#9;

  S := '';

  If Value<>'' Then
  Begin
    For I:=0 To Pred(_NodeCount) Do
    Begin
      S := S + ' '+ _Childs[I].Name + '="'+_Childs[I]._Value+'"';
    End;

    Dest.WriteLine(Tabs+'<'+Name+S+'>'+Value+'</'+Name+'>');
  End Else
  Begin
    Count := 0;

    If ((SaveFlags And xmlSaveCompact)<>0) Then
    Begin
      For I:=0 To Pred(_NodeCount) Do
      If (_Childs[I]._NodeCount<=0) Then
      Begin
        S := S + ' '+ _Childs[I].Name + '="'+_Childs[I]._Value+'"';
        Inc(Count);
      End;

      If Count>=_NodeCount Then
        S := S + '/';
    End Else
      S := '';

    Dest.WriteLine(Tabs+'<'+Name+S+'>');

    If Count>=_NodeCount Then
      Exit;

    For I:=0 To Pred(_NodeCount) Do
    If ((SaveFlags And xmlSaveCompact)=0) Or (_Childs[I]._NodeCount>0) Then
      _Childs[I].Save(Dest, SaveFlags);

    Dest.WriteLine(Tabs+'</'+Name+'>');
  End;
End;

Procedure XMLNode.AddNode(Node:XMLNode);
Begin
  Node._Parent:=Self;
  Inc(_NodeCount);
  SetLength(_Childs,_NodeCount);
  _Childs[Pred(_NodeCount)]:=Node;
End;

Function XMLNode.AddTag(Const Name, Value:TERRAString): XMLNode;
Var
  Node:XMLNode;
Begin
  Node := XMLNode.Create(Name,Value);
  AddNode(Node);
  Result:=Node;
End;

// LXMLDocument
Procedure XMLDocument.Release;
Begin
  ReleaseObject(_Root);
End;

Function XMLDocument.GetNodeByName(Const Name:TERRAString):XMLNode;
Begin
  Result := Nil;
  If Assigned(_Root) Then
    Result:=_Root.GetNodeByName(Name);
End;

Procedure XMLDocument.AddNode(Node:XMLNode; Parent:XMLNode=Nil);
Begin
  If Assigned(Parent) Then
    Parent.AddNode(Node)
  Else
  Begin
    If Not Assigned(_Root) Then
      _Root:=Node
    Else
      _Root.AddNode(Node);
  End;
End;

Function XMLDocument.AddString(Const Name:TERRAString; Value:TERRAString=''; Parent:XMLNode=Nil):XMLNode;
Begin
  If Assigned(Parent) Then
    Result:=Parent.AddTag(Name,Value)
  Else
  Begin
    If Not Assigned(_Root) Then
    Begin
      _Root := XMLNode.Create(Name,Value);
      Result := _Root;
    End Else
      Result := _Root.AddTag(Name,Value);
  End;
End;

Function XMLDocument.AddBoolean(Const Name:TERRAString; Value:Boolean; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, BoolToString(Value), Parent);
End;

Function XMLDocument.AddInteger(Const Name:TERRAString; Value:Integer; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, IntToString(Value), Parent);
End;

Function XMLDocument.AddCardinal(Const Name:TERRAString; Value:Cardinal; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, CardinalToString(Value), Parent);
End;

Function XMLDocument.AddSingle(Const Name:TERRAString; Value:Single; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, FloatToString(Value), Parent);
End;

Procedure DumpXML(Node:XMLNode; Dest:Stream; Level:Integer);
Var
  I:Integer;
  S:TERRAString;
Begin
  S := '';
  For I:=1 To Level Do
    S := S + '  ';

  Dest.WriteLine(S+Node.Name+':'+Node.Value);
  For I:=0 To Pred(Node.NodeCount) Do
    DumpXML(Node.GetNodeByIndex(I), Dest, Succ(Level));
End;

Procedure XMLDocument.LoadFromObject(Source:TERRAObject);
Begin
  ReleaseObject(_Root);
  _Root := XMLNode.Create();
  _Root.LoadFromObject(Self, Source);
End;

Procedure XMLDocument.LoadFromStream(Source:Stream);
Begin
  ReleaseObject(_Root);
  _Root := XMLNode.Create();
  _Root.LoadFromStream(Self, Source);

  {$IFDEF PC}
 (* Dest := FileStream.Create('debug\'+GetFileName(Source.Name,False));
  DumpXML(_Root, Dest, 0);
  ReleaseObject(Dest);*)
  {$ENDIF}
End;

Procedure XMLDocument.Save(Dest:Stream; SaveFlags:Cardinal);
Begin
  If ((SaveFlags And xmlSaveHeader)<>0) Then
    Dest.WriteLine('<?xml version="1.0" encoding="UTF-8"?>');

  _Root.Save(Dest, SaveFlags);
End;

Procedure XMLDocument.LoadFromFile(FileName:TERRAString; Encoding:StringEncoding = encodingUnknown);
Var
  Source:FileStream;
Begin
  Source := FileStream.Open(FileName);

  If Encoding <> encodingUnknown Then
    Source.Encoding := Encoding;

  LoadFromStream(Source);
  ReleaseObject(Source);
End;

Procedure XMLDocument.SaveToFile(FileName:TERRAString; SaveFlags:Cardinal);
Var
  Dest:FileStream;
Begin
  Dest := FileStream.Create(FileName);
  Save(Dest, SaveFlags);
  ReleaseObject(Dest);
End;

Procedure XMLDocument.LoadFromString(Data:TERRAString; Encoding:StringEncoding);
Var
  Source:MemoryStream;
Begin
  Source := MemoryStream.Create(Length(Data), @Data[1]);
  Source.Encoding := Encoding;
  LoadFromStream(Source);
  ReleaseObject(Source);
End;

Function XMLDocument.AddVector(Const Name:TERRAString; Value:Vector3D; Parent:XMLNode=Nil):XMLNode;
Var
  Node:XMLNode;
Begin
  Node := XMLNode.Create(Name,'');
  AddSingle('x', Value.X, Node);
  AddSingle('y', Value.Y, Node);
  AddSingle('z', Value.Z, Node);
  AddNode(Node, Parent);
  Result:=Node;
End;

Function XMLDocument.AddColor(Const Name:TERRAString; Value:Color; Parent:XMLNode=Nil):XMLNode;
Var
  Node:XMLNode;
Begin
  Node := XMLNode.Create(Name,'');
  AddInteger('r', Value.R, Node);
  AddInteger('g', Value.G, Node);
  AddInteger('b', Value.B, Node);
  AddInteger('a', Value.A, Node);
  AddNode(Node, Parent);
  Result:=Node;
End;

Function XMLDocument.AddTime(Const Name:TERRAString; Value:TERRATime; Parent:XMLNode=Nil):XMLNode;
Var
  Node:XMLNode;
Begin
  Node := XMLNode.Create(Name,'');
  AddInteger('hour', Value.Hour, Node);
  AddInteger('minute', Value.Minute, Node);
  AddInteger('second', Value.Second, Node);
  AddNode(Node, Parent);
  Result:=Node;
End;

Procedure XMLDocument.SetRoot(const Value: XMLNode);
Begin
  If Value = _Root Then
    Exit;

  ReleaseObject(_Root);
  _Root := Value;
End;


End.

