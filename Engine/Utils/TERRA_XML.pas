Unit TERRA_XML;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Stream, TERRA_Utils, TERRA_Vector3D, TERRA_Color;

Const
  xmlSaveHeader   = 1;
  xmlSaveCompact  = 2;

Type
  XMLTagType=(xmlBeginTag,xmlEndTag,xmlData);
  XMLStatus=(xmlWriting,xmlReading);
  XMLType=(xmlString, xmlBoolean, xmlInteger, xmlCardinal,
            xmlByte, xmlWord, xmlSingle,
            xmlVector, xmlColor);

  XMLDocument=Class;
  XMLNode=Class;

  XMLDescriptor=Object
    Name:TERRAString;
    Address:Pointer;
    ElementCount:Integer;
    XMLType:XMLType;
    Default:TERRAString;
    Found:Boolean;

    Procedure Read(Node:XMLNode);
    Procedure Write(Document:XMLDocument; Node:XMLNode);
  End;

  XMLElement = Class(TERRAObject)
    Protected
      _Descriptors:Array Of XMLDescriptor;
      _DescriptorCount:Integer;
      _Status:XMLStatus;

      Procedure XMLRegisterElement(Const Name:TERRAString; Address:Pointer; XMLType:XMLType; Default:TERRAString='');
      Procedure XMLRegisterArrayElement(Const Name:TERRAString; Address:Pointer; XMLType:XMLType; Size:Integer);

      Procedure XMLLoadElements(Source:XMLNode);Overload;
      Procedure XMLSaveElements(Document:XMLDocument; Parent:XMLNode=Nil);

    Public
      Procedure XMLRegisterStructure; Virtual;
      Procedure XMLClearStructure;
      Procedure XMLSynchronize; Virtual;

      Procedure XMLLoad(Node:XMLNode);Overload;

      Procedure XMLLoad(Document:XMLDocument);Overload;
      Procedure XMLSave(Document:XMLDocument);Overload;

      Procedure XMLLoad(Source:Stream);Overload;
      Procedure XMLSave(Dest:Stream);Overload;

      Procedure XMLLoad(Const FileName:TERRAString);Overload;
      Procedure XMLSave(Const FileName:TERRAString);Overload;

      Function XMLGetPropertyCount:Integer;
      Function XMLGetProperty(Index:Integer):XMLDescriptor; Overload;
      Function XMLGetProperty(Name:TERRAString):XMLDescriptor; Overload;

      Function XMLNewElement(Const Name:TERRAString):XMLElement;Virtual;
      Function XMLGetElement(Index:Integer):XMLElement;Virtual;
      Function XMLGetElementCount():Integer;Virtual;

      Property XMLStatus:XMLStatus Read _Status;
  End;

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

      Constructor Create(Const Name:TERRAString; Const Value:TERRAString = ''); Overload;
      Constructor Create(Document:XMLDocument; Source:Stream; InitTag:TERRAString = '');Overload;

      Procedure Release; Override;

      Function AddTag(Const Name,Value:TERRAString):XMLNode;
      Procedure AddNode(Node:XMLNode);

      Function GetNodeByName(Const Name:TERRAString):XMLNode;
      Function GetNodeByIndex(Index:Integer):XMLNode;

      Function GetNodeByPath(Path:TERRAString; Const PathSeparator:TERRAChar):XMLNode;

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
      Procedure Release; Override;

      Procedure Load(Source:Stream);
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

Function XMLGetInteger(P:XMLNode; Const Name:TERRAString; Out Dest:Integer; Default:Integer = 0):Boolean;
Function XMLGetCardinal(P:XMLNode; Const Name:TERRAString; Out Dest:Cardinal; Default:Cardinal = 0):Boolean;
Function XMLGetByte(P:XMLNode; Const Name:TERRAString; Out Dest:Byte; Default:Byte):Boolean;
Function XMLGetBoolean(P:XMLNode; Const Name:TERRAString; Out  Dest:Boolean; Default:Boolean = False):Boolean;
Function XMLGetString(P:XMLNode; Const Name:TERRAString; Out Dest:TERRAString; Const Default:TERRAString = ''):Boolean;
Function XMLGetSingle(P:XMLNode; Const Name:TERRAString; Out Dest:Single; Default:Single = 0):Boolean;

Implementation
Uses TERRA_Error, TERRA_Collections, TERRA_FileManager, TERRA_FileStream, TERRA_MemoryStream, TERRA_Log;

Function XMLGetInteger(P:XMLNode; Const Name:TERRAString; Out Dest:Integer; Default:Integer):Boolean;
Var
  PP:XMLNode;
Begin
  PP := P.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function XMLGetCardinal(P:XMLNode; Const Name:TERRAString; Out Dest:Cardinal; Default:Cardinal):Boolean;
Var
  PP:XMLNode;
Begin
  PP := P.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function XMLGetSingle(P:XMLNode; Const Name:TERRAString; Out Dest:Single; Default:Single):Boolean;
Var
  PP:XMLNode;
Begin
  PP := P.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function XMLGetBoolean(P:XMLNode; Const Name:TERRAString; Out Dest:Boolean; Default:Boolean):Boolean;
Var
  PP:XMLNode;
Begin
  PP := P.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := (PP.Value = '1') Or (StringLower(PP.Value) = 'true')
  Else
    Dest := Default;
End;

Function XMLGetByte(P:XMLNode; Const Name:TERRAString; Out Dest:Byte; Default:Byte):Boolean;
Var
  PP:XMLNode;
Begin
  PP := P.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := StringToInt(PP.Value)
  Else
    Dest := Default;
End;

Function XMLGetString(P:XMLNode; Const Name:TERRAString; Out Dest:TERRAString; Const Default:TERRAString):Boolean;
Var
  PP:XMLNode;
Begin
  PP := P.GetNodeByName(Name);
  Result := PP<>Nil;
  If Result Then
    Dest := PP.Value
  Else
    Dest := Default;
End;

{ XMLNode }
Constructor XMLNode.Create(Const Name:TERRAString; Const Value:TERRAString='');
Begin
  Self._Name:=Name;
  Self._Value:=Value;
End;

Constructor XMLNode.Create(Document:XMLDocument; Source:Stream; InitTag:TERRAString);
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
                      Node := XMLNode.Create(_Document, Source, S);

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

Procedure XMLDocument.Load(Source:Stream);
Var
  Dest:Stream;
Begin
  _Root := XMLNode.Create(Self, Source);

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

  Load(Source);
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
  Load(Source);
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

// XMLElement
Procedure XMLElement.XMLRegisterStructure;
Begin
End;

Procedure XMLElement.XMLSynchronize;
Begin
End;

Function XMLElement.XMLGetPropertyCount:Integer;
Begin
  If (Self._DescriptorCount=0) Then
    XMLRegisterStructure;
  Result := _DescriptorCount;
End;

Function XMLElement.XMLGetProperty(Index:Integer):XMLDescriptor;
Begin
  Result := _Descriptors[Index];
End;

Function XMLElement.XMLGetProperty(Name:TERRAString):XMLDescriptor;
Var
  S:TERRAString;
  I:Integer;
Begin
	FillChar(Result, SizeOf(Result), 0);

  I := StringPos('.', Name, True);
  If (I>0) Then
  Begin
    S := StringCopy(Name, Succ(I), MaxInt);
    Name := StringCopy(Name, 1, Pred(I));
    Result := XMLGetProperty(Name);
    If (StringEquals(S, 'x')) Then
    Begin
      Inc(PByte(Result.Address), 0);
      Result.XMLType := xmlSingle;
    End Else
    If (StringEquals(S, 'Y')) Then
    Begin
      Inc(PByte(Result.Address), 4);
      Result.XMLType := xmlSingle;
    End Else
    If (StringEquals(S, 'Z')) Then
    Begin
      Inc(PByte(Result.Address), 8);
      Result.XMLType := xmlSingle;
    End Else
    If (StringEquals(S, 'red')) Or (StringEquals(S, 'r')) Then
    Begin
      Inc(PByte(Result.Address), 0);
      Result.XMLType := xmlByte;
    End Else
    If (StringEquals(S, 'green')) Or (StringEquals(S, 'g')) Then
    Begin
      Inc(PByte(Result.Address), 1);
      Result.XMLType := xmlByte;
    End Else
    If (StringEquals(S, 'blue')) Or (StringEquals(S, 'b')) Then
    Begin
      Inc(PByte(Result.Address), 2);
      Result.XMLType := xmlByte;
    End Else
    If (StringEquals(S, 'alpha')) Or (StringEquals(S, 'a')) Then
    Begin
      Inc(PByte(Result.Address), 3);
      Result.XMLType := xmlByte;
    End Else
      RaiseError('XML: Unknow type component ['+S+']');
    Exit;
  End;

  For I:=0 To Pred(_DescriptorCount) Do
  If (StringUpper(_Descriptors[I].Name) = Name) Then
    Result := _Descriptors[I];
End;

Function XMLElement.XMLGetElement(Index:Integer):XMLElement;
Begin
  Result := Nil;
End;

Function XMLElement.XMLGetElementCount():Integer;
Begin
  Result := 0;
End;

Function XMLElement.XMLNewElement(Const Name:TERRAString):XMLElement;
Begin
  Result := Nil;
End;

Procedure XMLElement.XMLRegisterElement(Const Name:TERRAString; Address:Pointer;
                                        XMLType:XMLType; Default:TERRAString='');
Begin
  Inc(_DescriptorCount);
  SetLength(_Descriptors,_DescriptorCount);
  _Descriptors[Pred(_DescriptorCount)].Name:=Name;
  _Descriptors[Pred(_DescriptorCount)].Address:=Address;
  _Descriptors[Pred(_DescriptorCount)].XMLType:=XMLType;
  _Descriptors[Pred(_DescriptorCount)].ElementCount:=1;
  _Descriptors[Pred(_DescriptorCount)].Default:=Default;
  _Descriptors[Pred(_DescriptorCount)].Found:=False;
End;

Procedure XMLElement.XMLRegisterArrayElement(Const Name:TERRAString; Address:Pointer;
                                             XMLType:XMLType; Size:Integer);
Begin
  Inc(_DescriptorCount);
  SetLength(_Descriptors,_DescriptorCount);
  _Descriptors[Pred(_DescriptorCount)].Name:=Name;
  _Descriptors[Pred(_DescriptorCount)].Address:=Address;
  _Descriptors[Pred(_DescriptorCount)].XMLType:=XMLType;
  _Descriptors[Pred(_DescriptorCount)].ElementCount:=Size;
  _Descriptors[Pred(_DescriptorCount)].Default:='';
  _Descriptors[Pred(_DescriptorCount)].Found:=False;
End;

Procedure XMLElement.XMLClearStructure;
Begin
  _DescriptorCount := 0;
  SetLength(_Descriptors, 0);
End;

Procedure XMLDescriptor.Read(Node:XMLNode);
Var
  S,S2:TERRAString;
  K:Integer;
Begin
  Found:=True;
  S:=Node.Value;

  If XMLType=xmlVector Then
  Begin
    If (ElementCount>1) Then
    Begin
      Log(logError, 'XML', 'Vector array not supported!');
      Exit;
    End;

    PVector3D(Address).X := StringToFloat(Node._Childs[0]._Value);
    PVector3D(Address).Y := StringToFloat(Node._Childs[1]._Value);
    PVector3D(Address).Z := StringToFloat(Node._Childs[2]._Value);
  End Else
  If XMLType=xmlColor Then
  Begin
    If (ElementCount>1) Then
    Begin
      Log(logError, 'XML', 'Color array not supported!');
      Exit;
    End;

    PColor(Address).R:=StringToInt(Node._Childs[0]._Value);
    PColor(Address).G:=StringToInt(Node._Childs[1]._Value);
    PColor(Address).B:=StringToInt(Node._Childs[2]._Value);
    PColor(Address).A:=StringToInt(Node._Childs[3]._Value);
  End Else
{  If XMLType=xmlTime Then
  Begin
    If (ElementCount>1) Then
    Begin
      Log(logError, 'XML', 'Time array not supported!');
      Exit;
    End;

    PTime(Address).Hour := StringToInt(Node._Childs[0]._Value);
    PTime(Address).Minute := StringToInt(Node._Childs[1]._Value);
    PTime(Address).Second := StringToInt(Node._Childs[2]._Value);
  End Else}
  Begin
    For K:=1 To ElementCount Do
    Begin
      If ElementCount=1 Then
      Begin
        S2:=S;
        S:='';
      End Else
        S2 := StringGetNextSplit(S, Ord(','));
      If (S2='') Then
      Begin
        Log(logError, 'XML', 'Number of array elements differs from declaration! ['+Node.GetPath+']');
        Exit;
      End;

      Case XMLType Of
        xmlString:  Begin
                    PString(Address)^:=S2;
                      Inc(PString(Address));
                    End;

        xmlBoolean: Begin
                      PBoolean(Address)^:=StringToBool(S2);
                      Inc(PBoolean(Address));
                    End;

        xmlInteger: Begin
                      PInteger(Address)^:=StringToInt(S2);
                      Inc(PInteger(Address));
                    End;

        xmlCardinal:  Begin
                        PCardinal(Address)^ := StringToCardinal(S2);
                        Inc(PCardinal(Address));
                      End;

        xmlByte:  Begin
                    PByte(Address)^:=StringToInt(S2);
                    Inc(PByte(Address));
                  End;

        xmlWord:  Begin
                    PWord(Address)^ := StringToCardinal(S2);
                    Inc(PWord(Address));
                  End;

        xmlSingle:  Begin
                      PSingle(Address)^ := StringToFloat(S2);
                      Inc(PSingle(Address));
                    End;
        Else
          Log(logError, 'XML', 'Invalid XML type '+ IntToString(Cardinal(XMLType)));
      End;
    End;

    If (S<>'') Then
      Log(logWarning,'XML', 'Extra array elements discarded! ['+Node.GetPath+']');
  End;
End;

Procedure XMLDescriptor.Write(Document:XMLDocument; Node:XMLNode);
Var
  S:TERRAString;
  J:Integer;
Begin
  S:='';

  If XMLType=xmlVector Then
  Begin
    If (ElementCount>1) Then
    Begin
      Log(logError, 'XML', 'Vector array not supported!');
      Exit;
    End;
    Document.AddVector(Name, PVector3D(Address)^, Node);
  End Else
  If XMLType=xmlColor Then
  Begin
    If (ElementCount>1) Then
    Begin
      Log(logError, 'XML', 'Color array not supported!');
      Exit;
    End;
    Document.AddColor(Name, PColor(Address)^, Node);
  End Else
  {If XMLType=xmlTime Then
  Begin
    If (ElementCount>1) Then
    Begin
      Log(logError, 'XML', 'Time array not supported!');
      Exit;
    End;
    Document.AddTime(Name, PTime(Address)^, Node);
  End Else}
  Begin
    For J:=1 To ElementCount Do
    Begin
      If J>1 Then
        S:=S+',';
      Case XMLType Of
        xmlString:  Begin
                      S:=S+PString(Address)^;
                      Inc(PString(Address));
                    End;

        xmlBoolean: Begin
                      S:=S+BoolToString(PBoolean(Address)^);
                      Inc(PBoolean(Address));
                    End;

        xmlInteger: Begin
                      S:=S+IntToString(PInteger(Address)^);
                      Inc(PInteger(Address));
                    End;

        xmlCardinal:  Begin
                        S := S + CardinalToString(PCardinal(Address)^);
                        Inc(PCardinal(Address));
                      End;

        xmlByte:    Begin
                      S:=S+IntToString(PByte(Address)^);
                      Inc(PByte(Address));
                    End;

        xmlWord:    Begin
                      S := S + CardinalToString(PWord(Address)^);
                      Inc(PWord(Address));
                    End;

        xmlSingle:  Begin
                      S := S + FloatToString(PSingle(Address)^);
                      Inc(PSingle(Address));
                    End;
        Else
          Log(logError, 'XML', 'Invalid XML type '+ IntToString(Cardinal(XMLType)));
      End;
    End;

    If (S<>Default) Then
      Node.AddTag(Name, S);
  End;
End;

Procedure XMLElement.XMLLoadElements(Source:XMLNode);
Var
  Found:Boolean;
  I,J:Integer;
  Node:XMLNode;
  Element:XMLElement;
Begin
  Self._Status:=xmlReading;
  XMLClearStructure;
  XMLRegisterStructure;

  For I:=0 To Pred(Source._NodeCount) Do
  Begin
    Node := Source._Childs[I];
    Found := False;

    For J:=0 To Pred(_DescriptorCount) Do
    If (StringUpper(_Descriptors[J].Name)=StringUpper(Node.Name)) Then
    Begin
      _Descriptors[J].Read(Node);
      Found := True;
      Break;
    End;

    If Not Found Then
    Begin
      Element := XMLNewElement(Node.Name);
      If Not Assigned(Element) Then
      Begin
        Log(logError, 'XML', 'Could not create XML element! ['+Node.Name+']');
        Exit;
      End;

      Element.XMLLoadElements(Node);
      Found := True;
      Break;
    End;
  End;

  For J:=0 To Pred(_DescriptorCount) Do
  If (Not _Descriptors[J].Found) And (_Descriptors[J].Default<>'') Then
  Begin
    Node := XMLNode.Create(_Descriptors[J].Name, _Descriptors[J].Default);
    _Descriptors[J].Read(Node);
    ReleaseObject(Node);
  End;

  XMLSynchronize;
End;

Procedure XMLElement.XMLSaveElements(Document:XMLDocument; Parent:XMLNode=Nil);
Var
  I,J, Count:Integer;
  Node:XMLNode;
  Element:XMLElement;
Begin
  Self._Status:=xmlWriting;
  XMLClearStructure;
  XMLRegisterStructure;

  Node := XMLNode.Create(Self.ClassName);

  For I:=0 To Pred(_DescriptorCount) Do
    _Descriptors[I].Write(Document, Node);

  Count := XMLGetElementCount();
  For J:=0 To Pred(Count) Do
  Begin
    Element := XMLGetElement(J);
    If Not Assigned(Element) Then
    Begin
      Log(logError, 'XML', 'XML element not avaliable! ['+IntToString(J)+']');
      Exit;
    End;

    Element.XMLSaveElements(Document, Node);
  End;

  Document.AddNode(Node, Parent);
End;

Procedure XMLElement.XMLLoad(Node:XMLNode);
Begin
  XMLLoadElements(Node);
End;

Procedure XMLElement.XMLLoad(Document:XMLDocument);
Begin
  XMLLoadElements(Document.Root);
End;

Procedure XMLElement.XMLSave(Document: XMLDocument);
Begin
  XMLSaveElements(Document);
End;

Procedure XMLElement.XMLLoad(Source:Stream);
Var
  Document:XMLDocument;
Begin
  Document := XMLDocument.Create;
  Document.Load(Source);
  XMLLoad(Document);
  ReleaseObject(Document);
End;

Procedure XMLElement.XMLSave(Dest:Stream);
Var
  Document: XMLDocument;
Begin
  Document := XMLDocument.Create;
  XMLSave(Document);
  Document.Save(Dest);
  ReleaseObject(Document);
End;

Procedure XMLElement.XMLLoad(Const FileName:TERRAString);
Var
  Source:Stream;
Begin
  Source := FileStream.Open(FileName);
  XMLLoad(Source);
  ReleaseObject(Source);
End;

Procedure XMLElement.XMLSave(Const FileName:TERRAString);
Var
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  XMLSave(Dest);
  ReleaseObject(Dest);
End;

Procedure XMLDocument.SetRoot(const Value: XMLNode);
Begin
  If Value = _Root Then
    Exit;

  ReleaseObject(_Root);

  _Root := Value;
End;

End.

