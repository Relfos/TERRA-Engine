Unit TERRA_XML;
{$I terra.inc}

Interface
Uses TERRA_IO, TERRA_Utils, TERRA_Vector3D, TERRA_Color;

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
    Name:AnsiString;
    Address:Pointer;
    ElementCount:Integer;
    XMLType:XMLType;
    Default:AnsiString;
    Found:Boolean;

    Procedure Read(Node:XMLNode);
    Procedure Write(Document:XMLDocument; Node:XMLNode);
  End;

  XMLElement = Class
    Protected
      _Descriptors:Array Of XMLDescriptor;
      _DescriptorCount:Integer;
      _Status:XMLStatus;

      Procedure XMLRegisterElement(Name:AnsiString; Address:Pointer; XMLType:XMLType; Default:AnsiString='');
      Procedure XMLRegisterArrayElement(Name:AnsiString; Address:Pointer; XMLType:XMLType; Size:Integer);

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

      Procedure XMLLoad(FileName:AnsiString);Overload;
      Procedure XMLSave(FileName:AnsiString);Overload;

      Function XMLGetPropertyCount:Integer;
      Function XMLGetProperty(Index:Integer):XMLDescriptor; Overload;
      Function XMLGetProperty(Name:AnsiString):XMLDescriptor; Overload;

      Function XMLNewElement(Name:AnsiString):XMLElement;Virtual;
      Function XMLGetElement(Index:Integer):XMLElement;Virtual;
      Function XMLGetElementCount():Integer;Virtual;

      Property XMLStatus:XMLStatus Read _Status;
  End;

  XMLNode = Class
    Protected
      _Name:AnsiString;
      _Value:AnsiString;
      _Childs:Array Of XMLNode;
      _ChildCount:Integer;
      _Parent:XMLNode;
      _Document:XMLDocument;

      Procedure Save(Dest:Stream; SaveFlags:Cardinal);
      Function Read(Source:Stream):AnsiString;
      Function GetTagType(S:AnsiString):XMLTagType;
      Function GetTagName(S:AnsiString):AnsiString;
      Function GetPath:AnsiString;
      Function GetParentCount:Integer;

    Public

      Constructor Create(Name:AnsiString; Value:AnsiString = ''); Overload;
      Constructor Create(Document:XMLDocument; Source:Stream; InitTag:AnsiString = '');Overload;

      Destructor Destroy;Reintroduce;

      Function AddTag(Name,Value:AnsiString):XMLNode;
      Procedure AddNode(Node:XMLNode);

      Function GetNode(Name:AnsiString):XMLNode;
      Function GetChild(Index:Integer):XMLNode;

      Property Name:AnsiString Read _Name;
      Property Value:AnsiString Read _Value Write _Value;

      Property ChildCount:Integer Read _ChildCount;
  End;

  XMLDocument = Class
    Protected
      _Root:XMLNode;
      _TempBuffer:AnsiString;

    Public
      Destructor Destroy;Reintroduce;

      Procedure Load(Source:Stream);
      Procedure Save(Dest:Stream; SaveFlags:Cardinal = 0);

      Procedure LoadFromFile(FileName:AnsiString);
      Procedure SaveToFile(FileName:AnsiString; SaveFlags:Cardinal = 0);

      Procedure LoadFromString(Data:AnsiString);
      //Procedure SaveToFile(Var Data:AnsiString);Overload;

      Procedure AddNode(Node:XMLNode; Parent:XMLNode=Nil);
      Function GetNode(Name:AnsiString):XMLNode;

      Function AddString(Name:AnsiString; Value:AnsiString=''; Parent:XMLNode=Nil):XMLNode;
      Function AddBoolean(Name:AnsiString; Value:Boolean; Parent:XMLNode=Nil):XMLNode;
      Function AddInteger(Name:AnsiString; Value:Integer; Parent:XMLNode=Nil):XMLNode;
      Function AddCardinal(Name:AnsiString; Value:Cardinal; Parent:XMLNode=Nil):XMLNode;
      Function AddSingle(Name:AnsiString; Value:Single; Parent:XMLNode=Nil):XMLNode;
      Function AddVector(Name:AnsiString; Value:Vector3D; Parent:XMLNode=Nil):XMLNode;
      Function AddColor(Name:AnsiString; Value:Color; Parent:XMLNode=Nil):XMLNode;
      Function AddTime(Name:AnsiString; Value:TERRATime; Parent:XMLNode=Nil):XMLNode;

      Property Root:XMLNode Read _Root;
  End;

Function XMLConvertToBinary(SourceFile, DestFile, ConstantFile:AnsiString):Boolean;
Function XMLLoadBinary(SourceFile:AnsiString):XMLDocument;

Implementation
Uses TERRA_Error, TERRA_Classes, TERRA_FileManager, TERRA_FileIO, TERRA_Log, TERRA_Unicode;

// LXMLNode

Constructor XMLNode.Create(Name:AnsiString; Value:AnsiString='');
Begin
  Self._Name:=Name;
  Self._Value:=Value;
End;

Constructor XMLNode.Create(Document:XMLDocument; Source:Stream; InitTag:AnsiString);
Var
  S, S2:AnsiString;
  Tag, Value:AnsiString;
  I,J,K,Len:Integer;
  C:AnsiChar;
  Inside:Boolean;
  ShortTag:Boolean;
  Node:XMLNode;
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
    Until (ucs2_Length(S)>=2) And (ucs2_ascii(S, 2)<>'?');
  End Else
    S := InitTag;

  If GetTagType(S)<>xmlBeginTag Then
  Begin
    Log(logError, 'XML', 'Invalid XML sintax!');
    Exit;
  End;

  ShortTag := ucs2_ascii(S, Pred(ucs2_Length(S))) = '/';
  S := GetTagName(S);

  {If Pos('user_id',S)>0 Then
    IntToString(2);}

  I := ucs2_Pos(' ', S);
  If (I>0) Then
  Begin
    S2 := ucs2_Copy(S, Succ(I), MaxInt);
    S := ucs2_Copy(S, 1, Pred(I));
    Self._Name := S;

    S := S2;
    If (ucs2_ascii(S, ucs2_Length(S))='/') Then
      S := ucs2_copy(S, 1, Pred(ucs2_Length(S)));

    While (S<>'') Do
    Begin
      Len := ucs2_Length(S);
      Inside := False;
      K := -1;
      For J:=1 To Len Do
      Begin
        C := ucs2_Ascii(S, J);
        If (C='"') Then
          Inside := Not Inside
        Else
        If (C=' ') And (Not Inside) Then
        Begin
          K := J;
          Break;
        End;
      End;

      If K<0 Then
      Begin
        S2 := S;
        S := '';
      End Else
      Begin
        S2 := ucs2_Copy(S, 1, Pred(K));
        S := ucs2_Copy(S, Succ(K), MaxInt);
      End;

      I := ucs2_Pos('=', S2);
      Tag := ucs2_Copy(S2, 1, Pred(I));

      Value := ucs2_Copy(S2, I+1, MaxInt);
      Value := ucs2_Trim(Value);
      Value := ucs2_Copy(Value, 2, ucs2_Length(Value)-2);

      Node := XMLNode.Create(Tag, Value);
      AddNode(Node);
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
          Len := ucs2_Length(S);
          If (ucs2_ascii(S,Len)='"') Then
            S := ucs2_copy(S, 1, Pred(Len));

          RemoveSpecialHTMLChars(S);
          _Value := S;
        End;
    End;
  Until (Source.EOF) And (Self._Document._TempBuffer='');
End;

Destructor XMLNode.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ChildCount) Do
    _Childs[I].Destroy;
  SetLength(_Childs,0);
  _ChildCount:=0;
End;

Function XMLNode.GetTagType(S:AnsiString):XMLTagType;
Begin
  If (S='') Or(ucs2_ascii(S, 1)<>'<') Or (ucs2_ascii(S, ucs2_Length(S))<>'>') Then
    Result := xmlData
  Else
  If (ucs2_ascii(S, 2)='/') Then
    Result := xmlEndTag
  Else
    Result := xmlBeginTag;
End;

Function XMLNode.GetTagName(S:AnsiString):AnsiString;
Begin
  Result := ucs2_Copy(S, 2 ,ucs2_Length(S)-2);
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

Function XMLNode.GetPath:AnsiString;
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

Function XMLNode.GetChild(Index:Integer):XMLNode;
Begin
  If (Index<0) Or (Index>=_ChildCount) Then
    Result := Nil
  Else
    Result := _Childs[Index];
End;

Function XMLNode.GetNode(Name:AnsiString):XMLNode;
Var
  I:Integer;
Begin
  Name:=UpStr(Name);
  Result:=Nil;
  For I:=0 To Pred(_ChildCount) Do
    If UpStr(_Childs[I].Name)=Name Then
    Begin
      Result:=_Childs[I];
      Exit;
    End;
End;

Function XMLNode.Read(Source:Stream):AnsiString;
{Const
  BufferSize = 1024;}
Var
  S:AnsiString;
  I,J:Integer;
Begin
  If (_Document._TempBuffer<>'') Then
  Begin
    I := ucs2_Pos('</', _Document._TempBuffer);
    J := ucs2_Pos('>', _Document._TempBuffer);

    If (I=1) Then
      I := 0;

    If (J>0) And (((I>0) And (J<I)) Or (I<=0))  Then
    Begin
      I := J+1;
    End;

    If (I>1) Then
    Begin
      Result := ucs2_Copy(_Document._TempBuffer, 1, Pred(I));
      _Document._TempBuffer := ucs2_Copy(_Document._TempBuffer, I, MaxInt);
      _Document._TempBuffer := ucs2_TrimLeft(_Document._TempBuffer);
    End Else
    Begin
      Result := _Document._TempBuffer;
      _Document._TempBuffer := '';
    End;


    Result := ucs2_Trim(Result);

    If (Result='') And (Not Source.EOF) Then
    Begin
      Result := Read(Source);
      Exit;
    End;

    Exit;
  End;

  S := '';
  Source.ReadUnicodeLine(S);
  S := ucs2_TrimLeft(S);
  ucs2_ReplaceText('/ >', '/>', S);

  If (S='') And (Not Source.EOF) Then
  Begin
    Result := Read(Source);
    Exit;
  End;

  I := ucs2_Pos('</', S);
  J := ucs2_Pos('>', S);
  If (I>0) And ((I<J) Or (J<1)) Then
  Begin
    _Document._TempBuffer := ucs2_Copy(S, I, MaxInt);
    S := ucs2_Copy(S, 1, Pred(I));

    _Document._TempBuffer := ucs2_TrimLeft(_Document._TempBuffer);
  End Else
  If (J>0) Then
  Begin
    _Document._TempBuffer := ucs2_Copy(S, Succ(J), MaxInt);
    S := ucs2_Copy(S, 1, J);

    _Document._TempBuffer := ucs2_TrimLeft(_Document._TempBuffer);
  End;

  Result := ucs2_Trim(S);

  If (Result='') And (Not Source.EOF) Then
  Begin
    Result := Read(Source);
    Exit;
  End;
End;

Procedure XMLNode.Save(Dest:Stream; SaveFlags:Cardinal);
Var
  Tabs, S:AnsiString;
  I, Count:Integer;
Begin
  Tabs:='';
  For I:=1 To GetParentCount Do
    Tabs:=Tabs+#9;

  S := '';

  If Value<>'' Then
  Begin
    For I:=0 To Pred(_ChildCount) Do
    Begin
      S := S + ' '+ _Childs[I].Name + '="'+_Childs[I]._Value+'"';
    End;

    Dest.WriteLine(Tabs+'<'+Name+S+'>'+Value+'</'+Name+'>');
  End Else
  Begin
    Count := 0;

    If ((SaveFlags And xmlSaveCompact)<>0) Then
    Begin
      For I:=0 To Pred(_ChildCount) Do
      If (_Childs[I]._ChildCount<=0) Then
      Begin
        S := S + ' '+ _Childs[I].Name + '="'+_Childs[I]._Value+'"';
        Inc(Count);
      End;

      If Count>=_ChildCount Then
        S := S + '/';
    End Else
      S := '';

    Dest.WriteLine(Tabs+'<'+Name+S+'>');

    If Count>=_ChildCount Then
      Exit;

    For I:=0 To Pred(_ChildCount) Do
    If ((SaveFlags And xmlSaveCompact)=0) Or (_Childs[I]._ChildCount>0) Then
      _Childs[I].Save(Dest, SaveFlags);

    Dest.WriteLine(Tabs+'</'+Name+'>');
  End;
End;

Procedure XMLNode.AddNode(Node:XMLNode);
Begin
  Node._Parent:=Self;
  Inc(_ChildCount);
  SetLength(_Childs,_ChildCount);
  _Childs[Pred(_ChildCount)]:=Node;
End;

Function XMLNode.AddTag(Name, Value:AnsiString): XMLNode;
Var
  Node:XMLNode;
Begin
  Node := XMLNode.Create(Name,Value);
  AddNode(Node);
  Result:=Node;
End;

// LXMLDocument
Destructor XMLDocument.Destroy;
Begin
  If Assigned(_Root) Then
    _Root.Destroy;
End;

Function XMLDocument.GetNode(Name:AnsiString):XMLNode;
Begin
  Result:=Nil;
  If Assigned(_Root) Then
    Result:=_Root.GetNode(Name);
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

Function XMLDocument.AddString(Name:AnsiString; Value:AnsiString=''; Parent:XMLNode=Nil):XMLNode;
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

Function XMLDocument.AddBoolean(Name:AnsiString; Value:Boolean; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, BoolToString(Value), Parent);
End;

Function XMLDocument.AddInteger(Name:AnsiString; Value:Integer; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, IntToString(Value), Parent);
End;

Function XMLDocument.AddCardinal(Name:AnsiString; Value:Cardinal; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, CardinalToString(Value), Parent);
End;

Function XMLDocument.AddSingle(Name:AnsiString; Value:Single; Parent:XMLNode=Nil):XMLNode;
Begin
  Result := AddString(Name, FloatToString(Value), Parent);
End;

Procedure DumpXML(Node:XMLNode; Dest:Stream; Level:Integer);
Var
  I:Integer;
  S:AnsiString;
Begin
  S := '';
  For I:=1 To Level Do
    S := S + '  ';

  Dest.WriteLine(S+Node.Name+':'+Node.Value);
  For I:=0 To Pred(Node.ChildCount) Do
    DumpXML(Node.GetChild(I), Dest, Succ(Level));
End;

Procedure XMLDocument.Load(Source:Stream);
Var
  Dest:Stream;
Begin
  _Root := XMLNode.Create(Self, Source);

  {$IFDEF PC}
 (* Dest := FileStream.Create('debug\'+GetFileName(Source.Name,False));
  DumpXML(_Root, Dest, 0);
  Dest.Destroy;*)
  {$ENDIF}
End;

Procedure XMLDocument.Save(Dest:Stream; SaveFlags:Cardinal);
Begin
  If ((SaveFlags And xmlSaveHeader)<>0) Then
    Dest.WriteLine('<?xml version="1.0" encoding="UTF-8"?>');

  _Root.Save(Dest, SaveFlags);
End;

Procedure XMLDocument.LoadFromFile(FileName:AnsiString);
Var
  Source:FileStream;
Begin
  Source := FileStream.Open(FileName);
  Load(Source);
  Source.Destroy;
End;

Procedure XMLDocument.SaveToFile(FileName:AnsiString; SaveFlags:Cardinal);
Var
  Dest:FileStream;
Begin
  Dest := FileStream.Create(FileName);
  Save(Dest, SaveFlags);
  Dest.Destroy;
End;

Procedure XMLDocument.LoadFromString(Data:AnsiString);
Var
  Source:MemoryStream;
Begin
  Source := MemoryStream.Create(Length(Data), @Data[1]);
  Load(Source);
  Source.Destroy;
End;

Function XMLDocument.AddVector(Name:AnsiString; Value:Vector3D; Parent:XMLNode=Nil):XMLNode;
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

Function XMLDocument.AddColor(Name:AnsiString; Value:Color; Parent:XMLNode=Nil):XMLNode;
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

Function XMLDocument.AddTime(Name:AnsiString; Value:TERRATime; Parent:XMLNode=Nil):XMLNode;
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

Function XMLElement.XMLGetProperty(Name:AnsiString):XMLDescriptor;
Var
  S:AnsiString;
  I:Integer;
Begin
  Name := UpStr(Name);

  I := Pos('.', Name);
  If (I>0) Then
  Begin
    S := Copy(Name, Succ(I), MaxInt);
    Name := Copy(Name, 1, Pred(I));
    Result := XMLGetProperty(Name);
    If (S='X') Then
    Begin
      Inc(PByte(Result.Address), 0);
      Result.XMLType := xmlSingle;
    End Else
    If (S='Y') Then
    Begin
      Inc(PByte(Result.Address), 4);
      Result.XMLType := xmlSingle;
    End Else
    If (S='Z') Then
    Begin
      Inc(PByte(Result.Address), 8);
      Result.XMLType := xmlSingle;
    End Else
    If (S='RED') Or (S='R') Then
    Begin
      Inc(PByte(Result.Address), 0);
      Result.XMLType := xmlByte;
    End Else
    If (S='GREEN') Or (S='G') Then
    Begin
      Inc(PByte(Result.Address), 1);
      Result.XMLType := xmlByte;
    End Else
    If (S='BLUE') Or (S='B') Then
    Begin
      Inc(PByte(Result.Address), 2);
      Result.XMLType := xmlByte;
    End Else
    If (S='ALPHA') Or (S='A') Then
    Begin
      Inc(PByte(Result.Address), 3);
      Result.XMLType := xmlByte;
    End Else
      RaiseError('XML: Unknow type component ['+S+']');
    Exit;
  End;

  For I:=0 To Pred(_DescriptorCount) Do
  If (UpStr(_Descriptors[I].Name) = Name) Then
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

Function XMLElement.XMLNewElement(Name:AnsiString):XMLElement;
Begin
  Result := Nil;
End;

Procedure XMLElement.XMLRegisterElement(Name:AnsiString; Address:Pointer;
                                        XMLType:XMLType; Default:AnsiString='');
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

Procedure XMLElement.XMLRegisterArrayElement(Name:AnsiString; Address:Pointer;
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
  S,S2:AnsiString;
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
        S2:=GetNextWord(S,',');
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
  S:AnsiString;
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

  For I:=0 To Pred(Source._ChildCount) Do
  Begin
    Node := Source._Childs[I];
    Found := False;

    For J:=0 To Pred(_DescriptorCount) Do
    If (UpStr(_Descriptors[J].Name)=UpStr(Node.Name)) Then
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
    Node.Destroy;
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
  Document.Destroy;
End;

Procedure XMLElement.XMLSave(Dest:Stream);
Var
  Document: XMLDocument;
Begin
  Document := XMLDocument.Create;
  XMLSave(Document);
  Document.Save(Dest);
  Document.Destroy;
End;

Procedure XMLElement.XMLLoad(FileName:AnsiString);
Var
  Source:Stream;
Begin
  Source := FileStream.Open(FileName);
  XMLLoad(Source);
  Source.Destroy;
End;

Procedure XMLElement.XMLSave(FileName:AnsiString);
Var
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  XMLSave(Dest);
  Dest.Destroy;
End;

Procedure XMLConvertNode(Node:XMLNode; Constants:List);
Var
  I, N:Integer;
  P:KeyPairObject;
  ConstName:AnsiString;
Begin
  If (Pos('@',Node.Value)=1) Then
  Begin
    ConstName := Copy(Node.Value, 2, MaxInt);
    P := KeyPairObject(Constants.FindByKey(ConstName));
    If P = Nil Then
    Begin
      RaiseError('Constant not found: '+Node.Value);
      Exit;
    End;

    Node.Value := P.Value;
  End;

  For I:=0 To Pred(Node.ChildCount) Do
    XMLConvertNode(Node.GetChild(I), Constants);
End;

Procedure XMLWriteNode(Dest:Stream; Node:XMLNode);
Var
  I, N:Integer;
Begin
  Dest.WriteString(Node.Name);
  Dest.WriteString(Node.Value);
  N := Node.ChildCount;
  Dest.Write(@N, 4);
  For I:=0 To Pred(N) Do
    XMLWriteNode(Dest, Node.GetChild(I));
End;

Function XMLConvertToBinary(SourceFile, DestFile, ConstantFile:AnsiString):Boolean;
Var
  Doc:XMLDocument;
  Dest:Stream;
  Constants:List;
Begin
  Doc := XMLDocument.Create;
  Doc.LoadFromFile(SourceFile);

  Constants := LoadKeypairList(ConstantFile);
  XMLConvertNode(Doc.Root, Constants);
  Constants.Destroy();

  Dest := FileStream.Create(DestFile);
  XMLWriteNode(Dest, Doc.Root);
  Dest.Destroy;

  Doc.Destroy();

  Result := True;
End;

Procedure XMLLoadNode(Src:Stream; Node:XMLNode);
Var
  I, N:Integer;
  Child:XMLNode;
  S:AnsiString;
Begin
  S := '';
  Src.ReadString(S);
  Node._Name := S;
  Src.ReadString(S);
  Node._Value := S;
  Src.Read(@N, 4);
  For I:=0 To Pred(N) Do
  Begin
    Child := XMLNode.Create();
    Node.AddNode(Child);
    XMLLoadNode(Src, Child);
  End;
End;


Function XMLLoadBinary(SourceFile:AnsiString):XMLDocument;
Var
  S:AnsiString;
  Dest, Src:Stream;
Begin
  S := FileManager.Instance.SearchResourceFile(SourceFile);
  If (S='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := XMLDocument.Create;
  Src := FileManager.Instance.OpenFileStream(S);
  Result._Root := XMLNode.Create('');
  XMLLoadNode(Src, Result.Root);
  Src.Destroy;

  {$IFDEF PC}
  {Dest := FileStream.Create('debug\'+GetFileName(SourceFile,False));
  DumpXML(Result.Root, Dest, 0);
  Dest.Destroy;}
  {$ENDIF}

End;

End.

