Unit TERRA_XML;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_ObjectTree, TERRA_String, TERRA_FileFormat, TERRA_Stream;

(*Const
  xmlSaveHeader   = 1;
  xmlSaveCompact  = 2;*)

Type
  XMLTagType = (xmlBeginTag,xmlEndTag,xmlData);

  XMLFormat = Class(TERRAFileFormat)
    Protected
      _TempBuffer:TERRAString;
      _InitTag:TERRAString;

      Procedure Clear();

      Function ReadString(Source:TERRAStream):TERRAString;
      Function GetTagType(Const S:TERRAString):XMLTagType;
      Function GetTagName(Const S:TERRAString):TERRAString;

      Function Identify(Source:TERRAStream):Boolean; Override;

      Procedure ReadNode(Source:TERRAStream; Target:TERRAObjectNode);
      Procedure WriteNode(Dest:TERRAStream; Target:TERRAObjectNode; TabCount:Integer);

    Public
      Function LoadFromStream(Target:TERRAObject; Source:TERRAStream):Boolean; Override;
      Function SaveToStream(Target:TERRAObject; Dest:TERRAStream):Boolean; Override;
  End;

Implementation
Uses TERRA_Log, TERRA_Engine, TERRA_OS;

{ XMLFormat }
Procedure XMLFormat.ReadNode(Source:TERRAStream; Target:TERRAObjectNode);
Var
  S, S2:TERRAString;
  Tag, Value:TERRAString;
  It:StringIterator;
  J:Integer;
  C:TERRAChar;
  Inside, Found:Boolean;
  ShortTag:Boolean;
  Node:TERRAObjectNode;
  Temp:TERRAString;
Begin
  Target.Value := '';

  If (_InitTag ='') Then
  Begin
    Repeat
      If (Source.EOF) And (_TempBuffer='') Then
        Exit;

      S := ReadString(Source);
      C := StringGetChar(S, 2);
    Until (C <> '?') And (C <> '!');
  End Else
    S := _InitTag;

  If GetTagType(S)<>xmlBeginTag Then
  Begin
    Engine.Log.Write(logError, 'XML', 'Invalid XML sintax!');
    Exit;
  End;

  ShortTag := (StringGetChar(S, -2) = '/');
  S := GetTagName(S);

  It := StringCharPosIterator(' ', S);
  If (Assigned(It)) Then
  Begin
    It.Split(S, S2);
    Target.Name := S;

    ReleaseObject(It);

    S := S2;
    If (StringLastChar(S) = '/') Then
      StringDropChars(S, -1);

    While (S<>'') Do
    Begin
      It := StringCreateIterator(S);
      Inside := False;
      Found := False;

      While It.HasNext() Do
      Begin
        C := It.GetNext();
        If (C= '"') Then
          Inside := Not Inside
        Else
        If (C = ' ') And (Not Inside) Then
        Begin
          It.Split(S2, S);
          Found := True;
          Break;
        End;
      End;
      ReleaseObject(It);

      If Not Found Then
      Begin
        S2 := S;
        S := '';
      End;

      It := StringCharPosIterator('=', S2);
      If Assigned(It) Then
      Begin
        It.Split(Tag, Value);
        ReleaseObject(It);

        Value := StringTrim(Value);
        Temp := VAlue;
        Value := StringCopy(Value, 2, -2);

        If VAlue = 'base64' Then
          Value := 'lol';

        Node := TERRAObjectNode.Create(Tag, Value);
        Target.AddChild(Node);
      End Else
        Node := Nil;
    End;
  End Else
    Target.Name := S;

  If (ShortTag) Then
    Exit;

  Repeat
    S := ReadString(Source);
    Case GetTagType(S) Of
      xmlBeginTag:  Begin
                      Node := TERRAObjectNode.Create();
                      _InitTag := S;
                      Target.AddChild(Node);
                      Self.ReadNode(Source, Node);
                    End;

      xmlEndTag:    Break;

      xmlData:
        Begin
          If (StringLastChar(S) = '"') Then
            StringDropChars(S, -1);

          StringRemoveSpecialHTMLChars(S);
          Target.Value := S;
        End;
    End;
  Until (Source.EOF) And (_TempBuffer='');
End;


Function XMLFormat.GetTagType(Const S:TERRAString):XMLTagType;
Begin
  If (StringGetChar(S, 1) = '<') Then
  Begin
    If (StringGetChar(S, 2) = '/') Then
      Result := xmlEndTag
    Else
      Result := xmlBeginTag;
  End Else
    Result := xmlData;
End;

Function XMLFormat.GetTagName(Const S:TERRAString):TERRAString;
Begin
  Result := StringCopy(S, 2, -2);
End;

Function XMLFormat.ReadString(Source:TERRAStream):TERRAString;
{Const
  BufferSize = 1024;}
Var
  S:TERRAString;
  I,J:Integer;
Begin
  If (_TempBuffer<>'') Then
  Begin
    I := StringCharPos('<', _TempBuffer);
    J := StringCharPos('>', _TempBuffer);

    If (I=1) Then
      I := 0;

    If (J>0) And (((I>0) And (J<I)) Or (I<=0))  Then
    Begin
      I := J+1;
    End;

    If (I>1) Then
    Begin
      Result := StringCopy(_TempBuffer, 1, Pred(I));
      _TempBuffer := StringCopy(_TempBuffer, I, MaxInt);
      _TempBuffer := StringTrimLeft(_TempBuffer);
    End Else
    Begin
      Result := _TempBuffer;
      _TempBuffer := '';
    End;


    Result := StringTrim(Result);

    If (Result='') And (Not Source.EOF) Then
    Begin
      Result := ReadString(Source);
      Exit;
    End;

    //Application.Instance.LogToConsole(Result);
    Exit;
  End;

  S := '';
  Source.ReadLine(S);
  S := StringTrimLeft(S);
  StringReplaceText('/ >', '/>', S);

  If (S='') And (Not Source.EOF) Then
  Begin
    Result := ReadString(Source);
    Exit;
  End;

  I := StringPos('</', S);
  J := StringPos('>', S);
  If (I>0) And ((I<J) Or (J<1)) Then
  Begin
    _TempBuffer := StringCopy(S, I, MaxInt);
    S := StringCopy(S, 1, Pred(I));

    _TempBuffer := StringTrimLeft(_TempBuffer);
  End Else
  If (J>0) Then
  Begin
    _TempBuffer := StringCopy(S, Succ(J), MaxInt);
    S := StringCopy(S, 1, J);

    _TempBuffer := StringTrimLeft(_TempBuffer);
  End;

  Result := StringTrim(S);
  //Application.Instance.LogToConsole(Result);

  If (Result='') And (Not Source.EOF) Then
  Begin
    Result := ReadString(Source);
    Exit;
  End;
End;

Procedure XMLFormat.WriteNode(Dest:TERRAStream; Target:TERRAObjectNode; TabCount:Integer);
Var
  Tabs, S:TERRAString;
  I, Count:Integer;
Begin
  Tabs := StringFill(TabCount, #9);

  S := '';

  If Target.Value<>'' Then
  Begin
    For I:=0 To Pred(Target.ChildCount) Do
    Begin
      S := S + ' '+ Target.Children[I].Name + '="'+ Target.Children[I].Value+'"';
    End;

    Dest.WriteLine(Tabs+'<'+ Target.Name +S+'>'+ Target.Value+'</'+ Target.Name +'>');
  End Else
  Begin
    Count := 0;

    //If ((SaveFlags And xmlSaveCompact)<>0) Then
    If True Then
    Begin
      For I:=0 To Pred(Target.ChildCount) Do
      If (Target.Children[I].ChildCount<=0) Then
      Begin
        S := S + ' '+ Target.Children[I].Name + '="'+ Target.Children[I].Value+'"';
        Inc(Count);
      End;

      If Count>=Target.ChildCount Then
        S := S + '/';
    End Else
      S := '';

    Dest.WriteLine(Tabs+'<'+ Target.Name + S+'>');

    If Count>=Target.ChildCount Then
      Exit;

    For I:=0 To Pred(Target.ChildCount) Do
    If {((SaveFlags And xmlSaveCompact)=0) Or} (Target.Children[I].ChildCount>0) Then
      Self.WriteNode(Dest, Target.Children[I], Succ(TabCount));

    Dest.WriteLine(Tabs+'</'+ Target.Name+'>');
  End;
End;

Function XMLFormat.SaveToStream(Target:TERRAObject; Dest:TERRAStream): Boolean;
Begin
  Dest.WriteLine('<?xml version="1.0" encoding="UTF-8"?>');
  If (Target Is TERRAObjectNode) Then
    Self.WriteNode(Dest, TERRAObjectNode(Target), 0);
End;

Function XMLFormat.Identify(Source: TERRAStream): Boolean;
Begin
  Result := True;
End;

Function XMLFormat.LoadFromStream(Target: TERRAObject; Source: TERRAStream): Boolean;
Begin
  Result := (Target Is TERRAObjectNode);
  If Result Then
  Begin
    Self.Clear();
    Self.ReadNode(Source, TERRAObjectNode(Target));
  End;
End;

Procedure XMLFormat.Clear;
Begin
  _TempBuffer := '';
  _InitTag := '';
End;

Initialization
  Engine.Formats.Add(XMLFormat.Create(TERRAObjectNode, 'xml'));
End.

