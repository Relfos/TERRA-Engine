Unit TERRA_XML;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_ObjectTree, TERRA_String, TERRA_Stream, TERRA_Utils, TERRA_Vector3D, TERRA_Color;

Const
  xmlSaveHeader   = 1;
  xmlSaveCompact  = 2;

Type
  XMLTagType = (xmlBeginTag,xmlEndTag,xmlData);

  XMLNode = Class(TERRAObjectNode)
    Protected
      _TempBuffer:TERRAString;
      _InitTag:TERRAString;

      Function Read(Source:Stream):TERRAString;
      Function GetTagType(Const S:TERRAString):XMLTagType;
      Function GetTagName(Const S:TERRAString):TERRAString;

    Public
      //Procedure LoadFromObject(Source:TERRAObject); Override;
      Procedure LoadFromStream(Source:Stream); Override;

      Function SaveToObject(Target:TERRAObject):TERRAObject; Override;
      Procedure SaveToStream(Dest:Stream; SaveFlags:Cardinal = 0); Override;
  End;

Implementation
Uses TERRA_Error, TERRA_Collections, TERRA_FileManager, TERRA_FileStream, TERRA_MemoryStream, TERRA_Log;

{ XMLNode }

Procedure XMLNode.LoadFromStream(Source:Stream);
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

  Document:XMLNode;
Begin
  Document := XMLNode(Self.Root);

  _Value := '';
  If (Source = Nil) Then
    Exit;

  If (_InitTag ='') Then
  Begin
    Repeat
      If (Source.EOF) And (Document._TempBuffer='') Then
        Exit;

      S := Read(Source);
      C := StringGetChar(S, 2);
    Until (C <> Ord('?')) And (C <> Ord('!'));
  End Else
    S := _InitTag;

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
    Self._ObjectName := S;

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
        AddChild(Node);
      End Else
        Node := Nil;
    End;
  End Else
    Self._ObjectName := S;

  If (ShortTag) Then
    Exit;

  Repeat
    S := Read(Source);
    Case GetTagType(S) Of
      xmlBeginTag:  Begin
                      Node := XMLNode.Create();
                      Node._InitTag := S;
                      Node.LoadFromStream(Source);

                      AddChild(Node);
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
  Until (Source.EOF) And (Document._TempBuffer='');
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


Function XMLNode.Read(Source:Stream):TERRAString;
{Const
  BufferSize = 1024;}
Var
  S:TERRAString;
  I,J:Integer;
  Document:XMLNode;
Begin
  Document := XMLNode(Self.Root);

  If (Document._TempBuffer<>'') Then
  Begin
    I := StringCharPos(Ord('<'), Document._TempBuffer);
    J := StringCharPos(Ord('>'), Document._TempBuffer);

    If (I=1) Then
      I := 0;

    If (J>0) And (((I>0) And (J<I)) Or (I<=0))  Then
    Begin
      I := J+1;
    End;

    If (I>1) Then
    Begin
      Result := StringCopy(Document._TempBuffer, 1, Pred(I));
      Document._TempBuffer := StringCopy(Document._TempBuffer, I, MaxInt);
      Document._TempBuffer := StringTrimLeft(Document._TempBuffer);
    End Else
    Begin
      Result := Document._TempBuffer;
      Document._TempBuffer := '';
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
    Document._TempBuffer := StringCopy(S, I, MaxInt);
    S := StringCopy(S, 1, Pred(I));

    Document._TempBuffer := StringTrimLeft(Document._TempBuffer);
  End Else
  If (J>0) Then
  Begin
    Document._TempBuffer := StringCopy(S, Succ(J), MaxInt);
    S := StringCopy(S, 1, J);

    Document._TempBuffer := StringTrimLeft(Document._TempBuffer);
  End;

  Result := StringTrim(S);

  If (Result='') And (Not Source.EOF) Then
  Begin
    Result := Read(Source);
    Exit;
  End;
End;

Procedure XMLNode.SaveToStream(Dest:Stream; SaveFlags:Cardinal);
Var
  Tabs, S:TERRAString;
  I, Count:Integer;
Begin
  If (Self._Parent = Nil) And ((SaveFlags And xmlSaveHeader)<>0) Then
    Dest.WriteLine('<?xml version="1.0" encoding="UTF-8"?>');

  Tabs:='';
  For I:=1 To GetParentCount Do
    Tabs:=Tabs+#9;

  S := '';

  If Value<>'' Then
  Begin
    For I:=0 To Pred(_ChildCount) Do
    Begin
      S := S + ' '+ _ChildList[I].Name + '="'+_ChildList[I].Value+'"';
    End;

    Dest.WriteLine(Tabs+'<'+ Self.Name +S+'>'+ Self.Value+'</'+ Self.Name +'>');
  End Else
  Begin
    Count := 0;

    If ((SaveFlags And xmlSaveCompact)<>0) Then
    Begin
      For I:=0 To Pred(_ChildCount) Do
      If (_ChildList[I].ChildCount<=0) Then
      Begin
        S := S + ' '+ _ChildList[I].Name + '="'+ _ChildList[I].Value+'"';
        Inc(Count);
      End;

      If Count>=_ChildCount Then
        S := S + '/';
    End Else
      S := '';

    Dest.WriteLine(Tabs+'<'+ Self.Name + S+'>');

    If Count>=_ChildCount Then
      Exit;

    For I:=0 To Pred(_ChildCount) Do
    If ((SaveFlags And xmlSaveCompact)=0) Or (_ChildList[I].ChildCount>0) Then
      _ChildList[I].SaveToStream(Dest, SaveFlags);

    Dest.WriteLine(Tabs+'</'+ Self.Name+'>');
  End;
End;


Function XMLNode.SaveToObject(Target:TERRAObject):TERRAObject;
Var
  I:Integer;
Begin
  If (Self._Parent = Nil) Then
  Begin
    For I:=0 To Pred(ChildCount) Do
      _ChildList[I].SaveToObject(Target);

    Result := Nil;
    Exit;
  End;

  Result := Inherited SaveToObject(Target);
End;

End.

