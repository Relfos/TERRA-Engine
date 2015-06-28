Unit TERRA_XMLBinary;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_FileStream, TERRA_XML;

Const
  XMLBinaryHeader = 'TbXD';

Type
  XMLBinaryReader = Class(TERRAObject)
    Protected
      _Keys:Array Of TERRAString;
      _KeyCount:Integer;

      Procedure XMLReadNode(Src:Stream; Node:XMLNode);

    Public
      Function Load(SourceFile:TERRAString):XMLDocument;
  End;

  XMLBinaryWriter = Class(TERRAObject)
    Protected
      _Keys:Array Of TERRAString;
      _KeyCount:Integer;

      Function GetKey(Const Name:TERRAString):Cardinal;

      Procedure XMLWriteNode(Dest:Stream; Node:XMLNode);

    Public
      Function Convert(SourceFile, DestFile, ConstantFile:TERRAString):Boolean;
  End;

Function XMLLoadBinary(SourceFile:TERRAString):XMLDocument;
Function XMLConvertToBinary(SourceFile, DestFile, ConstantFile:TERRAString):Boolean;

Implementation
Uses TERRA_FileUtils, TERRA_FileManager, TERRA_Collections, TERRA_KeyPairObjects, TERRA_HashMap, TERRA_Error;

Procedure XMLBinaryReader.XMLReadNode(Src:Stream; Node:XMLNode);
Var
  N:Cardinal;
  I, Count:Integer;
  Child:XMLNode;
  S:TERRAString;
Begin
  Src.ReadCardinal(N);
  If (N<_KeyCount) Then
    Node.Name := _Keys[N]
  Else
    Node.Name := '';

  Src.ReadString(S);
  Node.Value := S;

  Src.ReadInteger(Count);
  For I:=0 To Pred(Count) Do
  Begin
    Child := XMLNode.Create();
    Node.AddNode(Child);
    XMLReadNode(Src, Child);
  End;
End;


Function XMLBinaryReader.Load(SourceFile:TERRAString):XMLDocument;
Var
  Header:FileHeader;
  Dest, Src:Stream;
  I:Integer;
  Temp, Ofs:Cardinal;
Begin
  Result := Nil;

  Src := FileManager.Instance.OpenStream(SourceFile);
  If (Src = Nil) Then
    Exit;

  Src.ReadHeader(Header);
  If Not CompareFileHeader(Header, XMLBinaryHeader) Then
    Exit;

  Result := XMLDocument.Create();
  Result.Root := XMLNode.Create('');

  Src.ReadCardinal(Ofs);
  Temp := Src.Position;

  Src.Seek(Ofs);
  Src.ReadInteger(_KeyCount);
  SetLength(_Keys, _KeyCount);
  For I:=0 To Pred(_KeyCount) Do
    Src.ReadString(_Keys[I]);

  Src.Seek(Temp);

  Self.XMLReadNode(Src, Result.Root);
  ReleaseObject(Src);

  {$IFDEF PC}
  {Dest := FileStream.Create('debug\'+GetFileName(SourceFile,False));
  DumpXML(Result.Root, Dest, 0);
  ReleaseObject(Dest);}
  {$ENDIF}
End;

Function XMLLoadBinary(SourceFile:TERRAString):XMLDocument;
Var
  Reader:XMLBinaryReader;
Begin
  Reader := XMLBinaryReader.Create();
  Result := Reader.Load(SourceFile);
  ReleaseObject(Reader);
End;

{ XMLBinaryWriter }
Function XMLConvertNode(Node:XMLNode; Constants:HashMap):TERRAString;
Var
  It:StringIterator;
  I, J, N, Len:Integer;
  P:StringKeyPair;
  S, ConstName, Target:TERRAString;
  C:TERRAChar;

Function IsValidChar(Const C:TERRAChar):Boolean;
Begin
  Result := ((C>= Ord('0')) And (C<= Ord('9'))) Or ((C>= Ord('A')) And (C<= Ord('Z'))) Or ((C>=Ord('a')) And (C<=Ord('z'))) Or (C=Ord('_'));
End;

Begin
  J := StringCharPos(Ord('@'), Node.Value);
  While (J>0) Do
  Begin
    N := 1;
    StringCreateIterator(Node.Value, It);
    It.Seek(Succ(J));
    While It.HasNext Do
    Begin
      C := It.GetNext();
      If (IsValidChar(C)) Then
        Inc(N)
      Else
        Break;
    End;

    S := StringCopy(Node.Value, J, N);

    ConstName := StringCopy(S, 2, MaxInt);
    P := StringKeyPair(Constants.GetItemByKey(ConstName));
    If P = Nil Then
    Begin
      Result := ConstName;
      Exit;
    End;

    Target := Node.Value;
    StringReplaceText(S, P.Value, Target);
    Node.Value := Target;

    J := StringCharPos(Ord('@'), Node.Value);
  End;

  For I:=0 To Pred(Node.NodeCount) Do
  Begin
    Result := XMLConvertNode(Node.GetNodeByIndex(I), Constants);
    If Result<>'' Then
      Exit;
  End;

  Result := '';
End;

Procedure XMLBinaryWriter.XMLWriteNode(Dest:Stream; Node:XMLNode);
Var
  Key:Cardinal;
  I:Integer;
Begin
  Key := Self.GetKey(Node.Name);
  Dest.WriteCardinal(Key);
  Dest.WriteString(Node.Value);
  Dest.WriteInteger(Node.NodeCount);
  For I:=0 To Pred(Node.NodeCount) Do
    XMLWriteNode(Dest, Node.GetNodeByIndex(I));
End;

Function XMLBinaryWriter.GetKey(const Name: TERRAString): Cardinal;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_KeyCount) Do
  If (StringEquals(Name, _Keys[I])) Then
  Begin
    Result := I;
    Exit;
  End;

  Result := _KeyCount;
  Inc(_KeyCount);
  SetLength(_Keys, _KeyCount);
  _Keys[Result] := Name;
End;


Function XMLBinaryWriter.Convert(SourceFile, DestFile, ConstantFile: TERRAString): Boolean;
Var
  I:Integer;
  Ofs:Cardinal;
  Doc:XMLDocument;
  Dest:Stream;
  Constants:HashMap;
  ErrorStr:TERRAString;
Begin
  SetLength(_Keys, 0);
  _KeyCount := 0;

  Doc := XMLDocument.Create;
  Doc.LoadFromFile(SourceFile);

  Constants := LoadKeypairList(ConstantFile);
  ErrorStr := XMLConvertNode(Doc.Root, Constants);
  ReleaseObject(Constants);

  If ErrorStr<>'' Then
    RaiseError('Constant not found: '+ErrorStr+' in file '+SourceFile);

  Dest := FileStream.Create(DestFile);

  Dest.WriteHeader(XMLBinaryHeader);
  Dest.WriteCardinal(0);

  XMLWriteNode(Dest, Doc.Root);

  Ofs := Dest.Position;

  Dest.WriteInteger(_KeyCount);
  For I:=0 To Pred(_KeyCount) Do
    Dest.WriteString(_Keys[I]);

  Dest.Seek(4);
  Dest.WriteCardinal(Ofs);

  ReleaseObject(Dest);
  ReleaseObject(Doc);

  Result := True;
End;

Function XMLConvertToBinary(SourceFile, DestFile, ConstantFile:TERRAString):Boolean;
Var
  Writer:XMLBinaryWriter;
Begin
  Writer := XMLBinaryWriter.Create();
  Result := Writer.Convert(SourceFile, DestFile, ConstantFile);
  ReleaseObject(Writer);
End;

End.
