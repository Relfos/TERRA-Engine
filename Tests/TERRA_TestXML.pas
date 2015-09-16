Unit TERRA_TestXML;

{$I terra.inc}

Interface
Uses TERRA_TestSuite;

Type
  TERRAXML_TestSimple = class(TestCase)
    Procedure Run; Override;
   End;

  TERRAXML_TestShortcuts = class(TestCase)
    Procedure Run; Override;
   End;

Implementation
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_ObjectTree, TERRA_XML;


Procedure TERRAXML_TestSimple.Run;
Var
  S:TERRAString;
  Doc:XMLFormat;
  Root, Header, Node:TERRAObjectNode;

  Function Expect(Root:TERRAObjectNode; Const Name, Value:TERRAString):TERRAObjectNode;
  Begin
    If Assigned(Root) Then
      Result := Root.GetChildByName(Name)
    Else
      Result := Nil;

    Check(Assigned(Result), 'Could not find node "'+Name+'"');

    If Assigned(Result) Then
      Check(Value = Result.Value, 'Expected value "'+Value+'" in node "'+Name+'" but got "'+Result.Value+'"');
  End;

Begin
  S := '<note><header><to>Tove</to><from>Jani</from><subject>Reminder</subject></header><body>Don''t forget me this weekend!</body></note>';
  Doc := XMLFormat.Create(TERRAObjectNode, 'xml');
  Root := TERRAObjectNode.Create();
  Doc.LoadFromString(Root, S, encodingUTF8);
  ReleaseObject(Doc);

  Header := Expect(Root, 'header', '');

  Node := Expect(Header, 'to', 'Tove');
  Node := Expect(Header, 'from', 'Jani');
  Node := Expect(Header, 'subject', 'Reminder');
  Node := Expect(Root, 'body', 'Don''t forget me this weekend!');

  ReleaseObject(Root);
End;


Procedure TERRAXML_TestShortcuts.Run;
Var
  S:TERRAString;
  Doc:XMLFormat;
  Root, Node:TERRAObjectNode;

  Function Expect(Root:TERRAObjectNode; Const Name, Value:TERRAString):TERRAObjectNode;
  Begin
    If Assigned(Root) Then
      Result := Root.GetChildByName(Name)
    Else
      Result := Nil;

    Check(Assigned(Result), 'Could not find node "'+Name+'"');

    If Assigned(Result) Then
      Check(Value = Result.Value, 'Expected value "'+Value+'" in node "'+Name+'" but got "'+Result.Value+'"');
  End;

Begin
  S := '<test x="100" y="200" />';
  Doc := XMLFormat.Create(TERRAObjectNode, 'xml');
  Root := TERRAObjectNode.Create();
  Doc.LoadFromString(Root, S, encodingUTF8);
  ReleaseObject(Doc);

  Node := Expect(Root, 'x', '100');
  Node := Expect(Root, 'y', '200');

  Root.Release();
End;


End.