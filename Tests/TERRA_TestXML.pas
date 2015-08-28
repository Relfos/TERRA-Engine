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
Uses TERRA_String, TERRA_Utils, TERRA_ObjectTree, TERRA_XML;


Procedure TERRAXML_TestSimple.Run;
Var
  S:TERRAString;
  Doc, Node:TERRAObjectNode;

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
  S := '<note><to>Tove</to><from>Jani</from><heading>Reminder</heading><body>Dont forget me this weekend!</body></note>';
  Doc := XMLNode.Create();
  Doc.LoadFromString(S, encodingUTF8);

  Node := Expect(Doc.Root, 'to', 'Tove');
  Node := Expect(Doc.Root, 'from', 'Jani');
  Node := Expect(Doc.Root, 'heading', 'Reminder');
  Node := Expect(Doc.Root, 'body', 'Dont forget me this weekend!');

  Doc.Release();
End;


Procedure TERRAXML_TestShortcuts.Run;
Var
  S:TERRAString;
  Doc, Node:TERRAObjectNode;

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
  Doc := XMLNode.Create();
  Doc.LoadFromString(S, encodingUTF8);

  Node := Expect(Doc.Root, 'x', '100');
  Node := Expect(Doc.Root, 'y', '200');

  Doc.Release();
End;


End.