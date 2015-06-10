Unit TERRA_Parser;

{$I terra.inc}

Interface
Uses SysUtils, TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Lexer;

Type
  ParsingException = Class(Exception)
    Protected
      _Line:Integer;
      _Column:Integer;

    Public
      Constructor Create(Msg:TERRAString;  Line, Column:Integer);
      Property Line:Integer Read _Line;
      Property Column:Integer Read _Column;
  End;

  ASTNode = Class(TERRAObject)
    Protected
      _Parent:ASTNode;
      _Children:Array Of ASTNode;
      _ChildrenCount:Integer;

      Procedure AddChild(Node:ASTNode);
      Procedure SetParent(const Value: ASTNode);

    Public
      Function GetChildByIndex(Index:Integer):ASTNode;

      Function GetValue():TERRAString; Virtual;

      Property ChildrenCount:Integer Read _ChildrenCount;
      Property Parent:ASTNode Read _Parent Write SetParent;
  End;

  ASTNodeClass = Class Of ASTNode;

  Parser = Class(TERRAObject)
    Protected
      _Lexer:Lexer;
      _TokenInit:Boolean;

      _OffsetToken:LexerToken;
      _PreviousToken:LexerToken;
      _CurrentToken:LexerToken;

      Function ParseGoal():ASTNode; Virtual; Abstract;

      Function GetTokenName(ID:Cardinal; Const Value:TERRAString = ''):TERRAString; Virtual; Abstract;

    Public
      Function Parse(Source:Stream; IgnoreCase:Boolean):ASTNode; Virtual;
      Procedure Release; Override;

      Function NextToken():Parser;
      Function ExpectToken(CurrentNode:ASTNodeClass; IsOpt:Boolean; TokenID:Cardinal):Boolean;
      //Function ExpectNode(Node:ASTNode; NodeClass:ASTNodeClass):ASTNode;

      Function TokenValue():TERRAString;

      Procedure ParsingExceptedError(CurrentNode:ASTNodeClass; Const Expected: TERRAString);

      Property CurrentToken:LexerToken Read _CurrentToken Write _CurrentToken;
  End;

Implementation
Uses TERRA_Error;

{ ParsingException }
Constructor ParsingException.Create(Msg: TERRAString; Line, Column: Integer);
Begin
  Msg := Msg + ' (Line '+IntToString(Line)+', Column '+IntToString(Column)+')';

  Inherited Create(Msg);
  Self._Line := Line;
  Self._Column := Column;
End;

{ ASTNode }
Procedure ASTNode.AddChild(Node: ASTNode);
Begin
  Inc(_ChildrenCount);
  SetLength(_Children, _ChildrenCount);
  _Children[Pred(_ChildrenCount)] := Node;
End;

Function ASTNode.GetChildByIndex(Index: Integer): ASTNode;
Begin
  If (Index<0) Or (Index>=_ChildrenCount) Then
    Result := Nil
  Else
    Result := _Children[Index];
End;

Function ASTNode.GetValue: TERRAString;
Begin
  Result := '';
End;

Procedure ASTNode.SetParent(const Value: ASTNode);
Begin
  _Parent := Value;

  If Assigned(Parent) Then
    Parent.AddChild(Self);
End;

{ Parser }
Function Parser.ExpectToken(CurrentNode:ASTNodeClass; IsOpt:Boolean; TokenID: Cardinal):Boolean;
Begin
//  WriteLn('Expect ',TokenID);
  Result := (_CurrentToken.ID = TokenID);

  If (Not Result) And (Not IsOpt) Then
  Begin
    CardinalToString(_CurrentToken.ID + TokenID);
    //(_Lexer.IsToken(_CurrentToken, TokenID);
    Self.ParsingExceptedError(CurrentNode, 'token '+GetTokenName(TokenID, '')+' but got '+ GetTokenName(_CurrentToken.ID, _CurrentToken.Value));
  End;
End;

Function Parser.TokenValue: TERRAString;
Begin
  If Assigned(_CurrentToken) Then
    Result := _CurrentToken.Value
  Else
    Result := '';
End;

Function Parser.NextToken: Parser;
Begin
  _PreviousToken := _CurrentToken;
  If (_CurrentToken = Nil) Then
  Begin
    If _TokenInit Then
    Begin
      _CurrentToken := _Lexer.FirstToken;
      _TokenInit := False;
    End;
  End Else
    _CurrentToken := _CurrentToken.Next;
  Result := Self;
End;

Function Parser.Parse(Source:Stream; IgnoreCase:Boolean):ASTNode;
Var
  Group:TokenGroup;
Begin
  _Lexer.Reset();
  _Lexer.Consume(Source, IgnoreCase);

{  Group := TokenGroup.Create(_Lexer);
  Group.FirstToken := _Lexer.FirstToken;
  Group.LastToken := _Lexer.LastToken;
  ReleaseObject(Group);}

  _TokenInit := True;
  _CurrentToken := Nil;
  Result := ParseGoal();
End;

Procedure Parser.Release;
Begin
  ReleaseObject(_Lexer);
End;

Procedure Parser.ParsingExceptedError(CurrentNode:ASTNodeClass; Const Expected: TERRAString);
Var
  Msg:TERRAString;
Begin
  Msg := 'Error in '+CurrentNode.ClassName +'. Expected '+Expected;
  Raise ParsingException.Create(Msg, _CurrentToken.Line, _CurrentToken.Column);
End;

End.
