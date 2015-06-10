Unit TERRA_Lexer;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Stream;

Type
  LexerMatchStatus = (lexerContinue, lexerAccept, lexerError, lexerDiscard);

  Lexer = Class;

  LexerToken = Class
    Value:TERRAString;
    ID:Cardinal;
    Index:Integer;
    Line:Integer;
    Column:Integer;

    Prev:LexerToken;
    Next:LexerToken;
  End;

  LexerPosition = Object
    Offset:Cardinal;
    Line:Cardinal;
    Column:Cardinal;

    Function Advance(Source:Stream):TERRAChar;
    Procedure Reset();
    Procedure Restore(Source:Stream; Const Other:LexerPosition);
  End;

  TokenMatcher = Class(TERRAObject)
    Protected
      _Length:Cardinal;
      _Token:Cardinal;
      _SucessValue:LexerMatchStatus;
      _Priority:Integer;

      _Mark:LexerPosition;

    Public
      Function Match(Const C:TERRAChar):Boolean; Virtual; Abstract;
      Procedure Reset; Virtual;

      Function IsComplete():Boolean; Virtual;

      Property Token:Cardinal Read _Token;
      Property Priority:Integer Read _Priority;
  End;

  CharTokenMatcher = Class(TokenMatcher)
    Protected
      _Value:TERRAChar;

    Public
      Constructor Create(Token:Cardinal; Const Value:TERRAChar; SucessValue:LexerMatchStatus);
      Function Match(Const C:TERRAChar):Boolean; Override;
  End;

  StringTokenMatcher = Class(TokenMatcher)
    Protected
      _Value:TERRAString;
      _Iterator:StringIterator;

    Public
      Constructor Create(Token:Cardinal; Const Value:TERRAString; SucessValue:LexerMatchStatus);
      Function Match(Const C:TERRAChar):Boolean; Override;
      Procedure Reset; Override;
      Function IsComplete():Boolean; Override;
  End;

  RegexTokenMatcher = Class(TokenMatcher)
    Protected
      _State:Integer;
      _Complete:Boolean;

    Public
      Constructor Create(Token:Cardinal; Priority:Integer; SucessValue:LexerMatchStatus);
      Procedure Reset; Override;

      Function IsComplete():Boolean; Override;
  End;

  TokenGroup = Class
    Protected
      _Lexer:Lexer;
      _FirstToken:LexerToken;
      _LastToken:LexerToken;

    Public
      Constructor Create(MyLexer:Lexer);
      Procedure Discard();

      Property FirstToken:LexerToken Read _FirstToken Write _FirstToken;
      Property LastToken:LexerToken Read _LastToken Write _LastToken;
  End;

  Lexer = Class(TERRAObject)
    Protected
      _Source:Stream;

      _FirstToken:LexerToken;
      _LastToken:LexerToken;
      _TokenCount:Integer;

      _CurrentValue:TERRAString;

      _PrevMark:LexerPosition;
      _CurrentMark:LexerPosition;
      _InitMark:LexerPosition;

      _Matchers:Array Of TokenMatcher;
      _MatcherCount:Integer;

      _ActiveMatchers:Array Of TokenMatcher;
      _ActiveCount:Integer;

      _AcceptedMatchers:Array Of TokenMatcher;
      _AcceptedCount:Integer;

      _RejectedMatchers:Array Of TokenMatcher;
      _RejectedCount:Integer;


      Function Match(C:TERRAChar; IgnoreCase:Boolean):LexerMatchStatus;

      Procedure ResetMatchers();

      Procedure AddMatcher(Matcher:TokenMatcher);

      Procedure DiscardToken();

    Public
      Procedure Reset(); Virtual;

      Procedure Release(); Override;

      Procedure Consume(Source:Stream; IgnoreCase:Boolean);

      Property TokenCount:Integer Read _TokenCount;
      Property FirstToken:LexerToken Read _FirstToken;
      Property LastToken:LexerToken Read _LastToken;
  End;

Implementation
Uses TERRA_Error, TERRA_Log;

{ TokenMatcher }
Function TokenMatcher.IsComplete:Boolean;
Begin
  Result := True;
End;

Procedure TokenMatcher.Reset;
Begin
End;

{ Lexer }
Procedure Lexer.Consume(Source: Stream; IgnoreCase:Boolean);
Var
  C:TERRAChar;
  M:LexerMatchStatus;
Begin
  If Source = Nil Then
    Exit;

  _Source := Source;

  _CurrentMark.Reset();

  ResetMatchers();

  While Not Source.EOF Do
  Begin
    _PrevMark := _CurrentMark;
    C := _CurrentMark.Advance(_Source);

    M := Match(C, IgnoreCase);

    If (M =lexerAccept) Then
      ResetMatchers()
    Else
    If (M = lexerDiscard) Then
    Begin
      DiscardToken();
      ResetMatchers();
    End Else
    If (M = lexerError) Then
      Break;
  End;

  C := 0;
  While (M = lexerContinue) Do
  Begin
    M := Match(C, IgnoreCase);
  End;

  _Source := Nil;

{1 - get next char
2 - test match for each possibility_value
3 - those that failed get discarded
 (just send them to the end of the list)
4 - check if any match is complete, if yes, generate a new token
5 - if all matches failed, generate error
6 - else, return to step 1}

End;

Procedure Lexer.ResetMatchers;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MatcherCount) Do
  Begin
    _Matchers[I].Reset();
    _Matchers[I]._Length := 0;

    _ActiveMatchers[I] := _Matchers[I];
  End;

  //_PrevMark := _TempMark;

  _ActiveCount := _MatcherCount;
  _AcceptedCount := 0;
  _RejectedCount := 0;

  _InitMark := _CurrentMark;
  _CurrentValue := '';
End;

Procedure Lexer.Reset;
Begin
  _TokenCount := 0;
End;

Function Lexer.Match(C: TERRAChar; IgnoreCase:Boolean):LexerMatchStatus;
Var
  I, J, K, Max, BestPriority:Integer;
  Temp:TokenMatcher;
  HadMatch:Boolean;
  Token:LexerToken;
  UC:TERRAChar;
Begin
  I := 0;

  If IgnoreCase Then
    UC := CharLower(C)
  Else
    UC := C;

  HadMatch := False;
  While I<_ActiveCount Do
  Begin
{    WriteLn('Pos: ',I);

    Write('Accepted: ');
    For J:=0 To Pred(_AcceptedCount) Do
      Write(_AcceptedMatchers[J]._Token,' ');
    WriteLn;

    Write('Active: ');
    For J:=0 To Pred(_ActiveCount) Do
      Write(_ActiveMatchers[J]._Token,' ');
    WriteLn;

    Write('Rejected: ');
    For J:=0 To Pred(_RejectedCount) Do
      Write(_RejectedMatchers[J]._Token,' ');
    WriteLn;

    WriteLn;}

    If _ActiveMatchers[I].Match(UC) Then
    Begin
      Inc(_ActiveMatchers[I]._Length);
      Inc(I);
      HadMatch := True;
      Continue;
    End;

    If (_ActiveMatchers[I]._Length>0) Then
    Begin
      _ActiveMatchers[I]._Mark := _PrevMark;

      _AcceptedMatchers[_AcceptedCount] := _ActiveMatchers[I];
      Inc(_AcceptedCount);
    End Else
    Begin
      _RejectedMatchers[_RejectedCount] := _ActiveMatchers[I];
      Inc(_RejectedCount);
    End;

    If (_ActiveCount>1) Then
      _ActiveMatchers[I] := _ActiveMatchers[Pred(_ActiveCount)];
    Dec(_ActiveCount);
  End;

  If (_ActiveCount > 1) Or (HadMatch) Then
  Begin
    StringAppendChar(_CurrentValue, C);
    Result := lexerContinue;
    Exit;
  End;

  If (_AcceptedCount>0) Then
  Begin
    K := _TokenCount;
    Inc(_TokenCount);


    Token := LexerToken.Create();
    Token.Value := '';
    Token.Prev := _LastToken;
    Token.Index := K;

    If _FirstToken = Nil Then
      _FirstToken := Token
    Else
      _LastToken.Next := Token;

    _LastToken := Token;

    J := -1;
    Max := 0;
    BestPriority := 0;
    For I:=0 To Pred(_AcceptedCount) Do
    If ((_AcceptedMatchers[I]._Length > Max)
    Or ((_AcceptedMatchers[I]._Length = Max) And (_AcceptedMatchers[I].Priority>BestPriority)))
    And (_AcceptedMatchers[I].IsComplete()) Then
    Begin
      J := I;
      BestPriority := _AcceptedMatchers[I].Priority;
      Max := _AcceptedMatchers[I]._Length;
    End;

    If (J<0) Then
    Begin
      RaiseError('all matchers failed, possible lexer bug?');
    End;

    Token.ID := _AcceptedMatchers[J]._Token;

{    If (_AcceptedMatchers[J]._Mark.Offset = _PrevMark.Offset) Then
        _AcceptedMatchers[J]._Mark.Offset := _Source.Size;}

    Result := _AcceptedMatchers[J]._SucessValue;

    _CurrentMark.Restore(_Source, _AcceptedMatchers[J]._Mark);

    Token.Line := _InitMark.Line;
    Token.Column := _InitMark.Column;
    Token.Value := StringCopy(_CurrentValue, 1, _AcceptedMatchers[J]._Length);

{    K := StringCharCount(_CurrentValue, NewLineChar);
    Max := StringLength(_CurrentValue);
    _CurrentMark.Line := _InitMark.Line;

    If (K>0) Then
    Begin
      Inc(_CurrentMark.Line, K);
      K := StringCharPosReverse(NewLineChar, _CurrentValue, False);
      _CurrentMark.Column := Succ(Max - K);
    End Else
    Begin
      _CurrentMark.Column := _InitMark.Column + Max;
    End;}

    {Write('Got ',_Tokens[K].Value,', (');
    For I:=0 To Pred(J) Do
      Write(CardinalToString(_Tokens[K].Ids[I]), ' ');
    WriteLn(')');}

    If Result = lexerAccept Then
      Log(logConsole, 'Lexer', 'Token '+CardinalToString(Token.ID)+' ('+IntToString(Token.Line)+':'+IntToString(Token.Column)+') : '+ Token.Value);
  End Else
  Begin
    Result := lexerError;
    RaiseError('Could not match anything with '+Chr(C));
  End;
End;


Procedure Lexer.AddMatcher(Matcher: TokenMatcher);
Begin
  Inc(_MatcherCount);
  SetLength(_Matchers, _MatcherCount);
  SetLength(_ActiveMatchers, _MatcherCount);
  SetLength(_AcceptedMatchers, _MatcherCount);
  SetLength(_RejectedMatchers, _MatcherCount);
  _Matchers[Pred(_MatcherCount)] := Matcher;
End;

Procedure Lexer.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MatcherCount) Do
    ReleaseObject(_Matchers[I]);
End;

Procedure Lexer.DiscardToken();
Var
  Token: LexerToken;
Begin
  Token := _LastToken;
  If Token = Nil Then
    Exit;

  If Assigned(Token.Prev) Then
  Begin
    Token.Prev.Next := Nil;
    _LastToken := Token.Prev;
  End Else
  Begin
    _LastToken := Nil;
    _FirstToken := Nil;
  End;

  ReleaseObject(Token);
End;

{ LexerPosition }
Function LexerPosition.Advance(Source: Stream): TERRAChar;
Begin
  Source.ReadChar(Result);

  Self.Offset := Source.Position;

  If Result = NewLineChar Then
  Begin
    Inc(Line);
    Column := 1;
  End Else
    Inc(Column);
End;

Procedure LexerPosition.Reset;
Begin
  Self.Offset := 0;
  Self.Line := 1;
  Self.Column := 1;
End;

Procedure LexerPosition.Restore(Source:Stream; Const Other:LexerPosition);
Begin
  Self.Offset := Other.Offset;
  Self.Line := Other.Line;
  Self.Column := Other.Column;
  Source.Seek(Self.Offset);
End;

{ TokenGroup }
Constructor TokenGroup.Create(MyLexer: Lexer);
Begin
  Self._Lexer := MyLexer;
End;

Procedure TokenGroup.Discard;
Var
  Temp:LexerToken;
Begin
  Temp := _LastToken.Next;

  If Assigned(_FirstToken.Prev) Then
  Begin
    _FirstToken.Prev.Next := _LastToken.Next;

    If Assigned(_LastToken.Next) Then
      _LastToken.Next.Prev := _FirstToken.Prev;
  End Else
  Begin
    _Lexer._FirstToken := _LastToken.Next;
    If Assigned(_LastToken.Next) Then
      _LastToken.Next.Prev := Nil;
  End;
End;

{ CharTokenMatcher }
Constructor CharTokenMatcher.Create(Token:Cardinal; Const Value: TERRAChar; SucessValue:LexerMatchStatus);
Begin
  Self._Token := Token;
  Self._Value := Value;
  Self._SucessValue := SucessValue;
  Self._Priority := 5;
End;

Function CharTokenMatcher.Match(const C: TERRAChar):Boolean;
Begin
  Result := (C = _Value) And (_Length=0);
End;

{ StringTokenMatcher }
Constructor StringTokenMatcher.Create(Token:Cardinal; const Value: TERRAString; SucessValue:LexerMatchStatus);
Begin
  Self._Token := Token;
  Self._Value := Value;
  Self._SucessValue := SucessValue;
  Self._Priority := 10;
  StringCreateIterator(Self._Value, _Iterator);
End;

Procedure StringTokenMatcher.Reset;
Begin
  _Iterator.Reset();
End;

Function StringTokenMatcher.Match(const C: TERRAChar):Boolean;
Begin
  Result := (C = _Iterator.GetNext());
End;

Function StringTokenMatcher.IsComplete: Boolean;
Begin
  Result := _Length>=StringLength(Self._Value);
End;

{ RegexTokenMatcher }
Constructor RegexTokenMatcher.Create(Token: Cardinal; Priority:Integer; SucessValue:LexerMatchStatus);
Begin
  Self._Token := Token;
  Self._SucessValue := SucessValue;
  Self._Priority := Priority;
End;

Function RegexTokenMatcher.IsComplete: Boolean;
Begin
  Result := _Complete;
End;

Procedure RegexTokenMatcher.Reset;
Begin
  _Complete := False;
  _State := 0;
End;

End.
