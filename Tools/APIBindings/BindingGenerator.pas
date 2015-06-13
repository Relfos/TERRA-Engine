Unit BindingGenerator;

{$I terra.inc}

Interface

uses TERRA_IO, TERRA_FileIO, TERRA_OS, TERRA_Utils, SysUtils;

Const
  TargetName = 'TERRA_Engine';

Type
  Arg = Record
    IsVar:Boolean;
    IsConst:Boolean;
    Name:String;
    VarType:String;
    Default:String;
  End;

  PFunc = ^ Func;
  Func = Record
    Name:String;
    Args:Array[0..64] Of Arg;
    ArgCount:Integer;
    Return:String;
  End;

  APIGenerator = Class
    Protected
      Funcs:Array Of Func;
      FuncCount:Integer;
      Code:AnsiString;

    Public
      Constructor Create();
      Function ReadFile(FileName:String):String;
      Procedure Execute();Virtual; Abstract;
  End;

Implementation

Constructor APIGenerator.Create;
Var
  I,J,K:Integer;
  Tag,Line,S,S2:String;
  IsFunction:Boolean;
  F:PFunc;

Procedure AddArg(S:String);
Var
  S2:String;
Begin
  S := Trim(S);

  If Pos(',',S)>0 Then
  Begin
    WriteLn('Error in function '+F.Name);
  End;


  If S='' Then
    Exit;

  Inc(F.ArgCount);
  S2 := Trim(GetNextWord(S,':'));

  F.Args[Pred(F.ArgCount)].IsVar := Pos('Var',S2)>0;
  F.Args[Pred(F.ArgCount)].IsConst := Pos('Const',S2)>0;

  If (F.Args[Pred(F.ArgCount)].IsVar) Or (F.Args[Pred(F.ArgCount)].IsConst ) Then
    GetNextWord(S2, ' ');

  F.Args[Pred(F.ArgCount)].Name := S2;


  F.Args[Pred(F.ArgCount)].VarType := Trim(GetNextWord(S, '='));
  F.Args[Pred(F.ArgCount)].Default := S;
End;
Begin
  Code := ReadFile(TargetName+'.dpr');

  // strip comments
  I := Pos('(*', Code);
  While I>0 Do
  Begin
    S := Copy(Code, 1, Pred(I));
    S2 := Copy(Code, I, MaxInt);
    I := Pos('*)', S2);
    S2 := Copy(S2, I, MaxInt);
    Code := S + S2;

    I := Pos('(*', Code);
  End;

  Tag := '{BEGIN_API}';
  I := Pos(Tag, Code);
  Code := Copy(Code, I+Length(Tag), MaxInt);

  Tag := '{END_API}';
  I := Pos(Tag, Code);
  Code := Copy(Code, 1, Pred(I));

  FuncCount := 0;
  While Code<>'' Do
  Begin
    I := Pos('Function', Code);
    J := Pos('Procedure', Code);

    If (I>0) And (J>0) And (J<I) Then
    Begin
      IsFunction := False;
      I := J;
    End Else
    Begin
      IsFunction := True;
    End;

    If I<=0 Then
      Break;

    Inc(FuncCount);
    SetLength(Funcs, FuncCount);
    F := @(Funcs[Pred(FuncCount)]);

    Code := Copy(Code, I, MaxInt);
    GetNextWord(Code, ' ');
    F.Name := GetNextWord(Code, '(');
    If Pos('Texture_UpdateRect',F.Name)>0 Then
      IntToString(2);
    Repeat
      I := Pos(';', Code);
      J := Pos(')', Code);

      If (I>0) And (J>0) And (J<I) Then
      Begin
        S := GetNextWord(Code, ')');

        AddArg(S);

        If IsFunction Then
        Begin
          GetNextWord(Code, ':');
          F.Return := GetNextWord(Code, ';');
        End;

        Break;
      End Else
      Begin
        S := GetNextWord(Code, ';');

        AddArg(S);
      End;
    Until False;
  End;
End;

Function APIGenerator.ReadFile(FileName:String):String;
Var
  Src:Stream;
  Line:AnsiString;
  I:Integer;
Begin
  Src := MemoryStream.Create(FileName);
  Result := '';
  While Not Src.EOF Do
  Begin
    Src.ReadLine(Line);
    Result := Result + Line + CrLf;
  End;
  Src.Destroy;
End;


End.