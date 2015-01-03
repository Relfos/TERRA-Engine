Unit BindingPas;

Interface
Uses TERRA_Utils, TERRA_OS, TERRA_IO, TERRA_FileIO, BindingGenerator;

Type
  PasGenerator = Class(APIGenerator)
    Function OutputParams(F:PFunc):String;
    Function ConvertType(T:String; IsVar:Boolean):String;
    Function GetStaticDecls():String;
    Procedure Execute(); Override;
  End;

Implementation

Function PasGenerator.ConvertType(T:String; IsVar:Boolean):String;
Begin
  If (Pos('Callback', T)>0) And (Pos('TERRA', T)<=0) Then
    T := 'TERRA'+T;
    
  Result := T;
End;

Function PasGenerator.OutputParams(F:PFunc):String;
Var
  I:Integer;
Begin
  Result := '';
  For I:=0 To Pred(F.ArgCount) Do
  Begin
    Result := Result + F.Args[I].Name+':'+ConvertType(F.Args[I].VarType, F.Args[I].IsVar);

    If I<Pred(F.ArgCount) Then
      Result := Result +'; ';
  End;
End;

Function PasGenerator.GetStaticDecls: String;
Var
  I:Integer;
  F:PFunc;
  S:AnsiString;
Begin
  Result := '';

  For I:=0 To Pred(FuncCount) Do
  Begin
    F := @(Funcs[I]);

    If (Funcs[I].Return<>'') Then
      S := 'Function'
    Else
      S := 'Procedure';

    Result := Result + S +' '+Funcs[I].Name+'('+OutputParams(F)+')';

    If (Funcs[I].Return<>'') Then
      Result := Result + ':'+ConvertType(Funcs[I].Return, False);

    Result := Result + '; External TERRA_LIB;' + CrLf;
  End;

  Result := Result + CrLf;
End;

procedure PasGenerator.Execute();
Var
  Template:String;
  Dest:Stream;
  Date:TERRADate;
begin
  Date := GetCurrentDate();

  Template := ReadFile('templates\template.pas');

  Dest := FileStream.Create(TargetName + '.pas');

  ReplaceText('{$1}', Self.GetStaticDecls(), Template);

  ReplaceText('$DATE', IntToString(Date.Day)+ '/' + IntToString(Date.Month) + '/' + IntToString(Date.Year), Template);

  Dest.WriteLine(Template);
  Dest.Destroy;
End;

End.