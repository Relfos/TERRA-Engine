Unit BindingCPP;

Interface
Uses TERRA_Utils, TERRA_OS, TERRA_IO, TERRA_FileIO, BindingGenerator;

Type
  CppGenerator = Class(APIGenerator)
    Function OutputParams(F:PFunc):String;
    Function ConvertType(T:String; IsVar:Boolean):String;
    Function GetDynamicDecls():String;
    Function GetStaticDecls():String;
    Function GetInit():String;
    Function GetImports():String;
    Procedure Execute(); Override;
  End;

Implementation

Function CppGenerator.ConvertType(T:String; IsVar:Boolean):String;
Var
  S:string;
Begin
  S := T;
  If (Pos('TERRA',S)>0)  Then
  Begin
    Result := T;
    Exit;
  End;

  If (Pos('Callback',S)>0) Or (Pos('Handler',S)>0) Then
  Begin
    Result := 'TERRA'+T;
    Exit;
  End;

  S := LowStr(S);

  If (S='color') Or (S='vector2d') Or (S='vector3d') Or (S='vector4d')
  Or (S='plane') Or (S='ray') Or (S='boundingbox') Or (S='frustum') Then
  Begin
    Result := T;
    Exit;
  End;

  If (S = 'matrix4x4') Then
    Result := 'Matrix4x4'
  Else
  If (S = 'matrix3x3') Then
    Result := 'Matrix3x3'
  Else
  If (S = 'integer') Then
    Result := 'INT32'
  Else
  If (S = 'cardinal') Then
    Result := 'UINT32'
  Else
  If (S = 'smallint') Then
    Result := 'INT16'
  Else
  If (S = 'word') Then
    Result := 'UINT16'
  Else
  If (S = 'shortint') Then
    Result := 'INT8'
  Else
  If (S = 'byte') Then
    Result := 'UINT8'
  Else
  If (S = 'integer') Then
    Result := 'INT32'
  Else
  If (S = 'single') Then
    Result := 'float'
  Else
  If (S = 'integer') Then
    Result := '__int32'
  Else
  If (S = 'ansichar') Then
    Result := 'char'
  Else
  If (S = 'boolean') Then
    Result := 'bool'
  Else
  If (S = 'pointer') Then
    Result := 'void*'
  Else
  If (S='') Then
    Result := 'void'
  Else
  If (S[1] = 'p') Then
  Begin
    Result := Self.ConvertType(Copy(T, 2, MaxInt), False)+'*';
    Exit;
  End Else
  Begin
    Result := S;
    WriteLn('Error: unknown type -> '+S);
  End;

  If IsVar Then
    Result := Result + '*';
End;

Function CppGenerator.OutputParams(F:PFunc):String;
Var
  I:Integer;
Begin
  Result := '';
  For I:=0 To Pred(F.ArgCount) Do
  Begin
    Result := Result + ConvertType(F.Args[I].VarType, F.Args[I].IsVar)+' '+F.Args[I].Name;

    If I<Pred(F.ArgCount) Then
      Result := Result +', ';
  End;
End;

Function CppGenerator.GetDynamicDecls: String;
Var
  I:Integer;
  F:PFunc;
Begin
  Result := '';

  For I:=0 To Pred(FuncCount) Do
  Begin
    F := @(Funcs[I]);

    Result := Result + 'typedef '+ConvertType(Funcs[I].Return, False)+' (FUNC_'+Funcs[I].Name+')('+OutputParams(F)+');' + CrLf;
  End;

    Result := Result + CrLf;

  For I:=0 To Pred(FuncCount) Do
  Begin
    F := @(Funcs[I]);

    Result := Result + 'FUNC_'+Funcs[I].Name+' *'+Funcs[I].Name+'=NULL;' + CrLf;
  End;
End;

Function CppGenerator.GetStaticDecls: String;
Var
  I:Integer;
  F:PFunc;
Begin
  Result := '';

  For I:=0 To Pred(FuncCount) Do
  Begin
    F := @(Funcs[I]);

    Result := Result + ' '+ConvertType(Funcs[I].Return, False)+' '+Funcs[I].Name+'('+OutputParams(F)+');' + CrLf;
  End;

    Result := Result + CrLf;

End;

Function CppGenerator.GetInit: String;
Var
  I:Integer;
  F:PFunc;
Begin
  Result := '';
  For I:=0 To Pred(FuncCount) Do
  Begin
    F := @(Funcs[I]);

    Result := Result +'  '+Funcs[I].Name+' = (FUNC_'+Funcs[I].Name+'*) GetProcAddress(terraDLL, "'+Funcs[I].Name+'");' + CrLf;
  End;

End;

Function CppGenerator.GetImports: String;
Var
  I:Integer;
  F:PFunc;
Begin
  Result := '';

  For I:=0 To Pred(FuncCount) Do
  Begin
    F := @(Funcs[I]);

    Result := Result + ConvertType(Funcs[I].Return, False)+' '+Funcs[I].Name+'('+OutputParams(F)+');' + CrLf;
  End;
End;

procedure CppGenerator.Execute();
Var
  Template:String;
  Dest:Stream;
begin
  Template := ReadFile('templates\template.cpp');

  Dest := FileStream.Create(TargetName + '.h');

  ReplaceText('$DYNAMIC_DECLARATIONS', Self.GetDynamicDecls(), Template);
  ReplaceText('$STATIC_DECLARATIONS', Self.GetStaticDecls(), Template);
  ReplaceText('$INITIALIZATION', Self.GetInit(), Template);

  Dest.WriteLine(Template);
  Dest.Destroy;
End;

End.