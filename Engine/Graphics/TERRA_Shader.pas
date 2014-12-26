{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_Shader
 * Implements Shader resource class
 ***********************************************************************************************************************
}

Unit TERRA_Shader;
{$I terra.inc}

  // If enabled, shadercode is written to txt file
{$IFNDEF PC}
{$UNDEF DEBUG_SHADERS}
{$ENDIF}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Collections, TERRA_IO,
  TERRA_Application, TERRA_ResourceManager, TERRA_Resource,
  TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Plane,
  TERRA_Color, TERRA_Matrix4x4, TERRA_Matrix3x3, TERRA_FileManager, SysUtils;

Type
  ShaderAttribute = Record
    Name:AnsiString;
    Handle:Integer;
  End;

  Shader = Class (Resource)
    Protected
      _VertexCode:AnsiString;
	    _FragmentCode:AnsiString;

	    _VertexShaderHandle:Cardinal;
	    _FragmentShaderHandle:Cardinal;

      _Program:Cardinal;
      _Linked:Boolean;
      _MRT:Boolean;

      _Attributes:Array Of ShaderAttribute;
      _AttributeCount:Integer;

      Procedure ProcessIncludes(Var S:AnsiString; IncludedList:List);

      Procedure UniformError(Const Name:AnsiString);

      Procedure AddAttributes(Source:AnsiString);

      Function CompileShader(Source:AnsiString; ShaderType:Cardinal; Var Shader:Cardinal):Boolean;
      Function LinkProgram:Boolean;

			Procedure Bind();
      Procedure Unbind();

    Public
      Defines:AnsiString;

      Constructor CreateFromString(Source:AnsiString; Name:AnsiString = ''; Defines:AnsiString = '');

      Function Load(Source:Stream):Boolean; Override;
      Function Unload:Boolean; Override;
      Function Update:Boolean; Override;
      Procedure OnContextLost; Override;

      Function GetAttribute(Name:AnsiString):Integer;

      Class Function GetManager:Pointer; Override;

			Procedure SetUniform(Const Name:AnsiString; Const Value:Integer); Overload;
			Procedure SetUniform(Const Name:AnsiString; Const Value:Single); Overload;
			Procedure SetUniform(Const Name:AnsiString; Const Value:Vector2D); Overload;
			Procedure SetUniform(Const Name:AnsiString; const Value:Vector3D); Overload;
			Procedure SetUniform(Const Name:AnsiString; const Value:Vector4D); Overload;
			Procedure SetUniform(Const Name:AnsiString; const Value:Plane); Overload;
			Procedure SetUniform(Const Name:AnsiString; Const Value:Color); Overload;
      Procedure SetUniform(Const Name:AnsiString; Value:Matrix4x4); Overload;
      Procedure SetUniform(Const Name:AnsiString; Const Value:Matrix3x3); Overload;

      Function GetUniform(Const Name:AnsiString):Integer;

      Function HasUniform(Const Name:AnsiString):Boolean;

      Property Handle:Cardinal Read _Program;
      Property MRT:Boolean Read _MRT;
  End;

  ShaderManager = Class(ResourceManager)
    Protected
      _ActiveShader:Shader;

    Public
      Destructor Destroy; Override;

      Class Function Instance:ShaderManager;

			Procedure Bind(MyShader:Shader);

      Function GetShader(Name:AnsiString; ValidateError:Boolean = True):Shader;

      Procedure AddShader(MyShader:Shader);
      Procedure DeleteShader(MyShader:Shader);

      Procedure InvalidateShaders;

      Property ActiveShader:Shader Read _ActiveShader;
  End;

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Log, TERRA_GraphicsManager, TERRA_FileUtils, TERRA_FileIO;

Var
  _ShaderManager_Instance:ApplicationObject = Nil;

  {
Class Function Shader.ActiveShader:Shader;
Begin
  Result := _ActiveShader;
  If (Assigned(Result)) And (Result.Handle<=0) Then
    Result := Nil;
End;}

Procedure Shader.UniformError(Const Name:AnsiString);
Begin
  {$IFDEF PC}
  //  Log(logWarning, 'Shader', 'Invalid uniform: '+Name+' in '+Self._Name);
  {$ENDIF}
End;

Function Shader.HasUniform(Const Name:AnsiString):Boolean;
Begin
  Result := GetUniform(Name)>=0;
End;

Function Shader.GetUniform(Const Name:AnsiString):Integer;
Begin
  Result := glGetUniformLocation(_Program, PAnsiChar(Name));
End;

Constructor Shader.CreateFromString(Source, Name, Defines:AnsiString);
Var
  MyStream:MemoryStream;
Begin
  Log(logDebug, 'Shader', 'Creating shader from string: '+ Name);
  Self.Defines := Defines;
  MyStream := MemoryStream.Create(Length(Source), @(Source[1]));
  If Name = '' Then
    Name := 'custom'+HexStr(GetTime);
  _Name := Name;

  If Load(MyStream) Then
  Begin
    _Status := rsReady;
    Self.Update;
  End Else
    _Status := rsInvalid;

  Log(logDebug, 'Shader', 'Destroying temp stream');
  MyStream.Destroy;

  Log(logDebug, 'Shader', 'Shader loaded ok!');
End;


Function Shader.GetAttribute(Name:AnsiString):Integer;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_AttributeCount) Do
  If (_Attributes[I].Name = Name) Then
  Begin
    Result := _Attributes[I].Handle;
    Exit;
  End;

//  Log(logError, 'Shader', 'Attribute '+Name+' not found in shader '+_Name);
  Result := -1;
End;

// Shader
Procedure Shader.ProcessIncludes(Var S:AnsiString; IncludedList:List);
Var
  Lib:StringObject;
  S2, S3, Content, LibPath:AnsiString;
  I, J, K:Integer;
  Temp:Stream;
Begin
  K := Pos('#MATERIAL', S);
  If (K>0) Then
  Begin
    ReplaceText('#MATERIAL', 'phong', S);
  End;

  Repeat
    S2 := UpStr(S);
    I := Pos('INCLUDE(', S2);
    If (I>0) Then
    Begin
      S2 := Copy(S2, I+9, MaxInt);
      J := Pos(')', S2);
      S3 := Copy(S2,1, J-2);
      LibPath := FileManager.Instance().SearchResourceFile('lib_' + S3 + '.glsl');
      If (LibPath='') Then
        LibPath := FileManager.Instance().SearchResourceFile('mat_' + LowStr(S3) + '.glsl');

      If (LibPath<>'') Then
      Begin
        Lib := StringObject.Create(UpStr(GetFileName(LibPath, True)));

        If (Not IncludedList.ContainsDuplicate(Lib)) Then
        Begin
          IncludedList.Add(Lib);

          Temp := FileManager.Instance().OpenFileStream(LibPath);
          SetLength(Content, Temp.Size);
          Temp.Read(@Content[1], Temp.Size);
          Temp.Destroy;

          ProcessIncludes(Content, IncludedList);
        End Else
          Lib.Destroy();
      End Else
      Begin
        RaiseError('Shader '+Self.Name+', library not found: '+ S3);
        S3 := '';
      End;

      Inc(J, I+10);
      S2 := Copy(S, J, MaxInt);
      S := Copy(S, 1, Pred(I));
      S := S + crLf + S3 +  S2;
    end;
  Until I<=0;
End;

Procedure Shader.AddAttributes(Source:AnsiString);
Var
  I:Integer;
  S, S2:AnsiString;
Begin
  _AttributeCount := 0;
  S := Source;
  ReplaceText('gl_Position', 'IGNORE',S);
  If (Pos('gl_', S)>0) Then
  Begin
     Log(logWarning, 'Shader', 'The following shader has deprecated attributes: '+_Name);
  End;

  Repeat
    I := Pos('attribute', Source);
    If (I<=0) Then
      Break;

    Source := Copy(Source, I + 10, MaxInt);
    S := GetNextWord(Source, ' ');      // type
    S := UpStr(S);
    If (S='HIGHP') Or (S='LOWP') Or (S='MEDIUMP') Then
      S := GetNextWord(Source, ' ');      // type
    S2 := GetNextWord(Source, ';');      // name

    Inc(_AttributeCount);
    SetLength(_Attributes, _AttributeCount);
    _Attributes[Pred(_AttributeCount)].Name := S2;
    _Attributes[Pred(_AttributeCount)].Handle := -1;
  Until (Source='');

  If (_AttributeCount<=0) Then
  Begin
    Log(logWarning, 'Shader', 'The following shader has no attributes: '+_Name);
  End;
End;

Function Shader.Load(Source:Stream):Boolean;
Var
  S:AnsiString;
  Version:AnsiString;

Function ReadBlock(Name:AnsiString):AnsiString;
Var
  S2:AnsiString;
  I:Integer;
  N:Integer;
Begin
  I := Pos(UpStr(Name), UpStr(S));
  If (I>0) Then
  Begin
    S2 := Copy(S, I +1, MaxInt);
    I := Pos('{', S2);
    S2 := Copy(S2, I+1, MaxInt);
    I := 1;
    N := 0;
    Repeat
      If (S2[I]='}') Then
      Begin
        If (N=0) Then
          Break
        Else
          Dec(N);
      End Else
      If (S2[I]='{') Then
        Inc(N);
      Inc(I);
    Until (I>=Length(S2));

    S := Copy(S2, I + 1, MaxInt);
    S2 := Copy(S2, 1, I-1);
    Result := S2;
  End Else
    Result := '';

  Result := TrimLeft(TrimRight(Result));
End;

Function PreProcess(Source:AnsiString):AnsiString;
Var
  I,J,K:Integer;
  Found:Boolean;
  S,S2,S3, SB, SN:AnsiString;
  Defines:AnsiString;
Begin
  Defines := '';
  Repeat
    I := Pos('#DEFINE', UpStr(Source));
    If (I<=0) Then
      Break;

    SB := Copy(Source, 1, Pred(I));
    S := Copy(Source, Succ(I), MaxInt);
    J := Pos(#10, S);
    SN := Copy(S, Succ(J), MaxInt);
    S := Copy(S, 1, Pred(J));
    J := Pos(' ', S);
    S2 := UpStr(Copy(S, 1, Pred(J)));

    S := Copy(S, Succ(J), MaxInt);
    S := TrimRight(S);
    Defines := Defines + UpStr(S) +',';
    Source := SB + SN;
  Until (False);

  Repeat
    I := Pos('#UNDEF', UpStr(Source));
    If (I<=0) Then
      Break;

    SB := Copy(Source, 1, Pred(I));
    S := Copy(Source, Succ(I), MaxInt);
    J := Pos(#10, S);
    SN := Copy(S, Succ(J), MaxInt);
    S := Copy(S, 1, Pred(J));
    J := Pos(' ', S);
    S2 := UpStr(Copy(S, 1, Pred(J)));

    S := Copy(S, Succ(J), MaxInt);
    S := TrimRight(S);
    ReplaceText(UpStr(S)+',', '', Defines);
    Source := SB + SN;
  Until (False);

  Repeat
    I := PosRev('#IF', UpStr(Source));
    If (I<=0) Then
      Break;
    SB := Copy(Source, 1, Pred(I));
    S := Copy(Source, Succ(I), MaxInt);
    J := Pos(#10, S);
    SN := Copy(S, Succ(J), MaxInt);
    S := Copy(S, 1, Pred(J));
    J := Pos(' ', S);
    S2 := UpStr(Copy(S, 1, Pred(J)));

    S := Copy(S, Succ(J), MaxInt);
    S := UpStr(TrimRight(S));
    If (S2='IFNDEF') Then
      Found := Pos(S +',', Defines)<=0
    Else
      Found := Pos(S +',', Defines)>0;

    J := Pos('#ELSE', UpStr(SN));
    K := Pos('#ENDIF', UpStr(SN));

    If (J>0) And (J<K) Then // #else found
    Begin
      S := Copy(SN, 1, Pred(J));
      S2 := TrimLeft(Copy(SN, J+6, K-J-6));
      SN := Copy(SN, K+7, MaxInt);
    End Else
    Begin
      S := Copy(SN, 1, Pred(K));
      SN := Copy(SN, K+7, MaxInt);
      S2 := '';
    End;

    If (Found) Then
      S3 := TrimRight(S)
    Else
      S3 := TrimRight(S2);

    SN := S3 + SN;
    Source := SB + SN;
  Until (False);

  Result := Source;
End;

Var
  I:Integer;
  IncludedList:List;
  HasGLSL120:Boolean;
  S1, S2:AnsiString;
Begin
  SetLength(S, Source.Size);
  Source.Read(@S[1], Source.Size);

  IncludedList := List.Create(coAppend);
  ProcessIncludes(S, IncludedList);
  IncludedList.Destroy;

  HasGLSL120 := (GraphicsManager.Instance().Version.Major>=1.0) And (GraphicsManager.Instance().Version.Minor>=20);

  Version := ReadBlock('version');
  {$IFDEF PC}
  If (Version <>'') And (HasGLSL120) Then
    Version := '#version '+ Version + crLf
  Else
  {$ENDIF}
    Version := '';

  Version := Version + '#define ' + GraphicsManager.Instance().Vendor + crLf;
  If (HasGLSL120) Then
    Version := Version + '#define MATRIX_CAST' + crLf;

  If (GraphicsManager.Instance().Settings.PostProcessing.Avaliable) Then
    Version := Version + '#define POSTPROCESSING' + crLf;

  If (GraphicsManager.Instance().Settings.NormalMapping.Enabled) Then
    Version := Version + '#define NORMAL_MAPPING' + crLf;

  If (GraphicsManager.Instance().Settings.FloatTexture.Avaliable) Then
    Version := Version + '#define FLOATBUFFERS' + crLf;

  If (GraphicsManager.Instance().Settings.DepthOfField.Enabled) Then
    Version := Version + '#define DEPTHOFFIELD' + crLf;

  If (GraphicsManager.Instance().Settings.ShadowSplitCount>1) Then
    Version := Version + '#define SHADOWSPLIT1' + crLf;
  If (GraphicsManager.Instance().Settings.ShadowSplitCount>2) Then
    Version := Version + '#define SHADOWSPLIT2' + crLf;
  If (GraphicsManager.Instance().Settings.ShadowSplitCount>3) Then
    Version := Version + '#define SHADOWSPLIT3' + crLf;

  S1 := Self.Defines;
  While (S1<>'') Do
  Begin
    S2 := GetNextWord(S1,';');
    If (S2<>'') Then
      Version := Version + '#define ' + S2 + crLf;
  End;

	_VertexCode := PreProcess(Version + ReadBlock('vertex'));
	_FragmentCode := PreProcess(Version + ReadBlock('fragment'));

  _MRT := Pos('gl_FragData', _FragmentCode)>0;

	Result := True;
End;

Function Shader.Update:Boolean;
Begin
  Inherited Update();
  
  _AttributeCount := 0;
  If (_VertexCode ='') Or (_FragmentCode ='') Then
  Begin
    Result := False;
    Exit;
  End;

  Log(logDebug, 'Shader', 'Compiling vertex code for ' +_Name);

  _Linked := False;
  Result := CompileShader(_VertexCode, GL_VERTEX_SHADER, _VertexShaderHandle);
  If Not Result Then
    Exit;

  Log(logDebug, 'Shader', 'Compiling fragment code for ' +_Name);

  Result := CompileShader(_FragmentCode, GL_FRAGMENT_SHADER, _FragmentShaderHandle);
  If Not Result Then
    Exit;

  Log(logDebug, 'Shader', 'Linking ' +_Name);
  If _Name = 'Font' Then
    IntToString(2);
  Result := LinkProgram;
  Log(logDebug, 'Shader', 'Finished linking ' +_Name+', result='+BoolToString(Result));
End;

Procedure Shader.OnContextLost;
Begin
  If (ShaderManager.Instance._ActiveShader = Self) Then
    ShaderManager.Instance._ActiveShader := Nil;

  _VertexShaderHandle := 0;
  _FragmentShaderHandle := 0;
  _Program := 0;

  _AttributeCount := 0;
  _Status := rsUnloaded;
  _Linked := False;
End;

Function Shader.Unload:Boolean;
Begin
  If (_ShaderManager_Instance<>Nil) And (ShaderManager(_ShaderManager_Instance.Instance)._ActiveShader = Self) Then
    ShaderManager(_ShaderManager_Instance.Instance)._ActiveShader := Nil;

  If (_Program>0) Then
  Begin
    If (_VertexShaderHandle>0) Then
    Begin
      If (Self._ContextID = Application.Instance.ContextID) Then
      Begin
        glDetachShader(_Program, _VertexShaderHandle);
        glDeleteShader(_VertexShaderHandle);
      End;

      _VertexShaderHandle := 0;
    End;

    If (_FragmentShaderHandle>0) Then
    Begin
      If (Self._ContextID = Application.Instance.ContextID) Then
      Begin
        glDetachShader(_Program, _FragmentShaderHandle);
        glDeleteShader(_FragmentShaderHandle);
      End;

      _FragmentShaderHandle := 0;
    End;

    If (Self._ContextID = Application.Instance.ContextID) Then
    Begin
      glDeleteProgram(_Program);
    End;
    
    _Program := 0;
  End;

  _AttributeCount := 0;
  _Status := rsUnloaded;
  _Linked := False;
	Result := True;
End;

Function Shader.CompileShader(Source:AnsiString; ShaderType:Cardinal; Var Shader:Cardinal):Boolean;
Var
  ShaderCode:PAnsiChar;
  CompileStatus, ShaderLength:Integer;
  LogInfo,PS:AnsiString;
  LogLength,slen:Integer;
  Dest:FileStream;
  {$IFDEF DEBUG_SHADERS}
  FileName:AnsiString;
  {$ENDIF}
Begin
  Result := False;
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;
(*
  ReplaceAllText(';', '@'+crLf, Source);
  ReplaceAllText('@', ';', Source);
  ReplaceAllText('{', crLf+'@'+crLf, Source);
  ReplaceAllText('@', '{', Source);
  ReplaceAllText('}', '@'+crLf+crLf, Source);
  ReplaceAllText('@', '}', Source);
*)
  {$IFDEF PC}
  ReplaceText('highp', '', Source);
  ReplaceText('lowp', '', Source);
  ReplaceText('mediump', '', Source);
  {$ENDIF}

  // Create shader
  ShaderCode := PAnsiChar(Source);
  ShaderLength := Length(Source);

  Shader := glCreateShader(ShaderType);  

  glShaderSource(Shader,1 ,@ShaderCode, @ShaderLength);   
  glCompileShader(Shader);                                
  glGetShaderiv(Shader, GL_COMPILE_STATUS, @CompileStatus);  

  {$IFDEF DEBUG_SHADERS}
  FileName := LowStr(Name);
  If ShaderType=GL_VERTEX_SHADER Then
    FileName:=FileName+'.vs'
  Else
    FileName:=FileName+'.fs';

  {$IFDEF PC}
  CreateDir('Debug');
  FileName:='Debug'+PathSeparator+FileName+'.txt';
  {$ELSE}
  FileName:=Application.Instance.DocumentPath+PathSeparator+FileName+'.txt';
  {$ENDIF}
  Dest := FileStream.Create(FileName);
  Dest.WriteLine(Source);
  Dest.Destroy;
  {$ENDIF}

  glGetShaderiv(Shader, GL_INFO_LOG_LENGTH, @LogLength);  
  If LogLength > 1 Then
  Begin
    SetLength(LogInfo, LogLength);
    glGetShaderInfoLog(Shader, LogLength, @slen, @LogInfo[1]);    
    LogInfo := TrimRight(LogInfo);

    If ShaderType=GL_VERTEX_SHADER Then
      PS:='Vertex'
    Else
      PS:='Fragment';
    Log(logDebug,'Shader', LogInfo);
  End Else
    LogInfo:='';

  If CompileStatus=0 Then
  Begin
    If ShaderType=GL_VERTEX_SHADER Then
      PS:='Vertex'
    Else
      PS:='Fragment';

    ReplaceText('ERROR:','@', LogInfo);
    ReplaceText('@',crLf+'ERROR:', LogInfo);
    Delete(LogInfo, 1, Length(crLf));
    Log(logDebug,'Shader', ShaderCode);
    RaiseError(Name+'.'+PS+': ' + LogInfo);
    Result:=False;
  End Else
    Result:=True;
End;

Function Shader.LinkProgram:Boolean;
Var
  LinkStatus:Integer;
  LogInfo:AnsiString;
  LogLength,slen:Integer;
Begin
  Result := False;
  _Program := 0;
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

  // Create program
  _Program := glCreateProgram;                      
  glAttachShader(_Program, _VertexShaderHandle);    
  glAttachShader(_Program, _FragmentShaderHandle);  

  glLinkProgram(_Program);                                    
  glGetProgramiv(_Program, GL_LINK_STATUS, @LinkStatus);      
  glGetProgramiv(_Program, GL_INFO_LOG_LENGTH, @LogLength);   
  If LogLength > 1 Then
  Begin
    SetLength(LogInfo, LogLength);
    glGetProgramInfoLog(_Program, LogLength, @slen, @LogInfo[1]);
  End Else
    LogInfo := '';

  _Linked := (LinkStatus=1);
  If Not _Linked Then
  Begin
    RaiseError('Shader Linking failed.['+Name+']'+crLf+LogInfo);
    Exit;
  End;

  Result := True;
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Single);
Var
  ID:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := glGetUniformLocation(_Program, PAnsiChar(Name));
  If (ID>=0) Then
  Begin
	  glUniform1f(Id, Value);
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Integer);
Var
  ID:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform1i(Id, Value);
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Vector2D);
Var
  ID:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform2fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Vector3D);
Var
  ID:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform3fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Vector4D);
Var
  ID:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform4fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Plane);
Var
  ID:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform4fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Color);
Var
  ID:Integer;
  P:Array[0..3] Of Single;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
    P[0] := Value.R / 255.0;
    P[1] := Value.G / 255.0;
    P[2] := Value.B / 255.0;
    P[3] := Value.A / 255.0;
	  glUniform4fv(Id, 1, @(P[0]));
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Const Value:Matrix3x3);
Var
  ID:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
    glUniformMatrix3fv(Id, 1, False, @(Value.V[0]));
  End Else
    UniformError(Name);
End;

Procedure Shader.SetUniform(Const Name:AnsiString; Value:Matrix4x4);
Var
  ID:Integer;
  IsModelMatrix:Boolean;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
    {If (GraphicsManager.Instance().RenderStage = renderStageReflection) And (UpStr(Name)='MODELMATRIX') Then
      Value := MatrixMultiply4x3(GraphicsManager.Instance().ReflectionMatrix, Value);}

    If (GraphicsManager.Instance().RenderStage = renderStageReflection) Then
    Begin
      IsModelMatrix := False;

      If Length(Name) = 11 Then
      Begin
        IsModelMatrix := (UpCase(Name[1])='M') And (UpCase(Name[2])='O') And (UpCase(Name[3])='D') And (UpCase(Name[4])='E') And (UpCase(Name[5])='L')
           And (UpCase(Name[6])='M')  And (UpCase(Name[7])='A') And (UpCase(Name[8])='T');
      End;

      If IsModelMatrix Then
        Value := Matrix4x4Multiply4x3(GraphicsManager.Instance().ReflectionMatrix, Value);
    End;
      

    glUniformMatrix4fv(Id, 1, False, @(Value.V[0]));
  End Else
    UniformError(Name);
End;

{ ShaderManager }
Destructor ShaderManager.Destroy;
Begin
  Inherited;
  _ShaderManager_Instance := Nil;
End;

Class Function ShaderManager.Instance:ShaderManager;
Begin
  If Not Assigned(_ShaderManager_Instance) Then
    _ShaderManager_Instance := InitializeApplicationComponent(ShaderManager, Nil);

  Result := ShaderManager(_ShaderManager_Instance.Instance);
End;


Procedure Shader.Bind();
Var
  I:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

  {$IFDEF ANDROID}
  If (Not glIsProgram(_Program)) And (_Status = rsReady) Then
  Begin
    Self.Unload();
    Exit;
  End;
  {$ENDIF}

  {$IFDEF DEBUG_CALLSTACK}PushCallStack(Self.ClassType, 'Bind');{$ENDIF}

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Shader', 'Binding shader: '+ Self.Name);
  {$ENDIF}

  glUseProgram(_Program);

  If (_AttributeCount<=0) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'Shader', 'Adding attributes');
    {$ENDIF}

    AddAttributes(_VertexCode);
  End;

  For I:=0 To Pred(_AttributeCount) Do
  Begin
    If (_Attributes[I].Handle<0) Then
    Begin
      _Attributes[I].Handle := glGetAttribLocation(_Program, PAnsiChar(_Attributes[I].Name));
      If (_Attributes[I].Handle<0) Then
      Begin
      {$IFDEF DEBUG_GRAPHICS}
        Log(logError, 'Shader', 'Could not find attribute '+_Attributes[I].Name+' on shader: '+_Name);
      {$ENDIF}
        Continue;
      End;
    End;

    {$IFDEF DEBUG_GRAPHICS}
    Log(logDebug, 'Shader', 'Enabling attribarray '+_Attributes[I].Name);
    {$ENDIF}
    glEnableVertexAttribArray(_Attributes[I].Handle);
  End;

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Shader', 'End bind');
  {$ENDIF}

  {$IFDEF DEBUG_CALLSTACK}PopCallStack();{$ENDIF}
End;

Procedure Shader.Unbind();
Var
  I:Integer;
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

  For I:=0 To Pred(_AttributeCount) Do
  If (_Attributes[I].Handle>=0) Then
    glDisableVertexAttribArray(_Attributes[I].Handle);
End;

Procedure ShaderManager.Bind(MyShader:Shader);
Begin
  If (Not GraphicsManager.Instance().Settings.Shaders.Avaliable) Then
    Exit;

  If (MyShader =_ActiveShader) And (MyShader.IsReady) And (MyShader._Linked) Then
    Exit;

  If Assigned(_ActiveShader) Then
    _ActiveShader.Unbind;

  If (Assigned(MyShader)) And (MyShader.IsReady) And (MyShader._Linked) Then
  Begin
    MyShader.Bind;
    _ActiveShader := MyShader;
  End Else
  Begin
    glUseProgram(0);    
    _ActiveShader := Nil;
     Log(logWarning, 'Shader', 'No shader binded!');
  End;

  GraphicsManager.Instance().Internal(1, 1);
End;

Procedure ShaderManager.DeleteShader(MyShader: Shader);
Var
  S:Shader;
  I:Iterator;
Begin
  I := Self.Resources.CreateIterator;
  While I.HasNext Do
  Begin
    S := Shader(I.GetNext());
    If (S=MyShader) Then
    Begin
      I.Discard();
      Break;
    End;
  End;
  I.Destroy;
End;

Procedure ShaderManager.AddShader(MyShader: Shader);
Begin
  Log(logDebug, 'Shader', 'Registering shader: '+MyShader.Name);
  Self.AddResource(MyShader);
End;

Function ShaderManager.GetShader(Name:AnsiString; ValidateError:Boolean):Shader;
Var
  S:AnsiString;
Begin
  Name := TrimLeft(TrimRight(Name));
  If (Name='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := Shader(GetResource(Name));
  If (Not Assigned(Result)) Then
  Begin
    S := FileManager.Instance().SearchResourceFile(Name+'.glsl');
    If S<>'' Then
    Begin
      Result := Shader.Create(S);
      Result.Priority := 80;
      Self.AddResource(Result);
    End Else
    If ValidateError Then
      RaiseError('Could not find shader resource. ['+Name +']');
  End;
End;

Procedure ShaderManager.InvalidateShaders;
Var
  I:Iterator;
  MyResource:Resource;
Begin
  I := _Resources.CreateIterator;
  While (I.HasNext) Do
  Begin
    MyResource := Resource(I.GetNext());
    If (MyResource Is Shader) And (MyResource.Status = rsReady) Then
      MyResource.Unload();
  End;
  I.Destroy;
  _ActiveShader := Nil;
End;

Class Function Shader.GetManager: Pointer;
Begin
  Result := ShaderManager.Instance;
End;


Begin
  Log(logDebug, 'Shaders', 'Initializing');
  RegisterResourceClass(Shader);
End.