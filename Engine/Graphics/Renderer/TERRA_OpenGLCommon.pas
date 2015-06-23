Unit TERRA_OpenGLCommon;

{$I terra.inc}

{$DEFINE TRUE_VBO}

Interface
Uses
  TERRA_String, TERRA_Utils, TERRA_Renderer, TERRA_VertexFormat,
  {$IFDEF MOBILE}TERRA_OpenGLES{$ELSE}TERRA_OpenGL{$ENDIF}, 
  TERRA_Color, TERRA_Image, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D,
  TERRA_Matrix3x3, TERRA_Matrix4x4;

Type
  OpenGLVBO = Class(VertexBufferInterface)
    Protected
      _Handle:Integer;

      Procedure Submit(Wireframe:Boolean); Override;

    Public
      Procedure Release; Override;

      Procedure Invalidate; Override;

      Function Generate(Vertices:VertexData; IndexData, EdgeData:Pointer; TriangleCount:Integer; DynamicUsage:Boolean):Boolean; Override;

      Function Update(Data:PByte):Boolean; Override;
  End;

  OpenGLShader = Class(ShaderInterface)
    Protected
      _VertexCode:TERRAString;
	    _FragmentCode:TERRAString;

	    _VertexShaderHandle:Cardinal;
	    _FragmentShaderHandle:Cardinal;

      _Program:Cardinal;
      _Linked:Boolean;
      _MRT:Boolean;

      Procedure UniformError(Const Name:TERRAString);

      Function CompileShader(Const Name:TERRAString; Source:TERRAString; ShaderType: Cardinal; var Shader:Cardinal; AllowFallback:Boolean): Boolean;
      Function LinkProgram:Boolean;

      Function Load():Boolean;
      Function Reload():Boolean;
      Procedure Unload();

      Function Bind():Boolean; Override;
      Function Unbind():Boolean; Override;

    Public
      Function Generate(Const Name:TERRAString; ShaderCode:TERRAString):Boolean; Override;

      Function IsReady():Boolean; Override;

      Procedure Release(); Override;

			Procedure SetIntegerUniform(Const Name:TERRAString; Const Value:Integer); Override;
			Procedure SetFloatUniform(Const Name:TERRAString; Const Value:Single); Override;
			Procedure SetVec2Uniform(Const Name:TERRAString; Const Value:Vector2D); Override;
			Procedure SetVec3Uniform(Const Name:TERRAString; const Value:Vector3D); Override;
			Procedure SetVec4Uniform(Const Name:TERRAString; const Value:Vector4D); Override;
      Procedure SetMat3Uniform(Const Name:TERRAString; Value:Matrix3x3); Override;
      Procedure SetMat4Uniform(Const Name:TERRAString; Value:Matrix4x4); Override;
      Procedure SetVec4ArrayUniform(Const Name:TERRAString; Count:Integer; Values:PVector4D); Override;

      Function HasUniform(Const Name:TERRAString):Boolean; Override;

      Function GetUniform(Name:TERRAString):Integer; Override;
      Function GetAttributeHandle(Const Name:TERRAString):Integer; Override;

      Procedure Invalidate(); Override;
  End;

Function CompareToGL(Mode:CompareMode):Integer;
  
Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Log, TERRA_GraphicsManager, TERRA_FileManager, TERRA_FileUtils, TERRA_FileStream;

Function CompareToGL(Mode:CompareMode):Integer;
Begin
  Case Mode Of
  compareNever:       Result := GL_NEVER;
  compareLess:        Result := GL_LESS;
  compareEqual:       Result := GL_EQUAL;
  compareLessOrEqual: Result := GL_LEQUAL;
  compareGreater:     Result := GL_GREATER;
  compareDifferent:   Result := GL_NOTEQUAL;
  compareGreaterOrEqual: Result := GL_GEQUAL;
  Else
    //compareAlways
    Result := GL_ALWAYS;
  End;
End;

{ OpenGLVBO }
Function OpenGLVBO.Generate(Vertices:VertexData; IndexData, EdgeData:Pointer; TriangleCount:Integer; DynamicUsage:Boolean):Boolean;
Var
  Index:Single;
  I, N:Integer;
  Flags:Integer;
  P:Pointer;
Begin
  Self._Vertices := Vertices;
  Self._IndexList := IndexData;
  Self._EdgeList := EdgeData;
  Self._Dynamic := DynamicUsage;
  Self._TriangleCount := TriangleCount;
  Self._EdgeCount := 0;
  Self._WireframeIndices := Nil;

  If _Dynamic Then
    Flags := GL_DYNAMIC_DRAW
  Else
    Flags := GL_STATIC_DRAW;

  {$IFDEF TRUE_VBO}
  glGenBuffers(1, @_Handle);
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);
  glBufferData(GL_ARRAY_BUFFER, Vertices.Size * Vertices.Count, Vertices.Buffer, Flags);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  {$ENDIF}

  Result := True;
End;

Procedure OpenGLVBO.Invalidate;
Begin
  _Handle := 0;
End;

Procedure OpenGLVBO.Release;
Begin
  {$IFDEF TRUE_VBO}
  If (_Handle<>0) And (Self.IsValid()) Then
  Begin
    glDeleteBuffers(1, @_Handle);
  End;
  _Handle := 0;
  {$ENDIF}

  Inherited;
End;

Procedure OpenGLVBO.Submit(Wireframe:Boolean);
Var
  I:Integer;
Begin
  {$IFDEF TRUE_VBO}
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);
  {$ELSE}
  Ofs := Cardinal(_Buffer);
  {$ENDIF}

  _Vertices.Bind(False);

{
  For I:=0 To Pred(_AttributeCount) Do
  If (_Attributes[I].Handle>=0) Then
    glVertexAttribPointer(_Attributes[I].Handle, _Attributes[I].Count, DataFormatToGL(_Attributes[I].Format), _Attributes[I].Normalized, _VertexSize, Pointer(_Attributes[I].Offset));}


  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Mesh', 'glDrawElements: '+IntToString(_TriangleCount*3));
  {$ENDIF}

  If WireFrame Then
  Begin
    glDrawElements(GL_LINES, _EdgeCount * 2, GL_UNSIGNED_SHORT, @(_WireframeIndices[0]));
  End Else
  Begin
    glDrawElements(GL_TRIANGLES, _TriangleCount * 3, GL_UNSIGNED_SHORT, _IndexList);
  End;
//    GraphicsManager.Instance.Internal(0 , _TriangleCount);

  {$IFDEF TRUE_VBO}
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  {$ENDIF}
End;

(*Function VBO.Lock: Pointer;
Begin
{$IFDEF TRUE_VBO}
  If (_Handle<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;
{$ENDIF}

{$IFDEF MAPBUFFERS}
{$IFDEF TRUE_VBO}
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);                                   
  Result := glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
{$ENDIF}
{$ELSE}
  If Not Assigned(_Temp) Then
  Begin
    GetMem(_Temp, _VertexCount * _VertexSize);
    Move(_Buffer^, _Temp^, _VertexCount * _VertexSize);
  End;
  Result := _Temp;
{$ENDIF}
End;

Procedure VBO.Unlock;
Begin
  If (_Handle<=0) Then
    Exit;

{$IFDEF TRUE_VBO}
{$IFDEF MAPBUFFERS}
  glUnmapBuffer(GL_ARRAY_BUFFER);
{$ELSE}
  glBindBuffer(GL_ARRAY_BUFFER, _Handle);
  glBufferSubData(GL_ARRAY_BUFFER, 0, _VertexCount * _VertexSize, _Temp);
{$ENDIF}

  glBindBuffer(GL_ARRAY_BUFFER, 0);
{$ENDIF}
End;*)

Function OpenGLVBO.Update(Data:PByte):Boolean;
Begin
  If (_Handle<=0) Then
  Begin
    Result := False;
    Exit;
  End;

  glBindBuffer(GL_ARRAY_BUFFER, _Handle);
  glBufferSubData(GL_ARRAY_BUFFER, 0, _Vertices.Count * _Vertices.Size, Data);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  Result := True;
End;

{ OpenGLShader }
Function OpenGLShader.Bind:Boolean;
Var
  I:Integer;
Begin
  Result := False;

  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

  Result := (_Program>0) And (Self.IsValid());
  If Not Result Then
  Begin
    If Not Self.Reload() Then
    Begin
      glUseProgram(0);
      Exit;
    End;
  End;

  {$IFDEF DEBUG_GRAPHICS}
  Log(logDebug, 'Shader', 'Binding shader: ');
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

  Result := True;
End;

Function OpenGLShader.Unbind():Boolean;
Var
  I:Integer;
Begin
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
    glDisableVertexAttribArray(_Attributes[I].Handle);
  End;

  Result := True;
End;

Const
  FallbackVertexSource = 'void main(){ gl_Position = gl_Vertex;};';
  FallbackFragmentSource = 'void main(){gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);};';

Function OpenGLShader.CompileShader(Const Name:TERRAString; Source:TERRAString; ShaderType: Cardinal; var Shader:Cardinal; AllowFallback:Boolean): Boolean;
Var
  CompileStatus, ShaderLength:Integer;
  LogInfo,PS:TERRAString;
  LogLength,slen:Integer;
  Dest:FileStream;
  P:Pointer;
Begin
  Result := False;
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;


  {$IFDEF PC}
  Source := '#version 120'+CrLf + Source;

  StringReplaceText('highp', '', Source);
  StringReplaceText('lowp', '', Source);
  StringReplaceText('mediump', '', Source);
  {$ENDIF}

  // Create shader
  ShaderLength := Length(Source);

  Shader := glCreateShader(ShaderType);

  P := PAnsiChar(Source);
  glShaderSource(Shader,1 ,@P, @ShaderLength);
  glCompileShader(Shader);
  glGetShaderiv(Shader, GL_COMPILE_STATUS, @CompileStatus);

  glGetShaderiv(Shader, GL_INFO_LOG_LENGTH, @LogLength);
  If LogLength > 1 Then
  Begin
    SetLength(LogInfo, LogLength);
    glGetShaderInfoLog(Shader, LogLength, @slen, @LogInfo[1]);    
    LogInfo := StringTrimRight(LogInfo);

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

    StringReplaceText('ERROR:','@', LogInfo);
    StringReplaceText('@', StringFromChar(NewLineChar)+'ERROR:', LogInfo);
    //Delete(LogInfo, 1, Length(crLf));
    Log(logDebug,'Shader', Source);

    //RaiseError(Name+'.'+PS+': ' + LogInfo);
    If (AllowFallback) And (ShaderType = GL_FRAGMENT_SHADER) Then
    Begin
      Result := CompileShader(Name, FallbackFragmentSource, ShaderType, Shader, False);
    End Else
    If (AllowFallback) And (ShaderType = GL_VERTEX_SHADER) Then
    Begin
      Result := CompileShader(Name, FallbackVertexSource, ShaderType, Shader, False);
    End Else
      Result := False;
  End Else
    Result := True;
End;

Function OpenGLShader.LinkProgram: Boolean;
Var
  LinkStatus:Integer;
  LogInfo:TERRAString;
  LogLength,slen:Integer;
Begin
  Result := False;
  _Program := 0;
  If (Not _Owner.Features.Shaders.Avaliable) Then
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
    RaiseError('Shader Linking failed.['{+Name}+']'+StringFromChar(NewLineChar)+LogInfo);
    Exit;
  End;

  Result := True;
End;

Function OpenGLShader.GetAttributeHandle(const Name: TERRAString): Integer;
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

Function OpenGLShader.GetUniform(Name: TERRAString): Integer;
Begin
  Result := glGetUniformLocation(_Program, PAnsiChar(Name));
End;

Function OpenGLShader.HasUniform(const Name: TERRAString): Boolean;
Begin
  Result := GetUniform(Name)>=0;
End;

Procedure OpenGLShader.Invalidate;
Begin
  _VertexShaderHandle := 0;
  _FragmentShaderHandle := 0;
  _Program := 0;

  _AttributeCount := 0;
//  _Status := rsUnloaded;
  _Linked := False;
End;

Function OpenGLShader.IsReady: Boolean;
Begin
  Result := _Linked;
End;

Procedure OpenGLShader.UniformError(const Name: TERRAString);
Begin
  {$IFDEF PC}
//  Log(logWarning, 'Shader', 'Invalid uniform: '+Name+' in '+Self._Name);
  {$ENDIF}
End;


Procedure OpenGLShader.SetFloatUniform(const Name: TERRAString; const Value: Single);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := glGetUniformLocation(_Program, PAnsiChar(Name));
  If (ID>=0) Then
  Begin
	  glUniform1f(Id, Value);
  End Else
    UniformError(Name);
End;

Procedure OpenGLShader.SetIntegerUniform(const Name: TERRAString; const Value: Integer);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := glGetUniformLocation(_Program, PAnsiChar(Name));
  If (ID>=0) Then
  Begin
	  glUniform1i(Id, Value);
  End Else
    UniformError(Name);
End;

Procedure OpenGLShader.SetMat3Uniform(const Name: TERRAString; Value: Matrix3x3);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
    glUniformMatrix3fv(Id, 1, False, @(Value.V[0]));
  End Else
    UniformError(Name);
End;

Procedure OpenGLShader.SetMat4Uniform(const Name: TERRAString; Value: Matrix4x4);
Var
  ID:Integer;
  IsModelMatrix:Boolean;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
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

Procedure OpenGLShader.SetVec2Uniform(const Name: TERRAString; const Value: Vector2D);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform2fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure OpenGLShader.SetVec3Uniform(const Name: TERRAString; const Value: Vector3D);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform3fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure OpenGLShader.SetVec4Uniform(const Name: TERRAString; const Value:Vector4D);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
	  glUniform4fv(Id, 1, @Value);
  End Else
    UniformError(Name);
End;

Procedure OpenGLShader.SetVec4ArrayUniform(const Name: TERRAString; Count:Integer; Values: PVector4D);
Var
  ID:Integer;
Begin
  If (Not _Owner.Features.Shaders.Avaliable) Then
    Exit;

	ID := GetUniform(Name);
  If (ID>=0) Then
  Begin
    glUniform4fv(Id, Count, Pointer(Values));
  End Else
    UniformError(Name);
End;

{$IFDEF DEBUG_SHADERS}
Procedure SaveShader(Const Code, Name, Prefix:TERRAString);
Var
  FileName:TERRAString;
  Dest:Stream;
Begin
  FileName := 'Shaders'+PathSeparator + Name+'.'+Prefix+'.txt';

  //CreateDir('Debug');
  Dest := FileStream.Create(FileName);
  Dest.WriteLine(Code);
  ReleaseObject(Dest);
End;
{$ENDIF}

Function OpenGLShader.Generate(const Name: TERRAString; ShaderCode: TERRAString): Boolean;
Var
  I:Integer;

Function ReadBlock(Name:TERRAString):TERRAString;
Var
  S2:TERRAString;
  I:Integer;
  N:Integer;
Begin
  I := Pos(StringUpper(Name), StringUpper(ShaderCode));
  If (I>0) Then
  Begin
    S2 := Copy(ShaderCode, I +1, MaxInt);
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

    ShaderCode := Copy(S2, I + 1, MaxInt);
    S2 := Copy(S2, 1, I-1);
    Result := S2;
  End Else
    Result := '';

  Result := StringTrim(Result);
End;

Var
  Version:TERRAString;
Begin
  _Name := Name;
  Log(logDebug, 'Shader', 'Creating shader from string: '+ Name);

{  Version := Version + '#define ' + _Owner.Vendor + StringFromChar(NewLineChar);
  If (HasGLSL120) Then
    Version := Version + '#define MATRIX_CAST' + StringFromChar(NewLineChar);

  If (_Owner.Features.PostProcessing.Avaliable) Then
    Version := Version + '#define POSTPROCESSING' + StringFromChar(NewLineChar);

  If (_Owner.Settings.NormalMapping.Enabled) Then
    Version := Version + '#define NORMAL_MAPPING' + StringFromChar(NewLineChar);

{  If (_Owner.Features.FloatTexture.Avaliable) Then
    Version := Version + '#define FLOATBUFFERS' + StringFromChar(NewLineChar);

  If (_Owner.Settings.DepthOfField.Enabled) Then
    Version := Version + '#define DEPTHOFFIELD' + StringFromChar(NewLineChar);

  If (_Owner.Settings.ShadowSplitCount>1) Then
    Version := Version + '#define SHADOWSPLIT1' + StringFromChar(NewLineChar);
  If (_Owner.Settings.ShadowSplitCount>2) Then
    Version := Version + '#define SHADOWSPLIT2' + StringFromChar(NewLineChar);
  If (_Owner.Settings.ShadowSplitCount>3) Then
    Version := Version + '#define SHADOWSPLIT3' + StringFromChar(NewLineChar);}

  Version := ''; //'#version 120'+StringFromChar(NewLineChar);
	_VertexCode := Version + ReadBlock('vertex');
	_FragmentCode := Version + ReadBlock('fragment');

  _MRT := Pos('gl_FragData', _FragmentCode)>0;

//  Inherited Update();

  {$IFDEF DEBUG_SHADERS}
  If Application.Instance.DebuggerPresent Then
  Begin
    CreateDir('Shaders');
    SaveShader(_VertexCode, Name, 'vs');
    SaveShader(_FragmentCode, Name, 'fs');
  End;
  {$ENDIF}

  _AttributeCount := 0;
  If (_VertexCode ='') Or (_FragmentCode ='') Then
  Begin
    Result := False;
    Exit;
  End;

  Result := Self.Load();
End;

Function OpenGLShader.Load():Boolean;
Begin
  Log(logDebug, 'Shader', 'Compiling vertex code for ' + _Name);

  _Linked := False;
  Result := CompileShader(_Name, _VertexCode, GL_VERTEX_SHADER, _VertexShaderHandle, True);
  If Not Result Then
    Exit;

  Log(logDebug, 'Shader', 'Compiling fragment code for ' + _Name);

  Result := CompileShader(_Name, _FragmentCode, GL_FRAGMENT_SHADER, _FragmentShaderHandle, True);
  If Not Result Then
    Exit;

  Log(logDebug, 'Shader', 'Linking ' + _Name);
  Result := LinkProgram();
  Log(logDebug, 'Shader', 'Finished linking ' +_Name+', result='+BoolToString(Result));

  Self._Context := _Owner.CurrentContext;
  Log(logDebug, 'Shader', 'Shader loaded ok!');
End;


Procedure OpenGLShader.Unload;
Begin
  If (_Program>0) Then
  Begin
    If (_VertexShaderHandle>0) Then
    Begin
      If (Self.IsValid()) Then
      Begin
        glDetachShader(_Program, _VertexShaderHandle);
        glDeleteShader(_VertexShaderHandle);
      End;

      _VertexShaderHandle := 0;
    End;

    If (_FragmentShaderHandle>0) Then
    Begin
      If (Self.IsValid()) Then
      Begin
        glDetachShader(_Program, _FragmentShaderHandle);
        glDeleteShader(_FragmentShaderHandle);
      End;

      _FragmentShaderHandle := 0;
    End;

    If (Self.IsValid()) Then
    Begin
      glDeleteProgram(_Program);
    End;

    _Program := 0;
  End;

  _AttributeCount := 0;
  _Linked := False;
End;

Function OpenGLShader.Reload():Boolean;
Begin
  Self.Unload();
  Result := Self.Load();
End;
Procedure OpenGLShader.Release;
Begin
  Self.Unload();
End;

End.