{$I terra.inc}
Unit TERRA_Java;
               
Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, JNI;

Const
  ExceptionJavaClass = 'java/lang/Exception';

Type
  JavaClass = Class;

  JavaFrame = PJNIEnv;

  JavaArguments = Class(TERRAObject)
    Protected
      _Frame:JavaFrame;
      _Params:Array Of JValue;
      _ParamCount:Integer;
      _Destroy:Array Of Boolean;
      _ArgList:AnsiString;

      Procedure NewArgument(TypeName:AnsiString);

    Public
      Constructor Create(Frame:JavaFrame);
      Procedure Release; Override;

      Procedure AddBoolean(Value:Boolean);
      Procedure AddInteger(Value:Integer);
      Procedure AddFloat(Value:Single);
      Procedure AddString(Const Value:AnsiString);

      Procedure AddWordArray(Values:Pointer; Size:Integer);
  End;

  JavaClass = Class(TERRAObject)
    Protected
      _ClassPath:AnsiString;
      _Class:JClass;
      _CurrentFrame:JavaFrame;

      Function GetStaticMethod(Env:PJNIEnv; Const Name:AnsiString; Args:JavaArguments; ResultType:AnsiString):JMethodID;

    Public
      Constructor Create(ClassName:AnsiString; Frame:JavaFrame);
      Procedure Release(); Override;

      Procedure CallStaticVoidMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments);
      Function CallStaticIntMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):Integer;
      Function CallStaticFloatMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments):Single;
      Function CallStaticBoolMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):Boolean;
      Function CallStaticStringMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):AnsiString;
      Function CallStaticObjectMethod(Frame:JavaFrame; Name:AnsiString; Const ObjClass:AnsiString; Args:JavaArguments):JObject;
  End;

  JavaObject = Class(JavaClass)
    Protected
      _Object:JObject;

      Function GetMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments; Const ResultType:AnsiString):JMethodID;

    Public
      Constructor Create(Const ClassName:AnsiString; Args:JavaArguments; Frame:JavaFrame);
      Procedure Release(); Override;

      Procedure CallVoidMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments);
      Function CallBoolMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):Boolean;
      Function CallIntMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):Integer;
      Function CallFloatMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):Single;
      Function CallStringMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):AnsiString;
      Function CallByteArrayMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments; Size:Integer):Pointer;
  End;

Function JavaToString(S:JObject):AnsiString;
Function StringToJava(Const Value:AnsiString):JString;

Procedure Java_AttachThread(Var Env:PJNIEnv);
Procedure Java_DetachThread();

Procedure Java_Begin(Out Frame:JavaFrame);
Procedure Java_End(Var Frame:JavaFrame);

//Procedure Java_CacheClass(Frame:JavaFrame; Name:AnsiString);
Procedure Java_CacheClass(Env:PJNIEnv; Const Name:AnsiString);

Procedure Java_ClearClassLoader();
//Procedure Java_LoadClassLoader(Env:PJNIEnv; Loader:JavaObject);

Procedure Java_Exception(Env:PJNIEnv; Const Msg:TERRAString);

Implementation
Uses TERRA_Error, TERRA_OS, TERRA_Log, Math;

Const
  StringClassName = 'Ljava/lang/String;';

{Procedure ClearExceptions;
Begin
  SetExceptionMask([exInvalidOp, exOverflow, exUnderflow, exPrecision]);
End;

Procedure TrapExceptions;
Begin
  SetExceptionMask([exInvalidOp, exOverflow, exUnderflow, exPrecision]);
End;}

Procedure Java_AttachThread(Var Env:PJNIEnv);
Var
  Status:Integer;
Begin
  If Not Assigned(curVM) Then
    Log(logDebug, 'Java', 'Java VM is NULL!');

  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Checking JNI env');{$ENDIF}
  Status := curVM^.GetEnv(curVM, @Env, JNI_VERSION_1_6);
  If (Status<0) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Attaching thread');{$ENDIF}
    curVM^.AttachCurrentThread(curVM, @Env, Nil);
  End;

  If Env = Nil Then
  Begin
    RaiseError('Error attaching thread!');
    Exit;
  End;

  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'JNIEnv is ready!');{$ENDIF}
End;

Procedure Java_DetachThread();
Begin
  If Not Assigned(curVM) Then
    Log(logDebug, 'Java', 'Java VM is NULL!')
  Else
  Begin
    Log(logDebug, 'Java', 'Dettaching thread...');
    curVM^.DetachCurrentThread(curVM);
  End;
End;

Var
  ClassLoader:JClass;
  ClassLoaderClass:JClass;

Procedure Java_ClearClassLoader();
Begin
  ClassLoader := Nil;
  ClassLoaderClass := Nil;
End;

(*Procedure Java_LoadClassLoader(Env:PJNIEnv; Loader:JavaObject);
Var
  Temp:JClass;
Begin
  ClassLoader := Loader;

{  Log(logDebug, 'Java', 'Getting native activity class');
  Temp := Env^^.FindClass(Env, 'android/app/NativeActivity');
  activityClass := Env^^.NewGlobalRef(Env, Temp);
  Env^^.DeleteLocalRef(Env, Temp);
  Log(logDebug, 'Java', 'Got activity class '+HexStr(Cardinal(activityClass)));

  Log(logDebug, 'Java', 'Getting getClassLoader');
  getClassLoader := Env^^.GetMethodID(Env, activityClass, 'getClassLoader', '()Ljava/lang/ClassLoader;');
  Log(logDebug, 'Java', 'Got getclassloader method '+HexStr(Cardinal(getClassLoader)));

  Log(logDebug, 'Java', 'Callign getClassLoader');
  classLoaderClass := Env^^.CallObjectMethod(Env, activityClass, getClassLoader);
  Log(logDebug, 'Java', 'Got classloader class '+HexStr(Cardinal(classLoaderClass)));

  CallObjectMethod(_Frame, _Object, method);}

  Log(logDebug, 'Java', 'Getting Class Loader class');
  Temp := Env^^.FindClass(Env, 'java/lang/ClassLoader');
  classLoaderClass :=  Env^^.NewGlobalRef(Env, Temp);
  Env^^.DeleteLocalRef(Env, Temp);
  Log(logDebug, 'Java', 'Got classloader class '+HexStr(Cardinal(ClassLoader)));
End;*)

Type
  JavaClassEntry = Record
    Name:TERRAString;
    Value:JClass;
  End;

Const
  MaxJavaClasses = 64;

Var
  _JavaClasses:Array[0..Pred(MaxJavaClasses)] Of JavaClassEntry;
  _JavaClassCount:Integer;

Function Java_FindClass(Name:AnsiString):JClass;
Var
  I:Integer;
Begin
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Finding class: '+Name);{$ENDIF}

  For I:=0 To Pred(_JavaClassCount) Do
  If (StringEquals(Name, _JavaClasses[I].Name)) Then
  Begin
    Result := _JavaClasses[I].Value;
    Exit;
  End;

  Result := Nil;
  RaiseError('Could not find class: '+ Name);
End;

Procedure Java_CacheClass(Env:PJNIEnv; Const Name:AnsiString);
Var
  params:JValue;
  Temp:JClass;
  FindClass:JMethodID;
  TempName:TERRAString;
  Result:JClass;
Begin
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Finding class: '+Name);{$ENDIF}

  TempName := Name;
  StringReplaceChar(Ord('.'), Ord('/'), TempName);

(*  If Assigned(ClassLoader) Then
  Begin
    Log(logDebug, 'Java', 'Getting method loadClass');
    findClass :=  Env^^.GetMethodID(Env, classLoaderClass, 'loadClass', '(Ljava/lang/String;)Ljava/lang/Class;');

    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Using cached class loader with method '+HexStr(Cardinal(FindClass)));{$ENDIF}
    params.l := PAnsiChar(Name);
    Result := JClass(Env^^.CallObjectMethodA(Env, ClassLoader, findClass, @params));
  End Else*)
    Result := JClass(Env^^.FindClass(Env, PAnsiChar(TempName)));

  If Result = Nil Then
  Begin
    RaiseError('Could not cache class: '+ Name);
    Exit;
  End;

  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Making classref global '+HexStr(Cardinal(Result)));{$ENDIF}
  Temp := Result;
  Result := Env^^.NewGlobalRef(Env, Result);
  Env^^.DeleteLocalRef(Env, Temp);

  _JavaClasses[_JavaClassCount].Name := Name;
  _JavaClasses[_JavaClassCount].Value := Result;
  Inc(_JavaClassCount);

  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Result: '+HexStr(Cardinal(Result)));{$ENDIF}
End;


Function Java_FindMethod(Env:PJNIEnv; Name, Signature:AnsiString; SourceClass:JClass):JMethodID;
Begin
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Finding method: '+Name+' for class '+HexStr(Cardinal(SourceClass)));{$ENDIF}
  Result := Env^^.GetMethodID(Env, sourceClass, PAnsiChar(Name), PAnsiChar(Signature));
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Result: '+HexStr(Cardinal(Result)));{$ENDIF}
End;

Function Java_FindStaticMethod(Env:PJNIEnv; Name, Signature:AnsiString; SourceClass:JClass):JMethodID;
Begin
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Finding static method: '+Name+' for class '+HexStr(Cardinal(SourceClass)));{$ENDIF}
  Result := Env^^.GetStaticMethodID(Env, sourceClass, PAnsiChar(Name), PAnsiChar(Signature));
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Result: '+HexStr(Cardinal(Result)));{$ENDIF}
End;

Function Java_ReadString(Env:PJNIEnv; S:JObject):AnsiString;
Var
  Len:Integer;
  IsCopy:Byte;
  P:PAnsiChar;
Begin
  If (S=Nil) Then
  Begin
    Result := '';
    Exit;
  End;

  Len := Env^^.GetStringLength(Env, S);

  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Reading string with length '+IntToString(len));{$ENDIF}

  IsCopy := 0;
  P := Env^^.GetStringUTFChars(Env, S, IsCopy);
  Result := P;
  Env^^.ReleaseStringUTFChars(Env, S, P);
  Log(logDebug, 'Java', 'Got '+Result);
End;

Function Java_NewString(Env:PJNIEnv; Value:AnsiString; MakeGlobal:Boolean):JString;
Var
  Temp:JString;
Begin
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Creating string: '+Value);{$ENDIF}

  If (Value='') Then
    Result := Nil
  Else
    Result := Env^^.NewStringUTF(Env, PAnsiChar(Value));

  If (MakeGlobal) And (Assigned(Result)) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Making string global '+HexStr(Cardinal(Result)));{$ENDIF}
    Temp := Result;
    Result := Env^^.NewGlobalRef(Env, Temp);
    Env^^.DeleteLocalRef(Env, Temp);
  End;
End;

Function Java_NewShortArray(Env:PJNIEnv; Size:Integer; Buf:Pointer):JObject;
Begin
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Creating short array, size: '+IntToString(Size));{$ENDIF}
  Result := Env^^.NewShortArray(Env, Size);
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Result: '+HexStr(Cardinal(Result)));{$ENDIF}

  (*If Assigned(Result) Then
  Begin
    Log(logDebug, 'Java', 'Making array global '+HexStr(Cardinal(Result)));
    Result := Env^^.NewGlobalRef(Env, Result);
  End;*)

  If Assigned(Buf) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Filling short array, buf: '+HexStr(Cardinal(Buf))+' size:'+IntToString(Size));{$ENDIF}
    Env^^.SetShortArrayRegion(Env, Result, 0, Size, Pjshort(Buf));
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Array filled ');{$ENDIF}
  End;
End;

Function Java_NewObject(Env:PJNIEnv; Signature:AnsiString; SourceClass:JClass; Args:Pjvalue):JObject;
Var
  Met:JMethodID;
Begin
  Met := Java_FindMethod(Env, '<init>', Signature, SourceClass);
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Creating object');{$ENDIF}
  If Assigned(Args) Then
    Result := Env^^.NewObjectA(Env, SourceClass, Met, Args)
  Else
    Result := Env^^.NewObject(Env, SourceClass, Met);

  (*If Assigned(Result) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Making object global '+HexStr(Cardinal(Result)));{$ENDIF}
    Result := Env^^.NewGlobalRef(Env, Result);
  End;*)
End;

Procedure Java_DeleteObject(Env:PJNIEnv; Var Obj:JObject);
Begin
  If Assigned(Obj) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Deleting local object '+HexStr(Cardinal(Obj)));{$ENDIF}
    Env^^.DeleteLocalRef(Env, Obj);
    Obj := Nil;
  End;
End;

Procedure Java_DeleteGlobalObject(Env:PJNIEnv; Var Obj:JObject);
Begin
  If Assigned(Obj) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Deleting global object '+HexStr(Cardinal(Obj)));{$ENDIF}
    Env^^.DeleteGlobalRef(Env, Obj);
    Obj := Nil;
  End;
End;

Function GetSignature(Args:JavaArguments; ResultType:AnsiString):AnsiString;
Var
  I:Integer;
Begin
  Result := '(';
  If Args<>Nil Then
    Result := Result + Args._ArgList;
  Result := Result + ')'+ResultType;

  {$IFDEF DEBUG_JAVA}
  If Assigned(Args) Then
  Begin
    For I:=0 To Pred(Args._ParamCount) Do
    If (Args._Destroy[I]) Then
      Log(logDebug, 'Java', 'Arg '+IntToString(I+1)+': '+HexStr(Cardinal(Args._Params[I].l)));
  End;
  {$ENDIF}
End;

Function GetParams(Args:JavaArguments):Pointer;
Begin
  If (Assigned(Args)) And (Args._ParamCount>0) Then
    Result := @(Args._Params[0])
  Else
    Result := Nil;
End;

{ JavaArguments }
Constructor JavaArguments.Create(Frame:JavaFrame);
Begin
  _Frame := Frame;
  _ParamCount := 0;
  _ArgList := '';
End;

Procedure JavaArguments.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ParamCount) Do
  If (_Destroy[I]) Then
  Begin
    Java_DeleteObject(_Frame, _Params[I].l);
    _Destroy[I] := False;
  End;

  _ParamCount := 0;
End;

Procedure JavaArguments.NewArgument(TypeName:AnsiString);
Begin
  Inc(_ParamCount);
  SetLength(_Params, _ParamCount);
  SetLength(_Destroy, _ParamCount);
  _Destroy[Pred(_ParamCount)] := False;

  _ArgList := _ArgList + TypeName;
End;

Procedure JavaArguments.AddBoolean(Value: Boolean);
Begin
  Self.NewArgument('Z');
  If Value Then
    _Params[Pred(_ParamCount)].z := 1
  Else
    _Params[Pred(_ParamCount)].z := 0;
End;

Procedure JavaArguments.AddFloat(Value: Single);
Begin
  Self.NewArgument('F');
  _Params[Pred(_ParamCount)].f := Value;
End;

Procedure JavaArguments.AddInteger(Value: Integer);
Begin
  Self.NewArgument('I');
  _Params[Pred(_ParamCount)].i := Value;
End;

Procedure JavaArguments.AddString(Const Value:AnsiString);
Begin
  Self.NewArgument(StringClassName);
  _Params[Pred(_ParamCount)].l := Java_NewString(_Frame, Value, False);
  _Destroy[Pred(_ParamCount)] := True;
End;

Procedure JavaArguments.AddWordArray(Values: Pointer; Size:Integer);
Begin
  Self.NewArgument('[S');
  _Params[Pred(_ParamCount)].l := Java_NewShortArray(_Frame, Size, Values);
  _Destroy[Pred(_ParamCount)] := True;
End;

{ JavaClass }
Constructor JavaClass.Create(ClassName:AnsiString; Frame:JavaFrame);
Begin
  _CurrentFrame := Frame;
  _ClassPath := ClassName;
  _Class := Java_FindClass(ClassName);
End;

Procedure JavaClass.Release;
Begin
  (*If (Assigned(_Class)) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Destroying class '+_ClassPath);{$ENDIF}
    Java_DeleteGlobalObject(_CurrentFrame, _Class);
  End;*)
End;

Function JavaClass.GetStaticMethod(Env:PJNIEnv; Const Name:AnsiString; Args:JavaArguments; ResultType:AnsiString):JMethodID;
Var
  Signature:AnsiString;
Begin
  Signature := GetSignature(Args, ResultType);
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Searching for static method '+Name+' with signature '+Signature+' of class '+_ClassPath);{$ENDIF}

  Result := Java_FindStaticMethod(Env, Name, Signature, _Class);
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Calling java method '+Name+' with address '+CardinalToString(Cardinal(Result)));{$ENDIF}
End;


Procedure JavaClass.CallStaticVoidMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments);
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetStaticMethod(Frame, Name, Args, 'V');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Frame^^.CallStaticVoidMethodA(Frame, _Class, method, GetParams(Args))
    Else
      Frame^^.CallStaticVoidMethod(Frame, _Class, method);
  End;
End;

Function JavaClass.CallStaticBoolMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments): Boolean;
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetStaticMethod(Frame, Name, Args, 'Z');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Result := Frame^^.CallStaticBooleanMethodA(Frame, _Class, method, GetParams(Args))<>0
    Else
      Result := Frame^^.CallStaticBooleanMethod(Frame, _Class, method)<>0;
  End Else
    Result := False;
End;

Function JavaClass.CallStaticIntMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments): Integer;
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetStaticMethod(Frame, Name, Args, 'I');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Result := Frame^^.CallStaticIntMethodA(Frame, _Class, method, GetParams(Args))
    Else
      Result := Frame^^.CallStaticIntMethod(Frame, _Class, method)
  End Else
    Result := 0;
End;

Function JavaClass.CallStaticFloatMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments):Single;
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetStaticMethod(Frame, Name, Args, 'F');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Result := Frame^^.CallStaticFloatMethodA(Frame, _Class, method, GetParams(Args))
    Else
     Result := Frame^^.CallStaticFloatMethod(Frame, _Class, method)
  End Else
    Result := 0.0;
End;

Function JavaClass.CallStaticStringMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments):AnsiString;
Var
  Method:JMethodID;
  Obj:JObject;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetStaticMethod(Frame, Name, Args, StringClassName);

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Obj := Frame^^.CallStaticObjectMethodA(Frame, _Class, method, GetParams(Args))
    Else
      Obj := Frame^^.CallStaticObjectMethod(Frame, _Class, method);

    Result := Java_ReadString(Frame, Obj);
    Java_DeleteObject(Frame, Obj);
  End Else
    Result := '';
End;

Function JavaClass.CallStaticObjectMethod(Frame:JavaFrame; Name:AnsiString; Const ObjClass: AnsiString; Args: JavaArguments): JObject;
Var
  Method:JMethodID;
  Obj:JObject;
Begin
  _CurrentFrame := Frame;

  StringReplaceChar(Ord('.'), Ord('/'), Name);
  Method := Self.GetStaticMethod(Frame, Name, Args, 'L'+ObjClass+';');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Obj := Frame^^.CallStaticObjectMethodA(Frame, _Class, method, GetParams(Args))
    Else
      Obj := Frame^^.CallStaticObjectMethod(Frame, _Class, method);

    Result := Frame^^.NewGlobalRef(Frame, Obj);
    Frame^^.DeleteLocalRef(Frame, Obj);
  End Else
    Result := Nil;
End;

{ JavaObject }
Constructor JavaObject.Create(Const ClassName:AnsiString; Args: JavaArguments; Frame:JavaFrame);
Var
  Temp:JObject;
Begin
  Inherited Create(ClassName, Frame);

  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Calling constructor of class '+_ClassPath);{$ENDIF}
  _Object := Java_NewObject(Frame, GetSignature(Args, 'V'), _Class, GetParams(Args));

  Temp := _Object;

  _Object := Frame^^.NewGlobalRef(Frame, _Object);
  Frame^^.DeleteLocalRef(Frame, Temp);
End;

Procedure JavaObject.Release;
Begin
  If (Assigned(_Object)) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Destroying object of class '+_ClassPath);{$ENDIF}
    Java_DeleteGlobalObject(_CurrentFrame, _Object);
  End;

  Inherited;
End;

Function JavaObject.GetMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments; Const ResultType:AnsiString):JMethodID;
Var
  Signature:AnsiString;
Begin
  _CurrentFrame := Frame;

  Signature := GetSignature(Args, ResultType);
  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Searching for method '+Name+' with signature '+Signature+' of class '+_ClassPath);{$ENDIF}

  If (_Object = Nil) Then
  Begin
    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Java error, object not initialized!');{$ENDIF}
    Result := Nil;
  End;

  Result := Java_FindMethod(Frame, Name, Signature, _Class);

  {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Calling java method '+Name+' with address '+CardinalToString(Cardinal(Result)));{$ENDIF}
End;

Procedure JavaObject.CallVoidMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments);
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;

  Method := Self.GetMethod(Frame, Name, Args, 'V');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Frame^^.CallVoidMethodA(Frame, _Object, method, GetParams(Args))
    Else
      Frame^^.CallVoidMethod(Frame, _Object, method);
  End;
End;


Function JavaObject.CallBoolMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments): Boolean;
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;

  Method := Self.GetMethod(Frame, Name, Args, 'Z');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Result := Frame^^.CallBooleanMethodA(Frame, _Object, method, GetParams(Args))<>0
    Else
      Result := Frame^^.CallBooleanMethod(Frame, _Object, method)<>0;
  End Else
    Result := False;
End;

Function JavaObject.CallFloatMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):Single;
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetMethod(Frame, Name, Args, 'F');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Result := Frame^^.CallFloatMethodA(Frame, _Object, method, GetParams(Args))
    Else
      Result := Frame^^.CallFloatMethod(Frame, _Object, method);
  End Else
    Result := 0.0;
End;

Function JavaObject.CallIntMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments): Integer;
Var
  Method:JMethodID;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetMethod(Frame, Name, Args, 'I');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Result := Frame^^.CallIntMethodA(Frame, _Object, method, GetParams(Args))
    Else
      Result := Frame^^.CallIntMethod(Frame, _Object, method)
  End Else
    Result := 0;
End;

Function JavaObject.CallStringMethod(Frame:JavaFrame; Const Name:AnsiString; Args:JavaArguments):AnsiString;
Var
  Method:JMethodID;
  Obj:JObject;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetMethod(Frame, Name, Args, StringClassName);

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Obj := Frame^^.CallObjectMethodA(Frame, _Object, method, GetParams(Args))
    Else
      Obj := Frame^^.CallObjectMethod(Frame, _Object, method);

    Result := Java_ReadString(Frame, Obj);
    Java_DeleteObject(Frame, Obj);
  End Else
    Result := '';
End;


Function JavaObject.CallByteArrayMethod(Frame:JavaFrame; Const Name:AnsiString; Args: JavaArguments; Size:Integer): Pointer;
Var
  Method:JMethodID;
  Obj:JObject;
  IsCopy:Byte;
  Buf:PJByte;
Begin
  _CurrentFrame := Frame;
  Method := Self.GetMethod(Frame, Name, Args, '[B');

  If (Assigned(Method)) Then
  Begin
    If (Args<>Nil) And (Args._ParamCount>0) Then
      Obj := Frame^^.CallObjectMethodA(Frame, _Object, method, GetParams(Args))
    Else
      Obj := Frame^^.CallObjectMethod(Frame, _Object, method);

    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Allocating '+IntToString(Size)+' bytes');{$ENDIF}
    GetMem(Result, Size);

    IsCopy := 0;

    {$IFDEF DEBUG_JAVA}Log(logDebug, 'Java', 'Copying bytes from java array');{$ENDIF}
    Buf := Frame^^.GetByteArrayElements(Frame, Obj, IsCopy);
    Move(Buf^, Result^, Size);

    Frame^^.ReleaseByteArrayElements(Frame, Obj, Buf, JNI_ABORT);

    Java_DeleteObject(Frame, Obj);
  End Else
    Result := Nil;
End;

{ JavaFrame }
Procedure Java_Begin(Out Frame:JavaFrame);
Begin
  Frame := Nil;
  Java_AttachThread(Frame);

  If (Not Assigned(Frame)) Then
  Begin
    RaiseError('JNI Error: Cannot acess current VM enviroment');
    Exit;
  End;

  Frame^^.PushLocalFrame(Frame, 20);
End;

Procedure Java_End(Var Frame:JavaFrame);
Begin
  If (Not Assigned(Frame)) Then
  Begin
    Exit;
  End;

  Frame^^.PopLocalFrame(Frame, Nil);
  //Java_DetachThread();

  Frame := Nil;
End;

Function JavaToString(S:JObject):AnsiString;
Var
  Frame:JavaFrame;
Begin
  {$IFDEF DEBUG_JAVA}
  Log(logDebug, 'App', 'Reading java string...');
  {$ENDIF}

  If (S=Nil) Then
  Begin
    Result := '';
    Exit;
  End;

  Java_Begin(Frame);
  Result := Java_ReadString(Frame, S);
  Java_End(Frame);
End;

Function StringToJava(Const Value:AnsiString):JString;
Var
  Frame:JavaFrame;
Begin
  {$IFDEF DEBUG_JAVA}
  Log(logDebug, 'App', 'Creating java string: '+Value);
  {$ENDIF}

  If (Value='') Then
  Begin
    Result := Nil;
    Exit;
  End;

  Java_Begin(Frame);
  Result := Java_NewString(Frame, Value, True);
  Java_End(Frame);
End;

Procedure Java_Exception(Env:PJNIEnv; Const Msg:TERRAString);
Var
  ErrorClass:JClass;
Begin
  ErrorClass := Java_FindClass(ExceptionJavaClass);
  If Assigned(ErrorClass) Then
    Env^^.ThrowNew(Env, ErrorClass, PAnsiChar(Msg));
End;

End.
