Unit TERRA_Object;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Tween;

Type
  TERRAObject = Class
    Protected
      _ObjectName:TERRAString;

      Procedure Release; Virtual;

      Procedure SetObjectName(const Value: TERRAString); Virtual;

    Public
      Function GetObjectType:TERRAString; Virtual;

      Function GetBlob():TERRAString; Virtual;
      Procedure SetBlob(Const Blob:TERRAString);Virtual;

      Function IsValueObject():Boolean; Virtual;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Virtual;
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Virtual;

      Function FindProperty(Const KeyName:TERRAString):TERRAObject;
      Function FindPropertyWithPath(Path:TERRAString):TERRAObject;

      Function HasPropertyTweens:Boolean; Virtual;

      Procedure CopyProperties(Other:TERRAObject);

      { Returns a ID to be used for sorting. }
      Function SortID:Integer; Virtual;

      Destructor Destroy; Override;

      Property Name:TERRAString Read _ObjectName Write SetObjectName;
  End;

  BooleanProperty = Class(TERRAObject)
    Protected
      _Value:Boolean;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Boolean);

      Function IsValueObject():Boolean; Override;

      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:Boolean Read _Value Write _Value;
  End;

  StringProperty = Class(TERRAObject)
    Protected
      _Value:TERRAString;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:TERRAString);

      Function IsValueObject():Boolean; Override;

      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:TERRAString Read _Value Write SetBlob;
  End;

  TweenCallback = Procedure (Target:TERRAObject) Of Object;
  TweenState = (tweenWaiting, tweenRunning, tweenFinished);

  TweenObject = Object
    TweenID:Cardinal;

    StartValue:Single;
    TargetValue:Single;

    StartTime:Cardinal;
    EndTime:Cardinal;
    Duration:Single;

    State:TweenState;
    Ease:TweenEaseType;

    Callback:TweenCallback;
    CallTarget:TERRAObject;
  End;

  TweenableProperty = Class(TERRAObject)
    Protected
      _TweenList:Array Of TweenObject;
      _TweenCount:Integer;

      Procedure RegisterTween(Const Ease:TweenEaseType; TweenID:Integer; Const StartValue, TargetValue:Single; Const Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);

      Procedure UpdateTweenValue(Const TweenID:Integer; Const Value:Single); Virtual;

    Public
      Procedure UpdateTweens(); Virtual;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Virtual; Abstract;

      Function IsValueObject():Boolean; Override;

      Function HasPropertyTweens:Boolean; Override;
  End;

  IntegerProperty = Class(TweenableProperty)
    Protected
      _Value:Integer;

      Function GetIntegerValue: Integer;
      Procedure SetIntegerValue(const NewValue:Integer);

      Procedure UpdateTweenValue(Const TweenID:Integer; Const Value:Single); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Integer);

      Function GetObjectType:TERRAString; Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Integer; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Class Function Stringify(Const N:Integer):TERRAString;

      Function SortID:Integer; Override;

      Property Value:Integer Read GetIntegerValue Write SetIntegerValue;
  End;

  ByteProperty = Class(TweenableProperty)
    Protected
      _Value:Byte;

      Function GetByteValue:Byte;
      Procedure SetByteValue(const NewValue:Byte);

      Procedure UpdateTweenValue(Const TweenID:Integer; Const Value:Single); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Byte);
      Function GetObjectType:TERRAString; Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Byte; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Function SortID:Integer; Override;

      Property Value:Byte Read GetByteValue Write SetByteValue;
  End;

  FloatProperty = Class(TweenableProperty)
    Protected
      _Value:Single;

      Function GetFloatValue: Single;
      Procedure SetFloatValue(const NewValue:Single);

      Procedure UpdateTweenValue(Const TweenID:Integer; Const Value:Single); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Single);
      Function GetObjectType:TERRAString; Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Single; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Function SortID:Integer; Override;

      Class Function Stringify(N:Single; DecimalPlacesLimit:Integer = 7):TERRAString;

      Property Value:Single Read GetFloatValue Write SetFloatValue;

  End;

  AngleProperty = Class(FloatProperty)
    Public
      Function GetObjectType:TERRAString; Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;
  End;


Procedure ReleaseObject(var Obj);

Implementation
Uses TERRA_Log, TERRA_Math, TERRA_OS, TERRA_RTTI;

Procedure ReleaseObject(Var Obj);
Var
  Temp:TObject;
Begin
  Temp := TERRAObject(Obj);
  If Temp = Nil Then
    Exit;

  If (Temp Is TERRAObject) Then
  Begin
    TERRAObject(Temp).Release();
    TERRAObject(Temp).Destroy();
  End Else
    Log(logWarning, 'App', Temp.ClassName +' is not a TERRA-Object!');

  Pointer(Obj) := Nil;
End;


{ TERRAObject }
Function TERRAObject.SortID: Integer;
Begin
  Result := 0;
End;

Procedure TERRAObject.Release;
Begin
{  S := Self.ClassName;
  Log(logWarning, 'App', 'Destroying instance of '+S);}
End;

Destructor TERRAObject.Destroy();
Begin
  {$IFDEF WINDOWS}
 // DebugBreak();
//  RaiseError('Destructors are not allowed in class: '+Self.ClassName);
  {$ENDIF}

  Inherited;
End;

Function TERRAObject.GetObjectType: TERRAString;
Begin
  Result := Self.ClassName;
End;

Procedure TERRAObject.SetObjectName(const Value: TERRAString);
Begin
  Self._ObjectName := Value;
End;

Function TERRAObject.HasPropertyTweens: Boolean;
Var
  I:Integer;
  Key:TERRAObject;
Begin
  I := 0;
  Repeat
    Key := Self.GetPropertyByIndex(I);
    Inc(I);

    If (Assigned(Key)) And (Key.HasPropertyTweens()) Then
    Begin
      Result := True;
      Exit;
    End;
  Until Key = Nil;

  Result := False;
End;

Procedure TERRAObject.CopyProperties(Other: TERRAObject);
Var
  I:Integer;
  KeyA, KeyB:TERRAObject;
  Temp:TERRAString;
Begin
  I := 0;
  Repeat
    KeyA := Self.GetPropertyByIndex(I);
    KeyB := Other.GetPropertyByIndex(I);
    Inc(I);

    If (KeyA = Nil) Or (KeyB = Nil) Then
      Break;

    If (KeyB.IsValueObject()) Then
    Begin
      Temp := KeyB.GetBlob();
      KeyA.SetBlob(Temp);
    End;

    KeyA.CopyProperties(KeyB);
  Until False;
End;

Function TERRAObject.CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject;
Var
  ObjType:TERRAObjectType;
Begin
  ObjType := RTTI.FindType(ObjectType);
  If ObjType = Nil Then
  Begin
    Log(logError, 'Application', 'Cannot unserialize object of type ' +ObjectType+' with name '+KeyName);
  End;

  Result := TERRAObject(ObjType.Create());

(*  If StringEquals(ObjectType, 'list') Then
  Begin
    Result := List.Create();
    Result.Name := KeyName;
  End Else
  Begin
    Result := Nil;

    For I:=0 To Pred(_ApplicationComponentCount) Do
    If Assigned(_ApplicationComponents[I].Instance) Then
    Begin
      Result := _ApplicationComponents[I].Instance.CreateProperty(KeyName, ObjectType);
      If Assigned(Result) Then
      Begin
        Log(logDebug, 'Application', 'Unserialized object of type ' +ObjectType+ ' from '+_ApplicationComponents[I].Name);
        Exit;
      End;

    End;
  End;*)

End;

Function TERRAObject.FindPropertyWithPath(Path:TERRAString):TERRAObject;
Var
  S:TERRAString;
Begin
  S := StringGetNextSplit(Path, Ord('.'));
  Result := Self.FindProperty(S);

  If (Path = '') Or (Result = Nil) Then
    Exit;

  Result := Result.FindPropertyWithPath(Path);
End;

Function TERRAObject.FindProperty(Const KeyName: TERRAString): TERRAObject;
Var
  I:Integer;
  Key:TERRAObject;
Begin
  If StringEquals(Self.Name, KeyName)  Then
  Begin
    Result := Self;
    Exit;
  End;

  I := 0;
  Repeat
    Key := Self.GetPropertyByIndex(I);
    Inc(I);

    If (Key = Nil) Then
      Break;

    Result := Key.FindProperty(KeyName);
    If Assigned(Result) Then
      Exit;
  Until False;

  Result := Nil;
End;

Function TERRAObject.GetPropertyByIndex(Index:Integer): TERRAObject;
Begin
  Result := Nil;
End;

Function TERRAObject.GetBlob: TERRAString;
Begin
  Result := '';
End;

Procedure TERRAObject.SetBlob(const Blob: TERRAString);
Begin
  // do nothing
End;

Function TERRAObject.IsValueObject: Boolean;
Begin
  Result := False;
End;

{ TweenableProperty }
Function TweenableProperty.HasPropertyTweens:Boolean;
Begin
  Self.UpdateTweens();
  Result := (_TweenCount>0);
End;

Procedure TweenableProperty.RegisterTween(Const Ease:TweenEaseType; TweenID:Integer; Const StartValue, TargetValue:Single; Const Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Var
  T:TweenObject;
Begin
  T.StartTime := Application.GetTime() + Delay;
  T.EndTime := T.StartTime + Duration;
  T.State := tweenWaiting;
  T.StartValue := StartValue;
  T.TargetValue := TargetValue;
  T.Duration := Duration;
  T.Ease := Ease;
  T.Callback := Callback;
  T.CallTarget := CallTarget;

  Inc(_TweenCount);
  If (Length(_TweenList)<_TweenCount) Then
    SetLength(_TweenList, _TweenCount);

  _TweenList[Pred(_TweenCount)] := T;
End;

Procedure TweenableProperty.UpdateTweens();
Var
  I:Integer;
  T:Cardinal;
  Val, Delta:Single;
Begin
  T := Application.GetTime();

  I:=0;
  While (I<_TweenCount) Do
  Case _TweenList[I].State Of
    tweenWaiting:
    Begin
      If (_TweenList[I].EndTime<=T) Then
      Begin
        _TweenList[I].State := tweenFinished;
      End Else
      If (_TweenList[I].StartTime<=T) Then
      Begin
        _TweenList[I].State := tweenRunning;
      End;

      Inc(I);
    End;

    tweenRunning:
      Begin
        Delta := (T - _TweenList[I].StartTime) / _TweenList[I].Duration;

        If Delta>=1.0 Then
        Begin
          Delta := 1.0;
          _TweenList[I].State := tweenFinished;

          If Assigned(_TweenList[I].Callback) Then
            _TweenList[I].Callback(_TweenList[I].CallTarget);
        End;

        Delta := GetEase(Delta, _TweenList[I].Ease);

        Val := (_TweenList[I].TargetValue * Delta) + (_TweenList[I].StartValue * (1.0 - Delta));
        Self.UpdateTweenValue(_TweenList[I].TweenID,  Val);

        //Inc(I);
        Break;
      End;

  Else
    Begin
      _TweenList[I] := _TweenList[Pred(_TweenCount)];
      Dec(_TweenCount);
    End;
  End;
End;

Function TweenableProperty.IsValueObject: Boolean;
Begin
  Result := True;
End;

Procedure TweenableProperty.UpdateTweenValue(const TweenID:Integer; const Value:Single);
Begin
  // do nothing
End;

{ IntegerProperty }
Constructor IntegerProperty.Create(const Name: TERRAString; const InitValue: Integer);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
End;

Function IntegerProperty.GetBlob: TERRAString;
Begin
  Result :=  IntegerProperty.Stringify(Value);
End;

Procedure IntegerProperty.SetBlob(const Blob: TERRAString);
Begin
  Value := StringToInt(Blob);
End;

Function IntegerProperty.GetIntegerValue:Integer;
Begin
  Self.UpdateTweens();
  Result := _Value;
End;

Procedure IntegerProperty.SetIntegerValue(const NewValue: Integer);
Begin
  _Value := NewValue;
End;

Function IntegerProperty.GetObjectType: TERRAString;
Begin
  Result := 'int';
End;

Procedure IntegerProperty.AddTween(const Ease: TweenEaseType; const StartValue, TargetValue: Integer; Duration, Delay: Cardinal;  Callback: TweenCallback; CallTarget: TERRAObject);
Begin
  Self.RegisterTween(Ease, 0, StartValue, TargetValue, Duration, Delay, Callback, CallTarget);
End;

Procedure IntegerProperty.AddTweenFromBlob(const Ease: TweenEaseType; const StartValue, TargetValue: TERRAString; Duration, Delay: Cardinal; Callback: TweenCallback; CallTarget: TERRAObject);
Begin
  Self.AddTween(Ease, StringToInt(StartValue), StringToInt(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure IntegerProperty.UpdateTweenValue(Const TweenID:Integer; Const Value:Single);
Begin
  _Value := Trunc(Value);
End;

Function IntegerProperty.SortID: Integer;
Begin
  Result := _Value;
End;

Class Function IntegerProperty.Stringify(Const N:Integer):TERRAString;
Begin
{$IFDEF OXYGENE}
  Result := System.Convert.ToString(N);
{$ELSE}
  Str(N, Result);
{$ENDIF}
End;

{ ByteProperty }
Constructor ByteProperty.Create(const Name: TERRAString; const InitValue: Byte);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
End;

Function ByteProperty.GetBlob: TERRAString;
Begin
  Result :=  IntegerProperty.Stringify(Value);
End;

Procedure ByteProperty.SetBlob(const Blob: TERRAString);
Begin
  Value := StringToInt(Blob);
End;

Function ByteProperty.GetByteValue:Byte;
Begin
  Self.UpdateTweens();
  Result := _Value;
End;

Procedure ByteProperty.SetByteValue(const NewValue: Byte);
Begin
  _Value := NewValue;
End;

Function ByteProperty.GetObjectType: TERRAString;
Begin
  Result := 'byte';
End;

Procedure ByteProperty.AddTween(const Ease: TweenEaseType; const StartValue, TargetValue: Byte; Duration, Delay: Cardinal;  Callback: TweenCallback; CallTarget: TERRAObject);
Begin
  Self.RegisterTween(Ease, 0, StartValue, TargetValue, Duration, Delay, Callback, CallTarget);
End;

Procedure ByteProperty.AddTweenFromBlob(const Ease: TweenEaseType; const StartValue, TargetValue: TERRAString; Duration, Delay: Cardinal; Callback: TweenCallback; CallTarget: TERRAObject);
Begin
  Self.AddTween(Ease, StringToInt(StartValue), StringToInt(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure ByteProperty.UpdateTweenValue(Const TweenID:Integer; Const Value:Single);
Begin
  _Value := Trunc(Value);
End;

Function ByteProperty.SortID: Integer;
Begin
  Result := _Value;
End;

{ FloatProperty }
Constructor FloatProperty.Create(Const Name:TERRAString; const InitValue: Single);
Begin
  _ObjectName := Name;
  _Value := InitValue;
End;

Function FloatProperty.GetObjectType: TERRAString;
Begin
  Result := 'float';
End;

Function FloatProperty.GetBlob: TERRAString;
Begin
  Result := FloatProperty.Stringify(GetFloatValue());
End;

Procedure FloatProperty.SetBlob(const Blob: TERRAString);
Begin
  _Value := StringToFloat(Blob);
End;

Procedure FloatProperty.SetFloatValue(const NewValue: Single);
Begin
  _Value := NewValue;
End;

Function FloatProperty.GetFloatValue():Single;
Begin
  Self.UpdateTweens();
  Result := _Value;
End;

Procedure FloatProperty.AddTween(const Ease: TweenEaseType; const StartValue, TargetValue:Single; Duration, Delay: Cardinal;  Callback: TweenCallback; CallTarget: TERRAObject);
Begin
  Self.RegisterTween(Ease, 0, StartValue, TargetValue, Duration, Delay, Callback, CallTarget);
End;

Procedure FloatProperty.AddTweenFromBlob(const Ease: TweenEaseType; const StartValue, TargetValue: TERRAString; Duration, Delay: Cardinal; Callback: TweenCallback; CallTarget: TERRAObject);
Begin
  Self.AddTween(Ease, StringToFloat(StartValue), StringToFloat(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure FloatProperty.UpdateTweenValue(Const TweenID:Integer; Const Value:Single);
Begin
  _Value := Value;
End;

Function FloatProperty.SortID: Integer;
Begin
  Result := Trunc(_Value);
End;

Class Function FloatProperty.Stringify(N:Single; DecimalPlacesLimit:Integer):TERRAString;
Var
  X:Single;
  A:Integer;
  DecimalPlaces, K:Integer;
  Ready:Boolean;
Begin
  If (N<0) Then
    Result := '-'
  Else
    Result := '';

  N := Abs(N);
  A := Trunc(N);
  X := Frac(N);

  Result := Result +  IntegerProperty.Stringify(A) +'.';

  DecimalPlaces := 0;
  Ready := False;
  K := 10;
  Repeat
    N := X * K;
    K := K * 10;
    A := Trunc(N) Mod 10;

    Result := Result +  IntegerProperty.Stringify(A);
    Inc(DecimalPlaces);

    If A>0 Then
      Ready := True;

  Until (DecimalPlaces>=DecimalPlacesLimit) Or ((A=0) And (Ready));
End;

{ BooleanProperty }
Constructor BooleanProperty.Create(const Name: TERRAString; const InitValue: Boolean);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
End;

Function BooleanProperty.GetObjectType: TERRAString;
Begin
  Result := 'bool';
End;

Function BooleanProperty.GetBlob: TERRAString;
Begin
  Result := BoolToString(_Value);
End;

Procedure BooleanProperty.SetBlob(const Blob: TERRAString);
Begin
  _Value := StringToBool(Blob);
End;


Function BooleanProperty.IsValueObject: Boolean;
Begin
  Result := True;
End;

{ StringProperty }
Constructor StringProperty.Create(const Name, InitValue: TERRAString);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
End;

Function StringProperty.GetBlob: TERRAString;
Begin
  Result := _Value;
End;

Procedure StringProperty.SetBlob(const Blob: TERRAString);
Begin
  _Value := Blob;
End;

Function StringProperty.GetObjectType: TERRAString;
Begin
  Result := 'string';
End;

Function StringProperty.IsValueObject: Boolean;
Begin
  Result := True;
End;

{ AngleProperty }
Function AngleProperty.GetObjectType: TERRAString;
Begin
  Result := 'angle';
End;

Function AngleProperty.GetBlob: TERRAString;
Begin
  Result := FloatProperty.Stringify(GetFloatValue() * DEG);
End;

Procedure AngleProperty.SetBlob(const Blob: TERRAString);
Begin
  _Value := StringToFloat(Blob) * RAD;
End;

Procedure AngleProperty.AddTweenFromBlob(const Ease: TweenEaseType; const StartValue, TargetValue: TERRAString; Duration, Delay: Cardinal; Callback: TweenCallback; CallTarget: TERRAObject);
Begin
  Self.AddTween(Ease, StringToFloat(StartValue) * RAD, StringToFloat(TargetValue) * RAD, Duration, Delay, Callback, CallTarget);
End;


End.
