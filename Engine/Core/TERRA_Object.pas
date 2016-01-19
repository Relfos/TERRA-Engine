Unit TERRA_Object;

{$I terra.inc}

Interface
Uses {$IFNDEF WINDOWS}cmem, cthreads,{$ENDIF}
  TERRA_Tween;

Type
  {$IFDEF OXYGENE}
  TERRAString = String;
  TERRAChar = Char;
  {$ELSE}
  
  {$IFDEF DCC}
    TERRAString = AnsiString; // Delphi XE2 and above
    TERRAChar = WideChar;
  {$ELSE}
    TERRAString = String;
    TERRAChar = WideChar;
  {$ENDIF}
  
  {$ENDIF}

  StringArray = Array Of TERRAString;

  TERRAObject = Class
    Protected
      _ObjectName:TERRAString;

      Procedure Release; Virtual;

      Procedure SetObjectName(const Value: TERRAString); Virtual;

    Public
      Constructor Create();

      Class Function GetObjectType:TERRAString; Virtual;
      Class Function CanBePooled:Boolean; Virtual;

      Function GetBlob():TERRAString; Virtual;
      Procedure SetBlob(Const Blob:TERRAString);Virtual;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Virtual;
      Function CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject; Virtual;

      Function FindProperty(Const KeyName:TERRAString):TERRAObject;
      Function FindPropertyWithPath(Path:TERRAString):TERRAObject;

      Function HasActiveTweens:Boolean; Virtual;
      Function HasProperties():Boolean;

      Procedure CopyProperties(Other:TERRAObject);

      { Returns a ID to be used for sorting. }
      Function SortID:Integer; Virtual;

      Destructor Destroy; Override;

      Property Name:TERRAString Read _ObjectName Write SetObjectName;
  End;

  TERRAObjectType = Class Of TERRAObject;

  BooleanProperty = Class(TERRAObject)
    Protected
      _Value:Boolean;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Boolean);

      Class Function GetObjectType:TERRAString; Override;
      Class Function CanBePooled:Boolean; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:Boolean Read _Value Write _Value;
  End;

  StringProperty = Class(TERRAObject)
    Protected
      _Value:TERRAString;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:TERRAString);

      Class Function GetObjectType:TERRAString; Override;
      Class Function CanBePooled:Boolean; Override;

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

      Class Function CanBePooled:Boolean; Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Virtual; Abstract;

      Function HasActiveTweens:Boolean; Override;
  End;

  IntegerProperty = Class(TweenableProperty)
    Protected
      _Value:Integer;

      Function GetIntegerValue: Integer;
      Procedure SetIntegerValue(const NewValue:Integer);

      Procedure UpdateTweenValue(Const TweenID:Integer; Const Value:Single); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Integer);

      Class Function GetObjectType:TERRAString; Override;

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
      Class Function GetObjectType:TERRAString; Override;

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
      Class Function GetObjectType:TERRAString; Override;

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
      Class Function GetObjectType:TERRAString; Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;
  End;


Procedure ReleaseObject(var Obj; AllowReuse:Boolean = True);

Implementation
Uses TERRA_Log, TERRA_Utils, TERRA_Math, TERRA_OS, TERRA_String, TERRA_Engine;

Procedure ReleaseObject(Var Obj; AllowReuse:Boolean);
Var
  Temp:TObject;
Begin
  Temp := TERRAObject(Obj);
  If Temp = Nil Then
    Exit;

  If (Temp Is TERRAObject) Then
  Begin
    TERRAObject(Temp).Release();

    If (AllowReuse) And (TERRAObject(Temp).CanBePooled) {And (Assigned(Engine()))} Then
      Engine.Pool.Recycle(TERRAObject(Temp))
    Else
      TERRAObject(Temp).Destroy();
  End Else
    Engine.Log.Write(logWarning, 'App', Temp.ClassName +' is not a TERRA-Object!');

  Pointer(Obj) := Nil;
End;


{ TERRAObject }
Constructor TERRAObject.Create;
Begin
//  Self._ObjectName := Self.ClassName;
End;

Procedure TERRAObject.Release;
Begin
{  S := Self.ClassName;
  Engine.Log.Write(logWarning, 'App', 'Destroying instance of '+S);}
End;

Function TERRAObject.SortID: Integer;
Begin
  Result := 0;
End;


Destructor TERRAObject.Destroy();
Begin
  {$IFDEF WINDOWS}
 // DebugBreak();
//  RaiseError('Destructors are not allowed in class: '+Self.ClassName);
  {$ENDIF}

  Inherited;
End;

Class Function TERRAObject.GetObjectType: TERRAString;
Begin
  Result := Self.ClassName;
End;

Procedure TERRAObject.SetObjectName(const Value: TERRAString);
Begin
  Self._ObjectName := Value;
End;

Function TERRAObject.HasActiveTweens: Boolean;
Var
  I:Integer;
  Key:TERRAObject;
Begin
  I := 0;
  Repeat
    Key := Self.GetPropertyByIndex(I);
    Inc(I);

    If (Assigned(Key)) And (Key.HasActiveTweens()) Then
    Begin
      Key.GetBlob();
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

    Temp := KeyB.GetBlob();
    If Temp<>'' Then
      KeyA.SetBlob(Temp);
      
    KeyA.CopyProperties(KeyB);
  Until False;
End;

Function TERRAObject.CreateProperty(Const KeyName, ObjectType:TERRAString):TERRAObject;
Begin
  Result := Application.Instance.CreateProperty(KeyName, ObjectType);
  If Assigned(Result) Then
    Exit;

  Engine.Log.Write(logError, 'Application', 'Cannot unserialize object of type ' +ObjectType+' with name '+KeyName);
End;

Function TERRAObject.FindPropertyWithPath(Path:TERRAString):TERRAObject;
Var
  S:TERRAString;
Begin
  S := StringGetNextSplit(Path, '.');
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

Function TERRAObject.HasProperties: Boolean;
Begin
  Result := Assigned(Self.GetPropertyByIndex(0));
End;

Class Function TERRAObject.CanBePooled: Boolean;
Begin
  Result := False;
End;

{ TweenableProperty }
Class Function TweenableProperty.CanBePooled: Boolean;
Begin
  Result := True;
End;

Function TweenableProperty.HasActiveTweens:Boolean;
Begin
  Self.UpdateTweens();
  Result := (_TweenCount>0);
End;

Procedure TweenableProperty.RegisterTween(Const Ease:TweenEaseType; TweenID:Integer; Const StartValue, TargetValue:Single; Const Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Var
  T:TweenObject;
Begin
  If (StartValue = TargetValue) Then
  Begin
    If Assigned(Callback) Then
      Callback(CallTarget);

    Exit;
  End;

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
  Begin
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

        End;
    End;

    If (_TweenList[I].State = tweenFinished) Then
    Begin
      _TweenList[I] := _TweenList[Pred(_TweenCount)];
      Dec(_TweenCount);
    End Else
      Inc(I);
  End;
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

Class Function IntegerProperty.GetObjectType: TERRAString;
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

Class Function ByteProperty.GetObjectType: TERRAString;
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

Class Function FloatProperty.GetObjectType: TERRAString;
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

  If (X = 0.0) Then
  Begin
    Result := Result + '0';
    Exit;
  End;

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

Class Function BooleanProperty.GetObjectType: TERRAString;
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

Class Function BooleanProperty.CanBePooled: Boolean;
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

Class Function StringProperty.GetObjectType: TERRAString;
Begin
  Result := 'string';
End;

Class Function StringProperty.CanBePooled: Boolean;
Begin
  Result := True;
End;

{ AngleProperty }
Class Function AngleProperty.GetObjectType: TERRAString;
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
