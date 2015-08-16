Unit TERRA_Object;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Tween, TERRA_Color, TERRA_Vector2D, TERRA_Vector3D;

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

      Procedure UpdateTweens(); Virtual;

      Procedure UpdateTweenValue(Const TweenID:Integer; Const Value:Single); Virtual;

    Public
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

      Property Value:Single Read GetFloatValue Write SetFloatValue;

  End;

  AngleProperty = Class(FloatProperty)
    Public
      Function GetObjectType:TERRAString; Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;
  End;

  ColorProperty = Class(TweenableProperty)
    Protected
      _Red:ByteProperty;
      _Green:ByteProperty;
      _Blue:ByteProperty;
      _Alpha:ByteProperty;

      Function GetColorValue:ColorRGBA;
      Procedure SetColorValue(const NewValue:ColorRGBA);

      Procedure UpdateTweens(); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:ColorRGBA);
      Procedure Release(); Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:ColorRGBA; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Red:ByteProperty Read _Red;
      Property Green:ByteProperty Read _Green;
      Property Blue:ByteProperty Read _Blue;
      Property Alpha:ByteProperty Read _Alpha;

      Property Value:ColorRGBA Read GetColorValue Write SetColorValue;
  End;

  Vector2DProperty = Class(TweenableProperty)
    Protected
      _X:FloatProperty;
      _Y:FloatProperty;

      Function GetVectorValue:Vector2D;
      Procedure SetVectorValue(const NewValue:Vector2D);

      Procedure UpdateTweens(); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Vector2D);
      Procedure Release(); Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Vector2D; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetObjectType:TERRAString; Override;


      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property X:FloatProperty Read _X;
      Property Y:FloatProperty Read _Y;

      Property Value:Vector2D Read GetVectorValue Write SetVectorValue;
  End;

  Vector3DProperty = Class(TweenableProperty)
    Protected
      _X:FloatProperty;
      _Y:FloatProperty;
      _Z:FloatProperty;

      Function GetVectorValue:Vector3D;
      Procedure SetVectorValue(const NewValue:Vector3D);

      Procedure UpdateTweens(); Override;

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Vector3D);
      Procedure Release(); Override;

      Procedure AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil); Override;
      Procedure AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Vector3D; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property X:FloatProperty Read _X;
      Property Y:FloatProperty Read _Y;
      Property Z:FloatProperty Read _Z;

      Property Value:Vector3D Read GetVectorValue Write SetVectorValue;
  End;

Procedure ReleaseObject(var Obj);

Implementation
Uses TERRA_Log, TERRA_OS, TERRA_Math;

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
Begin
  Result := Application.Instance.CreateProperty(Self, KeyName, ObjectType);
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

  If Duration = 150 Then
    IntToString(2);

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
  Result := IntToString(Value);
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

{ ByteProperty }
Constructor ByteProperty.Create(const Name: TERRAString; const InitValue: Byte);
Begin
  Self._ObjectName := Name;
  Self._Value := InitValue;
End;

Function ByteProperty.GetBlob: TERRAString;
Begin
  Result := IntToString(Value);
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
  Result := FloatToString(GetFloatValue());
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

{ ColorProperty }
Constructor ColorProperty.Create(Const Name:TERRAString; const InitValue:ColorRGBA);
Begin
  _ObjectName := Name;
  _Red := ByteProperty.Create('r', InitValue.R);
  _Green := ByteProperty.Create('g', InitValue.G);
  _Blue := ByteProperty.Create('b', InitValue.B);
  _Alpha := ByteProperty.Create('a', InitValue.A);
End;

Procedure ColorProperty.Release;
Begin
  ReleaseObject(_Red);
  ReleaseObject(_Green);
  ReleaseObject(_Blue);
  ReleaseObject(_Alpha);
End;

(*Function ColorProperty.GetBlob: TERRAString;
Begin
  Result := Red.GetBlob() + '/'+ Green.GetBlob() + '/'+ Blue.GetBlob() + '/'+ Alpha.GetBlob();
End;

Procedure ColorProperty.SetBlob(const Blob: TERRAString);
Var
  S:TERRAString;
Begin
  S := Blob;
  Red.SetBlob(StringGetNextSplit(S, Ord('/')));
  Green.SetBlob(StringGetNextSplit(S, Ord('/')));
  Blue.SetBlob(StringGetNextSplit(S, Ord('/')));
  Alpha.SetBlob(StringGetNextSplit(S, Ord('/')));
End;*)

Procedure ColorProperty.SetColorValue(const NewValue:ColorRGBA);
Begin
  Red.Value := NewValue.R;
  Green.Value := NewValue.G;
  Blue.Value := NewValue.B;
  Alpha.Value := NewValue.A;
End;

Function ColorProperty.GetColorValue:ColorRGBA;
Begin
  Result.R := Red.Value;
  Result.G := Green.Value;
  Result.B := Blue.Value;
  Result.A := Alpha.Value;
End;

Function ColorProperty.GetObjectType: TERRAString;
Begin
  Result := 'color';
End;

Procedure ColorProperty.AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.AddTween(Ease, ColorCreateFromString(StartValue), ColorCreateFromString(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure ColorProperty.AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:ColorRGBA; Duration, Delay:Cardinal; Callback: TweenCallback; CallTarget:TERRAObject);
Begin
  Self.Red.AddTween(Ease, StartValue.R, TargetValue.R, Duration, Delay, Callback, CallTarget);
  Self.Green.AddTween(Ease, StartValue.G, TargetValue.G, Duration, Delay, Nil);
  Self.Blue.AddTween(Ease, StartValue.B, TargetValue.B, Duration, Delay, Nil);
  Self.Alpha.AddTween(Ease, StartValue.A, TargetValue.A, Duration, Delay, Nil);
End;

Function ColorProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := Red;
  1:  Result := Green;
  2:  Result := Blue;
  3:  Result := Alpha;
  Else
    Result := Nil;
  End;
End;

Function ColorProperty.GetBlob: TERRAString;
Begin
  Result := ColorToString(Self.GetColorValue());
End;

Procedure ColorProperty.SetBlob(const Blob: TERRAString);
Begin
  Self.SetColorValue(ColorCreateFromString(Blob));
End;

Procedure ColorProperty.UpdateTweens;
Begin
  Red.UpdateTweens();
  Green.UpdateTweens();
  Blue.UpdateTweens();
  Alpha.UpdateTweens();
End;

{ Vector3DProperty }
Constructor Vector3DProperty.Create(Const Name:TERRAString; const InitValue: Vector3D);
Begin
  _ObjectName := Name;
  _X := FloatProperty.Create('x', InitValue.X);
  _Y := FloatProperty.Create('y', InitValue.Y);
  _Z := FloatProperty.Create('z', InitValue.Z);
End;

Procedure Vector3DProperty.Release;
Begin
  ReleaseObject(_X);
  ReleaseObject(_Y);
  ReleaseObject(_Z);
End;

Function Vector3DProperty.GetVectorValue: Vector3D;
Begin
  Result.X := X.Value;
  Result.Y := Y.Value;
  Result.Z := Z.Value;
End;

Procedure Vector3DProperty.SetVectorValue(const NewValue: Vector3D);
Begin
  X.Value := NewValue.X;
  Y.Value := NewValue.Y;
  Z.Value := NewValue.Z;
End;

Function Vector3DProperty.GetObjectType: TERRAString;
Begin
  Result := 'vec3';
End;

Procedure Vector3DProperty.AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.AddTween(Ease, StringToVector3D(StartValue), StringToVector3D(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure Vector3DProperty.AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Vector3D; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.X.AddTween(Ease, StartValue.X, TargetValue.X, Duration, Delay, Callback, CallTarget);
  Self.Y.AddTween(Ease, StartValue.Y, TargetValue.Y, Duration, Delay, Nil);
  Self.Z.AddTween(Ease, StartValue.Z, TargetValue.Z, Duration, Delay, Nil);
End;

Function Vector3DProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := X;
  1:  Result := Y;
  2:  Result := Z;
  Else
    Result := Nil;
  End;
End;

Procedure Vector3DProperty.UpdateTweens;
Begin
  X.UpdateTweens();
  Y.UpdateTweens();
  Z.UpdateTweens();
End;

{ Vector2DProperty }
Constructor Vector2DProperty.Create(Const Name:TERRAString; const InitValue: Vector2D);
Begin
  _ObjectName := Name;
  _X := FloatProperty.Create('x', InitValue.X);
  _Y := FloatProperty.Create('y', InitValue.Y);
End;

Procedure Vector2DProperty.Release;
Begin
  ReleaseObject(_X);
  ReleaseObject(_Y);
End;

(*Function Vector2DProperty.GetBlob: TERRAString;
Begin
  Result := X.GetBlob() + '/'+ Y.GetBlob();
End;

Procedure Vector2DProperty.SetBlob(const Blob: TERRAString);
Var
  S:TERRAString;
Begin
  S := Blob;
  X.SetBlob(StringGetNextSplit(S, Ord('/')));
  Y.SetBlob(StringGetNextSplit(S, Ord('/')));
End;*)

Function Vector2DProperty.GetVectorValue: Vector2D;
Begin
  Result.X := X.Value;
  Result.Y := Y.Value;
End;

Procedure Vector2DProperty.SetVectorValue(const NewValue: Vector2D);
Begin
  X.Value := NewValue.X;
  Y.Value := NewValue.Y;
End;

Function Vector2DProperty.GetObjectType: TERRAString;
Begin
  Result := 'vec2';
End;

Procedure Vector2DProperty.AddTweenFromBlob(Const Ease:TweenEaseType; Const StartValue, TargetValue:TERRAString; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.AddTween(Ease, StringToVector2D(StartValue), StringToVector2D(TargetValue), Duration, Delay, Callback, CallTarget);
End;

Procedure Vector2DProperty.AddTween(Const Ease:TweenEaseType; Const StartValue, TargetValue:Vector2D; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.X.AddTween(Ease, StartValue.X, TargetValue.X, Duration, Delay, Callback, CallTarget);
  Self.Y.AddTween(Ease, StartValue.Y, TargetValue.Y, Duration, Delay, Nil);
End;

Function Vector2DProperty.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
  0:  Result := X;
  1:  Result := Y;
  Else
    Result := Nil;
  End;
End;

Procedure Vector2DProperty.UpdateTweens;
Begin
  X.UpdateTweens();
  Y.UpdateTweens();
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
  Result := FloatToString(GetFloatValue() * DEG);
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
