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
      _CurrentValue:Double;

      _TweenList:Array Of TweenObject;
      _TweenCount:Integer;

      Function GetCurrentValue:Single;
      Procedure SetCurrentValue(Const NewValue:Single);

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Single);

      Procedure AddTween(Ease:TweenEaseType; TargetValue:Single; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function IsValueObject():Boolean; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Function HasPropertyTweens:Boolean; Override;

      Property Value:Single Read GetCurrentValue Write SetCurrentValue;
  End;

  IntegerProperty = Class(TweenableProperty)
    Protected
      Function GetIntegerValue: Integer;
      Procedure SetIntegerValue(const NewValue:Integer);

    Public
      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:Integer Read GetIntegerValue Write SetIntegerValue;
  End;

  ByteProperty = Class(TweenableProperty)
    Protected
      Function GetByteValue:Byte;
      Procedure SetByteValue(const NewValue:Byte);

    Public
      Function GetObjectType:TERRAString; Override;

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Property Value:Byte Read GetByteValue Write SetByteValue;
  End;

  FloatProperty = Class(TweenableProperty)
    Public
      Function GetObjectType:TERRAString; Override;
  End;

  AngleProperty = Class(FloatProperty)
    Public
      Function GetObjectType:TERRAString; Override;
  End;

  ColorProperty = Class(TERRAObject)
    Protected
      _Red:ByteProperty;
      _Green:ByteProperty;
      _Blue:ByteProperty;
      _Alpha:ByteProperty;

      Function GetColorValue:Color;
      Procedure SetColorValue(const NewValue:Color);

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Color);
      Procedure Release(); Override;

      Procedure AddTween(Ease:TweenEaseType; Const TargetValue:Color; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetBlob():TERRAString; Override;
      Procedure SetBlob(Const Blob:TERRAString); Override;

      Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property Red:ByteProperty Read _Red;
      Property Green:ByteProperty Read _Green;
      Property Blue:ByteProperty Read _Blue;
      Property Alpha:ByteProperty Read _Alpha;

      Property Value:Color Read GetColorValue Write SetColorValue;
  End;

  Vector2DProperty = Class(TERRAObject)
    Protected
      _X:FloatProperty;
      _Y:FloatProperty;

      Function GetVectorValue:Vector2D;
      Procedure SetVectorValue(const NewValue:Vector2D);

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Vector2D);
      Procedure Release(); Override;

      Procedure AddTween(Ease:TweenEaseType; Const TargetValue:Vector2D; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetObjectType:TERRAString; Override;


      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property X:FloatProperty Read _X;
      Property Y:FloatProperty Read _Y;

      Property Value:Vector2D Read GetVectorValue Write SetVectorValue;
  End;

  Vector3DProperty = Class(TERRAObject)
    Protected
      _X:FloatProperty;
      _Y:FloatProperty;
      _Z:FloatProperty;

      Function GetVectorValue:Vector3D;
      Procedure SetVectorValue(const NewValue:Vector3D);

    Public
      Constructor Create(Const Name:TERRAString; Const InitValue:Vector3D);
      Procedure Release(); Override;

      Procedure AddTween(Ease:TweenEaseType; Const TargetValue:Vector3D; Duration:Cardinal; Delay:Cardinal = 0; Callback:TweenCallback = Nil; CallTarget:TERRAObject = Nil);

      Function GetObjectType:TERRAString; Override;

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property X:FloatProperty Read _X;
      Property Y:FloatProperty Read _Y;
      Property Z:FloatProperty Read _Z;

      Property Value:Vector3D Read GetVectorValue Write SetVectorValue;
  End;

Procedure ReleaseObject(var Obj);

Implementation
Uses TERRA_Log, TERRA_OS;

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

    Temp := KeyB.GetBlob();
    KeyA.SetBlob(Temp);
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
Constructor TweenableProperty.Create(Const Name:TERRAString; const InitValue: Single);
Begin
  _ObjectName := Name;
  _CurrentValue := InitValue;
  _TweenCount := 0;
End;

Function TweenableProperty.GetBlob: TERRAString;
Begin
  Result := FloatToString(GetCurrentValue());
End;

Procedure TweenableProperty.SetBlob(const Blob: TERRAString);
Begin
  SetCurrentValue(StringToFloat(Blob));
End;

Function TweenableProperty.HasPropertyTweens:Boolean;
Begin
  Self.GetCurrentValue();
  Result := (_TweenCount>0);
End;

Procedure TweenableProperty.AddTween(Ease:TweenEaseType; TargetValue:Single; Duration:Cardinal; Delay:Cardinal; Callback:TweenCallback; CallTarget:TERRAObject);
Var
  T:TweenObject;
Begin
  T.StartTime := Application.GetTime() + Delay;
  T.EndTime := T.StartTime + Duration;
  T.State := tweenWaiting;
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

Function TweenableProperty.GetCurrentValue: Single;
Var
  I:Integer;
  T:Cardinal;
  Delta:Single;
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
        _TweenList[I].StartValue := Self._CurrentValue;
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
        _CurrentValue := (_TweenList[I].TargetValue * Delta) + (_TweenList[I].StartValue * (1.0 - Delta));

        //Inc(I);
        Break;
      End;

  Else
    Begin
      _TweenList[I] := _TweenList[Pred(_TweenCount)];
      Dec(_TweenCount);
    End;
  End;

  Result := _CurrentValue;
End;

Procedure TweenableProperty.SetCurrentValue(const NewValue: Single);
Begin
  _CurrentValue := NewValue;
End;

Function TweenableProperty.IsValueObject: Boolean;
Begin
  Result := True;
End;

{ IntegerProperty }
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
  Result := Round(GetCurrentValue());
End;

Procedure IntegerProperty.SetIntegerValue(const NewValue: Integer);
Begin
  SetCurrentValue(NewValue);
End;

Function IntegerProperty.GetObjectType: TERRAString;
Begin
  Result := 'number';
End;

{ ByteProperty }
Function ByteProperty.GetBlob: TERRAString;
Begin
  Result := IntToString(Value);
End;

Procedure ByteProperty.SetBlob(const Blob: TERRAString);
Begin
  Value := StringToInt(Blob);
End;

Function ByteProperty.GetByteValue:Byte;
Var
  N:Integer;
Begin
  N := Round(GetCurrentValue());

  If N>255 Then
    N := 255
  Else
  If (N<0) Then
    N := 0;

  Result := N;
End;

Procedure ByteProperty.SetByteValue(const NewValue: Byte);
Begin
  SetCurrentValue(NewValue);
End;

Function ByteProperty.GetObjectType: TERRAString;
Begin
  Result := 'byte';
End;

{ ColorProperty }
Constructor ColorProperty.Create(Const Name:TERRAString; const InitValue: Color);
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

Procedure ColorProperty.SetColorValue(const NewValue: Color);
Begin
  Red.Value := NewValue.R;
  Green.Value := NewValue.G;
  Blue.Value := NewValue.B;
  Alpha.Value := NewValue.A;
End;

Function ColorProperty.GetColorValue: Color;
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

Procedure ColorProperty.AddTween(Ease:TweenEaseType; Const TargetValue:Color; Duration, Delay:Cardinal; Callback: TweenCallback; CallTarget:TERRAObject);
Begin
  Self.Red.AddTween(Ease, TargetValue.R, Duration, Delay, Callback, CallTarget);
  Self.Green.AddTween(Ease, TargetValue.G, Duration, Delay, Nil);
  Self.Blue.AddTween(Ease, TargetValue.B, Duration, Delay, Nil);
  Self.Alpha.AddTween(Ease, TargetValue.A, Duration, Delay, Nil);
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

(*Function Vector3DProperty.GetBlob: TERRAString;
Begin
  Result := X.GetBlob() + '/'+ Y.GetBlob() + '/'+ Z.GetBlob();
End;

Procedure Vector3DProperty.SetBlob(const Blob: TERRAString);
Var
  S:TERRAString;
Begin
  S := Blob;
  X.SetBlob(StringGetNextSplit(S, Ord('/')));
  Y.SetBlob(StringGetNextSplit(S, Ord('/')));
  Z.SetBlob(StringGetNextSplit(S, Ord('/')));
End;*)

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

Procedure Vector3DProperty.AddTween(Ease:TweenEaseType; const TargetValue:Vector3D; Duration, Delay:Cardinal;  Callback:TweenCallback; CallTarget:TERRAObject);
Begin
  Self.X.AddTween(Ease, TargetValue.X, Duration, Delay, Callback, CallTarget);
  Self.Y.AddTween(Ease, TargetValue.Y, Duration, Delay, Nil);
  Self.Z.AddTween(Ease, TargetValue.Z, Duration, Delay, Nil);
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

Procedure Vector2DProperty.AddTween(Ease: TweenEaseType; const TargetValue: Vector2D; Duration, Delay: Cardinal; Callback: TweenCallback; CallTarget:TERRAObject);
Begin
  Self.X.AddTween(Ease, TargetValue.X, Duration, Delay, Callback, CallTarget);
  Self.Y.AddTween(Ease, TargetValue.Y, Duration, Delay, Nil);
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

{ FloatProperty }

Function FloatProperty.GetObjectType: TERRAString;
Begin
  Result := 'float';
End;

{ AngleProperty }

Function AngleProperty.GetObjectType: TERRAString;
Begin
  Result := 'angle';
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

End.
