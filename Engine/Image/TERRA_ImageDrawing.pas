Unit TERRA_ImageDrawing;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Image, TERRA_Color;

Const
  Kernel_Default:ImageKernel = (0, 0, 0, 0, 1, 0, 0, 0, 0);
  Kernel_EdgeDetect:ImageKernel = (-1, -1, -1, -1, 8, -1, -1, -1, -1);
  Kernel_Smooth:ImageKernel = (1, 1, 1, 1, 2, 1, 1, 1, 1);
  Kernel_Sharpen:ImageKernel = (-1, -1, -1, -1, 9, -1, -1, -1, -1);
  Kernel_Emboss:ImageKernel = (0, 0, -2, 0, 2, 0, 1, 0, 0);
  Kernel_Blur:ImageKernel = (0, 0, 1, 0, 0, 0, 1, 0, 0);
  //Kernel_Dilate:ImageKernel = (0, 1, 0, 1, 1, 1, 0, 1, 0);
  Kernel_Dilate:ImageKernel = (1, 1, 1, 1, 1, 1, 1, 1, 1);


Type
  FullImageIterator = Class(ImageIterator)
    Protected
      _Width:Integer;
      _Count:Integer;

      _PX:Integer;
      _PY:Integer;

      Function ObtainNext():Boolean; Override;

    Public
      Constructor Create(Target:Image; Flags:ImageProcessFlags; Const Mask:Cardinal);
  End;

  CircleImageIterator = Class(ImageIterator)
    Protected
      _XCenter:Integer;
      _YCenter:Integer;

      _PX:Integer;
      _PY:Integer;
      _PR:Integer;
      _PH:Integer;
      _Sub:Integer;

      Function ObtainNext():Boolean; Override;

    Public
      Constructor Create(Target:Image; XCenter, YCenter, Radius:Integer; Flags:ImageProcessFlags; Const Mask:Cardinal);
  End;


  RectImageIterator = Class(ImageIterator)
    Protected
      _PX:Integer;
      _PY:Integer;

      _X1:Integer;
      _Y1:Integer;

      _X2:Integer;
      _Y2:Integer;

      Function ObtainNext():Boolean; Override;

    Public
      Constructor Create(Target:Image; X1, Y1, X2, Y2:Integer; Flags:ImageProcessFlags; Const Mask:Cardinal);
  End;

  LineImageIterator = Class(ImageIterator)
    Protected
      _X1:Integer;
      _Y1:Integer;

      _X2:Integer;
      _Y2:Integer;

      _PX:Integer;
      _PY:Integer;

      _NumPixels:Integer;
      _Dir:Integer;
      _Dinc1:Integer;
      _Dinc2:Integer;

      _XInc1:Integer;
      _XInc2:Integer;
      _YInc1:Integer;
      _YInc2:Integer;

      _Sub:Integer;

      Function ObtainNext():Boolean; Override;

    Public
      Constructor Create(Target:Image; X1, Y1, X2, Y2:Integer; Flags:ImageProcessFlags; Const Mask:Cardinal);
  End;

Implementation

{ CircleImageIterator }
Constructor CircleImageIterator.Create(Target:Image; XCenter, YCenter, Radius:Integer; Flags:ImageProcessFlags; Const Mask:Cardinal);
Begin
  Inherited Create(Target, Flags, Mask);
  Self._XCenter := XCenter;
  Self._YCenter := YCenter;

  If (image_Fill In Flags) Then
  Begin
    _PX := -Radius;
    _PY := 0;
    _PR := Radius;
  End Else
  Begin
    _PX := 0;
    _PY := Radius;
    _PR := 3 - Radius * 2;
  End;

  _PH := 0;
  _Sub := 0;
End;

// Bresenham's circle algorithm
Function CircleImageIterator.ObtainNext():Boolean;
Begin
  If (image_Fill In _Flags) Then
  Begin
    If (_PH<=0) Or (_PY>=_PH) Then
    Begin
      Inc(_PX);
      _PH := Trunc(Sqrt((Sqr(_PR) - Sqr(_PX))));
      _PY := -_PH;
    End;

    If (_PX>=_PR) Then
    Begin
      Result := False;
      Exit;
    End;

    _X := _XCenter + _PX;
    _Y := _YCenter + _PY;
    Inc(_PY);

    Result := True;

    Exit;
  End;


  If (_PX > _PY) Then
  Begin
    Result := False;
    Exit;
  End;


  Case _Sub Of
  0:Begin
      _X := _xCenter + _PX;
      _Y := _yCenter + _PY;
    End;

  1:Begin
      _X := _xCenter - _PX;
      _Y := _yCenter + _PY;
    End;

  2:Begin
      _X := _xCenter + _PX;
      _Y := _yCenter - _PY;
    End;

  3:Begin
      _X := _xCenter - _PX;
      _Y := _yCenter - _PY;
    End;

  4:Begin
      _X := _xCenter + _PY;
      _Y := _yCenter + _PX;
    End;

  5:Begin
      _X := _xCenter - _PY;
      _Y := _yCenter + _PX;
    End;

  6:Begin
      _X := _xCenter + _PY;
      _Y := _yCenter - _PX;
    End;

  7:Begin
      _X := _xCenter - _PY;
      _Y := _yCenter - _PX;
    End;
  End;

  Inc(_Sub);
  If (_Sub>=8) Then
  Begin
    _Sub := 0;
    If (_PR<0) Then
    Begin
      Inc(_PX);
      Inc(_PR, 4 * _PX + 6);
    End Else
    Begin
      Inc(_PX);
      Dec(_PY);
      Inc(_PR, 4*(_PX - _PY)+10);
    End;
  End;

  Result := True;
End;

{ RectImageIterator }
Constructor RectImageIterator.Create(Target:Image; X1, Y1, X2, Y2: Integer; Flags: ImageProcessFlags; Const Mask:Cardinal);
Begin
  Inherited Create(Target, Flags, Mask);

  Self._X1 := IntMax(X1,0);
  Self._X2 := IntMin(X2,Integer(Pred(Target.Width)));

  Self._Y1 := IntMax(Y1,0);
  Self._Y2 := IntMin(Y2,Integer(Pred(Target.Height)));

  _PX := Pred(X1);
  _PY := Y1;
End;

Function RectImageIterator.ObtainNext():Boolean;
Begin
  If (_PY > _Y2) Then
  Begin
    Result := False;
    Exit;
  End;

  _X := _PX;
  _Y := _PY;

  Inc(_PX);

  If (_PX>_X2) Then
  Begin
    _PX := _X1;
    Inc(_PY);
  End;

  Result := True;
End;

{ LineImageIterator }
Constructor LineImageIterator.Create(Target: Image; X1, Y1, X2, Y2: Integer; Flags: ImageProcessFlags; Const Mask:Cardinal);
Var
  DeltaX, DeltaY:Integer;
Begin
  Inherited Create(Target, Flags, Mask);

  Self._X1 := X1;
  Self._X2 := X2;

  Self._Y1 := Y1;
  Self._Y2 := Y2;

  //calculate deltaX and deltaY
  DeltaX:=Abs(x2-x1);
  DeltaY:=Abs(y2-y1);
  //initialize
  If (DeltaX>=DeltaY) Then
  Begin
    //If x is independent variable
    _NumPixels := Succ(DeltaX);
    _Dir := (2*DeltaY)-DeltaX;
    _DInc1 := DeltaY Shl 1;
    _DInc2 := (DeltaY-DeltaX) Shl 1;
    _XInc1 := 1;
    _XInc2 := 1;
    _YInc1 := 0;
    _YInc2 := 1;
  End Else
  Begin
    //if y is independent variable
    _NumPixels := Succ(DeltaY);
    _Dir := (2*DeltaX)-DeltaY;
    _DInc1 := DeltaX Shl 1;
    _DInc2 := (DeltaX-DeltaY) Shl 1;
    _xinc1 := 0;
    _xinc2 := 1;
    _yinc1 := 1;
    _yinc2 := 1;
  End;

  //move in the right direction
  If (X1>X2) Then
  Begin
    _XInc1 := -_XInc1;
    _XInc2 := -_Xinc2;
  End;

  If (Y1>Y2) Then
  Begin
    _YInc1 := -_YInc1;
    _YInc2 := -_YInc2;
  End;

  _PX := X1;
  _PY := Y1;
  _Sub := 0;

  Dec(_NumPixels, 2);
End;

// Bresenham's line algorithm
Function LineImageIterator.ObtainNext:Boolean;
Begin
  If (_NumPixels<=0) Then
  Begin
    Result := False;
    Exit;
  End;

  Dec(_NumPixels);

  _X := _PX;
  _Y := _PY;

  If (_Dir<0) Then
  Begin
    Inc(_Dir, _DInc1);
    Inc(_PX, _XInc1);
    Inc(_PY, _YInc1);
  End Else
  Begin
    Inc(_Dir, _DInc2);
    Inc(_PX, _XInc2);
    Inc(_PY, _YInc2);
  End;

  Result := True;
End;

{ FullImageIterator }
Constructor FullImageIterator.Create(Target:Image; Flags:ImageProcessFlags; Const Mask:Cardinal);
Begin
  Inherited Create(Target, Flags, Mask);

  _Width := Target.Width;
  _Count := Target.Width * Target.Height;

  _PX := 0;
  _PY := 0;
End;

Function FullImageIterator.ObtainNext:Boolean;
Begin
  If (_Count<=0) Then
  Begin
    Result := False;
    Exit;
  End;

  Dec(_Count);

  _X := _PX;
  _Y := _PY;

  Inc(_PX);

  If (_PX>=_Width) Then
  Begin
    _PX := 0;
    Inc(_PY);
  End;

  Result := True;
End;


End.