Unit TERRA_Packer;
{$I Terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_ProgressNotifier;

Type
  PackerNode = Class;

  PPackerRect = ^PackerRect;
  PackerRect = Record
		ID:Integer;
		Width:Integer;
		Height:Integer;
		Done:Boolean;
		Node:PackerNode;
  End;

  PackerNode = Class(TERRAObject)
      Child:Array[0..1] Of PackerNode;
      X:Integer;
	    Y:Integer;
	    Width:Integer;
	    Height:Integer;
      Rect:PPackerRect;

	    Constructor Create(X, Y, Width, Height:Integer);
      Procedure Release; Override;

	    Function Insert(Rect:PPackerRect):PackerNode;
  End;

  RectanglePacker = Class(TERRAObject)
    Protected
		  _Root:PackerNode;
		  _RectList:Array Of PackerRect;
      _RectCount:Integer;

	  Public
		  Constructor Create;
		  Procedure Release; Override;

		  Procedure AddRect(Width, Height, ID:Integer);

		  Function GetRect(ID:Integer; Var X,Y:Integer):Boolean;
      Procedure DeleteRect(ID:Integer);


		  Function Pack(Width, Height:Integer; Callback:ProgressNotifier=Nil):Integer;

      Property RectCount:Integer Read _RectCount;
  End;

Implementation
Uses TERRA_Error, TERRA_Application;

Constructor PackerNode.Create(X, Y, Width, Height:Integer);
Begin
  Self.X := X;
	Self.Y := Y;
	Self.Width := Width;
	Self.Height := Height;
	Self.Rect := Nil;
	Self.Child[0] := Nil;
	Self.Child[1] := Nil;
End;

Procedure PackerNode.Release;
Var
  I:Integer;
Begin
  For I:=0 To 1 Do
    ReleaseObject(Child[I]);
End;

Function PackerNode.Insert(Rect:PPackerRect):PackerNode;
Var
  DW,DH:Integer;
Begin
  If (Child[0] <> Nil) And (Child[1] <> Nil) Then
	Begin
		Result := Child[0].Insert(rect);
    If (Result <> Nil) Then
			Exit;

    Result := Child[1].Insert(rect);
    Exit;
	End Else
	Begin
		//if there's already a lightmap here, return
		If (Self.Rect <> Nil) Then
		Begin
      Result := Nil;
      Exit;
    End;

        // if we're too small, return
		If (Rect.Width > Self.Width) Or (Rect.Height > Self.Height) Then
    Begin
      Result := Nil;
      Exit;
    End;

    // if we're just right, accept
    If (Rect.Width = Self.Width) And (Rect.Height = Self.Height) Then
		Begin
			Rect.Done := True;
			Self.Rect := Rect;
      Result := Self;
      Exit;
		End;

    // otherwise, gotta split this node and create some kids

		//decide which way to split
    Dw := Self.Width - Rect.Width;
    Dh := Self.Height - Rect.Height;

		If (dw > dh) Then
		Begin
			Self.Child[0] := PackerNode.Create(Self.x, Self.y, rect.width, Self.height);
			Self.Child[1] := PackerNode.Create(Self.x + Rect.width, Self.y, Self.Width - (rect.width ), Self.height);
		End Else
		Begin
			Self.child[0] := PackerNode.Create(Self.x, Self.y, Self.width , rect.height);
			Self.child[1] := PackerNode.Create(Self.x, Self.y + rect.height, Self.width, Self.height - (rect.height));
		End;

    //insert into first child we created
    Result := Child[0].Insert(rect);
	End;
End;


Procedure RectanglePacker.AddRect(Width, Height, ID:Integer);
Var
  Rect:PPackerRect;
Begin
	If (Width<1) Or (Height<1) Then
	Begin
		RaiseError('Invalid rect!');
		Exit;
	End;

  Inc(_RectCount);
  SetLength(_RectList, _RectCount);
  Rect := @(_RectList[Pred(_RectCount)]);

	Rect.Width := Width;
	Rect.Height := Height;
	Rect.ID := ID;
	Rect.Node := Nil;
	Rect.Done := False;
End;

Procedure RectanglePacker.DeleteRect(ID:Integer);
Var
  I,N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_RectCount) Do
  If (_RectList[I].ID = ID) Then
  Begin
    N := I;
    Break;
	End;

  If (N>=0) Then
  Begin
    _RectList[N] := _RectList[Pred(_RectCount)];
    Dec(_RectCount);
  End;
End;

Function RectanglePacker.GetRect(ID:Integer; Var X,Y:Integer):Boolean;
Var
  I,N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_RectCount) Do
  If (_RectList[I].ID = ID) Then
  Begin
    N := I;
    Break;
	End;

  If (N<0) Or (_RectList[N].Node = Nil) Or (Not _RectList[N].Done) Then
  Begin
    Result := False;
  	Exit;
  End;

	X :=  _RectList[N].Node.X;
	Y :=  _RectList[N].Node.Y;
  Result := True;
End;

Constructor RectanglePacker.Create;
Begin
	_Root := Nil;
  _RectCount := 0;
End;

Procedure RectanglePacker.Release;
Begin
  ReleaseObject(_Root);
End;

Function RectanglePacker.Pack(Width, Height:Integer; Callback:ProgressNotifier=Nil):Integer;
Var
  I, K, Count:Integer;
  Max, Index:Integer;
Begin
	Count := _RectCount;

  If Assigned(Callback) Then
    Callback.Reset(1);

  Index := 0;

	While (Count>0) Do
	Begin
    If Assigned(Callback) Then
      Callback.Notify(1.0 - (Count/_RectCount));
      
	  Max := 0;
		For I:=0 To Pred(_RectCount) Do
		Begin
			If (_RectList[I].Done) Then
				Continue;

			K := _RectList[I].Width * _RectList[I].Height;
			If (K>Max) Then
			Begin
				Max := K;
				Index := I;
			End;
		End;

		If (_Root=Nil) Then
      _Root := PackerNode.Create(0,0, Width, Height);

		_RectList[Index].Node := _Root.Insert(@(_RectList[Index]));
    If (_RectList[Index].Node = Nil) Then
    Begin
      Result := Count;
      Exit;
    End;

		Dec(Count);
	End;

	Result := Count;
End;


End.
