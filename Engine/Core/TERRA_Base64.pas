Unit TERRA_Base64;
{$I terra.inc}


Interface
Uses TERRA_Object, TERRA_String;

Function StringToBase64(Buf:TERRAString):TERRAString;
Function Base64ToString(B64:TERRAString):TERRAString;


Implementation

Const
  Base64Code:TERRAString= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                        'abcdefghijklmnopqrstuvwxyz'+
                        '0123456789+/';
  Pad = '=';

Function StringToBase64(Buf:TERRAString):TERRAString;
Var
  I:Integer;
  x1,x2,x3,x4:Byte;
  PadCount:Integer;
Begin
  PadCount := 0;
 // we need at least 3 input bytes...
  While Length(Buf)<3 Do
  Begin
    Buf := Buf + #0;
    Inc( PadCount );
  End;
 // ...and all input must be an even multiple of 3
  While (Length( Buf ) mod 3 ) <> 0 do
  Begin
    Buf := Buf + #0; // if not, zero padding is added
    Inc(PadCount);
  End;

  Result := '';
  I := 1;

 // process 3-byte blocks or 24 bits
  While (I<=Length(Buf)-2) Do
  Begin
    // each 3 input bytes are transformed into 4 index values
    // in the range of  0..63, by taking 6 bits each step

    // 6 high bytes of first char
    x1 := ( Ord( Buf[i] ) shr 2 ) and $3F;

    // 2 low bytes of first char + 4 high bytes of second char
    x2 := ( ( Ord( Buf[i] ) shl 4 ) and $3F )
      or Ord( Buf[i + 1] ) shr 4;

    // 4 low bytes of second char + 2 high bytes of third char
    x3 := ( ( Ord( Buf[i + 1] ) shl 2 ) and $3F )
      or Ord( Buf[i + 2] ) shr 6;

    // 6 low bytes of third char
    x4 := Ord( Buf[i + 2] ) and $3F;

    // the index values point into the code array
    Result := Result + Base64Code[x1 + 1] + Base64Code[x2 + 1]
                   + Base64Code[x3 + 1] + Base64Code[x4 + 1];
    Inc(i,3);
  End;

 // if needed, finish by forcing padding chars ('=') at end of string
  If PadCount>0 Then
    For i := Length( Result ) DownTo 1 do
    Begin
      Result[i] := Pad;
      Dec(PadCount);
      If PadCount=0 Then
        Break;
    End;
End;

// helper : given a char, returns the index in code table
Function Char2IDx(c:AnsiChar):Byte;
Var
  I:Integer;
Begin
  For I:=1 To Length(Base64Code) Do
  If Base64Code[i]=C Then
  Begin
    Result := (I-1);
    Exit;
  End;
  Result := Ord(Pad);
End;

{$IFNDEF OXYGENE}
{$RANGECHECKS OFF}
{$ENDIF}
Function Base64ToString(B64:TERRAString):TERRAString;
Var
  I,PadCount:Integer;
  Block:TERRAString;
  N:Integer;
  A,B,C,D:Byte;
Begin
	// input _must_ be at least 4 chars long,
	// or multiple of 4 chars
	If (Length(B64)<4) Or (Length(B64) Mod 4<>0) Then
	Begin
		Result := '';		
		Exit;
	End;

  PadCount:=0;
  I:=Length(B64);
  // count padding chars, if any
  While (B64[i]=Pad) And (i>0) Do
  Begin
    Inc(PadCount);
    Dec(I);
  End;

  Result := '';
  I := 1;
  {$IFDEF OXYGENE}
  Block := new String('0', 3);
  {$ELSE}
  SetLength(Block, 3);
  {$ENDIF}
  While i<=Length(B64)-3 Do
  Begin
    A := Char2Idx(B64[I+0]);
    B := Char2IDx(B64[I+1]);
    C := Char2IDx(B64[I+2]);
    D := Char2IDx(B64[I+3]);

    // reverse process of above
    N := (A Shl 2) Or (B Shr 4);
    Result := Result+Chr(N);

    N := (B Shl 4) Or (C Shr 2);
    Result := Result + Chr(N);

    N := (C Shl 6 ) Or D;
    Result := Result + Chr(N);
    Inc(i,4);
  End;

  // delete padding, if any
  While PadCount>0 Do
  Begin
    Delete(Result, Length(Result), 1);
    Dec(PadCount);
  End;
End;


End.
