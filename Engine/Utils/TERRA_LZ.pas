Unit TERRA_LZ;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Stream;

{$R-}

Const
// LZSS Parameters
  _N=4096; // Size Of TERRAString buffer
  _F=60;   // Size of look-ahead buffer
  THRESHOLD=2;
  _NULL=_N; // End of tree's node

// Huffman coding parameters
  N_CHAR=(256-THRESHOLD+_F);
                           // character code (:= 0..N_CHAR-1)
  _T=(N_CHAR * 2 - 1); // Size of table
  _R=(_T - 1); // root position
  MAX_FREQ=$8000; //update when cumulative frequency reaches to this value

Type
  //TCompressedStream private types
  Freqtype=Array[0.._T] Of Word;
  FreqPtr=^FreqType;
  PntrType=Array[0..PRED(_T + N_Char)] Of SmallInt;
  PntrPtr=^PntrType;
  SonType=Array[0..PRED(_T)] Of SmallInt;
  SonPtr=^SonType;


  TextBufType=Array[0.._N + _F - 2] Of Byte;
  TBufPtr=^TextBufType;
  WordRay=Array[0.._N] Of SmallInt;
  WordRayPtr=^WordRay;
  BWordRay=Array[0.._N + 256] Of SmallInt;
  BWordRayPtr=^BWordRay;

  StreamCompressor=Class
     Private
      Code,Len:Word;
      GetBuf:Word;
      GetLen:Byte;
      PutLen:Byte;
      PutBuf:Word;
      TextSize:Longint;
      CodeSize:Longint;
      PrintCount:Longint;
      Match_Position:SmallInt;
      Match_Length:SmallInt;
      Text_Buf:TBufPtr;
      Lson,Dad:WordRayPtr;
      Rson:BWordRayPtr;
      Freq:FreqPtr; // cumulative freq table
      Prnt:PntrPtr;
      Son:SonPtr;  // pointing children nodes (son[], son[] + 1)
      StreamSize:Integer;
      SourceTarget:Stream;
      DestTarget:Stream;
      Procedure InitTree;
      Procedure InsertNode(R:SmallInt);
      Procedure DeleteNode(P:SmallInt);
      Procedure GetBytes(Var Data;Count:Word;Var ReadCount:Word);
      Procedure PutBytes(Var Data;Count:Word;Var WriteCount:Word);
      Procedure Update(C:SmallInt);
      Procedure StartHuff;
      Procedure Putcode(L:SmallInt;C:Word);
      Procedure Reconst;
      Procedure EncodeChar(C:Word);
      Procedure EncodePosition(C:Word);
      Procedure EncodeEnd;
      Function GetBit:SmallInt;
      Function GetByte:SmallInt;
      Function DecodeChar:SmallInt;
      Function DecodePosition:Word;
     Public
      Constructor Create;
      Destructor Destroy;Override;

      Function Compress(Source:Stream):Stream;
      Function Decompress(Source:Stream):Stream;
    End;

Implementation

{*******************
  TCompressedStream
 ********************}

Const
// Tables FOR encoding/decoding upper 6 bits of sliding dictionary pointer
// Encoder table
  P_Len:Array[0..63] Of Byte =
  ($03, $04, $04, $04, $05, $05, $05, $05,
    $05, $05, $05, $05, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $08, $08, $08, $08, $08, $08, $08, $08,
    $08, $08, $08, $08, $08, $08, $08, $08);

  P_Code:Array[0..63] Of Byte =
  ($00, $20, $30, $40, $50, $58, $60, $68,
    $70, $78, $80, $88, $90, $94, $98, $9C,
    $A0, $A4, $A8, $AC, $B0, $B4, $B8, $BC,
    $C0, $C2, $C4, $C6, $C8, $CA, $CC, $CE,
    $D0, $D2, $D4, $D6, $D8, $DA, $DC, $DE,
    $E0, $E2, $E4, $E6, $E8, $EA, $EC, $EE,
    $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7,
    $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF);

// decoder table
  D_Code:Array[0..255] Of Byte =
  ($00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $01, $01, $01, $01, $01, $01, $01, $01,
    $01, $01, $01, $01, $01, $01, $01, $01,
    $02, $02, $02, $02, $02, $02, $02, $02,
    $02, $02, $02, $02, $02, $02, $02, $02,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $08, $08, $08, $08, $08, $08, $08, $08,
    $09, $09, $09, $09, $09, $09, $09, $09,
    $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A,
    $0B, $0B, $0B, $0B, $0B, $0B, $0B, $0B,
    $0C, $0C, $0C, $0C, $0D, $0D, $0D, $0D,
    $0E, $0E, $0E, $0E, $0F, $0F, $0F, $0F,
    $10, $10, $10, $10, $11, $11, $11, $11,
    $12, $12, $12, $12, $13, $13, $13, $13,
    $14, $14, $14, $14, $15, $15, $15, $15,
    $16, $16, $16, $16, $17, $17, $17, $17,
    $18, $18, $19, $19, $1A, $1A, $1B, $1B,
    $1C, $1C, $1D, $1D, $1E, $1E, $1F, $1F,
    $20, $20, $21, $21, $22, $22, $23, $23,
    $24, $24, $25, $25, $26, $26, $27, $27,
    $28, $28, $29, $29, $2A, $2A, $2B, $2B,
    $2C, $2C, $2D, $2D, $2E, $2E, $2F, $2F,
    $30, $31, $32, $33, $34, $35, $36, $37,
    $38, $39, $3A, $3B, $3C, $3D, $3E, $3F);

  D_Len:Array[0..255] Of Byte =
  ($03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $03, $03, $03, $03, $03, $03, $03, $03,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $04, $04, $04, $04, $04, $04, $04, $04,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $05, $05, $05, $05, $05, $05, $05, $05,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $06, $06, $06, $06, $06, $06, $06, $06,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $07, $07, $07, $07, $07, $07, $07, $07,
    $08, $08, $08, $08, $08, $08, $08, $08,
    $08, $08, $08, $08, $08, $08, $08, $08);

Constructor StreamCompressor.Create;
Begin
  GetBuf:=0;
  GetLen:=0;
  PutLen:=0;
  PutBuf:=0;
  TextSize:=0;
  CodeSize:=0;
  PrintCount:=0;
  Match_Position:=0;
  Match_Length:=0;

  New(LSon);
  New(Dad);
  New(RSon);
  New(Text_Buf);
  New(Freq);
  New(Prnt);
  New(Son);
End;

Destructor StreamCompressor.Destroy;
Begin
  Dispose(Son);
  Dispose(Prnt);
  Dispose(Freq);
  Dispose(Text_buf);
  Dispose(RSon);
  Dispose(Dad);
  Dispose(LSon);
End;

Procedure StreamCompressor.GetBytes;
Begin
 If SourceTarget.Position+Count>SourceTarget.Size Then
  Count:=SourceTarget.Size-SourceTarget.Position;

 SourceTarget.Read(@Data,Count);
 ReadCount:=Count;
End;

Procedure StreamCompressor.PutBytes;
Begin
 DestTarget.Write(@Data,Count);
 WriteCount:=Count;
End;

Procedure StreamCompressor.InitTree;
Var
  I:SmallInt;
Begin
 For I:=_N + 1 To _N + 256 Do
  RSon^[i]:=_NULL; // root
 For I:=0 To _N Do
  Dad^[i]:=_NULL; // node
End;

Procedure StreamCompressor.InsertNode;
Var
 Tmp,i,p,Cmp:SmallInt;
 Key:TBufPtr;
 C:Word;
Begin
 Cmp:=1;
 Key:=@Text_Buf^[r];
 P:=Succ(_N)+Key^[0];
 RSon^[r]:=_NULL;
 LSon^[r]:=_NULL;
 Match_Length:=0;
 While Match_Length<_F Do
  Begin
    If (Cmp>=0) Then
     Begin
      If (RSon^[p]<>_NULL)Then
       Begin
        p:=rson^[p];
       End Else
       Begin
        RSon^[p] := r;
        Dad^[r] := p;
        Exit;
       End;
     End Else
     Begin
      If (lson^[p]<>_NULL)Then
       Begin
       p:=lson^[p];
       End Else
       Begin
        lson^[p] := r;
        dad^[r] := p;
        Exit;
       End;
     End;
    i:=0;
    Cmp:=0;
    While (i < _F)And (Cmp = 0)Do
     Begin
      Inc(i);
      Cmp:=Key^[i] - text_buf^[p + i];
     End;
     If (i > THRESHOLD) Then
      Begin
       tmp:=Pred((r - p) And Pred(_N));
       If (i>Match_length)Then
        Begin
         Match_Position:=tmp;
         Match_Length:=i;
        End;
       If (Match_Length<_F)And(i=Match_length)Then
        Begin
         c:=Tmp;
         If (c<Match_Position)Then Match_Position:=C;
        End;
    End; { if i > threshold }
  End; { WHILE match_length < F }
  Dad^[r]:=Dad^[p];
  LSon^[r]:=LSon^[p];
  RSon^[r]:=RSon^[p];
  Dad^[LSon^[p]]:=r;
  Dad^[RSon^[p]]:=r;
  If(RSon^[dad^[p]] = p)Then
   Begin
    RSon^[dad^[p]]:=r;
   End Else
   Begin
    LSon^[dad^[p]]:=r;
   End;
  Dad^[p]:=_NULL; // remove p
End;

Procedure StreamCompressor.DeleteNode;
Var
  Q:SmallInt;
Begin
 If (Dad^[p]=_NULL)Then Exit; // unregistered
 If (RSon^[p]=_NULL) Then
  Begin
   q:=LSon^[p];
  End Else
  Begin
   If (LSon^[p]=_NULL)Then
    Begin
     q:=RSon^[p];
    End Else
    Begin
     q:=LSon^[p];
     If (RSon^[q]<>_NULL)Then
      Begin
       Repeat
        q:=RSon^[q];
       Until (RSon^[q]=_NULL);
       RSon^[dad^[q]]:=LSon^[q];
       Dad^[lson^[q]] := dad^[q];
       LSon^[q] := lson^[p];
       Dad^[lson^[p]] := q;
      End;
     RSon^[q]:=RSon^[p];
     Dad^[rson^[p]]:=q;
   End;
 End;
 Dad^[q]:=Dad^[p];
 If (RSon^[Dad^[p]]=p)Then
  RSon^[dad^[p]] := q
 Else
  LSon^[dad^[p]] := q;
 Dad^[p]:=_NULL;
End;

Function StreamCompressor.GetBit;
Var
 i:Byte;
 i2:SmallInt;
 Wresult:Word;
Begin
 While (GetLen<=8) Do
 Begin
  GetBytes(i, 1, Wresult);
  If WResult=1 Then
   i2:=i
  Else
   i2:=0;
  GetBuf:=Getbuf Or (i2 Shl (8 - getlen));
  Inc(GetLen,8);
  End;
  i2:=GetBuf;
  GetBuf:=GetBuf shl 1;
  Dec(GetLen);
  GetBit:=SmallInt((i2 < 0));
End;

Function StreamCompressor.GetByte;
Var
  j:Byte;
  i,Wresult:Word;
Begin
 While (Getlen <= 8)Do
  Begin
   GetBytes(j, 1, Wresult);
   If Wresult = 1 then
    i:=j
   Else
    i:=0;
  Getbuf := getbuf or (i shl (8 - getlen));
  Inc(GetLen, 8);
  End;
  i:=GetBuf;
  Getbuf:=GetBuf Shl 8;
  Dec(getlen,8);
  GetByte:=SmallInt(i Shr 8);
end;

Procedure StreamCompressor.Update;
Var
  i,j,k,l:SmallInt;
Begin
 If (Freq^[_R]=MAX_FREQ) Then Reconst;
 c:=Prnt^[c + _T];
 Repeat
  Inc(freq^[c]);
  K:=Freq^[c];
 // swap nodes to keep the tree freq-ordered
  L:=Succ(C);
  If (k>Freq^[l])Then
  Begin
   While (k>Freq^[l])Do Inc(l);
   Dec(l);
   Freq^[c]:=Freq^[l];
   Freq^[l]:=k;
   i:=Son^[c];
   Prnt^[i]:=l;
   If (i < _T) Then Prnt^[Succ(i)] := l;
   J:=Son^[l];
   Son^[l]:=i;
   Prnt^[j]:=c;
   If (j < _T)Then Prnt^[Succ(j)] := c;
   Son^[c]:=j;
   C:=l;
  End;
  c:=Prnt^[c];
 Until (c=0); // REPEAT it until reaching the root
End;

Procedure StreamCompressor.StartHuff;
Var
 I,J:SmallInt;
Begin
 For I:=0 To Pred(N_CHAR)Do
 Begin
  Freq^[i]:=1;
  Son^[i]:=i + _T;
  Prnt^[i + _T]:=i;
 End;
 I:=0;
 J:=N_CHAR;
 While (j <= _R)Do
 Begin
  Freq^[j]:=Freq^[i] + Freq^[i + 1];
  Son^[j]:=i;
  Prnt^[i]:=j;
  Prnt^[i + 1]:=j;
  Inc(I,2);
  Inc(J);
 End;
 Freq^[_T]:=$FFFF;
 Prnt^[_R]:=0;
End;

Procedure StreamCompressor.PutCode;
Var
  Temp:Byte;
  Got:Word;
Begin
 PutBuf:=PutBuf Or(C Shr PutLen);
 Inc(PutLen,L);
 If (PutLen>=8)Then
  Begin
   Temp:=PutBuf Shr 8;
   PutBytes(Temp, 1, Got);
   Dec(putlen, 8);
   If (PutLen >= 8)Then
    Begin
     Temp := Lo(PutBuf);
     PutBytes(Temp, 1, Got);
     Inc(codesize, 2);
     Dec(putlen, 8);
     PutBuf:=C Shl (L-PutLen);
    End Else
    Begin
     PutBuf:=PutBuf Shl 8;
     Inc(CodeSize);
    End;
  End;
End;

Procedure StreamCompressor.Reconst;
Var
 I,J,K,Tmp:SmallInt;
 F,L:Word;
Begin
// halven cumulative freq FOR leaf nodes
 j:=0;
 For i:=0 To Pred(_T)Do
 Begin
  If (Son^[i]>=_T)Then
  Begin
   Freq^[j] := Succ(Freq^[i]) Div 2; {@@ Bug Fix MOD -> DIV @@}
   Son^[j] := Son^[i];
   Inc(j);
  End;
 End;
 // make a tree : first, connect children nodes
 i:=0;
 j:=N_CHAR;
 While (j < _T)Do
 Begin
  k:=Succ(i);
  f := freq^[i] + freq^[k];
  Freq^[j] := f;
  k:=Pred(j);
  While f<Freq^[k] Do Dec(K);
  Inc(k);
  l := (j - k) shl 1;
  tmp := SUCC(k);
  Move(freq^[k], freq^[tmp], l);
  Freq^[k] := f;
  Move(son^[k], son^[tmp], l);
  Son^[k] := i;
  Inc(i, 2);
  Inc(j);
 End;
// connect parent nodes
 For i:=0 To Pred(_T)Do
 Begin
  k:=Son^[i];
  If (k >= _T)Then
  Begin
   Prnt^[k] := i;
  End Else
  Begin
   Prnt^[k] := i;
   Prnt^[SUCC(k)] := i;
  End;
 End;
End;

Procedure StreamCompressor.EncodeChar;
Var
 i:Word;
 j,k:SmallInt;
Begin
 i:=0;
 j:=0;
 k:=Prnt^[c + _T];
 // search connections from leaf node to the root
 Repeat
  i:=i Shr 1;
 {
 IF node's address is odd, output 1
 ELSE output 0
 }
  If Boolean(k And 1)Then Inc(i,$8000);
  Inc(j);
  k:=Prnt^[k];
 Until (k=_R);
 Putcode(j, i);
 Code:=i;
 Len:=j;
 Update(c);
End;

Procedure StreamCompressor.EncodePosition;
Var
 i,j:Word;
Begin
 // output upper 6 bits with encoding
 i:=c Shr 6;
 j:=p_code[i];
 PutCode(p_len[i],j Shl 8);
 // output lower 6 bits directly
 PutCode(6, (c And $3F) Shl 10);
End;

Procedure StreamCompressor.EncodeEnd;
Var
 Temp:Byte;
 Got:Word;
Begin
 If Boolean(PutLen) Then
 Begin
  Temp:=Lo(PutBuf Shr 8);
  PutBytes(Temp,1,Got);
  Inc(CodeSize);
 End;
End;

Function StreamCompressor.DecodeChar;
Var
 C:Word;
Begin
 c:=Son^[_R];
    {
     * start searching tree from the root to leaves.
     * choose node #(son[]) IF input bit = 0
     * ELSE choose #(son[]+1) (input bit = 1)
    }
 While (c < _T) Do
 Begin
  c:=c + GetBit;
  c:=Son^[c];
 End;
 c:=c - _T;
 Update(c);
 DecodeChar:=SmallInt(c);
End;

Function StreamCompressor.DecodePosition;
Var
 I,J,C:Word;
Begin
// decode upper 6 bits from given table
 i:=GetByte;
 c:=Word(d_code[i] shl 6);
 j:=d_len[i];
// input lower 6 bits directly
 Dec(j, 2);
 While j <> 0 Do
 Begin
  i:=(i Shl 1) + GetBit;
  Dec(J);
 End;
 DecodePosition:=c Or i And $3F;
End;

Function StreamCompressor.Compress(Source:Stream):Stream;
Var
  Ct:Byte;
  i,L,R,S,Last_Match_Length:SmallInt;
  Got:Word;
  SourceSize:Longint;
Begin
  SourceTarget:=Source;
  Result := MemoryStream.Create(SourceTarget.Size);;
  DestTarget := Result;
  DestTarget.Skip(4);
  Source.Seek(0);

  TextSize:=0; // rewind and rescan
  StartHuff;
  InitTree;
  s:=0;
  r:=_N - _F;
  FillChar(Text_buf^[0], r, ' ');
  L:=0;
  Got:=1;
  While(L < _F)And(Got <> 0)Do
  Begin
    GetBytes(ct, 1, Got);
    If Got <> 0 Then
    Begin
      Text_buf^[r+L] := ct;
      Inc(l);
    End;
  End;

  Textsize := len;
  For i:=1 To _F Do
    InsertNode(r - i);

  InsertNode(r);
  Repeat
    If (Match_Length > L)Then
      Match_Length:=L;

    If (Match_Length<=THRESHOLD)Then
    Begin
      Match_Length:=1;
      EncodeChar(text_buf^[r]);
    End Else
    Begin
      EncodeChar(255 - THRESHOLD + match_length);
      EncodePosition(match_position);
    End;

    Last_Match_Length:=Match_length;
    i:=0;
    Got:=1;
    While (i < last_match_length)And (Got <> 0) Do
    Begin
      GetBytes(ct, 1, Got);
      If Got <> 0 Then
      Begin
        DeleteNode(s);
        text_buf^[s] := ct;

        If (s < Pred(_F))Then
          text_buf^[s + _N]:=ct;

      s:=Succ(s) And Pred(_N);
      r:=Succ(r) And Pred(_N);
      InsertNode(r);
      Inc(i);
    End;
  End;
  Inc(textsize, i);
  While (i<Last_Match_Length)Do
  Begin
   Inc(i);
   DeleteNode(s);
   s:=Succ(s) And Pred(_N);
   r:=Succ(r) And Pred(_N);
   Dec(l);
   If Boolean(Len)Then InsertNode(r);
  End;
  Until (L<= 0);
  EncodeEnd;

  StreamSize := TextSize;
  SourceSize := Source.Size;
  Result.Seek(0);
  Result.Write(@SourceSize, 4);
End;

Function StreamCompressor.Decompress(Source:Stream):Stream;
Var
 c,i,j,k,r:SmallInt;
 c2:Byte;
 count:Longint;
 Put:Word;
Begin
  Source.Read(@StreamSize, 4);
  Result := MemoryStream.Create(StreamSize);

  SourceTarget := Source;
  DestTarget := Result;

  StartHuff;
  r:=_N - _F;
  FillChar(text_buf^[0], r, ' ');
  Count := 0;
  While Count<StreamSize Do
  Begin
    c:=DecodeChar;
    If (c < 256)Then
    Begin
      c2:=Lo(c);
      PutBytes(c2, 1, Put);
      text_buf^[r] := c;
      Inc(r);
      r:=r And Pred(_N);
      Inc(count);
    End Else
    Begin {c >= 256 }
      i:=(r - Succ(DecodePosition)) And Pred(_N);
      j:=c - 255 + THRESHOLD;
      For K:=0 To Pred(j) Do
      Begin
        c:=text_buf^[(i + k) And Pred(_N)];
        c2:=Lo(c);
        PutBytes(c2, 1, Put);
        text_buf^[r] := c;
        Inc(r);
        r:=r And Pred(_N);
        Inc(count);
      End;
    End;
  End;
End;

End.
