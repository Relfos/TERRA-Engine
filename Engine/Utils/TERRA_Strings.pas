{
Complex string functions:
- pattern matching
- string distance
}

Unit TERRA_Strings;
{$I terra.inc}

Interface
Uses TERRA_Utils;

// test if pattern Expression is found in string S
// example pattern: *.png 
Function MatchRegEx(S, Expression:AnsiString):Boolean;

//result is number between 0 (zero) and 1 (one), where 0 means not similar at all and 1 means equal strings.
Function StringSimilarityRatio(const Str1, Str2:AnsiString; IgnoreCase: Boolean):Single;

Implementation

Function MatchRegEx(S, Expression:AnsiString):Boolean;
Var
  I, J:Integer;
Begin
  I := 1;
  J := 1;
  Result := False;
  While (I<=Length(S)) And (J<=Length(Expression)) Do
  Begin
    If (Expression[J] = '*') Then
    Begin
      While (Expression[J] = '*') Do
      Begin
        If (J>=Length(Expression)) Then
        Begin
          Result := True;
          Exit;
        End;
        Inc(J);
      End;

      While (S[I]<>Expression[J]) Do
      Begin
        Inc(I);
        If (I>Length(S)) Then
          Exit;
      End;

      Inc(I);
      Inc(J);
    End Else
    If (S[I]=Expression[J]) Then
    Begin
      Inc(I);
      Inc(J);
    End Else
      Exit;
  End;

  Result := True;
End;

Function Min(const A, B, C: Integer): Integer;
Begin
  Result := A;
  If B < Result then
    Result := B;
  If C < Result then
    Result := C;
End;

Function DamerauLevenshteinDistance(const Str1, Str2:AnsiString): Integer;
Var
  LenStr1, LenStr2: Integer;
  I, J, T, Cost, PrevCost: Integer;
  pStr1, pStr2, S1, S2: PAnsiChar;
  D: PIntegerArray;
Begin
  LenStr1 := Length(Str1);
  LenStr2 := Length(Str2);

  // save a bit memory by making the second index points to the shorter string
  if LenStr1 < LenStr2 then
  begin
    T := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := T;
    pStr1 := PAnsiChar(Str2);
    pStr2 := PAnsiChar(Str1);
  end
  else
  begin
    pStr1 := PAnsiChar(Str1);
    pStr2 := PAnsiChar(Str2);
  end;

  // bypass leading identical characters
  while (LenStr2 <> 0) and (pStr1^ = pStr2^) do
  begin
    Inc(pStr1);
    Inc(pStr2);
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  // bypass trailing identical characters
  while (LenStr2 <> 0) and ((pStr1 + LenStr1 - 1)^ = (pStr2 + LenStr2 - 1)^) do
  begin
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  // is the shorter string empty? so, the edit distance is length of the longer one
  if LenStr2 = 0 then
  begin
    Result := LenStr1;
    Exit;
  end;

  // calculate the edit distance
  GetMem(D, (LenStr2 + 1) * SizeOf(Integer));

  for I := 0 to LenStr2 do
    D[I] := I;

  S1 := pStr1;
  for I := 1 to LenStr1 do
  begin
    PrevCost := I - 1;
    Cost := I;
    S2 := pStr2;
    for J := 1 to LenStr2 do
    begin
      if (S1^ = S2^) or ((I > 1) and (J > 1) and (S1^ = (S2 - 1)^) and (S2^ = (S1 - 1)^)) then
        Cost := PrevCost
      else
        Cost := 1 + Min(Cost, PrevCost, D[J]);
      PrevCost := D[J];
      D[J] := Cost;
      Inc(S2);
    end;
    Inc(S1);
  end;
  Result := D[LenStr2];
  FreeMem(D);
End;

Function StringSimilarityRatio(const Str1, Str2:AnsiString; IgnoreCase: Boolean):Single;
Var
  MaxLen: Integer;
  Distance: Integer;
Begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);
  if MaxLen <> 0 then
  begin
    if IgnoreCase then
      Distance := DamerauLevenshteinDistance(LowStr(Str1), LowStr(Str2))
    else
      Distance := DamerauLevenshteinDistance(Str1, Str2);
    Result := Result - (Distance / MaxLen);
  end;
End;

End.
