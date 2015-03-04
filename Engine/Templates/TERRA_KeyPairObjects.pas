Unit TERRA_KeyPairObjects;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Collections;

Type
  KeyPairObject=Class(ListObject)
    Public
      Key:TERRAString;
      Value:TERRAString;

      Constructor Create(Key, Value:TERRAString);
      Function ToString():TERRAString; Override;

    Protected
      Procedure CopyValue(Other:ListObject); Override;
      Function Sort(Other:ListObject):Integer; Override;
      Function GetHashKey():HashKey; Override;
  End;

Function LoadKeypairList(SourceFile:TERRAString):List;

Implementation
Uses TERRA_Stream, TERRA_FileStream;

Function LoadKeypairList(SourceFile:TERRAString):List;
Var
  Source:Stream;
  S,S2:TERRAString;
Begin
  S := '';
  Result := List.Create;
  If  (SourceFile<>'') And (FileStream.Exists(SourceFile)) Then
  Begin
    Source :=  FileStream.Open(SourceFile);
    While Not Source.EOF Do
    Begin
      Source.ReadLine(S);
      S2 := StringGetNextSplit(S, Ord(','));
      Result.Add(KeyPairObject.Create(S2,S));
    End;
    Source.Destroy;
  End;
End;

{ KeyPairObject }
Constructor KeyPairObject.Create(Key, Value:TERRAString);
Begin
  Self.Key := Key;
  Self.Value := Value;
End;

Procedure KeyPairObject.CopyValue(Other: ListObject);
Begin
  Self.Key := KeyPairObject(Other).Key;
  Self.Value := KeyPairObject(Other).Value;
End;

Function KeyPairObject.GetHashKey: HashKey;
Begin
  Result := GetStringHashKey(Key);
End;

Function KeyPairObject.Sort(Other: ListObject): Integer;
Var
  S:TERRAString;
Begin
  S := KeyPairObject(Other).Key;
  Result := GetStringSort(Self.Key, S);
End;

Function KeyPairObject.ToString:TERRAString;
Begin
  Result := Key;
End;

End.