Unit TERRA_INI;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Math, TERRA_IO;

Type
  TokenFormat=(tkInteger, tkCardinal, tkFloat,
                tkBoolean, tkByte, tkString,
                tkColor, tkVector, tkKey
                );

  PINIToken=^INIToken;
  INIToken=Record
    Name:AnsiString;
    Default:AnsiString;
    Format:TokenFormat;
    Data:Pointer;
    Found:Boolean;
  End;

  INIParser = Class(TERRAObject)
    Protected
      _TokenList:Array Of INIToken;
      _TokenCount:Integer;

    Public
      ParseCommas:Boolean;

      Constructor Create;
      Destructor Destroy; Override;

      Procedure AddToken(Token:AnsiString; Format:TokenFormat; Data:Pointer; Default:AnsiString='');
      Function GetToken(Token:AnsiString):PINIToken;
      Procedure DiscardTokens;

      Procedure Load(Source:Stream; IgnoreWarnings:Boolean=False);Overload;
      Procedure Load(Filename:AnsiString; IgnoreWarnings:Boolean=False);Overload;

      Procedure LoadFromString(S:AnsiString; IgnoreWarnings:Boolean=False);
      Function SaveToString(IgnoreDefaults:Boolean=True):AnsiString;

      Procedure Save(Dest:Stream; IgnoreDefaults:Boolean=True);Overload;
      Procedure Save(Filename:AnsiString; IgnoreDefaults:Boolean=True);Overload;
  End;

  Procedure ConvertFromToken(Source:AnsiString; Dest:Pointer; Format:TokenFormat);
  Function ConvertToToken(Source:Pointer; Format:TokenFormat):AnsiString;

Implementation
Uses TERRA_Error, TERRA_FileIO, TERRA_Log, TERRA_Application, TERRA_Color, TERRA_Vector3D;

// LINIParser

Constructor INIParser.Create;
Begin
  SetLength(_TokenList,0);
  _TokenCount:=0;
End;

Destructor INIParser.Destroy;
Begin
  SetLength(_TokenList,0);
End;

Procedure INIParser.AddToken(Token:AnsiString; Format:TokenFormat; Data:Pointer; Default:AnsiString='');
Begin
 If Assigned(GetToken(Token)) Then
  Exit;

  SetLength(_TokenList,Succ(_TokenCount));
  _TokenList[_TokenCount].Name:=Token;
  _TokenList[_TokenCount].Format:=Format;
  _TokenList[_TokenCount].Data:=Data;
  _TokenList[_TokenCount].Default:=Default;
  _TokenList[_TokenCount].Found:=False;
  Inc(_TokenCount);
End;

Function INIParser.GetToken(Token:AnsiString): PINIToken;
Var
 I:Integer;
Begin
  Result:=Nil;
  Token:=UpStr(Token);

  For I:=0 To Pred(_TokenCount) Do
  If UpStr(_TokenList[I].Name)=Token Then
  Begin
    Result:=@(_TokenList[I]);
    Break;
  End;
End;

Procedure ConvertFromToken(Source:AnsiString; Dest:Pointer; Format:TokenFormat);
Begin
  Case Format Of
    tkInteger:  PInteger(Dest)^ := StringToInt(Source);
    tkCardinal: PCardinal(Dest)^ := StringToCardinal(Source);
    tkFloat:   PSingle(Dest)^ := StringToFloat(Source);
    tkBoolean:  PBoolean(Dest)^ := StringToBool(UpStr(Source));
    tkByte:     PByte(Dest)^ := StringToInt(Source);
    tkString:   PString(Dest)^ := Source;
    tkColor:    If (Source<>'') And (Source[1]='#') Then
                Begin
                  PColor(Dest)^ := ColorCreate(Source);
                End Else
                Begin
                  PColor(Dest).R := StringToInt(GetNextWord(Source,'\'));
                  PColor(Dest).G := StringToInt(GetNextWord(Source,'\'));
                  PColor(Dest).B := StringToInt(GetNextWord(Source,'\'));
                  If Source<>'' Then
                    PColor(Dest).A := StringToInt(Source)
                  Else
                    PColor(Dest).A := 255;
                End;
    tkVector:   Begin
                  PVector3D(Dest).X := StringToFloat(GetNextWord(Source,'\'));
                  PVector3D(Dest).Y := StringToFloat(GetNextWord(Source,'\'));
                  PVector3D(Dest).Z := StringToFloat(Source);
                End;
    tkKey:      PInteger(Dest)^ := GetKeyByName(Source);
  Else
      RaiseError('Invalid token format.['+CardinalToString(Cardinal(Format))+']');
  End;
End;

Function ConvertToToken(Source:Pointer; Format:TokenFormat):AnsiString;
Begin
    Case Format Of
     tkInteger:   Result:=IntToString(PInteger(Source)^);
     tkCardinal:  Result:=CardinalToString(PCardinal(Source)^);
     tkFloat:     Result:=FloatToString(PSingle(Source)^);
     tkBoolean:   Result:=LowStr(BoolToString(PBoolean(Source)^));
     tkByte:      Result:=IntToString(PByte(Source)^);
     tkString:    Result:=PString(Source)^;
     tkColor:     Result:=ColorToString(PColor(Source)^);
     tkVector:    Begin
                    Result:= FloatToString(PVector3D(Source).X)+'\'+
                             FloatToString(PVector3D(Source).Y)+'\'+
                             FloatToString(PVector3D(Source).Z);
                  End;
     tkKey:      Result := GetKeyName(PInteger(Source)^);
     Else
     	Begin
     		Result := '';
			RaiseError('Invalid token format.['+CardinalToString(Cardinal(Format))+']');
		End;
    End;
End;

Procedure INIParser.DiscardTokens;
Begin
  _TokenCount:=0;
  SetLength(_TokenList, _TokenCount);
End;

Procedure INIParser.Load(Source:Stream; IgnoreWarnings:Boolean=False);
Var
 Token,S,SK:AnsiString;
 Info:PINIToken;
 I:Integer;
Begin
  While Source.Position<Source.Size Do
  Begin
    Source.ReadLine(SK);
    I:=Pos('//',SK);
    If I>0 Then
      SK:=Copy(SK,1,Pred(I)); // Strip comments from line

    While SK<>'' Do
    Begin
      If ParseCommas Then
        I:=Pos(',',SK)
      Else
        I:=0;

      If I>0 Then
      Begin
        S:=Copy(SK,1,Pred(I));
        SK:=TrimRight(TrimLeft(Copy(SK,Succ(I),Length(SK))));
      End Else
      Begin
        S:=SK;
        SK:='';
      End;

      I:=Pos('=',S);
      If I<=0 Then Break;

      Token:=UpStr(Copy(S,1,Pred(I)));
      Token:=TrimRight(TrimLeft(Token));
      S:=Copy(S,Succ(I),Length(S)); // Get Token and Value
      S:=TrimRight(TrimLeft(S));

      Info:=GetToken(Token);
      If Not Assigned(Info) Then
      Begin
        If Not IgnoreWarnings Then
          Log(logWarning,'INI','Invalid Token.['+Token+']');
        Continue;
      End;

      Info.Found:=True;
      ConvertFromToken(S, Info.Data, Info.Format);
    End;
  End;

  For I:=0 To Pred(_TokenCount) Do
    With _TokenList[I] Do
    If (Not Found) And (Default<>'') Then
      ConvertFromToken(Default, Data, Format);
End;

Function INIParser.SaveToString(IgnoreDefaults:Boolean=True):AnsiString;
Var
  Dest:MemoryStream;
Begin
  Dest := MemoryStream.Create(1024);
  Save(Dest, IgnoreDefaults);
  SetLength(Result, Pred(Dest.Position));
  Move(Dest.Buffer^, Result[1], Pred(Dest.Position));
  Dest.Destroy;
End;

Procedure INIParser.Save(Dest:Stream; IgnoreDefaults:Boolean=True);
Var
 S:AnsiString;
 I:Integer;
 Info:PINIToken;
Begin
  For I:=0 To Pred(_TokenCount) Do
  Begin
    Info:=@(_TokenList[I]);
    S:=ConvertToToken(Info.Data, Info.Format);

    If (Not IgnoreDefaults) Or (UpStr(S)<>UpStr(Info.Default)) Then
      Dest.WriteLine(Info.Name+'='+S);
  End;
End;

Procedure INIParser.Load(Filename:AnsiString; IgnoreWarnings:Boolean=False);
Var
  Source:Stream;
Begin
  If Not FileStream.Exists(FileName) Then
  Begin
    RaiseError('File not found ['+FileName+']');
    Exit;
  End;

  Source := FileStream.Open(FileName);
  Load(Source, IgnoreWarnings);
  Source.Destroy;
End;

Procedure INIParser.LoadFromString(S:AnsiString; IgnoreWarnings:Boolean=False);
Var
  Source:MemoryStream;
Begin
  If S='' Then
    Exit;

  Source := MemoryStream.Create(Length(S), @S[1]);
  Load(Source, IgnoreWarnings);
  Source.Destroy;
End;

Procedure INIParser.Save(Filename:AnsiString; IgnoreDefaults:Boolean=True);
Var
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  Save(Dest, IgnoreDefaults);
  Dest.Destroy;
End;

End.
