//BASED ON: http://www.gamedev.net/community/forums/mod/journal/journal.asp?jn=263350&reply_id=3198944
Unit TERRA_TextureAtlas;

{$I terra.inc}
Interface
Uses TERRA_Utils, TERRA_Image, TERRA_Texture;

Type
  TextureAtlas = Class(Texture)
    Protected
      _MipMaps:Array Of Image;
      _MipMapCount:Integer;
      _TilesPerRow:Integer;

      _SourceList:Array Of Image;
      _SourceIDs:Array Of Integer;
      _SourceCount:Integer;
      _SourceSize:Integer;

      Procedure Build(); Override;

    Public
      Destructor Destroy;

      Procedure Add(Source:Image; ID:Integer=-1); Overload;
      Procedure Add(Source:AnsiString; ID:Integer=-1); Overload;

      Procedure GenerateAtlas;

      Function GetAtlasImage(Level:Integer):Image;

      Procedure ExportAtlas(ExportName:AnsiString);

      Property LevelCount:Integer Read _MipMapCount;
      Property TilesPerRow:Integer Read _TilesPerRow Write _TilesPerRow;
  End;

Implementation
Uses {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_IO, TERRA_GraphicsManager, TERRA_FileManager;

Destructor TextureAtlas.Destroy;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_SourceCount) Do
    _SourceList[I].Destroy;

  For I:=0 To Pred(_MipMapCount) Do
  If Assigned(_MipMaps[I]) Then
    _MipMaps[I].Destroy;

  Inherited;
End;

Procedure TextureAtlas.Add(Source:Image; ID:Integer=-1);
Begin
  Inc(_SourceCount);
  SetLength(_SourceList, _SourceCount);
  SetLength(_SourceIDS, _SourceCount);

  If (_SourceSize<=0) Then
    _SourceSize := Source.Width;

  If (ID<0) Then
    ID := Pred(_SourceCount);

  _SourceList[Pred(_SourceCount)] := Image.Create(Source);
  _SourceIDs[Pred(_SourceCount)] := ID;
  Source := _SourceList[Pred(_SourceCount)];
  If (Source.Width <> _SourceSize) Or (Source.Height <> _SourceSize) Then
    Source.Resize(_SourceSize, _SourceSize);
End;

Procedure TextureAtlas.GenerateAtlas;
Var
  I, J, X, Y:Integer;
  N, K, Size, S:Integer;
  Temp:Image;
Begin
  _MipMapCount := 1;

  K := _TilesPerRow;
  If (K<=0) Then
    K := Trunc(Sqrt(_SourceCount));

  If (Odd(K)) Then
    Inc(K);
  S := _SourceSize * K;
  While (S>1) Do
  Begin
    Inc(_MipMapCount);
    S := S Shr 1;
  End;
  SetLength(_MipMaps, _MipMapCount);

  S := _SourceSize;
  Size := S * K;
  For J:=0 To Pred(LevelCount) Do
  Begin
    _MipMaps[J] := Image.Create(Size, Size);
    If (J = 0) Then
    Begin
      _Width := _MipMaps[J].Width;
      _Height := _MipMaps[J].Height;
    End;

    For I:=0 To Pred(_SourceCount) Do
    Begin
      N := _SourceIDs[I];
      X := N Mod K;
      Y := N Div K;
      Temp := _SourceList[I];
      If (S>1) Then
        Temp.Resize(S, S);
      If (_MipMaps[J].Width>1) Then
        _MipMaps[J].Blit(X * S, Y * S, 0,0, Temp.Width, Temp.Height, Temp);
    End;

    {If (_MipMaps[J]<>Nil) And (_MipMaps[J].Width>2) Then
      _MipMaps[J].Save('atlas_'+IntToString(J)+'.png');}

    S := S Shr 1;
    Size := Size Shr 1;
  End;
End;

Function TextureAtlas.GetAtlasImage(Level:Integer):Image;
Begin
  If (Level<0) Or (Level>=_MipMapCount) Then
    Result := Nil
  Else
    Result := _MipMaps[Level];
End;

Procedure TextureAtlas.Build();
Var
  I:Integer;
  Temp:Image;
Begin
  If (_FrameCount<=0) Then
  Begin
    _FrameCount := 1;
    SetLength(_Handles, _FrameCount);
    _Handles[0] := GraphicsManager.Instance.GenerateTexture();
  End;

  glActiveTexture(GL_TEXTURE0);
  {$IFDEF PC}
  glEnable(GL_TEXTURE_2D);
  {$ENDIF}

  glBindTexture(GL_TEXTURE_2D, _Handles[0]);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

  For I:=0 To Pred(LevelCount) Do
  Begin
    Temp := _MipMaps[I];
    glTexImage2D(GL_TEXTURE_2D, I, GL_RGBA8, Temp.Width, Temp.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Temp.Pixels);
  End;

  glBindTexture(GL_TEXTURE_2D, 0);
End;

Procedure TextureAtlas.ExportAtlas(ExportName:AnsiString);
Var
  I:Integer;
Begin
  For I:=0 To Pred(LevelCount) Do
    _MipMaps[I].Save(ExportName+IntToString(I)+'.png');
End;

Procedure TextureAtlas.Add(Source:AnsiString; ID:Integer=-1);
Var
  S:AnsiString;
  F:Stream;
  Src:Image;
Begin
  S := FileManager.Instance.SearchResourceFile(Source);
  If S='' Then
    Exit;

  F := FileManager.Instance.OpenFileStream(S);
  Src := Image.Create(F);
  Add(Src, ID);
  Src.Destroy;
  F.Destroy;
End;

End.