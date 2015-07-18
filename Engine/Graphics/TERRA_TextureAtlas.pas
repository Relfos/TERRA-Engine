{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_
 * Implements a texture atlas, which can be used to pack multiple textures
 ***********************************************************************************************************************
}
Unit TERRA_TextureAtlas;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Image, TERRA_Texture, TERRA_Packer, TERRA_Resource, TERRA_Color;

Type
  TextureAtlas = Class;

  TextureAtlasItem = Class(TERRAObject)
    Protected
      _Packed:Boolean;
      _Name:TERRAString;

      _U1,_V1:Single;
      _U2,_V2:Single;

      _X1,_Y1:Integer;
      _X2,_Y2:Integer;

    Public
      ID:Integer;
      PageID:Integer;
      Buffer:Image;

      Procedure Release; Override;

      Property Name:TERRAString Read _Name;

      Property U1:Single Read _U1;
      Property V1:Single Read _V1;
      Property U2:Single Read _U2;
      Property V2:Single Read _V2;

      Property X1:Integer Read _X1;
      Property Y1:Integer Read _Y1;
      Property X2:Integer Read _X2;
      Property Y2:Integer Read _Y2;
  End;

  TextureAtlasPage = Class(TERRATexture)
    Protected
      _PageID:Integer;
      _Atlas:TextureAtlas;

    Public
      Function Build():Boolean; Override;
  End;

  TextureAtlas = Class(TERRAObject)
    Protected
      _Name:TERRAString;
      _Width:Integer;
      _Height:Integer;
      _ItemList:Array Of TextureAtlasItem;
      _ItemCount:Integer;
      _Textures:Array Of TextureAtlasPage;
      _RefCount:Integer;
      _PageCount:Integer;
      _SaveCount:Integer;

      Function AllocTexture(Const Name:TERRAString):TextureAtlasPage; Virtual;

      Function CreateTexture(PageID:Integer):TextureAtlasPage;

      Function RedoPage(PageID:Integer):TERRATexture;
      Procedure RebuildPage(PageID:Integer; Target:TERRATexture);

    Public
      Constructor Create(Name:TERRAString; Width, Height:Integer);
      Procedure Release; Override;

      Function Add(Source:Image; Name:TERRAString; ShareSource:Boolean = False):TextureAtlasItem;
      Procedure Delete(ID:Integer);
      Function Get(ID:Integer):TextureAtlasItem; Overload;
      Function Get(Const Name:TERRAString):TextureAtlasItem; Overload;

      Procedure Clear;

      Function GetTexture(PageID:Integer):TERRATexture;

      Function Update:Boolean;

      Property Width:Integer Read _Width;
      Property Height:Integer Read _Height;
      Property PageCount:Integer Read _PageCount;

      Property ItemCount:Integer Read _ItemCount;
  End;

Implementation
Uses TERRA_Error, TERRA_FileUtils, TERRA_Log;

// LTextureAtlas
Constructor TextureAtlas.Create(Name:TERRAString; Width, Height:Integer);
Begin
  _Name := Name;
  _Width := Width;
  _Height := Height;
  _ItemCount := 0;
  _RefCount := 0;
  _PageCount := 1;
  SetLength(_Textures, 1);
End;

Procedure TextureAtlas.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(Length(_Textures)) Do
    ReleaseObject(_Textures[I]);

  Clear;
End;

Procedure TextureAtlas.Clear;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ItemCount) Do
  Begin
    ReleaseObject(_ItemList[I].Buffer);
    ReleaseObject(_ItemList[I]);
  End;
  _ItemCount := 0;
End;

Function TextureAtlas.Add(Source:Image; Name:TERRAString; ShareSource:Boolean): TextureAtlasItem;
Begin
  If (Source = Nil) Or (Source.Width<=0) Or (Source.Height<=0) Then
  Begin
    Result := Nil;
    Exit;
  End;

  Name := GetFileName(Name, True);
  Inc(_ItemCount);
  SetLength(_ItemList, _ItemCount);
  Log(logDebug, 'Game', 'Creating TextureAtlas item');
  Result := TextureAtlasItem.Create;
  _ItemList[Pred(_ItemCount)] := Result;
  Result._Name := Name;

  If ShareSource Then
    Result.Buffer := Source
  Else
  Begin
    Log(logDebug, 'Game', 'Creating image from source');
    Result.Buffer := Image.Create(Source);
  End;

  Result.ID := _RefCount;
  Inc(_RefCount);
  Log(logDebug, 'Game', 'TextureAtlas element added');
End;

Procedure TextureAtlas.Delete(ID:Integer);
Var
  I, N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_ItemCount) Do
  If (_ItemList[I].ID = ID) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  ReleaseObject(_ItemList[N].Buffer);
  _ItemList[N] := _ItemList[Pred(_ItemCount)];
  Dec(_ItemCount);
End;

Function TextureAtlas.Get(ID:Integer):TextureAtlasItem;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ItemCount) Do
  If (_ItemList[I].ID = ID) Then
  Begin
    Result := (_ItemList[I]);
    Exit;
  End;
  Result := Nil;
End;

Function TextureAtlas.Get(Const Name:TERRAString):TextureAtlasItem;
Var
  S:TERRAString;
  I:Integer;
Begin
  S := GetFileName(Name, True);

  For I:=0 To Pred(_ItemCount) Do
  If StringEquals(_ItemList[I]._Name, S) Then
  Begin
    Result := (_ItemList[I]);
    Exit;
  End;
  Result := Nil;
End;

Function TextureAtlas.Update:Boolean;
Var
  I, X, Y, W, H:Integer;
  Packer:RectanglePacker;
  LastCount, Count:Integer;
Begin
  Log(logDebug, 'TextureAtlas', 'Updating');

  _PageCount := 0;
  For I:=0 To Pred(_ItemCount) Do
  Begin
    _ItemList[I]._Packed := False;
    _ItemList[I].PageID := -1;
  End;

  LastCount := Succ(_ItemCount);
  Repeat
    Packer := RectanglePacker.Create;
    For I:=0 To Pred(_ItemCount) Do
    If Not _ItemList[I]._Packed Then
    Begin
      W := _ItemList[I].Buffer.Width;
      H := _ItemList[I].Buffer.Height;
      If (H<Self._Height) Then
        Inc(H);
      Packer.AddRect(W, H, _ItemList[I].ID);
    End;

    Count := Packer.Pack(Self._Width, Self._Height);
    If (Count=LastCount) Then
    Begin
      RaiseError('Not enough space to pack TextureAtlas.');
      Result := False;
      ReleaseObject(Packer);
      Exit;
    End;

    LastCount := Count;
    Inc(_PageCount);
    If Length(_Textures)<_PageCount Then
      SetLength(_Textures, _PageCount);

    For I:=0 To Pred(_ItemCount) Do
    Begin
      If (_ItemList[I]._Packed) Then
        Continue;

      X := 0;
      Y := 0;
      If (Packer.GetRect(_ItemList[I].ID, X, Y)) Then
      Begin
        _ItemList[I]._X1 := X;
        _ItemList[I]._Y1 := Y;

        _ItemList[I]._X2 := X + _ItemList[I].Buffer.Width;
        _ItemList[I]._Y2 := Y + _ItemList[I].Buffer.Height;

        _ItemList[I]._U1 := _ItemList[I]._X1 / Self._Width;
        _ItemList[I]._V1 := _ItemList[I]._Y1 / Self._Height;

        _ItemList[I]._U2 := _ItemList[I]._X2 / Self._Width;
        _ItemList[I]._V2 := _ItemList[I]._Y2 / Self._Height;

        _ItemList[I].PageID := Pred(_PageCount);
        _ItemList[I]._Packed := True;
      End Else
        Log(logError, 'TextureAtlas', 'Could not pack '+_ItemList[I]._Name);
    End;

    ReleaseObject(Packer);
  Until (Count = 0);

  For I:=0 To Pred(_PageCount) Do
    Self.RedoPage(I);

  Result := True;
  Log(logDebug, 'TextureAtlas', 'TextureAtlas is now updated');
End;

Function TextureAtlas.GetTexture(PageID:Integer):TERRATexture;
Begin
  If (PageID<Length(_Textures)) Then
  Begin
    If Not Assigned(_Textures[PageID]) Then
    Begin
      Result := CreateTexture(PageID);
    End  Else
    Begin
      Result := _Textures[PageID];

      If (Assigned(Result)) And (Result.IsReady()) And (Not Result.IsValid()) Then
      Begin
        Result := Self.RedoPage(PageID);
      End;
    End;
  End Else
    Result := Nil;
End;

Function TextureAtlas.RedoPage(PageID:Integer):TERRATexture;
Begin
  Result := Nil;

  If (PageID<0) Or (PageID>=Length(_Textures)) Then
    Exit;

  Result := _Textures[PageID];
  If Result = Nil Then
    Exit;

  Result.Rebuild();
End;


Function TextureAtlas.AllocTexture(Const Name:TERRAString): TextureAtlasPage;
Begin
  Result := TextureAtlasPage.Create(rtDynamic, Name);
End;

Function TextureAtlas.CreateTexture(PageID: Integer):TextureAtlasPage;
Var
  S:TERRAString;
Begin
  S := _Name+'_page'+IntToString(PageID);
  Log(logDebug, 'TextureAtlas', 'Creating TextureAtlas texture: '+S);

  ReleaseObject(_Textures[PageID]);
  Result := Self.AllocTexture(S);
  _Textures[PageID] := Result;
  Result.MipMapped := False;
  Result._PageID := PageID;
  Result._Atlas := Self;

  Result.InitFromSize(Width, Height, ColorWhite);

  Result.Rebuild();
End;

Procedure TextureAtlasItem.Release;
Begin
End;

{$DEFINE CPUBUFFER}
Procedure TextureAtlas.RebuildPage(PageID: Integer; Target: TERRATexture);
Var
  I:Integer;
  {$IFDEF CPUBUFFER}
  Buffer:Image;
  {$ENDIF}
Begin
  {$IFDEF CPUBUFFER}
  Buffer := Image.Create(_Width, _Height);
  {$ENDIF}

  For I:=0 To Pred(_ItemCount) Do
  Begin
    If (_ItemList[I].PageID <> PageID) Then
      Continue;

    If (Assigned(_ItemList[I].Buffer)) Then
    {$IFNDEF CPUBUFFER}
      Target.UpdateRect(_ItemList[I].Buffer, _ItemList[I].X1, _ItemList[I].Y1);
    {$ELSE}
      Buffer.Blit(_ItemList[I].X1, _ItemList[I].Y1, 0, 0, _ItemList[I].Buffer.Width, _ItemList[I].Buffer.Height, _ItemList[I].Buffer);
    {$ENDIF}
  End;

  {$IFDEF CPUBUFFER}
  Target.UpdateRect(Buffer);

  {$IFDEF PC}
  //Buffer.Save(_Name+'_TextureAtlas'+IntToString(_SaveCount)+'.png', 'png','depth=32');
  {$ENDIF}

  ReleaseObject(Buffer);
  {$ENDIF}
End;

{ TextureAtlasPage }
Function TextureAtlasPage.Build():Boolean;
Begin
  _Atlas.RebuildPage(Self._PageID, Self);
  Result := True;
End;

End.
