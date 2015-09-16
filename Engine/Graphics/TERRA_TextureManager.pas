Unit TERRA_TextureManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_ResourceManager, TERRA_Texture, TERRA_String, TERRA_Color, TERRA_Resource,
  TERRA_Renderer, TERRA_Image;

Type
  TextureManager = Class(ResourceManager)
    Protected
      _DefaultColorTable:TERRATexture;
      _DefaultNormalMap:TERRATexture;

      _WhiteTexture:TERRATexture;
      _BlackTexture:TERRATexture;
      _NullTexture:TERRATexture;

      _CellNoise:TERRATexture;

      Function GetDefaultNormalMap:TERRATexture;
      Function GetDefaultColorTable:TERRATexture;

      Function GetCellNoise:TERRATexture;

      Function CreateTextureWithColor(Name:TERRAString; TexColor:ColorRGBA):TERRATexture;

      Function GetWhiteTexture:TERRATexture;
      Function GetNullTexture:TERRATexture;
      Function GetBlackTexture:TERRATexture;
      Procedure FillTextureWithColor(Tex: TERRATexture; Const TexColor:ColorRGBA);


    Public
      Constructor Create();
//      Procedure OnContextLost; Override;

      Function GetItem(Name:TERRAString):TERRATexture;

      Procedure Release; Override;

      Property NullTexture:TERRATexture Read GetNullTexture;
      Property WhiteTexture:TERRATexture Read GetWhiteTexture;
      Property BlackTexture:TERRATexture Read GetBlackTexture;

      Property CellNoise:TERRATexture Read GetCellNoise;

      Property DefaultColorTable:TERRATexture Read GetDefaultColorTable;
      Property DefaultNormalMap:TERRATexture Read GetDefaultNormalMap;

      Property Textures[Name:TERRAString]:TERRATexture Read GetItem; Default;
  End;

Implementation
Uses TERRA_FileUtils, TERRA_FileFormat, TERRA_Engine, TERRA_FileManager, TERRA_Noise;

{ TextureManager }
Constructor TextureManager.Create();
Begin
  Inherited;

  Self.AutoUnload := True;
  //Self.UseThreads := True;
End;

Function TextureManager.GetItem(Name:TERRAString):TERRATexture;
Var
  Format:TERRAFileFormat;
  Location:TERRALocation;
Begin
  Result := Nil;

  Name := StringTrim(Name);
  Name := GetFileName(Name, True);
  If (Name='') Then
    Exit;

  Result := TERRATexture(GetResource(Name));
  If Assigned(Result) Then
    Exit;

  Format := Engine.Formats.FindLocationFromName(Name, TERRATexture, Location);
  If Format = Nil Then
    Format := Engine.Formats.FindLocationFromName(Name, TERRAImage, Location);

  If Assigned(Format) Then
  Begin
    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Found '+S+'...');{$ENDIF}

    Result := TERRATexture.Create(rtLoaded, Location);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture class instantiated sucessfully!');{$ENDIF}

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture loading priority set!');{$ENDIF}

    Self.AddResource(Result);

    {$IFDEF DEBUG_GRAPHICS}Log(logDebug, 'Texture', 'Texture added to manager!');{$ENDIF}
    Exit;
  End;

  //RaiseError('Could not find texture. ['+Name+']');
End;

Function TextureManager.CreateTextureWithColor(Name:TERRAString; TexColor:ColorRGBA):TERRATexture;
Begin
  Result := TERRATexture.Create(rtDynamic, Nil);
  //Result.Name := Name;
  Result.InitFromSize(64, 64, TexColor);
  Result.Uncompressed := True;
  Result.MipMapped := False;
  Result.Filter := filterLinear;
End;

Procedure TextureManager.FillTextureWithColor(Tex: TERRATexture; Const TexColor:ColorRGBA);
Var
  Buffer:TERRAImage;
Begin
  If (Tex = Nil) Then
    Exit;

  Buffer := TERRAImage.Create(Tex.Width, Tex.Height);
  Buffer.ClearWithColor(TexColor, maskRGBA);
  Tex.UpdateRect(Buffer, 0, 0);
  ReleaseObject(Buffer);
End;

Function TextureManager.GetBlackTexture: TERRATexture;
Begin
  If (Not Assigned(_BlackTexture)) Then
    _BlackTexture := Self.CreateTextureWithColor('default_black', ColorBlack)
  Else
  If (_BlackTexture.Status <> rsReady) Then
  Begin
    _BlackTexture.Update();
    FillTextureWithColor(_BlackTexture, ColorBlack);
  End Else
  If (Not _BlackTexture.IsValid()) Then
  Begin
    _BlackTexture.Unload();
  End;

  Result := _BlackTexture;
End;

Function TextureManager.GetWhiteTexture: TERRATexture;
Begin
  If (Not Assigned(_WhiteTexture)) Then
    _WhiteTexture := Self.CreateTextureWithColor('default_white', ColorWhite)
  Else
  If (_WhiteTexture.Status <> rsReady) Then
  Begin
    _WhiteTexture.Update();
    FillTextureWithColor(_WhiteTexture, ColorWhite);
  End Else
  If (Not _WhiteTexture.IsValid()) Then
  Begin
    _WhiteTexture.Unload();
  End;

  Result := _WhiteTexture;
End;

Function TextureManager.GetNullTexture: TERRATexture;
Begin
  If (Not Assigned(_NullTexture)) Then
    _NullTexture := Self.CreateTextureWithColor('default_null', ColorNull)
  Else
  If (_NullTexture.Status <> rsReady) Then
  Begin
    _NullTexture.Update();
    FillTextureWithColor(_NullTexture, ColorNull);
  End Else
  If (Not _NullTexture.IsValid()) Then
  Begin
    _NullTexture.Unload();
  End;

  Result := _NullTexture;
End;

Function GetDefaultNormalColor():ColorRGBA;
Begin
  Result := ColorCreate(128,128,255);
End;

Function TextureManager.GetDefaultNormalMap:TERRATexture;
Begin
  If (Not Assigned(_DefaultNormalMap)) Then
    _DefaultNormalMap := Self.CreateTextureWithColor('default_normal', GetDefaultNormalColor())
  Else
  Begin
    If (_DefaultNormalMap.Status <> rsReady) Then
    Begin
      _DefaultNormalMap.Update();
      FillTextureWithColor(_DefaultNormalMap, GetDefaultNormalColor());
    End Else
    If (Not _DefaultNormalMap.IsValid()) Then
    Begin
      _DefaultNormalMap.Unload();
    End;
  End;

  Result := _DefaultNormalMap;
End;

{Function MyTest(P:Color):Color; CDecl;
Var
  V:ColorHSL;
Begin
  Result :=P; Exit;

   V := ColorRGBToHSL(P);
   V.H := 140;
   Result := ColorHSLToRGB(V);
   Result := ColorGreen;
End;}

Function TextureManager.GetDefaultColorTable:TERRATexture;
Begin
  If (Not Assigned(_DefaultColorTable)) Then
  Begin
    _DefaultColorTable := DefaultColorTableTexture.Create(rtDynamic);
    _DefaultColorTable.InitFromSize(1024, 32, ColorNull);
    _DefaultColorTable.Rebuild();
  End Else
  If (Not _DefaultColorTable.IsValid()) Then
  Begin
    _DefaultColorTable.Rebuild();
  End;

  Result := _DefaultColorTable;
End;

Procedure TextureManager.Release;
begin
  ReleaseObject(_WhiteTexture);
  ReleaseObject(_BlackTexture);
  ReleaseObject(_NullTexture);
  ReleaseObject(_DefaultColorTable);
  ReleaseObject(_DefaultNormalMap);
  ReleaseObject(_CellNoise);

  Inherited;
End;

Function TextureManager.GetCellNoise: TERRATexture;
Var
  Noise:NoiseGenerator;
  Img:TERRAImage;
Begin
  If _CellNoise = Nil Then
  Begin
    Noise := CellNoiseGenerator.Create();
    //Noise := PerlinNoiseGenerator.Create();
    Img := TERRAImage.Create(512, 512);

    Noise.SaveToImage(Img, 0.0, maskRGB);
    //Img.Save('cellnoise.png');

    _CellNoise := TERRATexture.Create(rtDynamic);
    _CellNoise.InitFromImage(Img);

    ReleaseObject(Img);
    ReleaseObject(Noise);
  End;

  Result := _CellNoise;
End;

End.
