Unit TERRA_EngineManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Application, TERRA_Threads,
  TERRA_GraphicsManager, TERRA_TextureManager, TERRA_MeshManager, TERRA_FontManager, TERRA_InputManager,
  TERRA_PhysicsManager, TERRA_ParticleRenderer, TERRA_ShaderFactory, TERRA_MeshAnimation, TERRA_Lights, TERRA_UICursor,
  TERRA_FileManager, TERRA_SoundManager, TERRA_FileFormat, TERRA_MusicManager, TERRA_MIDI, TERRA_Localization,
  TERRA_Network, TERRA_NetDownloader
  {$IFDEF PC}, TERRA_Steam{$ENDIF};

Type
  EngineManager = Class(TERRAObject)
    Protected
      _Textures:TextureManager;
      _Meshes:MeshManager;
      _Animations:AnimationManager;
      _Fonts:FontManager;
      _Graphics:GraphicsManager;

      _FileManager:FileManager;
      _InputManager:InputManager;
      _Formats:FormatManager;

      _Cursors:CursorManager;

      _Audio:SoundManager;
      _Midi:MidiManager;
      _Music:MusicManager;

      _ShaderFactory:TERRAShaderFactory;
      _Lights:LightManager;
      _Particles:ParticleManager;
      _Physics:PhysicsManager;

      _Localization:LocalizationManager;

      _Tasks:ThreadPool;

      _Network:NetworkManager;

      _HTTP:DownloadManager;

      {$IFDEF PC}
      _Steam:SteamManager;
      {$ENDIF}

    Public
      Constructor Create();

      Procedure Init();
      Procedure Release(); Override;

      Procedure Update();
      Procedure OnContextLost();

      Function CreateObject(Const KeyName, ObjectType:TERRAString):TERRAObject;

      Property Textures:TextureManager Read _Textures;
      Property Meshes:MeshManager Read _Meshes;
      Property Animations:AnimationManager Read _Animations;
      Property Fonts:FontManager Read _Fonts;
      Property Graphics:GraphicsManager Read _Graphics;

      Property Input:InputManager Read _InputManager;
      Property Physics:PhysicsManager Read _Physics;
      Property Particles:ParticleManager Read _Particles;
      Property Lights:LightManager Read _Lights;

      Property Audio:SoundManager Read _Audio;
      Property Music:MusicManager Read _Music;
      Property MIDI:MidiManager Read _MIDI;

      Property ShaderFactory:TERRAShaderFactory Read _ShaderFactory;

      Property Localization:LocalizationManager Read _Localization;

      Property Cursors:CursorManager Read _Cursors;

      Property Formats:FormatManager Read _Formats;
      Property Files:FileManager Read _FileManager;
      Property Tasks:ThreadPool Read _Tasks;

      Property Network:NetworkManager Read _Network;
      Property HTTP:DownloadManager Read _HTTP;

      {$IFDEF PC}
      Property Steam:SteamManager Read _Steam;
      {$ENDIF}
  End;

Function Engine():EngineManager;

Implementation
Uses TERRA_List, TERRA_UIView, TERRA_UIDimension, TERRA_Color, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Quaternion,
  TERRA_Texture, TERRA_Font;

Var
  _EngineManager:EngineManager = Nil;

Function Engine():EngineManager;
Begin
  If _EngineManager = Nil Then
    _EngineManager := EngineManager.Create();

  Result := _EngineManager;
End;

{ EngineManager }
Constructor EngineManager.Create;
Begin
  _InputManager := InputManager.Create();
  _FileManager := FileManager.Create();
  _Formats := FormatManager.Create();

  _Physics := PhysicsManager.Create();
  _Animations := AnimationManager.Create();

  _Audio := SoundManager.Create();
  _Music := MusicManager.Create();
  _MIDI := MidiManager.Create();

  _Cursors := CursorManager.Create();

  _Network := NetworkManager.Create();
  _HTTP := DownloadManager.Create();

  _Localization := LocalizationManager.Create();

  {$IFDEF PC}
  _Steam := SteamManager.Create();
  {$ENDIF}
End;

Function EngineManager.CreateObject(const KeyName, ObjectType: TERRAString): TERRAObject;
Begin
  If (StringEquals(ObjectType, 'string')) Then
    Result := StringProperty.Create(Name, '')
  Else
  If (StringEquals(ObjectType, 'color')) Then
    Result := ColorProperty.Create(Name, ColorWhite)
  Else
  If (StringEquals(ObjectType, 'vec2')) Then
    Result := Vector2DProperty.Create(Name, Vector2D_Zero)
  Else
  If (StringEquals(ObjectType, 'vec3')) Then
    Result := Vector3DProperty.Create(Name, Vector3D_Zero)
  Else
(*  If (StringEquals(ObjectType, 'vec4')) Then
    Result := Vector4DProperty.Create(Name, Vector4D_Zero)
  Else
  If (StringEquals(ObjectType, 'quaternion')) Then
    Result := QuaternionProperty.Create(Name, Quaternion_Zero)
  Else*)
  If (StringEquals(ObjectType, 'float')) Then
    Result := FloatProperty.Create(Name, 0)
  Else
  If (StringEquals(ObjectType, 'integer')) Then
    Result := IntegerProperty.Create(Name, 0)
  Else
  If (StringEquals(ObjectType, 'byte')) Then
    Result := ByteProperty.Create(Name, 0)
  Else
  If (StringEquals(ObjectType, 'angle')) Then
    Result := AngleProperty.Create(Name, 0)
  Else
  If (StringEquals(ObjectType, 'dimension')) Then
    Result := DimensionProperty.Create(Name, UIPercent(100))
  Else
  If (StringEquals(ObjectType, 'margin')) Then
    Result := MarginProperty.Create(Name)
  Else
  If StringEquals(ObjectType, 'list') Then
  Begin
    Result := TERRAList.Create();
    Result.Name := KeyName;
  End Else
  If (StringEquals(ObjectType, 'UI')) Then
    Result := UIView.Create(Name, UIPercent(100), UIPercent(100))
  Else
  If (StringEquals(ObjectType, 'texture')) Then
    Result := TextureProperty.Create(Name, Textures.WhiteTexture)
  Else
  If (StringEquals(ObjectType, 'font')) Then
    Result := FontProperty.Create(Name, Fonts.DefaultFont)
  Else
  If (StringEquals(ObjectType, 'path')) Then
    Result := PathProperty.Create(Name, '')
  Else
    Result := Nil;
End;

Procedure EngineManager.Init();
Begin
  _Graphics := GraphicsManager.Create();
  _Textures := TextureManager.Create();
  _Meshes := MeshManager.Create();
  _Fonts := FontManager.Create();
  _Particles := ParticleManager.Create();
  _Lights := LightManager.Create();
  _ShaderFactory := TERRAShaderFactory.Create();
End;

Procedure EngineManager.OnContextLost;
Begin
  Graphics.Renderer.OnContextLost();
End;

Procedure EngineManager.Release;
Begin
  ReleaseObject(_Meshes);
  ReleaseObject(_Fonts);
  ReleaseObject(_Lights);
  ReleaseObject(_ShaderFactory);
  ReleaseObject(_Particles);
  ReleaseObject(_Textures);
  ReleaseObject(_Graphics);

  ReleaseObject(_Animations);
  ReleaseObject(_Physics);
  ReleaseObject(_Tasks);

  ReleaseObject(_Cursors);

  ReleaseObject(_MIDI);
  ReleaseObject(_Music);
  ReleaseObject(_Audio);

  ReleaseObject(_Localization);

  {$IFDEF PC}
  ReleaseObject(_Steam);
  {$ENDIF}

  ReleaseObject(_HTTP);
  ReleaseObject(_Network);

  ReleaseObject(_FileManager);
  ReleaseObject(_InputManager);
  ReleaseObject(_Formats);
End;

Procedure EngineManager.Update;
Begin
  _Physics.Update();
  _Meshes.Update();
  _Animations.Update();
  _Fonts.Update();
  _Textures.Update();
  _Graphics.Update();

  _InputManager.Update();

  _Audio.Update();
  _Music.Update();
  _MIDI.Update();

  _Network.Update();

  {$IFDEF PC}
  _Steam.Update();
  {$ENDIF}
End;

End.
