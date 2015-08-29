Unit TERRA_EngineManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Application, TERRA_Threads,
  TERRA_GraphicsManager, TERRA_TextureManager, TERRA_MeshManager, TERRA_FontManager, TERRA_InputManager,
  TERRA_PhysicsManager, TERRA_ParticleRenderer, TERRA_ShaderFactory, TERRA_MeshAnimation, TERRA_Lights,
  TERRA_FileManager, TERRA_SoundManager, TERRA_FileFormat, TERRA_MusicManager, TERRA_MIDI, TERRA_Localization,
  {$IFDEF PC}TERRA_Steam{$ENDIF};

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

      _Audio:SoundManager;
      _Midi:MidiManager;
      _Music:MusicManager;

      _ShaderFactory:TERRAShaderFactory;
      _Lights:LightManager;
      _Particles:ParticleManager;
      _Physics:PhysicsManager;

      _Localization:LocalizationManager;

      _Tasks:ThreadPool;

      {$IFDEF PC}
      _Steam:SteamManager;
      {$ENDIF}

    Public
      Constructor Create();

      Procedure Init();
      Procedure Release(); Override;

      Procedure Update();
      Procedure OnContextLost();

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

      Property Formats:FormatManager Read _Formats;
      Property Files:FileManager Read _FileManager;
      Property Tasks:ThreadPool Read _Tasks;

      {$IFDEF PC}
      Property Steam:SteamManager Read _Steam;
      {$ENDIF}
  End;

Function Engine():EngineManager;

Implementation

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

  _Localization := LocalizationManager.Create();

  {$IFDEF PC}
  _Steam := SteamManager.Create();
  {$ENDIF}
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

  ReleaseObject(_MIDI);
  ReleaseObject(_Music);
  ReleaseObject(_Audio);

  ReleaseObject(_Localization);

  {$IFDEF PC}
  ReleaseObject(_Steam);
  {$ENDIF}

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

  {$IFDEF PC}
  _Steam.Update();
  {$ENDIF}
End;

End.
