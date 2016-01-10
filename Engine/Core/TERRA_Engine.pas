Unit TERRA_Engine;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Application, TERRA_Threads, TERRA_List,
  TERRA_GraphicsManager, TERRA_TextureManager, TERRA_MeshManager, TERRA_FontManager, TERRA_InputManager, TERRA_Pool,
  TERRA_PhysicsManager, TERRA_ParticleRenderer, TERRA_ShaderFactory, TERRA_MeshAnimation, TERRA_Lights, TERRA_UICursor,
  TERRA_Sprite, TERRA_Log,
  TERRA_FileManager, TERRA_SoundManager, TERRA_FileFormat, TERRA_MusicManager, TERRA_MIDI, TERRA_Localization,
  TERRA_Callstack, TERRA_Error, TERRA_Network, TERRA_NetDownloader
  {$IFDEF PC}, TERRA_Steam{$ENDIF};

Type
  EngineManager = Class(TERRAObject)
    Protected
      _Pool:TERRAPool;
      _Error:TERRAError;

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

      _RendererList:TERRAList;

      _Log:TERRALog;

      {$IFDEF PC}
      _Steam:SteamManager;
      {$ENDIF}

      Procedure SetError(Value:TERRAError);

    Public
      Constructor Create();

      Procedure Init();
      Procedure Release(); Override;

      Procedure Update();
      Procedure OnContextLost();

      Procedure RaiseError(Const Desc:TERRAString);

      Function CreateObject(Const KeyName, ObjectType:TERRAString):TERRAObject;

      Function FetchSprite():TERRASprite;

      Property Textures:TextureManager Read _Textures;
      Property Meshes:MeshManager Read _Meshes;
      Property Animations:AnimationManager Read _Animations;
      Property Fonts:FontManager Read _Fonts;
      Property Graphics:GraphicsManager Read _Graphics;

      Property Input:InputManager Read _InputManager;
      Property Physics:PhysicsManager Read _Physics;
      Property Particles:ParticleManager Read _Particles;
      Property Lights:LightManager Read _Lights;

      Property Renderers:TERRAList Read _RendererList;

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

      Property Log:TERRALog Read _Log;

      Property Pool:TERRAPool Read _Pool;

      Property Error:TERRAError Read _Error Write SetError;

      {$IFDEF PC}
      Property Steam:SteamManager Read _Steam;
      {$ENDIF}
  End;

Function Engine():EngineManager;

Implementation
Uses TERRA_NullRenderer, TERRA_UIView, TERRA_UIDimension, TERRA_Color, TERRA_Vector2D, TERRA_Vector3D, TERRA_Vector4D, TERRA_Quaternion,
  TERRA_Texture, TERRA_Font, TERRA_DebugDraw, TERRA_ClipRect;

Var
  _EngineManager:EngineManager = Nil;

Function Engine():EngineManager;
Begin
  If _EngineManager = Nil Then
  Begin
    EngineManager.Create();
    _EngineManager.Renderers.Add(NullRenderer.Create());
  End;

  Result := _EngineManager;
End;

{ EngineManager }
Constructor EngineManager.Create;
Begin
  _EngineManager := Self;

  DebugClipRect.Style := clipNothing;

  _Pool := TERRAPool.Create();
  _Log := TERRALog.Create();

  _Tasks := ThreadPool.Create();

  _InputManager := InputManager.Create();
  _FileManager := FileManager.Create();
  _Formats := FormatManager.Create();

  _Physics := PhysicsManager.Create();
  _Animations := AnimationManager.Create();

{$IFNDEF DISABLETHREADS}
{$IFNDEF DISABLESOUND}
  _Audio := SoundManager.Create();
{$IFNDEF DISABLEMUSIC}
  _Music := MusicManager.Create();
  _MIDI := MidiManager.Create();
{$ENDIF}
{$ENDIF}
{$ENDIF}

  _Cursors := CursorManager.Create();

  _Network := NetworkManager.Create();
  _HTTP := DownloadManager.Create();

  _Localization := LocalizationManager.Create();

  {$IFDEF PC}
  _Steam := SteamManager.Create();
  {$ENDIF}

  _RendererList := TERRAList.Create();
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

{$IFNDEF DISABLESOUND}
{$IFNDEF DISABLEMUSIC}
  ReleaseObject(_MIDI);
  ReleaseObject(_Music);
{$ENDIF}
  ReleaseObject(_Audio);
{$ENDIF}

  ReleaseObject(_Localization);

  {$IFDEF PC}
  ReleaseObject(_Steam);
  {$ENDIF}

  ReleaseObject(_HTTP);
  ReleaseObject(_Network);

  ReleaseObject(_FileManager);
  ReleaseObject(_InputManager);
  ReleaseObject(_Formats);

  ReleaseObject(_RendererList);
  ReleaseObject(_Pool);

  ReleaseObject(_Log);
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

{$IFNDEF DISABLESOUND}
{$IFNDEF DISABLEMUSIC}
  _Music.Update();
  _MIDI.Update();
{$ENDIF}
  _Audio.Update();
{$ENDIF}


  _Network.Update();
  _HTTP.Update();

  {$IFDEF PC}
  _Steam.Update();
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
    Result := UIView.Create(Name, UIPercent(100), UIPercent(100), 0)
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

Procedure EngineManager.RaiseError(Const Desc:TERRAString);
Begin
  SetError(TERRAError.Create(Desc, Nil));
  Raise _Error;
End;

Procedure EngineManager.SetError(Value:TERRAError);
Begin
  ReleaseObject(_Error);
  _Error := Value;
End;

Function EngineManager.FetchSprite():TERRASprite;
Begin
  Result := TERRASprite(Engine.Pool.Fetch(TERRASprite));
  If (Assigned(Result)) Then
    Result.Create()
  Else
    Result := TERRASprite.Create();

  Result.Temporary := True;
End;

End.
