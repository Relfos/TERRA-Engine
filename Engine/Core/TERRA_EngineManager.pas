Unit TERRA_EngineManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Application,
  TERRA_GraphicsManager, TERRA_TextureManager, TERRA_MeshManager, TERRA_FontManager, TERRA_InputManager,
  TERRA_FileManager, TERRA_FileFormat;

Type
  EngineManager = Class(TERRAObject)
    Protected
      _TextureManager:ApplicationObject;
      _MeshManager:ApplicationObject;
      _FontManager:ApplicationObject;
      _InputManager:ApplicationObject;
      _FileManager:ApplicationObject;

      _Formats:FormatManager;

      Constructor Create();

    Public
      Procedure Release(); Override;

      Function Textures():TextureManager;
      Function Meshes():MeshManager;
      Function Fonts():FontManager;
      Function Input():InputManager;
      Function Files():FileManager;
      Property Formats:FormatManager Read _Formats;
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
  _TextureManager := InitializeApplicationComponent(TextureManager, GraphicsManager);
  _MeshManager := InitializeApplicationComponent(MeshManager, GraphicsManager);
  _FontManager := InitializeApplicationComponent(FontManager, TextureManager);
  _InputManager := InitializeApplicationComponent(InputManager, Nil);
  _FileManager :=  InitializeApplicationComponent(FileManager, Nil);

  _Formats := FormatManager.Create();
End;

Procedure EngineManager.Release;
Begin
  _TextureManager := Nil;
  _MeshManager := Nil;
  _FontManager := Nil;

  ReleaseObject(_Formats);
End;

Function EngineManager.Textures: TextureManager;
Begin
  Result := TextureManager(_TextureManager.Instance);
End;

Function EngineManager.Meshes: MeshManager;
Begin
  Result := MeshManager(_MeshManager.Instance);
End;

Function EngineManager.Fonts: FontManager;
Begin
  Result := FontManager(_FontManager.Instance);
End;

Function EngineManager.Input: InputManager;
Begin
  Result := InputManager(_InputManager.Instance);
End;

Function EngineManager.Files: FileManager;
Begin
  Result := FileManager(_FileManager.Instance);
End;

End.
