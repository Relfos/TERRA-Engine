Unit TERRA_EngineManager;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_GraphicsManager, TERRA_TextureManager, TERRA_MeshManager, TERRA_Application;

Type
  EngineManager = Class(TERRAObject)
    Protected
      _TextureManager:ApplicationObject;
      _MeshManager:ApplicationObject;

      Constructor Create();

    Public
      Procedure Release(); Override;

      Function Textures():TextureManager;
      Function Meshes():MeshManager;
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
End;

Procedure EngineManager.Release;
Begin
  _TextureManager := Nil;
End;

Function EngineManager.Textures: TextureManager;
Begin
  Result := TextureManager(_TextureManager.Instance);
End;

Function EngineManager.Meshes: MeshManager;
Begin
  Result := MeshManager(_MeshManager.Instance);
End;


End.
