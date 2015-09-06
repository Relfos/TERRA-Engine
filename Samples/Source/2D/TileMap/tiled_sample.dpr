{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

uses
  TERRA_MemoryManager,
  TERRA_Object,
  TERRA_Application,
  TERRA_DemoApplication,
  TERRA_Utils,
  TERRA_Renderer,
  TERRA_ResourceManager,
  TERRA_GraphicsManager,
  TERRA_Log,
  TERRA_OS, 
  TERRA_Vector2D,
  TERRA_Font,
  TERRA_Texture,
  TERRA_FileManager,
  TERRA_InputManager,
  TERRA_Collections,
  TERRA_Viewport,
  TERRA_EngineManager,
  TERRA_Matrix3x3,
  TERRA_Math,
  TERRA_Color,
  TERRA_String,
  TERRA_Sprite,
  TERRA_UIDimension,
  TERRA_UIDebug,
  TERRA_TileMap;

Type
  MyDemo = Class(DemoApplication)
    Protected
      _Map:TileMap;

    Public
			Procedure OnCreate; Override;
      Procedure OnRender2D(View:TERRAViewport); Override;
  End;

{ Game }
Procedure MyDemo.OnCreate;
Begin                      
  Inherited;

  // Enable 2D viewport for rendering
  Self.GUI.Viewport.Visible := True;

  UIMemoryAllocDebugWidget.Create('meminfo', Self.GUI, UIPixels(10), UIPixels(40), 90, UIPercent(100), UIPercent(100));

  // load a tilemap
  _Map := TileMap.Create();
  _Map.Load('demo_map');
End;

Procedure MyDemo.OnRender2D(View: TERRAViewport);
Begin
  Inherited;

  Engine.Graphics.AddRenderable(View, _Map);
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}


End.




