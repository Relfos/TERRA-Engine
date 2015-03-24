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
 * TERRA_Engine
 * Implements an flat API for the whole engine
 ***********************************************************************************************************************
}

Library TERRA_Engine;

{$I terra.inc}

{-$DEFINE DEBUG_TERRA}
Uses TERRA_Utils, TERRA_Color, TERRA_Vector3D, TERRA_Vector2D, TERRA_Matrix4x4, TERRA_Matrix3x3,
  TERRA_BoundingBox, TERRA_Texture, TERRA_Camera, TERRA_Ray, TERRA_Tilemap,
  TERRA_Application, TERRA_ResourceManager, TERRA_Lights, TERRA_Shader, TERRA_FileManager,
  TERRA_Resource, TERRA_Client, TERRA_UI, TERRA_Skybox, TERRA_MusicManager, TERRA_InputManager,
  TERRA_Font, TERRA_Scene, TERRA_XML, TERRA_GraphicsManager, TERRA_SpriteManager,
  TERRA_Mesh, TERRA_MeshAnimation, TERRA_Stream, TERRA_MemoryStream, TERRA_FileStream, TERRA_Viewport,

  TERRA_Log, TERRA_Localization, TERRA_Widgets, TERRA_Image, TERRA_ShaderFactory,
  TERRA_SoundManager, TERRA_Sound, TERRA_TextureAtlas, TERRA_Leaderboards,
  TERRA_PNG, TERRA_Milkshape, TERRA_OBJ, TERRA_Collada, TERRA_TTF, TERRA_AngelCodeFont,
  TERRA_ParticleRenderer, TERRA_ParticleEmitters, TERRA_OS,
  TERRA_FileUtils, TERRA_HTTP, TERRA_AIGridPath, TERRA_Session,
  TERRA_Decals, TERRA_Billboards, TERRA_Network, TERRA_NetClient, TERRA_Sockets, TERRA_WAVE,
  {$IFNDEF MOBILE}TERRA_OGG,{$ENDIF}
  {$IFNDEF IPHONE}TERRA_JPG,{$ENDIF}
  {$IFDEF IPHONE}
  //TERRA_PVR,
  {$ENDIF}
  {$IFDEF PC}
  {$IFNDEF LINUX}
//  TERRA_DDS,
  {$ENDIF}
  {$ENDIF}
  TERRA_Frustum, {$IFDEF GLDEBUG}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_DebugDraw, TERRA_Tween, TERRA_UITransition;

{$IFDEF WINDOWS}
{$ENDIF}

{$IFDEF OSX}
{$LINKFRAMEWORK opengl}
{$LINKFRAMEWORK Carbon}
{$ENDIF}

{$I TERRA_types.inc}

Type
  EngineScene = Class(Scene)
    Public
      _OnRenderShadowCasters:TERRASceneCallback;
      _OnRenderViewport:TERRASceneCallback;
      _OnRenderSky:TERRASceneCallback;
      _OnRenderSprites:TERRASceneCallback;

      Constructor Create();
      Destructor Destroy; Override;

      Procedure RenderShadowCasters(V:Viewport); Override;
      Procedure RenderViewport(V:Viewport); Override;
      Procedure RenderSky(V:Viewport); Override;
      Procedure RenderSprites(V:Viewport); Override;
  End;

  EngineClient = Class(AppClient)
    Protected
      _InitCalled:Boolean;

			Procedure OnCreate;  Override;
      Procedure OnDestroy;  Override;
			Procedure OnIdle;  Override;

			Procedure OnKeyDown(Key:Word);  Override;
			Procedure OnKeyUp(Key:Word);  Override;
      Procedure OnKeyPress(Key:Word);  Override;

			Procedure OnMouseDown(X,Y:Integer;Button:Word); Override;
			Procedure OnMouseUp(X,Y:Integer;Button:Word); Override;
			Procedure OnMouseMove(X,Y:Integer); Override;
			Procedure OnMouseWheel(X,Y:Integer; Delta:Integer); Override;

			Procedure OnAccelerometer(X,Y,Z:Single); Override;

			Procedure OnStateChange(State:Integer); Override;

      Function GetTitle:String; Override;
      Function GetHandle:Cardinal; Override;
      Function GetWidth:Word; Override;
      Function GetHeight:Word; Override;
      Function GetFullScreen:Boolean; Override;
      Function GetVSync:Boolean; Override;
      Function GetAntialiasSamples:Integer; Override;
  End;

Var
  _Client:EngineClient;
  _Scene:EngineScene;

  _OnError:TERRAErrorCallback = Nil;

  _OnCreate:TERRAEventCallback = Nil;
  _OnDestroy:TERRAEventCallback = Nil;
  _OnIdle:TERRAEventCallback = Nil;

  _OnMouseDown:TERRAMouseEventCallback = Nil;
  _OnMouseUp:TERRAMouseEventCallback = Nil;
  _OnMouseMove:TERRAMouseMoveCallback = Nil;
  _OnMouseWheel:TERRAMouseWheelCallback = Nil;

  _OnKeyDown:TERRAKeyEventCallback = Nil;
  _OnKeyUp:TERRAKeyEventCallback = Nil;

  _OnAccelerometer:TERRAAccelerometerCallback = Nil;
  _OnStateChange:TERRAStateChangeCallback = Nil;

  _AppWidth:Integer;
  _AppHeight:Integer;
  _AppTitle:String;
  _AppHandle:Integer;
  _AppFullScreen:Boolean;
  _AppVSync:Boolean;
  _AppAntialiasSamples:Integer;
  _AppLogging:Boolean;

Procedure ClearCallbacks;
Begin
  _OnCreate := Nil;
  _OnDestroy := Nil;
  _OnIdle:= Nil;

  _OnMouseDown := Nil;
  _OnMouseUp := Nil;
  _OnMouseMove := Nil;
  _OnMouseWheel := Nil;

  _OnKeyDown := Nil;
  _OnKeyUp := Nil;

  _OnAccelerometer := Nil;
  _OnStateChange := Nil;
End;

Procedure SetError(ErrorCode:Integer);
Begin
  If Assigned(_OnError) Then
    _OnError(ErrorCode);
End;

Function ValidateType(P:Pointer; ClassType:TClass):Boolean;
Begin
  If (P=Nil) Then
  Begin
    Result := False;
    Exit
  End;

  {$IFDEF DEBUG_TERRA}
  Result := (TObject(P)) Is ClassType;

  If Not Result Then
    SetError(errorInvalidType);
  {$ENDIF}
End;

{ EngineClient }
Function EngineClient.GetAntialiasSamples: Integer;
Begin
  Result := _AppAntialiasSamples;
End;

Function EngineClient.GetFullScreen: Boolean;
Begin
  Result := _AppFullScreen;
End;

Function EngineClient.GetHandle: Cardinal;
Begin
  Result := _AppHandle;
End;

Function EngineClient.GetTitle: String;
Begin
  Result := _AppTitle;
End;

Function EngineClient.GetVSync: Boolean;
Begin
  Result := _AppVSync;
End;

Function EngineClient.GetWidth: Word;
Begin
  If (_AppWidth<=0) Then
    _AppWidth := 960;

  Result := _AppWidth;
End;

Function EngineClient.GetHeight: Word;
Begin
  If (_AppHeight<=0) Then
    _AppHeight := 640;

   Result := _AppHeight;
End;

Procedure EngineClient.OnAccelerometer(X, Y, Z: Single);
Begin
  If Assigned(_OnAccelerometer) Then
    _OnAccelerometer(X, Y, Z);
End;

Procedure EngineClient.OnCreate;
Begin
  _Scene := EngineScene.Create();
  GraphicsManager.Instance.Scene := Scene(_Scene);
End;

Procedure EngineClient.OnDestroy;
Begin
  If Assigned(_OnDestroy) Then
    _OnDestroy;
End;

Procedure EngineClient.OnIdle;
Begin
  If (Not _InitCalled) Then
  Begin
    _InitCalled := True;
    If Assigned(_OnCreate) Then
      _OnCreate();
    Exit;
  End;

  If Assigned(_OnIdle) Then
    _OnIdle();
End;

Procedure EngineClient.OnKeyDown(Key: Word);
Begin
  If Assigned(_OnKeyDown) Then
    _OnKeyDown(Key);
End;

Procedure EngineClient.OnKeyPress(Key:Word);
Var
  I:Integer;
Begin
  For I:=0 To Pred(UIManager.Instance.Count) Do
    UIManager.Instance.GetUI(I).OnKeyPress(Key);
End;

Procedure EngineClient.OnKeyUp(Key: Word);
Begin
  If Assigned(_OnKeyUp) Then
    _OnKeyUp(Key);
End;

Procedure EngineClient.OnMouseDown(X, Y: Integer; Button: Word);
Var
  I:Integer;
Begin
  For I:=0 To Pred(UIManager.Instance.Count) Do
    UIManager.Instance.GetUI(I).OnMouseDown(X, Y, Button);

  If Assigned(_OnMouseDown) Then
    _OnMouseDown(X,Y, Button);
End;

Procedure EngineClient.OnMouseMove(X, Y: Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(UIManager.Instance.Count) Do
    UIManager.Instance.GetUI(I).OnMouseMove(X, Y);

  If Assigned(_OnMouseMove) Then
    _OnMouseMove(X,Y);
End;

Procedure EngineClient.OnMouseUp(X, Y: Integer; Button: Word);
Var
  I:Integer;
Begin
  For I:=0 To Pred(UIManager.Instance.Count) Do
    UIManager.Instance.GetUI(I).OnMouseUp(X, Y, Button);

  If Assigned(_OnMouseUp) Then
    _OnMouseUp(X,Y, Button);
End;

Procedure EngineClient.OnMouseWheel(X, Y, Delta: Integer);
Var
  I:Integer;
Begin
  For I:=0 To Pred(UIManager.Instance.Count) Do
    UIManager.Instance.GetUI(I).OnMouseWheel(X, Y, Delta);

  If Assigned(_OnMouseWheel) Then
    _OnMouseWheel(Delta);
End;

Procedure EngineClient.OnStateChange(State: Integer);
Begin
  If Assigned(_OnStateChange) Then
    _OnStateChange(State);
End;

(*Procedure Widget_Reveal(MyWidget:TERRAWidget; DurationPerLetter, Delay:Integer; OnReveal:WidgetEventHandler); Cdecl;
Var
  W:Widget;
Begin
  If (MyWidget=Nil) Then
    Exit;

  W := Widget(MyWidget);
  If W Is UILabel Then
  Begin
    UILabel(W).OnReveal := OnReveal;
    UILabel(W).Reveal(DurationPerLetter, Delay);
  End;
End;

//Pathfinding
Function AI_PathGetSize(Path:TERRAPath):Integer;  Cdecl; external {$IFNDEF STATIC_LINKING}TERRA_DLL{$ENDIF};
Function AI_PathGetNextNode(Path:TERRAPath):TERRAPathNode;  Cdecl; external {$IFNDEF STATIC_LINKING}TERRA_DLL{$ENDIF};
Procedure AI_NodeGetCoords(Node:TERRAPathNode; Var X,Y:Integer); Cdecl; external {$IFNDEF STATIC_LINKING}TERRA_DLL{$ENDIF};

// Search a path in a limited area
Function AI_CreatePath(StartX,StartY:Integer; EndX,EndY:Integer; MinX,MinY:Integer; MaxX,MaxY:Integer;
                      Var Path:TERRAPath; Flags:Integer; GetCostCallback:PathCostCallback; VisitCallback:VisitNodeCallback = Nil):Integer; external {$IFNDEF STATIC_LINKING}TERRA_DLL{$ENDIF};
Procedure AI_DestroyPath(Var Path:TERRAPath); external {$IFNDEF STATIC_LINKING}TERRA_DLL{$ENDIF};
*)

Type
  CustomNetClient = Class(NetClient)
    Procedure HandleMessageCustom(Msg:NetMessage);
    Procedure ConnectionStart; Override;
    Procedure ConnectionEnd(ErrorCode:Integer; ErrorLog:String); Override;
  End;

Var
  _NetClient:CustomNetClient;
  _NetHandlers:Array[0..255] Of TERRANetworkHandler;
  _OnConnectionStart:TERRANetworkCallback;
  _OnConnectionEnd:TERRANetworkCallback;

Procedure CustomNetClient.ConnectionStart;
Begin
  If Assigned(_OnConnectionStart) Then
    _OnConnectionStart(0);
End;

Procedure CustomNetClient.ConnectionEnd(ErrorCode:Integer; ErrorLog:String);
Begin
  If Assigned(_OnConnectionEnd) Then
    _OnConnectionEnd(ErrorCode);
End;

Procedure CustomNetClient.HandleMessageCustom(Msg:NetMessage);
Var
  S:Stream;
Begin
  If (Msg =Nil) Or (Not Assigned(_NetHandlers[Msg.OpCode])) Then
    Exit;

  _NetHandlers[Msg.OpCode](Msg);
End;

{ EngineScene }
Constructor EngineScene.Create();
Begin
  _OnRenderShadowCasters := Nil;
  _OnRenderViewport := Nil;
  _OnRenderSky := Nil;
End;

Destructor EngineScene.Destroy;
Begin
End;

Procedure EngineScene.RenderSprites;
Begin
  If (Assigned(_OnRenderSprites)) Then
    _OnRenderSprites(Nil);
End;

Procedure EngineScene.RenderViewport(V:Viewport);
Begin
  If (Assigned(_OnRenderViewport)) Then
    _OnRenderViewport(V);
End;

Procedure EngineScene.RenderShadowCasters;
Begin
  If (Assigned(_OnRenderShadowCasters)) Then
    _OnRenderShadowCasters(V);
End;

Procedure EngineScene.RenderSky;
Begin
  If (Assigned(_OnRenderSky)) Then
    _OnRenderSky(V);
End;

{$IFDEF MOBILE}
procedure internal_initializeunits(); external name 'FPC_INITIALIZEUNITS';

Function TERRA_LoadLibrary():Boolean; Cdecl;
Begin
  internal_initializeunits();
  Result := True;
End;
{$ENDIF}

{BEGIN_API}
Procedure Settings_SetWidth(Width:Integer); CDecl;
Begin
  _AppWidth  := Width;
End;

Procedure Settings_SetHeight(Height:Integer); CDecl;
Begin
  _AppHeight  := Height;
End;

Procedure Settings_SetTitle(Title:PAnsiChar); CDecl;
Begin
  _AppTitle  := Title;
End;

Procedure Settings_SetHandle(Handle:Integer); CDecl;
Begin
  _AppHandle  := Handle;
End;

Procedure Settings_SetFullScreen(Fullscreen:Boolean); CDecl;
Begin
  _AppFullScreen  := FullScreen;
End;

Procedure Settings_SetVSync(Value:Boolean); CDecl;
Begin
  _AppVSync := Value;
End;

Procedure Settings_SetAntialias(Samples:Integer); CDecl;
Begin
  _AppAntialiasSamples  := Samples;
End;

Procedure Settings_SetLogging(Value:Boolean); CDecl;
Begin
  _AppLogging  := Value;
End;

Procedure Scene_SetOnRenderShadowCasters(Callback:TERRASceneCallback); Cdecl;
Begin
  _Scene._OnRenderShadowCasters := Callback;
End;

Procedure Scene_SetOnRenderViewport(Callback:TERRASceneCallback); Cdecl;
Begin
  _Scene._OnRenderViewport := Callback;
End;

Procedure Scene_SetOnRenderSprites(Callback:TERRASceneCallback); Cdecl;
Begin
  _Scene._OnRenderSprites := Callback;
End;

Procedure Scene_SetOnRenderSky(Callback:TERRASceneCallback); Cdecl;
Begin
  _Scene._OnRenderSky := Callback;
End;

Procedure TERRA_SetOnError(Callback:TERRAErrorCallback); CDecl;
Begin
  _OnError := Callback;
End;


Procedure TERRA_SetOnCreate(Callback:TERRAEventCallback); CDecl;
Begin
  _OnCreate := Callback;
End;

Procedure TERRA_SetOnDestroy(Callback:TERRAEventCallback); CDecl;
Begin
  _OnDestroy := Callback;
End;

Procedure TERRA_SetOnIdle(Callback:TERRAEventCallback); CDecl;
Begin
  _OnIdle := Callback;
End;

Procedure TERRA_SetOnMouseDown(Callback:TERRAMouseEventCallback); CDecl;
Begin
  _OnMouseDown := Callback;
End;

Procedure TERRA_SetOnMouseUp(Callback:TERRAMouseEventCallback); CDecl;
Begin
  _OnMouseUp := Callback;
End;

Procedure TERRA_SetOnMouseMove(Callback:TERRAMouseMoveCallback); CDecl;
Begin
  _OnMouseMove := Callback;
End;

Procedure TERRA_SetOnMouseWheel(Callback:TERRAMouseWheelCallback); CDecl;
Begin
  _OnMouseWheel := Callback;
End;


Procedure TERRA_SetOnKeyDown(Callback:TERRAKeyEventCallback); CDecl;
Begin
  _OnKeyDown := Callback;
End;

Procedure TERRA_SetOnKeyUp(Callback:TERRAKeyEventCallback); CDecl;
Begin
  _OnKeyUp := Callback;
End;

Procedure TERRA_SetOnAccelerometer(Callback:TERRAAccelerometerCallback); CDecl;
Begin
  _OnAccelerometer := Callback;
End;

Procedure TERRA_SetOnStateChange(Callback:TERRAStateChangeCallback); CDecl;
Begin
  _OnStateChange := Callback;
End;

Procedure TERRA_Run(); Cdecl;                                                                                                     
Begin
  Application.Instance.Run();
End;

Procedure TERRA_Init(); Cdecl;
Begin
  _Client := EngineClient.Create;
  ApplicationStart(_Client);
End;

Procedure TERRA_Shutdown(); Cdecl;
Begin
  Application.Instance.Terminate;
End;

Procedure TERRA_Resize(W:Integer; H:Integer); Cdecl;
Begin
  Application.Instance.Resize(W, H);
End;

Procedure TERRA_Log(Desc:PAnsiChar; LogType:Integer = logDebug); Cdecl;
Begin
  Log(LogType, 'Engine', Desc);
End;

Function TERRA_GetTime():Cardinal; Cdecl;
Begin
  Result := Application.Instance.GetElapsedTime();
End;

Function TERRA_GetWidth():Cardinal; Cdecl;
Begin
  Result := Application.Instance.Width;
End;

Function TERRA_GetHeight():Cardinal; Cdecl;
Begin
  Result := Application.Instance.Height;
End;

Procedure TERRA_Pause(Pause:Boolean); Cdecl;
Begin
  Application.Instance.Paused := Pause;
End;

Function TERRA_Paused():Boolean; Cdecl;
Begin
  Result := Application.Instance.Paused;
End;

Procedure TERRA_EnableAds(); Cdecl;
Begin
  Application.Instance.EnableAds;
End;

Procedure TERRA_SendAnalytics(Event:PAnsiChar; Value:PAnsiChar); Cdecl;
Begin
  Application.Instance.SendAnalytics(Event, Value);
End;

Var
  _Lang:String;
  _Str:Array[0..3] Of String;
  _StrIndex :Integer = 0;

Function Localization_GetLanguage():PAnsiChar; Cdecl;
Begin
  _Lang := Application.Instance.Language;
  _Lang[Length(_Lang) + 1] := #0;
  Result := @(_Lang[1]);
End;

Procedure Localization_SetLanguage(Lang:PAnsiChar); Cdecl;
Begin
  If Lang<>Nil Then
    LocalizationManager.Instance.SetLanguage(Lang);
End;

Function Localization_GetString(ID:PAnsiChar):PAnsiChar; Cdecl;
Begin
  _Str[_StrIndex] := LocalizationManager.Instance.GetString(ID);
  _Str[_StrIndex, Length(_Str[_StrIndex]) + 1] := #0;
  Result := @(_Str[_StrIndex, 1]);
  Inc(_StrIndex);
  If (_StrIndex>3) Then
    _StrIndex := 0;
End;

Procedure Input_SetKeyState(Key:Integer; State:Boolean); Cdecl;
Begin
  InputManager.Instance.Keys.SetState(Key, State);
End;

Function Input_KeyDown(Key:Integer):Boolean; Cdecl;
Begin
  Result := InputManager.Instance.Keys.IsDown(Key);
End;

Function Input_KeyPressed(Key:Integer):Boolean; Cdecl;
Begin
  Result := InputManager.Instance.Keys.WasPressed(Key);
End;

Function Input_GetMousePosition():Vector2D; Cdecl;
Begin
  Result := VectorCreate2D(InputManager.Instance.Mouse.X , InputManager.Instance.Mouse.X);
End;

Function Stream_CreateBuffer(Size:Integer; Ptr:Pointer = Nil):TERRAStream;  Cdecl;
Begin
  Result := MemoryStream.Create(Size, Ptr);
End;

Function Stream_Create(Name:PAnsiChar):TERRAStream; Cdecl;
Begin
  Result := FileStream.Create(Name);
End;

Function Stream_Open(Name:PAnsiChar):TERRAStream; Cdecl;
Begin
  Result := MemoryStream.Create(Name);
End;

Function Stream_EOF(MyStream:TERRAStream):Boolean;  Cdecl;
Begin
  If Not ValidateType(MyStream, Stream) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := Stream(MyStream).EOF;
End;

Procedure Stream_Destroy(Var MyStream:TERRAStream); Cdecl;
Begin
  If Not ValidateType(MyStream, Stream) Then
    Exit;

  Stream(MyStream).Destroy;
  MyStream := Nil;
End;

Var
  Buf:Array[0..1024] Of Char;

Function Stream_ReadLine(MyStream:TERRAStream):PAnsiChar;  Cdecl;
Var
  S:String;
  I:Integer;
Begin
  Result := Nil;

  If Not ValidateType(MyStream, Stream) Then
    Exit;

  Stream(MyStream).ReadLine(S);
  For I:=0 To Pred(Length(S)) Do
    Buf[I] := S[I+1];
  Buf[Length(S)] := #0;
  Result := @Buf;
End;

Function Stream_Read(MyStream:TERRAStream; Data:Pointer; Size:Integer):Integer; Cdecl;
Begin
  Result := 0;

  If Not ValidateType(MyStream, Stream) Then
    Exit;

  Result := Stream(MyStream).Read(Data, Size);
End;

Function Stream_Write(MyStream:TERRAStream; Data:Pointer; Size:Integer):Integer; Cdecl;
Begin
  Result := 0;

  If Not ValidateType(MyStream, Stream) Then
    Exit;

  Result := Stream(MyStream).Write(Data, Size);
End;

Procedure Stream_Seek(MyStream:TERRAStream; Position:Integer); Cdecl;
Begin
  If Not ValidateType(MyStream, Stream) Then
    Exit;

  Stream(MyStream).Seek(Position);
End;

Function Stream_GetSize(MyStream:TERRAStream):Integer; Cdecl;
Begin
  Result := 0;

  If Not ValidateType(MyStream, Stream) Then
    Exit;

  Result := Stream(MyStream).Size;
End;

Function Stream_GetPosition(MyStream:TERRAStream):Integer; Cdecl;
Begin
  Result := 0;
  If Not ValidateType(MyStream, Stream) Then
    Exit;

  Result := Stream(MyStream).Position;
End;

Var
  _FilePath:String;

Function AssetManager_Search(FileName:PAnsiChar):PAnsiChar; Cdecl;
Begin
  _FilePath := FileManager.Instance.SearchResourceFile(FileName);
  If (_FilePath<>'') Then
  Begin
    _FilePath[Length(_FilePath) + 1] := #0;
    Result := @(_FilePath[1]);
  End Else
    Result := Nil;
End;

Procedure AssetManager_AddPath(Path:PAnsiChar); Cdecl;
Begin
  FileManager.Instance.AddPath(Path);
End;

Procedure AssetManager_RemovePath(Path:PAnsiChar); Cdecl;
Begin
  FileManager.Instance.RemovePath(Path);
End;

Function AssetManager_GetTexture(Name:PAnsiChar):Pointer; Cdecl;
Begin
  Result := TextureManager.Instance.GetTexture(Name);
End;

Function AssetManager_GetSound(Name:PAnsiChar):Pointer; Cdecl;
Begin
  Result := SoundManager.Instance.GetSound(Name);
End;

Function AssetManager_GetMesh(Name:PAnsiChar):Pointer; Cdecl;
Begin
  Result := MeshManager.Instance.GetMesh(Name);
End;

Function AssetManager_GetFont(Name:PAnsiChar):Pointer; Cdecl;
Begin
  Result := FontManager.Instance.GetFont(Name);
End;

Procedure AssetManager_Prefetch(MyResource:Pointer); Cdecl;
Begin
  If Not ValidateType(MyResource, Resource) Then
    Exit;

  Resource(MyResource).Prefetch();
End;

Procedure AssetManager_Unload(MyResource:Pointer); Cdecl;
Begin
  If Not ValidateType(MyResource, Resource) Then
    Exit;

  Resource(MyResource).Unload();
End;

Function Session_Create(FileName:PAnsiChar):TERRASession; Cdecl;
Begin
  Result := Session.Create(FileName);
End;

Procedure Session_Save(MySession:TERRASession); Cdecl;
Begin
  If Not ValidateType(MySession, Session) Then
    Exit;

  Session(MySession).Save();
End;

Procedure Session_SetValue(MySession:TERRASession; Key:PAnsiChar; Value:PAnsiChar); Cdecl;
Begin
  If Not ValidateType(MySession, Session) Then
    Exit;

  Session(MySession).SetValue(Key, Value);
End;

Var
  _SV:String;

Function Session_GetValue(MySession:TERRASession; Key:PAnsiChar):PAnsiChar; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MySession, Session) Then
    Exit;

  _SV := Session(MySession).GetValue(Key);
  _SV := _SV + #0;
  Result := @(_SV[1]);
End;

Function Font_GetTextWidth(MyFont:TERRAFont; Text:PAnsiChar):Single; Cdecl;
Begin
  Result := 0;

  If Not ValidateType(MyFont, Font) Then
    Exit;

  Result := UIManager.Instance.FontRenderer.SetFont(MyFont).GetTextWidth(Text);
End;

Function Font_GetTextHeight(MyFont:TERRAFont; Text:PAnsiChar):Single; Cdecl;
Begin
  Result := 0;

  If Not ValidateType(MyFont, Font) Then
    Exit;

  Result := UIManager.Instance.FontRenderer.SetFont(MyFont).GetTextHeight(Text);
End;

Function Font_GetTextRect(MyFont:TERRAFont; Text:PAnsiChar):Vector2D; Cdecl;
Begin
  Result := VectorCreate2D(0,0);

  If Not ValidateType(MyFont, Font) Then
    Exit;

  Result := UIManager.Instance.FontRenderer.SetFont(MyFont).GetTextRect(Text);
End;

Procedure Font_DrawText(MyFont:TERRAFont; X:Single; Y:Single; Layer:Single; Text:PAnsiChar; Const MyColor:Color); Cdecl;
Begin
  If Not ValidateType(MyFont, Font) Then
    Exit;

  UIManager.Instance.FontRenderer.SetFont(MyFont).SetColor(MyColor).DrawText(X, Y, Layer, Text);
End;

// Tile sheet generator
(*Function TileSheet_Create(Name:PAnsiChar):TERRATileSheet; Cdecl;
Begin
  Result := TextureAtlas.New(Name, 1024, 1024);
End;

Procedure TileSheet_AddFile(Sheet:TERRATileSheet; FileName:PAnsiChar; ID:Integer); Cdecl;
Begin
  If Not ValidateType(Sheet, TextureAtlas) Then
    Exit;

  TextureAtlas(Sheet).Add(FileName, ID);
End;

Procedure TileSheet_SetTilesPerRow(Sheet:TERRATileSheet; TilesPerRow:Integer); Cdecl;
Begin
  If Not ValidateType(Sheet, TextureAtlas) Then
    Exit;

  TextureAtlas(Sheet).TilesPerRow := TilesPerRow;
End;

Procedure TileSheet_Generate(Sheet:TERRATileSheet); Cdecl;
Begin
  If Not ValidateType(Sheet, TextureAtlas) Then
    Exit;

  TextureAtlas(Sheet).GenerateAtlas;
End;

Procedure TileSheet_Destroy(Var Sheet:TERRATileSheet); Cdecl;
Begin
  If Not ValidateType(TextureAtlas, Sheet) Then
    Exit;

  TextureAtlas(Sheet).Destroy;
  Sheet := Nil;
End;*)

//Shaders
Function Shader_GetFromFactory(FxFlags:Cardinal; OutFlags:Cardinal;  FogFlags:Cardinal; LightModel:Integer; DirLightCount:Integer;  PointLightCount:Integer;  SpotLightCount:Integer):TERRAShader; Cdecl;
Var
  Lights:LightBatch;
Begin
  Lights.DirectionalLightCount := DirLightCount;
  Lights.PointLightCount := PointLightCount;
  Lights.SpotLightCount := SpotLightCount;
  Result := ShaderFactory.Instance.GetShader(FxFlags, OutFlags, FogFlags, LightModel, Lights);
End;

Procedure Shader_Bind(MyShader:TERRAShader); Cdecl;
Begin
  If Not ValidateType(MyShader, Shader) Then
    Exit;

  ShaderManager.Instance.Bind(Shader(MyShader));
End;

Procedure Shader_SetDefaultCameraUniforms(); Cdecl;
Begin
  GraphicsManager.Instance.ActiveViewport.Camera.SetupUniforms;
End;

Function Shader_GetAttribute(MyShader:TERRAShader; AttribName:PAnsiChar):Integer; Cdecl;
Begin
  Result := -1;

  If Not ValidateType(MyShader, Shader) Then
    Exit;

  Result := glGetAttribLocation(Shader(MyShader).Handle, AttribName)
End;

Procedure Shader_SetFloat(MyShader:TERRAShader; UniformName:PAnsiChar; Value:Single); Cdecl;
Begin
  If Not ValidateType(MyShader, Shader) Then
    Exit;

  Shader(MyShader).SetUniform(UniformName, Value);
End;

Procedure Shader_SetVector(MyShader:TERRAShader; UniformName:PAnsiChar; Value:Vector3D); Cdecl;
Begin
  If Not ValidateType(MyShader, Shader) Then
    Exit;

  Shader(MyShader).SetUniform(UniformName, Value);
End;

Procedure Shader_SetColor(MyShader:TERRAShader; UniformName:PAnsiChar; Const Value:Color); Cdecl;
Begin
  If Not ValidateType(MyShader, Shader) Then
    Exit;

  Shader(MyShader).SetUniform(UniformName, Value);
End;

Procedure Shader_SetMatrix(MyShader:TERRAShader; UniformName:PAnsiChar; Const Value:Matrix4x4); Cdecl;
Begin
  If Not ValidateType(MyShader, Shader) Then
    Exit;

  Shader(MyShader).SetUniform(UniformName, Value);
End;

Procedure Texture_Bind(Tex:TERRATexture; Slot:Integer=0); Cdecl;
Begin
  If Not ValidateType(Tex, Texture) Then
  Begin
    TextureManager.Instance.WhiteTexture.Bind(Slot);
    Exit;
  End;

  Texture(Tex).Bind(Slot);
End;

Function Texture_GetWidth(Tex:TERRAImage):Integer; Cdecl;
Begin
  Result := 0;

  If Not ValidateType(Tex, Texture) Then
    Exit;

  Result := Texture(Tex).Width;
End;

Function Texture_GetHeight(Tex:TERRAImage):Integer; Cdecl;
Begin
  Result := 0;

  If Not ValidateType(Tex, Texture) Then
    Exit;

  Result := Texture(Tex).Height;
End;

Function Texture_Create(Name:PAnsiChar; Width:Integer; Height:Integer):TERRATexture; Cdecl;
Var
  S:String;
Begin
  S := Name;
  If Pos('@', S)<=0 Then
    S := '@' + S;

  Result := Texture.New(S, Width, Height);
End;

Procedure Texture_UpdateRect(Tex:TERRATexture; Source:TERRAImage; X:Integer; Y:Integer); Cdecl;
Begin
  If Not ValidateType(Tex, Texture) Then
    Exit;

  Texture(Tex).UpdateRect(Image(Source), X, Y);
End;

Function Texture_GetImage(Tex:TERRATexture):TERRAImage; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(Tex, Texture) Then
    Exit;

  Result := Texture(Tex).GetImage;
End;

Procedure Texture_SetWrapMode(Tex:TERRATexture; Wrap:Boolean); Cdecl;
Begin
  If Not ValidateType(Tex, Texture) Then
    Exit;

  Texture(Tex).SetWrap(Wrap);
End;

Procedure Texture_SetMipMapping(Tex:TERRATexture; UseMipmaps:Boolean); Cdecl;
Begin
  If Not ValidateType(Tex, Texture) Then
    Exit;

  Texture(Tex).MipMapped := UseMipMaps;
End;

Procedure Texture_SetFiltering(Tex:TERRATexture; Filtering:Integer); Cdecl;
Begin
  If Not ValidateType(Tex, Texture) Then
    Exit;

  Texture(Tex).BilinearFilter := (Filtering = textureFilterBilinear);
End;

Function Skybox_Create(Name:PAnsiChar):TERRASkybox; Cdecl;
Begin
  Result := Skybox.Create(Name);
End;

Procedure Skybox_Render(Sky:TERRASkybox); Cdecl;
Begin
  If Not ValidateType(Sky, Skybox) Then
    Exit;

  Skybox(Sky).Render;
End;

Procedure Skybox_Destroy(Var Sky:TERRASkybox); Cdecl;
Begin
  If Not ValidateType(Sky, Skybox) Then
    Exit;

  Skybox(Sky).Destroy;
  Sky := Nil;
End;

{HERE}
Function MeshInstance_Create(MyMesh:TERRAMesh):TERRAMeshInstance; Cdecl;
Begin
  If MyMesh = Nil Then
  Begin
    Result := Nil;
    Exit;
  End;

  Result := MeshInstance.Create(Mesh(MyMesh));
End;

Procedure MeshInstance_PlayAnimation(Instance:TERRAMeshInstance; AnimName:PAnsiChar); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).Animation.Play(AnimName);
End;

Procedure MeshInstance_CrossFadeAnimation(Instance:TERRAMeshInstance; AnimName:PAnsiChar; Duration:Cardinal); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).Animation.Crossfade(AnimName, Duration);
End;

Procedure MeshInstance_SetDiffuseColor(Instance:TERRAMeshInstance; Index:Integer; Const MyColor:Color); Cdecl;
Begin
  MeshInstance(Instance).SetDiffuseColor(Index, MyColor);
End;

Procedure MeshInstance_SetDiffuseMap(Instance:TERRAMeshInstance; Index:Integer; Tex:TERRATexture); Cdecl;
Begin
  MeshInstance(Instance).SetDiffuseMap(Index, Tex);
End;

Procedure MeshInstance_SetVisibility(Instance:TERRAMeshInstance; Index:Integer; Visible:Boolean); Cdecl;
Begin
  MeshInstance(Instance).SetVisibility(Index, Visible);
End;

Function MeshInstance_Intersect(Instance:TERRAMeshInstance; R:Ray; Var T:Single):Boolean; Cdecl;
Var
  M:Mesh;
Begin
  Result := False;
  If (Instance = Nil) Then
    Exit;

  M := MeshInstance(Instance).Geometry;
  If (M = Nil) Then
    Exit;
  Result := M.Intersect( R, T, MeshInstance(Instance).Transform);
End;

Procedure MeshInstance_SetPosition(Instance:TERRAMeshInstance; Position:Vector3D); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).Position := Position;
End;

Procedure MeshInstance_SetRotation(Instance:TERRAMeshInstance; Rotation:Vector3D); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).Rotation := Rotation;
End;

Procedure MeshInstance_SetScale(Instance:TERRAMeshInstance; Scale:Vector3D); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).Scale := Scale;
End;

Procedure MeshInstance_Destroy(Var Instance:TERRAMeshInstance); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).Destroy;
  Instance := Nil;
End;

Function MeshInstance_GetGroup(Instance:TERRAMeshInstance; Name:PAnsiChar):TERRAMeshGroup; Cdecl;
Begin
  If Instance = Nil Then
    Result := Nil
  Else
    Result := MeshInstance(Instance).Geometry.GetGroup(Name);
End;

Procedure MeshInstance_SetGroupLocalTransform(Instance:TERRAMeshInstance; Index:Integer; Const Transform:Matrix4x4); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).SetGroupLocalTransform(Index, Transform);
End;

Procedure MeshInstance_SetGroupGlobalTransform(Instance:TERRAMeshInstance; Index:Integer; Const Transform:Matrix4x4); Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  MeshInstance(Instance).SetGroupGlobalTransform(Index, Transform);
End;

Function MeshInstance_GetBoundingBox(Instance:TERRAMeshInstance):BoundingBox; Cdecl;
Begin
  If Instance = Nil Then
    Exit;

  Result := MeshInstance(Instance).GetBoundingBox;
End;

Function MeshGroup_GetIndex(Group:TERRAMeshGroup):Integer; Cdecl;
Begin
  If Group = Nil Then
    Exit;

  Result := MeshGroup(Group).ID;
End;

Function MeshGroup_GetBoundingBox(Group:TERRAMeshGroup):BoundingBox; Cdecl;
Begin
  If Group = Nil Then
    Exit;

  Result := MeshGroup(Group).GetBoundingBox;
End;

Function Occluder_Create():TERRAOccluder; Cdecl;
Begin
  Result := Occluder.Create;
End;

Procedure Occluder_SetTransform(Occluder:TERRAOccluder; Const Transform:Matrix4x4; Width:Single; Height:Single); Cdecl;
Begin
  If (Occluder = Nil) Then
    Exit;

  TERRA_GraphicsManager.Occluder(Occluder).SetTransform(Transform, Width, Height);
End;

Function Occluder_PointOccluded(Occluder:TERRAOccluder; P:Vector3D):Boolean; Cdecl;
Begin
  If (Occluder = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := TERRA_GraphicsManager.Occluder(Occluder).PointOccluded(P);
End;

Function Occluder_BoxOccluded(Occluder:TERRAOccluder; Box:BoundingBox):Boolean; Cdecl;
Begin
  If (Occluder = Nil) Then
  Begin
    Result := False;
    Exit;
  End;

  Result := TERRA_GraphicsManager.Occluder(Occluder).BoxOccluded(Box, GraphicsManager.Instance.ActiveViewport);
End;

Procedure Occluder_Destroy(Var Occluder:TERRAOccluder); Cdecl;
Begin
  If (Occluder = Nil) Then
    Exit;

  TERRA_GraphicsManager.Occluder(Occluder).Destroy;
  Occluder := Nil;
End;

Function Graphics_IsBoxVisible(Box:BoundingBox):Boolean; Cdecl;
Begin
  Result := GraphicsManager.Instance.ActiveViewport.Camera.Frustum.BoxVisible(Box);
End;

Procedure Graphics_SetBackgroundColor(Const MyColor:Color); Cdecl;
Begin
  GraphicsManager.Instance.ActiveViewport.BackgroundColor := MyColor;
End;

Procedure Graphics_SetDynamicShadows(Enabled:Boolean); Cdecl;
Begin
  GraphicsManager.Instance.Settings.DynamicShadows.SetValue(Enabled);
End;

Procedure Graphics_SetPostProcessing(Enabled:Boolean); Cdecl;
Begin
  GraphicsManager.Instance.Settings.PostProcessing.SetValue(Enabled);
End;

Procedure Graphics_SetNormalMapping(Enabled:Boolean); Cdecl;
Begin
  GraphicsManager.Instance.Settings.NormalMapping.SetValue(Enabled);
End;

Procedure Graphics_SetDepthOfField(Enabled:Boolean); Cdecl;
Begin
  GraphicsManager.Instance.Settings.DepthOfField.SetValue(Enabled);
End;

Procedure Graphics_SetSSAO(Enabled:Boolean); Cdecl;
Begin
  GraphicsManager.Instance.Settings.SSAO.SetValue(Enabled);
End;

Procedure Graphics_SetFogFlags(Flags:Cardinal); Cdecl;
Begin
  GraphicsManager.Instance.Settings.FogMode := Flags;
End;

Procedure Graphics_SetFogColor(Const MyColor:Color); Cdecl;
Begin
  GraphicsManager.Instance.Settings.FogColor := myColor;
End;

Procedure Graphics_SetFogDensity(Density:Single); Cdecl;
Begin
  GraphicsManager.Instance.Settings.FogDensity := Density;
End;

Function Graphics_StatsTriangleCount():Integer; Cdecl;
Begin
  Result := GraphicsManager.Instance.Stats.TriangleCount;
End;

Function Graphics_StatsShaderSwitches():Integer; Cdecl;
Begin
  Result := GraphicsManager.Instance.Stats.ShaderSwitches;
End;

Function Graphics_StatsDrawCalls():Integer; Cdecl;
Begin
  Result := GraphicsManager.Instance.Stats.DrawCalls;
End;

Function Graphics_StatsLightCount():Integer; Cdecl;
Begin
  Result := GraphicsManager.Instance.Stats.LightCount;
End;

Function Graphics_StatsOccluderCount():Integer; Cdecl;
Begin
  Result := GraphicsManager.Instance.Stats.OccluderCount;
End;

Function Graphics_StatsRenderableCount():Integer; Cdecl;
Begin
  Result := GraphicsManager.Instance.Stats.RenderableCount;
End;

Function Graphics_StatsFramesPerSecond():Integer; Cdecl;
Begin
  Result := GraphicsManager.Instance.Stats.FramesPerSecond;
End;

Function Graphics_GetPickRay(View:TERRAViewport; X:Integer; Y:Integer):Ray; Cdecl;
Begin
  If (View = Nil) Then
    Exit;

  Result := Viewport(View).GetPickRay(X, Y);
End;

Procedure Graphics_AddRenderable(MyRenderable:Pointer; Flags:Cardinal); Cdecl;
Begin
  If MyRenderable=Nil Then
    Exit;

  GraphicsManager.Instance.AddRenderable(Renderable(MyRenderable), Flags);
End;

Procedure Graphics_AddOccluder(Occluder:TERRAOccluder); Cdecl;
Begin
  If Occluder<>Nil Then
    GraphicsManager.Instance.AddOccluder(Occluder);
End;

Procedure Graphics_AddLight(Light:TERRALight);  Cdecl;
Begin
  If Light<>Nil Then
    LightManager.Instance.AddLight(Light);
End;

Function Graphics_AddSprite(X:Single;  Y:Single;  Layer:Single; Tex:TERRATexture):TERRASprite; Cdecl;
Begin
  Result := SpriteManager.Instance.DrawSprite(X, Y, Layer, Tex);
End;

Procedure Graphics_FlushSprites(); CDecl;
Begin
  SpriteManager.Instance.Flush;
End;

Function Graphics_GetActiveCamera():TERRACamera; Cdecl;
Begin
  Result := GraphicsManager.Instance.ActiveViewport.Camera;
End;

Function Graphics_GetActiveViewport():TERRAViewport; Cdecl;
Begin
  Result := GraphicsManager.Instance.ActiveViewport;
End;

Function Graphics_ElapsedTime():Single; Cdecl;
Begin
  Result := GraphicsManager.Instance.ElapsedTime;
End;

Procedure Light_SetPriority(MyLight:TERRALight; Priority:Integer); Cdecl;
Begin
  If Assigned(MyLight) Then
    Light(MyLight).Priority := Priority;
End;

Procedure Light_SetColor(MyLight:TERRALight; Const MyColor:Color); Cdecl;
Begin
  If Assigned(MyLight) Then
    Light(MyLight).Color := MyColor;
End;

Procedure Light_SetStatic(MyLight:TERRALight; IsStatic:Boolean); Cdecl;
Begin
  If Assigned(MyLight) Then
    Light(MyLight).Static := IsStatic;
End;

Function Light_CreateDirectional(Dir:Vector3D):TERRALight; Cdecl;
Begin
  Result := DirectionalLight.Create(Dir);
End;

Function Light_CreatePoint(Position:Vector3D; Radius:Single):TERRALight; Cdecl;
Begin
  Result := PointLight.Create(Position);
  PointLight(Result).Radius := Radius;
End;

Procedure Light_Destroy(Var P:TERRALight); Cdecl;
Begin
  If Assigned(P) Then
  Begin
    Light(P).Destroy;
    P := Nil;
  End;
End;

Procedure Debug_DrawBoundingBox(Const MyBox:BoundingBox; Const MyColor:Color); Cdecl;
Begin
  DrawBoundingBox(MyBox, MyColor);
End;

Procedure Debug_DrawFrustum(Const MyFrustum:Frustum; Const MyColor:Color); Cdecl;
Begin
  DrawFrustum(MyFrustum, MyColor);
End;

Procedure Debug_DrawRay(Const MyRay:Ray; Const  MyColor:Color; Length:Single=0); Cdecl;
Begin
  DrawRay(MyRay, MyColor, Length);
End;

Procedure Debug_DrawPlane(Const Position:Vector3D; Normal:Vector3D; Scale:Single; Const MyColor:Color); Cdecl;
Begin
  DrawPlane(Position, Normal, Scale, MyColor);
End;

Function Camera_Create(Name:PAnsiChar):TERRACamera; Cdecl;
Begin
  Result := Camera.Create(Name);
End;

Procedure Camera_Destroy(Var Cam:TERRACamera); Cdecl;
Begin
  If Assigned(Cam) Then
  Begin
    Camera(Cam).Destroy;
    Cam := Nil;
  End;
End;

Procedure Camera_Move(Cam:TERRACamera; Dir:Integer; Speed:Single); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).Move(Dir, Speed);
End;

Procedure Camera_Rotate(Cam:TERRACamera; X:Single; Y:Single); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).Rotate(X, Y);
End;

Procedure Camera_Fly(Cam:TERRACamera); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).FreeCam;
End;

Procedure Camera_SetPosition(Cam:TERRACamera; Position:Vector3D); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).SetPosition(Position);
End;

Procedure Camera_SetDirection(Cam:TERRACamera; Direction:Vector3D); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).FreeCam;
End;

Procedure Camera_SetNearPlane(Cam:TERRACamera; Distance:Single); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).Near := Distance;
End;

Procedure Camera_SetFarPlane(Cam:TERRACamera; Distance:Single); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).Far := Distance;
End;

Procedure Camera_LookAt(Cam:TERRACamera; Position:Vector3D); Cdecl;
Begin
  If Assigned(Cam) Then
    Camera(Cam).LookAt(Position);
End;

Function Camera_GetPosition(Cam:TERRACamera):Vector3D; Cdecl;
Begin
  If Assigned(Cam) Then
    Result := Camera(Cam).Position;
End;

Function Camera_GetDirection(Cam:TERRACamera):Vector3D; Cdecl;
Begin
  If Assigned(Cam) Then
    Result := Camera(Cam).View;
End;

Function Camera_GetRight(Cam:TERRACamera):Vector3D; Cdecl;
Begin
  If Assigned(Cam) Then
    Result := Camera(Cam).Right;
End;

Function Camera_GetProjection(Cam:TERRACamera):Matrix4x4; Cdecl;
Begin
  If Assigned(Cam) Then
    Result := Camera(Cam).Projection
  Else
    Result := Matrix4x4Identity;
End;

Function Camera_GetTransform(Cam:TERRACamera):Matrix4x4; Cdecl;
Begin
  If Assigned(Cam) Then
    Result := Camera(Cam).Transform
  Else
    Result := Matrix4x4Identity;
End;

Function Camera_GetUp(Cam:TERRACamera):Vector3D; Cdecl;
Begin
  If Assigned(Cam) Then
    Result := Camera(Cam).Up;
End;

Function Image_Create(Width:Integer; Height:Integer):TERRAImage; Cdecl;
Begin
  Result := Image.Create(Width, Height);
End;

Function Image_Load(FileName:PAnsiChar):TERRAImage; Cdecl;
Begin
  Result := Image.Create(FileName);
End;

Procedure Image_Destroy(Var Img:TERRAImage); Cdecl;
Begin
  If Assigned(Img) Then
  Begin
    Image(Img).Destroy;
    Img := Nil;
  End;
End;

Function Image_GetPixels(Image:TERRAImage):PColor; Cdecl;
Begin
  If Assigned(Image) Then
    Result := TERRA_Image.Image(Image).Pixels
  Else
    Result := Nil;
End;

Function Image_GetWidth(Image:TERRAImage):Integer; Cdecl;
Begin
  If Assigned(Image) Then
    Result := TERRA_Image.Image(Image).Width
  Else
    Result := 0;
End;

Function Image_GetHeight(Image:TERRAImage):Integer; Cdecl;
Begin
  If Assigned(Image) Then
    Result := TERRA_Image.Image(Image).Height
  Else
    Result := 0;
End;

Function Image_GetPixel(Image:TERRAImage; X:Integer; Y:Integer):Color; Cdecl;
Begin
  If Assigned(Image) Then
    Result := TERRA_Image.Image(Image).GetPixel(X,Y)
  Else
    Result := ColorBlack;
End;

Procedure Image_SetPixel(Image:TERRAImage; X:Integer; Y:Integer; Const Pixel:Color); Cdecl;
Begin
  If Assigned(Image) Then
    TERRA_Image.Image(Image).SetPixel(X,Y, Pixel);
End;

Function UI_Create():TERRAUI; Cdecl;
Begin
  Result := UI.Create();
  UIManager.Instance.AddUI(Result);
End;

Procedure UI_Destroy(MyUI:TERRAUI); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UIManager.Instance.RemoveUI(UI(MyUI));
  UI(MyUI).Destroy;
  MyUI := Nil;
End;

Procedure UI_SetColor(MyUI:TERRAUI; Const MyColor:Color); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).Color := MyColor;
End;

Procedure UI_SetFocus(MyUI:TERRAUI; W:TERRAWidget); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).SetFocus(W);
End;

Procedure UI_Clear(MyUI:TERRAUI); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).Clear();
End;

Function UI_GetLastWidget(MyUI:TERRAUI):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := UI(MyUI).LastWidget;
End;

Function UI_GetWidget(MyUI:TERRAUI; Name:PAnsiChar):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := UI(MyUI).GetWidget(Name);
End;

Procedure UI_DeleteWidget(MyUI:TERRAUI; MyWidget:TERRAWidget); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).DeleteWidget(MyWidget);
End;

Procedure UI_LoadCursor(MyUI:TERRAUI; Name:PAnsiChar); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).LoadCursor(Name);
End;

Function UI_GetDefaultFont(MyUI:TERRAUI):TERRAFont;  Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := UI(MyUI).DefaultFont;
End;

Procedure UI_SetDefaultFont(MyUI:TERRAUI; MyFont:TERRAFont); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).DefaultFont := Font(MyFont);
End;

Function UI_HasTransition(MyUI:TERRAUI):Boolean; Cdecl;
Begin
  Result := False;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := Assigned(UI(MyUI).Transition);
End;

Procedure UI_CreateSlideTransition(MyUI:TERRAUI; Direction:Vector2D; Duration:Cardinal = 1000; Delay:Cardinal = 0); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).Transition := UISlide.Create(Direction, Duration, Delay);
End;

Procedure UI_CreateFadeTransition(MyUI:TERRAUI; Tex:TERRATexture; Duration:Cardinal = 1000; Delay:Cardinal = 0; Invert:Boolean = False); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).Transition := UIFade.Create(Tex, Duration, Delay, Invert);
End;

Procedure UI_SetTransitionCallback(MyUI:TERRAUI; Callback:FadeCallback; UserData:Pointer = Nil; OnStart:Boolean=False); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  UI(MyUI).Transition.SetCallback(Callback, UserData, OnStart);
End;

Function UI_GetWidth():Integer; Cdecl;
Begin
  Result := UIManager.Instance.Width;
End;

Function UI_GetHeight():Integer; Cdecl;
Begin
  Result := UIManager.Instance.Height;
End;

Function UI_CreateWindow(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Width:Integer; Height:Integer):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  If Assigned(Parent) Then
    Result := TERRA_Widgets.UIWindow.Create(Name, UI(MyUI), Widget(Parent), X, Y, Z, Width, Height)
  Else
    Result := TERRA_Widgets.UIWindow.Create(Name, UI(MyUI), X, Y, Z, Width, Height);
End;

Function UI_CreateButton(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar; CustomPNG:PAnsiChar):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := TERRA_Widgets.UIButton.Create(Name, UI(MyUI), Parent, X, Y, Z, Caption, CustomPNG);
End;

Function UI_CreateEditText(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Width:Integer):TERRAWidget;  Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := TERRA_Widgets.UIEditText.Create(Name, UI(MyUI), Parent, X, Y, Z, Width);
End;

Function UI_CreateIcon(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Icon:PAnsiChar):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := TERRA_Widgets.UIIcon.Create(Name, UI(MyUI), Parent, X, Y, Z, Icon);
End;

Function UI_CreateLabel(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := TERRA_Widgets.UILabel.Create(Name, UI(MyUI), Parent, X, Y, Z, Caption);
End;

Function UI_CreateCheckbox(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := TERRA_Widgets.UICheckbox.Create(Name, UI(MyUI), Parent, X, Y, Z, Caption);
End;

Function UI_CreateRadioButton(MyUI:TERRAUI; Name:PAnsiChar; Parent:TERRAWidget; X:Single; Y:Single; Z:Single; Caption:PAnsiChar):TERRAWidget; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(MyUI, UI) Then
    Exit;

  Result := TERRA_Widgets.UIRadioButton.Create(Name, UI(MyUI), Parent, X, Y, Z, Caption);
End;

Procedure UI_MessageBox(MyUI:TERRAUI; Msg:PAnsiChar; Callback: WidgetEventHandler); Cdecl;
Begin
  If Assigned(MyUI) Then
    UI(MyUI).MessageBox(Msg, Callback);
End;

Procedure UI_ChoiceBox(MyUI:TERRAUI; Msg:PAnsiChar; Btn1:PAnsiChar; Btn2:PAnsiChar; Callback1:WidgetEventHandler; Callback2: WidgetEventHandler); Cdecl;
Begin
  If Not ValidateType(MyUI, UI) Then
    Exit;

  If Assigned(MyUI) Then
    UI(MyUI).ChoiceBox(Msg, Btn1, Btn2, Callback1, Callback2);
End;

Function Widget_AddTween(W:TERRAWidget; TweenType:Integer; TargetValue:Single; Time:Integer; Delay:Integer=0):TERRATween; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).AddTween(TweenType, TargetValue, Time, Delay);
End;

Procedure Widget_SetDragging(W:TERRAWidget; Value:Boolean); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  If (Widget(W) Is UIWindow) Then
    UIWindow(W).AllowDragging := Value;
End;

Procedure Widget_SetScale(W:TERRAWidget; Scale:Single);  Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).Scale := Scale;
End;


Procedure Widget_SetGroup(W:TERRAWidget; Group:Integer); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  If (Widget(W) Is UIRadioButton) Then
    UIRadioButton(W).Group := Group;
End;

Procedure Widget_SetChecked(W:TERRAWidget; Value:Boolean); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  If (Widget(W) Is UICheckbox) Then
    UICheckbox(W).Checked := Value;
End;

Function Widget_IsChecked(W:TERRAWidget):Boolean; Cdecl;
Begin
  Result := False;

  If Not ValidateType(W, Widget) Then
    Exit;

  If (Widget(W) Is UICheckbox) Then
    Result := UICheckbox(W).Checked;
End;

Function Widget_GetFont(W:TERRAWidget):TERRAFont; Cdecl;
Begin
  Result := Nil;
  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).Font;
End;

Procedure Widget_SetCaption(W:TERRAWidget; Caption:PAnsiChar); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  If Widget(W) Is UILabel Then
    UILabel(W).Caption := Caption
  Else
    Log(logWarning, 'Engine', 'This widget does not have a caption property: '+Widget(W).Name);
End;

Procedure Widget_SetText(W:TERRAWidget; Text:PAnsiChar); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  If Widget(W) Is UILabel Then
    UILabel(W).Caption := Text
  Else
  If Widget(W) Is UIEditText Then
    UIEditText(W).Text := Text
  Else
    Log(logWarning, 'Engine', 'This widget does not have a text property: '+Widget(W).Name);
End;

Procedure Widget_SetOnClick(W:TERRAWidget; OnClick:WidgetEventHandler); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).OnMouseClick := OnClick;
End;

Procedure Widget_SetOnRelease(W:TERRAWidget; OnClick:WidgetEventHandler); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).OnMouseRelease := OnClick;
End;

Procedure Widget_SetOnMouseOver(W:TERRAWidget; OnClick:WidgetEventHandler); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).OnMouseOver := OnClick;
End;

Procedure Widget_SetOnMouseOut(W:TERRAWidget; OnClick:WidgetEventHandler); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).OnMouseOut := OnClick;
End;

Procedure Widget_SetColor(W:TERRAWidget; Const MyColor:Color); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).Color := MyColor;
End;

Procedure Widget_SetPosition(W:TERRAWidget; Pos:Vector2D); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).Position := Pos;
End;


Procedure Widget_SetVisible(W:TERRAWidget; Visible:Boolean); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).Visible := Visible;
End;

Function Widget_ToggleVisibility(W:TERRAWidget; Flags:Cardinal; Delay:Cardinal = 0; EaseType:Integer = 0; Duration:Integer=1000):TERRATween; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).ToggleVisibility(Flags, Delay, EaseType, Duration);
End;

Function Widget_Hide(W:TERRAWidget; Flags:Cardinal; Delay:Cardinal =0; EaseType:Integer=0; Duration:Integer=1000):TERRATween; Cdecl;
Begin
  Result := Nil;
  
  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).Hide(Flags, Delay, EaseType, Duration);
End;

Function Widget_Show(W:TERRAWidget; Flags:Cardinal; Delay:Cardinal =0; EaseType:Integer=0; Duration:Integer=1000):TERRATween; Cdecl;
Begin
  Result := Nil;

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).Show(Flags, Delay, EaseType, Duration);
End;

Function Widget_IsVisible(W:TERRAWidget):Boolean; Cdecl;
Begin
  Result := False;

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).Visible;
End;

Function Widget_HasTweens(W:TERRAWidget):Boolean; Cdecl;
Begin
  Result := False;

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).HasTweens;
End;

Function Widget_GetSize(W:TERRAWidget):Vector2D; Cdecl;
Begin
  Result := VectorCreate2D(0, 0);

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).Size;
End;

Function Widget_GetPosition(W:TERRAWidget):Vector2D; Cdecl;
Begin
  Result := VectorCreate2D(0, 0);

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).Position;
End;

Function Widget_GetColor(W:TERRAWidget):Color; Cdecl;
Begin
  Result := ColorNull;

  If Not ValidateType(W, Widget) Then
    Exit;

  Result := Widget(W).Color;
End;

Var
  _Caption:String;

Function Widget_GetName(W:TERRAWidget):PAnsiChar; Cdecl;
Var
  S:String;
Begin
  Result := Nil;

  If Not ValidateType(W, Widget) Then
    Exit;

  _Caption := Widget(W).Name + #0;
  Result := @(_Caption[1]);
End;

Function Widget_GetCaption(W:TERRAWidget):PAnsiChar; Cdecl;
Var
  S:String;
Begin
  Result := Nil;

  If Not ValidateType(W, Widget) Then
    Exit;

  If (Widget(W) Is UILabel) Then
    S := UILabel(W).Caption
  Else
  If (Widget(W) Is UIButton) Then
    S := UIButton(W).Caption
  Else
  If (Widget(W) Is UIWindow) Then
    S := UIWindow(W).Caption
  Else
    Exit;

  If (S='') Then
    Exit;

  _Caption := S;
  _Caption[Length(_Caption)+1] := #0;
  Result := @(_Caption[1]);
End;

Function Widget_GetText(W:TERRAWidget):PAnsiChar; Cdecl;
Var
  S:String;
Begin
  Result := Nil;

  If Not ValidateType(W, Widget) Then
    Exit;

  If (Widget(W) Is UIEditText) Then
    S := UIEditText(W).Text
  Else
    Exit;

  If (S='') Then
    Exit;

  _Caption := S;
  _Caption[Length(_Caption)+1] := #0;
  Result := @(_Caption[1]);
End;

Procedure Widget_SetAlign(W:TERRAWidget; AlignMode:Cardinal);  Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).Align := AlignMode;
End;

Procedure Widget_CenterOnPoint(W:TERRAWidget; X:Single; Y:Single); Cdecl;
Begin
  If Not ValidateType(W, Widget) Then
    Exit;

  Widget(W).CenterOnPoint(X,Y);
End;

//Tweens
Function Tween_Create(MyType:Integer; Data:Pointer; TargetValue:Single; UserData:Pointer = Nil):TERRATween; Cdecl;
Begin
  Result := Tween.Create(UserData, MyType, Data, TargetValue, UserData);
End;

Procedure Tween_SetEaseType(T:TERRATween; EaseType:Integer); Cdecl;
Begin
  If Not ValidateType(T, Tween) Then
    Exit;

  Tween(T).EaseType := EaseType ;
End;

Procedure Tween_SetTime(T:TERRATween; Time:Cardinal); Cdecl;
Begin
  If Not ValidateType(T, Tween) Then
    Exit;


  Tween(T).Time := Time;
End;

Procedure Tween_SetDelay(T:TERRATween; Delay:Cardinal); Cdecl;
Begin
  If Not ValidateType(T, Tween) Then
    Exit;

  Tween(T).Delay := Delay;
End;

Procedure Tween_SetUserData(T:TERRATween; Data:Pointer); Cdecl;
Begin
  If Not ValidateType(T, Tween) Then
    Exit;

  Tween(T).UserData := Data;
End;

Procedure Tween_SetCallback(T:TERRATween; Callback:FadeCallback); Cdecl;
Begin
  If Not ValidateType(T, Tween) Then
    Exit;

  Tween(T).OnFinished := Callback;
End;

Procedure Sprite_SetPosition(Spr:TERRASprite; X:Single; Y:Single);  Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Position := VectorCreate2D(X, Y);
End;

Function Sprite_GetWidth(Spr:TERRASprite):Integer;  Cdecl;
Begin
  Result := 0;

  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Result := Sprite(Spr).Rect.Width
End;

Function Sprite_GetHeight(Spr:TERRASprite):Integer;  Cdecl;
Begin
  Result := 0;

  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Result := Sprite(Spr).Rect.Height;
End;

Procedure Sprite_SetWidth(Spr:TERRASprite; Width:Integer); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Rect.Width := Width;
End;

Procedure Sprite_SetHeight(Spr:TERRASprite; Height:Integer); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Rect.Height := Height;
End;

Procedure Sprite_SetAnchor(Spr:TERRASprite; P:Vector2D); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Anchor := P;
End;

Procedure Sprite_SetTransform(Spr:TERRASprite; Const Mat:Matrix3x3); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetTransform(Mat);
End;

Procedure Sprite_SetTransformOnCenter(Spr:TERRASprite; Const Center:Vector2D; Const Mat:Matrix3x3); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetTransform(Center, Mat);
End;


Procedure Sprite_SetScaleOnCenter(Spr:TERRASprite; Const Center:Vector2D; ScaleX:Single; ScaleY:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScale(Center, ScaleX, ScaleY);
End;

Procedure Sprite_SetScale(Spr:TERRASprite; ScaleX:Single; ScaleY:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScale(ScaleX, ScaleY);
End;

Procedure Sprite_SetUniformScale(Spr:TERRASprite; Scale:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScale(Scale);
End;

Procedure Sprite_SetRotation(Spr:TERRASprite; Angle:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleAndRotation(1.0, Angle);
End;

Procedure Sprite_SetScaleAndRotationOnCenter(Spr:TERRASprite; Const Center:Vector2D; ScaleX:Single; ScaleY:Single; Rotation:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleAndRotation(Center, ScaleX, ScaleY);
End;

Procedure Sprite_SetUniformScaleAndRotationOnCenter(Spr:TERRASprite; Const Center:Vector2D; Scale:Single; Rotation:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleAndRotation(Center, Scale, Rotation);
End;

Procedure Sprite_SetScaleAndRotation(Spr:TERRASprite; ScaleX:Single; ScaleY:Single; Rotation:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleAndRotation(ScaleX, ScaleY);
End;

Procedure Sprite_SetUniformScaleAndRotation(Spr:TERRASprite; Scale:Single; Rotation:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleAndRotation(Scale, Rotation);
End;


Procedure Sprite_SetScaleRelativeOnCenter(Spr:TERRASprite; Const Center:Vector2D; ScaleX:Single; ScaleY:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleRelative(Center, ScaleX, ScaleY);
End;

Procedure Sprite_SetUniformScaleRelativeOnCenter(Spr:TERRASprite; Const Center:Vector2D; Scale:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleRelative(Center, Scale);
End;

Procedure Sprite_SetScaleAndRotationRelativeOnCenter(Spr:TERRASprite; Const Center:Vector2D; ScaleX:Single; ScaleY:Single; Rotation:Single ); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleAndRotationRelative(Center, ScaleX, ScaleY, Rotation);
End;

Procedure Sprite_SetUniformScaleAndRotationRelativeOnCenter(Spr:TERRASprite; Const Center:Vector2D; Scale:Single; Rotation:Single ); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScaleAndRotationRelative(Center, Scale, Rotation);
End;

Procedure Sprite_SetTransformRelative(Spr:TERRASprite; Const Center:Vector2D; Const Mat:Matrix3x3); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetTransformRelative(Center, Mat);
End;


Procedure Sprite_SetColors(Spr:TERRASprite; Const A:Color; Const B:Color; Const C:Color; Const D:Color); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetColors(A,B,C,D);
End;

Procedure Sprite_SetColor(Spr:TERRASprite; Const C:Color); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetColor(C);
End;

Procedure Sprite_SetAlpha(Spr:TERRASprite; Alpha:Byte); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetAlpha(Alpha);
End;


Procedure Sprite_SetScroll(Spr:TERRASprite; U:Single; V:Single); CDecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).SetScroll(U,V);
End;


Procedure Sprite_SetMirror(Spr:TERRASprite; Mirror:Boolean); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Mirror := Mirror;
End;

Procedure Sprite_SetFlip(Spr:TERRASprite; Flip:Boolean); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Flip := Flip;
End;

Procedure Sprite_TileRemapByID(Spr:TERRASprite; TileID:Integer; TilesPerRow:Integer;  TileSize:Integer); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Rect.TileRemapByID(TileID, TilesPerRow, TileSize);
End;

Procedure Sprite_TileRemap(Spr:TERRASprite; X:Integer; Y:Integer;  TilesPerX:Integer;  TilesPerY:Integer); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Rect.TileRemap(X, Y, TilesPerX, TilesPerY);
End;

Procedure Sprite_PixelRemap(Spr:TERRASprite; X1:Integer; Y1:Integer;  X2:Integer;  Y2:Integer; W:Integer=0; H:Integer=0); Cdecl;
Begin
  If Not ValidateType(Spr, Sprite) Then
    Exit;

  Sprite(Spr).Rect.PixelRemap(X1, Y1, X2, Y2, W, H);
End;

{HERE}
Function TileMap_Create(Source:PAnsiChar):TERRATileMap; Cdecl;
Var
  Map:TileMap;
  S:String;
Begin
  S := Source;
  If (Pos('.', S)<=0) Then
    S := S + '.tmx';


  S := FileManager.Instance.SearchResourceFile(S);

  If (S <>'') Then
  Begin
    Map := TileMap.Create();
    Map.Load(S);
    Result := Map;
  End Else
    Result := Nil;
End;

Procedure TileMap_Destroy(Var Map:TERRATileMap); Cdecl;
Begin
  If Assigned(Map) Then
  Begin
    TileMap(Map).Destroy;
    Map := Nil;
  End;
End;

Procedure TileMap_Draw(Map:TERRATileMap; Depth:Single); Cdecl;
Begin
  If Assigned(Map) Then
    TileMap(Map).Render(Depth);
End;

Procedure TileMap_GetPosition(Map:TERRATileMap; Var X:Single; Y:Single); Cdecl;
Begin
  If Not Assigned(Map) Then
  Begin
    X := 0;
    Y := 0;
    Exit;
  End;

  X := TileMap(Map).CamX;
  Y := TileMap(Map).CamY;
End;

Procedure TileMap_SetPosition(Map:TERRATileMap; Var X:Single; Y:Single); Cdecl;
Var
  MaxX, MaxY:Single;
Begin
  If Not Assigned(Map) Then
    Exit;

  If (X<0) Then X := 0;
  If (Y<0) Then Y := 0;

  MaxX := TileMap(Map).GetLayer(0).Width * TileMap(Map).TileWidth - GraphicsManager.Instance.Width;
  MaxY := TileMap(Map).GetLayer(0).Height * TileMap(Map).TileHeight - GraphicsManager.Instance.Height;

  If (X>MaxX) Then X := MaxX;
  If (Y>MaxY) Then Y := MaxY;

  TileMap(Map).CamX := X;
  TileMap(Map).CamY := Y;
End;

Function TileMap_GetObjectCount(Map:TERRATileMap):Integer; Cdecl;
Begin
  If Assigned(Map) Then
    Result := TileMap(Map).ObjectCount
  Else
    Result := 0;
End;

Procedure TileMap_GetObjectPosition(Map:TERRATileMap; ID:Integer; Var X:Single; Var Y:Single); Cdecl;
Begin
  If (Assigned(Map)) And (ID>=0) And (ID<TileMap(Map).ObjectCount) Then
  Begin
    X := TileMap(Map).GetObject(ID).X;
    Y := TileMap(Map).GetObject(ID).Y;
  End;
End;

Procedure TileMap_GetObjectSize(Map:TERRATileMap; ID:Integer; Var W:Single; Var H:Single); Cdecl;
Begin
  If (Assigned(Map)) And (ID>=0) And (ID<TileMap(Map).ObjectCount) Then
  Begin
    W := TileMap(Map).GetObject(ID).Width;
    H := TileMap(Map).GetObject(ID).Height;
  End;
End;

Procedure TileMap_GetObjectTile(Map:TERRATileMap; ID:Integer; Var TX:Integer; Var TY:Integer); Cdecl;
Begin
  If (Assigned(Map)) And (ID>=0) And (ID<TileMap(Map).ObjectCount) Then
  Begin
    TX := TileMap(Map).GetObject(ID).TX;
    TY := TileMap(Map).GetObject(ID).TY;
  End;
End;

Function TileMap_GetTileAt(Map:TERRATileMap; X:Integer; Y:Integer; Layer:Integer=-1; UsePalette:Boolean = False):Integer; Cdecl;
Var
  I:Integer;
Begin
  Result := 0;
  If (TileMap(Map).LayerCount<=0) Then
    Exit;

  If (X<0) Or (Y<0) Or (X>= TileMap(Map).GetLayer(0).Width) Or (Y>= TileMap(Map).GetLayer(0).Height) Then
    Exit;

  For I:=Pred(TileMap(Map).LayerCount) DownTo 0 Do
  If (I <> Layer) And (Layer>=0) Then
    Continue
  Else
  If (TileMap(Map).GetLayer(I).GetTileAt(X,Y)>0) Then
  Begin
    Result := TileMap(Map).GetLayer(I).GetTileAt(X,Y);
    Exit;
  End;
End;

Var
  ObjProp:String;

Function TileMap_GetObjectProperty(Map:TERRATileMap; ID:Integer; Key:PAnsiChar):PAnsiChar; Cdecl;
Var
  I:Integer;
  Obj:PTileObject;
Begin
  Result := Nil;
  If (Assigned(Map)) And (ID>=0) And (ID<TileMap(Map).ObjectCount) Then
  Begin
    Obj := TileMap(Map).GetObject(ID);
    For I:=0 To Pred(Obj.PropertyCount) Do
    If (Obj.Properties[I].Key = Key) Then
    Begin
      ObjProp := Obj.Properties[I].Value  + #0;
      Result := PAnsiChar(ObjProp);
    End;
  End;
End;

Function TileMap_GetTileProperty(Map:TERRATileMap; ID:Integer; Key:PAnsiChar):PAnsiChar; Cdecl;
Var
  I:Integer;
Begin
  Result := Nil;
  If (Assigned(Map)) And (ID>=0) And (ID<MaxTileIDs) Then
    Result := PAnsiChar(TileMap(Map).GetTileProperty(ID, Key));
End;

Procedure Music_Play(FileName:PAnsiChar); Cdecl;
Begin
  MusicManager.Instance.Play(FileName);
End;

Procedure Music_Stop(); Cdecl;
Begin
  MusicManager.Instance.Stop;
End;

Procedure Music_SetVolume(Volume:Single); Cdecl;
Begin
  MusicManager.Instance.SetVolume(Volume);
End;

Function Sound_Play(MySound:TERRASound):TERRASoundSource; Cdecl;
Begin
  If MySound = Nil Then
    Exit;

  SoundManager.Instance.Play(Sound(MySound));
End;

//Pathfinding
Function AI_PathGetSize(Path:TERRAPath):Integer; Cdecl;
Begin
End;

Function AI_PathGetNextNode(Path:TERRAPath):TERRAPathNode; Cdecl;
Begin
End;

Procedure AI_NodeGetCoords(Node:TERRAPathNode; Var X:Integer; Var Y:Integer); Cdecl;
Begin
End;


// Search a path in a limited area
Function AI_CreatePath(StartX:Integer; StartY:Integer;
                      EndX:Integer; EndY:Integer;
                      MinX:Integer; MinY:Integer; MaxX:Integer; MaxY:Integer;
                      Var Path:TERRAPath; Flags:Integer;
                      GetCostCallback:TERRAPathCostCallback;
                      VisitCallback:TERRAVisitNodeCallback = Nil):Integer; Cdecl;
Begin
End;

Procedure AI_DestroyPath(Var Path:TERRAPath); Cdecl; Cdecl;
Begin
End;


Function XML_Open(Name:PAnsiChar):TERRAXML; Cdecl;
Var
  Src:Stream;
  Doc:XMLDocument;
Begin
  Src := FileManager.Instance.OpenStream(Name);
  If Assigned(Src) Then
  Begin
    Doc := XMLDocument.Create;
    Doc.Load(Src);
    Src.Destroy;
    Result := Doc;
  End Else
    Result := Nil;
End;

Function XML_GetRoot(XML:TERRAXML):TERRAXMLNode; Cdecl;
Begin
  If Assigned(XML) Then
    Result := XMLDocument(XML).Root
  Else
    Result := Nil;
End;

Function XML_GetNode(Node:TERRAXMLNode; Name:PAnsiChar):TERRAXMLNode; Cdecl;
Begin
  If Assigned(Node) Then
    Result := XMLNode(Node).GetNodeByName(Name)
  Else
    Result := Nil;
End;

Function XML_GetNodeByIndex(Node:TERRAXMLNode; ID:Integer):TERRAXMLNode; Cdecl;
Begin
  If Assigned(Node) Then
    Result := XMLNode(Node).GetNodeByIndex(ID)
  Else
    Result := Nil;
End;

Function XML_GetNodeCount(Node:TERRAXMLNode):Integer; Cdecl;
Begin
  If Assigned(Node) Then
    Result := XMLNode(Node).NodeCount
  Else
    Result := 0;
End;

Var
  XMLS:String;
Function XML_GetNodeName(Node:TERRAXMLNode):PAnsiChar; Cdecl;
Begin
  If Assigned(Node) Then
    XMLS := XMLNode(Node).Name + #0
  Else
    XMLS := #0;
  Result := @XMLS[1];
End;

Function XML_GetNodeValue(Node:TERRAXMLNode):PAnsiChar; Cdecl;
Begin
  If Assigned(Node) Then
    XMLS := XMLNode(Node).Value + #0
  Else
    XMLS := #0;
  Result := @XMLS[1];
End;

Procedure XML_Destroy(Var XML:TERRAXML); Cdecl;
Begin
  If Assigned(XML) Then
  Begin
    XMLDocument(XML).Destroy;
    XML := Nil;
  End;
End;

Procedure NetClient_SetOnConnectionStart(Handler:TERRANetworkCallback); Cdecl;
Begin
  _OnConnectionStart := Handler;
End;

Procedure NetClient_SetOnConnectionEnd(Handler:TERRANetworkCallback); Cdecl;
Begin
  _OnConnectionEnd := Handler;
End;

Function NetClient_GetLocalId():Integer; CDecl;
Begin
  If Assigned(_NetClient) Then
    Result := _NetClient._LocalID
  Else
    Result := -1;
End;

Function NetClient_IsConnected():Boolean; Cdecl;
Begin
  Result := (Assigned(_NetClient)) And (_NetClient._Status = nsConnected);
End;

Procedure NetClient_Connect(Address:PAnsiChar; Port:Word; Version:Word;  Username:PAnsiChar; Password:PAnsiChar); Cdecl;
Begin
  If Assigned(_NetClient) Then
    _NetClient.Destroy;

  _NetClient := CustomNetClient.Create();
  _NetClient.Connect(Port, Version, Address, Username, Password);
End;

Procedure NetClient_Disconnect(); Cdecl;
Begin
  If Assigned(_NetClient) Then
  Begin
    _NetClient.Destroy;
    _NetClient := Nil;
  End;
End;

Procedure NetClient_AddHandler(OpCode:Byte; Handler:TERRANetworkHandler); Cdecl;
Begin
  If (Not Assigned(_NetClient)) Then
    Exit;

  _NetClient._OpcodeList[OpCode] := _NetClient.HandleMessageCustom;
  _NetHandlers[OpCode] := Handler;
End;

Function NetClient_SendMessage(S:TERRANetMessage):Boolean; Cdecl;
Begin
  If Not Assigned(_NetClient) Then
    Exit;

  _NetClient.SendMessage(NetMessage(S));
End;

Procedure Decal_Add(TextureName:PAnsiChar; Position:Vector3D; Normal:Vector3D; Const DecalColor:Color; Size:Single; Rotation:Single = 0; Duration:Integer=20000); CDecl;
Begin
  DecalManager.Instance.AddDecal(TextureName, Position, Normal, DecalColor, Size, Rotation,Duration);
End;

Function Billboard_Add(Position:Vector3D; Width:Single; Height:Single; MyTexture:TERRATexture):TERRABillboard; CDecl;
Begin
  Result := BillboardManager.Instance.AddBillboard(Position, Width, Height, MyTexture);
End;

Procedure Billboard_Remap(Billboard:TERRABillboard; U1:Single; V1:Single; U2:Single; V2:Single); CDecl;
Begin
  If (Billboard = Nil) Then
    Exit;
  PBillboard(Billboard).U1 := U1;
  PBillboard(Billboard).V1 := V1;
  PBillboard(Billboard).U2 := U2;
  PBillboard(Billboard).V2 := V2;
End;

Function Particles_Spawn(FXName:PAnsiChar; Position:Vector3D; Loop:Boolean):TERRAParticles; CDecl;
Begin
  Result := ParticleSettingsEmitter.Create(FXName, Position);
  If Not Loop Then
    ParticleManager.Instance.AddParticleCollection(Result);
End;

Procedure Particles_Release(Var Particles:TERRAParticles); CDecl;
Begin
  If Particles = Nil Then
    Exit;

  ParticleManager.Instance.AddParticleCollection(ParticleCollection(Particles));
  Particles := Nil;
End;

Procedure Particles_Destroy(Var Particles:TERRAParticles); CDecl;
Begin
  If Particles = Nil Then
    Exit;

  ParticleCollection(Particles).Destroy;
  Particles := Nil;
End;

{END_API}

Exports
  Settings_SetWidth,
  Settings_SetHeight,
  Settings_SetTitle,
  Settings_SetHandle,
  Settings_SetFullScreen,
  Settings_SetVSync,
  Settings_SetAntialias,
  Settings_SetLogging,

  Graphics_SetBackgroundColor,
  Graphics_SetDynamicShadows,
  Graphics_SetPostProcessing,
  Graphics_SetNormalMapping,
  Graphics_SetDepthOfField,
  Graphics_SetSSAO,
  Graphics_SetFogFlags,
  Graphics_SetFogDensity,
  Graphics_SetFogColor,
  Graphics_StatsTriangleCount,
  Graphics_StatsShaderSwitches,
  Graphics_StatsDrawCalls,
  Graphics_StatsLightCount,
  Graphics_StatsOccluderCount,
  Graphics_StatsRenderableCount,
  Graphics_StatsFramesPerSecond,
  Graphics_AddRenderable,
  Graphics_AddOccluder,
  Graphics_AddSprite,
  Graphics_AddLight,
  Graphics_FlushSprites,
  Graphics_GetActiveCamera,
  Graphics_IsBoxVisible,
  Graphics_ElapsedTime,
  Graphics_GetActiveViewport,
  Graphics_GetPickRay,

  Scene_SetOnRenderShadowCasters,
  Scene_SetOnRenderViewport,
  Scene_SetOnRenderSprites,
  Scene_SetOnRenderSky,

  Debug_DrawBoundingBox,
  Debug_DrawFrustum,
  Debug_DrawRay,
  Debug_DrawPlane,

  NetClient_SendMessage,
  NetClient_Connect,
  NetClient_Disconnect,
  NetClient_IsConnected,
  NetClient_AddHandler,
  NetClient_GetLocalId,
  NetClient_SetOnConnectionStart,
  NetClient_SetOnConnectionEnd,

  Stream_Create,
  Stream_Open,
  Stream_CreateBuffer,
  Stream_Destroy,
  Stream_Read,
  Stream_Write,
  Stream_Seek,
  Stream_GetSize,
  Stream_GetPosition,
  Stream_ReadLine,
  Stream_EOF,

  Camera_Create,
  Camera_Destroy,
  Camera_Move,
  Camera_Rotate,
  Camera_Fly,
  Camera_SetPosition,
  Camera_SetDirection,
  Camera_LookAt,
  Camera_GetPosition,
  Camera_GetDirection,
  Camera_GetRight,
  Camera_GetUp,
  Camera_SetFarPlane,
  Camera_SetNearPlane,
  Camera_GetProjection,
  Camera_GetTransform,

  {$IFDEF MOBILE}
  TERRA_LoadLibrary,
  {$ENDIF}
  TERRA_Init,
  TERRA_Run,
  TERRA_Shutdown,
  TERRA_GetTime,
  TERRA_GetWidth,
  TERRA_GetHeight,
  TERRA_Log,
  TERRA_Pause,
  TERRA_Paused,
  TERRA_EnableAds,
  TERRA_SendAnalytics,
  TERRA_Resize,

  TERRA_SetOnCreate,
  TERRA_SetOnDestroy,
  TERRA_SetOnIdle,
  TERRA_SetOnError,
  TERRA_SetOnMouseDown,
  TERRA_SetOnMouseUp,
  TERRA_SetOnMouseMove,
  TERRA_SetOnMouseWheel,
  TERRA_SetOnKeyDown,
  TERRA_SetOnKeyUp,
  TERRA_SetOnAccelerometer,
  TERRA_SetOnStateChange,

  Localization_GetLanguage,
  Localization_SetLanguage,
  Localization_GetString,

{  Leaderboard_Select,
  Leaderboard_Show,
  Leaderboard_Submit,}

  Input_SetKeyState,
  Input_KeyDown,
  Input_KeyPressed,
  Input_GetMousePosition,

  AssetManager_AddPath,
  AssetManager_RemovePath,
  AssetManager_Search,
  AssetManager_Prefetch,
  AssetManager_Unload,
  AssetManager_GetTexture,
  AssetManager_GetSound,
  AssetManager_GetMesh,
  AssetManager_GetFont,

  UI_SetColor,
  UI_SetFocus,
  UI_Clear,
  UI_LoadCursor,
  UI_GetDefaultFont,
  UI_SetDefaultFont,
  UI_Create,
  UI_Destroy,
  UI_GetWidth,
  UI_GetHeight,
  UI_CreateWindow,
  UI_CreateButton,
  UI_CreateEditText,
  UI_CreateIcon,
  UI_CreateLabel,
  UI_CreateCheckbox,
  UI_CreateRadioButton,
  UI_MessageBox,
  UI_GetLastWidget,
  UI_GetWidget,
  UI_DeleteWidget,
  UI_HasTransition,
  UI_CreateSlideTransition,
  UI_CreateFadeTransition,
  UI_SetTransitionCallback,

  Widget_SetDragging,
  Widget_IsChecked,
  Widget_SetCaption,
  Widget_SetText,
  Widget_SetColor,
  Widget_SetPosition,
  Widget_SetVisible,
  Widget_SetOnClick,
  Widget_SetOnRelease,
  Widget_SetOnMouseOver,
  Widget_SetOnMouseOut,
  Widget_SetChecked,
  Widget_SetGroup,
  Widget_ToggleVisibility,
  Widget_Show,
  Widget_Hide,
  Widget_AddTween,
  Widget_SetAlign,
  Widget_CenterOnPoint,
  Widget_HasTweens,
  Widget_IsVisible,
  Widget_GetSize,
  Widget_GetPosition,
  Widget_GetColor,
  Widget_GetName,
  Widget_GetCaption,
  Widget_GetText,
  Widget_GetFont,
  Widget_SetScale,

  Image_Create,
  Image_Load,
  Image_Destroy,
  Image_GetPixels,
  Image_GetPixel,
  Image_SetPixel,
  Image_GetWidth,
  Image_GetHeight,

  Skybox_Create,
  Skybox_Render,
  Skybox_Destroy,

  MeshInstance_Create,
  MeshInstance_SetPosition,
  MeshInstance_SetRotation,
  MeshInstance_SetScale,
  MeshInstance_PlayAnimation,
  MeshInstance_CrossfadeAnimation,
  MeshInstance_Destroy,
  MeshInstance_GetBoundingBox,
  MeshInstance_GetGroup,
  MeshInstance_SetGroupLocalTransform,
  MeshInstance_SetGroupGlobalTransform,
  MeshInstance_Intersect,
  MeshInstance_SetDiffuseColor,
  MeshInstance_SetDiffuseMap,
  MeshInstance_SetVisibility,

  MeshGroup_GetBoundingBox,
  MeshGroup_GetIndex,
  
  Occluder_Create,
  Occluder_SetTransform,
  Occluder_PointOccluded,
  Occluder_BoxOccluded,
  Occluder_Destroy,

  Sprite_SetPosition,
  Sprite_GetWidth,
  Sprite_GetHeight,
  Sprite_SetWidth,
  Sprite_SetHeight,
  Sprite_SetAnchor,
  Sprite_SetMirror,
  Sprite_SetFlip,
  Sprite_TileRemap,
  Sprite_TileRemapByID,
  Sprite_PixelRemap,
  Sprite_SetTransform,
  Sprite_SetTransformOnCenter,
  Sprite_SetScaleOnCenter,
  Sprite_SetScale,
  Sprite_SetUniformScale,
  Sprite_SetScaleAndRotationOnCenter,
  Sprite_SetUniformScaleAndRotationOnCenter,
  Sprite_SetRotation,
  Sprite_SetScaleAndRotation,
  Sprite_SetUniformScaleAndRotation,
  Sprite_SetScaleRelativeOnCenter,
  Sprite_SetUniformScaleRelativeOnCenter,
  Sprite_SetScaleAndRotationRelativeOnCenter,
  Sprite_SetUniformScaleAndRotationRelativeOnCenter,
  Sprite_SetTransformRelative,
  Sprite_SetColors,
  Sprite_SetColor,
  Sprite_SetAlpha,
  Sprite_SetScroll,

  TileMap_Create,
  TileMap_Destroy,
  TileMap_Draw,
  TileMap_GetPosition,
  TileMap_SetPosition,

  TileMap_GetObjectCount,
  TileMap_GetObjectPosition,
  TileMap_GetObjectSize,
  TileMap_GetObjectTile,
  TileMap_GetObjectProperty,
  TileMap_GetTileProperty,
  TileMap_GetTileAt,

  Music_Play,
  Music_Stop,
  Music_SetVolume,

  Sound_Play,

  Texture_Create,
  Texture_Bind,
  Texture_UpdateRect,
  Texture_GetImage,
  Texture_GetWidth,
  Texture_GetHeight,
  Texture_SetWrapMode,
  Texture_SetMipMapping,
  Texture_SetFiltering,

  {TileSheet_Create,
  TileSheet_AddFile,
  TileSheet_SetTilesPerRow,
  TileSheet_Generate,
  TileSheet_Destroy,}

  Shader_GetFromFactory,
  Shader_Bind,
  Shader_SetDefaultCameraUniforms,
  Shader_GetAttribute,
  Shader_SetFloat,
  Shader_SetVector,
  Shader_SetColor,
  Shader_SetMatrix,

  Font_DrawText,
  Font_GetTextWidth,
  Font_GetTextHeight,
  Font_GetTextRect,

  AI_PathGetSize,
  AI_PathGetNextNode,
  AI_NodeGetCoords,
  AI_CreatePath,
  AI_DestroyPath,

  XML_Open,
  XML_GetRoot,
  XML_GetNode,
  XML_GetNodeByIndex,
  XML_GetNodeCount,
  XML_GetNodeName,
  XML_GetNodeValue,
  XML_Destroy,

  Session_Save,
  Session_SetValue,
  Session_GetValue,

  Decal_Add,

  Billboard_Add,
  Billboard_Remap,

  Particles_Spawn,
  Particles_Release,
  Particles_Destroy,

  Tween_Create,
  Tween_SetTime,
  Tween_SetEaseType,
  Tween_SetDelay,
  Tween_SetUserData,
  Tween_SetCallback;

Begin
End.
