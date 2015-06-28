{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Utils, TERRA_Object, TERRA_Application, TERRA_Scene, TERRA_UI, TERRA_GraphicsManager,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_OS, TERRA_FileManager, TERRA_Texture,
  TERRA_PNG, TERRA_TTF, TERRA_Viewport, TERRA_SpriteManager, TERRA_InputManager,
  TERRA_FontRenderer, TERRA_Localization, TERRA_Renderer;

Type
  // A client is used to process application events
  Demo = Class(Application)
    Protected
      _Scene:Scene;

			Procedure OnCreate; Override;
      Procedure OnDestroy; Override;
			Procedure OnIdle; Override;
  End;

  // A scene is used to render objects
  MyScene = Class(Scene)
      Procedure RenderSprites(V:Viewport); Override;
  End;

Const
  LanguageCount = 8;
  LanguageList: Array[1..LanguageCount] Of AnsiString = ('en', 'de', 'fr', 'es', 'pt', 'it', 'ru', 'zh');

Var
  _Font:Font = Nil;
  _FontRenderer:FontRenderer;
  _SelectedLanguage:Integer;

{ Game }
Procedure Demo.OnCreate;
Begin
  // Add asset folders
  FileManager.Instance.AddPath('assets');

  GraphicsManager.Instance.DeviceViewport.BackgroundColor := ColorRed;

  // Load a font
  _Font := FontManager.Instance.GetFont('droid');

  _FontRenderer := FontRenderer.Create();
  _FontRenderer.SetFont(_Font);

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  _SelectedLanguage := 1;
End;

// OnIdle is called once per frame, put your game logic here
Procedure Demo.OnDestroy;
Begin
  ReleaseObject(_FontRenderer);
  ReleaseObject(_Scene);
End;

Procedure Demo.OnIdle;
Begin
  If (InputManager.Instance.Keys.WasPressed(keyEscape)) Then
    Application.Instance.Terminate;
    
  If (InputManager.Instance.Keys.WasPressed(keyLeft)) And (_SelectedLanguage>1) Then
  Begin
    Dec(_SelectedLanguage);
    LocalizationManager.Instance.SetLanguage(LanguageList[_SelectedLanguage]);
  End;

  If (InputManager.Instance.Keys.WasPressed(keyright)) And (_SelectedLanguage<LanguageCount) Then
  Begin
    Inc(_SelectedLanguage);
    LocalizationManager.Instance.SetLanguage(LanguageList[_SelectedLanguage]);
  End;
End;

{ MyScene }
Procedure MyScene.RenderSprites(V:Viewport);
Var
  I:Integer;
  Saturation:Single;
Begin
  For I:=1 To LanguageCount Do
  Begin
    If I = _SelectedLanguage Then
      Saturation := 1.0
    Else
      Saturation := 0.0;

    SpriteManager.Instance.DrawSprite(10 + I * 70, 10, 10, TextureManager.Instance.GetTexture('flag_'+LanguageList[I]), Nil, blendBlend, Saturation);
  End;

  // render some text
  If Assigned(_Font) Then
  Begin
    _FontRenderer.DrawText(50, 90, 10, ' Language: ' + GetLanguageDescription(LanguageList[_SelectedLanguage]));
    _FontRenderer.DrawText(100, 160, 10, LocalizationManager.Instance['score'] + ': 1000');
    _FontRenderer.DrawText(100, 190, 10, LocalizationManager.Instance['totaltime'] + ': 1:23');
    _FontRenderer.DrawText(100, 230, 10, LocalizationManager.Instance['coinscollected'] + ': 56');

  End;
End;

Begin
  // Start the application
  Demo.Create();
End.