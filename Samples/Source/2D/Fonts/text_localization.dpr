{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Utils, TERRA_Application, TERRA_Scene, TERRA_Client, TERRA_UI, TERRA_GraphicsManager,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_OS, TERRA_FileManager, TERRA_Texture,
  TERRA_PNG, TERRA_TTF, TERRA_Viewport, TERRA_SpriteManager, TERRA_Localization;

Type
  // A client is used to process application events
  MyGame = Class(AppClient)
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
  _SelectedLanguage:Integer;

{ Game }
Procedure MyGame.OnCreate;
Begin
  // Add asset folders
  FileManager.Instance.AddPath('assets');

  GraphicsManager.Instance.ActiveViewport.BackgroundColor := ColorRed;

  // Load a font
  _Font := FontManager.Instance.GetFont('droid');

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);

  _SelectedLanguage := 1;
End;

// OnIdle is called once per frame, put your game logic here
Procedure MyGame.OnDestroy;
Begin
  _Scene.Destroy();
End;

Procedure MyGame.OnIdle;
Begin
  If (Application.Instance.Input.Keys.WasPressed(keyEscape)) Then
    Application.Instance.Terminate;

  If (Application.Instance.Input.Keys.WasPressed(keyLeft)) And (_SelectedLanguage>1) Then
  Begin
    Dec(_SelectedLanguage);
    StringManager.Instance.SetLanguage(LanguageList[_SelectedLanguage]);
  End;

  If (Application.Instance.Input.Keys.WasPressed(keyright)) And (_SelectedLanguage<LanguageCount) Then
  Begin
    Inc(_SelectedLanguage);
    StringManager.Instance.SetLanguage(LanguageList[_SelectedLanguage]);
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
    _Font.DrawText(50, 90, 10, ' Language: ' + GetLanguageDescription(LanguageList[_SelectedLanguage]), ColorWhite, 1.0, True);
    _Font.DrawText(100, 160, 10, StringManager.Instance.GetString('score') + ': 1000', ColorYellow, 1.0, True);
    _Font.DrawText(100, 190, 10, StringManager.Instance.GetString('totaltime') + ': 1:23', ColorYellow, 1.0, True);
    _Font.DrawText(100, 230, 10, StringManager.Instance.GetString('coinscollected') + ': 56', ColorYellow, 1.0, True);

  End;
End;

Begin
  // Start the application
  ApplicationStart(MyGame.Create);
End.