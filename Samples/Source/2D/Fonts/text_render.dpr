{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Utils, TERRA_Application, TERRA_Scene, TERRA_Client, TERRA_UI, TERRA_GraphicsManager,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_OS, TERRA_FileManager, TERRA_Unicode,
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

Var
  _Font:Font = Nil;

{ Game }
Procedure MyGame.OnCreate;
Begin
  // Add asset folders
  FileManager.Instance.AddPath('assets');

  GraphicsManager.Instance.BackgroundColor := ColorRed;

  // Load a font
  _Font := FontManager.Instance.GetFont('droid');

  // Create a scene and set it as the current scene
  _Scene := MyScene.Create;
  GraphicsManager.Instance.SetScene(_Scene);
End;

// OnIdle is called once per frame, put your game logic here
Procedure MyGame.OnDestroy;
Begin
  _Scene.Destroy();
End;

Procedure MyGame.OnIdle;
Begin
  If Keys[keyEscape] Then
    Application.Instance.Terminate;
End;

// function to translate Unicode strings to TERRA internal format
Function U2T(Const S:WideString):AnsiString;
Var
  I:Integer;
  W:WideChar;
Begin
  Result := '';
  For I:=1 To Length(S) Do
    Result := Result + UnicodeToUCS2(Word(W));
End;

{ MyScene }
Procedure MyScene.RenderSprites(V:Viewport);
Begin
  // render some text
  If Assigned(_Font) Then
  Begin
    _Font.DrawText(50, 70, 10, ' Hello World!', ColorWhite, 1.0, True);
    _Font.DrawText(200, 160, 10, ' This is a\nline break!', ColorYellow, 1.0, True);

    _Font.DrawText(200, 100, 10, ' \wWavy text!', ColorBlue, 1.0, True);

    _Font.DrawText(400, 100, 10, ' \iItalic text!', ColorGreen, 1.0, True);

    // unicode rendering
    _Font.DrawText(50, 200, 10, GetLanguageDescription(language_Russian), ColorWhite, 1.0, True);
    _Font.DrawText(50, 230, 10, GetLanguageDescription(language_Chinese), ColorWhite, 1.0, True);
    _Font.DrawText(50, 260, 10, GetLanguageDescription(language_Korean), ColorWhite, 1.0, True);
    _Font.DrawText(50, 290, 10, GetLanguageDescription(language_Japanese), ColorWhite, 1.0, True);
  End;
End;

Begin
  // Start the application
  ApplicationStart(MyGame.Create);
End.