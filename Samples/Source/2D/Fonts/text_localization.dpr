{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_Utils, TERRA_Object, TERRA_Application,  TERRA_GraphicsManager,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_OS, TERRA_FileManager, TERRA_Texture,
  TERRA_PNG, TERRA_TTF, TERRA_Viewport, TERRA_InputManager,
  TERRA_Engine, TERRA_Renderer, TERRA_Sprite,
  TERRA_Vector2D,
  TERRA_FontRenderer, TERRA_Localization, TERRA_DemoApplication;

Type
  // A client is used to process application events
  Demo = Class(DemoApplication)
    Protected
      _SelectedLanguage:Integer;

    Public
			Procedure OnCreate; Override;
			Procedure OnRender2D(V:TERRAViewport); Override;
      Procedure OnIdle; Override;
  End;

Const
  LanguageCount = 8;
  LanguageList: Array[1..LanguageCount] Of AnsiString = ('en', 'de', 'fr', 'es', 'pt', 'it', 'ru', 'zh');

{ Game }
Procedure Demo.OnCreate;
Begin
  Inherited;
  
  // enable 2D rendering
  Self.GUI.Viewport.Visible := True;

  _SelectedLanguage := 1;
End;

// handle input
Procedure Demo.OnIdle;
Begin
  Inherited;

  If (Engine.Input.Keys.WasPressed(keyLeft)) And (_SelectedLanguage>1) Then
  Begin
    Dec(_SelectedLanguage);
    Engine.Localization.SetLanguage(LanguageList[_SelectedLanguage]);
  End;

  If (Engine.Input.Keys.WasPressed(keyright)) And (_SelectedLanguage<LanguageCount) Then
  Begin
    Inc(_SelectedLanguage);
    Engine.Localization.SetLanguage(LanguageList[_SelectedLanguage]);
  End;
End;

Procedure Demo.OnRender2D(V:TERRAViewport);
Var
  I:Integer;
  Saturation:Single;
  S:TERRASprite;
  Tex:TERRATexture;
Begin
  For I:=1 To LanguageCount Do
  Begin
    If I = _SelectedLanguage Then
      Saturation := 1.0
    Else
      Saturation := 0.0;

    Tex := Engine.Textures['flag_'+LanguageList[I]];

    S := V.SpriteRenderer.FetchSprite();
    S.Translate(10 + I * 70, 20);
    S.Layer := 10;
    S.SetTexture(Tex);
    S.Saturation := Saturation;

    S.AddQuad(spriteAnchor_TopLeft, Vector2D_Create(0, 0), 0.0, Tex.Width, Tex.Height);

    V.SpriteRenderer.QueueSprite(S);
  End;

  // render some text
  Self.FontRenderer.DrawText(V, 50, 100, 10, ' Language: ' + GetLanguageDescription(LanguageList[_SelectedLanguage]));
  Self.FontRenderer.DrawText(V, 100, 160, 10, Engine.Localization['score'] + ': 1000');
  Self.FontRenderer.DrawText(V, 100, 190, 10, Engine.Localization['totaltime'] + ': 1:23');
  Self.FontRenderer.DrawText(V, 100, 230, 10, Engine.Localization['coinscollected'] + ': 56');

  Inherited;
End;

Begin
  // Start the application
  Demo.Create();
End.