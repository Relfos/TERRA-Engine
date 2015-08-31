{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Application, TERRA_GraphicsManager,
  TERRA_ResourceManager, TERRA_Color, TERRA_Font, TERRA_FontRenderer, TERRA_OS, TERRA_FileManager,
  TERRA_EngineManager,
  TERRA_PNG, TERRA_TTF, TERRA_Viewport, TERRA_Localization, TERRA_Sprite,
  TERRA_InputManager, TERRA_DemoApplication;

Type
  // A client is used to process application events
  SampleApp = Class(DemoApplication)
    Protected
      Procedure OnCreate(); Override;
      Procedure OnRender2D(V:TERRAViewport); Override;
  End;

// function to translate Unicode strings to TERRA strings
Function U2T(Const S:WideString):AnsiString;
Var
  I:Integer;
  W:WideChar;
Begin
  Result := '';
  For I:=1 To Length(S) Do
  Begin
    W := S[I];
    Result := Result + StringFromChar(TERRAChar(W));
  End;
End;

Procedure SampleApp.OnCreate();
Begin
  Inherited;

  Self.GUI.Viewport.Visible := True;
End;

{ SampleApp }
Procedure SampleApp.OnRender2D(V:TERRAViewport);
Begin
  Inherited;

  Self.FontRenderer.SetSize(60.0);
  Self.FontRenderer.DrawText(V, 50, 70, 10, ' Hello World!');

  // restore size
  Self.FontRenderer.SetSize(30.0);

  // unicode rendering
  Self.FontRenderer.SetColor(ColorWhite);
  Self.FontRenderer.DrawText(V, 50, 200, 10, GetLanguageDescription(language_Russian));
  Self.FontRenderer.DrawText(V, 50, 250, 10, GetLanguageDescription(language_Chinese));

  Self.FontRenderer.DrawText(V, 50, 300, 10, GetLanguageDescription(language_Korean));
  Self.FontRenderer.DrawText(V, 50, 350, 10, GetLanguageDescription(language_Japanese));

  // bbcode text
  Self.FontRenderer.SetColor(ColorCreate(128, 128, 255));
  Self.FontRenderer.DrawText(V, 500, 100, 10, '[w]Wavy text![/w]');

  Self.FontRenderer.SetColor(ColorYellow);
  Self.FontRenderer.DrawText(V, 550, 200, 10, 'This is a' + CrLf + 'line break!');

  Self.FontRenderer.SetColor(ColorGreen);
  Self.FontRenderer.DrawText(V, 600, 300, 10, '[i]Italic text![/i]');

  // dynamic text
  Self.FontRenderer.DrawText(V, V.Width - 100, 50, 10, CardinalToString(Application.GetTime() Div 1000));
End;

Begin
  // Start the application
  SampleApp.Create();
End.