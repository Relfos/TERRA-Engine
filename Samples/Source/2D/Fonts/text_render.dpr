{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} BasicSample;

Uses
  {$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_String, TERRA_Object, TERRA_Utils, TERRA_Application, 
  TERRA_ResourceManager, TERRA_Color, TERRA_OS,
  TERRA_Engine, TERRA_ScreenFX,       
  TERRA_Viewport, TERRA_Localization, TERRA_DemoApplication;
                        
Type
  // A client is used to process application events
  SampleApp = Class(DemoApplication)
    Public
      Procedure OnCreate(); Override;
      Procedure OnRender2D(V:TERRAViewport); Override;
  End;

Procedure SampleApp.OnCreate();
Begin
  Inherited;

  Self.GUI.Viewport.Visible := True;
  Self.GUI.Viewport.FXChain.AddEffect(GlowFX.Create());
End;

{ SampleApp }
Procedure SampleApp.OnRender2D(V:TERRAViewport);
Var
  S:TERRAString;
  T:Single;
Begin
  Inherited;

  Self.FontRenderer.Reset();

  Self.FontRenderer.SetSize(60.0);
  Self.FontRenderer.SetOutline(ColorBlue);
  Self.FontRenderer.DrawText(V, 50, 80, 10, ' Hello World!');

  Self.FontRenderer.Reset();
  Self.FontRenderer.SetSize(100.0);
  Self.FontRenderer.SetOutline(ColorNull);
  Self.FontRenderer.SetOutline(ColorCreateFromString('#EF8C00'));
  Self.FontRenderer.SetPattern(Engine.Textures['cheese_diffuse']);
  Self.FontRenderer.DrawText(V, -20, 150, 10, ' Textured text, oh!');

  // change text size
  Self.FontRenderer.Reset();
  Self.FontRenderer.SetSize(30.0);

  // unicode rendering
  Self.FontRenderer.SetColor(ColorWhite);
  Self.FontRenderer.DrawText(V, 50, 200, 10, GetLanguageDescription(language_Russian));
  Self.FontRenderer.DrawText(V, 50, 240, 10, GetLanguageDescription(language_Chinese));

  Self.FontRenderer.DrawText(V, 50, 280, 10, GetLanguageDescription(language_Korean));
  Self.FontRenderer.DrawText(V, 50, 320, 10, GetLanguageDescription(language_Japanese));

  // bbcode text
  Self.FontRenderer.SetColor(ColorYellow);
  Self.FontRenderer.DrawText(V, 500, 200, 10, 'This is a' + CrLf + 'line break!');

  Self.FontRenderer.SetColor(ColorCreate(128, 128, 255));
  Self.FontRenderer.DrawText(V, 680, 200, 10, 'You can have [w]wavy[/w] text!');

  Self.FontRenderer.SetColor(ColorGreen);
  Self.FontRenderer.DrawText(V, 500, 300, 10, 'Text can be stylized [i]with italics[/i] and [b]bold[/b]!');

  Self.FontRenderer.SetColor(ColorRed);
  Self.FontRenderer.DrawText(V, 500, 400, 10, 'This is a ghost[w][img]ghost[/img][/w], haha!');

  Self.FontRenderer.DrawText(V, 500, 500, 10, 'This is bigger ghost[img=64x64]ghost[/img], hehehe!');

  Self.FontRenderer.SetColor(ColorWhite);
  Self.FontRenderer.DrawText(V, 50, 550, 10, 'Can also [u]underline text[/u] and [s]strike[/s] through!');

  // glowing text
  T := Abs(Sin(Application.GetTime()/5000)); // just get a pulsating animated value from 0 to 1
  Self.FontRenderer.SetColor(ColorWhite);
  Self.FontRenderer.SetGlow(ColorCreate(0, Trunc(255 * T), 0));
  Self.FontRenderer.DrawText(V, 50, 600, 10, 'Glowing text!');

  // dynamic text
  Self.FontRenderer.Reset();
  Self.FontRenderer.DrawText(V, V.Width - 250, 50, 10, 'Seconds ellapsed: '+CardinalToString(Application.GetTime() Div 1000));

  // limiting chars (useful for animated game messages)
  S := 'This message is animated via SetCharLimit()';
  Self.FontRenderer.SetColor(ColorWhite);
  Self.FontRenderer.SetCharLimit((Application.GetTime() Div 100) Mod (Length(S)+1));
  Self.FontRenderer.DrawText(V, 50, 400, 10, S);

  // gradients
  Self.FontRenderer.Reset();
  Self.FontRenderer.SetHorizontalGradient(ColorWhite, ColorNull);
  Self.FontRenderer.DrawText(V, 50, 450, 10, 'This uses a gradient to fade out...');

  Self.FontRenderer.Reset();
  Self.FontRenderer.SetVerticalGradient(ColorRed, ColorBlue);
  Self.FontRenderer.DrawText(V, 50, 500, 10, 'This uses a vertical gradient..');
End;

Begin
  // Start the application
  SampleApp.Create();
End.