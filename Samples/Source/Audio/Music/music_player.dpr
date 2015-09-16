{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses TERRA_Object, TERRA_DemoApplication, TERRA_Utils, TERRA_OS, TERRA_Viewport, TERRA_FileUtils,
  TERRA_Engine, TERRA_MusicManager;

Type
  Demo = Class(DemoApplication)
    Public
			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;
			Procedure OnIdle; Override;

      Procedure OnRender2D(View:TERRAViewport); Override;
  End;

{ Demo }
Procedure Demo.OnCreate;
Begin
  Inherited;

  // Enable 2d rendering viewport
  Self.GUI.Visible := True; 

  //Engine.Music.Play('mar');
  Engine.Music.Play('nox');
End;

Procedure Demo.OnDestroy;
Begin
  Engine.Music.Stop();

  Inherited;
End;

Procedure Demo.OnIdle;
Begin
  Inherited;

  If (Engine.Input.Keys.WasPressed(keyLeft)) Then
    Engine.Music.SetVolume(Engine.Music.Volume - 0.1)
  Else
  If (Engine.Input.Keys.WasPressed(keyRight)) Then
    Engine.Music.SetVolume(Engine.Music.Volume + 0.1);
End;


Procedure Demo.OnRender2D(View:TERRAViewport);
Begin
  Inherited;
  
  If Not Assigned(Self.Font) Then
    Exit;

  Self.FontRenderer.DrawText(View, 5, 60, 5, 'Volume: '+IntegerProperty.Stringify(Trunc(Engine.Music.Volume * 100))+'%');

  If Assigned(Engine.Music.CurrentTrack) Then
  Begin
    Self.FontRenderer.DrawText(View, 5, 80, 5, 'Title: '+ GetFileName(Engine.Music.CurrentTrack.FileName, False));
    Self.FontRenderer.DrawText(View, 5, 100, 5, 'Type: '+ Engine.Music.CurrentTrack.ClassName);
  End;
End;

{$IFDEF IPHONE}
Procedure StartDemo; cdecl; export;
{$ENDIF}
Begin
  Demo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}
End.

