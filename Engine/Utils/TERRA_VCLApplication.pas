Unit TERRA_VCLApplication;

{$I terra.inc}
Interface
Uses Classes, Forms, ExtCtrls, Graphics, TERRA_String, TERRA_Utils, TERRA_Application,
  TERRA_Object, TERRA_GraphicsManager, TERRA_Viewport, TERRA_Image, TERRA_Color, TERRA_OS, TERRA_Renderer;

Type
  VCLCanvasViewport = Class(TERRAObject)
    Protected
      _Source:TERRAViewport;
      _Target:TCanvas;

      Procedure Update();

    Public
      Constructor Create(Source:TERRAViewport; Target:TCanvas);
      Procedure Release; Override;
  End;

  VCLApplication = Class(Application)
      Protected
        _Timer:TTimer;
        _CurrentWidth:Integer;
        _CurrentHeight:Integer;

        _Target:TComponent;

        _Viewports:Array Of VCLCanvasViewport;
        _ViewportCount:Integer;

        Procedure TimerTick(Sender: TObject);
        Procedure UpdateSize();
        Procedure UpdateViewports();

	  		Function InitWindow:Boolean; Override;
  			Procedure CloseWindow; Override;


      Public
        Constructor Create(Target:TComponent);
        Procedure OnDestroy; Override;

        Function GetWidth:Word; Override;
        Function GetHeight:Word; Override;

        Function GetTitle:TERRAString; Override;

        Procedure AddViewport(V:VCLCanvasViewport);
  End;

Function TERRAColorUnpack(Const C:TERRA_Color.Color):TColor;
Function TERRAColorPack(Const C:TColor):TERRA_Color.Color;

Implementation

Function TERRAColorUnpack(Const C:TERRA_Color.Color):TColor;
Begin
  Result := C.R + C.G Shl 8 + C.B Shl 16;
End;

Function TERRAColorPack(Const C:TColor):TERRA_Color.Color;
Begin
  Result := TERRA_Color.Color(ColorToRGB(C));
  Result.A := 255;
End;

{ VCLApplication }
Constructor VCLApplication.Create(Target:TComponent);
Begin
  _Target := Target;
  _Timer := TTimer.Create(Target);
  _Timer.Interval := 15;
  _Timer.Enabled := True;
  _Timer.OnTimer := TimerTick;

  If (_Target Is TForm) Then
    _Handle := TForm(_Target).Handle
  Else
  If (_Target Is TPanel) Then
    _Handle := TPanel(_Target).Handle
  Else
    _Handle := 0;

  _Managed := True;
  Inherited Create();
End;

Procedure VCLApplication.OnDestroy;
Begin
  Inherited;

  _Timer.Enabled := False;
  _Timer.Free();
End;

Procedure VCLApplication.TimerTick(Sender: TObject);
Begin
  Self.UpdateSize();
  Application.Instance.Run();
  Self.UpdateViewports();
End;

Procedure VCLApplication.UpdateSize;
Begin
  If Not Self.CanReceiveEvents Then
    Exit;

  If (_CurrentWidth<>Self.GetWidth()) Or (_CurrentHeight<>Self.GetHeight()) Then
  Begin
    _CurrentWidth := GetWidth();
    _CurrentHeight := GetHeight();
    Application.Instance.AddRectEvent(eventWindowResize, _CurrentWidth, _CurrentHeight, 0, 0);
  End;
End;

Procedure VCLApplication.AddViewport(V: VCLCanvasViewport);
Begin
  Inc(_ViewportCount);
  SetLength(_Viewports, _ViewportCount);
  _Viewports[Pred(_ViewportCount)] := V;
End;

Procedure VCLApplication.UpdateViewports;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ViewportCount) Do
    _Viewports[I].Update();
End;

Function VCLApplication.GetTitle: TERRAString;
Begin
  If (_Target Is TForm) Then
    Result := TForm(_Target).Caption
  Else
  If (_Target Is TPanel) Then
    Result := TPanel(_Target).Caption
  Else
    Result := '';
End;

Function VCLApplication.GetWidth: Word;
Begin
  If (_Target Is TForm) Then
    Result := TForm(_Target).ClientWidth
  Else
  If (_Target Is TPanel) Then
    Result := TPanel(_Target).Width
  Else
    Result := 0;
End;

Function VCLApplication.GetHeight: Word;
Begin
  If (_Target Is TForm) Then
    Result := TForm(_Target).ClientHeight
  Else
  If (_Target Is TPanel) Then
    Result := TPanel(_Target).Height
  Else
    Result := 0;
End;

Function VCLApplication.InitWindow: Boolean;
Begin
  Result := True;
End;

Procedure VCLApplication.CloseWindow;
Begin
  // do nothing
End;

{ VCLCanvasViewport }
Constructor VCLCanvasViewport.Create(Source:TERRAViewport; Target: TCanvas);
Begin
  Self._Source := Source;
  Self._Target := Target;
End;

Procedure VCLCanvasViewport.Release;
Begin

End;


// this is slow!!!! just experimental test
Procedure VCLCanvasViewport.Update;
Var
  Temp:Image;
  I, J:Integer;
  C:Color;
Begin
  Temp := Self._Source.GetRenderTarget(captureTargetColor).GetImage();

  _Target.Lock();
  For I:=0 To Pred(Temp.Width) Do
    For J:=0 To Pred(Temp.Height) Do
    Begin
      C := Temp.GetPixel(I, J);
      _Target.Pixels[I,J] := TERRAColorUnpack(C);
    End;
  _Target.Unlock();

  ReleaseObject(Temp);
End;

End.
