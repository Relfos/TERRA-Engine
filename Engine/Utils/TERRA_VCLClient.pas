Unit TERRA_VCLClient;

{$I terra.inc}
Interface
Uses Classes, Forms, ExtCtrls, Graphics, TERRA_String, TERRA_Utils, TERRA_Application,
  TERRA_Client, TERRA_Viewport, TERRA_RenderTarget, TERRA_Image, TERRA_Color;

Type
  VCLCanvasViewport = Class(TERRAObject)
    Protected
      _Source:Viewport;
      _Target:TCanvas;

      Procedure Update();

    Public
      Constructor Create(Source:Viewport; Target:TCanvas);
      Procedure Release; Override;
  End;

  VCLClient = Class(AppClient)
      Protected
        _Timer:TTimer;
        _CurrentWidth:Integer;
        _CurrentHeight:Integer;

        _Viewports:Array Of VCLCanvasViewport;
        _ViewportCount:Integer;

        Procedure TimerTick(Sender: TObject);
        Procedure UpdateSize();
        Procedure UpdateViewports();

      Public
        Constructor Create(Target:TComponent);
        Procedure Release; Override;

        Procedure AddViewport(V:VCLCanvasViewport);
  End;

  VCLWindowClient = Class(VCLClient)
      Protected
        _Target:TForm;

      Public
        Constructor Create(Target:TForm);

        Function GetWidth:Word; Override;
        Function GetHeight:Word; Override;

        Function GetTitle:TERRAString; Override;
        Function GetHandle:Cardinal; Override;
  End;

  VCLPanelClient = Class(VCLClient)
      Protected
        _Target:TPanel;

      Public
        Constructor Create(Target:TPanel);

        Function GetWidth:Word; Override;
        Function GetHeight:Word; Override;

        Function GetHandle:Cardinal; Override;
  End;

Implementation

{ VCLClient }
Constructor VCLClient.Create(Target:TComponent);
Begin
  _Timer := TTimer.Create(Target);
  _Timer.Interval := 15;
  _Timer.Enabled := True;
  _Timer.OnTimer := TimerTick;
End;

Procedure VCLClient.Release;
Begin
  _Timer.Enabled := False;
  _Timer.Free();

  Inherited;
End;

Procedure VCLClient.TimerTick(Sender: TObject);
Begin
  Self.UpdateSize();
  Application.Instance.Run();
  Self.UpdateViewports();
End;

Procedure VCLClient.UpdateSize;
Begin
  If (_CurrentWidth<>Self.GetWidth()) Or (_CurrentHeight<>Self.GetHeight()) Then
  Begin
    _CurrentWidth := GetWidth();
    _CurrentHeight := GetHeight();
    Application.Instance.AddRectEvent(eventWindowResize, _CurrentWidth, _CurrentHeight, 0, 0);
  End;
End;

Procedure VCLClient.AddViewport(V: VCLCanvasViewport);
Begin
  Inc(_ViewportCount);
  SetLength(_Viewports, _ViewportCount);
  _Viewports[Pred(_ViewportCount)] := V;
End;

Procedure VCLClient.UpdateViewports;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ViewportCount) Do
    _Viewports[I].Update();
End;

{ VCLWindowClient }
Constructor VCLWindowClient.Create(Target: TForm);
Begin
  _Target := Target;
  Inherited Create(Target);
End;

Function VCLWindowClient.GetHandle: Cardinal;
Begin
  Result := _Target.Handle;
End;

Function VCLWindowClient.GetTitle: TERRAString;
Begin
  Result := _Target.Caption;
End;

Function VCLWindowClient.GetHeight: Word;
Begin
  Result := _Target.ClientHeight;
End;

Function VCLWindowClient.GetWidth: Word;
Begin
  Result := _Target.ClientWidth;
End;


{ VCLPanelClient }
Constructor VCLPanelClient.Create(Target: TPanel);
Begin
  _Target := Target;
  Inherited Create(Target);
End;

Function VCLPanelClient.GetHandle: Cardinal;
Begin
  Result := _Target.Handle;
End;

Function VCLPanelClient.GetWidth: Word;
Begin
  Result := _Target.Width;
End;

Function VCLPanelClient.GetHeight: Word;
Begin
  Result := _Target.Height;
End;


{ VCLCanvasViewport }
Constructor VCLCanvasViewport.Create(Source:Viewport; Target: TCanvas);
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
      _Target.Pixels[I,J] := C.R + C.G Shl 8 + C.B Shl 16;
    End;
  _Target.Unlock();

  ReleaseObject(Temp);
End;

End.
