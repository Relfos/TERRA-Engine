Unit TERRA_VCLApplication;

{$I terra.inc}
Interface
Uses Classes, Forms, ExtCtrls, Graphics, TERRA_String, TERRA_Utils, TERRA_Application, TERRA_Vector2D,
  TERRA_Object, TERRA_GraphicsManager, TERRA_Viewport, TERRA_Image, TERRA_Color, TERRA_OS, TERRA_Renderer,
  TERRA_Window, TERRA_UIView, TERRA_UIDimension
  {$IFDEF OSX}, MacOSAll, CarbonDef{$ENDIF};

Type
  TERRAVCLWindow = Class(TERRAWindow)
    Protected
      _Target:TComponent;

      Constructor Create(Target:TComponent);
  End;

  TERRAVCLViewport = Class(TERRAObject)
    Protected
      _Target:TImage;

      _GUI:UIView;

      _Dest:TBitmap;
      _BufferA:TBitmap;
      _BufferB:TBitmap;

      Procedure Update();

      Function GetViewport: TERRAViewport;
    Public
      Constructor Create(Target:TImage);
      Procedure Release; Override;

      Property Viewport:TERRAViewport Read GetViewport;
  End;

  { VCLApplication }

  VCLApplication = Class(Application)
      Protected
        _Target:TComponent;

        _Timer:TTimer;
        _CurrentWidth:Integer;
        _CurrentHeight:Integer;

        _Viewports:Array Of TERRAVCLViewport;
        _ViewportCount:Integer;

        _GUI:UIView;

        Procedure TimerTick(Sender: TObject);
        Procedure UpdateSize();
        Procedure UpdateViewports();

	  		Function CreateWindow():TERRAWindow; Override;

        Function GetViewport: TERRAViewport;
        Function GetGUI: UIView;

      Public
        Constructor Create(Target:TComponent);
        Procedure OnDestroy; Override;

        Function GetWidth:Word; Override;
        Function GetHeight:Word; Override;

        Function GetTitle:TERRAString; Override;

        Procedure AddRenderTarget(V:TERRAVCLViewport);

        Property GUI:UIView Read GetGUI;
        Property Viewport:TERRAViewport Read GetViewport;
  End;

Function TERRAColorUnpack(Const C:ColorRGBA):TColor;
Function TERRAColorPack(Const C:TColor):ColorRGBA;

Implementation

Function TERRAColorUnpack(Const C:ColorRGBA):TColor;
Begin
  Result := C.R + C.G Shl 8 + C.B Shl 16;
End;

Function TERRAColorPack(Const C:TColor):ColorRGBA;
Begin
  Result := ColorRGBA(ColorToRGB(C));
  Result.A := 255;
End;

{ VCLApplication }
Constructor VCLApplication.Create(Target: TComponent);
Begin
  _Target := Target;
  _Timer := TTimer.Create(Target);
  _Timer.Interval := 15;
  _Timer.Enabled := True;
  _Timer.OnTimer := TimerTick;

  Inherited Create();
End;

procedure VCLApplication.OnDestroy;
Begin
  Inherited;

  _Timer.Enabled := False;
  _Timer.Free();
End;

procedure VCLApplication.TimerTick(Sender: TObject);
Begin
  Self.UpdateSize();
  Application.Instance.Run();
  Self.UpdateViewports();
End;

procedure VCLApplication.UpdateSize;
Begin
  If Not Self.CanReceiveEvents Then
    Exit;

  If (_CurrentWidth<>Self.GetWidth()) Or (_CurrentHeight<>Self.GetHeight()) Then
  Begin
    _CurrentWidth := GetWidth();
    _CurrentHeight := GetHeight();
    Application.Instance.AddRectEvent(eventWindowResize, _CurrentWidth, _CurrentHeight, 0, 0);
  End;

  If Assigned(_GUI) Then
    _GUI.AutoResize();
End;

procedure VCLApplication.AddRenderTarget(V: TERRAVCLViewport);
Begin
  V.Viewport.AutoResolve := True;
  Inc(_ViewportCount);
  SetLength(_Viewports, _ViewportCount);
  _Viewports[Pred(_ViewportCount)] := V;
End;

procedure VCLApplication.UpdateViewports;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ViewportCount) Do
    _Viewports[I].Update();
End;

function VCLApplication.GetTitle: TERRAString;
Begin
  If (_Target Is TForm) Then
    Result := TForm(_Target).Caption
  Else
  If (_Target Is TPanel) Then
    Result := TPanel(_Target).Caption
  Else
    Result := '';
End;

function VCLApplication.GetWidth: Word;
Begin
  If (_Target Is TForm) Then
    Result := TForm(_Target).ClientWidth
  Else
  If (_Target Is TPanel) Then
    Result := TPanel(_Target).ClientWidth
  Else
    Result := 0;
End;

function VCLApplication.GetHeight: Word;
Begin
  If (_Target Is TForm) Then
    Result := TForm(_Target).ClientHeight
  Else
  If (_Target Is TPanel) Then
    Result := TPanel(_Target).ClientHeight
  Else
    Result := 0;
End;

Function VCLApplication.CreateWindow():TERRAWindow;
Begin
  Result := TERRAVCLWindow.Create(_Target);
End;

function VCLApplication.GetViewport: TERRAViewport;
Begin
  Result := Self.GUI.Viewport;
End;

function VCLApplication.GetGUI: UIView;
Begin
  If (_GUI = Nil) Then
  Begin
    _GUI := UIView.Create('gui', UIPercent(100), UIPercent(100), 0.0);
  End;

  Result := _GUI;
End;

{ TERRAVCLViewport }
Constructor TERRAVCLViewport.Create(Target:TImage);
Begin
  Self._Target := Target;
  _GUI := UIView.Create('gui', UIPixels(Target.ClientWidth), UIPixels(Target.ClientHeight), 0.0);

  _Dest := TBitmap.Create();
  _Dest.Width := _GUI.Viewport.Width;
  _Dest.Height := _GUI.Viewport.Height;
  _Dest.PixelFormat := pf32bit;
End;

Function TERRAVCLViewport.GetViewport: TERRAViewport;
Begin
  If Assigned(_GUI) Then
    Result := _GUI.Viewport
  Else
    Result := Nil;
End;

Procedure TERRAVCLViewport.Release;
Begin
  ReleaseObject(_GUI);
  _Dest.Destroy();
End;


// this is slow!!!! just experimental test
Procedure TERRAVCLViewport.Update;
Var
  Temp:TERRAImage;
  It:ImageIterator;
  I, J:Integer;
  C:ColorRGBA;
  Scanline:PByte;
Begin
  Temp := Self._GUI.Viewport.GetRenderTarget(captureTargetColor).GetImage();

  For J:=0 To Pred(_Dest.Height) Do
  Begin
    Scanline := _Dest.ScanLine[J];

    For I:=0 To Pred(_Dest.Width) Do
    Begin
      C := Temp.GetPixel(I, J);

      Scanline^ := C.B; Inc(Scanline);
      Scanline^ := C.G; Inc(Scanline);
      Scanline^ := C.R; Inc(Scanline);
      Scanline^ := C.A; Inc(Scanline);

      //Dest.Pixels[I,J] := TERRAColorUnpack(C);
    End;
  End;

  ReleaseObject(Temp);

  //_Target.Picture.Bitmap := _Dest;
  _Target.Canvas.Draw(0, 0, _Dest);
End;

{ TERRAVCLWindow }

Constructor TERRAVCLWindow.Create(Target: TComponent);
Begin
  _Target := Target;
  _Managed := True;
  
  {$IFDEF OSX}
  If (_Target Is TForm) Then
    _Handle :=WindowPtr(TCarbonWidget(TForm(_Target).Handle).Widget)
  Else
  If (_Target Is TPanel) Then
   _Handle :=WindowPtr(TCarbonWidget(TPanel(_Target).Handle).Widget)
  {$ELSE}
  If (_Target Is TForm) Then
    _Handle := TForm(_Target).Handle
  Else
  If (_Target Is TPanel) Then
    _Handle := TPanel(_Target).Handle
  {$ENDIF}
  Else
    _Handle := 0;
End;



End.
