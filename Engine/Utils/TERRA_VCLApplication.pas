Unit TERRA_VCLApplication;

{$I terra.inc}
Interface
Uses Classes, Forms, ExtCtrls, Graphics, TERRA_String, TERRA_Utils, TERRA_Application,
  TERRA_Object, TERRA_GraphicsManager, TERRA_Viewport, TERRA_Image, TERRA_Color, TERRA_OS, TERRA_Renderer,
  TERRA_UIView, TERRA_UIDimension;

Type
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

  VCLApplication = Class(Application)
      Protected
        _Timer:TTimer;
        _CurrentWidth:Integer;
        _CurrentHeight:Integer;

        _Target:TComponent;

        _Viewports:Array Of TERRAVCLViewport;
        _ViewportCount:Integer;

        _GUI:UIView;
        _Scene:TERRAScene;

        Procedure TimerTick(Sender: TObject);
        Procedure UpdateSize();
        Procedure UpdateViewports();

	  		Function InitWindow:Boolean; Override;
  			Procedure CloseWindow; Override;

      Public
        OnRender:VLCRenderEvent;

        Constructor Create(Target:TComponent);
        Procedure OnDestroy; Override;

        Function GetWidth:Word; Override;
        Function GetHeight:Word; Override;

        Function GetTitle:TERRAString; Override;

        Procedure AddRenderTarget(V:TERRAVCLViewport);
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

    If (_GUI = Nil) Then
    Begin
      _GUI := UIView.Create('gui', UIPixels(GetWidth()), UIPixels(GetHeight()));
      _GUI.Viewport.AutoResize := True;
      _GUI.Viewport.OnRender := Self.Render2D;
    End;

  End;
End;

Procedure VCLApplication.AddRenderTarget(V:TERRAVCLViewport);
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

{ TERRAVCLViewport }
Constructor TERRAVCLViewport.Create(Target:TImage);
Begin
  Self._Target := Target;
  _GUI := UIView.Create('gui', UIPixels(Target.ClientWidth), UIPixels(Target.ClientHeight));

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
  Temp:Image;
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

End.
