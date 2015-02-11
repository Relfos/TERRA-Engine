Unit TERRA_Webcam;

{$I terra.inc}
Interface

{$DEFINE DXErr}
Uses TERRA_Utils, Windows, Messages, SysUtils, ActiveX,
     {$ifdef DXErr} DXErr9, {$endif}
     DirectShow9, TERRA_Texture, TERRA_Color, TERRA_Image, TERRA_Application;

CONST
  CBufferCnt = 3;

TYPE
  TVideoProperty = (VP_Brightness,
                    VP_Contrast,
                    VP_Hue,
                    VP_Saturation,
                    VP_Sharpness,
                    VP_Gamma,
                    VP_ColorEnable,
                    VP_WhiteBalance,
                    VP_BacklightCompensation,
                    VP_Gain);

CONST
  WM_GRAPHNOTIFY = WM_APP+1;
  WM_NewFrame    = WM_User+2;   // Used to inform application that a new video
                                // frame has arrived. Necessary only, if
                                // application hasn't defined a callback
                                // routine via TVideoSample.SetCallBack(...).


CONST  { Copied from OLE2.pas }
  {$EXTERNALSYM IID_IUnknown}
  IID_IUnknown: TGUID = (
    D1:$00000000;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));



TYPE
  TPLAYSTATE      = (PS_Stopped,
                     {PS_Init,}
                     PS_Paused,
                     PS_Running);

TYPE
  TVideoSampleCallBack= procedure(pb : pbytearray; var Size: integer) of object;
  TSampleGrabberCBInt = interface(ISampleGrabberCB)
                          function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
                          function  BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
                        end;
  TSampleGrabberCBImpl= class
                          CallBack    : TVideoSampleCallBack;
                          function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
                          function  BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
                        end;
  TSampleGrabberCB =    class(TInterfacedObject, TSampleGrabberCBInt)
                          FSampleGrabberCB: TSampleGrabberCBImpl;
                          CallBack    : TVideoSampleCallBack;
                          property SampleGrabberCB: TSampleGrabberCBImpl read FSampleGrabberCB implements TSampleGrabberCBInt;
                        end;


  TFormatInfo   = RECORD
                    Width,
                    Height : integer;
                    SSize  : cardinal;
                    OIndex : integer;
                    pmt    : PAMMediaType;
                    FourCC : ARRAY[0..3] Of TERRAChar;
                  END;

  TVideoSample  = class(TObject)
                    private
                      ghApp             : HWND;
                      pIVideoWindow     : IVideoWindow;
                      pIMediaControl    : IMediaControl;
                      pIMediaEventEx    : IMediaEventEx;
                      pIGraphBuilder    : IGraphBuilder;
                      pICapGraphBuild2  : ICaptureGraphBuilder2;
                      g_psCurrent       : TPLAYSTATE;

                      pIAMStreamConfig  : IAMStreamConfig;
                      piBFSampleGrabber : IBaseFilter;
                      pIAMVideoProcAmp  : IAMVideoProcAmp;
                      pIBFNullGraphicsManager  : IBaseFilter;

                      pIKsPropertySet   : IKsPropertySet;
                      pISampleGrabber   : ISampleGrabber;
                      pIBFVideoSource   : IBaseFilter;

                      SGrabberCB  : TSampleGrabberCB;
                      _SGrabberCB : TSampleGrabberCBInt;
                      fVisible    : boolean;
                      CallBack    : TVideoSampleCallBack;
                      FormatArr   : ARRAY OF TFormatInfo;
                      FUNCTION    GetInterfaces(ForceRGB: boolean; WhichMethodToCallback: integer): HRESULT;
                      FUNCTION    SetupVideoWindow(): HRESULT;
                      FUNCTION    ConnectToCaptureDevice(DeviceName:TERRAString; VAR DeviceSelected:TERRAString; VAR ppIBFVideoSource: IBaseFilter): HRESULT;
                      FUNCTION    RestartVideoEx(Visible: boolean):HRESULT;
                      FUNCTION    ShowPropertyDialogEx(const IBF: IUnknown; FilterName:  PWideChar): HResult;
                      FUNCTION    LoadListOfResolution: HResult;
                      procedure   DeleteBelow(const IBF: IBaseFilter);
                      procedure   CloseInterfaces;

                    public
                      {$ifdef DXErr}
                        DXErrString:TERRAString;  // for debugging
                      {$endif}
                      constructor Create(VideoCanvasHandle: THandle; ForceRGB: boolean; WhichMethodToCallback: integer; VAR HR: HResult);
                      destructor  Destroy; override;
                      property    PlayState: TPLAYSTATE read g_psCurrent;
                      procedure   ResizeVideoWindow();
                      FUNCTION    RestartVideo:HRESULT;
                      FUNCTION    StartVideo(CaptureDeviceName:TERRAString; Visible: boolean; VAR DeviceSelected:TERRAString):HRESULT;
                      FUNCTION    PauseVideo: HResult;  // Pause running video
                      FUNCTION    ResumeVideo: HResult; // Re-start paused video
                      FUNCTION    StopVideo: HResult;
                      function    GetImageBuffer(VAR pb : pbytearray; var Size: integer): HResult;
                      FUNCTION    SetPreviewState(nShow: boolean): HRESULT;
                      FUNCTION    ShowPropertyDialog: HResult;
                      FUNCTION    ShowPropertyDialog_CaptureStream: HResult;
                      FUNCTION    GetVideoPropAmpEx(    Prop           : TVideoProcAmpProperty;
                                                    VAR pMin, pMax,
                                                        pSteppingDelta,
                                                        pDefault       : longint;
                                                    VAR pCapsFlags     : TVideoProcAmpFlags;
                                                    VAR pActual        : longint): HResult;
                      FUNCTION    SetVideoPropAmpEx(    Prop           : TVideoProcAmpProperty;
                                                        pCapsFlags     : TVideoProcAmpFlags;
                                                        pActual        : longint): HResult;
                      PROCEDURE   GetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; VAR AcPerCent: integer);
                      PROCEDURE   SetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; AcPerCent: integer);
                      PROCEDURE   GetVideoSize(VAR Width, height: integer);
                      FUNCTION    ShowVfWCaptureDlg: HResult;
                      FUNCTION    GetStreamInfo(VAR Width, Height: integer; VAR FourCC: dword): HResult;
                      FUNCTION    GetExProp(    guidPropSet   : TGuiD;
                                                dwPropID      : TAMPropertyPin;
                                                pInstanceData : pointer;
                                                cbInstanceData: DWORD;
                                            out pPropData;
                                                cbPropData    : DWORD;
                                            out pcbReturned   : DWORD): HResult;
                      FUNCTION    SetExProp(   guidPropSet : TGuiD;
                                                  dwPropID : TAMPropertyPin;
                                            pInstanceData  : pointer;
                                            cbInstanceData : DWORD;
                                                 pPropData : pointer;
                                                cbPropData : DWORD): HResult;
                      FUNCTION    GetCaptureIAMStreamConfig(VAR pSC: IAMStreamConfig): HResult;
                      PROCEDURE   DeleteCaptureGraph;
                      PROCEDURE   SetCallBack(CB: TVideoSampleCallBack);
                      FUNCTION    GetPlayState: TPlayState;  // Deprecated
                      FUNCTION    SetVideoSizeByListIndex(ListIndex: integer): HResult;
                  END;




  Webcam = Class(ApplicationComponent)
            Protected
              VideoSample   : TVideoSample;
              OnNewFrameBusy: boolean;
              fVideoRunning : boolean;
              fBusy         : boolean;
              fSkipCnt      : integer;
              fFrameCnt     : integer;
              f30FrameTick  : cardinal;
              fFPS          : double;  // "Real" fps, even if not all frames will be displayed.
              fWidth,
              fHeight       : integer;
              fFourCC       : cardinal;

              _DeviceCount : Integer;
              _DeviceList:  Array Of TERRAString;

              fImagePtr     : ARRAY[0..CBufferCnt] OF pointer; // Local copy of image data
              fImagePtrSize : ARRAY[0..CBufferCnt] OF integer;
              fImagePtrIndex: integer;

              fImageUnpacked: boolean;

              _Image:Image;
              _Texture:Texture;

              _LastFrame:Cardinal;

              _NeedsRefresh:Boolean;

              procedure     UnpackFrame(Size: integer; pData: pointer);

              function      VideoSampleIsPaused: boolean;
              procedure     CallBack(pb : pbytearray; var Size: integer);
              function      TranslateProperty(const VP: TVideoProperty; VAR VPAP: TVideoProcAmpProperty): HResult;
              procedure     YUY2_to_RGB(pData: pointer);
              procedure     I420_to_RGB(pData: pointer);

              Procedure InitDeviceList;

              Procedure Init; Override;
              Destructor Destroy; Override;

              Procedure Update; Override;

              Class Function GetUntypedInstance():ApplicationComponent; Override;

            Public
              Class Function Instance:Webcam;

              Property      Width: integer read fWidth;
              Property      Height: integer read fHeight;
              Property DeviceCount:Integer Read _DeviceCount;
              Property      Texture:TERRA_Texture.Texture Read _Texture;

              Property      IsPaused: boolean read VideoSampleIsPaused;
              Property      VideoRunning : boolean read fVideoRunning;

              Property      FramesPerSecond: double read fFPS;
              Property      FramesSkipped: integer read fSkipCnt;

              Function GetDeviceName(ID:Integer):TERRAString;

              Procedure     Stop;
              Procedure     Pause;
              Procedure     Resume;
              Function      Start(DeviceID:Integer): integer;

              Procedure     ShowProperty_Stream;
              Procedure     GetBrightnessSettings(VAR Actual: integer);
              Procedure     SetBrightnessSettings(const Actual: integer);
              Procedure SetResolutionByIndex(Index: integer);
              Function GetVideoPropertySettings(    VP                : TVideoProperty;
                                                           VAR MinVal, MaxVal,
                                                               StepSize, Default,
                                                               Actual            : integer;
                                                           VAR AutoMode: boolean): HResult;
              Function SetVideoPropertySettings(VP: TVideoProperty; Actual: integer; AutoMode: boolean): HResult;
            End;

//FUNCTION GetVideoPropertyName(VP: TVideoProperty):TERRAString;


// http://www.fourcc.org/yuv.php#UYVY

CONST
  FourCC_YUY2 = $32595559;
  FourCC_YUYV = $56595559;
  FourCC_YUNV = $564E5559;

  FourCC_MJPG = $47504A4D;

  FourCC_I420 = $30323449;
  FourCC_YV12 = $32315659;
  FourCC_IYUV = $56555949;



Implementation
Uses TERRA_GraphicsManager, TERRA_OS, TERRA_Math, TERRA_Log;

Procedure DXErrString(S:TERRAString);
Begin
  Log(logError, 'Webcam', S);
End;

FUNCTION GetVideoPropertyName(VP: TVideoProperty):TERRAString;
BEGIN
  CASE VP OF
    VP_Brightness           : Result := 'Brightness';
    VP_Contrast             : Result := 'Contrast';
    VP_Hue                  : Result := 'Hue';
    VP_Saturation           : Result := 'Saturation';
    VP_Sharpness            : Result := 'Sharpness';
    VP_Gamma                : Result := 'Gamma';
    VP_ColorEnable          : Result := 'ColorEnable';
    VP_WhiteBalance         : Result := 'WhiteBalance';
    VP_BacklightCompensation: Result := 'Backlight';
    VP_Gain                 : Result := 'Gain';
  END; {case}
END;



(* Finally, callback seems to work. Previously it only ran for a few seconds.
   The reason for that seemed to be a deadlock (see http://msdn.microsoft.com/en-us/library/ms786692(VS.85).aspx)
   Now the image data is copied immediatly, and a message is sent to invoke the
   display of the data. *)
procedure Webcam.CallBack(pb : pbytearray; var Size: integer);
var
  i  : integer;
  T1 : cardinal;
begin
  Inc(fFrameCnt);

  // Calculate "Frames per second"...
  T1 := GetTime;
  IF fFrameCnt mod 30 = 0 then
    begin
      if f30FrameTick > 0 then
        fFPS := 30000 / (T1-f30FrameTick);
      f30FrameTick := T1;
    end;

  If (GetTime - _LastFrame < 10) Then
  Begin
    Inc(Self.fSkipCnt);
    Exit;
  End;

  _LastFrame := GetTime;

  // Adjust pointer to image data if necessary
  i := (fImagePtrIndex+1) mod CBufferCnt;
  IF fImagePtrSize[i] <> Size then
    begin
      IF fImagePtrSize[i] > 0 then
        FreeMem(fImagePtr[i], fImagePtrSize[i]);
      fImagePtrSize[i] := Size;
      GetMem(fImagePtr[i], fImagePtrSize[i]);
    end;
  // Save image data to local memory
  move(pb^, fImagePtr[i]^, Size);
  fImagePtrIndex := i;
  fImageUnpacked := false;

  UnpackFrame(fImagePtrSize[fImagePtrIndex], fImagePtr[fImagePtrIndex]);
  sleep(0);
end;

Constructor Webcam.Create;
VAR
  i : integer;
begin
  inherited Create;
  fVideoRunning   := false;
  OnNewFrameBusy  := false;

  _NeedsRefresh := False;

  fWidth          := 0;
  fHeight         := 0;
  fFourCC         := 0;
  FOR i := 0 TO CBufferCnt-1 DO
    BEGIN
      fImagePtr[i]     := nil; 
      fImagePtrSize[i] := 0;
    END;

  fBusy           := false;

  _Texture := Nil;
  _Image := Nil;
End;

Destructor  Webcam.Destroy;
VAR
  i : integer;
begin
  Self.Stop;

  FOR i := CBufferCnt-1 DOWNTO 0 DO
    IF fImagePtrSize[i] <> 0 then
      begin
        FreeMem(fImagePtr[i], fImagePtrSize[i]);
        fImagePtr[i] := nil;
        fImagePtrSize[i] := 0;
      end;

  If Assigned(_Texture) Then
    _Texture.Destroy;

  If Assigned(_Image) Then
    _Image.Destroy;
End;




// For Properties see also http://msdn.microsoft.com/en-us/library/ms786938(VS.85).aspx
function Webcam.TranslateProperty(const VP: TVideoProperty; VAR VPAP: TVideoProcAmpProperty): HResult;
begin
  Result := S_OK;
  CASE VP OF
    VP_Brightness             : VPAP := VideoProcAmp_Brightness;
    VP_Contrast               : VPAP := VideoProcAmp_Contrast;
    VP_Hue                    : VPAP := VideoProcAmp_Hue;
    VP_Saturation             : VPAP := VideoProcAmp_Saturation;
    VP_Sharpness              : VPAP := VideoProcAmp_Sharpness;
    VP_Gamma                  : VPAP := VideoProcAmp_Gamma;
    VP_ColorEnable            : VPAP := VideoProcAmp_ColorEnable;
    VP_WhiteBalance           : VPAP := VideoProcAmp_WhiteBalance;
    VP_BacklightCompensation  : VPAP := VideoProcAmp_BacklightCompensation;
    VP_Gain                   : VPAP := VideoProcAmp_Gain;
    else Result := S_False;
  END; {case}
end;



FUNCTION Webcam.GetVideoPropertySettings(VP: TVideoProperty; VAR MinVal, MaxVal, StepSize, Default, Actual: integer; VAR AutoMode: boolean): HResult;
VAR
  VPAP       : TVideoProcAmpProperty;
  pCapsFlags : TVideoProcAmpFlags;
BEGIN
  Result   := S_FALSE;
  MinVal   := -1;
  MaxVal   := -1;
  StepSize := 0;
  Default  := 0;
  Actual   := 0;
  AutoMode := true;
  IF not(assigned(VideoSample)) or Failed(TranslateProperty(VP, VPAP)) then
    exit;
  Result := TranslateProperty(VP, VPAP);
  IF Failed(Result) then
    exit;

  Result := VideoSample.GetVideoPropAmpEx(VPAP, MinVal, MaxVal, StepSize, Default, pCapsFlags, Actual);
  IF Failed(Result) then
    begin
      MinVal   := -1;
      MaxVal   := -1;
      StepSize := 0;
      Default  := 0;
      Actual   := 0;
      AutoMode := true;
    end
    else begin
      AutoMode := pCapsFlags <> VideoProcAmp_Flags_Manual;
    end;
END;



FUNCTION Webcam.SetVideoPropertySettings(VP: TVideoProperty; Actual: integer; AutoMode: boolean): HResult;
VAR
  VPAP       : TVideoProcAmpProperty;
  pCapsFlags : TVideoProcAmpFlags;
BEGIN
  Result := TranslateProperty(VP, VPAP);
  IF not(assigned(VideoSample)) or Failed(Result) then
    exit;
  IF AutoMode
    then pCapsFlags := VideoProcAmp_Flags_Auto
    else pCapsFlags := VideoProcAmp_Flags_Manual;
  Result := VideoSample.SetVideoPropAmpEx(VPAP, pCapsFlags, Actual);
END;

Procedure Webcam.InitDeviceList;
VAR
  Result:HResult;
  pDevEnum     : ICreateDevEnum;
  pClassEnum   : IEnumMoniker;
  st           :TERRAString;

          // Okay, in the original C code from the microsoft samples this
          // is not a subroutine.
          // I decided to use it as a subroutine, because Delphi won't let
          // me free pMoniker or pPropertyBag myself. ( ":= nil" )
          // Hopefully ending the subroutine will clean up all instances of
          // these interfaces automatically...
          FUNCTION GetNextDeviceName(VAR Name:TERRAString): boolean;
          VAR
            pMoniker     : IMoniker;
            pPropertyBag : IPropertyBag;
            v            : OLEvariant;
            cFetched     : ulong;
          BEGIN
            Result := false;
            Name   := '';
            pMoniker := nil;
            IF (S_OK = (pClassEnum.Next (1, pMoniker, @cFetched))) THEN
              BEGIN
                pPropertyBag := nil;
                if S_OK = pMoniker.BindToStorage(nil, nil, IPropertyBag, pPropertyBag) then
                  begin
                    if S_OK = pPropertyBag.Read('FriendlyName', v, nil) then
                      begin
                        Name := v;
                        Result := true;
                      end;
                  end;
              END;
          END; {GetNextDeviceName}

begin
  // Create the system device enumerator
  Result := CoCreateInstance (CLSID_SystemDeviceEnum,
                              nil,
                              CLSCTX_INPROC_SERVER,
                              IID_ICreateDevEnum,
                              pDevEnum);
  {$ifdef DXErr} DXErrString(DXGetErrorDescription9A(Result)); {$endif}
  if (FAILED(Result)) then
    begin
      // Couldn't create system enumerator!
      exit;
    end;

  // Create an enumerator for the video capture devices
  pClassEnum := nil;

  Result := pDevEnum.CreateClassEnumerator (CLSID_VideoInputDeviceCategory, pClassEnum, 0);
  {$ifdef DXErr} DXErrString(DXGetErrorDescription9A(Result)); {$endif}
  if (FAILED(Result)) then
    begin
      // Couldn't create class enumerator!
      exit;
    end;

  // If there are no enumerators for the requested type, then
  // CreateClassEnumerator will succeed, but pClassEnum will be nil.
  if (pClassEnum = nil) then
    begin
       // No video capture device was detected.
       exit;
    end;

  _DeviceCount := 0;
  WHILE GetNextDeviceName(st) DO
  Begin
    Inc(_DeviceCount);
    SetLength(_DeviceList, _DeviceCount);
    _DeviceList[Pred(_DeviceCount)] := St;
  End;
End;

Function Webcam.GetDeviceName(ID:Integer):TERRAString;
Begin
  If (_DeviceCount<=0) Then
    Self.InitDeviceList;

  If (ID>=0) And (ID<_DeviceCount) Then
    Result := _DeviceList[ID]
  Else
    Result := '';
End;


procedure Webcam.Pause;
begin
  if not assigned(VideoSample) then
    exit;
  VideoSample.PauseVideo;
end;


procedure Webcam.Resume;
begin
  if not assigned(VideoSample) then
    exit;
  VideoSample.ResumeVideo;
end;



procedure Webcam.Stop;
begin
  fFPS := 0;
  if not assigned(VideoSample) then
    exit;

  try
    VideoSample.Free;
    VideoSample := nil;
  except
  end;
  fVideoRunning := false;
end;



function Webcam.Start(DeviceID:Integer): integer;
VAR
  hr     : HResult;
  st     :TERRAString;
  W, H   : integer;
  FourCC : cardinal;
  DeviceName:TERRAString;
begin
  fSkipCnt       := 0;
  fFrameCnt      := 0;
  f30FrameTick   := 0;
  fFPS           := 0;
  fImageUnpacked := false;

  If (_DeviceCount<=0) Then
    Self.InitDeviceList;

  DeviceName:= _DeviceList[DeviceID];

  Result := 0;
  if assigned(VideoSample) then
    Self.Stop;

  VideoSample := TVideoSample.Create(Application.Instance.Handle, false, 0, HR); // No longer force RGB24
  try
    hr := VideoSample.StartVideo(DeviceName, false, st) // Not visible. Displays itself...
  except
    hr := -1;
  end;  

  if Failed(hr)then
  Begin
      Self.Stop;
     // SpeedButton_RunVideo.Down := false;
     // ShowMessage(DXGetErrorDescription9A(hr));
     Result := 1;
    end
    else begin
      hr := VideoSample.GetStreamInfo(W, H, FourCC);
      IF Failed(HR)
        then begin
          Self.Stop;
          Result := 1;
        end
        else
        BEGIN
          fWidth := W;
          fHeight := H;
          fFourCC := FourCC;

          _Texture := TERRA_Texture.Texture.New(W, H);
          _Image := Image.Create(_Texture.Width, _Texture.Height);
          _Texture.Update;
          VideoSample.SetCallBack(CallBack);  // Do not call GDI routines in Callback!
        END;
    end;
end;



function Webcam.VideoSampleIsPaused: boolean;
begin
  if assigned(VideoSample)
    then Result := VideoSample.PlayState = PS_PAUSED
    else Result := false;
end;


Var
  ValueY_298, ValueU_100, ValueU_516, ValueV_409, ValueV_208    : ARRAY[byte] OF integer;
  ValueClip     : ARRAY[-1023..1023] OF byte;

Procedure PrepareTables;
Var
  I:Integer;
Begin
  For I:=0 To 255 Do
  Begin
      { http://msdn.microsoft.com/en-us/library/ms893078.aspx
      ValueY_298[i] := (i- 16) * 298  +  128;      //  -4640 .. 71350
      ValueU_100[i] := (i-128) * 100;              // -12800 .. 12700
      ValueU_516[i] := (i-128) * 516;              // -66048 .. 65532
      ValueV_409[i] := (i-128) * 409;              // -52352 .. 51943
      ValueV_208[i] := (i-128) * 208;              // -26624 .. 26416
      }
      // http://en.wikipedia.org/wiki/YCbCr  (ITU-R BT.601)
    ValueY_298[i] := round(i *  298.082);
    ValueU_100[i] := round(i * -100.291);
    ValueU_516[i] := round(i *  516.412  - 276.836*256);
    ValueV_409[i] := round(i *  408.583  - 222.921*256);
    ValueV_208[i] := round(i * -208.120  + 135.576*256);
  End;

  FillChar(ValueClip, SizeOf(ValueClip), #0);
  For I := 0 TO 255 Do
    ValueClip[i] := i;
  For I := 256 TO 1023 Do
    ValueClip[i] := 255;
END;


procedure Webcam.I420_to_RGB(pData: pointer);
// http://en.wikipedia.org/wiki/YCbCr
VAR
  L, X, Y    : integer;
  ps         : PColor;
  pY, pU, pV : pbyte;
begin
  pY := pData;

  FOR Y := 0 TO Pred(_Image.Height) DO
    BEGIN
      ps := _Image.GetPixelOffset(0, Y);

      pU := pData;
      Inc(pU, _Image.Width*(_Image.height + Y div 4));
      pV := PU;
      Inc(pV, _Image.Width*_Image.height div 4);

      FOR X := 0 TO Pred(_Image.Width div 2) DO
        begin
          L := ValueY_298[pY^];
          ps.R := ValueClip[(L + ValueU_516[pU^]                  ) div 256];
          ps.G := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          ps.B := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          ps.A := 255;
          Inc(ps);
          Inc(pY);

          L := ValueY_298[pY^];
          ps.R := ValueClip[(L + ValueU_516[pU^]                     ) div 256];
          ps.G := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          ps.B := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          ps.A := 255;
          Inc(ps);
          Inc(pY);

          Inc(pU);
          Inc(pV);
        end;
    END;
end;



procedure Webcam.YUY2_to_RGB(pData: pointer);
// http://msdn.microsoft.com/en-us/library/ms893078.aspx
// http://en.wikipedia.org/wiki/YCbCr
type
  TFour  = ARRAY[0..3] OF byte;
VAR
  L, X, Y : integer;
  ps      : PColor;
  pf      : ^TFour;
begin
  pf := pData;
  FOR Y := 0 TO Pred(_Image.Height) DO
    BEGIN
      ps := _Image.GetPixelOffset(0, Y);
      FOR X := 0 To Pred(_Image.Width div 2) DO
        begin
          L := ValueY_298[pf^[0]];
          ps.B := ValueClip[(L + ValueU_516[pf^[1]]                     ) div 256];
          ps.G := ValueClip[(L + ValueU_100[pf^[1]] + ValueV_208[pf^[3]]) div 256];
          ps.R := ValueClip[(L                      + ValueV_409[pf^[3]]) div 256];
          ps.A := 255;
          Inc(ps);
          L := ValueY_298[pf^[2]];
          ps.B := ValueClip[(L + ValueU_516[pf^[1]]                     ) div 256];
          ps.G := ValueClip[(L + ValueU_100[pf^[1]] + ValueV_208[pf^[3]]) div 256];
          ps.R := ValueClip[(L                      + ValueV_409[pf^[3]]) div 256];
          ps.A := 255;
          Inc(ps);

          Inc(pf);
        end;
    END;
end;


procedure Webcam.UnpackFrame(Size: integer; pData: pointer);
var
  {f       : file;}
  Unknown : boolean;
  FourCCSt:TERRAString[4];
begin
  IF pData = nil then exit;
  Unknown := false;
  try
    Case fFourCC OF
      0           :  BEGIN
                       IF (Size = fWidth*fHeight*3)
                         then move(pData^, _Image.Pixels^, Size)
                         else Unknown := true;
                     END;
      FourCC_YUY2,
      FourCC_YUYV,
      FourCC_YUNV :  BEGIN
                       IF (Size = fWidth*fHeight*2)
                         then YUY2_to_RGB(pData)
                         else Unknown := true;
                     END;
//      FourCC_MJPG :  BEGIN                    END;
      FourCC_I420,
      FourCC_YV12,
      FourCC_IYUV : BEGIN
                      IF (Size = (fWidth*fHeight*3) div 2)
                        then I420_to_RGB(pData)
                        else Unknown := true;
                    END;
      else          BEGIN
                      {
                      assignfile(f, 'Unknown_Frame.dat');
                      rewrite(f, 1);
                      Blockwrite(f, pData^, Size);
                      closefile(f);
                      }
                      Unknown := true;
                    END;
    end; {case}

    IF Unknown then
      begin
        IF fFourCC = 0
          then FourCCSt := 'RGB'
          else begin
            FourCCSt := '    ';
            move(fFourCC, FourCCSt[1], 4);
          end;

        Log(logDebug, 'Webcam', 'Unknown compression, DataSize: '+INtToStr(Size)+'  FourCC: '+FourCCSt);
      end Else
        _NeedsRefresh := True;

    fImageUnpacked := true;
  except
  end;
end;




procedure Webcam.ShowProperty_Stream;
var
  hr     : HResult;
  W, H   : integer;
  FourCC : cardinal;
begin
  VideoSample.ShowPropertyDialog_CaptureStream;
  hr := VideoSample.GetStreamInfo(W, H, FourCC);
  IF Failed(HR)
    then begin
      Self.Stop;
    end
    else BEGIN
      fWidth := W;
      fHeight := H;
      fFourCC := FourCC;
      _Image.Resize(W, H);
      VideoSample.SetCallBack(CallBack);
    END;
end;



procedure Webcam.GetBrightnessSettings(VAR Actual: integer);
begin
//  VideoSample.GetVideoPropAmp(VideoProcAmp_Brightness, Actual)
end;



procedure Webcam.SetBrightnessSettings(const Actual: integer);
begin
//  VideoSample.SetVideoPropAmp(VideoProcAmp_Brightness, Actual);
end;


PROCEDURE Webcam.SetResolutionByIndex(Index: integer);
VAR
  hr     : HResult;
  W, H   : integer;
  FourCC : cardinal;
BEGIN
  VideoSample.SetVideoSizeByListIndex(Index);
  hr := VideoSample.GetStreamInfo(W, H, FourCC);
  IF Succeeded(HR)
    then begin
      fWidth := W;
      fHeight := H;
      fFourCC := FourCC;
      _Image.Resize(W, H);
    END;
END;


FUNCTION TGUIDEqual(const TG1, TG2 : TGUID): boolean;
BEGIN
  Result := CompareMem(@TG1, @TG2, SizeOf(TGUID));
END; {TGUIDEqual}






// ---= Sample Grabber callback routines =------------------------------------


// In routine TVideoSample.GetInterfaces(..) the callback routine is defined
// with pISampleGrabber.SetCallback(..,..). If the second parameter in that
// call is 1, then the routine below is called during a callback.
// Otherwise, if the parameter is 0, callback routine BufferCB would be called.
function TSampleGrabberCBImpl.SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
var
  BufferLen: integer;
  ppBuffer : pbyte;
begin
  BufferLen := pSample.GetSize;
  if BufferLen > 0 then
    begin
      pSample.GetPointer(ppBuffer); {*}
      if @CallBack = nil
        then SendMessage(Application.Instance.Handle, WM_NewFrame, BufferLen, integer(ppBuffer))
        else Callback(pbytearray(ppBuffer), BufferLen);
    end;
  Result := 0;
end;

{*}
// Nebenbei bemerkt: Beim Debuggen fiel mir auf, daß die von mir verwendete
// WebCam scheinbar einen Triple-Buffer für die Bilddaten verwendet. Die oben
// von pSample.GetPointer(ppBuffer) zurückgelieferte Adresse wiederholt sich
// in einem 3-er Zyklus. Wenn das ein Feature von DirectShow ist und nicht
// von der Kamera-Steuersoftware, dann könnte man selbst auf Double- oder
// Triplebuffering verzichten. 


// In routine TVideoSample.GetInterfaces(..) the callback routine is defined
// with pISampleGrabber.SetCallback(..,..). If the second parameter in that
// call is 0, then the routine below is called during a callback.
// Otherwise, if the parameter is 1, callback routine SampleCB would be called.
function TSampleGrabberCBImpl.BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
begin
  if BufferLen > 0 then
    begin
      if @CallBack = nil
        then SendMessage(Application.Instance.Handle, WM_NewFrame, BufferLen, integer(pBuffer))
        else Callback(pbytearray(pBuffer), BufferLen);
    end;
  Result := 0;
end;


// ---= End of Sample Grabber callback routines =---------------------------






constructor TVideoSample.Create(VideoCanvasHandle: THandle; ForceRGB: boolean; WhichMethodToCallback: integer; VAR HR: HResult);
begin
  ghApp             := 0;

  pIVideoWindow     := nil;
  pIMediaControl    := nil;
  pIMediaEventEx    := nil;
  pIGraphBuilder    := nil;
  pICapGraphBuild2  := nil;
  g_pSCurrent       := PS_Stopped;

  pIAMStreamConfig  := nil;
  piBFSampleGrabber := nil;
  pIAMVideoProcAmp  := nil;

  pIKsPropertySet   := nil;

  pISampleGrabber   := nil;
  pIBFVideoSource   := nil;
  SGrabberCB        := nil;
  _SGrabberCB       := nil;
  pIBFNullGraphicsManager  := nil;

  CallBack          := nil;

  inherited create;

  ghApp             := VideoCanvasHandle;

  HR                := GetInterfaces(ForceRGB, WhichMethodToCallback);
end;




FUNCTION TVideoSample.GetInterfaces(ForceRGB: boolean; WhichMethodToCallback: integer): HRESULT;
VAR
  MT: _AMMediaType;
BEGIN
  //--- Create the filter graph
  Result := CoCreateInstance(CLSID_FilterGraph,
                             nil,
                             CLSCTX_INPROC,
                             IID_IGraphBuilder,
                             pIGraphBuilder);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
   exit;

  //--- Create Sample grabber
  Result := CoCreateInstance(CLSID_SampleGrabber,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IBaseFilter,
                             piBFSampleGrabber);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := CoCreateInstance(CLSID_NullGraphicsManager, nil, CLSCTX_INPROC_SERVER,
                             IID_IBaseFilter, pIBFNullGraphicsManager);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := piBFSampleGrabber.QueryInterface(IID_ISampleGrabber, pISampleGrabber);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  pISampleGrabber.SetBufferSamples(false);  // No buffering required in this demo

  //--- Force 24bit color depth. (RGB24 erzwingen)
  IF ForceRGB then
    begin
      FillChar(MT, sizeOf(MT), #0);
      MT.majortype := MediaType_Video;
      MT.subtype := MediaSubType_RGB24;
      Result := pISampleGrabber.SetMediaType(MT);
      {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
      if (FAILED(Result)) then
        exit;
    end;

  //--- Prepare Sample-Grabber Callback Object----
  if not assigned(SGrabberCB) then
    begin
      SGrabberCB := TSampleGrabberCB.Create;
      TSampleGrabberCB(SGrabberCB).FSampleGrabberCB := TSampleGrabberCBImpl.Create;
      _SGrabberCB := TSampleGrabberCB(SGrabberCB);
         // Should this be _SGrabberCB := SGrabberCB as TSampleGrabberCB ?????!!!!!
         // Compare discussion on
         // http://delphi.newswhat.com/geoxml/forumgetthread?groupname=borland.public.delphi.oodesign&messageid=44f84705@newsgroups.borland.com&displaymode=all
         // However, link has been lost in the web  :(
    end;

  pISampleGrabber.SetCallback(ISampleGrabberCB(_SGrabberCB), WhichMethodToCallback);
         // WhichMethodToCallback=0: SampleGrabber calls SampleCB with the original media sample
         // WhichMethodToCallback=1: SampleGrabber calls BufferCB with a copy of the media sample

  //--- Create the capture graph builder
  Result := CoCreateInstance(CLSID_CaptureGraphBuilder2,
                             nil,
                             CLSCTX_INPROC,
                             IID_ICaptureGraphBuilder2,
                             pICapGraphBuild2);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  // Obtain interfaces for media control and Video Window
  Result := pIGraphBuilder.QueryInterface(IID_IMediaControl, pIMediaControl);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := pIGraphBuilder.QueryInterface(IID_IVideoWindow, pIVideoWindow);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := pIGraphBuilder.QueryInterface(IID_IMediaEvent, pIMediaEventEx);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  //--- Set the window handle used to process graph events
  Result := pIMediaEventEx.SetNotifyWindow(OAHWND(ghApp), WM_GRAPHNOTIFY, 0);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
end;




FUNCTION TVideoSample.ConnectToCaptureDevice(DeviceName:TERRAString; VAR DeviceSelected:TERRAString; VAR ppIBFVideoSource: IBaseFilter): HRESULT;
VAR
  pDevEnum   : ICreateDevEnum;
  pClassEnum : IEnumMoniker;
  Index      : integer;
  Found      : boolean;


          // see also: http://msdn.microsoft.com/en-us/library/ms787619.aspx
          FUNCTION CheckNextDeviceName(Name:TERRAString; VAR Found: boolean): HResult;
          VAR
            pMoniker     : IMoniker;
            pPropertyBag : IPropertyBag;
            v            : OLEvariant;
            cFetched     : ulong;
            MonName      :TERRAString;
          BEGIN
            Found  := false;
            pMoniker := nil;
            // Note that if the Next() call succeeds but there are no monikers,
            // it will return S_FALSE (which is not a failure).  Therefore, we
            // check that the return code is S_OK instead of using SUCCEEDED() macro.
            Result := pClassEnum.Next(1, pMoniker, @cFetched);
            IF (S_OK = Result) THEN
              BEGIN
                Inc(Index);
                pPropertyBag := nil;
                Result := pMoniker.BindToStorage(nil, nil, IPropertyBag, pPropertyBag);
                if S_OK = Result then
                  begin
                    Result := pPropertyBag.Read('FriendlyName', v, nil);   // BTW: Other useful parameter: 'DevicePath'
                    if S_OK = Result then
                      begin
                        MonName := v;
                        if (Uppercase(Trim(MonName)) = UpperCase(Trim(Name))) or
                          ((Length(Name)=2) and (Name[1]='#') and (ord(Name[2])-48=Index)) then
                          begin
                            DeviceSelected := Trim(MonName);
                            Result := pMoniker.BindToObject(nil, nil, IID_IBaseFilter, ppIBFVideoSource);
                            Found := Result = S_OK;
                          end;
                      end;
                  end;
              END;
          END; {CheckNextDeviceName}



BEGIN
  DeviceSelected := '';
  Index := 0;
  DeviceName := Trim(DeviceName);
  IF DeviceName = '' then
    DeviceName := '#1'; // Default: First device (Erstes Gerät)

  if @ppIBFVideoSource = nil then
    begin
      result := E_POINTER;
      exit;
    end;

  // Create the system device enumerator
  Result := CoCreateInstance(CLSID_SystemDeviceEnum,
                             nil,
                             CLSCTX_INPROC,
                             IID_ICreateDevEnum,
                             pDevEnum);
  if (FAILED(Result)) then
    begin
      // Couldn't create system enumerator!
      exit;
    end;

  // Create an enumerator for the video capture devices
  pClassEnum := nil;

  Result := pDevEnum.CreateClassEnumerator (CLSID_VideoInputDeviceCategory, pClassEnum, 0);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      // Couldn't create class enumerator!
      exit;
    end;

  // If there are no enumerators for the requested type, then
  // CreateClassEnumerator will succeed, but pClassEnum will be nil.
  if (pClassEnum = nil) then
    begin
      // No video capture device was detected.
      result := E_FAIL;
      exit;
    end;

  Found := false;
  REPEAT
    try
      Result := CheckNextDeviceName(DeviceName, Found)
    except
      IF Result = 0 then
        result := E_FAIL;
    end;
  UNTIL Found or (Result <> S_OK);
end; {ConnectToCaptureDevice}





procedure TVideoSample.ResizeVideoWindow();
var
  rc : TRect;
begin
  // Resize the video preview window to match owner window size
  if (pIVideoWindow) <> nil then
    begin
        // Make the preview video fill our window
      GetClientRect(ghApp, rc);
      pIVideoWindow.SetWindowPosition(0, 0, rc.right, rc.bottom);
    end;
end; {ResizeVideoWindow}




FUNCTION TVideoSample.SetupVideoWindow(): HRESULT;
BEGIN
  // Set the video window to be a child of the main window
  Result := pIVideoWindow.put_Owner(OAHWND(ghApp));
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      exit;
    end;

  // Set video window style
  Result := pIVideoWindow.put_WindowStyle(WS_CHILD or WS_CLIPCHILDREN);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      exit;
    end;

  // Use helper function to position video window in client rect
  // of main application window
  ResizeVideoWindow();

  // Make the video window visible, now that it is properly positioned
  Result := pIVideoWindow.put_Visible(TRUE);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      exit;
    end;

end; {SetupVideoWindow}




FUNCTION TVideoSample.RestartVideoEx(Visible: boolean):HRESULT;
VAR
  pCut, pTyp : pGuiD;
  {
  pAMVidControl: IAMVideoControl;
  pPin         : IPin;
  }
BEGIN
  if (pIAMVideoProcAmp = nil) then
    if not(S_OK = pIBFVideoSource.QueryInterface(IID_IAMVideoProcAmp, pIAMVideoProcAmp)) then
      pIAMVideoProcAmp := nil;

   if (pIKsPropertySet = nil) then
    if not(S_OK = pIBFVideoSource.QueryInterface(IID_IKsPropertySet, pIKsPropertySet)) then
      pIKsPropertySet := nil;


    // Add Capture filter to our graph.
    Result := pIGraphBuilder.AddFilter(pIBFVideoSource, Widestring('Video Capture'));
    if (FAILED(Result)) then
      begin
        // Couldn''t add the capture filter to the graph!
        exit;
      end;

    Result := pIGraphBuilder.AddFilter(piBFSampleGrabber, Widestring('Sample Grabber'));
    if (FAILED(Result)) then
      EXIT;

    if not(Visible) then
      begin
        Result := pIGraphBuilder.AddFilter(pIBFNullGraphicsManager, WideString('Null GraphicsManager'));
        if (FAILED(Result)) then
          EXIT;
      end;

    // Render the preview pin on the video capture filter
    // Use this instead of pIGraphBuilder->RenderFile
    New(pCut);
    New(pTyp);
    //pCut^ := PIN_CATEGORY_PREVIEW;
    pCut^ := PIN_CATEGORY_CAPTURE;
    pTyp^ := MEDIATYPE_Video;
    try
      if Visible
        then Result := pICapGraphBuild2.RenderStream (pCut, pTyp,
                                    //Addr(PIN_CATEGORY_PREVIEW), Addr(MEDIATYPE_Video),
                                    pIBFVideoSource, piBFSampleGrabber, nil)

        else Result := pICapGraphBuild2.RenderStream (pCut, pTyp,
                                    //Addr(PIN_CATEGORY_PREVIEW), Addr(MEDIATYPE_Video),
                                    pIBFVideoSource, piBFSampleGrabber, pIBFNullGraphicsManager);
    except
      Result := -1;
    end;
    if (FAILED(Result)) then
      begin
        // Couldn''t render the video capture stream.
        // The capture device may already be in use by another application.
        exit;
      end;


    // Set video window style and position
    if Visible then
      begin
        Result := SetupVideoWindow();
        if (FAILED(Result)) then
          begin
            // Couldn't initialize video window!
            exit;
          end;
      end;

  //  if Visible then
      begin
        // Start previewing video data
        Result := pIMediaControl.Run();
        if (FAILED(Result)) then
          begin
            // Couldn't run the graph!
          end;
      end;

    // Remember current state
    g_psCurrent := PS_Running;

    (*
    // !!!!!!!!!
    // Prepare getting images in higher resolution than video stream
    // See DirectX9 Help "Capturing an Image From a Still Image Pin"
    // Not working yet.....
    pAMVidControl := nil;
    Result := pIBFVideoSource.QueryInterface(IID_IAMVideoControl, pAMVidControl);
    IF succeeded(Result) then
      begin
        pTyp := 0;
        pPin := nil;
        Result := pICapGraphBuild2.FindPin(pIBFVideoSource, PINDIR_OUTPUT, PIN_CATEGORY_STILL, pTyp^, false, 0, pPin);
        if (SUCCEEDED(Result)) then
          Result := pAMVidControl.SetMode(pPin, VideoControlFlag_Trigger);
      end;
    *)

end; {RestartVideoEx}


FUNCTION TVideoSample.RestartVideo: HRESULT;
BEGIN
  Result := RestartVideoEx(FVisible);
END; {RestartVideo}


FUNCTION TVideoSample.StartVideo(CaptureDeviceName:TERRAString; Visible: boolean; VAR DeviceSelected:TERRAString):HRESULT;
BEGIN
  pIBFVideoSource := nil;
  FVisible   := Visible;

   // Attach the filter graph to the capture graph
  Result := pICapGraphBuild2.SetFiltergraph(pIGraphBuilder);
  if (FAILED(Result)) then
    begin
      // Failed to set capture filter graph!
      exit;
    end;

  // Use the system device enumerator and class enumerator to find
  // a video capture/preview device, such as a desktop USB video camera.
  Result := ConnectToCaptureDevice(CaptureDeviceName, DeviceSelected, pIBFVideoSource);
  if (FAILED(Result)) then
    begin
      exit;
    end;

  LoadListOfResolution;    
  Result := RestartVideo;
end;



FUNCTION TVideoSample.PauseVideo: HResult;
BEGIN
  IF g_psCurrent = PS_Paused
    then begin
      Result := S_OK;
      EXIT;
    end;
  IF g_psCurrent = PS_Running then
    begin
      Result := pIMediaControl.Pause;
      if Succeeded(Result) then
        g_psCurrent := PS_Paused;
    end
    else Result := S_FALSE;
END;


FUNCTION TVideoSample.ResumeVideo: HResult;
BEGIN
  IF g_psCurrent = PS_Running then
    begin
      Result := S_OK;
      EXIT;
    end;
  IF g_psCurrent = PS_Paused then
    begin
      Result := pIMediaControl.Run;
      if Succeeded(Result) then
        g_psCurrent := PS_Running;
    end
    else Result := S_FALSE;
END;



FUNCTION TVideoSample.StopVideo: HResult;
BEGIN
  // Stop previewing video data
  Result := pIMediaControl.StopWhenReady();
  g_psCurrent := PS_Stopped;
  SetLength(FormatArr, 0);
END;



// Delete filter and pins bottom-up...
PROCEDURE TVideoSample.DeleteBelow(const IBF: IBaseFilter);
VAR
  hr         : HResult;
  pins       : IEnumPins;
  pIPinFrom,
  pIPinTo    : IPin;
  fetched    : ulong;
  pInfo      : _PinInfo;
BEGIN
  pIPinFrom := nil;
  pIPinTo   := nil;
  hr := IBF.EnumPins(pins);
  WHILE (hr = NoError) DO
    BEGIN
      hr := pins.Next(1, pIPinFrom, @fetched);
      if (hr = S_OK) and (pIPinFrom <> nil) then
        BEGIN
          hr := pIPinFrom.ConnectedTo(pIPinTo);
          if (hr = S_OK) and (pIPinTo <> nil) then
            BEGIN
              hr := pIPinTo.QueryPinInfo(pInfo);
              if (hr = NoError) then
                BEGIN
                  if pinfo.dir = PINDIR_INPUT then
                    BEGIN
                      DeleteBelow(pInfo.pFilter);
                      pIGraphBuilder.Disconnect(pIPinTo);
                      pIGraphBuilder.Disconnect(pIPinFrom);
                      pIGraphBuilder.RemoveFilter(pInfo.pFilter);
                    ENd;
                END;
            END;
        END;
    END;
END; {DeleteBelow}



PROCEDURE TVideoSample.DeleteCaptureGraph;
BEGIN
  pIBFVideoSource.Stop;
  DeleteBelow(pIBFVideoSource);
END;



procedure TVideoSample.CloseInterfaces;
begin
  if (pISampleGrabber <> nil) then
    pISampleGrabber.SetCallback(nil, 1);

  // Stop previewing data
  if (pIMediaControl <> nil) then
    pIMediaControl.StopWhenReady();

  g_psCurrent := PS_Stopped;

  // Stop receiving events
  if (pIMediaEventEx <> nil) then
    pIMediaEventEx.SetNotifyWindow(OAHWND(nil), WM_GRAPHNOTIFY, 0);

  // Relinquish ownership (IMPORTANT!) of the video window.
  // Failing to call put_Owner can lead to assert failures within
  // the video GraphicsManager, as it still assumes that it has a valid
  // parent window.
  if (pIVideoWindow<>nil) then
    begin
      pIVideoWindow.put_Visible(FALSE);
      pIVideoWindow.put_Owner(OAHWND(nil));
    end;
end;



function TVideoSample.GetImageBuffer(VAR pb : pbytearray; var Size: integer): HResult;
VAR
  NewSize : integer;
begin
  Result := pISampleGrabber.GetCurrentBuffer(NewSize, nil);
  if (Result <> S_OK) then
    EXIT;
  if (pb <> nil) then
    begin
      if Size <> NewSize then
        begin
          try
            FreeMem(pb, Size);
          except
          end;
          pb := nil;
          Size := 0;
        end;
    end;
  Size := NewSize;
  IF Result = S_OK THEN
    BEGIN
      if pb = nil then
        GetMem(pb, NewSize);
      Result := pISampleGrabber.GetCurrentBuffer(NewSize, pb);
    END;
end;



FUNCTION TVideoSample.SetPreviewState(nShow: boolean): HRESULT;
BEGIN
  Result := S_OK;

  // If the media control interface isn't ready, don't call it
  if (pIMediaControl = nil) then
    exit;

  if (nShow) then
    begin
      if (g_psCurrent <> PS_Running) then
        begin
          // Start previewing video data
          Result := pIMediaControl.Run();
          g_psCurrent := PS_Running;
        end;
    end
    else begin
        // Stop previewing video data
        // Result := pIMediaControl.StopWhenReady(); // Program may get stucked here!
        Result := pIMediaControl.Stop;
        g_psCurrent := PS_Stopped;
    end;
end;




FUNCTION TVideoSample.ShowPropertyDialogEx(const IBF: IUnknown; FilterName: PWideChar): HResult;
VAR
  pProp      : ISpecifyPropertyPages;
  c          : tagCAUUID;
begin
 pProp  := nil;
 Result := IBF.QueryInterface(ISpecifyPropertyPages, pProp);
 if Result = S_OK then
   begin
     Result := pProp.GetPages(c);
     if (Result = S_OK) and (c.cElems > 0) then
       begin
         Result := OleCreatePropertyFrame(ghApp, 0, 0, FilterName, 1, @IBF, c.cElems, c.pElems, 0, 0, nil);
         CoTaskMemFree(c.pElems);
       end;
   end;
end;





FUNCTION TVideoSample.ShowPropertyDialog: HResult;
VAR
  FilterInfo : FILTER_INFO;
begin
  Result := pIBFVideoSource.QueryFilterInfo(FilterInfo);
  if not(Failed(Result)) then
    Result := ShowPropertyDialogEx(pIBFVideoSource, FilterInfo.achName);
end;



FUNCTION TVideoSample.GetCaptureIAMStreamConfig(VAR pSC: IAMStreamConfig): HResult;
BEGIN
  pSC := nil;
  Result := pICapGraphBuild2.FindInterface(@PIN_CATEGORY_capture,
                                           @MEDIATYPE_Video,
                                           pIBFVideoSource,
                                           IID_IAMStreamConfig, pSC);

END;



FUNCTION TVideoSample.ShowPropertyDialog_CaptureStream: HResult;
VAR
  pSC       : IAMStreamConfig;
BEGIN
  pIMediaControl.Stop;
  Result := GetCaptureIAMStreamConfig(pSC);
  if Result = S_OK then
    Result := ShowPropertyDialogEx(pSC, '');
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  pIMediaControl.Run;
END;


// Fills "FormatArr" with list of all supported video formats (resolution, compression etc...)
FUNCTION TVideoSample.LoadListOfResolution: HResult;
VAR
  pSC                   : IAMStreamConfig;
  VideoStreamConfigCaps : TVideoStreamConfigCaps;
  p                     : ^TVideoStreamConfigCaps;
  ppmt                  : PAMMediaType;
  i, j,
  piCount,
  piSize                : integer;
  Swap                  : boolean;
  FM                    : TFormatInfo;
BEGIN
  SetLength(FormatArr, 0);
  Result := GetCaptureIAMStreamConfig(pSC);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  IF Result = S_OK then
    Result := pSC.GetNumberOfCapabilities(piCount, piSize);
  j := 0;
  if Result = S_OK then
    begin
      FOR i := 0 TO piCount-1 DO
        begin
          p := @VideoStreamConfigCaps;
          Result := pSC.GetStreamCaps(i, ppmt, p^);
          IF Succeeded(Result){ and (cardinal(p^.InputSize.cx*p^.InputSize.cy*3) = ppmt^.lSampleSize)} then
            begin
              Inc(j);
              SetLength(FormatArr, j);
              FormatArr[j-1].OIndex := i;
              FormatArr[j-1].Width  := p^.InputSize.cx;
              FormatArr[j-1].Height := p^.InputSize.cy;
              FormatArr[j-1].pmt    := ppmt;
              FormatArr[j-1].SSize  := ppmt^.lSampleSize;
              IF TGuIDEqual(MEDIASUBTYPE_RGB24, ppmt^.Subtype)
                then FormatArr[j-1].FourCC := 'RGB '
                else move(ppmt^.Subtype.D1, FormatArr[j-1].FourCC, 4);
            end;
        end;
    end;

  // Simple sort by width and height
  IF j > 1 then
    begin
      REPEAT
        Swap := false;
        FOR i := 0 TO j-2 DO
          IF (FormatArr[i].Width > FormatArr[i+1].Width) or
             (((FormatArr[i].Width = FormatArr[i+1].Width)) and ((FormatArr[i].Height > FormatArr[i+1].Height)))
          then
            begin
              Swap := true;
              FM := FormatArr[i];
              FormatArr[i] := FormatArr[i+1];
              FormatArr[i+1] := FM;
            end;
      UNTIL not(Swap);
    end;
END;



FUNCTION TVideoSample.SetVideoSizeByListIndex(ListIndex: integer): HResult;
// Sets one of the supported video stream sizes listed in "FormatArr".
// ListIndex is the index to one of the sizes from the stringlist received
// from "GetListOfVideoSizes".
VAR
  pSC                   : IAMStreamConfig;
  VideoStreamConfigCaps : TVideoStreamConfigCaps;
  p                     : ^TVideoStreamConfigCaps;
  ppmt                  : PAMMediaType;
  piCount,
  piSize                : integer;
BEGIN
  IF (ListIndex < 0) or (ListIndex >= Length(FormatArr)) then
    begin
      Result := S_FALSE;
      exit;
    end;

  ListIndex := FormatArr[ListIndex].OIndex;

  pIMediaControl.Stop;

  Result := GetCaptureIAMStreamConfig(pSC);
  IF Succeeded(Result) then
    begin
      piCount := 0;
      piSize  := 0;
      pSC.GetNumberOfCapabilities(piCount, piSize);
      p := @VideoStreamConfigCaps;
      Result := pSC.GetStreamCaps(ListIndex, ppmt, p^);
      IF Succeeded(Result) then
        Result := pSC.SetFormat(ppmt^);
    end;

  pIMediaControl.Run;
END;



FUNCTION TVideoSample.GetStreamInfo(VAR Width, Height: integer; VAR FourCC: dword): HResult;
VAR
  pSC   : IAMStreamConfig;
  ppmt  : PAMMediaType;
  pmt   : _AMMediaType;

  VI    : VideoInfo;
  VIH   : VideoInfoHeader;
BEGIN
  Width := 0;
  Height := 0;
  pIMediaControl.Stop;
  pIBFVideoSource.Stop;  // nicht zwingend nötig

  Result := GetCaptureIAMStreamConfig(pSC);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if Result = S_OK then
    begin
      Result := pSC.GetFormat(ppmt);
      pmt := ppmt^;
      if  TGUIDEqual(ppmt.formattype, FORMAT_VideoInfo) then
        begin
          FillChar(VI, SizeOf(VI), #0);
          VIH := VideoInfoHeader(ppmt^.pbFormat^);
          move(VIH, VI, SizeOf(VIH));
          Width := VI.bmiHeader.biWidth;
          Height := Abs(VI.bmiHeader.biHeight);
          FourCC := VI.bmiHeader.biCompression;
        end;
    end;
  pIBFVideoSource.Run(0);// nicht zwingend nötig
  pIMediaControl.Run;
END;






// See also: http://msdn.microsoft.com/en-us/library/ms784400(VS.85).aspx
FUNCTION TVideoSample.GetVideoPropAmpEx(    Prop                     : TVideoProcAmpProperty;
                                        VAR pMin, pMax,
                                            pSteppingDelta, pDefault : longint;
                                        VAR pCapsFlags               : TVideoProcAmpFlags;
                                        VAR pActual                  : longint): HResult;
BEGIN
  Result := S_False;
  if pIAMVideoProcAmp = nil then
    exit;
  Result := pIAMVideoProcAmp.GetRange(Prop, pMin, pMax, pSteppingDelta, pDefault, pCapsFlags);
  pActual := pDefault;
  IF Result = S_OK then
    Result := pIAMVideoProcAmp.Get(Prop, pActual, pCapsFlags)
END;



FUNCTION TVideoSample.SetVideoPropAmpEx(    Prop           : TVideoProcAmpProperty;
                                            pCapsFlags     : TVideoProcAmpFlags;
                                            pActual        : longint): HResult;
BEGIN
  Result := S_False;
  if pIAMVideoProcAmp = nil then
    exit;
  Result := pIAMVideoProcAmp.Set_(Prop, pActual, pCapsFlags)
END;



PROCEDURE TVideoSample.GetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; VAR AcPerCent: integer);
VAR
  pMin, pMax,
  pSteppingDelta,
  pDefault       : longint;
  pCapsFlags     : TVideoProcAmpFlags;
  pActual        : longint;
BEGIN
  IF GetVideoPropAmpEx(Prop, pMin, pMax, pSteppingDelta, pDefault, pCapsFlags, pActual) = S_OK
    THEN BEGIN
      AcPerCent := round(100 * (pActual-pMin)/(pMax-pMin));
    END
    ELSE AcPerCent := -1;
END;



PROCEDURE TVideoSample.SetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; AcPerCent: integer);
VAR
  pMin, pMax,
  pSteppingDelta,
  pDefault        : longint;
  pCapsFlags      : TVideoProcAmpFlags;
  pActual         : longint;
  d               : double;
BEGIN
  IF GetVideoPropAmpEx(Prop, pMin, pMax, pSteppingDelta, pDefault, pCapsFlags, pActual) = S_OK
    THEN BEGIN
      IF (AcPercent < 0) or (AcPercent > 100) then
        begin
          pActual := pDefault;
        end
        else begin
          d := (pMax-pMin)/100*AcPercent;
          pActual := round(d);
          pActual := (pActual div pSteppingDelta) * pSteppingDelta;
          pActual := pActual + pMin;
        end;
      pIAMVideoProcAmp.Set_(Prop, pActual, pCapsFlags);
    END
END;



PROCEDURE TVideoSample.GetVideoSize(VAR Width, height: integer);
VAR
  pBV : IBasicVideo;
BEGIN
  Width := 0;
  Height := 0;
  pBV := nil;
  if pIGraphBuilder.QueryInterface(IID_IBasicVideo, pBV)=S_OK then
//  if pICapGraphBuild2.FindInterface(@PIN_CATEGORY_capture, @MEDIATYPE_Video, pIBFVideoSource, IID_IBasicVideo, pBV) = S_OK then
    pBV.GetVideoSize(Width, height);
END; {GetVideoSize}



FUNCTION TVideoSample.ShowVfWCaptureDlg: HResult;
VAR
  pVfw : IAMVfwCaptureDialogs;
BEGIN
  pVfw := nil;
  pIMediaControl.Stop;
  Result := pICapGraphBuild2.FindInterface(@PIN_CATEGORY_CAPTURE,
                                     @MEDIATYPE_Video,
                                     pIBFVideoSource,
                                     IID_IAMVfwCaptureDialogs, pVfW);

  if not(Succeeded(Result)) then // Retry
    Result := pICapGraphBuild2.queryinterface(IID_IAMVfwCaptureDialogs, pVfw);
  if not(Succeeded(Result)) then // Retry
    Result := pIGraphBuilder.queryinterface(IID_IAMVfwCaptureDialogs, pVfw);

  if (SUCCEEDED(Result)) THEN
    BEGIN
      // Check if the device supports this dialog box.
      if (S_OK = pVfw.HasDialog(VfwCaptureDialog_Source)) then
        // Show the dialog box.
        Result := pVfw.ShowDialog(VfwCaptureDialog_Source, ghApp);
    END;
  pIMediaControl.Run;
END;



FUNCTION TVideoSample.GetExProp(   guidPropSet : TGuiD;
                                      dwPropID : TAMPropertyPin;
                                pInstanceData  : pointer;
                                cbInstanceData : DWORD;
                                 out pPropData;
                                    cbPropData : DWORD;
                                out pcbReturned: DWORD): HResult;
BEGIN
  Result := pIKsPropertySet.Get(guidPropSet, dwPropID, pInstanceData, cbInstanceData, pPropData, cbPropData, pcbReturned);
END;



FUNCTION TVideoSample.SetExProp(   guidPropSet : TGuiD;
                                      dwPropID : TAMPropertyPin;
                                pInstanceData  : pointer;
                                cbInstanceData : DWORD;
                                     pPropData : pointer;
                                    cbPropData : DWORD): HResult;
BEGIN
  Result := pIKsPropertySet.Set_(guidPropSet, dwPropID, pInstanceData, cbInstanceData, pPropData, cbPropData);
END;


// Does work, if no GDI functions are called within callback!
// See remark on http://msdn.microsoft.com/en-us/library/ms786692(VS.85).aspx
PROCEDURE TVideoSample.SetCallBack(CB: TVideoSampleCallBack);
BEGIN
  CallBack := CB;
  SGrabberCB.FSampleGrabberCB.CallBack := CB;
END;


FUNCTION TVideoSample.GetPlayState: TPlayState;
BEGIN
  Result := g_psCurrent;
END;




destructor TVideoSample.Destroy;
begin
  try
    SetPreviewState(false);
    pIMediaControl.Stop;
    pIBFVideoSource.Stop;
    DeleteCaptureGraph;
    closeInterfaces;
  finally
    try
      inherited destroy;
    except
    end;
  end;
end;

Var
  _Webcam_Instance:Webcam;

Class Function Webcam.Instance: Webcam;
Begin
  If Not Assigned(_Webcam_Instance) Then
    _Webcam_Instance := Webcam.Create;

  Result := _Webcam_Instance;
End;

Procedure Webcam.OnAppUpdate;
Begin
  If _NeedsRefresh Then
  Begin
    _Texture.Refresh(_Image);
    _NeedsRefresh := False;
  End;
End;

Initialization
  PrepareTables;
  RegisterApplicationComponent(Webcam.Instance, GraphicsManager.Instance);
End.
