Unit TERRA_Multimedia;

{$I terra.inc}

Interface
Uses Windows;

Const
  MMLib = 'winmm.dll';

  MAXPNAMELEN      =  32;    // max product name length (including nil)
  MAXERRORLENGTH   = 128;    // max error text length (including nil)
  MAX_JOYSTICKOEMVXDNAME = 260; // max oem vxd name length (including nil)

  MM_MCINOTIFY        = $3B9;

  MMSYSERR_BASE          = 0;
  WAVERR_BASE            = 32;
  MIDIERR_BASE           = 64;
  TIMERR_BASE            = 96;
  JOYERR_BASE            = 160;
  MCIERR_BASE            = 256;
  MIXERR_BASE            = 1024;

  MCI_STRING_OFFSET      = 512;
  MCI_VD_OFFSET          = 1024;
  MCI_CD_OFFSET          = 1088;
  MCI_WAVE_OFFSET        = 1152;
  MCI_SEQ_OFFSET         = 1216;

  JOYERR_NOERROR        = 0;                  // no error
  JOYERR_PARMS          = JOYERR_BASE+5;      // bad parameters
  JOYERR_NOCANDO        = JOYERR_BASE+6;      // request not completed
  JOYERR_UNPLUGGED      = JOYERR_BASE+7;      // joystick is unplugged

  JOY_BUTTON1         = $0001;
  JOY_BUTTON2         = $0002;
  JOY_BUTTON3         = $0004;
  JOY_BUTTON4         = $0008;
  JOY_BUTTON1CHG      = $0100;
  JOY_BUTTON2CHG      = $0200;
  JOY_BUTTON3CHG      = $0400;
  JOY_BUTTON4CHG      = $0800;

// constants used with TJoyInfoEx
  JOY_BUTTON5         = $00000010;
  JOY_BUTTON6         = $00000020;
  JOY_BUTTON7         = $00000040;
  JOY_BUTTON8         = $00000080;
  JOY_BUTTON9         = $00000100;
  JOY_BUTTON10        = $00000200;
  JOY_BUTTON11        = $00000400;
  JOY_BUTTON12        = $00000800;
  JOY_BUTTON13        = $00001000;
  JOY_BUTTON14        = $00002000;
  JOY_BUTTON15        = $00004000;
  JOY_BUTTON16        = $00008000;
  JOY_BUTTON17        = $00010000;
  JOY_BUTTON18        = $00020000;
  JOY_BUTTON19        = $00040000;
  JOY_BUTTON20        = $00080000;
  JOY_BUTTON21        = $00100000;
  JOY_BUTTON22        = $00200000;
  JOY_BUTTON23        = $00400000;
  JOY_BUTTON24        = $00800000;
  JOY_BUTTON25        = $01000000;
  JOY_BUTTON26        = $02000000;
  JOY_BUTTON27        = $04000000;
  JOY_BUTTON28        = $08000000;
  JOY_BUTTON29        = $10000000;
  JOY_BUTTON30        = $20000000;
  JOY_BUTTON31        = $40000000;
  JOY_BUTTON32        = $80000000;

  JOY_POVCENTERED	= -1;
  JOY_POVFORWARD	= 0;
  JOY_POVRIGHT		= 9000;
  JOY_POVBACKWARD	= 18000;
  JOY_POVLEFT		= 27000;

  JOY_RETURNX		= $00000001;
  JOY_RETURNY		= $00000002;
  JOY_RETURNZ		= $00000004;
  JOY_RETURNR		= $00000008;
  JOY_RETURNU		= $00000010; { axis 5 }
  JOY_RETURNV		= $00000020; { axis 6 }
  JOY_RETURNPOV		= $00000040;
  JOY_RETURNBUTTONS	= $00000080;
  JOY_RETURNRAWDATA	= $00000100;
  JOY_RETURNPOVCTS	= $00000200;
  JOY_RETURNCENTERED	= $00000400;
  JOY_USEDEADZONE		= $00000800;
  JOY_RETURNALL  = (JOY_RETURNX or JOY_RETURNY or JOY_RETURNZ or
    JOY_RETURNR or JOY_RETURNU or JOY_RETURNV or
    JOY_RETURNPOV or JOY_RETURNBUTTONS);
  JOY_CAL_READALWAYS	= $00010000;
  JOY_CAL_READXYONLY	= $00020000;
  JOY_CAL_READ3		= $00040000;
  JOY_CAL_READ4		= $00080000;
  JOY_CAL_READXONLY	= $00100000;
  JOY_CAL_READYONLY	= $00200000;
  JOY_CAL_READ5		= $00400000;
  JOY_CAL_READ6		= $00800000;
  JOY_CAL_READZONLY	= $01000000;
  JOY_CAL_READRONLY	= $02000000;
  JOY_CAL_READUONLY	= $04000000;
  JOY_CAL_READVONLY	= $08000000;

// joystick ID constants
  JOYSTICKID1         = 0;
  JOYSTICKID2         = 1;

// joystick driver capabilites
  JOYCAPS_HASZ		= $0001;
  JOYCAPS_HASR		= $0002;
  JOYCAPS_HASU		= $0004;
  JOYCAPS_HASV		= $0008;
  JOYCAPS_HASPOV		= $0010;
  JOYCAPS_POV4DIR		= $0020;
  JOYCAPS_POVCTS		= $0040;

Type
  GamepadInfo = Packed Record
    wXpos: Cardinal;                 { x position }
    wYpos: Cardinal;                 { y position }
    wZpos: Cardinal;                 { z position }
    wButtons: Cardinal;              { button states }
  end;

  GamepadInfoEx = Packed Record
    dwSize: Cardinal;		 { size of structure }
    dwFlags: Cardinal;		 { flags to indicate what to return }
    wXpos: Cardinal;         { x position }
    wYpos: Cardinal;         { y position }
    wZpos: Cardinal;         { z position }
    dwRpos: Cardinal;		 { rudder/4th axis position }
    dwUpos: Cardinal;		 { 5th axis position }
    dwVpos: Cardinal;		 { 6th axis position }
    wButtons: Cardinal;      { button states }
    dwButtonNumber: Cardinal;  { current button number pressed }
    dwPOV: Cardinal;           { point of view state }
    dwReserved1: Cardinal;		 { reserved for communication between winmm & driver }
    dwReserved2: Cardinal;		 { reserved for future expansion }
  end;

  GamepadCaps = Packed Record
    wMid: Word;                  // manufacturer ID
    wPid: Word;                  // product ID
    szPname: array[0..MAXPNAMELEN-1] of AnsiChar;  // product name (NULL terminated string)
    wXmin: Cardinal;                 // minimum x position value
    wXmax: Cardinal;                 // maximum x position value
    wYmin: Cardinal;                 // minimum y position value
    wYmax: Cardinal;                 // maximum y position value
    wZmin: Cardinal;                 // minimum z position value
    wZmax: Cardinal;                 // maximum z position value
    wNumButtons: Cardinal;           // number of buttons
    wPeriodMin: Cardinal;            // minimum message period when captured
    wPeriodMax: Cardinal;            // maximum message period when captured
    wRmin: Cardinal;                 // minimum r position value
    wRmax: Cardinal;                 // maximum r position value
    wUmin: Cardinal;                 // minimum u (5th axis) position value
    wUmax: Cardinal;                 // maximum u (5th axis) position value
    wVmin: Cardinal;                 // minimum v (6th axis) position value
    wVmax: Cardinal;                 // maximum v (6th axis) position value
    wCaps: Cardinal;                 // joystick capabilites
    wMaxAxes: Cardinal;	 	// maximum number of axes supported
    wNumAxes: Cardinal;	 	// number of axes in use
    wMaxButtons: Cardinal;	 	/// maximum number of buttons supported
    szRegKey: array[0..MAXPNAMELEN - 1] of AnsiChar; // registry key
    szOEMVxD: array[0..MAX_JOYSTICKOEMVXDNAME - 1] of AnsiChar; // OEM VxD in use
  End;


Const
  DRVCNF_CANCEL           = $0000;
  DRVCNF_OK               = $0001;
  DRVCNF_RESTART          = $0002;

  DRV_LOAD                = $0001;
  DRV_ENABLE              = $0002;
  DRV_OPEN                = $0003;
  DRV_CLOSE               = $0004;
  DRV_DISABLE             = $0005;
  DRV_FREE                = $0006;
  DRV_CONFIGURE           = $0007;
  DRV_QUERYCONFIGURE      = $0008;
  DRV_INSTALL             = $0009;
  DRV_REMOVE              = $000A;
  DRV_EXITSESSION         = $000B;
  DRV_POWER               = $000F;
  DRV_RESERVED            = $0800;
  DRV_USER                = $4000;

  DRV_CANCEL             = DRVCNF_CANCEL;
  DRV_OK                 = DRVCNF_OK;
  DRV_RESTART            = DRVCNF_RESTART;

  DRV_MCI_FIRST          = DRV_RESERVED;
  DRV_MCI_LAST           = DRV_RESERVED + $FFF;

  MCIERR_INVALID_DEVICE_ID        = MCIERR_BASE + 1;
  MCIERR_UNRECOGNIZED_KEYWORD     = MCIERR_BASE + 3;
  MCIERR_UNRECOGNIZED_COMMAND     = MCIERR_BASE + 5;
  MCIERR_HARDWARE                 = MCIERR_BASE + 6;
  MCIERR_INVALID_DEVICE_NAME      = MCIERR_BASE + 7;
  MCIERR_OUT_OF_MEMORY            = MCIERR_BASE + 8;
  MCIERR_DEVICE_OPEN              = MCIERR_BASE + 9;
  MCIERR_CANNOT_LOAD_DRIVER       = MCIERR_BASE + 10;
  MCIERR_MISSING_COMMAND_STRING   = MCIERR_BASE + 11;
  MCIERR_PARAM_OVERFLOW           = MCIERR_BASE + 12;
  MCIERR_MISSING_STRING_ARGUMENT  = MCIERR_BASE + 13;
  MCIERR_BAD_INTEGER              = MCIERR_BASE + 14;
  MCIERR_PARSER_INTERNAL          = MCIERR_BASE + 15;
  MCIERR_DRIVER_INTERNAL          = MCIERR_BASE + 16;
  MCIERR_MISSING_PARAMETER        = MCIERR_BASE + 17;
  MCIERR_UNSUPPORTED_FUNCTION     = MCIERR_BASE + 18;
  MCIERR_FILE_NOT_FOUND           = MCIERR_BASE + 19;
  MCIERR_DEVICE_NOT_READY         = MCIERR_BASE + 20;
  MCIERR_INTERNAL                 = MCIERR_BASE + 21;
  MCIERR_DRIVER                   = MCIERR_BASE + 22;
  MCIERR_CANNOT_USE_ALL           = MCIERR_BASE + 23;
  MCIERR_MULTIPLE                 = MCIERR_BASE + 24;
  MCIERR_EXTENSION_NOT_FOUND      = MCIERR_BASE + 25;
  MCIERR_OUTOFRANGE               = MCIERR_BASE + 26;
  MCIERR_FLAGS_NOT_COMPATIBLE     = MCIERR_BASE + 28;
  MCIERR_FILE_NOT_SAVED           = MCIERR_BASE + 30;
  MCIERR_DEVICE_TYPE_REQUIRED     = MCIERR_BASE + 31;
  MCIERR_DEVICE_LOCKED            = MCIERR_BASE + 32;
  MCIERR_DUPLICATE_ALIAS          = MCIERR_BASE + 33;
  MCIERR_BAD_CONSTANT             = MCIERR_BASE + 34;
  MCIERR_MUST_USE_SHAREABLE       = MCIERR_BASE + 35;
  MCIERR_MISSING_DEVICE_NAME      = MCIERR_BASE + 36;
  MCIERR_BAD_TIME_FORMAT          = MCIERR_BASE + 37;
  MCIERR_NO_CLOSING_QUOTE         = MCIERR_BASE + 38;
  MCIERR_DUPLICATE_FLAGS          = MCIERR_BASE + 39;
  MCIERR_INVALID_FILE             = MCIERR_BASE + 40;
  MCIERR_NULL_PARAMETER_BLOCK     = MCIERR_BASE + 41;
  MCIERR_UNNAMED_RESOURCE         = MCIERR_BASE + 42;
  MCIERR_NEW_REQUIRES_ALIAS       = MCIERR_BASE + 43;
  MCIERR_NOTIFY_ON_AUTO_OPEN      = MCIERR_BASE + 44;
  MCIERR_NO_ELEMENT_ALLOWED       = MCIERR_BASE + 45;
  MCIERR_NONAPPLICABLE_FUNCTION   = MCIERR_BASE + 46;
  MCIERR_ILLEGAL_FOR_AUTO_OPEN    = MCIERR_BASE + 47;
  MCIERR_FILENAME_REQUIRED        = MCIERR_BASE + 48;
  MCIERR_EXTRA_CHARACTERS         = MCIERR_BASE + 49;
  MCIERR_DEVICE_NOT_INSTALLED     = MCIERR_BASE + 50;
  MCIERR_GET_CD                   = MCIERR_BASE + 51;
  MCIERR_SET_CD                   = MCIERR_BASE + 52;
  MCIERR_SET_DRIVE                = MCIERR_BASE + 53;
  MCIERR_DEVICE_LENGTH            = MCIERR_BASE + 54;
  MCIERR_DEVICE_ORD_LENGTH        = MCIERR_BASE + 55;
  MCIERR_NO_INTEGER               = MCIERR_BASE + 56;

  MCIERR_WAVE_OUTPUTSINUSE        = MCIERR_BASE + 64;
  MCIERR_WAVE_SETOUTPUTINUSE      = MCIERR_BASE + 65;
  MCIERR_WAVE_INPUTSINUSE         = MCIERR_BASE + 66;
  MCIERR_WAVE_SETINPUTINUSE       = MCIERR_BASE + 67;
  MCIERR_WAVE_OUTPUTUNSPECIFIED   = MCIERR_BASE + 68;
  MCIERR_WAVE_INPUTUNSPECIFIED    = MCIERR_BASE + 69;
  MCIERR_WAVE_OUTPUTSUNSUITABLE   = MCIERR_BASE + 70;
  MCIERR_WAVE_SETOUTPUTUNSUITABLE = MCIERR_BASE + 71;
  MCIERR_WAVE_INPUTSUNSUITABLE    = MCIERR_BASE + 72;
  MCIERR_WAVE_SETINPUTUNSUITABLE  = MCIERR_BASE + 73;

  MCIERR_SEQ_DIV_INCOMPATIBLE     = MCIERR_BASE + 80;
  MCIERR_SEQ_PORT_INUSE           = MCIERR_BASE + 81;
  MCIERR_SEQ_PORT_NONEXISTENT     = MCIERR_BASE + 82;
  MCIERR_SEQ_PORT_MAPNODEVICE     = MCIERR_BASE + 83;
  MCIERR_SEQ_PORT_MISCERROR       = MCIERR_BASE + 84;
  MCIERR_SEQ_TIMER                = MCIERR_BASE + 85;
  MCIERR_SEQ_PORTUNSPECIFIED      = MCIERR_BASE + 86;
  MCIERR_SEQ_NOMIDIPRESENT        = MCIERR_BASE + 87;

  MCIERR_NO_WINDOW                = MCIERR_BASE + 90;
  MCIERR_CREATEWINDOW             = MCIERR_BASE + 91;
  MCIERR_FILE_READ                = MCIERR_BASE + 92;
  MCIERR_FILE_WRITE               = MCIERR_BASE + 93;

  MCIERR_NO_IDENTITY              = MCIERR_BASE + 94;
  MCIERR_CUSTOM_DRIVER_BASE       = mcierr_Base + 256;

// MCI command message identifiers
  MCI_OPEN       = $0803;
  MCI_CLOSE      = $0804;
  MCI_ESCAPE     = $0805;
  MCI_PLAY       = $0806;
  MCI_SEEK       = $0807;
  MCI_STOP       = $0808;
  MCI_PAUSE      = $0809;
  MCI_INFO       = $080A;
  MCI_GETDEVCAPS = $080B;
  MCI_SPIN       = $080C;
  MCI_SET        = $080D;
  MCI_STEP       = $080E;
  MCI_RECORD     = $080F;
  MCI_SYSINFO    = $0810;
  MCI_BREAK      = $0811;
  MCI_SOUND      = $0812;
  MCI_SAVE       = $0813;
  MCI_STATUS     = $0814;
  MCI_CUE        = $0830;
  MCI_REALIZE    = $0840;
  MCI_WINDOW     = $0841;
  MCI_PUT        = $0842;
  MCI_WHERE      = $0843;
  MCI_FREEZE     = $0844;
  MCI_UNFREEZE   = $0845;
  MCI_LOAD       = $0850;
  MCI_CUT        = $0851;
  MCI_COPY       = $0852;
  MCI_PASTE      = $0853;
  MCI_UPDATE     = $0854;
  MCI_RESUME     = $0855;
  MCI_DELETE     = $0856;

// all custom MCI command messages must be >= this value
  MCI_USER_MESSAGES               = $400 + drv_MCI_First;
  MCI_LAST                        = $0FFF;

// device ID for "all devices"
  MCI_ALL_DEVICE_ID               = Cardinal(-1);

// constants for predefined MCI device types
  MCI_DEVTYPE_VCR                 = MCI_STRING_OFFSET + 1;
  MCI_DEVTYPE_VIDEODISC           = MCI_STRING_OFFSET + 2;
  MCI_DEVTYPE_OVERLAY             = MCI_STRING_OFFSET + 3;
  MCI_DEVTYPE_CD_AUDIO            = MCI_STRING_OFFSET + 4;
  MCI_DEVTYPE_DAT                 = MCI_STRING_OFFSET + 5;
  MCI_DEVTYPE_SCANNER             = MCI_STRING_OFFSET + 6;
  MCI_DEVTYPE_ANIMATION           = MCI_STRING_OFFSET + 7;
  MCI_DEVTYPE_DIGITAL_VIDEO       = MCI_STRING_OFFSET + 8;
  MCI_DEVTYPE_OTHER               = MCI_STRING_OFFSET + 9;
  MCI_DEVTYPE_WAVEFORM_AUDIO      = MCI_STRING_OFFSET + 10;
  MCI_DEVTYPE_SEQUENCER           = MCI_STRING_OFFSET + 11;

  MCI_DEVTYPE_FIRST              = MCI_DEVTYPE_VCR;
  MCI_DEVTYPE_LAST               = MCI_DEVTYPE_SEQUENCER;
  MCI_DEVTYPE_FIRST_USER         = 1000;

  MCI_OPEN_SHAREABLE              = $00000100;
  MCI_OPEN_ELEMENT                = $00000200;
  MCI_OPEN_ALIAS                  = $00000400;
  MCI_OPEN_ELEMENT_ID             = $00000800;
  MCI_OPEN_TYPE_ID                = $00001000;
  MCI_OPEN_TYPE                   = $00002000;

  MCI_NOTIFY_SUCCESSFUL           = $0001;
  MCI_NOTIFY_SUPERSEDED           = $0002;
  MCI_NOTIFY_ABORTED              = $0004;
  MCI_NOTIFY_FAILURE              = $0008;

// common flags for dwFlags parameter of MCI command messages
  MCI_NOTIFY                      = $00000001;
  MCI_WAIT                        = $00000002;
  MCI_FROM                        = $00000004;
  MCI_TO                          = $00000008;
  MCI_TRACK                       = $00000010;

  MCI_MODE_NOT_READY              = MCI_STRING_OFFSET + 12;
  MCI_MODE_STOP                   = MCI_STRING_OFFSET + 13;
  MCI_MODE_PLAY                   = MCI_STRING_OFFSET + 14;
  MCI_MODE_RECORD                 = MCI_STRING_OFFSET + 15;
  MCI_MODE_SEEK                   = MCI_STRING_OFFSET + 16;
  MCI_MODE_PAUSE                  = MCI_STRING_OFFSET + 17;
  MCI_MODE_OPEN                   = MCI_STRING_OFFSET + 18;

  MCI_SEEK_TO_START               = $00000100;
  MCI_SEEK_TO_END                 = $00000200;

// flags for dwFlags parameter of MCI_STATUS command message
  MCI_STATUS_ITEM                 = $00000100;
  MCI_STATUS_START                = $00000200;

// flags for dwItem field of the MCI_STATUS_PARMS parameter block
  MCI_STATUS_LENGTH               = $00000001;
  MCI_STATUS_POSITION             = $00000002;
  MCI_STATUS_NUMBER_OF_TRACKS     = $00000003;
  MCI_STATUS_MODE                 = $00000004;
  MCI_STATUS_MEDIA_PRESENT        = $00000005;
  MCI_STATUS_TIME_FORMAT          = $00000006;
  MCI_STATUS_READY                = $00000007;
  MCI_STATUS_CURRENT_TRACK        = $00000008;

Type
  MCI_OPEN_PARAMS = Packed Record
    dwCallback:Cardinal;
    wDeviceID:Integer;
    lpstrDeviceType:PAnsiChar;
    lpstrElementName:PAnsiChar;
    lpstrAlias:PAnsiChar;
  End;

  MCI_STATUS_PARAMS = Packed Record
    dwCallback:Cardinal;
    dwReturn:Cardinal;
    dwItem:Cardinal;
    dwTrack:Cardinal;
  End;

  MCI_PLAY_PARAMS = Packed Record
    dwCallback: Cardinal;
    dwFrom: Cardinal;
    dwTo: Cardinal;
  End;

// midi  
  HMIDIIN = Integer;
  HMIDIOUT = Integer;

Const
  CALLBACK_TYPEMASK   = $00070000;    { callback type mask }
  CALLBACK_NULL       = $00000000;    { no callback }
  CALLBACK_WINDOW     = $00010000;    { dwCallback is a HWND }
  CALLBACK_TASK       = $00020000;    { dwCallback is a HTASK }
  CALLBACK_FUNCTION   = $00030000;    { dwCallback is a FARPROC }
  CALLBACK_THREAD     = CALLBACK_TASK;{ thread ID replaces 16 bit task }
  CALLBACK_EVENT      = $00050000;    { dwCallback is an EVENT Handle }

Var
  timeGetTime:Function:Cardinal; stdcall;

  joyGetNumDevs:Function:Cardinal; stdcall;
  joyGetDevCaps:Function (JoyID: Cardinal; Var Caps:GamepadCaps; uSize: Cardinal):Cardinal; stdcall;
  joyGetPos:Function (JoyID: Cardinal; Var Info:GamepadInfo): Cardinal; stdcall;
  joyGetPosEx:Function (JoyID: Cardinal; Var Info:GamepadInfoEx): Cardinal; stdcall;
  joyGetThreshold:Function (JoyID: Cardinal; Var Threshold:Cardinal): Cardinal; stdcall;
  joyReleaseCapture:Function (JoyID: Cardinal): Cardinal; stdcall;
  joySetCapture:Function (Handle: HWND; JoyID, Period: Cardinal; bChanged: BOOL): Cardinal; stdcall;
  joySetThreshold:Function (JoyID, Threshold: Cardinal): Cardinal; stdcall;

  mciGetErrorString:Function (mcierr: Integer; Text:PAnsiChar; Length:Cardinal): BOOL; stdcall;
  mciSendString:Function(Command, ReturnString: PAnsiChar; ReturnLength: Cardinal; WndCallback:HWND): Integer; stdcall;
  mciGetDeviceID:Function (Device: PAnsiChar): Integer; stdcall;
  mciSendCommand:Function (mciId:Integer; uMessage:Cardinal; Param1,Param2:Cardinal): Integer; stdcall;
  mciGetDeviceIDFromElementID:Function (ElementID:Cardinal; strType:PAnsiChar): Integer; stdcall;
  mciExecute:Function (Command:LPCSTR): BOOL; stdcall;

  midiOutOpen:Function(Out lphMidiOut: HMIDIOUT; uDeviceID:Cardinal; dwCallback, dwInstance, dwFlags:Cardinal):Integer; stdcall;
  midiOutClose:Function(hMidiOut: HMIDIOUT):Integer; stdcall;
  midiOutShortMsg:Function(hMidiOut: HMIDIOUT; dwMsg: Cardinal): Integer; stdcall;


Procedure LoadMultimedia();

Implementation
Uses TERRA_Log;

Var
  LibHandle:THandle;

Function _timeGetTime:Cardinal; stdcall;
Begin
  Result := GetTickCount();
End;

Function _joyGetNumDevs: Cardinal; stdcall;
Begin
  Result := 0;
End;

Function _joyGetDevCaps(JoyID: Cardinal; Var lpCaps:GamepadCaps; uSize: Cardinal):Cardinal; stdcall;
Begin
  FillChar(lpCaps, SizeOf(lpCaps), 0);
  Result := 0;
End;

Function _joyGetPos(JoyID: Cardinal; Var Info:GamepadInfo): Cardinal; stdcall;
Begin
  FillChar(Info, SizeOf(Info), 0);
  Result := 0;
End;

Function _joyGetPosEx(JoyID: Cardinal; Var Info:GamepadInfoEx): Cardinal; stdcall;
Begin
  FillChar(Info, SizeOf(Info), 0);
  Result := 0;
End;

Function _joyGetThreshold(JoyID: Cardinal; Var Threshold:Cardinal): Cardinal; stdcall;
Begin
  Threshold := 0;
  Result := 0;
End;

Function _joyReleaseCapture(JoyID: Cardinal): Cardinal; stdcall;
Begin
  Result := 0;
End;

Function _joySetCapture(Handle: HWND; JoyID, uPeriod: Cardinal; bChanged: BOOL): Cardinal; stdcall;
Begin
  Result := 0;
End;

Function _joySetThreshold(JoyID, uThreshold: Cardinal): Cardinal; stdcall;
Begin
  Result := 0;
End;

Function _mciGetErrorString(mcierr: Integer; pszText: PAnsiChar; uLength: Cardinal): BOOL; stdcall;
Begin
  Result := False;
End;

Function _mciSendString(lpstrCommand, lpstrReturnString: PAnsiChar; uReturnLength: Cardinal; hWndCallback: HWND): Integer; stdcall;
Begin
  Result := -1;
End;

Function _mciGetDeviceID(pszDevice: PAnsiChar): Integer; stdcall;
Begin
  Result := -1;
End;

Function _mciSendCommand(mciId:Integer; uMessage: Cardinal; dwParam1, dwParam2: Cardinal): Integer; stdcall;
Begin
  Result := -1;
End;

Function _mciGetDeviceIDFromElementID(dwElementID: Cardinal; lpstrType: PAnsiChar): Integer; stdcall;
Begin
  Result := -1;
End;

Function _mciExecute(pszCommand: LPCSTR): BOOL; stdcall;
Begin
  Result := False;
End;

Function LoadFunction(Name:PAnsiChar; Alternative:FarProc):FARPROC;
Begin
  If (LibHandle = 0) Then
    Result := Nil
  Else
  Begin
    Result := GetProcAddress(LibHandle, Name);
    If Result = Nil Then
      Result := GetProcAddress(LibHandle, PAnsiChar(Name+'A')); // ansi version
  End;

  If Result = Nil Then
    Result := Alternative;
End;

Function _midiOutOpen(Out lphMidiOut: HMIDIOUT; uDeviceID:Cardinal; dwCallback, dwInstance, dwFlags:Cardinal):Integer; stdcall;
Begin
  Result := 0;
End;

Function _midiOutClose(hMidiOut: HMIDIOUT):Integer; stdcall;
Begin
  Result := 0;
End;

Function _midiOutShortMsg(hMidiOut: HMIDIOUT; dwMsg: Cardinal): Integer; stdcall;
Begin
  Result := 0;
End;

Procedure LoadMultimedia();
Begin
  Log(logDebug, 'Multimedia', 'Loading Windows Multimedia librar');

  LibHandle := LoadLibrary(MMLib);

  timeGetTime := LoadFunction('timeGetTime', @_timeGetTime);
  joyGetNumDevs := LoadFunction('joyGetNumDevs', @_joyGetNumDevs);
  joyGetDevCaps := LoadFunction('joyGetDevCaps', @_joyGetDevCaps);
  joyGetPos := LoadFunction('joyGetPos', @_joyGetPos);
  joyGetPosEx := LoadFunction('joyGetPosEx', @_joyGetPosEx);
  joyGetThreshold := LoadFunction('joyGetThreshold', @_joyGetThreshold);
  joyReleaseCapture := LoadFunction('joyReleaseCapture', @_joyReleaseCapture);
  joySetCapture := LoadFunction('joySetCapture', @_joySetCapture);
  joySetThreshold := LoadFunction('joySetThreshold', @_joySetThreshold);
  mciGetErrorString := LoadFunction('mciGetErrorString', @_mciGetErrorString);
  mciSendString := LoadFunction('mciSendString', @_mciSendString);
  mciGetDeviceID := LoadFunction('mciGetDeviceID', @_mciGetDeviceID);
  mciSendCommand := LoadFunction('mciSendCommand', @_mciSendCommand);
  mciGetDeviceIDFromElementID := LoadFunction('mciGetDeviceIDFromElementID', @_mciGetDeviceIDFromElementID);
  mciExecute := LoadFunction('mciExecute', @_mciExecute);

  midiOutOpen := LoadFunction('midiOutOpen', @_midiOutOpen);
  midiOutClose := LoadFunction('midiOutClose', @_midiOutClose);
  midiOutShortMsg := LoadFunction('midiOutShortMsg', @_midiOutShortMsg);
End;

End.
