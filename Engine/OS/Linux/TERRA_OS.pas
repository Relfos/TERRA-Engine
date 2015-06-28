// Linux port
//http://www.cl.cam.ac.uk/~mgk25/ucs/keysym2ucs.c

Unit TERRA_OS;
{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Application, unix, baseunix, dateutils,
  GLX,X,Xlib,Xutil,Keysym;

Const
	PathSeparator = '/';
	CrLf = #10;

	keyBackspace  = 8;
	keyTab        = 9;
	keyEnter      = 13;
	keyShift      = 16;
	keyControl    = 17;
	keyAlt        = 18;
	keyPause      = 19;
	keyEscape     = 27;
	keySpace      = 32;
	keyPageUp     = 33;
	keyPageDown   = 34;
	keyEnd        = 35;
	keyHome       = 36;
  keyPlus       = 107;
  keyMinus      = 109;
  keyPeriod     = 190;

	keyLeft       = 37;
	keyUp         = 38;
	keyRight      = 39;
	keyDown       = 40;

	keyInsert     = 45;
	keyDelete     = 46;
	keyF1         = 112;
	keyF2         = 113;
	keyF3         = 114;
	keyF4         = 115;
	keyF5         = 116;
	keyF6         = 117;
	keyF7         = 118;
	keyF8         = 119;
	keyF9         = 120;
	keyF10        = 121;
	keyF11        = 122;
	keyF12        = 123;

  keyA = Ord('A');
  keyB = Ord('B');
  keyC = Ord('C');
  keyD = Ord('D');
  keyE = Ord('E');
  keyF = Ord('F');
  keyG = Ord('G');
  keyH = Ord('H');
  keyI = Ord('I');
  keyJ = Ord('J');
  keyK = Ord('K');
  keyL = Ord('L');
  keyM = Ord('M');
  keyN = Ord('N');
  keyO = Ord('O');
  keyP = Ord('P');
  keyQ = Ord('Q');
  keyR = Ord('R');
  keyS = Ord('S');
  keyT = Ord('T');
  keyU = Ord('U');
  keyV = Ord('V');
  keyW = Ord('W');
  keyX = Ord('X');
  keyY = Ord('Y');
  keyZ = Ord('Z');

Type
  LinuxApplication = Class(BaseApplication)
    Protected
        _Display:PDisplay;
      	_ScreenHandle:Integer;
      	_Window:TWindow;
      	_Attr:TXSetWindowAttributes;
        _ComposeStatus:TXComposeStatus;

      Function InitSettings:Boolean; Override;

      Function InitWindow:Boolean; Override;
      Procedure CloseWindow; Override;
      Procedure ProcessMessages; Override;

    Public
      Constructor Create();

    	Procedure SetState(State:Cardinal); Override;

      Class Function GetCurrentTime:TERRATime;
      Class Function GetCurrentDate:TERRADate;
      Class Function GetTime:Cardinal;

      Class Function Instance:LinuxApplication;

      Property Handle:TWindow Read _Window;
      Property ScreenHandle:Integer Read _ScreenHandle;
      Property Display:PDisplay Read _Display;
  End;

  Application = LinuxApplication;

Implementation
Uses sysutils, ctypes, xrandr, xkblib,
  TERRA_InputManager, TERRA_Gamepad, TERRA_Error, TERRA_Log, TERRA_Renderer, TERRA_GLRenderer;

Var
  _Application_Instance:LinuxApplication;

Constructor LinuxApplication.Create();
Begin
  _Application_Instance := Self;
  Inherited Create();
End;

Class Function LinuxApplication.Instance:LinuxApplication;
Begin
  Result := _Application_Instance;
End;

Class Function LinuxApplication.GetCurrentTime:TERRATime;
Var
 Datetime:Tdatetime;
Begin
 datetime := Now();
 Result.Hour     := hourof( datetime );
 Result.minute   := minuteof( datetime );
 Result.second   := secondof( datetime );
 Result.MiliSecond  := millisecondof( datetime );
End;

Class Function LinuxApplication.GetCurrentDate:TERRADate;
Var
 Datetime:Tdatetime;
Begin
 datetime := Today();
  Result.Year := yearof(datetime);
  Result.Month := monthof(datetime);
  Result.Day := dayof(datetime);
  Result.WeekDay := dayoftheweek(datetime);
End;


Type
	PtTimeSpec = ^tTimeSpec;
Function clock_gettime(clkid:Integer; t:PtTimeSpec):Integer; cdecl; external;
Const CLOCK_MONOTONIC = 1;

Class Function LinuxApplication.GetTime:Cardinal;
var
  ts: TTimeSpec;
  i: Int64;
begin
  clock_gettime(CLOCK_MONOTONIC, @ts);
  i := ts.tv_sec;
  i := i*1000 + ts.tv_nsec div 1000000;
  Result := i and $ffffffff; //cut to unsig 32bit

//assembler; asm DW 0310FH end;
  //fpgettimeofday(@t,nil);
//  Result := Cardinal(Trunc(t.seconds * 1000 + t.nanos + 0.5));
//  Result := Cardinal(Trunc((t.tv_sec * 1000) + (t.tv_usec / 1000000) + 0.5));
//   Result := Trunc(Now * 24 * 60 * 60 * 1000);
End;

{Function XKeycodeToKeysym(Display:PDisplay; Keycode, Shift:Integer):Integer;
Var
  keysyms_per_keycode_return:Integer;
  keysym:PKeySym;
  I:Integer;
Begin
  keysym := XGetKeyboardMapping(Display, Keycode, 1, @keysyms_per_keycode_return);

  For I:=Pred(keysyms_per_keycode_return) DownTo 0 Do
  Begin
      Result := keysym[I];
      If Result>0 Then
         Break;
  End;

  XFree(keysym);
End;}

Function KeyLookUp(Key:Integer):Integer;
Begin
  Case Key Of
       XK_BackSpace: Result := keyBackspace;
	XK_Tab: Result := keyTab;
	XK_Return: Result := keyEnter;

	XK_Shift_L,
        XK_Shift_R: Result := keyShift;

	XK_Control_L,
        XK_Control_R: Result := keyControl;

	XK_Alt_L,
        XK_Alt_R: Result := keyAlt;

	XK_Break: Result := keyPause;
	XK_Escape: Result := keyEscape;
        XK_space: Result := keySpace;
	XK_Page_Up: Result := keyPageUp;
	XK_Page_Down: Result := keyPageDown;
	XK_End: Result := keyEnd;
	XK_Home: Result := keyHome;

	XK_Left: Result := keyLeft;
	XK_Up: Result := keyUp;
	XK_Right: Result := keyRight;
	XK_Down: Result := keyDown;

	XK_Insert: Result := keyInsert;
        XK_Delete: Result := keyDelete;

	XK_F1: Result := keyF1;
        XK_F2: Result := keyF2;
	XK_F3: Result := keyF3;
	XK_F4: Result := keyF4;
	XK_F5: Result := keyF5;
	XK_F6: Result := keyF6;
	XK_F7: Result := keyF7;
	XK_F8: Result := keyF8;
	XK_F9: Result := keyF9;
	XK_F10: Result := keyF10;
	XK_F11: Result := keyF11;
	XK_F12: Result := keyF12;

        XK_A: Result := keyA;
        XK_B: Result := keyB;
        XK_C: Result := keyC;
        XK_D: Result := keyD;
        XK_E: Result := keyE;
        XK_F: Result := keyF;
        XK_G: Result := keyG;
        XK_H: Result := keyH;
        XK_I: Result := keyI;
        XK_J: Result := keyJ;
        XK_K: Result := keyK;
        XK_L: Result := keyL;
        XK_M: Result := keyM;
        XK_N: Result := keyN;
        XK_O: Result := keyO;
        XK_P: Result := keyP;
        XK_Q: Result := keyQ;
        XK_R: Result := keyR;
        XK_S: Result := keyS;
        XK_T: Result := keyT;
        XK_U: Result := keyU;
        XK_V: Result := keyV;
        XK_W: Result := keyW;
        XK_X: Result := keyX;
        XK_Y: Result := keyY;
        XK_Z: Result := keyZ;
  Else
    Result := 0;
  End;
End;

Const
  keysymtabCount = 758;
  keysymtab:Array[0..Pred(keysymtabCount), 0..1] Of Cardinal = (
  ( $01a1, $0104 ), //                     Aogonek A LATIN CAPITAL LETTER A WITH OGONEK
  ( $01a2, $02d8 ), //                       breve ? BREVE
  ( $01a3, $0141 ), //                     Lstroke L LATIN CAPITAL LETTER L WITH STROKE
  ( $01a5, $013d ), //                      Lcaron L LATIN CAPITAL LETTER L WITH CARON
  ( $01a6, $015a ), //                      Sacute S LATIN CAPITAL LETTER S WITH ACUTE
  ( $01a9, $0160 ), //                      Scaron Š LATIN CAPITAL LETTER S WITH CARON
  ( $01aa, $015e ), //                    Scedilla S LATIN CAPITAL LETTER S WITH CEDILLA
  ( $01ab, $0164 ), //                      Tcaron T LATIN CAPITAL LETTER T WITH CARON
  ( $01ac, $0179 ), //                      Zacute Z LATIN CAPITAL LETTER Z WITH ACUTE
  ( $01ae, $017d ), //                      Zcaron Ž LATIN CAPITAL LETTER Z WITH CARON
  ( $01af, $017b ), //                   Zabovedot Z LATIN CAPITAL LETTER Z WITH DOT ABOVE 
  ( $01b1, $0105 ), //                     aogonek a LATIN SMALL LETTER A WITH OGONEK 
  ( $01b2, $02db ), //                      ogonek ? OGONEK 
  ( $01b3, $0142 ), //                     lstroke l LATIN SMALL LETTER L WITH STROKE 
  ( $01b5, $013e ), //                      lcaron l LATIN SMALL LETTER L WITH CARON 
  ( $01b6, $015b ), //                      sacute s LATIN SMALL LETTER S WITH ACUTE 
  ( $01b7, $02c7 ), //                       caron ? CARON 
  ( $01b9, $0161 ), //                      scaron š LATIN SMALL LETTER S WITH CARON 
  ( $01ba, $015f ), //                    scedilla s LATIN SMALL LETTER S WITH CEDILLA 
  ( $01bb, $0165 ), //                      tcaron t LATIN SMALL LETTER T WITH CARON 
  ( $01bc, $017a ), //                      zacute z LATIN SMALL LETTER Z WITH ACUTE 
  ( $01bd, $02dd ), //                 doubleacute ? DOUBLE ACUTE ACCENT 
  ( $01be, $017e ), //                      zcaron ž LATIN SMALL LETTER Z WITH CARON 
  ( $01bf, $017c ), //                   zabovedot z LATIN SMALL LETTER Z WITH DOT ABOVE 
  ( $01c0, $0154 ), //                      Racute R LATIN CAPITAL LETTER R WITH ACUTE 
  ( $01c3, $0102 ), //                      Abreve A LATIN CAPITAL LETTER A WITH BREVE 
  ( $01c5, $0139 ), //                      Lacute L LATIN CAPITAL LETTER L WITH ACUTE 
  ( $01c6, $0106 ), //                      Cacute C LATIN CAPITAL LETTER C WITH ACUTE 
  ( $01c8, $010c ), //                      Ccaron C LATIN CAPITAL LETTER C WITH CARON 
  ( $01ca, $0118 ), //                     Eogonek E LATIN CAPITAL LETTER E WITH OGONEK 
  ( $01cc, $011a ), //                      Ecaron E LATIN CAPITAL LETTER E WITH CARON 
  ( $01cf, $010e ), //                      Dcaron D LATIN CAPITAL LETTER D WITH CARON 
  ( $01d0, $0110 ), //                     Dstroke Ð LATIN CAPITAL LETTER D WITH STROKE 
  ( $01d1, $0143 ), //                      Nacute N LATIN CAPITAL LETTER N WITH ACUTE 
  ( $01d2, $0147 ), //                      Ncaron N LATIN CAPITAL LETTER N WITH CARON 
  ( $01d5, $0150 ), //                Odoubleacute O LATIN CAPITAL LETTER O WITH DOUBLE ACUTE 
  ( $01d8, $0158 ), //                      Rcaron R LATIN CAPITAL LETTER R WITH CARON 
  ( $01d9, $016e ), //                       Uring U LATIN CAPITAL LETTER U WITH RING ABOVE 
  ( $01db, $0170 ), //                Udoubleacute U LATIN CAPITAL LETTER U WITH DOUBLE ACUTE 
  ( $01de, $0162 ), //                    Tcedilla T LATIN CAPITAL LETTER T WITH CEDILLA 
  ( $01e0, $0155 ), //                      racute r LATIN SMALL LETTER R WITH ACUTE
  ( $01e3, $0103 ), //                      abreve a LATIN SMALL LETTER A WITH BREVE 
  ( $01e5, $013a ), //                      lacute l LATIN SMALL LETTER L WITH ACUTE 
  ( $01e6, $0107 ), //                      cacute c LATIN SMALL LETTER C WITH ACUTE 
  ( $01e8, $010d ), //                      ccaron c LATIN SMALL LETTER C WITH CARON 
  ( $01ea, $0119 ), //                     eogonek e LATIN SMALL LETTER E WITH OGONEK
  ( $01ec, $011b ), //                      ecaron e LATIN SMALL LETTER E WITH CARON 
  ( $01ef, $010f ), //                      dcaron d LATIN SMALL LETTER D WITH CARON 
  ( $01f0, $0111 ), //                     dstroke d LATIN SMALL LETTER D WITH STROKE 
  ( $01f1, $0144 ), //                      nacute n LATIN SMALL LETTER N WITH ACUTE 
  ( $01f2, $0148 ), //                      ncaron n LATIN SMALL LETTER N WITH CARON 
  ( $01f5, $0151 ), //                odoubleacute o LATIN SMALL LETTER O WITH DOUBLE ACUTE 
  ( $01f8, $0159 ), //                      rcaron r LATIN SMALL LETTER R WITH CARON 
  ( $01f9, $016f ), //                       uring u LATIN SMALL LETTER U WITH RING ABOVE 
  ( $01fb, $0171 ), //                udoubleacute u LATIN SMALL LETTER U WITH DOUBLE ACUTE 
  ( $01fe, $0163 ), //                    tcedilla t LATIN SMALL LETTER T WITH CEDILLA 
  ( $01ff, $02d9 ), //                    abovedot ? DOT ABOVE 
  ( $02a1, $0126 ), //                     Hstroke H LATIN CAPITAL LETTER H WITH STROKE 
  ( $02a6, $0124 ), //                 Hcircumflex H LATIN CAPITAL LETTER H WITH CIRCUMFLEX 
  ( $02a9, $0130 ), //                   Iabovedot I LATIN CAPITAL LETTER I WITH DOT ABOVE 
  ( $02ab, $011e ), //                      Gbreve G LATIN CAPITAL LETTER G WITH BREVE 
  ( $02ac, $0134 ), //                 Jcircumflex J LATIN CAPITAL LETTER J WITH CIRCUMFLEX 
  ( $02b1, $0127 ), //                     hstroke h LATIN SMALL LETTER H WITH STROKE 
  ( $02b6, $0125 ), //                 hcircumflex h LATIN SMALL LETTER H WITH CIRCUMFLEX 
  ( $02b9, $0131 ), //                    idotless i LATIN SMALL LETTER DOTLESS I 
  ( $02bb, $011f ), //                      gbreve g LATIN SMALL LETTER G WITH BREVE 
  ( $02bc, $0135 ), //                 jcircumflex j LATIN SMALL LETTER J WITH CIRCUMFLEX 
  ( $02c5, $010a ), //                   Cabovedot C LATIN CAPITAL LETTER C WITH DOT ABOVE 
  ( $02c6, $0108 ), //                 Ccircumflex C LATIN CAPITAL LETTER C WITH CIRCUMFLEX 
  ( $02d5, $0120 ), //                   Gabovedot G LATIN CAPITAL LETTER G WITH DOT ABOVE 
  ( $02d8, $011c ), //                 Gcircumflex G LATIN CAPITAL LETTER G WITH CIRCUMFLEX 
  ( $02dd, $016c ), //                      Ubreve U LATIN CAPITAL LETTER U WITH BREVE 
  ( $02de, $015c ), //                 Scircumflex S LATIN CAPITAL LETTER S WITH CIRCUMFLEX 
  ( $02e5, $010b ), //                   cabovedot c LATIN SMALL LETTER C WITH DOT ABOVE 
  ( $02e6, $0109 ), //                 ccircumflex c LATIN SMALL LETTER C WITH CIRCUMFLEX 
  ( $02f5, $0121 ), //                   gabovedot g LATIN SMALL LETTER G WITH DOT ABOVE 
  ( $02f8, $011d ), //                 gcircumflex g LATIN SMALL LETTER G WITH CIRCUMFLEX
  ( $02fd, $016d ), //                      ubreve u LATIN SMALL LETTER U WITH BREVE 
  ( $02fe, $015d ), //                 scircumflex s LATIN SMALL LETTER S WITH CIRCUMFLEX 
  ( $03a2, $0138 ), //                         kra ? LATIN SMALL LETTER KRA 
  ( $03a3, $0156 ), //                    Rcedilla R LATIN CAPITAL LETTER R WITH CEDILLA 
  ( $03a5, $0128 ), //                      Itilde I LATIN CAPITAL LETTER I WITH TILDE
  ( $03a6, $013b ), //                    Lcedilla L LATIN CAPITAL LETTER L WITH CEDILLA 
  ( $03aa, $0112 ), //                     Emacron E LATIN CAPITAL LETTER E WITH MACRON 
  ( $03ab, $0122 ), //                    Gcedilla G LATIN CAPITAL LETTER G WITH CEDILLA 
  ( $03ac, $0166 ), //                      Tslash T LATIN CAPITAL LETTER T WITH STROKE 
  ( $03b3, $0157 ), //                    rcedilla r LATIN SMALL LETTER R WITH CEDILLA 
  ( $03b5, $0129 ), //                      itilde i LATIN SMALL LETTER I WITH TILDE 
  ( $03b6, $013c ), //                    lcedilla l LATIN SMALL LETTER L WITH CEDILLA 
  ( $03ba, $0113 ), //                     emacron e LATIN SMALL LETTER E WITH MACRON 
  ( $03bb, $0123 ), //                    gcedilla g LATIN SMALL LETTER G WITH CEDILLA 
  ( $03bc, $0167 ), //                      tslash t LATIN SMALL LETTER T WITH STROKE 
  ( $03bd, $014a ), //                         ENG ? LATIN CAPITAL LETTER ENG 
  ( $03bf, $014b ), //                         eng ? LATIN SMALL LETTER ENG 
  ( $03c0, $0100 ), //                     Amacron A LATIN CAPITAL LETTER A WITH MACRON 
  ( $03c7, $012e ), //                     Iogonek I LATIN CAPITAL LETTER I WITH OGONEK 
  ( $03cc, $0116 ), //                   Eabovedot E LATIN CAPITAL LETTER E WITH DOT ABOVE 
  ( $03cf, $012a ), //                     Imacron I LATIN CAPITAL LETTER I WITH MACRON 
  ( $03d1, $0145 ), //                    Ncedilla N LATIN CAPITAL LETTER N WITH CEDILLA 
  ( $03d2, $014c ), //                     Omacron O LATIN CAPITAL LETTER O WITH MACRON 
  ( $03d3, $0136 ), //                    Kcedilla K LATIN CAPITAL LETTER K WITH CEDILLA 
  ( $03d9, $0172 ), //                     Uogonek U LATIN CAPITAL LETTER U WITH OGONEK 
  ( $03dd, $0168 ), //                      Utilde U LATIN CAPITAL LETTER U WITH TILDE 
  ( $03de, $016a ), //                     Umacron U LATIN CAPITAL LETTER U WITH MACRON 
  ( $03e0, $0101 ), //                     amacron a LATIN SMALL LETTER A WITH MACRON 
  ( $03e7, $012f ), //                     iogonek i LATIN SMALL LETTER I WITH OGONEK 
  ( $03ec, $0117 ), //                   eabovedot e LATIN SMALL LETTER E WITH DOT ABOVE 
  ( $03ef, $012b ), //                     imacron i LATIN SMALL LETTER I WITH MACRON 
  ( $03f1, $0146 ), //                    ncedilla n LATIN SMALL LETTER N WITH CEDILLA 
  ( $03f2, $014d ), //                     omacron o LATIN SMALL LETTER O WITH MACRON 
  ( $03f3, $0137 ), //                    kcedilla k LATIN SMALL LETTER K WITH CEDILLA 
  ( $03f9, $0173 ), //                     uogonek u LATIN SMALL LETTER U WITH OGONEK 
  ( $03fd, $0169 ), //                      utilde u LATIN SMALL LETTER U WITH TILDE
  ( $03fe, $016b ), //                     umacron u LATIN SMALL LETTER U WITH MACRON 
  ( $047e, $203e ), //                    overline ? OVERLINE 
  ( $04a1, $3002 ), //               kana_fullstop ? IDEOGRAPHIC FULL STOP 
  ( $04a2, $300c ), //         kana_openingbracket ? LEFT CORNER BRACKET 
  ( $04a3, $300d ), //         kana_closingbracket ? RIGHT CORNER BRACKET
  ( $04a4, $3001 ), //                  kana_comma ? IDEOGRAPHIC COMMA 
  ( $04a5, $30fb ), //            kana_conjunctive · KATAKANA MIDDLE DOT 
  ( $04a6, $30f2 ), //                     kana_WO ? KATAKANA LETTER WO 
  ( $04a7, $30a1 ), //                      kana_a ? KATAKANA LETTER SMALL A 
  ( $04a8, $30a3 ), //                      kana_i ? KATAKANA LETTER SMALL I 
  ( $04a9, $30a5 ), //                      kana_u ? KATAKANA LETTER SMALL U 
  ( $04aa, $30a7 ), //                      kana_e ? KATAKANA LETTER SMALL E 
  ( $04ab, $30a9 ), //                      kana_o ? KATAKANA LETTER SMALL O 
  ( $04ac, $30e3 ), //                     kana_ya ? KATAKANA LETTER SMALL YA 
  ( $04ad, $30e5 ), //                     kana_yu ? KATAKANA LETTER SMALL YU 
  ( $04ae, $30e7 ), //                     kana_yo ? KATAKANA LETTER SMALL YO 
  ( $04af, $30c3 ), //                    kana_tsu ? KATAKANA LETTER SMALL TU 
  ( $04b0, $30fc ), //              prolongedsound ? KATAKANA-HIRAGANA PROLONGED SOUND MARK 
  ( $04b1, $30a2 ), //                      kana_A ? KATAKANA LETTER A 
  ( $04b2, $30a4 ), //                      kana_I ? KATAKANA LETTER I 
  ( $04b3, $30a6 ), //                      kana_U ? KATAKANA LETTER U 
  ( $04b4, $30a8 ), //                      kana_E ? KATAKANA LETTER E 
  ( $04b5, $30aa ), //                      kana_O ? KATAKANA LETTER O 
  ( $04b6, $30ab ), //                     kana_KA ? KATAKANA LETTER KA 
  ( $04b7, $30ad ), //                     kana_KI ? KATAKANA LETTER KI 
  ( $04b8, $30af ), //                     kana_KU ? KATAKANA LETTER KU 
  ( $04b9, $30b1 ), //                     kana_KE ? KATAKANA LETTER KE 
  ( $04ba, $30b3 ), //                     kana_KO ? KATAKANA LETTER KO 
  ( $04bb, $30b5 ), //                     kana_SA ? KATAKANA LETTER SA 
  ( $04bc, $30b7 ), //                    kana_SHI ? KATAKANA LETTER SI 
  ( $04bd, $30b9 ), //                     kana_SU ? KATAKANA LETTER SU 
  ( $04be, $30bb ), //                     kana_SE ? KATAKANA LETTER SE 
  ( $04bf, $30bd ), //                     kana_SO ? KATAKANA LETTER SO 
  ( $04c0, $30bf ), //                     kana_TA ? KATAKANA LETTER TA 
  ( $04c1, $30c1 ), //                    kana_CHI ? KATAKANA LETTER TI 
  ( $04c2, $30c4 ), //                    kana_TSU ? KATAKANA LETTER TU
  ( $04c3, $30c6 ), //                     kana_TE ? KATAKANA LETTER TE 
  ( $04c4, $30c8 ), //                     kana_TO ? KATAKANA LETTER TO 
  ( $04c5, $30ca ), //                     kana_NA ? KATAKANA LETTER NA 
  ( $04c6, $30cb ), //                     kana_NI ? KATAKANA LETTER NI 
  ( $04c7, $30cc ), //                     kana_NU ? KATAKANA LETTER NU
  ( $04c8, $30cd ), //                     kana_NE ? KATAKANA LETTER NE 
  ( $04c9, $30ce ), //                     kana_NO ? KATAKANA LETTER NO 
  ( $04ca, $30cf ), //                     kana_HA ? KATAKANA LETTER HA 
  ( $04cb, $30d2 ), //                     kana_HI ? KATAKANA LETTER HI 
  ( $04cc, $30d5 ), //                     kana_FU ? KATAKANA LETTER HU 
  ( $04cd, $30d8 ), //                     kana_HE ? KATAKANA LETTER HE 
  ( $04ce, $30db ), //                     kana_HO ? KATAKANA LETTER HO 
  ( $04cf, $30de ), //                     kana_MA ? KATAKANA LETTER MA 
  ( $04d0, $30df ), //                     kana_MI ? KATAKANA LETTER MI 
  ( $04d1, $30e0 ), //                     kana_MU ? KATAKANA LETTER MU 
  ( $04d2, $30e1 ), //                     kana_ME ? KATAKANA LETTER ME 
  ( $04d3, $30e2 ), //                     kana_MO ? KATAKANA LETTER MO 
  ( $04d4, $30e4 ), //                     kana_YA ? KATAKANA LETTER YA 
  ( $04d5, $30e6 ), //                     kana_YU ? KATAKANA LETTER YU 
  ( $04d6, $30e8 ), //                     kana_YO ? KATAKANA LETTER YO 
  ( $04d7, $30e9 ), //                     kana_RA ? KATAKANA LETTER RA 
  ( $04d8, $30ea ), //                     kana_RI ? KATAKANA LETTER RI 
  ( $04d9, $30eb ), //                     kana_RU ? KATAKANA LETTER RU 
  ( $04da, $30ec ), //                     kana_RE ? KATAKANA LETTER RE 
  ( $04db, $30ed ), //                     kana_RO ? KATAKANA LETTER RO 
  ( $04dc, $30ef ), //                     kana_WA ? KATAKANA LETTER WA 
  ( $04dd, $30f3 ), //                      kana_N ? KATAKANA LETTER N 
  ( $04de, $309b ), //                 voicedsound ? KATAKANA-HIRAGANA VOICED SOUND MARK 
  ( $04df, $309c ), //             semivoicedsound ? KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK 
  ( $05ac, $060c ), //                Arabic_comma ? ARABIC COMMA 
  ( $05bb, $061b ), //            Arabic_semicolon ? ARABIC SEMICOLON 
  ( $05bf, $061f ), //        Arabic_question_mark ? ARABIC QUESTION MARK 
  ( $05c1, $0621 ), //                Arabic_hamza ? ARABIC LETTER HAMZA 
  ( $05c2, $0622 ), //          Arabic_maddaonalef ? ARABIC LETTER ALEF WITH MADDA ABOVE 
  ( $05c3, $0623 ), //          Arabic_hamzaonalef ? ARABIC LETTER ALEF WITH HAMZA ABOVE 
  ( $05c4, $0624 ), //           Arabic_hamzaonwaw ? ARABIC LETTER WAW WITH HAMZA ABOVE
  ( $05c5, $0625 ), //       Arabic_hamzaunderalef ? ARABIC LETTER ALEF WITH HAMZA BELOW 
  ( $05c6, $0626 ), //           Arabic_hamzaonyeh ? ARABIC LETTER YEH WITH HAMZA ABOVE 
  ( $05c7, $0627 ), //                 Arabic_alef ? ARABIC LETTER ALEF 
  ( $05c8, $0628 ), //                  Arabic_beh ? ARABIC LETTER BEH 
  ( $05c9, $0629 ), //           Arabic_tehmarbuta ? ARABIC LETTER TEH MARBUTA
  ( $05ca, $062a ), //                  Arabic_teh ? ARABIC LETTER TEH 
  ( $05cb, $062b ), //                 Arabic_theh ? ARABIC LETTER THEH 
  ( $05cc, $062c ), //                 Arabic_jeem ? ARABIC LETTER JEEM 
  ( $05cd, $062d ), //                  Arabic_hah ? ARABIC LETTER HAH 
  ( $05ce, $062e ), //                 Arabic_khah ? ARABIC LETTER KHAH 
  ( $05cf, $062f ), //                  Arabic_dal ? ARABIC LETTER DAL 
  ( $05d0, $0630 ), //                 Arabic_thal ? ARABIC LETTER THAL 
  ( $05d1, $0631 ), //                   Arabic_ra ? ARABIC LETTER REH 
  ( $05d2, $0632 ), //                 Arabic_zain ? ARABIC LETTER ZAIN 
  ( $05d3, $0633 ), //                 Arabic_seen ? ARABIC LETTER SEEN 
  ( $05d4, $0634 ), //                Arabic_sheen ? ARABIC LETTER SHEEN 
  ( $05d5, $0635 ), //                  Arabic_sad ? ARABIC LETTER SAD 
  ( $05d6, $0636 ), //                  Arabic_dad ? ARABIC LETTER DAD 
  ( $05d7, $0637 ), //                  Arabic_tah ? ARABIC LETTER TAH 
  ( $05d8, $0638 ), //                  Arabic_zah ? ARABIC LETTER ZAH 
  ( $05d9, $0639 ), //                  Arabic_ain ? ARABIC LETTER AIN 
  ( $05da, $063a ), //                Arabic_ghain ? ARABIC LETTER GHAIN 
  ( $05e0, $0640 ), //              Arabic_tatweel ? ARABIC TATWEEL 
  ( $05e1, $0641 ), //                  Arabic_feh ? ARABIC LETTER FEH 
  ( $05e2, $0642 ), //                  Arabic_qaf ? ARABIC LETTER QAF 
  ( $05e3, $0643 ), //                  Arabic_kaf ? ARABIC LETTER KAF 
  ( $05e4, $0644 ), //                  Arabic_lam ? ARABIC LETTER LAM 
  ( $05e5, $0645 ), //                 Arabic_meem ? ARABIC LETTER MEEM 
  ( $05e6, $0646 ), //                 Arabic_noon ? ARABIC LETTER NOON 
  ( $05e7, $0647 ), //                   Arabic_ha ? ARABIC LETTER HEH 
  ( $05e8, $0648 ), //                  Arabic_waw ? ARABIC LETTER WAW 
  ( $05e9, $0649 ), //          Arabic_alefmaksura ? ARABIC LETTER ALEF MAKSURA 
  ( $05ea, $064a ), //                  Arabic_yeh ? ARABIC LETTER YEH 
  ( $05eb, $064b ), //             Arabic_fathatan ? ARABIC FATHATAN 
  ( $05ec, $064c ), //             Arabic_dammatan ? ARABIC DAMMATAN 
  ( $05ed, $064d ), //             Arabic_kasratan ? ARABIC KASRATAN
  ( $05ee, $064e ), //                Arabic_fatha ? ARABIC FATHA 
  ( $05ef, $064f ), //                Arabic_damma ? ARABIC DAMMA 
  ( $05f0, $0650 ), //                Arabic_kasra ? ARABIC KASRA 
  ( $05f1, $0651 ), //               Arabic_shadda ? ARABIC SHADDA 
  ( $05f2, $0652 ), //                Arabic_sukun ? ARABIC SUKUN
  ( $06a1, $0452 ), //                 Serbian_dje ? CYRILLIC SMALL LETTER DJE 
  ( $06a2, $0453 ), //               Macedonia_gje ? CYRILLIC SMALL LETTER GJE 
  ( $06a3, $0451 ), //                 Cyrillic_io ? CYRILLIC SMALL LETTER IO 
  ( $06a4, $0454 ), //                Ukrainian_ie ? CYRILLIC SMALL LETTER UKRAINIAN IE 
  ( $06a5, $0455 ), //               Macedonia_dse ? CYRILLIC SMALL LETTER DZE 
  ( $06a6, $0456 ), //                 Ukrainian_i ? CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I 
  ( $06a7, $0457 ), //                Ukrainian_yi ? CYRILLIC SMALL LETTER YI 
  ( $06a8, $0458 ), //                 Cyrillic_je ? CYRILLIC SMALL LETTER JE 
  ( $06a9, $0459 ), //                Cyrillic_lje ? CYRILLIC SMALL LETTER LJE 
  ( $06aa, $045a ), //                Cyrillic_nje ? CYRILLIC SMALL LETTER NJE 
  ( $06ab, $045b ), //                Serbian_tshe ? CYRILLIC SMALL LETTER TSHE 
  ( $06ac, $045c ), //               Macedonia_kje ? CYRILLIC SMALL LETTER KJE 
  ( $06ae, $045e ), //         Byelorussian_shortu ? CYRILLIC SMALL LETTER SHORT U 
  ( $06af, $045f ), //               Cyrillic_dzhe ? CYRILLIC SMALL LETTER DZHE 
  ( $06b0, $2116 ), //                  numerosign ? NUMERO SIGN 
  ( $06b1, $0402 ), //                 Serbian_DJE ? CYRILLIC CAPITAL LETTER DJE 
  ( $06b2, $0403 ), //               Macedonia_GJE ? CYRILLIC CAPITAL LETTER GJE 
  ( $06b3, $0401 ), //                 Cyrillic_IO ? CYRILLIC CAPITAL LETTER IO 
  ( $06b4, $0404 ), //                Ukrainian_IE ? CYRILLIC CAPITAL LETTER UKRAINIAN IE 
  ( $06b5, $0405 ), //               Macedonia_DSE ? CYRILLIC CAPITAL LETTER DZE 
  ( $06b6, $0406 ), //                 Ukrainian_I ? CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I 
  ( $06b7, $0407 ), //                Ukrainian_YI ? CYRILLIC CAPITAL LETTER YI 
  ( $06b8, $0408 ), //                 Cyrillic_JE ? CYRILLIC CAPITAL LETTER JE 
  ( $06b9, $0409 ), //                Cyrillic_LJE ? CYRILLIC CAPITAL LETTER LJE 
  ( $06ba, $040a ), //                Cyrillic_NJE ? CYRILLIC CAPITAL LETTER NJE 
  ( $06bb, $040b ), //                Serbian_TSHE ? CYRILLIC CAPITAL LETTER TSHE 
  ( $06bc, $040c ), //               Macedonia_KJE ? CYRILLIC CAPITAL LETTER KJE 
  ( $06be, $040e ), //         Byelorussian_SHORTU ? CYRILLIC CAPITAL LETTER SHORT U 
  ( $06bf, $040f ), //               Cyrillic_DZHE ? CYRILLIC CAPITAL LETTER DZHE 
  ( $06c0, $044e ), //                 Cyrillic_yu ? CYRILLIC SMALL LETTER YU 
  ( $06c1, $0430 ), //                  Cyrillic_a ? CYRILLIC SMALL LETTER A
  ( $06c2, $0431 ), //                 Cyrillic_be ? CYRILLIC SMALL LETTER BE 
  ( $06c3, $0446 ), //                Cyrillic_tse ? CYRILLIC SMALL LETTER TSE 
  ( $06c4, $0434 ), //                 Cyrillic_de ? CYRILLIC SMALL LETTER DE 
  ( $06c5, $0435 ), //                 Cyrillic_ie ? CYRILLIC SMALL LETTER IE 
  ( $06c6, $0444 ), //                 Cyrillic_ef ? CYRILLIC SMALL LETTER EF
  ( $06c7, $0433 ), //                Cyrillic_ghe ? CYRILLIC SMALL LETTER GHE 
  ( $06c8, $0445 ), //                 Cyrillic_ha ? CYRILLIC SMALL LETTER HA 
  ( $06c9, $0438 ), //                  Cyrillic_i ? CYRILLIC SMALL LETTER I 
  ( $06ca, $0439 ), //             Cyrillic_shorti ? CYRILLIC SMALL LETTER SHORT I 
  ( $06cb, $043a ), //                 Cyrillic_ka ? CYRILLIC SMALL LETTER KA 
  ( $06cc, $043b ), //                 Cyrillic_el ? CYRILLIC SMALL LETTER EL 
  ( $06cd, $043c ), //                 Cyrillic_em ? CYRILLIC SMALL LETTER EM 
  ( $06ce, $043d ), //                 Cyrillic_en ? CYRILLIC SMALL LETTER EN 
  ( $06cf, $043e ), //                  Cyrillic_o ? CYRILLIC SMALL LETTER O 
  ( $06d0, $043f ), //                 Cyrillic_pe ? CYRILLIC SMALL LETTER PE 
  ( $06d1, $044f ), //                 Cyrillic_ya ? CYRILLIC SMALL LETTER YA 
  ( $06d2, $0440 ), //                 Cyrillic_er ? CYRILLIC SMALL LETTER ER 
  ( $06d3, $0441 ), //                 Cyrillic_es ? CYRILLIC SMALL LETTER ES 
  ( $06d4, $0442 ), //                 Cyrillic_te ? CYRILLIC SMALL LETTER TE 
  ( $06d5, $0443 ), //                  Cyrillic_u ? CYRILLIC SMALL LETTER U 
  ( $06d6, $0436 ), //                Cyrillic_zhe ? CYRILLIC SMALL LETTER ZHE 
  ( $06d7, $0432 ), //                 Cyrillic_ve ? CYRILLIC SMALL LETTER VE 
  ( $06d8, $044c ), //           Cyrillic_softsign ? CYRILLIC SMALL LETTER SOFT SIGN 
  ( $06d9, $044b ), //               Cyrillic_yeru ? CYRILLIC SMALL LETTER YERU 
  ( $06da, $0437 ), //                 Cyrillic_ze ? CYRILLIC SMALL LETTER ZE 
  ( $06db, $0448 ), //                Cyrillic_sha ? CYRILLIC SMALL LETTER SHA 
  ( $06dc, $044d ), //                  Cyrillic_e ? CYRILLIC SMALL LETTER E 
  ( $06dd, $0449 ), //              Cyrillic_shcha ? CYRILLIC SMALL LETTER SHCHA 
  ( $06de, $0447 ), //                Cyrillic_che ? CYRILLIC SMALL LETTER CHE 
  ( $06df, $044a ), //           Cyrillic_hardsign ? CYRILLIC SMALL LETTER HARD SIGN 
  ( $06e0, $042e ), //                 Cyrillic_YU ? CYRILLIC CAPITAL LETTER YU 
  ( $06e1, $0410 ), //                  Cyrillic_A ? CYRILLIC CAPITAL LETTER A 
  ( $06e2, $0411 ), //                 Cyrillic_BE ? CYRILLIC CAPITAL LETTER BE 
  ( $06e3, $0426 ), //                Cyrillic_TSE ? CYRILLIC CAPITAL LETTER TSE 
  ( $06e4, $0414 ), //                 Cyrillic_DE ? CYRILLIC CAPITAL LETTER DE 
  ( $06e5, $0415 ), //                 Cyrillic_IE ? CYRILLIC CAPITAL LETTER IE
  ( $06e6, $0424 ), //                 Cyrillic_EF ? CYRILLIC CAPITAL LETTER EF 
  ( $06e7, $0413 ), //                Cyrillic_GHE ? CYRILLIC CAPITAL LETTER GHE 
  ( $06e8, $0425 ), //                 Cyrillic_HA ? CYRILLIC CAPITAL LETTER HA 
  ( $06e9, $0418 ), //                  Cyrillic_I ? CYRILLIC CAPITAL LETTER I 
  ( $06ea, $0419 ), //             Cyrillic_SHORTI ? CYRILLIC CAPITAL LETTER SHORT I
  ( $06eb, $041a ), //                 Cyrillic_KA ? CYRILLIC CAPITAL LETTER KA 
  ( $06ec, $041b ), //                 Cyrillic_EL ? CYRILLIC CAPITAL LETTER EL 
  ( $06ed, $041c ), //                 Cyrillic_EM ? CYRILLIC CAPITAL LETTER EM 
  ( $06ee, $041d ), //                 Cyrillic_EN ? CYRILLIC CAPITAL LETTER EN 
  ( $06ef, $041e ), //                  Cyrillic_O ? CYRILLIC CAPITAL LETTER O 
  ( $06f0, $041f ), //                 Cyrillic_PE ? CYRILLIC CAPITAL LETTER PE 
  ( $06f1, $042f ), //                 Cyrillic_YA ? CYRILLIC CAPITAL LETTER YA 
  ( $06f2, $0420 ), //                 Cyrillic_ER ? CYRILLIC CAPITAL LETTER ER 
  ( $06f3, $0421 ), //                 Cyrillic_ES ? CYRILLIC CAPITAL LETTER ES 
  ( $06f4, $0422 ), //                 Cyrillic_TE ? CYRILLIC CAPITAL LETTER TE 
  ( $06f5, $0423 ), //                  Cyrillic_U ? CYRILLIC CAPITAL LETTER U 
  ( $06f6, $0416 ), //                Cyrillic_ZHE ? CYRILLIC CAPITAL LETTER ZHE 
  ( $06f7, $0412 ), //                 Cyrillic_VE ? CYRILLIC CAPITAL LETTER VE 
  ( $06f8, $042c ), //           Cyrillic_SOFTSIGN ? CYRILLIC CAPITAL LETTER SOFT SIGN 
  ( $06f9, $042b ), //               Cyrillic_YERU ? CYRILLIC CAPITAL LETTER YERU 
  ( $06fa, $0417 ), //                 Cyrillic_ZE ? CYRILLIC CAPITAL LETTER ZE 
  ( $06fb, $0428 ), //                Cyrillic_SHA ? CYRILLIC CAPITAL LETTER SHA 
  ( $06fc, $042d ), //                  Cyrillic_E ? CYRILLIC CAPITAL LETTER E 
  ( $06fd, $0429 ), //              Cyrillic_SHCHA ? CYRILLIC CAPITAL LETTER SHCHA 
  ( $06fe, $0427 ), //                Cyrillic_CHE ? CYRILLIC CAPITAL LETTER CHE 
  ( $06ff, $042a ), //           Cyrillic_HARDSIGN ? CYRILLIC CAPITAL LETTER HARD SIGN 
  ( $07a1, $0386 ), //           Greek_ALPHAaccent ? GREEK CAPITAL LETTER ALPHA WITH TONOS 
  ( $07a2, $0388 ), //         Greek_EPSILONaccent ? GREEK CAPITAL LETTER EPSILON WITH TONOS 
  ( $07a3, $0389 ), //             Greek_ETAaccent ? GREEK CAPITAL LETTER ETA WITH TONOS 
  ( $07a4, $038a ), //            Greek_IOTAaccent ? GREEK CAPITAL LETTER IOTA WITH TONOS 
  ( $07a5, $03aa ), //         Greek_IOTAdiaeresis ? GREEK CAPITAL LETTER IOTA WITH DIALYTIKA 
  ( $07a7, $038c ), //         Greek_OMICRONaccent ? GREEK CAPITAL LETTER OMICRON WITH TONOS 
  ( $07a8, $038e ), //         Greek_UPSILONaccent ? GREEK CAPITAL LETTER UPSILON WITH TONOS 
  ( $07a9, $03ab ), //       Greek_UPSILONdieresis ? GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA 
  ( $07ab, $038f ), //           Greek_OMEGAaccent ? GREEK CAPITAL LETTER OMEGA WITH TONOS 
  ( $07ae, $0385 ), //        Greek_accentdieresis ? GREEK DIALYTIKA TONOS
  ( $07af, $2015 ), //              Greek_horizbar ? HORIZONTAL BAR 
  ( $07b1, $03ac ), //           Greek_alphaaccent ? GREEK SMALL LETTER ALPHA WITH TONOS 
  ( $07b2, $03ad ), //         Greek_epsilonaccent ? GREEK SMALL LETTER EPSILON WITH TONOS 
  ( $07b3, $03ae ), //             Greek_etaaccent ? GREEK SMALL LETTER ETA WITH TONOS 
  ( $07b4, $03af ), //            Greek_iotaaccent ? GREEK SMALL LETTER IOTA WITH TONOS
  ( $07b5, $03ca ), //          Greek_iotadieresis ? GREEK SMALL LETTER IOTA WITH DIALYTIKA 
  ( $07b6, $0390 ), //    Greek_iotaaccentdieresis ? GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS 
  ( $07b7, $03cc ), //         Greek_omicronaccent ? GREEK SMALL LETTER OMICRON WITH TONOS 
  ( $07b8, $03cd ), //         Greek_upsilonaccent ? GREEK SMALL LETTER UPSILON WITH TONOS 
  ( $07b9, $03cb ), //       Greek_upsilondieresis ? GREEK SMALL LETTER UPSILON WITH DIALYTIKA 
  ( $07ba, $03b0 ), // Greek_upsilonaccentdieresis ? GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS 
  ( $07bb, $03ce ), //           Greek_omegaaccent ? GREEK SMALL LETTER OMEGA WITH TONOS 
  ( $07c1, $0391 ), //                 Greek_ALPHA ? GREEK CAPITAL LETTER ALPHA 
  ( $07c2, $0392 ), //                  Greek_BETA ? GREEK CAPITAL LETTER BETA 
  ( $07c3, $0393 ), //                 Greek_GAMMA G GREEK CAPITAL LETTER GAMMA 
  ( $07c4, $0394 ), //                 Greek_DELTA ? GREEK CAPITAL LETTER DELTA 
  ( $07c5, $0395 ), //               Greek_EPSILON ? GREEK CAPITAL LETTER EPSILON 
  ( $07c6, $0396 ), //                  Greek_ZETA ? GREEK CAPITAL LETTER ZETA 
  ( $07c7, $0397 ), //                   Greek_ETA ? GREEK CAPITAL LETTER ETA 
  ( $07c8, $0398 ), //                 Greek_THETA T GREEK CAPITAL LETTER THETA 
  ( $07c9, $0399 ), //                  Greek_IOTA ? GREEK CAPITAL LETTER IOTA 
  ( $07ca, $039a ), //                 Greek_KAPPA ? GREEK CAPITAL LETTER KAPPA 
  ( $07cb, $039b ), //                Greek_LAMBDA ? GREEK CAPITAL LETTER LAMDA 
  ( $07cc, $039c ), //                    Greek_MU ? GREEK CAPITAL LETTER MU 
  ( $07cd, $039d ), //                    Greek_NU ? GREEK CAPITAL LETTER NU 
  ( $07ce, $039e ), //                    Greek_XI ? GREEK CAPITAL LETTER XI 
  ( $07cf, $039f ), //               Greek_OMICRON ? GREEK CAPITAL LETTER OMICRON 
  ( $07d0, $03a0 ), //                    Greek_PI ? GREEK CAPITAL LETTER PI 
  ( $07d1, $03a1 ), //                   Greek_RHO ? GREEK CAPITAL LETTER RHO 
  ( $07d2, $03a3 ), //                 Greek_SIGMA S GREEK CAPITAL LETTER SIGMA 
  ( $07d4, $03a4 ), //                   Greek_TAU ? GREEK CAPITAL LETTER TAU 
  ( $07d5, $03a5 ), //               Greek_UPSILON ? GREEK CAPITAL LETTER UPSILON 
  ( $07d6, $03a6 ), //                   Greek_PHI F GREEK CAPITAL LETTER PHI 
  ( $07d7, $03a7 ), //                   Greek_CHI ? GREEK CAPITAL LETTER CHI 
  ( $07d8, $03a8 ), //                   Greek_PSI ? GREEK CAPITAL LETTER PSI 
  ( $07d9, $03a9 ), //                 Greek_OMEGA O GREEK CAPITAL LETTER OMEGA
  ( $07e1, $03b1 ), //                 Greek_alpha a GREEK SMALL LETTER ALPHA 
  ( $07e2, $03b2 ), //                  Greek_beta ß GREEK SMALL LETTER BETA 
  ( $07e3, $03b3 ), //                 Greek_gamma ? GREEK SMALL LETTER GAMMA 
  ( $07e4, $03b4 ), //                 Greek_delta d GREEK SMALL LETTER DELTA 
  ( $07e5, $03b5 ), //               Greek_epsilon e GREEK SMALL LETTER EPSILON
  ( $07e6, $03b6 ), //                  Greek_zeta ? GREEK SMALL LETTER ZETA 
  ( $07e7, $03b7 ), //                   Greek_eta ? GREEK SMALL LETTER ETA 
  ( $07e8, $03b8 ), //                 Greek_theta ? GREEK SMALL LETTER THETA 
  ( $07e9, $03b9 ), //                  Greek_iota ? GREEK SMALL LETTER IOTA 
  ( $07ea, $03ba ), //                 Greek_kappa ? GREEK SMALL LETTER KAPPA 
  ( $07eb, $03bb ), //                Greek_lambda ? GREEK SMALL LETTER LAMDA 
  ( $07ec, $03bc ), //                    Greek_mu µ GREEK SMALL LETTER MU 
  ( $07ed, $03bd ), //                    Greek_nu ? GREEK SMALL LETTER NU 
  ( $07ee, $03be ), //                    Greek_xi ? GREEK SMALL LETTER XI 
  ( $07ef, $03bf ), //               Greek_omicron ? GREEK SMALL LETTER OMICRON 
  ( $07f0, $03c0 ), //                    Greek_pi p GREEK SMALL LETTER PI 
  ( $07f1, $03c1 ), //                   Greek_rho ? GREEK SMALL LETTER RHO 
  ( $07f2, $03c3 ), //                 Greek_sigma s GREEK SMALL LETTER SIGMA 
  ( $07f3, $03c2 ), //       Greek_finalsmallsigma ? GREEK SMALL LETTER FINAL SIGMA 
  ( $07f4, $03c4 ), //                   Greek_tau t GREEK SMALL LETTER TAU 
  ( $07f5, $03c5 ), //               Greek_upsilon ? GREEK SMALL LETTER UPSILON 
  ( $07f6, $03c6 ), //                   Greek_phi f GREEK SMALL LETTER PHI 
  ( $07f7, $03c7 ), //                   Greek_chi ? GREEK SMALL LETTER CHI 
  ( $07f8, $03c8 ), //                   Greek_psi ? GREEK SMALL LETTER PSI 
  ( $07f9, $03c9 ), //                 Greek_omega ? GREEK SMALL LETTER OMEGA 
  ( $08a1, $23b7 ), //                 leftradical ? ??? 
  ( $08a2, $250c ), //              topleftradical + BOX DRAWINGS LIGHT DOWN AND RIGHT 
  ( $08a3, $2500 ), //              horizconnector - BOX DRAWINGS LIGHT HORIZONTAL 
  ( $08a4, $2320 ), //                 topintegral ( TOP HALF INTEGRAL 
  ( $08a5, $2321 ), //                 botintegral ) BOTTOM HALF INTEGRAL 
  ( $08a6, $2502 ), //               vertconnector ¦ BOX DRAWINGS LIGHT VERTICAL 
  ( $08a7, $23a1 ), //            topleftsqbracket ? ??? 
  ( $08a8, $23a3 ), //            botleftsqbracket ? ??? 
  ( $08a9, $23a4 ), //           toprightsqbracket ? ??? 
  ( $08aa, $23a6 ), //           botrightsqbracket ? ??? 
  ( $08ab, $239b ), //               topleftparens ? ???
  ( $08ac, $239d ), //               botleftparens ? ??? 
  ( $08ad, $239e ), //              toprightparens ? ??? 
  ( $08ae, $23a0 ), //              botrightparens ? ??? 
  ( $08af, $23a8 ), //        leftmiddlecurlybrace ? ??? 
  ( $08b0, $23ac ), //       rightmiddlecurlybrace ? ???
  ( $08bc, $2264 ), //               lessthanequal = LESS-THAN OR EQUAL TO 
  ( $08bd, $2260 ), //                    notequal ? NOT EQUAL TO 
  ( $08be, $2265 ), //            greaterthanequal = GREATER-THAN OR EQUAL TO 
  ( $08bf, $222b ), //                    integral ? INTEGRAL 
  ( $08c0, $2234 ), //                   therefore ? THEREFORE 
  ( $08c1, $221d ), //                   variation ? PROPORTIONAL TO 
  ( $08c2, $221e ), //                    infinity 8 INFINITY 
  ( $08c5, $2207 ), //                       nabla ? NABLA 
  ( $08c8, $223c ), //                 approximate ~ TILDE OPERATOR 
  ( $08c9, $2243 ), //                similarequal ? ASYMPTOTICALLY EQUAL TO 
  ( $08cd, $21d4 ), //                    ifonlyif ? LEFT RIGHT DOUBLE ARROW 
  ( $08ce, $21d2 ), //                     implies ? RIGHTWARDS DOUBLE ARROW 
  ( $08cf, $2261 ), //                   identical = IDENTICAL TO 
  ( $08d6, $221a ), //                     radical v SQUARE ROOT 
  ( $08da, $2282 ), //                  includedin ? SUBSET OF 
  ( $08db, $2283 ), //                    includes ? SUPERSET OF 
  ( $08dc, $2229 ), //                intersection n INTERSECTION 
  ( $08dd, $222a ), //                       union ? UNION 
  ( $08de, $2227 ), //                  logicaland ? LOGICAL AND 
  ( $08df, $2228 ), //                   logicalor ? LOGICAL OR 
  ( $08ef, $2202 ), //           partialderivative ? PARTIAL DIFFERENTIAL 
  ( $08f6, $0192 ), //                    function ƒ LATIN SMALL LETTER F WITH HOOK 
  ( $08fb, $2190 ), //                   leftarrow ? LEFTWARDS ARROW 
  ( $08fc, $2191 ), //                     uparrow ? UPWARDS ARROW 
  ( $08fd, $2192 ), //                  rightarrow ? RIGHTWARDS ARROW 
  ( $08fe, $2193 ), //                   downarrow ? DOWNWARDS ARROW 
  ( $09e0, $25c6 ), //                soliddiamond ? BLACK DIAMOND 
  ( $09e1, $2592 ), //                checkerboard ¦ MEDIUM SHADE 
  ( $09e2, $2409 ), //                          ht ? SYMBOL FOR HORIZONTAL TABULATION 
  ( $09e3, $240c ), //                          ff ? SYMBOL FOR FORM FEED 
  ( $09e4, $240d ), //                          cr ? SYMBOL FOR CARRIAGE RETURN
  ( $09e5, $240a ), //                          lf ? SYMBOL FOR LINE FEED 
  ( $09e8, $2424 ), //                          nl ? SYMBOL FOR NEWLINE 
  ( $09e9, $240b ), //                          vt ? SYMBOL FOR VERTICAL TABULATION 
  ( $09ea, $2518 ), //              lowrightcorner + BOX DRAWINGS LIGHT UP AND LEFT 
  ( $09eb, $2510 ), //               uprightcorner + BOX DRAWINGS LIGHT DOWN AND LEFT
  ( $09ec, $250c ), //                upleftcorner + BOX DRAWINGS LIGHT DOWN AND RIGHT 
  ( $09ed, $2514 ), //               lowleftcorner + BOX DRAWINGS LIGHT UP AND RIGHT 
  ( $09ee, $253c ), //               crossinglines + BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL 
  ( $09ef, $23ba ), //              horizlinescan1 ? HORIZONTAL SCAN LINE-1 (Unicode 3.2 draft) 
  ( $09f0, $23bb ), //              horizlinescan3 ? HORIZONTAL SCAN LINE-3 (Unicode 3.2 draft) 
  ( $09f1, $2500 ), //              horizlinescan5 - BOX DRAWINGS LIGHT HORIZONTAL 
  ( $09f2, $23bc ), //              horizlinescan7 ? HORIZONTAL SCAN LINE-7 (Unicode 3.2 draft) 
  ( $09f3, $23bd ), //              horizlinescan9 ? HORIZONTAL SCAN LINE-9 (Unicode 3.2 draft) 
  ( $09f4, $251c ), //                       leftt + BOX DRAWINGS LIGHT VERTICAL AND RIGHT 
  ( $09f5, $2524 ), //                      rightt ¦ BOX DRAWINGS LIGHT VERTICAL AND LEFT 
  ( $09f6, $2534 ), //                        bott - BOX DRAWINGS LIGHT UP AND HORIZONTAL 
  ( $09f7, $252c ), //                        topt - BOX DRAWINGS LIGHT DOWN AND HORIZONTAL 
  ( $09f8, $2502 ), //                     vertbar ¦ BOX DRAWINGS LIGHT VERTICAL 
  ( $0aa1, $2003 ), //                     emspace   EM SPACE 
  ( $0aa2, $2002 ), //                     enspace   EN SPACE 
  ( $0aa3, $2004 ), //                    em3space   THREE-PER-EM SPACE 
  ( $0aa4, $2005 ), //                    em4space   FOUR-PER-EM SPACE 
  ( $0aa5, $2007 ), //                  digitspace ? FIGURE SPACE 
  ( $0aa6, $2008 ), //                  punctspace ? PUNCTUATION SPACE 
  ( $0aa7, $2009 ), //                   thinspace ? THIN SPACE 
  ( $0aa8, $200a ), //                   hairspace ? HAIR SPACE 
  ( $0aa9, $2014 ), //                      emdash — EM DASH 
  ( $0aaa, $2013 ), //                      endash – EN DASH 
  ( $0aae, $2026 ), //                    ellipsis … HORIZONTAL ELLIPSIS 
  ( $0aaf, $2025 ), //             doubbaselinedot ? TWO DOT LEADER 
  ( $0ab0, $2153 ), //                    onethird ? VULGAR FRACTION ONE THIRD 
  ( $0ab1, $2154 ), //                   twothirds ? VULGAR FRACTION TWO THIRDS 
  ( $0ab2, $2155 ), //                    onefifth ? VULGAR FRACTION ONE FIFTH 
  ( $0ab3, $2156 ), //                   twofifths ? VULGAR FRACTION TWO FIFTHS 
  ( $0ab4, $2157 ), //                 threefifths ? VULGAR FRACTION THREE FIFTHS 
  ( $0ab5, $2158 ), //                  fourfifths ? VULGAR FRACTION FOUR FIFTHS
  ( $0ab6, $2159 ), //                    onesixth ? VULGAR FRACTION ONE SIXTH 
  ( $0ab7, $215a ), //                  fivesixths ? VULGAR FRACTION FIVE SIXTHS 
  ( $0ab8, $2105 ), //                      careof ? CARE OF 
  ( $0abb, $2012 ), //                     figdash ? FIGURE DASH 
  ( $0abc, $2329 ), //            leftanglebracket < LEFT-POINTING ANGLE BRACKET
  ( $0abe, $232a ), //           rightanglebracket > RIGHT-POINTING ANGLE BRACKET 
  ( $0ac3, $215b ), //                   oneeighth ? VULGAR FRACTION ONE EIGHTH 
  ( $0ac4, $215c ), //                threeeighths ? VULGAR FRACTION THREE EIGHTHS 
  ( $0ac5, $215d ), //                 fiveeighths ? VULGAR FRACTION FIVE EIGHTHS 
  ( $0ac6, $215e ), //                seveneighths ? VULGAR FRACTION SEVEN EIGHTHS 
  ( $0ac9, $2122 ), //                   trademark ™ TRADE MARK SIGN 
  ( $0aca, $2613 ), //               signaturemark ? SALTIRE 
  ( $0acc, $25c1 ), //            leftopentriangle ? WHITE LEFT-POINTING TRIANGLE 
  ( $0acd, $25b7 ), //           rightopentriangle ? WHITE RIGHT-POINTING TRIANGLE 
  ( $0ace, $25cb ), //                emopencircle ? WHITE CIRCLE 
  ( $0acf, $25af ), //             emopenrectangle ? WHITE VERTICAL RECTANGLE 
  ( $0ad0, $2018 ), //         leftsinglequotemark ‘ LEFT SINGLE QUOTATION MARK 
  ( $0ad1, $2019 ), //        rightsinglequotemark ’ RIGHT SINGLE QUOTATION MARK 
  ( $0ad2, $201c ), //         leftdoublequotemark “ LEFT DOUBLE QUOTATION MARK 
  ( $0ad3, $201d ), //        rightdoublequotemark ” RIGHT DOUBLE QUOTATION MARK 
  ( $0ad4, $211e ), //                prescription ? PRESCRIPTION TAKE 
  ( $0ad6, $2032 ), //                     minutes ' PRIME 
  ( $0ad7, $2033 ), //                     seconds ? DOUBLE PRIME 
  ( $0ad9, $271d ), //                  latincross ? LATIN CROSS 
  ( $0adb, $25ac ), //            filledrectbullet ? BLACK RECTANGLE 
  ( $0adc, $25c0 ), //         filledlefttribullet ? BLACK LEFT-POINTING TRIANGLE 
  ( $0add, $25b6 ), //        filledrighttribullet ? BLACK RIGHT-POINTING TRIANGLE 
  ( $0ade, $25cf ), //              emfilledcircle ? BLACK CIRCLE 
  ( $0adf, $25ae ), //                emfilledrect ? BLACK VERTICAL RECTANGLE 
  ( $0ae0, $25e6 ), //            enopencircbullet ? WHITE BULLET 
  ( $0ae1, $25ab ), //          enopensquarebullet ? WHITE SMALL SQUARE 
  ( $0ae2, $25ad ), //              openrectbullet ? WHITE RECTANGLE 
  ( $0ae3, $25b3 ), //             opentribulletup ? WHITE UP-POINTING TRIANGLE 
  ( $0ae4, $25bd ), //           opentribulletdown ? WHITE DOWN-POINTING TRIANGLE 
  ( $0ae5, $2606 ), //                    openstar ? WHITE STAR 
  ( $0ae6, $2022 ), //          enfilledcircbullet • BULLET
  ( $0ae7, $25aa ), //            enfilledsqbullet ? BLACK SMALL SQUARE 
  ( $0ae8, $25b2 ), //           filledtribulletup ? BLACK UP-POINTING TRIANGLE 
  ( $0ae9, $25bc ), //         filledtribulletdown ? BLACK DOWN-POINTING TRIANGLE 
  ( $0aea, $261c ), //                 leftpointer ? WHITE LEFT POINTING INDEX 
  ( $0aeb, $261e ), //                rightpointer ? WHITE RIGHT POINTING INDEX
  ( $0aec, $2663 ), //                        club ? BLACK CLUB SUIT 
  ( $0aed, $2666 ), //                     diamond ? BLACK DIAMOND SUIT 
  ( $0aee, $2665 ), //                       heart ? BLACK HEART SUIT 
  ( $0af0, $2720 ), //                maltesecross ? MALTESE CROSS 
  ( $0af1, $2020 ), //                      dagger † DAGGER 
  ( $0af2, $2021 ), //                doubledagger ‡ DOUBLE DAGGER 
  ( $0af3, $2713 ), //                   checkmark ? CHECK MARK 
  ( $0af4, $2717 ), //                 ballotcross ? BALLOT X 
  ( $0af5, $266f ), //                musicalsharp ? MUSIC SHARP SIGN 
  ( $0af6, $266d ), //                 musicalflat ? MUSIC FLAT SIGN 
  ( $0af7, $2642 ), //                  malesymbol ? MALE SIGN 
  ( $0af8, $2640 ), //                femalesymbol ? FEMALE SIGN 
  ( $0af9, $260e ), //                   telephone ? BLACK TELEPHONE 
  ( $0afa, $2315 ), //           telephonerecorder ? TELEPHONE RECORDER 
  ( $0afb, $2117 ), //         phonographcopyright ? SOUND RECORDING COPYRIGHT 
  ( $0afc, $2038 ), //                       caret ? CARET 
  ( $0afd, $201a ), //          singlelowquotemark ‚ SINGLE LOW-9 QUOTATION MARK 
  ( $0afe, $201e ), //          doublelowquotemark „ DOUBLE LOW-9 QUOTATION MARK 
  ( $0ba3, $003c ), //                   leftcaret < LESS-THAN SIGN 
  ( $0ba6, $003e ), //                  rightcaret > GREATER-THAN SIGN 
  ( $0ba8, $2228 ), //                   downcaret ? LOGICAL OR 
  ( $0ba9, $2227 ), //                     upcaret ? LOGICAL AND 
  ( $0bc0, $00af ), //                     overbar ¯ MACRON 
  ( $0bc2, $22a5 ), //                    downtack ? UP TACK 
  ( $0bc3, $2229 ), //                      upshoe n INTERSECTION 
  ( $0bc4, $230a ), //                   downstile ? LEFT FLOOR 
  ( $0bc6, $005f ), //                    underbar _ LOW LINE 
  ( $0bca, $2218 ), //                         jot ° RING OPERATOR 
  ( $0bcc, $2395 ), //                        quad ? APL FUNCTIONAL SYMBOL QUAD 
  ( $0bce, $22a4 ), //                      uptack ? DOWN TACK 
  ( $0bcf, $25cb ), //                      circle ? WHITE CIRCLE
  ( $0bd3, $2308 ), //                     upstile ? LEFT CEILING 
  ( $0bd6, $222a ), //                    downshoe ? UNION 
  ( $0bd8, $2283 ), //                   rightshoe ? SUPERSET OF 
  ( $0bda, $2282 ), //                    leftshoe ? SUBSET OF 
  ( $0bdc, $22a2 ), //                    lefttack ? RIGHT TACK
  ( $0bfc, $22a3 ), //                   righttack ? LEFT TACK 
  ( $0cdf, $2017 ), //        hebrew_doublelowline = DOUBLE LOW LINE 
  ( $0ce0, $05d0 ), //                hebrew_aleph ? HEBREW LETTER ALEF 
  ( $0ce1, $05d1 ), //                  hebrew_bet ? HEBREW LETTER BET 
  ( $0ce2, $05d2 ), //                hebrew_gimel ? HEBREW LETTER GIMEL 
  ( $0ce3, $05d3 ), //                hebrew_dalet ? HEBREW LETTER DALET 
  ( $0ce4, $05d4 ), //                   hebrew_he ? HEBREW LETTER HE 
  ( $0ce5, $05d5 ), //                  hebrew_waw ? HEBREW LETTER VAV 
  ( $0ce6, $05d6 ), //                 hebrew_zain ? HEBREW LETTER ZAYIN 
  ( $0ce7, $05d7 ), //                 hebrew_chet ? HEBREW LETTER HET 
  ( $0ce8, $05d8 ), //                  hebrew_tet ? HEBREW LETTER TET 
  ( $0ce9, $05d9 ), //                  hebrew_yod ? HEBREW LETTER YOD 
  ( $0cea, $05da ), //            hebrew_finalkaph ? HEBREW LETTER FINAL KAF 
  ( $0ceb, $05db ), //                 hebrew_kaph ? HEBREW LETTER KAF 
  ( $0cec, $05dc ), //                hebrew_lamed ? HEBREW LETTER LAMED 
  ( $0ced, $05dd ), //             hebrew_finalmem ? HEBREW LETTER FINAL MEM 
  ( $0cee, $05de ), //                  hebrew_mem ? HEBREW LETTER MEM 
  ( $0cef, $05df ), //             hebrew_finalnun ? HEBREW LETTER FINAL NUN 
  ( $0cf0, $05e0 ), //                  hebrew_nun ? HEBREW LETTER NUN 
  ( $0cf1, $05e1 ), //               hebrew_samech ? HEBREW LETTER SAMEKH 
  ( $0cf2, $05e2 ), //                 hebrew_ayin ? HEBREW LETTER AYIN 
  ( $0cf3, $05e3 ), //              hebrew_finalpe ? HEBREW LETTER FINAL PE 
  ( $0cf4, $05e4 ), //                   hebrew_pe ? HEBREW LETTER PE 
  ( $0cf5, $05e5 ), //            hebrew_finalzade ? HEBREW LETTER FINAL TSADI 
  ( $0cf6, $05e6 ), //                 hebrew_zade ? HEBREW LETTER TSADI 
  ( $0cf7, $05e7 ), //                 hebrew_qoph ? HEBREW LETTER QOF 
  ( $0cf8, $05e8 ), //                 hebrew_resh ? HEBREW LETTER RESH 
  ( $0cf9, $05e9 ), //                 hebrew_shin ? HEBREW LETTER SHIN 
  ( $0cfa, $05ea ), //                  hebrew_taw ? HEBREW LETTER TAV 
  ( $0da1, $0e01 ), //                  Thai_kokai ? THAI CHARACTER KO KAI 
  ( $0da2, $0e02 ), //                Thai_khokhai ? THAI CHARACTER KHO KHAI
  ( $0da3, $0e03 ), //               Thai_khokhuat ? THAI CHARACTER KHO KHUAT 
  ( $0da4, $0e04 ), //               Thai_khokhwai ? THAI CHARACTER KHO KHWAI 
  ( $0da5, $0e05 ), //                Thai_khokhon ? THAI CHARACTER KHO KHON 
  ( $0da6, $0e06 ), //             Thai_khorakhang ? THAI CHARACTER KHO RAKHANG 
  ( $0da7, $0e07 ), //                 Thai_ngongu ? THAI CHARACTER NGO NGU
  ( $0da8, $0e08 ), //                Thai_chochan ? THAI CHARACTER CHO CHAN 
  ( $0da9, $0e09 ), //               Thai_choching ? THAI CHARACTER CHO CHING 
  ( $0daa, $0e0a ), //               Thai_chochang ? THAI CHARACTER CHO CHANG 
  ( $0dab, $0e0b ), //                   Thai_soso ? THAI CHARACTER SO SO 
  ( $0dac, $0e0c ), //                Thai_chochoe ? THAI CHARACTER CHO CHOE 
  ( $0dad, $0e0d ), //                 Thai_yoying ? THAI CHARACTER YO YING 
  ( $0dae, $0e0e ), //                Thai_dochada ? THAI CHARACTER DO CHADA 
  ( $0daf, $0e0f ), //                Thai_topatak ? THAI CHARACTER TO PATAK 
  ( $0db0, $0e10 ), //                Thai_thothan ? THAI CHARACTER THO THAN 
  ( $0db1, $0e11 ), //          Thai_thonangmontho ? THAI CHARACTER THO NANGMONTHO 
  ( $0db2, $0e12 ), //             Thai_thophuthao ? THAI CHARACTER THO PHUTHAO 
  ( $0db3, $0e13 ), //                  Thai_nonen ? THAI CHARACTER NO NEN 
  ( $0db4, $0e14 ), //                  Thai_dodek ? THAI CHARACTER DO DEK 
  ( $0db5, $0e15 ), //                  Thai_totao ? THAI CHARACTER TO TAO 
  ( $0db6, $0e16 ), //               Thai_thothung ? THAI CHARACTER THO THUNG 
  ( $0db7, $0e17 ), //              Thai_thothahan ? THAI CHARACTER THO THAHAN 
  ( $0db8, $0e18 ), //               Thai_thothong ? THAI CHARACTER THO THONG 
  ( $0db9, $0e19 ), //                   Thai_nonu ? THAI CHARACTER NO NU 
  ( $0dba, $0e1a ), //               Thai_bobaimai ? THAI CHARACTER BO BAIMAI 
  ( $0dbb, $0e1b ), //                  Thai_popla ? THAI CHARACTER PO PLA 
  ( $0dbc, $0e1c ), //               Thai_phophung ? THAI CHARACTER PHO PHUNG 
  ( $0dbd, $0e1d ), //                   Thai_fofa ? THAI CHARACTER FO FA 
  ( $0dbe, $0e1e ), //                Thai_phophan ? THAI CHARACTER PHO PHAN 
  ( $0dbf, $0e1f ), //                  Thai_fofan ? THAI CHARACTER FO FAN 
  ( $0dc0, $0e20 ), //             Thai_phosamphao ? THAI CHARACTER PHO SAMPHAO 
  ( $0dc1, $0e21 ), //                   Thai_moma ? THAI CHARACTER MO MA 
  ( $0dc2, $0e22 ), //                  Thai_yoyak ? THAI CHARACTER YO YAK 
  ( $0dc3, $0e23 ), //                  Thai_rorua ? THAI CHARACTER RO RUA 
  ( $0dc4, $0e24 ), //                     Thai_ru ? THAI CHARACTER RU 
  ( $0dc5, $0e25 ), //                 Thai_loling ? THAI CHARACTER LO LING 
  ( $0dc6, $0e26 ), //                     Thai_lu ? THAI CHARACTER LU
  ( $0dc7, $0e27 ), //                 Thai_wowaen ? THAI CHARACTER WO WAEN 
  ( $0dc8, $0e28 ), //                 Thai_sosala ? THAI CHARACTER SO SALA 
  ( $0dc9, $0e29 ), //                 Thai_sorusi ? THAI CHARACTER SO RUSI 
  ( $0dca, $0e2a ), //                  Thai_sosua ? THAI CHARACTER SO SUA 
  ( $0dcb, $0e2b ), //                  Thai_hohip ? THAI CHARACTER HO HIP
  ( $0dcc, $0e2c ), //                Thai_lochula ? THAI CHARACTER LO CHULA 
  ( $0dcd, $0e2d ), //                   Thai_oang ? THAI CHARACTER O ANG 
  ( $0dce, $0e2e ), //               Thai_honokhuk ? THAI CHARACTER HO NOKHUK 
  ( $0dcf, $0e2f ), //              Thai_paiyannoi ? THAI CHARACTER PAIYANNOI 
  ( $0dd0, $0e30 ), //                  Thai_saraa ? THAI CHARACTER SARA A 
  ( $0dd1, $0e31 ), //             Thai_maihanakat ? THAI CHARACTER MAI HAN-AKAT 
  ( $0dd2, $0e32 ), //                 Thai_saraaa ? THAI CHARACTER SARA AA 
  ( $0dd3, $0e33 ), //                 Thai_saraam ? THAI CHARACTER SARA AM 
  ( $0dd4, $0e34 ), //                  Thai_sarai ? THAI CHARACTER SARA I 
  ( $0dd5, $0e35 ), //                 Thai_saraii ? THAI CHARACTER SARA II 
  ( $0dd6, $0e36 ), //                 Thai_saraue ? THAI CHARACTER SARA UE 
  ( $0dd7, $0e37 ), //                Thai_sarauee ? THAI CHARACTER SARA UEE 
  ( $0dd8, $0e38 ), //                  Thai_sarau ? THAI CHARACTER SARA U 
  ( $0dd9, $0e39 ), //                 Thai_sarauu ? THAI CHARACTER SARA UU 
  ( $0dda, $0e3a ), //                Thai_phinthu ? THAI CHARACTER PHINTHU 
  ( $0ddf, $0e3f ), //                   Thai_baht ? THAI CURRENCY SYMBOL BAHT 
  ( $0de0, $0e40 ), //                  Thai_sarae ? THAI CHARACTER SARA E 
  ( $0de1, $0e41 ), //                 Thai_saraae ? THAI CHARACTER SARA AE 
  ( $0de2, $0e42 ), //                  Thai_sarao ? THAI CHARACTER SARA O 
  ( $0de3, $0e43 ), //          Thai_saraaimaimuan ? THAI CHARACTER SARA AI MAIMUAN 
  ( $0de4, $0e44 ), //         Thai_saraaimaimalai ? THAI CHARACTER SARA AI MAIMALAI 
  ( $0de5, $0e45 ), //            Thai_lakkhangyao ? THAI CHARACTER LAKKHANGYAO 
  ( $0de6, $0e46 ), //               Thai_maiyamok ? THAI CHARACTER MAIYAMOK 
  ( $0de7, $0e47 ), //              Thai_maitaikhu ? THAI CHARACTER MAITAIKHU 
  ( $0de8, $0e48 ), //                  Thai_maiek ? THAI CHARACTER MAI EK 
  ( $0de9, $0e49 ), //                 Thai_maitho ? THAI CHARACTER MAI THO 
  ( $0dea, $0e4a ), //                 Thai_maitri ? THAI CHARACTER MAI TRI 
  ( $0deb, $0e4b ), //            Thai_maichattawa ? THAI CHARACTER MAI CHATTAWA 
  ( $0dec, $0e4c ), //            Thai_thanthakhat ? THAI CHARACTER THANTHAKHAT 
  ( $0ded, $0e4d ), //               Thai_nikhahit ? THAI CHARACTER NIKHAHIT 
  ( $0df0, $0e50 ), //                 Thai_leksun ? THAI DIGIT ZERO
  ( $0df1, $0e51 ), //                Thai_leknung ? THAI DIGIT ONE 
  ( $0df2, $0e52 ), //                Thai_leksong ? THAI DIGIT TWO 
  ( $0df3, $0e53 ), //                 Thai_leksam ? THAI DIGIT THREE 
  ( $0df4, $0e54 ), //                  Thai_leksi ? THAI DIGIT FOUR 
  ( $0df5, $0e55 ), //                  Thai_lekha ? THAI DIGIT FIVE
  ( $0df6, $0e56 ), //                 Thai_lekhok ? THAI DIGIT SIX 
  ( $0df7, $0e57 ), //                Thai_lekchet ? THAI DIGIT SEVEN 
  ( $0df8, $0e58 ), //                Thai_lekpaet ? THAI DIGIT EIGHT 
  ( $0df9, $0e59 ), //                 Thai_lekkao ? THAI DIGIT NINE 
  ( $0ea1, $3131 ), //               Hangul_Kiyeog ? HANGUL LETTER KIYEOK 
  ( $0ea2, $3132 ), //          Hangul_SsangKiyeog ? HANGUL LETTER SSANGKIYEOK 
  ( $0ea3, $3133 ), //           Hangul_KiyeogSios ? HANGUL LETTER KIYEOK-SIOS 
  ( $0ea4, $3134 ), //                Hangul_Nieun ? HANGUL LETTER NIEUN 
  ( $0ea5, $3135 ), //           Hangul_NieunJieuj ? HANGUL LETTER NIEUN-CIEUC 
  ( $0ea6, $3136 ), //           Hangul_NieunHieuh ? HANGUL LETTER NIEUN-HIEUH 
  ( $0ea7, $3137 ), //               Hangul_Dikeud ? HANGUL LETTER TIKEUT 
  ( $0ea8, $3138 ), //          Hangul_SsangDikeud ? HANGUL LETTER SSANGTIKEUT 
  ( $0ea9, $3139 ), //                Hangul_Rieul ? HANGUL LETTER RIEUL 
  ( $0eaa, $313a ), //          Hangul_RieulKiyeog ? HANGUL LETTER RIEUL-KIYEOK 
  ( $0eab, $313b ), //           Hangul_RieulMieum ? HANGUL LETTER RIEUL-MIEUM 
  ( $0eac, $313c ), //           Hangul_RieulPieub ? HANGUL LETTER RIEUL-PIEUP 
  ( $0ead, $313d ), //            Hangul_RieulSios ? HANGUL LETTER RIEUL-SIOS 
  ( $0eae, $313e ), //           Hangul_RieulTieut ? HANGUL LETTER RIEUL-THIEUTH 
  ( $0eaf, $313f ), //          Hangul_RieulPhieuf ? HANGUL LETTER RIEUL-PHIEUPH 
  ( $0eb0, $3140 ), //           Hangul_RieulHieuh ? HANGUL LETTER RIEUL-HIEUH 
  ( $0eb1, $3141 ), //                Hangul_Mieum ? HANGUL LETTER MIEUM 
  ( $0eb2, $3142 ), //                Hangul_Pieub ? HANGUL LETTER PIEUP 
  ( $0eb3, $3143 ), //           Hangul_SsangPieub ? HANGUL LETTER SSANGPIEUP 
  ( $0eb4, $3144 ), //            Hangul_PieubSios ? HANGUL LETTER PIEUP-SIOS 
  ( $0eb5, $3145 ), //                 Hangul_Sios ? HANGUL LETTER SIOS 
  ( $0eb6, $3146 ), //            Hangul_SsangSios ? HANGUL LETTER SSANGSIOS 
  ( $0eb7, $3147 ), //                Hangul_Ieung ? HANGUL LETTER IEUNG 
  ( $0eb8, $3148 ), //                Hangul_Jieuj ? HANGUL LETTER CIEUC 
  ( $0eb9, $3149 ), //           Hangul_SsangJieuj ? HANGUL LETTER SSANGCIEUC 
  ( $0eba, $314a ), //                Hangul_Cieuc ? HANGUL LETTER CHIEUCH 
  ( $0ebb, $314b ), //               Hangul_Khieuq ? HANGUL LETTER KHIEUKH
  ( $0ebc, $314c ), //                Hangul_Tieut ? HANGUL LETTER THIEUTH 
  ( $0ebd, $314d ), //               Hangul_Phieuf ? HANGUL LETTER PHIEUPH 
  ( $0ebe, $314e ), //                Hangul_Hieuh ? HANGUL LETTER HIEUH 
  ( $0ebf, $314f ), //                    Hangul_A ? HANGUL LETTER A 
  ( $0ec0, $3150 ), //                   Hangul_AE ? HANGUL LETTER AE
  ( $0ec1, $3151 ), //                   Hangul_YA ? HANGUL LETTER YA 
  ( $0ec2, $3152 ), //                  Hangul_YAE ? HANGUL LETTER YAE 
  ( $0ec3, $3153 ), //                   Hangul_EO ? HANGUL LETTER EO 
  ( $0ec4, $3154 ), //                    Hangul_E ? HANGUL LETTER E 
  ( $0ec5, $3155 ), //                  Hangul_YEO ? HANGUL LETTER YEO 
  ( $0ec6, $3156 ), //                   Hangul_YE ? HANGUL LETTER YE 
  ( $0ec7, $3157 ), //                    Hangul_O ? HANGUL LETTER O 
  ( $0ec8, $3158 ), //                   Hangul_WA ? HANGUL LETTER WA 
  ( $0ec9, $3159 ), //                  Hangul_WAE ? HANGUL LETTER WAE 
  ( $0eca, $315a ), //                   Hangul_OE ? HANGUL LETTER OE 
  ( $0ecb, $315b ), //                   Hangul_YO ? HANGUL LETTER YO 
  ( $0ecc, $315c ), //                    Hangul_U ? HANGUL LETTER U 
  ( $0ecd, $315d ), //                  Hangul_WEO ? HANGUL LETTER WEO 
  ( $0ece, $315e ), //                   Hangul_WE ? HANGUL LETTER WE 
  ( $0ecf, $315f ), //                   Hangul_WI ? HANGUL LETTER WI 
  ( $0ed0, $3160 ), //                   Hangul_YU ? HANGUL LETTER YU 
  ( $0ed1, $3161 ), //                   Hangul_EU ? HANGUL LETTER EU 
  ( $0ed2, $3162 ), //                   Hangul_YI ? HANGUL LETTER YI 
  ( $0ed3, $3163 ), //                    Hangul_I ? HANGUL LETTER I 
  ( $0ed4, $11a8 ), //             Hangul_J_Kiyeog ? HANGUL JONGSEONG KIYEOK 
  ( $0ed5, $11a9 ), //        Hangul_J_SsangKiyeog ? HANGUL JONGSEONG SSANGKIYEOK 
  ( $0ed6, $11aa ), //         Hangul_J_KiyeogSios ? HANGUL JONGSEONG KIYEOK-SIOS 
  ( $0ed7, $11ab ), //              Hangul_J_Nieun ? HANGUL JONGSEONG NIEUN 
  ( $0ed8, $11ac ), //         Hangul_J_NieunJieuj ? HANGUL JONGSEONG NIEUN-CIEUC 
  ( $0ed9, $11ad ), //         Hangul_J_NieunHieuh ? HANGUL JONGSEONG NIEUN-HIEUH 
  ( $0eda, $11ae ), //             Hangul_J_Dikeud ? HANGUL JONGSEONG TIKEUT 
  ( $0edb, $11af ), //              Hangul_J_Rieul ? HANGUL JONGSEONG RIEUL 
  ( $0edc, $11b0 ), //        Hangul_J_RieulKiyeog ? HANGUL JONGSEONG RIEUL-KIYEOK 
  ( $0edd, $11b1 ), //         Hangul_J_RieulMieum ? HANGUL JONGSEONG RIEUL-MIEUM 
  ( $0ede, $11b2 ), //         Hangul_J_RieulPieub ? HANGUL JONGSEONG RIEUL-PIEUP 
  ( $0edf, $11b3 ), //          Hangul_J_RieulSios ? HANGUL JONGSEONG RIEUL-SIOS
  ( $0ee0, $11b4 ), //         Hangul_J_RieulTieut ? HANGUL JONGSEONG RIEUL-THIEUTH 
  ( $0ee1, $11b5 ), //        Hangul_J_RieulPhieuf ? HANGUL JONGSEONG RIEUL-PHIEUPH 
  ( $0ee2, $11b6 ), //         Hangul_J_RieulHieuh ? HANGUL JONGSEONG RIEUL-HIEUH 
  ( $0ee3, $11b7 ), //              Hangul_J_Mieum ? HANGUL JONGSEONG MIEUM 
  ( $0ee4, $11b8 ), //              Hangul_J_Pieub ? HANGUL JONGSEONG PIEUP
  ( $0ee5, $11b9 ), //          Hangul_J_PieubSios ? HANGUL JONGSEONG PIEUP-SIOS 
  ( $0ee6, $11ba ), //               Hangul_J_Sios ? HANGUL JONGSEONG SIOS 
  ( $0ee7, $11bb ), //          Hangul_J_SsangSios ? HANGUL JONGSEONG SSANGSIOS 
  ( $0ee8, $11bc ), //              Hangul_J_Ieung ? HANGUL JONGSEONG IEUNG 
  ( $0ee9, $11bd ), //              Hangul_J_Jieuj ? HANGUL JONGSEONG CIEUC 
  ( $0eea, $11be ), //              Hangul_J_Cieuc ? HANGUL JONGSEONG CHIEUCH 
  ( $0eeb, $11bf ), //             Hangul_J_Khieuq ? HANGUL JONGSEONG KHIEUKH 
  ( $0eec, $11c0 ), //              Hangul_J_Tieut ? HANGUL JONGSEONG THIEUTH 
  ( $0eed, $11c1 ), //             Hangul_J_Phieuf ? HANGUL JONGSEONG PHIEUPH 
  ( $0eee, $11c2 ), //              Hangul_J_Hieuh ? HANGUL JONGSEONG HIEUH 
  ( $0eef, $316d ), //     Hangul_RieulYeorinHieuh ? HANGUL LETTER RIEUL-YEORINHIEUH 
  ( $0ef0, $3171 ), //    Hangul_SunkyeongeumMieum ? HANGUL LETTER KAPYEOUNMIEUM 
  ( $0ef1, $3178 ), //    Hangul_SunkyeongeumPieub ? HANGUL LETTER KAPYEOUNPIEUP 
  ( $0ef2, $317f ), //              Hangul_PanSios ? HANGUL LETTER PANSIOS 
  ( $0ef3, $3181 ), //    Hangul_KkogjiDalrinIeung ? HANGUL LETTER YESIEUNG 
  ( $0ef4, $3184 ), //   Hangul_SunkyeongeumPhieuf ? HANGUL LETTER KAPYEOUNPHIEUPH 
  ( $0ef5, $3186 ), //          Hangul_YeorinHieuh ? HANGUL LETTER YEORINHIEUH 
  ( $0ef6, $318d ), //                Hangul_AraeA ? HANGUL LETTER ARAEA 
  ( $0ef7, $318e ), //               Hangul_AraeAE ? HANGUL LETTER ARAEAE 
  ( $0ef8, $11eb ), //            Hangul_J_PanSios ? HANGUL JONGSEONG PANSIOS 
  ( $0ef9, $11f0 ), //  Hangul_J_KkogjiDalrinIeung ? HANGUL JONGSEONG YESIEUNG 
  ( $0efa, $11f9 ), //        Hangul_J_YeorinHieuh ? HANGUL JONGSEONG YEORINHIEUH
  ( $0eff, $20a9 ), //                  Korean_Won ? WON SIGN 
  ( $13a4, $20ac ), //                        Euro € EURO SIGN 
  ( $13bc, $0152 ), //                          OE Œ LATIN CAPITAL LIGATURE OE 
  ( $13bd, $0153 ), //                          oe œ LATIN SMALL LIGATURE OE 
  ( $13be, $0178 ), //                  Ydiaeresis Ÿ LATIN CAPITAL LETTER Y WITH DIAERESIS 
  ( $20ac, $20ac ) //                    EuroSign € EURO SIGN
);

Function keysym2ucs(keysym:Cardinal):TERRAChar;
Var
  Min, Max, Mid:Integer;
Begin
  min := 0;
  max := Pred(keysymtabCount);

  //first check for Latin-1 characters (1:1 mapping)
  If ((keysym >= $0020) And (keysym <= $007e) Or ((keysym >= $00a0) And (keysym <= $00ff))) Then
  Begin
    Result := TERRAChar(keysym);
    Exit;
  End;

   // also check for directly encoded 24-bit UCS characters
   If ((keysym And $ff000000) = $01000000) Then
   Begin
	  Result := keysym And $00ffffff;
    Exit;
   End;

   // binary search in table
    While (max >= min) Do
    Begin
	    mid := (min + max) Shr 1;
	    If (keysymtab[mid, 0] < keysym) Then
	      min := mid + 1
      Else
      If (keysymtab[mid, 0] > keysym) Then
	      max := mid - 1
	    Else
      Begin //found it
	      Result := keysymtab[mid, 1];
        Exit;
      End;
    End;

    // no matching Unicode value found
    Result := 0;
End;


Const
  _SC_NPROCESSORS_ONLN = 83;

Function sysconf(i:Integer):CLong; CDecl; External Name 'sysconf';

Function LinuxApplication.InitWindow:Boolean;
Var
//  CMap:TColorMap;
  wmDelete:TAtom;

  Root:TWindow;
  Cursor:TCursor;
  CursorMask:TPixmap;
  DummyColor:TXColor;
Begin
  Result := False;

  _Display := XOpenDisplay(Nil);
  If (Not Assigned(_Display)) Then
  Begin
    RaiseError('CreateWindow: Cannot connect to X server.');
    Exit;
  End; 

  _ScreenHandle := DefaultScreen(_Display);

  //Root := RootWindow(_Display, Vi.Screen);
  Root := RootWindow(_Display, _ScreenHandle);

{  // create a color map
  CMap := XCreateColormap(_Display, Root, Vi.visual, AllocNone);
  _Attr.Colormap:=CMap;}
  _Attr.border_pixel := 0;

  // create a window in window mode
  _Attr.Event_mask := ExposureMask Or StructureNotifyMask Or PointerMotionMask;
  _Attr.Event_mask := _Attr.Event_mask Or KeyPressMask Or KeyReleaseMask;
  _Attr.Event_mask := _Attr.Event_mask Or ButtonPressMask Or ButtonReleaseMask;
  _Attr.Event_mask := _Attr.Event_mask Or ExposureMask;

  _Window := XCreateWindow(_Display, Root, 0, 0, _Width, _Height,
                         0, CopyFromParent, InputOutput, {Vi.visual}Nil,
                         CWBorderPixel {Or CWColormap} Or CWEventMask,
                         @_Attr);

  // only set window title and handle wm_delete_events if in windowed mode
  wmDelete := XInternAtom(_Display, 'WM_DELETE_WINDOW', True);
  XSetWMProtocols(_Display, _Window, @wmDelete, 1);
  XSetStandardProperties(_Display, _Window, PAnsiChar(Title), PAnsiChar(Title),
                         None, Nil, 0, Nil);

  XMapRaised(_Display, _Window);

  // Hide cursor
  If (Not _IgnoreCursor) Then
  Begin
    CursorMask := XCreatePixmap(_Display, Root, 1, 1, 1);
    DummyColor.Pixel:=0;
    DummyColor.Red:=0;
    DummyColor.Flags := 4;

    Cursor:=XCreatePixmapCursor(_Display, CursorMask, CursorMask, @DummyColor, @DummyColor, 0, 0);

    XFreePixmap(_Display, Cursormask);
    XDefineCursor(_Display, _Window, Cursor);
  End;

  _Ready := True;
  Result:=True;
End;

Procedure LinuxApplication.CloseWindow;
Begin
  XDestroyWindow(_Display, _Window);
End;

Procedure LinuxApplication.SetState(State:Cardinal);
Begin
  // TODO
End;

Procedure LinuxApplication.ProcessMessages;
Var
  Event:TXEvent;
  Key:Cardinal;
  AtomName:PAnsiChar;
  C:TERRAChar;
  Buf:Array[0..10] Of AnsiChar;
  N:Integer;
  WA:TXWindowAttributes;
Begin
  If _Display = Nil Then
     Exit;

    While (XPending(_Display)> 0) Do
    Begin
      XNextEvent(_Display, @Event);

      Case (Event._type) Of
      ButtonPress:  Case Event.xbutton.button Of
			  1:AddValueEvent(eventMouseDown, keyMouseLeft);
  			2:AddValueEvent(eventMouseDown, keyMouseMiddle);
  			3:AddValueEvent(eventMouseDown, keyMouseRight);
			End;

      ButtonRelease:Case Event.xbutton.button Of
	  		1:AddValueEvent(eventMouseUp, keyMouseLeft);
  			2:AddValueEvent(eventMouseUp, keyMouseMiddle);
  			3:AddValueEvent(eventMouseUp, keyMouseRight);
			End;

      KeyPress:
        Begin
          Key := XKeycodeToKeysym(_Display, Event.xkey.keycode, 0);
          //Key := XkbKeycodeToKeysym (_Display, Event.xkey.keycode, 0, 0);
          Key := KeyLookUp(Key);

          N := XLookupString(@Event.xkey, @Buf[0], 4, @C, Nil);

          If (N<=0) Then
          Begin
            C := keysym2ucs(C);
          End Else
          Begin
            C := 0;
            Move(Buf[0], C, N);
          End;

          If Key>0 Then
	     AddValueEvent(eventKeyDown, Key);

          If (C>0) Then
             AddValueEvent(eventKeyPress, C);
        End;

      KeyRelease:
                 Begin
                      Key := XKeycodeToKeysym(_Display, Event.xkey.keycode, 0);
                      //Key := XkbKeycodeToKeysym (_Display, Event.xkey.keycode, 0, 0);
                      Key := KeyLookUp(Key);

                      If Key>0 Then
		         AddValueEvent(eventKeyUp, Key);
                 End;

      MotionNotify: AddCoordEvent(eventMouseMove, event.xmotion.X, event.xmotion.Y, 0);

      Expose:	Begin
			XGetWindowAttributes(_Display, _Window, @WA);
			AddCoordEvent(eventWindowResize, WA.width, WA.height, 0);
	End;

      ClientMessage:  Begin
                        AtomName:=XGetAtomName(_Display, event.xClient.message_type);
                        If (AtomName='WM_PROTOCOLS') Then
                          _Running:=False;
                      End;
      End;
    End;
End;

Type
  PXRRScreenSizeArray = ^XRRScreenSizeArray;
  XRRScreenSizeArray = Array[0..128] Of TXRRScreenSize;

//http://www.blitzbasic.com/Community/posts.php?topic=86911
Function LinuxApplication.InitSettings: Boolean;
Var
  I:Integer;
  Lang:TERRAString;
  Root:TWindow;
  xrrs:PXRRScreenSizeArray;
  num_sizes:Integer;
  original_rotation:PRotation;
  original_size_id:TSizeID;
  conf:PXRRScreenConfiguration;
  current_rate:Integer;
Begin
  Inherited InitSettings;

  Log(logDebug,'App', 'Getting user locale...');
  lang := GetEnvironmentVariable('LANG');
  _Language := StringGetNextSplit(Lang, Ord('_'));
  _Country := StringGetNextSplit(Lang, Ord('.'));

  If (_Country='') Then
    _Country := _Language;

  SetLength(_Language, 2);
  SetLength(_Country, 2);

  _Language := StringUpper(_Language);
  _Country := StringUpper(_Country);

  Log(logDebug, 'App', 'Country: '+_Country);
  Log(logDebug, 'App', 'Language: '+_Language);


  Log(logDebug,'App', 'Getting cpu core count...');
  _CPUCores := sysconf(_SC_NPROCESSORS_ONLN);
  Log(logDebug, 'App', 'Found '+IntToString(_CPUCores)+' cores');

  Log(logDebug,'App', 'Getting screen resolution...');

  _Display := XOpenDisplay(Nil);
  If (Not Assigned(_Display)) Then
  Begin
    RaiseError('CreateWindow: Cannot connect to X server.');
    Exit;
  End;

  original_rotation := Nil;

  Root := RootWindow(_Display, 0);

  xrrs := PXRRScreenSizeArray(XRRSizes(_Display, 0, @num_sizes));
  If Assigned(xrrs) Then
  Begin
    conf := XRRGetScreenInfo(_Display, Root);
    current_rate := XRRConfigCurrentRate(conf);
    original_size_id := XRRConfigCurrentConfiguration(conf, @original_rotation);
    _Screen.Width := xrrs[original_size_id].width;
    _Screen.Height := xrrs[original_size_id].height;
  End;

  Renderers.Add(OpenGLRenderer.Create());

  XCloseDisplay(_Display);
  _Display := Nil;

  // Initialize joysticks/gamepads
  //For I:=0 To 3 Do
  For I:=0 To 3 Do
    InputManager.Instance.AddGamePad(LinuxGamePad.Create(I));

  Result := True;
End;

Initialization
Finalization
  ReleaseObject(_Application_Instance);
End.
