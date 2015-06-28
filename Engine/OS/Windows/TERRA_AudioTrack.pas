Unit TERRA_AudioTrack;
{$I terra.inc}
Interface

Uses Windows, TERRA_Object, TERRA_String, TERRA_Utils, TERRA_FileUtils, TERRA_Application, TERRA_MusicTrack, TERRA_Multimedia;

Type
  AudioMusicTrack = Class(MusicTrack)
    Protected
      _DeviceID:Word;
      _IsMIDI:Boolean;

      Function CheckError(aError: integer):Boolean;
      Function GetMode :Integer;

      Procedure ChangeVolume(Volume:Single); Override;

    Public
      Procedure Init(); Override;
      Procedure Play(); Override;
      Procedure Stop; Override;

      Class Function Supports(Const Extension:TERRAString):Boolean; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_OS, TERRA_FileManager, TERRA_FileStream, TERRA_Stream, TERRA_Math;

Const
  mpNotReady = 0;
  mpStopped = 1;
  mpPlaying = 2;
  mpRecording = 3;
  mpSeeking = 4;
  mpPaused = 5;
  mpOpen = 6;

{ AudioMusicTrack }
Function AudioMusicTrack.CheckError(aError: integer):Boolean;
Var
  ErrMsg : array[0..4095] of AnsiChar;
  strMsg :AnsiString;
Begin
  // exit if no error
  If ( aError = 0 )then
  Begin
    Result := FalsE;
    Exit;
  End;

  FillChar(ErrMsg, SizeOf(ErrMsg), 0);

  // get error message and raise exception
  If( mciGetErrorString(aError, ErrMsg, SizeOf( ErrMsg ) ) ) Then
  Begin
    SetString(strMsg, ErrMsg, Length(ErrMsg));
  End Else
    strMsg := 'Unknown error code';

  Result := True;
  Log(logError, 'Music', strMsg);
End;

(*Function MusicManager.mciLgthInSecs: integer;
var
    intLength : integer;
    intMM, intSS : integer;
    SetParm: TMCI_Set_Parms;
    intFlags : integer;
    intError : integer;
Begin
  // this function is called from the constructor, so it opens and closes the player
  mciOpen();
  Try
    // set time format to milliseconds
    intFlags := mci_Set_Time_Format;
    SetParm.dwTimeFormat := 0;
    intError := mciSendCommand( _DeviceID, mci_Set,intFlags,Longint( @SetParm ) );
    CheckError( intError );

    // get length of .mp3
    intLength := mciGetLength;
    // do the math to return number of seconds
    intLength := intLength div 1000;
    intMM := intLength div 60;
    intSS := intLength mod 60;
     Result := (intMM * 60) + intSS;
  Finally
    mciClose;
  End; // try finally
End; // mciLgthInSecs
*)


Function AudioMusicTrack.GetMode :Integer;
Var
  StatusParm:MCI_Status_Params;
  intFlags:Integer;
  intError:Integer;
Begin
    // set initial result
    Result := 0;

    // exit if player is not active
    if( _DeviceID = 0 )then Exit;

    // get current state
    intFlags := mci_Wait or mci_Status_Item;
    StatusParm.dwItem := mci_Status_Mode;
    intError := mciSendCommand( _DeviceID, mci_Status,intFlags, PtrUInt( @StatusParm ));
    CheckError( intError );

    // set result
    case StatusParm.dwReturn of
    MCI_MODE_NOT_READY :
        begin
        Result := mpNotReady;
        end; // MCI_MODE_NOT_READY
    MCI_MODE_STOP :
        begin
        Result := mpStopped;
        end; // MCI_MODE_STOP
    MCI_MODE_PLAY :
        begin
        Result := mpPlaying;
        end; // MCI_MODE_PLAY
    MCI_MODE_RECORD :
        begin
        Result := mpRecording;
        end; // MCI_MODE_RECORD
    MCI_MODE_SEEK :
        begin
        Result := mpSeeking;
        end; // MCI_MODE_SEEK
    MCI_MODE_PAUSE :
        begin
        Result := mpPaused;
        end; //
    MCI_MODE_OPEN :
        begin
        Result := mpOpen;
        end; // MCI_MODE_OPEN
    end; // case
End;

(*Function AudioMusicTrack.mciGetLength : Longint;
Var
  StatusParm:MCI_Status_Params;
  intFlags:Integer;
  intError:Integer;
Begin
  // set initial result
  Result := 0;

  // exit if player is not active
  If (_DeviceID=0) Then
    Exit;

  // get length of .mp3
  intFlags := mci_Wait or mci_Status_Item;
  StatusParm.dwItem := mci_Status_Length;
  intError := mciSendCommand( _DeviceID,mci_Status,intFlags,Longint( @StatusParm ) );
  CheckError( intError );
  Result := StatusParm.dwReturn;
End;*)

(*Function AudioMusicTrack.mciGetPosition : Longint;
Var
  StatusParm:MCI_Status_Params;
  intFlags:Integer;
  intError:Integer;
Begin
  // set initial result
  Result := 0;

  // exit if player is not active
  If (_DeviceID = 0) Then
    Exit;

  // get current position
  intFlags := mci_Wait or mci_Status_Item;
  StatusParm.dwItem := mci_Status_Position;
  intError := mciSendCommand( _DeviceID, mci_Status,intFlags,Longint( @StatusParm ) );
  CheckError( intError );
  Result := StatusParm.dwReturn;
End; *)


Procedure AudioMusicTrack.ChangeVolume(Volume:Single);
Var
  intError:integer;
Begin
  // exit if player is not active
  If (_DeviceID = 0 ) Or (_IsMIDI) Then
    Exit;

  intError := mciSendString(PAnsiChar('setaudio '+_FileName+' volume to '+IntToString(Trunc(_Volume*1000))), Nil, 0, Application.Instance().Handle);
  CheckError( intError );
End;

Procedure AudioMusicTrack.Init();
Var
  Src, Dest:Stream;
  OpenParm:MCI_OPEN_PARAMS;
  lMode :Integer;
  intFlags:Integer;
  intError:Integer;
Begin
  // open player if not already open
  If (_DeviceID = 0) Then
  Begin
    // close player if active
    Self.Stop();


    // extract from package if necessary
    If IsPackageFileName(FileName) Then
    Begin
      Src := FileManager.Instance.OpenStream(FileName);
      If Not Assigned(Src) Then
        Exit;

      _FileName := Application.Instance.TempPath + PathSeparator + GetFileName(FileName, False);
      Dest := FileStream.Create(_FileName);
      Src.Copy(Dest);
      ReleaseObject(Dest);
      ReleaseObject(Src);
    End;

    // open player
    OpenParm.dwCallback := 0;
    OpenParm.lpstrElementName := PAnsiChar(_FileName);

    _IsMIDI := StringContains('mid', GetFileExtension(_FileName));
    If (_IsMIDI) Then
    Begin
      OpenParm.lpstrDeviceType := PAnsiChar(MCI_DEVTYPE_SEQUENCER);
      intFlags := MCI_OPEN_TYPE Or MCI_OPEN_ELEMENT Or MCI_OPEN_TYPE_ID;
    End Else
    Begin
      OpenParm.lpstrDeviceType := 'WaveAudio';
      intFlags := MCI_WAIT or MCI_OPEN_ELEMENT or MCI_OPEN_SHAREABLE;
    End;

    intError := mciSendCommand( 0, MCI_OPEN, intFlags, PtrUInt(@OpenParm));
    CheckError( intError );

    // save device ID
    _DeviceID := OpenParm.wDeviceID;

    // get current state
    lMode := Self.GetMode();

    // if currently playing; stop player (to restart at the beginning)
//    if( lMode = mpPlaying )then Stop;
  End;
End;

Procedure AudioMusicTrack.Play();
Var
  PlayParm:MCI_Play_Params;
  intFlags, intError:Integer;
Begin

  // exit if player is not active
  If (_DeviceID = 0 ) Then
    Exit;

  intError := mciSendString(PAnsiChar('seek '+FileName+' to 0'), Nil, 0, Application.Instance().Handle);
  CheckError( intError );

  intFlags := mci_Notify;
  PlayParm.dwCallback := Application.Instance().Handle;
  intError := mciSendString(PAnsiChar('Play '+FileName+' notify'), Nil, 0, Application.Instance().Handle);
  //intError := mciSendCommand(_DeviceID, mci_Play,intFlags,Longint( @PlayParm ) );
  CheckError( intError );

  Self.SetVolume(_Volume);
End;

Procedure AudioMusicTrack.Stop;
Var
  intFlags : integer;
  intError:Integer;
Begin
  // exit if player is not active
  If (_DeviceID <> 0 ) Then
  Begin
    // exit if player is not active
    if( _DeviceID = 0 )then Exit;

    // stop the player
    intError := mciSendCommand( _DeviceID,mci_Stop,0,0 );
    CheckError( intError );

    // close the player
    intFlags := MCI_WAIT;
    intError := mciSendCommand( _DeviceID,mci_Close,intFlags,0 );
    CheckError( intError );

    // zero device ID
    _DeviceID := 0;
  End;
End;

{Procedure MusicManager.Pause;
Var
  lMode:Integer;
    intError : integer;
Begin
  If ( _DeviceID <> 0 ) Then
  Begin
    // get current state
    lMode := mciGetMode;

    //  if currently playing; pause the player, suspend the progress thread if it is running
    If (lMode = mpPlaying) Then
    Begin
    // exit if player is not active
    if( _DeviceID = 0 )then Exit;

    // do pause
    intError := mciSendCommand( _DeviceID,mci_Pause,0,0 );
    CheckError( intError );
    End; // else if
  End;
}

{Procedure MusicManager.Resume;
Var
    intError : integer;
  lMode:Integer;
Begin
  If (_DeviceID<>0) Then
  Begin
    // get current state
    lMode := mciGetMode;

    // if paused; resume play, resume progress thread if it is running
    If (lMode = mpPaused) Then
    Begin
    // exit if player is not active
    if( _DeviceID = 0 )then Exit;

    // do resume
    intError := mciSendCommand( _DeviceID,mci_Resume,0,0 );
    CheckError( intError );
    End;
  End;
}

Class Function AudioMusicTrack.Supports(Const Extension: TERRAString): Boolean;
Begin
  Result := (Extension = 'mid') Or (Extension = 'mp3') Or (Extension = 'wav');
End;

End.
