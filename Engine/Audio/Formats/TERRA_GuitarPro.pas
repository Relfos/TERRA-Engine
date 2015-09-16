{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_
 * Implements a loader/writer for Guitar Pro 5 files
 ***********************************************************************************************************************
}
Unit TERRA_GuitarPro;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_Stream, TERRA_FileStream, TERRA_Color;

Const
  PercussionChannel = 10;

	GP5_FORMAT_EXTENSION = '.gp5';
	GP5_VERSION = 'FICHIER GUITAR PRO v5.00';
	GP_BEND_SEMITONE = 25;
	GP_BEND_POSITION = 60;

  PAGE_SETUP_LINES_SIZE = 11;
	PAGE_SETUP_LINES :Array[1..PAGE_SETUP_LINES_SIZE] Of String = (
		'%TITLE%',
		'%SUBTITLE%',
		'%ARTIST%',
		'%ALBUM%',
		'Words by %WORDS%',
    'Music by %MUSIC%',
		'Words & Music by %WORDSMUSIC%',
		'Copyright %COPYRIGHT%',
		'All Rights Reserved - International Copyright Secured',
		'Page %N%/%P%',
		'Moderate'
	);

  TRIPLET_FEEL_NONE = 0;
  TRIPLET_FEEL_EIGHTH = 1;
  TRIPLET_FEEL_SIXTEENTH = 2;

  STROKE_NONE = 0;

  DIVISION_NORMAL = 0;

Type
  TimeSignature = Object
    Numerator:Byte;
    Denominator:Byte;

    Procedure Init;
  End;

  TablatureChannel = Record
    _Program:Integer;
		_Volume:Byte;
    _Balance:Byte;
    _Chorus:Byte;
    _Reverb:Byte;
    _Phaser:Byte;
    _Tremolo:Byte;
  End;

  TrackChannel = Record
    A,B:Integer;
  End;

  MeasureHeader = Record
    Number:Integer;
    Tempo:Integer;
    Signature:TimeSignature;
		RepeatOpen:Boolean;
		RepeatClose:Byte;
		RepeatAlternative:Byte;
		Marker:Byte;
    TripletFeel:Byte;
  End;

  TablatureNoteEffect = Record
    FadeIn:Boolean;
    TremoloBar:Boolean;
    Tapping:Boolean;
    Slapping:Boolean;
    Popping:Boolean;
  End;

  TablatureNoteDuration = Record
    Dotted:Boolean;
    DoubleDotted:Boolean;
    Division:Integer;
  End;

  TablatureNote = Class(TERRAObject)
    Protected
      _Effect:TablatureNoteEffect;
  End;

  TablatureVoice = Class(TERRAObject)
    Protected
      _Index:Integer;
      _Notes:Array Of TablatureNote;
      _NoteCount:Integer;
      _Duration:TablatureNoteDuration;
      _Rest:Boolean;

      Function ParseDuration():Byte;
    Public
  End;

  TablatureStroke = Record
    _Direction:Integer;
  End;

  TablatureMeasure = Class;

  TablatureBeat = Class(TERRAObject)
    Protected
      _Voices:Array Of TablatureVoice;
      _VoiceCount:Integer;

      _Stroke:TablatureStroke;

      Procedure Save(Dest:Stream; Voice:TablatureVoice; Measure:TablatureMeasure; TempoChange:Boolean);

    Public

      Function IsChordBeat():Boolean;
  End;

  TablatureMeasure = Class(TERRAObject)
    Protected
      _Beats:Array Of TablatureBeat;
      _BeatCount:Integer;

      Procedure Save(Dest:Stream; TempoChange:Boolean);
    Public
  End;

  TablatureTrack = Class(TERRAObject)
    Protected
      _Name:String;

      _Channel:TrackChannel;

      _Offset:Integer;

      _Color:Color;

      _StringCount:Integer;
      _Strings:Array Of Integer;

      _Measures:Array Of TablatureMeasure;
      _MeasureCount:Integer;

      Procedure Save(Dest:Stream);

    Public
  End;

  Tablature = Class(TERRAObject)
    Protected
      _Title:String;
      _Artist:String;
      _Album:String;
      _Author:String;
      _Copyright:String;
      _Writer:String;

      _Channels:Array Of TablatureChannel;
      _ChannelCount:Integer;

      _Comments:Array Of String;
      _CommentCount:Integer;

      _Tempo:Integer;

      _MeasureHeaders:Array Of MeasureHeader;
      _MeasureHeaderCount:Integer;


      _Tracks:Array Of TablatureTrack;
      _TrackCount:Integer;

      Procedure writeStringByteSizeOfInteger(Dest:Stream; S:String);
      Procedure writeStringInteger(Dest:Stream; S:String);

      Function makeBeamEighthNoteBytes(ts:TimeSignature ):Cardinal;

    Public
      Procedure Save(Const FileName:String); Overload;
      Procedure Save(Dest:Stream); Overload;

      Procedure AddComment(Comment:String);
  End;

Implementation

Procedure Tablature.writeStringInteger(Dest:Stream; S:String);
Var
  N:Integer;
Begin
  N := Length(S);
  Dest.Write(@N, 4);
  If S<>'' Then
    Dest.Write(@S[1], N);
End;

Procedure Tablature.writeStringByteSizeOfInteger(Dest:Stream; S:String);
Var
  N:Integer;
Begin
  N := Length(S);
  Dest.Write(@N, 4);
  Dest.WriteString(S);
End;

Procedure Tablature.Save(Const FileName:String);
Var
  Dest:Stream;
Begin
  Dest := FileStream.Create(FileName);
  Self.Save(Dest);
  ReleaseObject(Dest);
End;

Procedure Tablature.Save(Dest:Stream);
Var
  I,J, N:Integer;
  C:Cardinal;
  B,Flags:Byte;
  S:String;
  TS:TimeSignature;
  CurrentTempo:Integer;
Begin
  Dest.WriteString(GP5_VERSION);

  writeStringByteSizeOfInteger(Dest, _Title);
  writeStringByteSizeOfInteger(Dest, '');
  writeStringByteSizeOfInteger(Dest, _Artist);
	writeStringByteSizeOfInteger(Dest, _Album);
	writeStringByteSizeOfInteger(Dest, _Author);
	writeStringByteSizeOfInteger(Dest, '');
	writeStringByteSizeOfInteger(Dest, _Copyright);
	writeStringByteSizeOfInteger(Dest, _Writer);
	writeStringByteSizeOfInteger(Dest, '');
	Dest.Write(@_CommentCount, 4);
  For I:=0 To Pred(_CommentCount) Do
			writeStringByteSizeOfInteger(Dest, _Comments[I]);

  // lyrics
//		while(it.hasNext()) 	TGTrack track = (TGTrack)it.next(); 	if(!track.getLyrics().isEmpty()){				lyricTrack = track;
  N := 0;
  Dest.Write(@N, 4); //( (lyricTrack == null)?0:lyricTrack.getNumber() );
  Dest.Write(@N, 4); //writeInt((lyricTrack == null)?0:lyricTrack.getLyrics().getFrom());
  writeStringByteSizeOfInteger(Dest, ''); //writeStringInteger((lyricTrack == null)?"":lyricTrack.getLyrics().getLyrics());
  For I:=0 To 3 Do
  Begin
    Dest.Write(@N, 4); //	writeInt((lyricTrack == null)?0:1);
		WriteStringInteger(Dest, '');
  End;

  // page setup
	N := 210;	Dest.Write(@N, 4); // Page width
  N := 297;	Dest.Write(@N, 4); // Page height
  N := 10;	Dest.Write(@N, 4);  // Margin left
  N := 10;	Dest.Write(@N, 4);  // Margin right
  N := 15;	Dest.Write(@N, 4);  // Margin top
  N := 10;	Dest.Write(@N, 4);  // Margin bottom
  N := 100;	Dest.Write(@N, 4);; // Score size percent

  B := $FF;	Dest.Write(@B, 1); // View flags
  B := $01;	Dest.Write(@B, 1); // View flags

  For I:=1 To PAGE_SETUP_LINES_SIZE Do
  Begin
    writeStringByteSizeOfInteger(Dest, PAGE_SETUP_LINES[i]);
  End;

	Dest.Write(@_Tempo, 4);
  N := 0; Dest.Write(@N, 4);
  B := 0; Dest.Write(@B, 1);

  // channells
  For I:=0 To Pred(_ChannelCount) Do
  Begin
    Dest.Write(@_Channels[i]._Program, 4);
    Dest.Write(@_Channels[i]._Volume, 1);
    Dest.Write(@_Channels[i]._Balance, 1);
    Dest.Write(@_Channels[i]._Chorus, 1);
    Dest.Write(@_Channels[i]._Reverb, 1);
    Dest.Write(@_Channels[i]._Phaser, 1);
    Dest.Write(@_Channels[i]._Tremolo, 1);
    N := 0; Dest.Write(@N, 2); // writeBytes(new byte[]{0,0});
  End;

  B := $FF;
  For I:=0 To 41 Do
    Dest.Write(@B, 1);

  Dest.Write(@_MeasureHeaderCount, 4);
  Dest.Write(@_TrackCount, 4);

  TS.Init();
  For I:=0 To Pred(_MeasureHeaderCount) Do
  Begin
    If (I>0) Then
    Begin
      B := 0; Dest.Write(@B, 1);
    End;

		Flags := 0;

		If (_MeasureHeaders[I].Number=1) Then
			Flags := Flags Or $40;

    If (_MeasureHeaders[I].Number=1) Or (_MeasureHeaders[I].Signature.Denominator<>TS.Denominator) Or (_MeasureHeaders[I].Signature.Numerator<>TS.Numerator) Then
    Begin
			Flags := Flags Or $01;
			flags := Flags Or $02;
		End;

		If (_MeasureHeaders[I].RepeatOpen) Then
			flags := Flags Or $04;

		If (_MeasureHeaders[I].RepeatClose>0) Then
			Flags := Flags Or $08;

		If (_MeasureHeaders[I].RepeatAlternative > 0) Then
			Flags := Flags Or $10;

		If (_MeasureHeaders[I].Marker>0) Then
			flags := Flags Or $20;

    Dest.Write(@Flags, 1);

		If ((flags And $01) <> 0) Then
      Dest.Write(@_MeasureHeaders[I].Signature.Numerator, 1);

		If ((flags And $02) <> 0) Then
      Dest.Write(@_MeasureHeaders[I].Signature.Denominator, 1);

		If ((flags And $08) <> 0) Then
      Dest.Write(@_MeasureHeaders[I].RepeatClose, 1);

		If ((flags And $20) <> 0) Then
			Dest.Write(@_MeasureHeaders[I].Marker, 1);

		if ((flags And $10) <> 0) Then
      Dest.Write(@_MeasureHeaders[I].RepeatAlternative, 1);

		if ((flags And $40) <> 0) Then
    Begin
      N := 0; Dest.Write(@N, 2);
    End;

		if ((flags And $01) <> 0) Then
    Begin
      C := makeBeamEighthNoteBytes(_MeasureHeaders[I].Signature);
      Dest.Write(@C, 4);
		End;

		If ((flags And $10) = 0) Then
    Begin
      B := 0; Dest.Write(@B, 1);
    End;

    Dest.Write(@_MeasureHeaders[I].TripletFeel, 1);

    TS := _MeasureHeaders[I].Signature;
  End;

  For I:=0 To Pred(_TrackCount) Do
    _Tracks[I].Save(Dest);

  N := 0; Dest.Write(@N, 2);

  CurrentTempo := _Tempo;
  For I:=0 To Pred(_MeasureHeaderCount) Do
  Begin
    For J:=0 To Pred(_TrackCount) Do
    Begin
      _Tracks[J]._Measures[I].Save(Dest, CurrentTempo<>_MeasureHeaders[I].Tempo);
      B := 0; Dest.Write(@B, 1);
    End;
    CurrentTempo := _MeasureHeaders[I].Tempo;
  End;
End;

Procedure TablatureTrack.Save(Dest: Stream);
Const
  Buf_Size = 44;
  Buf:Array[0..Pred(Buf_Size)] Of Shortint = ( 67, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
Var
  Value, I, N:Integer;
  Flags:Byte;
Begin
  Flags := 0;

  If (_Channel.A = PercussionChannel) Then
			flags := Flags Or $01;

  Dest.Write(@Flags, 1);
  Flags := Flags Or 8;
  Dest.Write(@Flags, 1); //writeUnsignedByte((8 | flags));
  Dest.WriteString(_Name);
  Dest.Write(@_StringCount, 4);

  For I:=0 To 6 Do
  Begin
    Value := 0;
    If (I<_StringCount) Then
    Begin
      Value := Self._Strings[I];
    End;
    Dest.Write(@Value, 4);
  End;

  N := 1; Dest.Write(@N, 4);
  Dest.Write(@_Channel.A, 4);
  Dest.Write(@_Channel.B, 4);
  N := 24; Dest.Write(@N, 4);
  Dest.Write(@_Offset, 4);
  _Color.A := 0;
  Dest.Write(@_Color, 4);

  Dest.Write(@Buf[0], Buf_Size);
End;

Procedure TablatureMeasure.Save(Dest: Stream; TempoChange: Boolean);
Var
  V,I,Count:Integer;
Begin
  For V:=0 To 1 Do
  Begin
    Count := 0;
    For I:=0 To Pred(_BeatCount) Do
    Begin
      If( v < _Beats[I]._VoiceCount) And (_Beats[I]._Voices[V]._NoteCount>0) Then
      Begin
        Inc(Count);
      End;
    End;

    If (Count > 0 ) Then
    Begin
      Dest.Write(@Count, 4);

      For I:=0 To Pred(_BeatCount) Do
      Begin
        If( v < _Beats[I]._VoiceCount) And (_Beats[I]._Voices[V]._NoteCount>0) Then
        Begin
          _Beats[I].Save(Dest, _Beats[I]._Voices[V], Self, TempoChange And (I=0));
        End;
      End;
    End Else
    Begin
      Dest.Write(@Count, 4);
      (*
      // Fill empty voices.
      Count := Signature.Numerator;
      beat = getFactory().newBeat();
      If (v < beat.VoiceCount) Then
      Begin
        voice = beat.getVoice( v );
        voice.getDuration().setValue( measure.getTimeSignature().getDenominator().getValue() );
        voice.setEmpty(true);

				writeInt( count );
				For I:=0 To Pred(Count) Do
          writeBeat(voice, voice.getBeat(), measure, ( changeTempo && i == 0 ));
      End;*)
    End;
  End;
End;

Procedure TablatureBeat.Save(Dest: Stream; Voice:TablatureVoice; Measure:TablatureMeasure; TempoChange: Boolean);
Var
  Flags,B:Byte;
  V,I,Count:Integer;
  playedNote:TablatureNote;
  Effect:TablatureNoteEffect;
Begin
  //		TGDuration duration = voice.getDuration();
  //TGNoteEffect effect = getFactory().newEffect();

  FillChar(Effect, SizeOf(Effect), 0);
	For I:=0 To Pred(Voice._NoteCount) Do
  Begin
    playedNote := Voice._Notes[i];
    If (playedNote._Effect.FadeIn) Then
      Effect.FadeIn := True;

    If (playedNote._Effect.TremoloBar) Then
      Effect.TremoloBar := playedNote._Effect.TremoloBar;

    If (playedNote._Effect.Tapping) Then
      Effect.Tapping := True;

    If (playedNote._Effect.Slapping) Then
      Effect.Slapping := True;

    If (playedNote._Effect.Popping) Then
      Effect.Popping := True;
  End;

  Flags := 0;

  If (Voice._Duration.Dotted Or Voice._Duration.DoubleDotted) Then
		 Flags := Flags Or $01;

  If (Voice._Index = 0)  And (Self.IsChordBeat()) Then
		Flags := Flags Or $02;

  (*If (voice.Index = 0= And (Self.isTextBeat()) Then
    Flags := Flags Or $04;*)

  If (Self._Stroke._Direction<>STROKE_NONE ) Then
    Flags := Flags Or $08
  Else
  If (effect.TremoloBar) Or (effect.Tapping) Or (effect.Slapping) Or (effect.Popping) Or (effect.FadeIn) Then
    Flags := Flags Or $08;

  If (TempoChange)  Then
    Flags := Flags Or $10;

  If (Voice._Duration.Division<>DIVISION_NORMAL) Then
    Flags := Flags Or $20;

  If (Voice._NoteCount<=0) Or (Voice._Rest) Then
    Flags := Flags Or $40;

  Dest.Write(@Flags, 1);

  If ((Flags And $40) <> 0) Then
  Begin
    If (Voice._NoteCount<=0) Then
      B := 0
    Else
      B := 2;

    Dest.Write(@B, 1);
  End;

  B := Voice.ParseDuration();
  Dest.Write(@B, 1);
  
		if ((flags And $20) <> 0) {
			writeInt(duration.getDivision().getEnters());
		}

		if ((flags And $02) <> 0) {
			writeChord(beat.getChord());
		}

		if ((flags And $04) <> 0) {
			writeText(beat.getText());
		}

		if ((flags And $08) <> 0) {
			writeBeatEffects(beat, effect);
		}
		
		if ((flags And $10) <> 0) {
			writeMixChange(measure.getTempo());
		}
		int stringFlags = 0;
		if (!voice.isRestVoice()) {
			for (int i = 0; i < voice.countNotes(); i++) {
				TGNote playedNote = voice.getNote(i);
				int string = (7 - playedNote.getString());
				stringFlags := Flags Or (1 << string);
			}
		}
		writeUnsignedByte(stringFlags);
		for (int i = 6; i >= 0; i--) {
			if ((stringFlags And (1 << i)) <> 0 ) {
				for( int n = 0; n < voice.countNotes(); n ++){
					TGNote playedNote = voice.getNote( n );
					if( playedNote.getString() == (6 - i + 1) ){
						writeNote(playedNote);
						break;
					}
				}
			}
		}
		
		skipBytes(2);
	}
	
	private void writeNote(TGNote note) throws IOException {
		//int flags = $20;
		int flags = ( $20 | $10 );
		
		if (note.getEffect().isVibrato()  ||
		    note.getEffect().isBend()     ||
		    note.getEffect().isSlide()    ||
		    note.getEffect().isHammer()   ||
		    note.getEffect().isLetRing()  ||
		    note.getEffect().isPalmMute() ||
		    note.getEffect().isStaccato() ||
		    note.getEffect().isTrill()    ||
		    note.getEffect().isGrace()    ||
		    note.getEffect().isHarmonic() ||
		    note.getEffect().isTremoloPicking()) {
		    flags := Flags Or $08;
		}
		if( note.getEffect().isGhostNote() ){
			flags := Flags Or $04;
		}
		if( note.getEffect().isHeavyAccentuatedNote() ){
			flags := Flags Or $02;
		}
		if( note.getEffect().isAccentuatedNote() ){
			flags := Flags Or $40;
		}
		writeUnsignedByte(flags);
		
		if ((flags And $20) <> 0) {
			int typeHeader = $01;
			if (note.isTiedNote()) {
				typeHeader = $02;
			}else if(note.getEffect().isDeadNote()){
				typeHeader = $03;
			}
			writeUnsignedByte(typeHeader);
		}
		if ((flags And $10) <> 0) {
			writeByte((byte)(((note.getVelocity() - TGVelocities.MIN_VELOCITY) / TGVelocities.VELOCITY_INCREMENT) + 1));
		}
		if ((flags And $20) <> 0) {
			writeByte((byte) note.getValue());
		}
		skipBytes(1);
		if ((flags And $08) <> 0) {
			writeNoteEffects(note.getEffect());
		}
	}
	
	private byte parseDuration(TGDuration duration) {
		byte value = 0;
		switch (duration.getValue()) {
		case TGDuration.WHOLE:
			value = -2;
			break;
		case TGDuration.HALF:
			value = -1;
			break;
		case TGDuration.QUARTER:
			value = 0;
			break;
		case TGDuration.EIGHTH:
			value = 1;
			break;
		case TGDuration.SIXTEENTH:
			value = 2;
			break;
		case TGDuration.THIRTY_SECOND:
			value = 3;
			break;
		case TGDuration.SIXTY_FOURTH:
			value = 4;
			break;
		}
		return value;
	}
	
	private void writeChord(TGChord chord) throws IOException{
		this.writeBytes( new byte[] {1,1,0,0,0,12,0,0,-1,-1,-1,-1,0,0,0,0,0} );
		writeStringByte( chord.getName(), 21);
		skipBytes(4);
		writeInt( chord.getFirstFret() );
		for (int i = 0; i < 7; i++) {
			writeInt( (i < chord.countStrings() ? chord.getFretValue(i) : -1 ) ) ;
		}
		this.skipBytes(32);
	}
	
	private void writeBeatEffects(TGBeat beat,TGNoteEffect effect) throws IOException{
		int flags1 = 0;
		int flags2 = 0;
		
		if(effect.isFadeIn()){
			flags1 := Flags Or $10;
		}
		if(effect.isTapping() || effect.isSlapping() || effect.isPopping()){
			flags1 := Flags Or $20;
		}
		if(effect.isTremoloBar()){
			flags2 := Flags Or $04;
		}
		if(beat.getStroke().getDirection() <> TGStroke.STROKE_NONE){
			flags1 := Flags Or $40;
		}
		writeUnsignedByte(flags1);
		writeUnsignedByte(flags2);
		
		if ((flags1 And $20) <> 0) {
			if(effect.isTapping()){
				writeUnsignedByte(1);
			}else if(effect.isSlapping()){
				writeUnsignedByte(2);
			}else if(effect.isPopping()){
				writeUnsignedByte(3);
			}
		}
		if ((flags2 And $04) <> 0) {
			writeTremoloBar(effect.getTremoloBar());
		}
		if ((flags1 And $40) <> 0) {
			writeUnsignedByte( (beat.getStroke().getDirection() == TGStroke.STROKE_UP ? toStrokeValue(beat.getStroke()) : 0 ) );
			writeUnsignedByte( (beat.getStroke().getDirection() == TGStroke.STROKE_DOWN ? toStrokeValue(beat.getStroke()) : 0 ) );
		}
	}
	
	private void writeNoteEffects(TGNoteEffect effect) throws IOException {
		int flags1 = 0;
		int flags2 = 0;
		if (effect.isBend()) {
			flags1 := Flags Or $01;
		}
		if (effect.isHammer()) {
			flags1 := Flags Or $02;
		}
		if (effect.isLetRing()) {
			flags1 := Flags Or $08;
		}
		if (effect.isGrace()) {
			flags1 := Flags Or $10;
		}
		if (effect.isStaccato()) {
			flags2 := Flags Or $01;
		}
		if (effect.isPalmMute()) {
			flags2 := Flags Or $02;
		}
		if (effect.isTremoloPicking()) {
			flags2 := Flags Or $04;
		}
		if (effect.isSlide()) {
			flags2 := Flags Or $08;
		}
		if (effect.isHarmonic()) {
			flags2 := Flags Or $10;
		}
		if (effect.isTrill()) {
			flags2 := Flags Or $20;
		}
		if (effect.isVibrato()) {
			flags2 := Flags Or $40;
		}
		writeUnsignedByte(flags1);
		writeUnsignedByte(flags2);
		if ((flags1 And $01) <> 0) {
			writeBend(effect.getBend());
		}
		
		if ((flags1 And $10) <> 0) {
			writeGrace(effect.getGrace());
		}
		
		if ((flags2 And $04) <> 0) {
			writeTremoloPicking(effect.getTremoloPicking());
		}
		
		if ((flags2 And $08) <> 0) {
			writeByte((byte)1);
		}
		
		if ((flags2 And $10) <> 0) {
			writeByte((byte)1);
		}
		
		if ((flags2 And $20) <> 0) {
			writeTrill(effect.getTrill());
		}

	}
	
	private void writeBend(TGEffectBend bend) throws IOException {
		int points = bend.getPoints().size();
		writeByte((byte) 1);
		writeInt(0);
		writeInt(points);
		for (int i = 0; i < points; i++) {
			TGEffectBend.BendPoint point = (TGEffectBend.BendPoint) bend.getPoints().get(i);
			writeInt( (point.getPosition() * GP_BEND_POSITION / TGEffectBend.MAX_POSITION_LENGTH) );
			writeInt( (point.getValue() * GP_BEND_SEMITONE / TGEffectBend.SEMITONE_LENGTH) );
			writeByte((byte) 0);
		}
	}
	
	private void writeTremoloBar(TGEffectTremoloBar tremoloBar) throws IOException {
		int points = tremoloBar.getPoints().size();
		writeByte((byte) 1);
		writeInt(0);
		writeInt(points);
		for (int i = 0; i < points; i++) {
			TGEffectTremoloBar.TremoloBarPoint point = (TGEffectTremoloBar.TremoloBarPoint) tremoloBar.getPoints().get(i);
			writeInt( (point.getPosition() * GP_BEND_POSITION / TGEffectBend.MAX_POSITION_LENGTH) );
			writeInt( (point.getValue() * (GP_BEND_SEMITONE * 2)) );
			writeByte((byte) 0);
		}
	}
	
	private void writeGrace(TGEffectGrace grace) throws IOException {
		writeUnsignedByte(grace.getFret());
		writeUnsignedByte(((grace.getDynamic() - TGVelocities.MIN_VELOCITY) / TGVelocities.VELOCITY_INCREMENT) + 1);
		if(grace.getTransition() == TGEffectGrace.TRANSITION_NONE){
			writeUnsignedByte(0);
		}
		else if(grace.getTransition() == TGEffectGrace.TRANSITION_SLIDE){
			writeUnsignedByte(1);
		}
		else if(grace.getTransition() == TGEffectGrace.TRANSITION_BEND){
			writeUnsignedByte(2);
		}
		else if(grace.getTransition() == TGEffectGrace.TRANSITION_HAMMER){
			writeUnsignedByte(3);
		}
		writeUnsignedByte(grace.getDuration());
		writeUnsignedByte( (grace.isDead() ? $01 : 0) | (grace.isOnBeat() ? $02 : 0) );
	}
	
	private void writeTrill(TGEffectTrill trill) throws IOException {
		writeByte((byte)trill.getFret());
		if(trill.getDuration().getValue() == TGDuration.SIXTEENTH){
			writeByte((byte)1);
		}else if(trill.getDuration().getValue() == TGDuration.THIRTY_SECOND){
			writeByte((byte)2);
		}else if(trill.getDuration().getValue() == TGDuration.SIXTY_FOURTH){
			writeByte((byte)3);
		}
	}
	
	private void writeTremoloPicking(TGEffectTremoloPicking tremoloPicking) throws IOException{
		if(tremoloPicking.getDuration().getValue() == TGDuration.EIGHTH){
			writeByte((byte)1);
		}else if(tremoloPicking.getDuration().getValue() == TGDuration.SIXTEENTH){
			writeByte((byte)2);
		}else if(tremoloPicking.getDuration().getValue() == TGDuration.THIRTY_SECOND){
			writeByte((byte)3);
		}
	}
	
	private void writeText(TGText text) throws IOException {
		writeStringByteSizeOfInteger(text.getValue());
	}
	
	private void writeMixChange(TGTempo tempo) throws IOException {
		writeByte((byte) $ff);
		for(int i = 0; i < 16; i++){
			writeByte((byte) $ff);
		}
		writeByte((byte) $ff); //volume
		writeByte((byte) $ff); //int pan
		writeByte((byte) $ff); //int chorus
		writeByte((byte) $ff); //int reverb
		writeByte((byte) $ff); //int phaser
		writeByte((byte) $ff); //int tremolo
		writeStringByteSizeOfInteger(""); //tempo name
		writeInt((tempo <> null)?tempo.getValue():-1); //tempo value
		if((tempo <> null)){
			skipBytes(1);
		}
		writeByte((byte)1);
		writeByte((byte)$ff);
	}
	
	private void writeMarker(TGMarker marker) throws IOException {
		writeStringByteSizeOfInteger(marker.getTitle());
		writeColor(marker.getColor());
	}
	
	private void writeColor(TGColor color) throws IOException {
		writeUnsignedByte(color.getR());
		writeUnsignedByte(color.getG());
		writeUnsignedByte(color.getB());
		writeByte((byte)0);
	}
	
	private TGChannel[] makeChannels(TGSong song) {
		TGChannel[] channels = new TGChannel[64];
		for (int i = 0; i < channels.length; i++) {
			channels[i] = getFactory().newChannel();
			channels[i].setProgram((short)24);
			channels[i].setVolume((short)13);
			channels[i].setBalance((short)8);
			channels[i].setChorus((short)0);
			channels[i].setReverb((short)0);
			channels[i].setPhaser((short)0);
			channels[i].setTremolo((short)0);
		}

		Iterator it = song.getChannels();
		while (it.hasNext()) {
			TGChannel tgChannel = (TGChannel) it.next();
			GMChannelRoute gmChannelRoute = getChannelRoute(tgChannel.getChannelId());
			channels[gmChannelRoute.getChannel1()].setProgram(tgChannel.getProgram());
			channels[gmChannelRoute.getChannel1()].setVolume(tgChannel.getVolume());
			channels[gmChannelRoute.getChannel1()].setBalance(tgChannel.getBalance());
			channels[gmChannelRoute.getChannel2()].setProgram(tgChannel.getProgram());
			channels[gmChannelRoute.getChannel2()].setVolume(tgChannel.getVolume());
			channels[gmChannelRoute.getChannel1()].setBalance(tgChannel.getBalance());
		}
		
		return channels;
	}

	Function makeBeamEighthNoteBytes(ts:TimeSignature ):Cardinal;
		byte[] bytes = new byte[]{0,0,0,0};
		if( ts.getDenominator().getValue() <= TGDuration.EIGHTH ){
			int eighthsInDenominator = (TGDuration.EIGHTH / ts.getDenominator().getValue());
			int total = (eighthsInDenominator * ts.getNumerator());
			int byteValue = ( total / 4 );
			int missingValue = ( total - (4 * byteValue) );
			for( int i = 0 ; i < bytes.length; i ++ ){
				bytes[i] = (byte)byteValue;
			}
			if( missingValue > 0 ){
				bytes[0] += missingValue;
			}
		}
		return bytes;
	}
	
	private int toStrokeValue( TGStroke stroke ){
		if( stroke.getValue() == TGDuration.SIXTY_FOURTH ){
			return 2;
		}
		if( stroke.getValue() == TGDuration.THIRTY_SECOND ){
			return 3;
		}
		if( stroke.getValue() == TGDuration.SIXTEENTH ){
			return 4;
		}
		if( stroke.getValue() == TGDuration.EIGHTH ){
			return 5;
		}
		if( stroke.getValue() == TGDuration.QUARTER ){
			return 6;
		}
		return 2;
	}
	
	private byte toChannelByte(short s){
		return  (byte) ((s + 1) / 8);
	}
	
	private List toCommentLines( String comments ){
		List lines = new ArrayList();
		
		String line = comments;
		while( line.length() > Byte.MAX_VALUE ) {
			String subline = line.substring(0, Byte.MAX_VALUE);
			lines.add( subline );
			line = line.substring( Byte.MAX_VALUE );
		}
		lines.add( line );
		
		return lines;
	}
}