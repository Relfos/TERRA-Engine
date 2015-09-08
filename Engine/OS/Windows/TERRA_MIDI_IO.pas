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
 * TERRA_MIDI_IO
 * Implements MIDI input/output in Windows
 ***********************************************************************************************************************
}

Unit TERRA_MIDI_IO;

// http://www.blitter.com/~russtopia/MIDI/~jglatt/tech/lowmidi.htm

{$I terra.inc}

{s$DEFINE USE_COREMIDI}

Interface


Function MIDI_Out(Event:Cardinal):Boolean;

Implementation

Uses TERRA_Object, TERRA_Utils, TERRA_Engine, TERRA_Multimedia, TERRA_Log, TERRA_MIDI;

Var
   _midiInitialized:Boolean = False;
   _midiOut:HMIDIOUT;

Const
  DEFAULT_MIDI_DEVICE = 0;

Function MIDI_Init():Boolean;
Var
  Status:Integer;
Begin
     // Open the MIDI Mapper
     Status := midiOutOpen(_midiOut, DEFAULT_MIDI_DEVICE, 0, 0, CALLBACK_NULL);
     If (Status<>0) Then
     Begin
          Engine.Log.Write(logError, 'MIDI', 'Error opening midi device ' +  IntegerProperty.Stringify(status));
          Result := False;
          Exit;
     End;


    Result := True;

    _midiInitialized := True;
End;

Function MIDI_Out(Event:Cardinal):Boolean;
Var
  Status:Integer;
  Opcode, Arg1, Arg2:Byte;
Begin
  If (Not _midiInitialized) Then
  Begin
    If (Not MIDI_Init()) Then
    Begin
      Result := False;
      Exit;
    End;
  End;

  midiOutShortMsg(_midiOut, Event);

  Result := True
End;

End.

