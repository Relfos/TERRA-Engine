Unit TERRA_ProgressNotifier;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils;

Type
  ProgressNotifier = Class(TERRAObject)
    Protected
      _CurrentPhase:Integer;
      _PhaseCount:Integer;

    Public
      Procedure Reset(Count:Integer);
      Procedure Notify(Value:Single);
      Procedure OnProgress(Progress:Integer); Virtual; Abstract;

      Procedure NextPhase();

      Property PhaseCount:Integer Read _PhaseCount Write _PhaseCount;
      Property CurrentPhase:Integer Read _CurrentPhase Write _CurrentPhase;
  End;

Implementation

{ ProgressNotifier }
Procedure ProgressNotifier.Reset(Count:Integer);
Begin
  _CurrentPhase := 0;
  _PhaseCount := Count;
End;

Procedure ProgressNotifier.NextPhase();
Begin
  If (_CurrentPhase<Pred(_PhaseCount)) Then
    Inc(_CurrentPhase);
End;

Procedure ProgressNotifier.Notify(Value:Single);
Begin
  If (_CurrentPhase<0) Then
    _CurrentPhase := 0;

  If (_PhaseCount<=0) Then
    _PhaseCount := 1;

  If (Value<0) Then
    Value := 0
  Else
  If (Value>1) Then
    Value := 1;

  If (_PhaseCount>1) Then
    Value := (_CurrentPhase/_PhaseCount) + Value * (1/_PhaseCount);

  Self.OnProgress(Trunc(Value*100));
End;

End.

