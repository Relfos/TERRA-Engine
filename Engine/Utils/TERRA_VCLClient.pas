Unit TERRA_VCLClient;

{$I terra.inc}
Interface
Uses Classes, Forms, ExtCtrls, TERRA_String, TERRA_Application, TERRA_Client;

Type
  VCLClient = Class(AppClient)
      Protected
        _Timer:TTimer;

        Procedure TimerTick(Sender: TObject);

      Public
        Constructor Create(Target:TComponent);
        Procedure Release; Override;
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
  Application.Instance.Run();
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


End.
