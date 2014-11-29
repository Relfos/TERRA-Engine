{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}
Unit TERRA_Client;
{$I terra.inc}
{$ENDIF}

Interface
Uses TERRA_Image;

Const
  apiFacebook = 1;
  apiTapjoy   = 2;

  facebookPostSucess        = 1;
  facebookConnectionError   = 2;
  facebookLikeSucess        = 3;
  facebookLikeError         = 4;
  facebookAuthError         = 5;

  tapjoyUpdateError        = 30;
  tapjoyConnectionError    = 31;
  tapjoySpendError         = 32;
  tapjoySpendSuccess       = 33;
  tapjoyOffersError        = 34;
  tapjoyVideoUnvailable    = 35;
  tapjoyVideoSuccess       = 36;
  tapjoyOfferSuccess       = 37;

  MaxKeys = 512;

Type
	PInputState = ^InputState;
	InputState = Array[0..Pred(MaxKeys)] Of Boolean;

  AppClient = Class
    Public
      Keys:PInputState;

      Procedure SelectResolution3D(Var Width, Height:Integer); Virtual;
      Procedure SelectResolution2D(Var Width, Height:Integer); Virtual;

			Procedure OnKeyDown(Key:Word); Virtual;
			Procedure OnKeyUp(Key:Word); Virtual;
			Procedure OnKeyPress(Key:Word); Virtual;

			Procedure OnMouseDown(X,Y:Integer;Button:Word); Virtual;
			Procedure OnMouseUp(X,Y:Integer;Button:Word); Virtual;
			Procedure OnMouseMove(X,Y:Integer); Virtual;
			Procedure OnMouseWheel(X,Y:Integer; Delta:Integer); Virtual;

			Procedure OnAccelerometer(X,Y,Z:Single); Virtual;
			Procedure OnGyroscope(X,Y,Z:Single); Virtual;
			Procedure OnCompass(Heading, Pitch, Roll:Single); Virtual;

      Procedure OnOrientation(Orientation:Integer); Virtual;

      Procedure OnAdClick(X,Y:Integer); Virtual;

      Procedure OnIAP_Error(ErrorCode:Integer); Virtual;
      Procedure OnIAP_Purchase(ID:AnsiString); Overload; Virtual;
      Procedure OnIAP_Purchase(Credits:Integer); Overload; Virtual;
      Procedure OnIAP_External(Const PurchaseID:AnsiString; UserData:Pointer); Virtual;

      Function ServeAd(Width,Height:Integer):Image; Virtual;

      Procedure OnAPIResult(API, Code:Integer); Virtual;

      Procedure OnFatalError(Const ErrorMsg:AnsiString); Virtual;

      Procedure OnContextLost(); Virtual;

			Procedure OnCreate; Virtual;
			Procedure OnDestroy; Virtual;
			Procedure OnIdle; Virtual;
			Procedure OnStateChange(State:Integer); Virtual;

      Procedure OnGesture(StartX, StartY, EndX, EndY, GestureType:Integer; Delta:Single); Virtual;

      Function GetTitle:AnsiString; Virtual;
      Function GetHandle:Cardinal; Virtual;
      Function GetWidth:Word; Virtual;
      Function GetHeight:Word; Virtual;
      Function GetFullScreen:Boolean; Virtual;
      Function GetVSync:Boolean; Virtual;
      Function GetIgnoreCursor:Boolean; Virtual;
      Function GetHidden:Boolean; Virtual;
      Function GetAntialiasSamples:Integer; Virtual;
      Function GetLogging:Boolean; Virtual;

      Function GetAppID:AnsiString; Virtual;
      Function GetAdMobID:AnsiString; Virtual;

      Function GetInterstitialID:AnsiString; Virtual;
      Function GetFlurryID:AnsiString; Virtual;
      Function GetTestFlightID:AnsiString; Virtual;
      Function GetFacebookID:AnsiString; Virtual;
      Function GetBillingID:AnsiString; Virtual;

      Function GetFortumoID:AnsiString; Virtual;
      Function GetFortumoSecret:AnsiString; Virtual;

      Function GetChartboostID:AnsiString; Virtual;
      Function GetChartboostSecret:AnsiString; Virtual;

      Function GetTapjoyID:AnsiString; Virtual;
      Function GetTapjoySecret:AnsiString; Virtual;
  End;


Function IsMouseInput(Key:Integer):Boolean;
Function IsKeyboardInput(Key:Integer):Boolean;
Function IsGamepadInput(Key:Integer):Boolean;

Implementation
Uses TERRA_Utils, TERRA_Application, TERRA_OS, TERRA_Log;

Function IsMouseInput(Key:Integer):Boolean;
Begin
  Result := (Key >= keyMouseLeft) And (Key<=keyMouseMiddle);
End;

Function IsKeyboardInput(Key:Integer):Boolean;
Begin
  Result := (Key <=255);
End;

Function IsGamepadInput(Key:Integer):Boolean;
Begin
  Result := (Key>=keyGamepadIndex) And (Key<keyMouseLeft);
End;

{ Client }
Procedure AppClient.OnAccelerometer(X, Y, Z: Single);
Begin

End;

Procedure AppClient.OnCreate;
Begin

End;

Procedure AppClient.OnDestroy;
Begin

End;

Procedure AppClient.OnAPIResult(API, Code:Integer);
Begin

End;

Procedure AppClient.OnGesture(StartX, StartY, EndX, EndY, GestureType: Integer; Delta:Single);
Begin

End;

Procedure AppClient.OnIAP_Error(ErrorCode:Integer);
Begin
  Log(logWarning, 'Client', 'Please implement Client.OnIAP_Cancel, error code = '+IntToString(ErrorCode));
End;

Procedure AppClient.OnIAP_Purchase(ID:AnsiString);
Begin
  Log(logWarning, 'Client', 'Please implement Client.OnIAP_Purchase, product ID = '+ID);
End;

Procedure AppClient.OnIAP_Purchase(Credits: Integer);
Begin
  Log(logWarning, 'Client', 'Please implement Client.OnIAP_Purchase, credits  = '+IntToString(Credits));
End;

Procedure AppClient.OnIdle;
Begin

End;

Procedure AppClient.OnKeyDown(Key: Word);
Begin
  If Key = keyEscape  Then
    Application.Instance.Terminate;
End;

Procedure AppClient.OnKeyPress(Key:Word);
Begin

End;

Procedure AppClient.OnKeyUp(Key: Word);
Begin

End;

Procedure AppClient.OnContextLost;
Begin
  // Do nothing
End;

Procedure AppClient.OnMouseDown(X, Y: Integer; Button: Word);
Begin
//  UI.Instance.OnMouseDown(X, Y, Button);
End;

Procedure AppClient.OnMouseMove(X, Y: Integer);
Begin
//  UI.Instance.OnMouseMove(X, Y);
End;

Procedure AppClient.OnMouseUp(X, Y: Integer; Button: Word);
Begin
//  UI.Instance.OnMouseUp(X, Y, Button);
End;

Procedure AppClient.OnMouseWheel(X,Y:Integer; Delta: Integer);
Begin
//  UI.Instance.OnMouseWheel(Delta);
End;

Procedure AppClient.OnStateChange(State: Integer);
Begin

End;

Procedure AppClient.OnCompass(Heading, Pitch, Roll: Single);
Begin
End;

Procedure AppClient.OnGyroscope(X, Y, Z: Single);
Begin
End;

Procedure AppClient.SelectResolution3D(var Width, Height: Integer);
Begin
End;

Procedure AppClient.SelectResolution2D(var Width, Height: Integer);
Begin
End;

Procedure AppClient.OnOrientation(Orientation: Integer);
Begin
  Application.Instance.SetOrientation(Orientation);
End;

Procedure AppClient.OnAdClick(X, Y: Integer);
Begin
  Log(logWarning, 'Client', 'Please implement Client.OnAdClick');
End;

Function AppClient.ServeAd(Width, Height: Integer):Image;
Begin
  Result := Nil;
End;

Function AppClient.GetAdMobID:AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetAntialiasSamples: Integer;
Begin
  Result := 0;
End;

Function AppClient.GetAppID:AnsiString;
Begin
  Result := '0001';
End;

Function AppClient.GetBillingID:AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetFacebookID:AnsiString;
Begin
  Result := '';
End;

function AppClient.GetFlurryID:AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetFullScreen: Boolean;
Begin
  Result := False;
End;

Function AppClient.GetHandle: Cardinal;
Begin
  Result := 0;
End;

Function AppClient.GetWidth: Word;
Begin
  Result := 960;
End;

Function AppClient.GetHeight: Word;
Begin
  Result := 640;
End;

Function AppClient.GetHidden: Boolean;
Begin
  Result := False;
End;

Function AppClient.GetIgnoreCursor: Boolean;
Begin
  Result := True;
End;


Procedure AppClient.OnFatalError(const ErrorMsg: AnsiString);
Begin

End;

Procedure AppClient.OnIAP_External(Const PurchaseID:AnsiString; UserData:Pointer);
Begin
  Self.OnIAP_Error(-1);
End;

Function AppClient.GetInterstitialID:AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetLogging: Boolean;
Begin
  Result := True;
End;

Function AppClient.GetFortumoID:AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetFortumoSecret:AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetTestFlightID:AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetTitle:AnsiString;
Begin
  Result := GetProgramName();
End;

Function AppClient.GetVSync: Boolean;
Begin
  Result := False;
End;

Function AppClient.GetTapjoyID: AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetTapjoySecret: AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetChartboostID: AnsiString;
Begin
  Result := '';
End;

Function AppClient.GetChartboostSecret: AnsiString;
Begin
  Result := '';
End;


End.