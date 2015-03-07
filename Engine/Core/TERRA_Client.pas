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
 * TERRA_Client
 * Implements a generic Application client that can respond to input events and other events, in a portable way
 ***********************************************************************************************************************
}

{$IFDEF OXYGENE}
namespace TERRA;
{$ELSE}
Unit TERRA_Client;
{$I terra.inc}
{$ENDIF}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Input;

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
Type

  AppClient = Class(TERRAObject)
    Public
      Keys:InputState;

      Procedure SelectResolution3D(Var Width, Height:Integer); Virtual;
      Procedure SelectResolution2D(Var Width, Height:Integer; Var Scale:Single); Virtual;

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

      Procedure OnIAP_Error(ErrorCode:Integer); Virtual;
      Procedure OnIAP_Purchase(Const ID:TERRAString); Overload; Virtual;
      Procedure OnIAP_Purchase(Credits:Integer); Overload; Virtual;
      Procedure OnIAP_External(Const PurchaseID:TERRAString; UserData:Pointer); Virtual;

      Procedure OnAPIResult(API, Code:Integer); Virtual;

      Procedure OnFatalError(Const ErrorMsg:TERRAString); Virtual;

      Procedure OnContextLost(); Virtual;

			Procedure OnCreate; Virtual;
			Procedure OnDestroy; Virtual;
			Procedure OnIdle; Virtual;
			Procedure OnStateChange(State:Integer); Virtual;

      Procedure OnGesture(StartX, StartY, EndX, EndY, GestureType:Integer; Delta:Single); Virtual;

      Function GetTitle:TERRAString; Virtual;
      Function GetHandle:Cardinal; Virtual;
      Function GetWidth:Word; Virtual;
      Function GetHeight:Word; Virtual;
      Function GetFullScreen:Boolean; Virtual;
      Function GetVSync:Boolean; Virtual;
      Function GetIgnoreCursor:Boolean; Virtual;
      Function GetHidden:Boolean; Virtual;
      Function GetAntialiasSamples:Integer; Virtual;
      Function GetLogging:Boolean; Virtual;

      Function IsConsole():Boolean; Virtual;

      Function GetAppID:TERRAString; Virtual;

      Function GetAdMobBannerID:TERRAString; Virtual;
      Function GetAdMobInterstitialID:TERRAString; Virtual;

      Function GetAdBuddizID:TERRAString; Virtual;

      Function GetFlurryID:TERRAString; Virtual;
      Function GetTestFlightID:TERRAString; Virtual;
      Function GetFacebookID:TERRAString; Virtual;
      Function GetBillingID:TERRAString; Virtual;

      Function GetFortumoID:TERRAString; Virtual;
      Function GetFortumoSecret:TERRAString; Virtual;

      Function GetChartboostID:TERRAString; Virtual;
      Function GetChartboostSecret:TERRAString; Virtual;

      Function GetTapjoyID:TERRAString; Virtual;
      Function GetTapjoySecret:TERRAString; Virtual;

      Function GetVungleID:TERRAString; Virtual;
  End;

  ConsoleClient = Class(AppClient)
      Function GetWidth:Word; Override;
      Function GetHeight:Word; Override;
      Function GetVSync:Boolean; Override;

      Function IsConsole():Boolean; Override;
  End;


Function IsMouseInput(Key:Integer):Boolean;
Function IsKeyboardInput(Key:Integer):Boolean;
Function IsGamepadInput(Key:Integer):Boolean;

Implementation
Uses TERRA_Application, TERRA_OS, TERRA_Log;

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

Procedure AppClient.OnIAP_Purchase(Const ID:TERRAString);
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

Procedure AppClient.SelectResolution2D(var Width, Height: Integer; Var Scale:Single);
Begin
End;

Procedure AppClient.OnOrientation(Orientation: Integer);
Begin
  Application.Instance.SetOrientation(Orientation);
End;

Function AppClient.GetAntialiasSamples: Integer;
Begin
  Result := 0;
End;

Function AppClient.GetAppID:TERRAString;
Begin
  Result := '0001';
End;

Function AppClient.GetBillingID:TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetFacebookID:TERRAString;
Begin
  Result := '';
End;

function AppClient.GetFlurryID:TERRAString;
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


Procedure AppClient.OnFatalError(const ErrorMsg: TERRAString);
Begin

End;

Procedure AppClient.OnIAP_External(Const PurchaseID:TERRAString; UserData:Pointer);
Begin
  Self.OnIAP_Error(-1);
End;

Function AppClient.GetAdMobBannerID:TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetAdMobInterstitialID:TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetLogging: Boolean;
Begin
  Result := True;
End;

Function AppClient.GetFortumoID:TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetFortumoSecret:TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetTestFlightID:TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetTitle:TERRAString;
Begin
  Result := GetProgramName();
End;

Function AppClient.GetVSync: Boolean;
Begin
  Result := False;
End;

Function AppClient.GetTapjoyID: TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetTapjoySecret: TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetChartboostID: TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetChartboostSecret: TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetAdBuddizID: TERRAString;
Begin
  Result := '';
End;

Function AppClient.GetVungleID: TERRAString;
Begin
  Result := '';
End;

Function AppClient.IsConsole: Boolean;
Begin
  Result := False;
End;

{ ConsoleClient }

Function ConsoleClient.IsConsole: Boolean;
Begin
  Result := True;
End;

Function ConsoleClient.GetVSync: Boolean;
Begin
  Result := False;
End;

Function ConsoleClient.GetWidth: Word;
Begin
  Result := 0;
End;

Function ConsoleClient.GetHeight: Word;
Begin
  Result := 0;
End;


End.
