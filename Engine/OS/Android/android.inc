procedure internal_initializeunits; external name 'FPC_INITIALIZEUNITS';

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationInit(env, obj:Pointer; isOuya:Integer); stdcall; export;  [public];
Begin
	{$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP START');{$ENDIF}
	//SetGlobalEnv(Env);

	internal_initializeunits();
	ApplicationInit();
End;

Var
	_WasRunning:Boolean = False;
	_Destroyed:Boolean = False;
             
Procedure Java_com_pascal_terra_TERRALibrary_ApplicationShutdown(env, obj:Pointer); stdcall; export;  [public];
Begin
	//SetGlobalEnv(Env);
	{$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP END');{$ENDIF}
	Application.Instance.Terminate();
	_Destroyed := True;
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationResize(env, obj:Pointer; w,h:Integer); stdcall; export;  [public];
Begin
	If (_Destroyed) Then
		Exit;
  //SetGlobalEnv(Env);
  //Log(logDebug, 'App', 'TERRA APP RESIZE w:'+IntToString(W)+' h:'+IntToString(H));
  Application.Instance.AddCoordEvent(eventWindowResize, W, H, 0);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationViewport(env, obj:Pointer; x1,y1,x2,y2:Integer); stdcall; export;  [public];
Begin
	If (_Destroyed) Then
		Exit;
  //SetGlobalEnv(Env);
  //Log(logDebug, 'App', 'TERRA APP RESIZE w:'+IntToString(W)+' h:'+IntToString(H));
  Application.Instance.AddRectEvent(eventViewport, x1, y1, x2, y2);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationSetState(env, obj:Pointer; state:Integer); stdcall; export;  [public];
Begin
  //SetGlobalEnv(Env);
  Application.Instance.Client.OnStateChange(State);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationSetOrientation(env, obj:Pointer; Orientation:Integer); stdcall; export;  [public];
Begin
  //SetGlobalEnv(Env);
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA ORIENTATION '+IntToString(Orientation));{$ENDIF}  
  //Log(logDebug, 'App', 'TERRA APP RESIZE w:'+IntToString(W)+' h:'+IntToString(H));
	Application.Instance.AddValueEvent(eventOrientation, Orientation);
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationGetOrientation(env, obj:Pointer):Integer; stdcall; export;  [public];
Begin
	//SetGlobalEnv(Env);
  //Log(logDebug, 'App', 'TERRA APP RESIZE w:'+IntToString(W)+' h:'+IntToString(H));
	Result := Application.Instance.Orientation;
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationUpdate(env, obj:Pointer):Boolean; stdcall; export;  [public];
Begin
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP BEGIN UPDATE');{$ENDIF}
  _WasRunning := Application.Instance.Run();
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP FINISHD UPDATE');{$ENDIF} 
  Result := _WasRunning;
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationPause(env, obj:Pointer); stdcall; export;  [public];
Begin
  //SetGlobalEnv(Env);
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP PAUSE');{$ENDIF}
  AndroidApplication(Application.Instance).SetSuspend(True);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationResume(env, obj:Pointer); stdcall; export;  [public];
Begin
  //SetGlobalEnv(Env);
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP RESUME');{$ENDIF}
  If _WasRunning Then
	AndroidApplication(Application.Instance).SetSuspend(False);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationContextLost(env, obj:Pointer); stdcall; export;  [public];
Begin
	//SetGlobalEnv(Env);	
	Application.Instance.AddValueEvent(eventContextLost, 0);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationKeyPress(env, obj:Pointer; key:Integer); stdcall; export;  [public];
Begin
	Application.Instance.AddValueEvent(eventKeyPress, key);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationKeyDown(env, obj:Pointer; key:Integer); stdcall; export;  [public];
Begin
	Application.Instance.AddValueEvent(eventKeyDown, key);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationKeyUp(env, obj:Pointer; key:Integer); stdcall; export;  [public];
Begin
	Application.Instance.AddValueEvent(eventKeyUp, key);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationOnAccelerometer(env, obj:Pointer; x,y,z:Single); stdcall; export;  [public];
Begin
  Application.Instance.AddVectorEvent(eventAccelerometer, x,y ,z);  
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationOnGyroscope(env, obj:Pointer; x,y,z:Single); stdcall; export;  [public];
Begin
  Application.Instance.AddVectorEvent(eventGyroscope, x,y ,z);  
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationOnCompass(env, obj:Pointer; heading, Pitch, Roll:Single); stdcall; export;  [public];
Begin
  Application.Instance.AddVectorEvent(eventGyroscope, heading, Pitch, Roll);
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationTouchMove(env, obj:Pointer; x,y:Integer); stdcall; export;  [public];
Begin
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP MOVE x:'+IntToString(X)+' y:'+IntToString(Y));{$ENDIF}
  Application.Instance.AddCoordEvent(eventMouseMove, x,y, 0);  
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationTouchBegin(env, obj:Pointer; x,y:Integer); stdcall; export;  [public];
Begin	
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP TOUCH BEGIN x:'+IntToString(X)+' y:'+IntToString(Y));{$ENDIF}
  Application.Instance.AddCoordEvent(eventMouseDown, x,y, keyMouseLeft);  
  Log(logDebug, 'App', 'TERRA APP TOUCH BEGIN DONE');
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationTouchEnd(env, obj:Pointer; x,y:Integer); stdcall; export;  [public];
Begin
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP TOUCH END x:'+IntToString(X)+' y:'+IntToString(Y));{$ENDIF}
  Application.Instance.AddCoordEvent(eventMouseUp, x,y, keyMouseLeft);  
  {$IFDEF DEBUG_CORE}Log(logDebug, 'App', 'TERRA APP TOUCH END DONE');{$ENDIF}
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationIAPConfirm(env, obj, str:Pointer); stdcall; export;  [public];
Var
	ID:String;
Begin
  If Not Application.Instance.CanReceiveEvents Then
    Exit;

  ID := JavaToString(Str);
  Log(logDebug, 'App', 'IAP purchased: '+ID);
  Application.Instance.AddStringEvent(eventIAPPurchase, ID);    
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationIAPError(env, obj:Pointer; ErrorCode:Integer); stdcall; export;  [public];
Begin
  If Not Application.Instance.CanReceiveEvents Then
    Exit;

  Log(logDebug, 'App', 'IAP error: '+IntToString(ErrorCode));
  Application.Instance.AddValueEvent(eventIAPError, ErrorCode);    
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationIAPCredits(env, obj:Pointer; Credits:Integer); stdcall; export;  [public];
Begin
  If Not Application.Instance.CanReceiveEvents Then
    Exit;

  Log(logDebug, 'App', 'TERRA APP IAP: Credits ' + IntToString(Credits));
  Application.Instance.AddValueEvent(eventIAPCredits, Credits);    
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationListFile(env, obj, str:Pointer; size:Integer); stdcall; export;  [public];
Begin
  If Not Application.Instance.CanReceiveEvents Then
    Exit;

  //Log(logDebug, 'App', 'TERRA APP IAP CONFIRM');
  AndroidApplication(Application.Instance).ListFile(Str, size);
End;


Function Java_com_pascal_terra_TERRALibrary_ApplicationChartboostGetKey(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  Result := StringToJava(Application.Instance.Client.GetChartboostID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationChartboostGetSecret(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  Result := StringToJava(Application.Instance.Client.GetChartboostSecret());
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationTapjoyCredits(env, obj:Pointer; credits:Integer); stdcall; export;  [public];
Begin
  AndroidApplication(Application.Instance).Tapjoy_Update(Credits);
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationTapjoyGetKey(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  Result := StringToJava(Application.Instance.Client.GetTapjoyID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationTapjoyGetSecret(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  Result := StringToJava(Application.Instance.Client.GetTapjoySecret());
End;
        
Function Java_com_pascal_terra_TERRALibrary_ApplicationFlurryGetID(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  Result := StringToJava(Application.Instance.Client.GetFlurryID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationAdMobInterstitialGetID(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  Result := StringToJava(Application.Instance.Client.GetAdMobInterstitialID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationAdBuddizGetID(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  Result := StringToJava(Application.Instance.Client.GetAdBuddizID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationVungleGetID(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  Result := StringToJava(Application.Instance.Client.GetVungleID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationIAPGetID(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  Result := StringToJava(Application.Instance.Client.GetBillingID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationIAPGetServiceID(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  Result := StringToJava(Application.Instance.Client.GetFortumoID());
End;

Function Java_com_pascal_terra_TERRALibrary_ApplicationIAPGetServiceSecret(env, obj:Pointer):JavaString; stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  Result := StringToJava(Application.Instance.Client.GetFortumoSecret());
End;

Procedure Java_com_pascal_terra_TERRALibrary_ApplicationAPIResult(env, obj:Pointer; API,Code:Integer); stdcall; export;  [public];
Begin
  //Log(logDebug, 'App', 'TERRA APP IAP CANCEL');
  If (Application.Instance.Client<>Nil) Then
	Application.Instance.Client.OnAPIResult(API, Code);
End;

function JNI_OnLoad(vm, reserved:pointer):Integer; cdecl; export; {$IFDEF OLD_FPC}[public];{$ENDIF}
begin
  //Log(logDebug, 'App', 'JNI Startup called with VM='+HexStr(Cardinal(VM)));
  result := startupWithVM(vm);
end;

procedure JNI_OnUnload(vm, reserved:pointer); cdecl; export; {$IFDEF OLD_FPC}[public];{$ENDIF}
begin
end;

exports 
		Java_com_pascal_terra_TERRALibrary_ApplicationInit,
        Java_com_pascal_terra_TERRALibrary_ApplicationShutdown,
        Java_com_pascal_terra_TERRALibrary_ApplicationResize,
		Java_com_pascal_terra_TERRALibrary_ApplicationViewport,
        Java_com_pascal_terra_TERRALibrary_ApplicationUpdate,
        Java_com_pascal_terra_TERRALibrary_ApplicationResume,
        Java_com_pascal_terra_TERRALibrary_ApplicationPause,
		Java_com_pascal_terra_TERRALibrary_ApplicationSetState,
		Java_com_pascal_terra_TERRALibrary_ApplicationSetOrientation,
		Java_com_pascal_terra_TERRALibrary_ApplicationGetOrientation,
		Java_com_pascal_terra_TERRALibrary_ApplicationContextLost,
        Java_com_pascal_terra_TERRALibrary_ApplicationKeyPress,
        Java_com_pascal_terra_TERRALibrary_ApplicationKeyDown,
        Java_com_pascal_terra_TERRALibrary_ApplicationKeyUp,
		Java_com_pascal_terra_TERRALibrary_ApplicationOnAccelerometer,
		Java_com_pascal_terra_TERRALibrary_ApplicationOnGyroscope,
		Java_com_pascal_terra_TERRALibrary_ApplicationOnCompass,		
		Java_com_pascal_terra_TERRALibrary_ApplicationTouchMove,
        Java_com_pascal_terra_TERRALibrary_ApplicationTouchBegin,
        Java_com_pascal_terra_TERRALibrary_ApplicationTouchEnd,
		Java_com_pascal_terra_TERRALibrary_ApplicationFlurryGetID,
		Java_com_pascal_terra_TERRALibrary_ApplicationAdMobInterstitialGetID,
        Java_com_pascal_terra_TERRALibrary_ApplicationAdBuddizGetID,
        Java_com_pascal_terra_TERRALibrary_ApplicationVungleGetID,
		Java_com_pascal_terra_TERRALibrary_ApplicationIAPConfirm,
		Java_com_pascal_terra_TERRALibrary_ApplicationIAPError,
		Java_com_pascal_terra_TERRALibrary_ApplicationIAPCredits,
		Java_com_pascal_terra_TERRALibrary_ApplicationIAPGetID,
		Java_com_pascal_terra_TERRALibrary_ApplicationIAPGetServiceID,
		Java_com_pascal_terra_TERRALibrary_ApplicationIAPGetServiceSecret,
		Java_com_pascal_terra_TERRALibrary_ApplicationAPIResult,
		Java_com_pascal_terra_TERRALibrary_ApplicationListFile,
        Java_com_pascal_terra_TERRALibrary_ApplicationChartboostGetKey,
        Java_com_pascal_terra_TERRALibrary_ApplicationChartboostGetSecret,
        Java_com_pascal_terra_TERRALibrary_ApplicationTapjoyGetKey,
        Java_com_pascal_terra_TERRALibrary_ApplicationTapjoyGetSecret,
        Java_com_pascal_terra_TERRALibrary_ApplicationTapjoyCredits,
        
		JNI_OnLoad,
		JNI_OnUnload;
