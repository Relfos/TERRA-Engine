Unit TERRA_IAP;

{$I terra.inc}
Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_OS, TERRA_Application
  {$IFDEF ANDROID},TERRA_JAVA{$ENDIF};

{$IFDEF WINDOWS}{$UNDEF ANDROID}{$ENDIF}

Const
  IAP_Success = 0;
  IAP_PurchaseBlocked = 1;
  IAP_PurchaseCanceled = 2;
  IAP_ConnectionError = 3;
  IAP_DeviceNotSupported = 4;
  IAP_InvalidKey        = 5;
  IAP_PurchaseFailed = 6;

Type
  PIAPCatalogEntry = ^IAPCatalogEntry;
  IAPCatalogEntry = Record
    ID:TERRAString;
    Title:TERRAString;
    Description:TERRAString;
    Price:TERRAString;
  End;

  IAPCatalog = Class(TERRAObject)
    Protected
      _CatalogList:Array Of IAPCatalogEntry;
      _CatalogCount:Integer;

      Procedure AddInfo(ID, Title, Description, Price:TERRAString);

    Public
      Constructor Create;
      Procedure Release; Override;

      Class Function Instance:IAPCatalog;

      //Procedure AddCatalogEntry(ID, Title, Description,Price:TERRAString);
      Function GetInfo(ID:TERRAString):PIAPCatalogEntry;
      Procedure Purchase(ID:TERRAString; UserData:Pointer);
      Procedure PurchaseCredits(UserData:Pointer);
  End;

Procedure IAP_Callback_Canceled(ID:PAnsiChar); cdecl; export;
Procedure IAP_Callback_Purchase(ID:PAnsiChar); cdecl; export;
//Procedure IAP_Callback_Info(ID, Title, Description, Price:PTERRAChar); cdecl; export;

Implementation
Uses TERRA_Log{$IFDEF STEAM},TERRA_Steam{$ENDIF};

Var
  _IAPCatalog_Instance:IAPCatalog = Nil;

Procedure IAP_Callback_Canceled(ID:PAnsiChar); cdecl; export;
Begin
   Log(logDebug, 'IAP', 'Cancelled: '+ID);
   Application.Instance.OnIAP_Error(IAP_PurchaseCanceled);
End;

Procedure IAP_Callback_Purchase(ID:PAnsiChar); cdecl; export;
Begin
  Log(logDebug, 'IAP', 'Purchased: '+ID);

  Application.Instance.OnIAP_Purchase(ID);
End;

{Procedure IAP_Callback_Info(ID, Title, Description, Price:PAnsiChar); cdecl; export;
Var
  S2,S3,S4:TERRAString;
Begin
Exit;
  S2 := utf8_to_ucs2(Title);
  S3 := utf8_to_ucs2(Description);
  S4 := utf8_to_ucs2(Price);

  IAPCatalog.Instance.AddInfo(ID, S2, S3, S4);
  If Assigned(Application.Instance.Client) Then
    Application.Instance.Client.OnIAP_Info(ID, Title, Description, Price);
End;}


{ IAPCatalog }
Constructor IAPCatalog.Create;
Begin

End;

Procedure IAPCatalog.Release;
Begin
  _IAPCatalog_Instance := Nil;
End;

Class Function IAPCatalog.Instance: IAPCatalog;
Begin
  If Not Assigned(_IAPCatalog_Instance) Then
    _IAPCatalog_Instance := IAPCatalog.Create;

  Result := _IAPCatalog_Instance;
End;

Procedure IAPCatalog.PurchaseCredits(UserData:Pointer);
{$IFDEF ANDROID}
Var
  Utils:JavaClass;
  Frame:JavaFrame;
{$ENDIF}
Begin
{$IFDEF ANDROID}                                    
  Java_Begin(Frame);
  Utils := JavaClass.Create(ActivityClassPath, Frame);
  Log(logDebug, 'App', 'Purchasing credits');
  Utils.CallStaticVoidMethod(Frame, 'purchaseCredits', Nil);
  ReleaseObject(Utils);
  Java_End(Frame);
  Exit;
{$ELSE}
  Log(logWarning, 'IAP', 'Purchasing credits not supported in this platform!');
{$ENDIF}

  IAP_Callback_Canceled('credits');
End;

Procedure IAPCatalog.Purchase(ID:TERRAString; UserData:Pointer);
{$IFDEF ANDROID}
Var
  Utils:JavaClass;
  Params:JavaArguments;
  Frame:JavaFrame;
{$ENDIF}
Begin
{$IFDEF ANDROID}
  {$IFDEF OUYA}
  ReplaceText('.', '_', ID);
  {$ENDIF}

  Log(logWarning, 'IAP', 'Trying purchase...');

  Java_Begin(Frame);
  Utils := JavaClass.Create(ActivityClassPath, Frame);

  If (Utils.CallStaticBoolMethod(Frame, 'canPurchase', Nil)) Then
  Begin
    Log(logDebug, 'App', 'Purchasing '+ID);
    Params := JavaArguments.Create(Frame);
    Params.AddString(ID);
    Utils.CallStaticVoidMethod(Frame, 'purchase', Params);
    ReleaseObject(Params);
  End Else
  Begin
    Log(logWarning, 'IAP', 'Purchases are disabled!');
    IAP_Callback_Canceled(PAnsiChar(ID));
  End;

  ReleaseObject(Utils);
  Java_End(Frame);
  Exit;
{$ENDIF}

{$IFDEF IPHONE}
  If (IAP_CanPurchase()) Then
    IAP_Purchase(PAnsiChar(ID))
  Else
    IAP_Callback_Canceled(PAnsiChar(ID));

  Exit;
{$ENDIF}

{$IFDEF STEAM}
  Log(logWarning, 'IAP', 'Purchases not allowed in Steam!');
  IAP_Callback_Canceled(PAnsiChar(ID));
  //Application.Instance.Client.OnIAP_External(ID, UserData);
  Exit;
{$ENDIF}

  If Application.Instance.DebuggerPresent Then
    IAP_Callback_Purchase(PAnsiChar(ID))
  Else
    IAP_Callback_Canceled(PAnsiChar(ID));
End;

(*Procedure IAPCatalog.AddCatalogEntry(ID, Title, Description,Price:TERRAString);
Var
  I:Integer;
Begin
  Self.AddInfo(ID, Title, Description, Price);
{$IFDEF IPHONE}
  IAP_RequestProduct(PAnsiChar(ID));
{$ENDIF}

{$IFNDEF MOBILE}
  IAP_Callback_Info(PAnsiChar(ID), PAnsiChar(Title), PAnsiChar(Description), PAnsiChar(Price));
{$ENDIF}
End;*)

Procedure IAPCatalog.AddInfo(ID, Title, Description, Price:TERRAString);
Var
  I, N:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_CatalogCount) Do
  If (_CatalogList[I].ID = ID) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
  Begin
    N := _CatalogCount;
    Inc(_CatalogCount);
    SetLength(_CatalogList, _CatalogCount);
    _CatalogList[Pred(_CatalogCount)].ID := ID;
  End;

  _CatalogList[Pred(_CatalogCount)].Title := Title;
  _CatalogList[Pred(_CatalogCount)].Description := Description;
  _CatalogList[Pred(_CatalogCount)].Price := Price;
End;

Function IAPCatalog.GetInfo(ID:TERRAString): PIAPCatalogEntry;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_CatalogCount) Do
  If (_CatalogList[I].ID = ID) Then
  Begin
    Result := @(_CatalogList[I]);
    Exit;
  End;

  Result := Nil;
End;

Initialization
  Log(logDebug, 'IAP', 'IAP Module started!');
Finalization
  ReleaseObject(_IAPCatalog_Instance);
End.
