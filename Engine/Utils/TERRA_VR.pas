Unit TERRA_VR;

Interface
Uses TERRA_Application, TERRA_OS, OpenVR_API;

Type
  VRApplication = Class(Application)
    Protected
      _VRHandle:Pointer;

    Public
      Constructor Create();
      Destructor Destroy; Override;

  End;

Implementation
Uses TERRA_Log;


{ VRApplication }

Constructor VRApplication.Create;
Var
  peError:VR_HmdError;
Begin
  _VRHandle := VR_Init(peError);

  If _VRHandle = Nil Then
  Begin
    Log(logError, 'VR', 'VR initialization failed: '+VR_GetStringForHmdError(peError));
  End;
End;

Destructor VRApplication.Destroy;
Begin
  VR_Shutdown();
End;

End.