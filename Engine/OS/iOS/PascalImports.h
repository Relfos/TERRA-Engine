#ifndef PASCALIMPORTS
#define PASCALIMPORTS

#ifdef __cplusplus
extern "C" {
#endif

void ApplicationInit();
void ApplicationUpdate();
void ApplicationShutdown();

bool ApplicationIsHiRes();
    
void ApplicationEnterState(int state);	
void ApplicationBeginTouch(int x, int y);
void ApplicationMoveTouch(int x, int y);
void ApplicationEndTouch(int x, int y);

void ApplicationSetOrientation(int orientation);
void ApplicationSetViewport(int x1, int y1, int x2, int y2);

void ApplicationOnAccelerometer(float x, float y, float z);
void ApplicationOnGyroscope(float x, float y, float z);
void ApplicationOnCompass(float heading, float pitch, float roll);

void ApplicationOnKeyDown(int key);
void ApplicationOnKeyUp(int key);

void ApplicationOnFacebookPost();
void ApplicationOnFacebookError();

void ApplicationSendInput(char s);

void ApplicationOnCamera(int width, int height, char* buffer);

void ApplicationSetOrigin(int x, int y);
void ApplicationResize(int x, int y);

void ApplicationSetScale(float scale);	

void ApplicationPostedToFacebook();
    
void ApplicationMemoryWarning();

void IAP_Callback_Canceled(char *ID);
void IAP_Callback_Purchase(char *ID);
    
void ApplicationSetScreenRegion(int width, int height);
void ApplicationSetResourcesPath(char *path); 
void ApplicationTempPath(char *path); 
void ApplicationDocumentPath(char *path); 

void ApplicationSetCountry(char *country); 
void ApplicationSetLanguage(char *lang); 
	
char* ApplicationGetAppID();
char* ApplicationGetFlurryID();
char* ApplicationGetAdMobID();
char* ApplicationGetFacebookID();
char* ApplicationGetTestFlightID();
	
#ifdef __cplusplus
}
#endif

#endif