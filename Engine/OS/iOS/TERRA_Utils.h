#import <Foundation/Foundation.h>
#import <AVFoundation/AVFoundation.h>
#import <UIKit/UIKit.h>

#define DOCUMENTS_FOLDER()      [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0]

#define SETTINGS_FILE()         [DOCUMENTS_FOLDER() stringByAppendingString:@"/settings.plist"]
#define SAVES_DIRECTORY()       [DOCUMENTS_FOLDER() stringByAppendingString:@"/Saves/"]

#define IS_IPAD()               (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad)

void showAlert(char *message);
BOOL rotationManager (UIInterfaceOrientation interfaceOrientation);
BOOL isiPad();
void vibrate();
void getModelType(char * name);
void iPhoneLog(char *s);

void setAccelFreq(float freq);
void initAppViews();
void enableAVCapture();

void openAppStore(char *appid);

bool isSocialFrameworkAvailable();

AVAudioPlayer *audioOpen(char *name);
void audioPlay(AVAudioPlayer *audioPlayer);
void audioStop(AVAudioPlayer *audioPlayer);
void audioSetVolume(AVAudioPlayer *audioPlayer, float value);
void audioClose(AVAudioPlayer *audioPlayer);

char* resolveHost(char *host);

void IAP_RequestProduct(char *s);
bool IAP_CanPurchase();
void IAP_Purchase(char *s);

void changeLeaderboard(char *board);
void showLeaderboard();
void submitScore(int score);

void setPrefString(char* key, char* data);
void getPrefString(char* key, char* dest);

void startAVCapture();
void stopAVCapture();

void AnalyticsLog(char *s);
void AnalyticsLogWithParams(char *s, char *s2);

void showAds();

void initFramebuffers(bool canScale,  int msaaSamples);


@interface NSString (extra)

-(NSString *) MD5hash;

@end

