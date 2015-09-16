//
//  AdBuddiz.h
//  Copyright (c) 2014 Purple Brain. All rights reserved.
//

#import <AdBuddiz/AdBuddizDelegate.h>

typedef enum {
    ABLogLevelInfo = 1,
    ABLogLevelError = 2,
    ABLogLevelSilent = 0
} ABLogLevel;

@interface AdBuddiz : NSObject

/*!
 @brief Set AdBuddiz SDK log level.
 @code
 - (void)applicationDidBecomeActive:(UIApplication *)application {
    [AdBuddiz setLogLevel:ABLogLevelInfo];            // log level
    [AdBuddiz setPublisherKey:@"TEST_PUBLISHER_KEY"]; // replace with your app publisher key
    [AdBuddiz cacheAds];                              // start caching ads
 }
 @endcode
 @param level, log level
 @see http://publishers.adbuddiz.com/portal/support
 */
+ (void)setLogLevel:(ABLogLevel)level;

/*!
 @brief Configure the SDK for your app.
 @code
 - (void)applicationDidBecomeActive:(UIApplication *)application 
 {
    [AdBuddiz setPublisherKey:@"TEST_PUBLISHER_KEY"]; // replace with your app publisher key
    [AdBuddiz cacheAds];                              // start caching ads
 }
 @endcode
 @param publisherKey, your app publisher key
 @see http://publishers.adbuddiz.com/portal/support
 */
+ (void)setPublisherKey:(NSString *)publisherKey;

/*!
 @brief Activate test mode. Delete or comment it before submitting to store.
 @code
 - (void)applicationDidBecomeActive:(UIApplication *)application 
 {
    [AdBuddiz setPublisherKey:@"TEST_PUBLISHER_KEY"]; // replace with your app publisher key
    [AdBuddiz setTestModeActive];                     // to delete before submitting to store
    [AdBuddiz cacheAds];                              // start caching ads
 }
 @endcode
 @see http://publishers.adbuddiz.com/portal/support
 */
+ (void)setTestModeActive;

/*!
@brief Initializes AdBuddiz SDK. Call it in your app delegate in <i>application: applicationDidBecomeActive:</i>.
@code
 - (void)applicationDidBecomeActive:(UIApplication *)application 
 {
    [AdBuddiz setPublisherKey:@"TEST_PUBLISHER_KEY"]; // replace with your app publisher key
    [AdBuddiz cacheAds];                              // start caching ads
 }
@endcode
@see http://publishers.adbuddiz.com/portal/support
 */
+ (void)cacheAds;

/*!
 @brief Returns YES if the SDK is ready to show an ad.
 @code
 if ([AdBuddiz isReadyToShowAd]) {
    [AdBuddiz showAd];
 }
 @endcode
 @note if you get YES, you're 100% sure that a showAd call will display an ad.
 @see http://publishers.adbuddiz.com/portal/support
*/
+ (BOOL)isReadyToShowAd;

/*!
 @brief Returns YES if the SDK is ready to show an ad for the placement.
 @code
 if ([AdBuddiz isReadyToShowAd:@"placement_1"]) {
    [AdBuddiz showAd:@"placement_1"];
 }
 @endcode
 @param placement, name of the placement
 @note if you get YES, you're 100% sure that a showAd call will display an ad.
 @see http://publishers.adbuddiz.com/portal/support
 */
+ (BOOL)isReadyToShowAd:(NSString *)placement;

/*!
 @brief Shows an AdBuddiz ad.
 @code
    [AdBuddiz showAd];
 @endcode
 @see http://publishers.adbuddiz.com/portal/support
 */
+ (void)showAd;

/*!
 @brief Shows an AdBuddiz ad for specific placement.
 @code
 [AdBuddiz showAd:@"placement_1"];
 @endcode
 @param placement, name of the placement
 @see http://publishers.adbuddiz.com/portal/support
 */+ (void)showAd:(NSString*)placement;

/*!
 @brief Sets AdBuddizDelegate that will receive events callbacks.
 @code
 [AdBuddiz setDelegate:self];
 @endcode
 @param delegate, must implement <AdBuddizDelegate> protocol
 @see http://publishers.adbuddiz.com/portal/support
 */
+ (void)setDelegate:(id<AdBuddizDelegate>)delegate;

/*!
 @brief Convert AdBuddizError code to understandable NSString *.
 @code
  - (void)didFailToShowAd:(AdBuddizError) error
  {
    NSLog(@"AdBuddizDelegate: didFailToShowAd : %i - %@", error, [AdBuddiz nameForError:error]);
  }
 @endcode
 @param error, the AdBuddizError code
 @see http://publishers.adbuddiz.com/portal/support
 */
+ (NSString *)nameForError:(AdBuddizError)error;

@end
