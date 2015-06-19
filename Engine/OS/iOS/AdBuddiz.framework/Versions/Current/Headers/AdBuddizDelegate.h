//
//  AdBuddizDelegate.h
//  Copyright (c) 2014 Purple Brain. All rights reserved.
//

typedef enum AdBuddizErrors {
    UNSUPPORTED_IOS_SDK = 0,
	CONFIG_NOT_READY = 1,
	CONFIG_EXPIRED = 2,
	MISSING_PUBLISHER_KEY = 3,
	INVALID_PUBLISHER_KEY = 4,
	PLACEMENT_BLOCKED = 5,
	NO_NETWORK_AVAILABLE = 6,
	NO_MORE_AVAILABLE_CACHED_ADS = 7,
	NO_MORE_AVAILABLE_ADS = 8,
	SHOW_AD_CALLS_TOO_CLOSE = 9,
	UNKNOWN_EXCEPTION_RAISED = 999
} AdBuddizError;

@protocol AdBuddizDelegate <NSObject>

@optional

/*! @brief Called when an Ad has been downloaded and is ready to show. */
- (void)didCacheAd;

/*! @brief Called when an Ad has been displayed to user. */
- (void)didShowAd;

/*! 
 @brief Called when [AdBuddiz showAd] was called and SDK wasn't able to show an ad.
 @code
 - (void)didFailToShowAd:(AdBuddizError) error
 {
    NSLog(@"AdBuddizDelegate: didFailToShowAd : %i - %@", error, [AdBuddiz nameForError:error]);
 }
 @endcode
 @param AdBuddizError code explaining why
 */
- (void)didFailToShowAd:(AdBuddizError) error;

/*! @brief Called when a user clicked on an ad. */
- (void)didClick;

/*! @brief Called when a ad was hidden (user clicked on X or on the ad). */
- (void)didHideAd;

@end
