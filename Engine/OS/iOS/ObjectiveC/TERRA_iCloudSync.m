#import "TERRA_iCloudSync.h"

@implementation iCloudSync

+(void) updateToiCloud:(NSNotification*) notificationObject {
    
    NSDictionary *dict = [[NSUserDefaults standardUserDefaults] dictionaryRepresentation];
    
    [dict enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        
        [[NSUbiquitousKeyValueStore defaultStore] setObject:obj forKey:key];
    }];
    
    [[NSUbiquitousKeyValueStore defaultStore] synchronize];
}

+(void) updateFromiCloud:(NSNotification*) notificationObject {
    
    NSUbiquitousKeyValueStore *iCloudStore = [NSUbiquitousKeyValueStore defaultStore];
    NSDictionary *dict = [iCloudStore dictionaryRepresentation];
    
    // prevent NSUserDefaultsDidChangeNotification from being posted while we update from iCloud
    
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                                    name:NSUserDefaultsDidChangeNotification
                                                  object:nil];

    [dict enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        
        [[NSUserDefaults standardUserDefaults] setObject:obj forKey:key];
    }];

    [[NSUserDefaults standardUserDefaults] synchronize];

    // enable NSUserDefaultsDidChangeNotification notifications again

    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(updateToiCloud:)
                                                 name:NSUserDefaultsDidChangeNotification
object:nil];
    
    [[NSNotificationCenter defaultCenter] postNotificationName:kiCloudSyncNotification object:nil];
}

+(void) start {
    
    if(NSClassFromString(@"NSUbiquitousKeyValueStore")) { // is iOS 5?
        
        if([NSUbiquitousKeyValueStore defaultStore]) { // is iCloud enabled
            
            [[NSNotificationCenter defaultCenter] addObserver:self
                                                     selector:@selector(updateFromiCloud:)
                                                         name:NSUbiquitousKeyValueStoreDidChangeExternallyNotification
                                                       object:nil];
            
            [[NSNotificationCenter defaultCenter] addObserver:self
                                                     selector:@selector(updateToiCloud:)
                                                         name:NSUserDefaultsDidChangeNotification object:nil];
        } else {
            NSLog(@"iCloud not enabled");
        }
    }
    else {
        NSLog(@"Not an iOS 5 device");
    }
}

+ (void) dealloc {
    
    [[NSNotificationCenter defaultCenter] removeObserver:self
                                                    name:NSUbiquitousKeyValueStoreDidChangeExternallyNotification
                                                  object:nil];

    [[NSNotificationCenter defaultCenter] removeObserver:self
                                                    name:NSUserDefaultsDidChangeNotification
                                                  object:nil];
}
@end