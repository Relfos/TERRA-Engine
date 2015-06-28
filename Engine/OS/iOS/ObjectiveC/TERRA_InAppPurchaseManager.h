// InAppPurchaseManager.h

#import <StoreKit/StoreKit.h>

// add a couple notifications sent out when the transaction completes
#define kInAppPurchaseManagerTransactionFailedNotification @"kInAppPurchaseManagerTransactionFailedNotification"
#define kInAppPurchaseManagerTransactionSucceededNotification @"kInAppPurchaseManagerTransactionSucceededNotification"

void IAP_RequestProduct(char *s);
void IAP_Purchase(char *s);

@interface InAppPurchaseManager : NSObject <SKProductsRequestDelegate, SKPaymentTransactionObserver>
{
    SKProduct *product;
    SKProductsRequest *productsRequest;
}

// public methods
- (void)loadStore;
- (BOOL)canMakePurchases;
- (void)purchaseProduct:(char *)s;
- (void)requestProductData:(char *)s;

@end 