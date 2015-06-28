#import <StoreKit/StoreKit.h>
#import "TERRA_InAppPurchaseManager.h"
#import "PascalImports.h"

@implementation InAppPurchaseManager

//@synthesize proUpgradeProduct;
//@synthesize productsRequest;



// call this method once on startup
- (void)loadStore
{
    NSLog(@"Loading store...");

	IAPmanager = self;
	
    // restarts any purchases if they were interrupted last time the app was open
    [[SKPaymentQueue defaultQueue] addTransactionObserver:self];
}

- (void)requestProductData:(char *)s
{
    NSString *productID = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
    NSSet *productIdentifiers = [NSSet setWithObject:productID ];
    productsRequest = [[SKProductsRequest alloc] initWithProductIdentifiers:productIdentifiers];
    productsRequest.delegate = self;
    [productsRequest start];
    
    // we will release the request object in the delegate callback
} 


- (void)productsRequest:(SKProductsRequest *)request didReceiveResponse:(SKProductsResponse *)response
{
    NSArray *products = response.products;
    product = [products count] == 1 ? [products firstObject] : nil;
    if (product)
    {
        NSLog(@"Product title: %@" , product.localizedTitle);
        NSLog(@"Product description: %@" , product.localizedDescription);
        NSLog(@"Product price: %@" , product.price);
        NSLog(@"Product id: %@" , product.productIdentifier);
		
		NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
		[numberFormatter setFormatterBehavior:NSNumberFormatterBehavior10_4];
		[numberFormatter setNumberStyle:NSNumberFormatterCurrencyStyle];
		[numberFormatter setLocale:product.priceLocale];
        
	/*	 NSString* price = [numberFormatter stringFromNumber:product.price];
        
        char *pid = [product.productIdentifier cStringUsingEncoding:NSUTF8StringEncoding];
        char *desc = [product.localizedDescription cStringUsingEncoding:NSUTF8StringEncoding];
        char *title = [product.localizedTitle cStringUsingEncoding:NSUTF8StringEncoding];
        char *pprice = [price cStringUsingEncoding:NSUTF8StringEncoding];

        IAP_Callback_Info(pid, title, desc, pprice);
        */
    }
    
    for (NSString *invalidProductId in response.invalidProductIdentifiers)
    {
        NSLog(@"Invalid product id: %@" , invalidProductId);
    }
    
    // finally release the reqest we alloc/init’ed in requestProUpgradeProductData
    //[productsRequest release];
}


//
// call this before making a purchase
//
- (BOOL)canMakePurchases
{
    return [SKPaymentQueue canMakePayments];
}

//
// kick off the upgrade transaction
//
char *productStr = NULL;

-(void)purchaseProduct:(char *)s
{
    NSLog(@"Purchasing product...%s",s);
    
    NSString *productID = [NSString stringWithCString:s encoding:NSASCIIStringEncoding];
	const char *ptr = [productID cStringUsingEncoding:NSUTF8StringEncoding];
    productStr = strdup(ptr);
    
    SKPayment *payment = [SKPayment paymentWithProductIdentifier:productID];
    [[SKPaymentQueue defaultQueue] addPayment:payment];
}

//
// enable pro features
//
- (void)provideContent:(NSString *)productId
{
    NSLog(@"Purchased product...%@",productId);
	const char *ptr = [productId cStringUsingEncoding:NSUTF8StringEncoding];
    IAP_Callback_Purchase(ptr);
}

//
// removes the transaction from the queue and posts a notification with the transaction result
//
- (void)finishTransaction:(SKPaymentTransaction *)transaction wasSuccessful:(BOOL)wasSuccessful
{
    // remove the transaction from the payment queue.
    [[SKPaymentQueue defaultQueue] finishTransaction:transaction];
    
    NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:transaction, @"transaction" , nil];
    if (wasSuccessful)
    {
        // send out a notification that we’ve finished the transaction
        [[NSNotificationCenter defaultCenter] postNotificationName:kInAppPurchaseManagerTransactionSucceededNotification object:self userInfo:userInfo];
    }
    else
    {
        // send out a notification for the failed transaction
        [[NSNotificationCenter defaultCenter] postNotificationName:kInAppPurchaseManagerTransactionFailedNotification object:self userInfo:userInfo];
        IAP_Callback_Canceled(productStr);
    }
}

//
// called when the transaction was successful
//
- (void)completeTransaction:(SKPaymentTransaction *)transaction
{
    [self provideContent:transaction.payment.productIdentifier];
    [self finishTransaction:transaction wasSuccessful:YES];
}

//
// called when a transaction has been restored and and successfully completed
//
- (void)restoreTransaction:(SKPaymentTransaction *)transaction
{
    [self provideContent:transaction.originalTransaction.payment.productIdentifier];
    [self finishTransaction:transaction wasSuccessful:YES];
}

//
// called when a transaction has failed
//
- (void)failedTransaction:(SKPaymentTransaction *)transaction
{
    if (transaction.error.code != SKErrorPaymentCancelled)
    {
        // error!
        [self finishTransaction:transaction wasSuccessful:NO];
    }
    else
    {
        // this is fine, the user just cancelled, so don’t notify
        [[SKPaymentQueue defaultQueue] finishTransaction:transaction];
    }
}


//
// called when the transaction status is updated
//
- (void)paymentQueue:(SKPaymentQueue *)queue updatedTransactions:(NSArray *)transactions
{
    NSLog(@"Processing transaction...");
    for (SKPaymentTransaction *transaction in transactions)
    {
        switch (transaction.transactionState)
        {
            case SKPaymentTransactionStatePurchased:
                NSLog(@"Transaction purchased...");
                [self completeTransaction:transaction];
                break;
            case SKPaymentTransactionStateFailed:
                NSLog(@"Transaction failed...");
                [self failedTransaction:transaction];
                break;
            case SKPaymentTransactionStateRestored:
                NSLog(@"Transaction restored...");
                [self restoreTransaction:transaction];
                break;
            default:
                break;
        }
    }
}

@end
