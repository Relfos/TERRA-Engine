package purchases;

/**
 * With this PurchaseInfo a developer is able verify
 * a purchase from the google play store on his own
 * server. An example implementation of how to verify
 * a purchase you can find <a href="https://github.com/mgoldsborough/google-play-in-app-billing-verification/blob/master/library/GooglePlay/InAppBilling/GooglePlayResponseValidator.php#L64">here</a>
 *
 */
public class PurchaseInfo {

	public final String responseData;

	public final String signature;

	PurchaseInfo(String responseData, String signature) {
		this.responseData = responseData;
		this.signature = signature;
	}
}
