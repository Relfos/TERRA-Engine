package purchases;

import org.json.JSONException;
import org.json.JSONObject;

public class SkuDetails {

	public final String productId;

	public final String title;

	public final String description;

	public final boolean isSubscription;

	public final String currency;

	public final Double priceValue;

	public final String priceText;

	public SkuDetails(JSONObject source) throws JSONException {
		String responseType = source.getString(PurchaseProcessor.RESPONSE_TYPE);
		if (responseType == null)
			responseType = PurchaseProcessor.PRODUCT_TYPE_MANAGED;
		productId = source.getString(PurchaseProcessor.RESPONSE_PRODUCT_ID);
		title = source.getString(PurchaseProcessor.RESPONSE_TITLE);
		description = source.getString(PurchaseProcessor.RESPONSE_DESCRIPTION);
		isSubscription = responseType.equalsIgnoreCase(PurchaseProcessor.PRODUCT_TYPE_SUBSCRIPTION);
		currency = source.getString(PurchaseProcessor.RESPONSE_PRICE_CURRENCY);
		priceValue = source.getDouble(PurchaseProcessor.RESPONSE_PRICE_MICROS) / 1000000;
		priceText = source.getString(PurchaseProcessor.RESPONSE_PRICE);
	}

	@Override
	public String toString() {
		return String.format("%s: %s(%s) %f in %s (%s)", productId, title, description, priceValue, currency, priceText);
	}
}
