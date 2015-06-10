package purchases;

import com.pascal.terra.TERRALibrary;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Base64;
import android.text.TextUtils;
import android.util.Log;

import com.android.vending.billing.IInAppBillingService;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

//import java.net.URLConnection;

import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.X509EncodedKeySpec;

public class PurchaseProcessor extends PurchaseBase {

    public static final int GOOGLE_API_VERSION = 3;

    public static final String PRODUCT_TYPE_MANAGED = "inapp";
    public static final String PRODUCT_TYPE_SUBSCRIPTION = "subs";

	public static final int BILLING_RESPONSE_RESULT_OK = 0; 				//Success
	public static final int BILLING_RESPONSE_RESULT_USER_CANCELED =	1; 		//User pressed back or canceled a dialog
	public static final int BILLING_RESPONSE_RESULT_BILLING_UNAVAILABLE = 3;//Billing API version is not supported for the type requested
	public static final int BILLING_RESPONSE_RESULT_ITEM_UNAVAILABLE = 4; 	//Requested product is not available for purchase
	public static final int BILLING_RESPONSE_RESULT_DEVELOPER_ERROR = 5; 	//Invalid arguments provided to the API. This error can also indicate that the application was not correctly signed or properly set up for In-app Billing in Google Play, or does not have the necessary permissions in its manifest
	public static final int BILLING_RESPONSE_RESULT_ERROR =	6; 				//Fatal error during the API action
	public static final int BILLING_RESPONSE_RESULT_ITEM_ALREADY_OWNED = 7; //Failure to purchase since item is already owned
	public static final int BILLING_RESPONSE_RESULT_ITEM_NOT_OWNED = 8; 	//Failure to consume since item is not owned

	public static final String RESPONSE_CODE = "RESPONSE_CODE";
	public static final String DETAILS_LIST = "DETAILS_LIST";
    public static final String PRODUCTS_LIST = "ITEM_ID_LIST";
	public static final String BUY_INTENT = "BUY_INTENT";
	public static final String INAPP_PURCHASE_DATA_LIST = "INAPP_PURCHASE_DATA_LIST";
	public static final String INAPP_PURCHASE_DATA = "INAPP_PURCHASE_DATA";
    public static final String RESPONSE_INAPP_SIGNATURE = "INAPP_DATA_SIGNATURE";
	public static final String INAPP_DATA_SIGNATURE_LIST = "INAPP_DATA_SIGNATURE_LIST";
    public static final String RESPONSE_ORDER_ID = "orderId";
    public static final String RESPONSE_PRODUCT_ID = "productId";
    public static final String RESPONSE_TYPE = "type";
    public static final String RESPONSE_TITLE = "title";
    public static final String RESPONSE_DESCRIPTION = "description";
    public static final String RESPONSE_PRICE = "price";
    public static final String RESPONSE_PRICE_CURRENCY = "price_currency_code";
    public static final String RESPONSE_PRICE_MICROS = "price_amount_micros";
    public static final String RESPONSE_PURCHASE_TOKEN = "purchaseToken";
    public static final String RESPONSE_PURCHASE_TIME = "purchaseTime";
    public static final String RESPONSE_PAYLOAD = "developerPayload";

	public static final int BILLING_ERROR_FAILED_LOAD_PURCHASES = 100;
	public static final int BILLING_ERROR_FAILED_TO_INITIALIZE_PURCHASE = 101;
	public static final int BILLING_ERROR_INVALID_SIGNATURE = 102;
	public static final int BILLING_ERROR_LOST_CONTEXT = 103;
	public static final int BILLING_ERROR_OTHER_ERROR = 110;

	/**
	 * Callback methods where billing events are reported.
	 * Apps must implement one of these to construct a PurchaseProcessor.
	 */
	public static interface IBillingHandler {
		void onProductPurchased(String productId, TransactionDetails details);

		void onPurchaseHistoryRestored();

		void onBillingError(int errorCode, Throwable error);

        void onBillingDisabled();
        
		void onBillingInitialized();
	}

	private static final int PURCHASE_FLOW_REQUEST_CODE = 11784;
	private static final String LOG_TAG = "purchases";
	private static final String SETTINGS_VERSION = ".v6";
	private static final String RESTORE_KEY = ".bills.restored" + SETTINGS_VERSION;
	private static final String MANAGED_PRODUCTS_CACHE_KEY = ".products.cache" + SETTINGS_VERSION;
	private static final String SUBSCRIPTIONS_CACHE_KEY = ".subscriptions.cache" + SETTINGS_VERSION;
	private static final String PURCHASE_PAYLOAD_CACHE_KEY = ".purchase.last" + SETTINGS_VERSION;

	private IInAppBillingService billingService;
	private String contextPackageName;
	private PurchaseCache cachedProducts;
	private PurchaseCache cachedSubscriptions;
	private IBillingHandler eventHandler;

	private ServiceConnection serviceConnection = new ServiceConnection() {
		@Override
		public void onServiceDisconnected(ComponentName name) {
			billingService = null;
			if (eventHandler != null)
				eventHandler.onBillingDisabled();
		}

		@Override
		public void onServiceConnected(ComponentName name, IBinder service) {
			billingService = IInAppBillingService.Stub.asInterface(service);
			if (!isPurchaseHistoryRestored() && loadOwnedPurchasesFromGoogle()) {
				setPurchaseHistoryRestored();
				if (eventHandler != null)
					eventHandler.onPurchaseHistoryRestored();
			}
			if (eventHandler != null)
				eventHandler.onBillingInitialized();
		}
	};
    
    private static String verifyServiceKey() {
		Log.d("BILLING", "Getting Billing key...");		
        String localKey = TERRALibrary.ApplicationIAPGetID();
        return localKey;
    }
    

	public PurchaseProcessor(Activity context, IBillingHandler handler) {
		super(context);		
		eventHandler = handler;
		contextPackageName = context.getApplicationContext().getPackageName();
		cachedProducts = new PurchaseCache(context, MANAGED_PRODUCTS_CACHE_KEY);
		cachedSubscriptions = new PurchaseCache(context, SUBSCRIPTIONS_CACHE_KEY);
		bindPlayServices();
	}

	private void bindPlayServices() {
		try {
			Intent iapIntent = new Intent("com.android.vending.billing.InAppBillingService.BIND");
			iapIntent.setPackage("com.android.vending");
			getContext().bindService(iapIntent, serviceConnection, Context.BIND_AUTO_CREATE);
		} catch (Exception e) {
			Log.e(LOG_TAG, e.toString());
		}
	}

	@Override
	public void release() {
		if (serviceConnection != null && getContext() != null) {
			try {
				getContext().unbindService(serviceConnection);
			} catch (Exception e) {
				Log.e(LOG_TAG, e.toString());
			}
			billingService = null;
		}
		cachedProducts.release();
		super.release();
	}

	public boolean isInitialized() {
		return billingService != null;
	}

	public boolean isPurchased(String productId) {
		return cachedProducts.includesProduct(productId);
	}

	public boolean isSubscribed(String productId) {
		return cachedSubscriptions.includesProduct(productId);
	}

	public List<String> listOwnedProducts() {
		return cachedProducts.getContents();
	}

	public List<String> listOwnedSubscriptions() {
		return cachedSubscriptions.getContents();
	}

	private boolean loadPurchasesByType(String type, PurchaseCache cacheStorage) {
		if (!isInitialized())
			return false;
		try {
			Bundle bundle = billingService.getPurchases(PurchaseProcessor.GOOGLE_API_VERSION, contextPackageName, type, null);
			if (bundle.getInt(PurchaseProcessor.RESPONSE_CODE) == PurchaseProcessor.BILLING_RESPONSE_RESULT_OK) {
				cacheStorage.clear();
				ArrayList<String> purchaseList = bundle.getStringArrayList(PurchaseProcessor.INAPP_PURCHASE_DATA_LIST);
				ArrayList<String> signatureList = bundle.getStringArrayList(PurchaseProcessor.INAPP_DATA_SIGNATURE_LIST);
				for (int i = 0; i < purchaseList.size(); i++) {
					String jsonData = purchaseList.get(i);
					JSONObject purchase = new JSONObject(jsonData);
					String signature = signatureList != null && signatureList.size() > i ? signatureList.get(i) : null;
					cacheStorage.put(purchase.getString(PurchaseProcessor.RESPONSE_PRODUCT_ID), jsonData, signature);
				}
			}
			return true;
		} catch (Exception e) {
			if (eventHandler != null)
				eventHandler.onBillingError(PurchaseProcessor.BILLING_ERROR_FAILED_LOAD_PURCHASES, e);
			Log.e(LOG_TAG, e.toString());
		}
		return false;
	}

	public boolean loadOwnedPurchasesFromGoogle() {
		return isInitialized() &&
				loadPurchasesByType(PurchaseProcessor.PRODUCT_TYPE_MANAGED, cachedProducts) &&
				loadPurchasesByType(PurchaseProcessor.PRODUCT_TYPE_SUBSCRIPTION, cachedSubscriptions);
	}

	public boolean purchase(String productId) {
		return purchase(productId, PurchaseProcessor.PRODUCT_TYPE_MANAGED);
	}

	public boolean subscribe(String productId) {
		return purchase(productId, PurchaseProcessor.PRODUCT_TYPE_SUBSCRIPTION);
	}

	private boolean purchase(String productId, String purchaseType) {
		if (!isInitialized() || TextUtils.isEmpty(productId) || TextUtils.isEmpty(purchaseType))
			return false;
		try {
			String purchasePayload = purchaseType + ":" + UUID.randomUUID().toString();
			savePurchasePayload(purchasePayload);
			Bundle bundle = billingService.getBuyIntent(PurchaseProcessor.GOOGLE_API_VERSION, contextPackageName, productId, purchaseType, purchasePayload);
			if (bundle != null) {
				int response = bundle.getInt(PurchaseProcessor.RESPONSE_CODE);
				if (response == PurchaseProcessor.BILLING_RESPONSE_RESULT_OK) {
					PendingIntent pendingIntent = bundle.getParcelable(PurchaseProcessor.BUY_INTENT);
					if (getContext() != null)
						getContext().startIntentSenderForResult(pendingIntent.getIntentSender(), PURCHASE_FLOW_REQUEST_CODE, new Intent(), 0, 0, 0);
					else if (eventHandler != null)
						eventHandler.onBillingError(PurchaseProcessor.BILLING_ERROR_LOST_CONTEXT, null);
				} else if (response == PurchaseProcessor.BILLING_RESPONSE_RESULT_ITEM_ALREADY_OWNED) {
					if (!isPurchased(productId) && !isSubscribed(productId)) {
						loadOwnedPurchasesFromGoogle();
                    }
                    
					if (eventHandler != null) {
						TransactionDetails details = getPurchaseTransactionDetails(productId);
						if (details == null)
							details = getSubscriptionTransactionDetails(productId);
                        
						eventHandler.onProductPurchased(productId, details);
					}
                    
				} else if (eventHandler != null)
					eventHandler.onBillingError(PurchaseProcessor.BILLING_ERROR_FAILED_TO_INITIALIZE_PURCHASE, null);
			}
			return true;
		} catch (Exception e) {
			Log.e(LOG_TAG, e.toString());
		}
		return false;
	}

	private TransactionDetails getPurchaseTransactionDetails(String productId, PurchaseCache cache) {
		PurchaseInfo details = cache.getDetails(productId);
		if (details != null && !TextUtils.isEmpty(details.responseData)) {
			try {
				return new TransactionDetails(details);
			} catch (JSONException e) {
				Log.e(LOG_TAG, "Failed to load saved purchase details for " + productId);
			}
		}
		return null;
	}

	public boolean consumePurchase(String productId) {
		if (!isInitialized())
			return false;
		try {
			TransactionDetails transactionDetails = getPurchaseTransactionDetails(productId, cachedProducts);
			if (transactionDetails != null && !TextUtils.isEmpty(transactionDetails.purchaseToken)) {
				int response = billingService.consumePurchase(PurchaseProcessor.GOOGLE_API_VERSION, contextPackageName, transactionDetails.purchaseToken);
				if (response == PurchaseProcessor.BILLING_RESPONSE_RESULT_OK) {
					cachedProducts.remove(productId);
					Log.d(LOG_TAG, "Successfully consumed " + productId + " purchase.");
					return true;
				} else {
					if (eventHandler != null)
						eventHandler.onBillingError(response, null);
					Log.e(LOG_TAG, String.format("Failed to consume %s: error %d", productId, response));
				}
			}
		} catch (Exception e) {
			Log.e(LOG_TAG, e.toString());
		}
		return false;
	}

	private SkuDetails getSkuDetails(String productId, String purchaseType) {
		if (billingService != null) {
			try {
				ArrayList<String> skuList = new ArrayList<String>();
				skuList.add(productId);
				Bundle products = new Bundle();
				products.putStringArrayList(PurchaseProcessor.PRODUCTS_LIST, skuList);
				Bundle skuDetails = billingService.getSkuDetails(PurchaseProcessor.GOOGLE_API_VERSION, contextPackageName, purchaseType, products);
				int response = skuDetails.getInt(PurchaseProcessor.RESPONSE_CODE);
				if (response == PurchaseProcessor.BILLING_RESPONSE_RESULT_OK) {
					for (String responseLine : skuDetails.getStringArrayList(PurchaseProcessor.DETAILS_LIST)) {
						JSONObject object = new JSONObject(responseLine);
						String responseProductId = object.getString(PurchaseProcessor.RESPONSE_PRODUCT_ID);
						if (productId.equals(responseProductId))
							return new SkuDetails(object);
					}
				} else {
					if (eventHandler != null)
						eventHandler.onBillingError(response, null);
					Log.e(LOG_TAG, String.format("Failed to retrieve info for %s: error %d", productId, response));
				}
			} catch (Exception e) {
				Log.e(LOG_TAG, String.format("Failed to call getSkuDetails %s", e.toString()));
			}
		}
		return null;
	}

	public SkuDetails getPurchaseListingDetails(String productId) {
		return getSkuDetails(productId, PurchaseProcessor.PRODUCT_TYPE_MANAGED);
	}

	public SkuDetails getSubscriptionListingDetails(String productId) {
		return getSkuDetails(productId, PurchaseProcessor.PRODUCT_TYPE_SUBSCRIPTION);
	}

	public TransactionDetails getPurchaseTransactionDetails(String productId) {
		return getPurchaseTransactionDetails(productId, cachedProducts);
	}

	public TransactionDetails getSubscriptionTransactionDetails(String productId) {
		return getPurchaseTransactionDetails(productId, cachedSubscriptions);

    }
    
    public void processActivity(int resultCode, Intent data)
    {
		int responseCode = data.getIntExtra(PurchaseProcessor.RESPONSE_CODE, PurchaseProcessor.BILLING_RESPONSE_RESULT_OK);
        Log.d(LOG_TAG, String.format("resultCode = %d, responseCode = %d", resultCode, responseCode));
        
        String purchasePayload = getPurchasePayload();
        if (resultCode == Activity.RESULT_OK && responseCode == PurchaseProcessor.BILLING_RESPONSE_RESULT_OK && !TextUtils.isEmpty(purchasePayload)) {
            String purchaseData = data.getStringExtra(PurchaseProcessor.INAPP_PURCHASE_DATA);
            String dataSignature = data.getStringExtra(PurchaseProcessor.RESPONSE_INAPP_SIGNATURE);
            try {
                JSONObject purchase = new JSONObject(purchaseData);
                String productId = purchase.getString(PurchaseProcessor.RESPONSE_PRODUCT_ID);
                String developerPayload = purchase.getString(PurchaseProcessor.RESPONSE_PAYLOAD);
                if (developerPayload == null)
                    developerPayload = "";
                boolean purchasedSubscription = purchasePayload.startsWith(PurchaseProcessor.PRODUCT_TYPE_SUBSCRIPTION);
                if (purchasePayload.equals(developerPayload)) {
                    if (verifyPurchaseSignature(purchaseData, dataSignature)) {
                        PurchaseCache cache = purchasedSubscription ? cachedSubscriptions : cachedProducts;
                        cache.put(productId, purchaseData, dataSignature);
                        
                        if (eventHandler != null) {
                            eventHandler.onProductPurchased(productId, new TransactionDetails(new PurchaseInfo(purchaseData, dataSignature)));
                        }
                        
                        Log.e(LOG_TAG, "Purchase done");
                    } else {
                        Log.e(LOG_TAG, "Public key signature doesn't match!");
                        if (eventHandler != null)
                            eventHandler.onBillingError(PurchaseProcessor.BILLING_ERROR_INVALID_SIGNATURE, null);
                    }
                } else {
                    Log.e(LOG_TAG, String.format("Payload mismatch: %s != %s", purchasePayload, developerPayload));
                    if (eventHandler != null)
                        eventHandler.onBillingError(PurchaseProcessor.BILLING_ERROR_INVALID_SIGNATURE, null);
                }
            } catch (Exception e) {
                Log.e(LOG_TAG, e.toString());
                if (eventHandler != null)
                    eventHandler.onBillingError(PurchaseProcessor.BILLING_ERROR_OTHER_ERROR, null);
            }
        } else {
            if (eventHandler != null)
                eventHandler.onBillingError(PurchaseProcessor.BILLING_ERROR_OTHER_ERROR, null);
        }
    }

	public boolean handleActivityResult(int requestCode, int resultCode, Intent data) {
		if (requestCode != PURCHASE_FLOW_REQUEST_CODE) {
			return false;            
            }

          processActivity(resultCode, data);
/*        new AsyncTask<Void, Void, Void>() {

            @Override
            protected void onPreExecute()
            {
                // here is for code you do before the network call. you 
                // leave it empty
            }

            @Override
            protected Void doInBackground(Void... params)
            {
                Log.d(LOG_TAG, "Starting async receipt");
            }
            
            @Override
            protected void onPostExecute(Void res)
            {
                // here goes your UI code. i.e if you want to hide a button
            }
        }.execute();        */
        
		return true;
	}

    

    private static final String KEY_FACTORY_ALGORITHM = "RSA";
    private static final String SIGNATURE_ALGORITHM = "SHA1withRSA";

    /**
     * Generates a PublicKey instance from a string containing the
     * Base64-encoded public key.
     *
     * @param encodedPublicKey Base64-encoded public key
     * @throws IllegalArgumentException if encodedPublicKey is invalid
     */
    public static PublicKey getPublicKey(String encodedPublicKey) {
        try {
            byte[] decodedKey = Base64.decode(encodedPublicKey, Base64.DEFAULT);
            KeyFactory keyFactory = KeyFactory.getInstance(KEY_FACTORY_ALGORITHM);
            return keyFactory.generatePublic(new X509EncodedKeySpec(decodedKey));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        } catch (InvalidKeySpecException e) {
            Log.e(LOG_TAG, "Invalid key specification.");
            throw new IllegalArgumentException(e);
        } catch (IllegalArgumentException e) {
            Log.e(LOG_TAG, "Base64 decoding failed.");
            throw e;
        }
    }
   
    public static final String TERRA_BILLING_KEY = "ACGs1a7KNIlawqtS0rI0Zp16q+7mfNV+wlwou3QAyRAfQhP8aYoeJKACAOm6W8oZBLLTrwGyfl8U26bbUrma8xJ/fKKcO2+ZfepQDXYjEv/3ZnMpt5UKrdiLst4g6QpwZBEIyFrxfZ7saaWl1YLXJx6eVs3SdtV5K/clQxxp8Lj5";
   
    private static boolean confirmPurchaseSignature(String signedData, String signature) {
            PublicKey key = getPublicKey(TERRA_BILLING_KEY);
            Signature sig;
            try {
                sig = Signature.getInstance(SIGNATURE_ALGORITHM);
                sig.initVerify(key);
                sig.update(signedData.getBytes());
                if (!sig.verify(Base64.decode(signature, Base64.DEFAULT))) { 
                    Log.e(LOG_TAG, "TERRA Signature verification failed.");
                    return true;
                }
                return false;
            } catch (NoSuchAlgorithmException e) {
                Log.e(LOG_TAG, "NoSuchAlgorithmException.");
            } catch (InvalidKeyException e) {
                Log.e(LOG_TAG, "Invalid key specification.");
            } catch (SignatureException e) {
                Log.e(LOG_TAG, "Signature exception.");
            } catch (IllegalArgumentException e) {
                Log.e(LOG_TAG, "Base64 decoding failed.");
            }
            return true;
        }
       
    
     /**
     * Verifies that the data was signed with the given signature, and returns
     * the verified purchase. The data is in JSON format and signed
     * with a private key. The data also contains the {@link PurchaseState}
     * and product ID of the purchase.
     * @param base64PublicKey the base64-encoded public key to use for verifying.
     * @param signedData the signed JSON string (signed, not encrypted)
     * @param signature the signature for the data, signed with the private key
     */
	private static boolean verifyPurchaseSignature(String signedData, String signature) {
        String localKey = verifyServiceKey();
		if (!TextUtils.isEmpty(localKey)) {
			try {
				
                //return Security.verifyPurchase(localKey, purchaseData, dataSignature);

                if (TextUtils.isEmpty(signedData) || TextUtils.isEmpty(localKey) || TextUtils.isEmpty(signature)) {
                    Log.e(LOG_TAG, "Purchase verification failed: missing data.");
                    return false;
                }

                PublicKey key = getPublicKey(localKey);
                Signature sig;
                try {
                    sig = Signature.getInstance(SIGNATURE_ALGORITHM);
                    sig.initVerify(key);
                    sig.update(signedData.getBytes());
                    if (!sig.verify(Base64.decode(signature, Base64.DEFAULT))) {
                        Log.e(LOG_TAG, "Signature verification failed.");
                        return false;
                    }
                    
                    return true;
                    //return confirmPurchaseSignature(signedData, signature);
                } catch (NoSuchAlgorithmException e) {
                    Log.e(LOG_TAG, "NoSuchAlgorithmException.");
                } catch (InvalidKeyException e) {
                    Log.e(LOG_TAG, "Invalid key specification.");
                } catch (SignatureException e) {
                    Log.e(LOG_TAG, "Signature exception.");
                } catch (IllegalArgumentException e) {
                    Log.e(LOG_TAG, "Base64 decoding failed.");
                }
                return false;
                
                /*
                 
                HttpClient httpClient = new DefaultHttpClient();
                HttpContext localContext = new BasicHttpContext();
                
                HttpGet httpGet = new HttpGet("http://minimonworld.com/API/purchase.php?receipt="+purchaseData+"&signature="+dataSignature);
                String text = null;

                HttpResponse response = httpClient.execute(httpGet, localContext);
                HttpEntity entity = response.getEntity();
                text = getASCIIContentFromEntity(entity);
                
                return (text!=null && text.indexOf("success")>=0);*/
                
            } catch (Exception e) {
                    return false;
			}
		}
		return false;
	}

	private boolean isPurchaseHistoryRestored() {
		return loadBoolean(getPreferencesBaseKey() + RESTORE_KEY, false);
	}

	public void setPurchaseHistoryRestored() {
		saveBoolean(getPreferencesBaseKey() + RESTORE_KEY, true);
	}

	private void savePurchasePayload(String value) {
		saveString(getPreferencesBaseKey() + PURCHASE_PAYLOAD_CACHE_KEY, value);
	}

	private String getPurchasePayload() {
		return loadString(getPreferencesBaseKey() + PURCHASE_PAYLOAD_CACHE_KEY, null);
	}
}
