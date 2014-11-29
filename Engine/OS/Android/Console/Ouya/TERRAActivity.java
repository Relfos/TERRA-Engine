package com.pascal.terra;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.WindowManager;
import android.widget.RelativeLayout;
import android.view.Display;
import android.graphics.Point;
import android.content.Intent;
import android.util.DisplayMetrics;
import android.widget.Toast;
import android.content.Context;
import android.content.IntentFilter;
import android.content.BroadcastReceiver;
import android.util.Base64;
import android.view.KeyEvent;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.X509EncodedKeySpec;
import java.text.ParseException;
import java.util.*;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;

import org.json.JSONException;
import org.json.JSONObject;

import com.terra.minimon3d.R;
import tv.ouya.console.api.*;

public class TERRAActivity extends Activity {

	private int keyGamepadIndex   = 255;
	private int keyGamepadLeft    = keyGamepadIndex + 0;
	private int keyGamepadUp      = keyGamepadIndex + 1;
	private int keyGamepadRight   = keyGamepadIndex + 2;
	private int keyGamepadDown    = keyGamepadIndex + 3;
	private int keyGamepadA       = keyGamepadIndex + 4;
	private int keyGamepadB       = keyGamepadIndex + 5;
	private int keyGamepadC       = keyGamepadIndex + 6;
	private int keyGamepadD       = keyGamepadIndex + 7;
	private int keyGamepadX       = keyGamepadIndex + 8;
	private int keyGamepadY       = keyGamepadIndex + 9;
	private int keyGamepadZ       = keyGamepadIndex + 10;
	private int keyGamepadR       = keyGamepadIndex + 11;
	private int keyGamepadL       = keyGamepadIndex + 12;
	private int keyGamepadMenu    = keyGamepadIndex + 13;
	private int keyGamepadCount   = 32;

    public TERRAView glView;
	public RelativeLayout topView = null;
	
	public String developerEmail = null;
		
	public static TERRAActivity instance = null;
	
	private String purchaseID;
	
	private static Map<String, String> mOutstandingPurchaseRequests = new HashMap<String, String>();	
	private static PublicKey mPublicKey;	

	public static boolean ouyaMenuVisible = false;
	public static boolean hasBilling = false;
	
    @Override 
	protected void onCreate(Bundle icicle) {
		Log.d("App", "OUYA App started...");		
	
        super.onCreate(icicle);
		instance = this;

		TERRALibrary.LoadTERRA();

		RelativeLayout.LayoutParams glParams = new RelativeLayout.LayoutParams(
			RelativeLayout.LayoutParams.FILL_PARENT, 
            RelativeLayout.LayoutParams.FILL_PARENT); 
			
		Log.d("App", "Initializing GLview...");		
		
        glView = new TERRAView(getApplication());		
		glView.setFocusable(true);
		glView.setFocusableInTouchMode(true);
		glView.setKeepScreenOn(true);
		
		topView = new RelativeLayout(this);
		topView.setFocusable(true);
		topView.setFocusableInTouchMode(true);
 
		topView.addView(glView, glParams);               
		this.setContentView(topView);		

		Log.d("App", "Initializng Ouya controller...");		
		OuyaController.init(this);		
	}

	public void initBilling()
	{	
		Log.d("App", "Getting Ouya ID...");		
		String ouyaID = null;
		
		synchronized (TERRAActivity.instance)
		{								
			ouyaID = TERRALibrary.ApplicationIAPGetID();				
		}
		
		if (ouyaID == null)
			return;
			
		OuyaFacade.getInstance().init(this, ouyaID);

		// Read in the key.der file (downloaded from the developer portal)
		byte[] applicationKey;
		try {
			InputStream inputStream = getResources().openRawResource(R.raw.key);
			applicationKey = new byte[inputStream.available()];
			inputStream.read(applicationKey);
			inputStream.close();
        } catch (Exception e) {
            Log.e("TERRAActivity", "Unable to read key.der file", e);
			System.exit(0);
			return;
        }		
			
		Log.d("App", "Creating public key from blob...");		
		// Create a PublicKey object from the key data downloaded from the developer portal.
        try {
            X509EncodedKeySpec keySpec = new X509EncodedKeySpec(applicationKey);
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            mPublicKey = keyFactory.generatePublic(keySpec);
        } catch (Exception e) {
            Log.e("TERRAActivity", "Unable to create encryption key", e);
			return;
        }	

		hasBilling = true;
	}
	
    @Override 
	protected void onPause() 
	{
		synchronized (TERRAActivity.instance)
		{				
			if (TERRAMusicPlayer.instance!=null)
			{
				Log.d("App", "Pausing music ");
				TERRAMusicPlayer.instance.pause();
			}
		
			glView.paused = true;
			super.onPause();
			if (glView!=null && glView.isInitialized())
			{
				Log.d("App", "Begin native OnPause");		
				TERRALibrary.ApplicationPause();
				Log.d("App", "End native OnPause");		
			}

			Log.d("App", "Begin OpenGL OnPause");		
			glView.onPause();
			Log.d("App", "End OpenGL OnPause");		
		}
    }

    @Override 
	protected void onResume() {
        super.onResume();
		ouyaMenuVisible = false;

		synchronized (TERRAActivity.instance)
		{				
			Log.d("App", "Begin OpenGL OnResume");		
			glView.onResume();
			Log.d("App", "End OpenGL OnResume");		
		
			Log.d("App", "Calling TERRA.Resume()");
			TERRALibrary.ApplicationResume(); 
			Log.d("App", "TERRA.Resume() called sucessfuly!");

			if (TERRAMusicPlayer.instance!=null)
			{
				Log.d("App", "Resuming music");
				TERRAMusicPlayer.instance.play();
			}
			
			glView.paused = false;
			Log.d("App", "App resumed sucessfuly!");
		}
    }

	@Override
	protected void onDestroy() {	
		TERRALibrary.ApplicationShutdown();
		Log.d("App", "TERRA.ONDESTROY");

		OuyaFacade.getInstance().shutdown();
		
		super.onDestroy();
	}

    /*@Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) 
	{
        super.onActivityResult(requestCode, resultCode, data);

		switch(requestCode) {
        //case 123:	billingHelper.handleActivityResult(requestCode, resultCode, data);
				//	break;
		default:	TERRAUtils.facebook.authorizeCallback(requestCode, resultCode, data);
		}
    }*/
	 
	public static boolean canPurchase() 
	{
		if (OuyaFacade.getInstance().isRunningOnOUYAHardware())
			return true;
		else
			return false;
	}
	
	/**
     * Display an error to the user. We're using a toast for simplicity.
     */

    private void showError(final String errorMessage) 
	{
        Toast.makeText(this, errorMessage, Toast.LENGTH_LONG).show();
    }
	
	@Override
	public boolean onKeyDown(final int keyCode, KeyEvent event)
	{
		if (glView.paused)
		{
			return super.onKeyDown(keyCode, event);
		}
	
		//Get the player #
		int player = OuyaController.getPlayerNumByDeviceId(event.getDeviceId());       
		boolean handled = false;
		Log.d("App", "OUYA keypressed, code ="+ keyCode+"  	player = "+player);		

		synchronized (TERRAActivity.instance)
		{										
		//Handle the input
		switch(keyCode){
			case OuyaController.BUTTON_O:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadA);
				handled = true;
				break;
				
			case OuyaController.BUTTON_A:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadB);
				handled = true;
				break;
				
			case OuyaController.BUTTON_U:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadX);
				handled = true;
				break;

			case OuyaController.BUTTON_Y:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadY);
				handled = true;
				break;
				
			case OuyaController.BUTTON_MENU:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadMenu);
				handled = true;
				break;
				
			case OuyaController.BUTTON_DPAD_DOWN :
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadDown);
				handled = true;
				break;

			case OuyaController.BUTTON_DPAD_UP:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadUp);
				handled = true;
				break;
			
			case OuyaController.BUTTON_DPAD_LEFT :
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadLeft);
				handled = true;
				break;

			case OuyaController.BUTTON_DPAD_RIGHT :
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadRight);
				handled = true;
				break;
				
			case OuyaController.BUTTON_L1:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadL);
				handled = true;
				break;
				
			case OuyaController.BUTTON_R1:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadR);
				handled = true;
				break;
			}
		}
		return handled || super.onKeyDown(keyCode, event);
	}

	@Override
	public boolean onKeyUp(final int keyCode, KeyEvent event)
	{
		if (glView.paused)
		{
			return super.onKeyUp(keyCode, event);
		}

		//Get the player #
		int player = OuyaController.getPlayerNumByDeviceId(event.getDeviceId());       
		boolean handled = false;

		//Handle the input
		synchronized (TERRAActivity.instance)
		{										
		switch(keyCode){
			case OuyaController.BUTTON_O:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadA);
				handled = true;
				break;
				
			case OuyaController.BUTTON_A:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadB);
				handled = true;
				break;
				
			case OuyaController.BUTTON_U:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadX);
				handled = true;
				break;

			case OuyaController.BUTTON_Y:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadY);
				handled = true;
				break;
				
			case OuyaController.BUTTON_MENU:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadMenu);
				handled = true;
				break;
				
			case OuyaController.BUTTON_DPAD_DOWN :
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadDown);
				handled = true;
				break;

			case OuyaController.BUTTON_DPAD_UP:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadUp);
				handled = true;
				break;
			
			case OuyaController.BUTTON_DPAD_LEFT :
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadLeft);
				handled = true;
				break;

			case OuyaController.BUTTON_DPAD_RIGHT :
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadRight);
				handled = true;
				break;
				
			case OuyaController.BUTTON_L1:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadL);
				handled = true;
				break;
				
			case OuyaController.BUTTON_R1:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadR);
				handled = true;
				break;
			}
		}
		return handled || super.onKeyUp(keyCode, event);
	}
	
	public static void purchase(String sku) 
	{	
		if (!hasBilling)
		{
			instance.glView.purchaseID = sku;
			instance.glView.purchaseCode = 1;
			return;
		}
	
		if (OuyaFacade.getInstance().isRunningOnOUYAHardware())
		{
			instance.purchaseID = sku;

			try {
				SecureRandom sr = SecureRandom.getInstance("SHA1PRNG");

				// This is an ID that allows you to associate a successful purchase with
				// it's original request. The server does nothing with this string except
				// pass it back to you, so it only needs to be unique within this instance
				// of your app to allow you to pair responses with requests.
				String uniqueId = Long.toHexString(sr.nextLong());

				JSONObject purchaseRequest = new JSONObject();
				purchaseRequest.put("uuid", uniqueId);
				purchaseRequest.put("identifier", sku);
				// This value is only needed for testing, not setting it results in a live purchase
				purchaseRequest.put("testing", "true"); 
				String purchaseRequestJson = purchaseRequest.toString();

				byte[] keyBytes = new byte[16];
				sr.nextBytes(keyBytes);
				SecretKey key = new SecretKeySpec(keyBytes, "AES");

				byte[] ivBytes = new byte[16];
				sr.nextBytes(ivBytes);
				IvParameterSpec iv = new IvParameterSpec(ivBytes);

				Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding", "BC");
				cipher.init(Cipher.ENCRYPT_MODE, key, iv);
				byte[] payload = cipher.doFinal(purchaseRequestJson.getBytes("UTF-8"));

				cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding", "BC");
				cipher.init(Cipher.ENCRYPT_MODE, mPublicKey);
				byte[] encryptedKey = cipher.doFinal(keyBytes);

				Purchasable purchasable =
						new Purchasable(
								sku,
								Base64.encodeToString(encryptedKey, Base64.NO_WRAP),
								Base64.encodeToString(ivBytes, Base64.NO_WRAP),
								Base64.encodeToString(payload, Base64.NO_WRAP) );

				synchronized (mOutstandingPurchaseRequests) {
					mOutstandingPurchaseRequests.put(uniqueId, sku);
				}
				OuyaFacade.getInstance().requestPurchase(purchasable, new PurchaseListener(sku));
			} catch (Exception e) {
				Log.e("TERRAActivity", "Unable to request purchase", e);
				return;
			}		
		}
	}

	/**
     * The callback for when the user attempts to purchase something. We're not worried about
     * the user cancelling the purchase so we extend CancelIgnoringOuyaResponseListener, if
     * you want to handle cancelations differently you should extend OuyaResponseListener and
     * implement an onCancel method.
     *
     * @see tv.ouya.console.api.CancelIgnoringOuyaResponseListener
     * @see tv.ouya.console.api.OuyaResponseListener#onCancel()
     */
    private static class PurchaseListener implements OuyaResponseListener<String> 
	{
        /**
         * The ID of the product the user is trying to purchase. This is used in the
         * onFailure method to start a re-purchase if they user wishes to do so.
         */

        private String sku;

        /**
         * Constructor. Store the ID of the product being purchased.
         */

        PurchaseListener(final String productID) {
            sku = productID;
        }

        /**
         * Handle a successful purchase.
         *
         * @param result The response from the server.
         */
        @Override
        public void onSuccess(String result) {            
            String id;
            try {
                    OuyaEncryptionHelper helper = new OuyaEncryptionHelper();

                    JSONObject response = new JSONObject(result);

                    id = helper.decryptPurchaseResponse(response, mPublicKey);
                    String productsku;
                    synchronized (instance.mOutstandingPurchaseRequests) {
                        productsku = instance.mOutstandingPurchaseRequests.remove(id);
                    }

					instance.glView.purchaseID = productsku;
                    if (productsku == null) {
						Log.d("App", "Error purchasing product, it was null... ");
						instance.glView.purchaseID = null;
						instance.glView.purchaseCode = 3;
                        return;
                    }

					Log.d("App", "Purchased: " + instance.glView.purchaseID);
					instance.glView.purchaseCode = 0;
					
                    return;
			} catch (Exception e) {
                    Log.e("Purchase", "Your purchase failed.", e);					
					instance.glView.purchaseID = null;
					instance.glView.purchaseCode = 3;
            }
        }

        @Override
        public void onFailure(int errorCode, String errorMessage, Bundle optionalData) 
		{
			Log.d("App", "Purchase failure: " + errorMessage);
			instance.glView.purchaseID = instance.purchaseID;
			instance.glView.purchaseCode = 3;
        }

        /*
         * Handling the user canceling
         */
        @Override
        public void onCancel() {
			instance.glView.purchaseID = instance.purchaseID;
			instance.glView.purchaseCode = 2;
            //showError("User cancelled purchase");
        }
    }
	
	public void initInterstitials()
	{
		Log.d("TERRA", "Interstials not supported...");		
	}
 
	public void initTapjoy()
	{
		Log.d("TERRA", "Tapjoy not supported...");		
	}
}
