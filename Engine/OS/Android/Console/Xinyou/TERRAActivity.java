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
import android.view.MotionEvent;

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

import com.vxinyou.box.tools.BoxContorl;
import com.vxinyou.box.tools.BoxPlayCommandInterface;

import com.terra.minimon3d.R;

public class TERRAActivity extends Activity implements BoxPlayCommandInterface {

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

	public static final int XINYOU_KEY_HOME = 9000;               //HOME¼ü
	public static final int XINYOU_KEY_LEFT_LEVER_UP = 9001;      //×óÒ¡¸ËµÄÏòÉÏ¼ü
	public static final int XINYOU_KEY_LEFT_LEVER_DOWN = 9002;    //×óÒ¡¸ËµÄÏòÏÂ¼ü
	public static final int XINYOU_KEY_LEFT_LEVER_LEFT = 9003;    //×óÒ¡¸ËµÄÏò×ó¼ü
	public static final int XINYOU_KEY_LEFT_LEVER_RIGHT = 9004;   //×óÒ¡¸ËµÄÏòÓÒ¼ü
	public static final int XINYOU_KEY_LEFT_LEVER_CENTRE = 9005;  //×óÒ¡¸ËµÄÖÐÐÄ¼ü
	public static final int XINYOU_KEY_UP = 9006;                 //·½ÏòÅÌµÄÏòÉÏ¼ü
	public static final int XINYOU_KEY_DOWN = 9007;               //·½ÏòÅÌµÄÏòÏÂ¼ü
	public static final int XINYOU_KEY_LEFT = 9008;               //·½ÏòÅÌµÄÏò×ó¼ü
	public static final int XINYOU_KEY_RIGHT = 9009;              //·½ÏòÅÌµÄÏòÓÒ¼ü
	public static final int XINYOU_KEY_A = 9010;                  //ÊÖ±úµÄA¼ü
	public static final int XINYOU_KEY_B = 9011;                  //ÊÖ±úµÄB¼ü
	public static final int XINYOU_KEY_X = 9012;                  //ÊÖ±úµÄX¼ü
	public static final int XINYOU_KEY_Y = 9013;                  //ÊÖ±úµÄY¼ü
	public static final int XINYOU_KEY_L1 = 9014;                 //ÊÖ±úL1¼ü£¬¶ÔÓ¦È·ÈÏ¼ü
	public static final int XINYOU_KEY_L2 = 9015;                 //ÊÖ±úL2¼ü£¬¶ÔÓ¦ÉÏ·­Ò³
	public static final int XINYOU_KEY_R1 = 9016;                 //ÊÖ±úR1¼ü£¬¶ÔÓ¦È¡Ïû¼ü
	public static final int XINYOU_KEY_R2 = 9017;                 //ÊÖ±úR2¼ü£¬¶ÔÓ¦ÏÂ·­Ò³
	public static final int XINYOU_KEY_RIGHT_LEVER_UP = 9018;     //ÓÒÒ¡¸ËÏòÉÏ¼ü
	public static final int XINYOU_KEY_RIGHT_LEVER_DOWN = 9019;   //ÓÒÒ¡¸ËÏòÏÂ¼ü
	public static final int XINYOU_KEY_RIGHT_LEVER_LEFT = 9020;   //ÓÒÒ¡¸ËÏò×ó¼ü
	public static final int XINYOU_KEY_RIGHT_LEVER_RIGHT = 9021;  //ÓÒÒ¡¸ËÏòÓÒ¼ü
	public static final int XINYOU_KEY_RIGHT_LEVER_CENTRE = 9022; //ÓÒÒ¡¸ËÖÐÐÄ¼ü
	public static final int XINYOU_KEY_SELECT = 9023;             //Ñ¡Ôñ¼ü
	public static final int XINYOU_KEY_START = 9024;              //¿ªÊ¼ÔÝÍ
	
    public TERRAView glView;
	public RelativeLayout topView = null;
		
	public static TERRAActivity instance = null;
	
	private BoxContorl mBoxContorl = null;
	
    @Override 
	protected void onCreate(Bundle icicle) {
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
		
		// init controller
		mBoxContorl = BoxContorl.getInstance(this);
	}

	public void initBilling()
	{
		Log.d("App", "Billing not implemented!");		
	}
	
    @Override 
	protected void onPause() 
	{
		synchronized (TERRAActivity.instance)
		{				
			glView.paused = true;
			super.onPause();
			if (glView!=null && glView.isInitialized())
			{
				
                synchronized (TERRAActivity.instance)                
                {
                    Log.d("App", "Begin native OnPause");		
                    TERRALibrary.ApplicationPause();
                    Log.d("App", "End native OnPause");		
                }
				
			}

			Log.d("App", "Begin OpenGL OnPause");		
			glView.onPause();
			Log.d("App", "End OpenGL OnPause");		
		}
    }

    @Override 
	protected void onResume() {
        super.onResume();
		
		synchronized (TERRAActivity.instance)
		{				
			Log.d("App", "Begin OpenGL OnResume");		
			glView.onResume();
			Log.d("App", "End OpenGL OnResume");		
		
            synchronized (TERRAActivity.instance)                
            {
                Log.d("App", "Calling TERRA.Resume()");
                TERRALibrary.ApplicationResume(); 
                Log.d("App", "TERRA.Resume() called sucessfuly!");
            }
            
			glView.paused = false;
			Log.d("App", "App resumed sucessfuly!");
		}
    }

	@Override
	protected void onDestroy() {	
    
        synchronized (TERRAActivity.instance)                
        {    
            TERRALibrary.ApplicationShutdown();
            Log.d("App", "TERRA.ONDESTROY");
        }

		super.onDestroy();
	}

	/*
    @Override
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
		return false;
	}
		
	@Override
	public boolean dispatchKeyEvent(KeyEvent event) 
	{
		if (mBoxContorl.dispatchKeyEvent(this, this, event)) 
		{
			return true;
		}
		return super.dispatchKeyEvent(event);
	}
		
	@Override
	public boolean onKeyDown(final int keyCode, KeyEvent event)
	{
		return super.onKeyDown(keyCode, event);
	}

	@Override
	public boolean onKeyUp(final int keyCode, KeyEvent event)
	{		
		return super.onKeyUp(keyCode, event);
	}
	
	@Override
	public boolean PlayKeyDown(int player, int keyCode, KeyEvent event) 
	{
		Log.d("App", "PlayKeyDown play=" + player + " keyCode=" + keyCode);
	
        synchronized (TERRAActivity.instance)                
        {
    
		//Handle the input
		switch(keyCode){
			case XINYOU_KEY_A:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadA);
				break;
				
			case XINYOU_KEY_B:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadB);
				break;
				
			case XINYOU_KEY_X:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadX);
				break;

			case XINYOU_KEY_Y:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadY);
				break;
				
			case XINYOU_KEY_HOME:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadMenu);
				break;
				
			case XINYOU_KEY_DOWN :
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadDown);
				break;

			case XINYOU_KEY_UP:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadUp);
				break;
			
			case XINYOU_KEY_LEFT :
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadLeft);
				break;

			case XINYOU_KEY_RIGHT :
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadRight);
				break;
				
			case XINYOU_KEY_L1:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadL);
				break;
				
			case XINYOU_KEY_R1:
				TERRALibrary.ApplicationKeyDown(keyGamepadCount * player + keyGamepadR);
				break;
            }
        }
	
		return true;
	}

	@Override
	public boolean PlayKeyUP(int player, int keyCode, KeyEvent event) 
	{
		Log.d("App", "PlayKeyUP play=" + player + " keyCode=" + keyCode);
		
        synchronized (TERRAActivity.instance)                
        {        
		//Handle the input
		switch(keyCode){
			case XINYOU_KEY_A:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadA);
				break;
				
			case XINYOU_KEY_B:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadB);
				break;
				
			case XINYOU_KEY_X:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadX);
				break;

			case XINYOU_KEY_Y:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadY);
				break;
				
			case XINYOU_KEY_HOME:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadMenu);
				break;
				
			case XINYOU_KEY_DOWN :
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadDown);
				break;

			case XINYOU_KEY_UP:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadUp);
				break;
			
			case XINYOU_KEY_LEFT :
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadLeft);
				break;

			case XINYOU_KEY_RIGHT :
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadRight);
				break;
				
			case XINYOU_KEY_L1:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadL);
				break;
				
			case XINYOU_KEY_R1:
				TERRALibrary.ApplicationKeyUp(keyGamepadCount * player + keyGamepadR);
				break;
            }
		}
        
		return false;
	}

	@Override
	public boolean PlayLongPressKeyDown(int play, int keyCode, int duration, KeyEvent event) 
	{
		Log.d("App", "PlayLongKeyDown play=" + play + " keyCode=" + keyCode);
		return true;
	}

	@Override
	public boolean playJoystickGenericMotionEvent(int play, MotionEvent event) 
	{
		Log.d("App", "playJoystickGenericMotionEvent play=" + play);
		return false;
	}

	@Override
	public boolean playJoystickGenericMotionEvent(int play, int Joystickid, int x, int y, int oldx, int oldy) 
	{
		Log.d("App", "playJoystickGenericMotionEvent play=" + play + " Joystickid=" + Joystickid + " x=" + x + " y=" + y + " oldx=" + oldx + " oldy=" + oldy);
		return false;
	}

	@Override
	public boolean playJoystickGenericMotionEventKey(int playIdx, int syskeyidx, int axisIdx, int newdata, int olddata) 
	{
		return false;
	}

	
	public static void purchase(String sku) 
	{	
		instance.glView.purchaseID = sku;
		instance.glView.purchaseCode = 2;
	}

	public void initInterstitials()
	{
		Log.d("TERRA", "Interstials not supported...");		
	}
 
	public void initTapjoy()
	{
		Log.d("TERRA", "Tapjoy not supported...");		
	}

   	public void initAnalytics()
	{
		Log.d("TERRA", "Analytics not supported...");		
	}

}
