package com.pascal.terra;

import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.app.AlertDialog.Builder;

import android.content.DialogInterface;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageInfo;
import android.content.res.AssetManager;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.pm.ResolveInfo;
import android.preference.PreferenceManager;

import android.os.Bundle;
import android.os.AsyncTask;

import android.view.WindowManager;
import android.view.Display;
import android.view.KeyEvent;
import android.view.View;
import android.view.OrientationEventListener;

import android.util.Log;
import android.util.DisplayMetrics;

import android.widget.RelativeLayout;
import android.graphics.Point;
import android.content.Intent;
import android.Manifest.permission;
import android.telephony.TelephonyManager;

import android.content.Context;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;

import purchases.*;

//import android.app.backup.BackupAgentHelper;
//import android.app.backup.FileBackupHelper;

import com.tapjoy.TapjoyConnect;
import com.tapjoy.TJError;
import com.tapjoy.TJEvent;
import com.tapjoy.TJEventCallback;
import com.tapjoy.TJEventPreloadStatus;
import com.tapjoy.TJEventRequest;
import com.tapjoy.TapjoyAwardPointsNotifier;
import com.tapjoy.TapjoyConnect;
import com.tapjoy.TapjoyConnectFlag;
import com.tapjoy.TapjoyConnectNotifier;
import com.tapjoy.TapjoyConstants;
import com.tapjoy.TapjoyDisplayAdNotifier;
import com.tapjoy.TapjoyEarnedPointsNotifier;
import com.tapjoy.TapjoyNotifier;
import com.tapjoy.TapjoyOffersNotifier;
import com.tapjoy.TapjoySpendPointsNotifier;
import com.tapjoy.TapjoyVideoNotifier;
import com.tapjoy.TapjoyViewNotifier;
import com.tapjoy.TapjoyViewType;

import mp.MpUtils;
import mp.PaymentActivity;
import mp.PaymentRequest;
import mp.PaymentResponse;

import java.io.File;
import java.util.Hashtable;

import com.chartboost.sdk.*;
import com.chartboost.sdk.Model.CBError.CBImpressionError;


import com.google.android.gms.ads.*;
/*import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.games.Games;
import com.google.android.gms.plus.Plus;
import com.google.example.games.basegameutils.BaseGameUtils;*/

import com.purplebrain.adbuddiz.sdk.AdBuddiz;
import com.purplebrain.adbuddiz.sdk.AdBuddizDelegate;
import com.purplebrain.adbuddiz.sdk.AdBuddizError;

/*
import com.vungle.publisher.VunglePub;
import com.vungle.publisher.EventListener;
import com.vungle.publisher.AdConfig;*/

import com.terra.minimon3d.R;

public class TERRAActivity extends PaymentActivity 
implements /*AdListener,*/ PurchaseProcessor.IBillingHandler, SensorEventListener, 
TapjoyNotifier, TJEventCallback,
AdBuddizDelegate,
android.content.DialogInterface.OnClickListener
//, GoogleApiClient.ConnectionCallbacks, GoogleApiClient.OnConnectionFailedListener
{
	private static final int DIALOG_REPORT_FORCE_CLOSE = 3535788;
	private static final int DIALOG_UNSUPPORTED_DEVICE = 3535742;
	
    public static TERRAView glView;
	public RelativeLayout topView = null;
	public static boolean hasBilling = false;
		
	public static TERRAActivity instance = null;
	
    public static AssetManager assetManager;
    
	public static String developerEmail = null;
	public static String interstitialID = null;	
    
    public static boolean adbuddizEnabled = false;
    private boolean showedAdBuddiz = false;

    private boolean usingChartboost = false;    
    private boolean showedCharboost = false;
    
	public InterstitialAd interstitial = null;
    private boolean showedAdMob = false;

    /*public static boolean vungleEnabled = false;
    private boolean showedVungle = false;
    private VunglePub vunglePub = null;*/

	public PurchaseProcessor billmaster;
	public String purchaseID;
 	
 
	
	private int currentDialog = 0;
	
	private boolean hasAccel = false;
	private boolean hasGyro = false;
	private boolean hasCompass = false;
	
	private boolean initialized = false;
	
	private boolean usingAccelerometer = false;
	private boolean usingGyroscope = false;
	private boolean usingCompass = false;   
    

	private SensorManager mSensorManager; 
	private Sensor mAccelerometer; 
	private Sensor mGyroscope; 
	private Sensor mCompass; 
	private float[] _accel = new float[3];
	private float[] gravity = new float[3];
	/*private float R[] = new float[9];
	private float I[] = new float[9];	*/
    
    //private GoogleApiClient googleApiClient;
	
	private TERRACrashReporter collector = null;
	
	private OrientationEventListener orientationListener = null;
	
	//private TERRACloudBackup backup = null;
	
    private String EULA_PREFIX = "eula_";
        
	class MainView extends RelativeLayout
	{
	
		public MainView(Activity activity) 
		{
			super(activity);
		}
	
		@Override
		public boolean onKeyDown(int keyCode, KeyEvent event) 
		{
			Log.w("App", "keydown: "+keyCode);
			if(event.getAction() == KeyEvent.ACTION_DOWN && !glView.terminated && keyCode!=KeyEvent.KEYCODE_VOLUME_UP  && keyCode!=KeyEvent.KEYCODE_VOLUME_DOWN) 
			{
				synchronized (TERRAActivity.instance)
				{							
					TERRALibrary.ApplicationKeyDown(keyCode);
				}
				return true;		
			}	 
			return super.onKeyDown(keyCode, event);
		}
		
		@Override
		public boolean onKeyUp(int keyCode, KeyEvent event) 
		{
			Log.w("App", "keyup: "+keyCode);
			if (event.getAction() == KeyEvent.ACTION_UP && !glView.terminated  && keyCode!=KeyEvent.KEYCODE_VOLUME_UP  && keyCode!=KeyEvent.KEYCODE_VOLUME_DOWN) 
			{
				synchronized (TERRAActivity.instance)
				{				
					TERRALibrary.ApplicationKeyUp(keyCode);
				}
				return true;		
			}	 
			return super.onKeyDown(keyCode, event);
		}
	}

	@Override
	public void onAccuracyChanged(Sensor sensor, int accuracy) 
	{
		// do nothing
	}
	
	@Override
	public void onSensorChanged(SensorEvent event) 
	{
		if (glView.terminated)
			return;
		
		if (event.sensor.getType() == Sensor.TYPE_ACCELEROMETER)
		{
			final float alpha = 0.8f;

			gravity[0] = alpha * gravity[0] + (1 - alpha) * event.values[0];
			gravity[1] = alpha * gravity[1] + (1 - alpha) * event.values[1];
			gravity[2] = alpha * gravity[2] + (1 - alpha) * event.values[2];

			TERRAView.accelX = event.values[0] - gravity[0];
			TERRAView.accelY = event.values[1] - gravity[1];
			TERRAView.accelZ = event.values[2] - gravity[2]; 
			TERRAView.hasAccelEvent = true;
			
			_accel[0] = event.values[0];
			_accel[1] = event.values[1];
			_accel[2] = event.values[2]; 
		
			Log.d("App", "Accel X:"+TERRAView.accelX+" Y:"+TERRAView.accelY+" Z:"+TERRAView.accelZ);	
		}
		else
		if (event.sensor.getType() == Sensor.TYPE_GYROSCOPE)
		{
			TERRAView.gyroX = event.values[0];
			TERRAView.gyroY = event.values[1];
			TERRAView.gyroZ = event.values[2]; 
			TERRAView.hasGyroEvent = true;
		
			Log.d("App", "Gyro X:"+TERRAView.gyroX+" Y:"+TERRAView.gyroY+" Z:"+TERRAView.gyroZ);	
		}
		else
		if (event.sensor.getType() == Sensor.TYPE_ORIENTATION)
		{
			TERRAView.compassHeading = event.values[0];
			TERRAView.compassPitch = event.values[1];
			TERRAView.compassRoll = event.values[2];
			TERRAView.hasCompassEvent = true;
			Log.d("App", "Compass X:"+event.values[0]+" Y:"+event.values[1]+" Z:"+event.values[2]);	
		}
		
	}

	public void onClick(DialogInterface dialog, int which) {
	
		switch (currentDialog)
		{
			case DIALOG_UNSUPPORTED_DEVICE:
				finish();
				break;
		
			case DIALOG_REPORT_FORCE_CLOSE:					
				switch (which) 	{
					case DialogInterface.BUTTON_POSITIVE:
					{
						sendLogs();
						break;
					}
					
					case DialogInterface.BUTTON_NEGATIVE:
					{
						showEULA();
						break;
					}				
				}
				break;
		}
		dialog.dismiss();
	}
	
	@Override
	protected Dialog onCreateDialog(int id) {
		Dialog dialog = null;
		currentDialog = id;
		switch (id) {
		case DIALOG_REPORT_FORCE_CLOSE:
			Builder builder = new AlertDialog.Builder(this);
			String message = "It appears that this app crashed last time you ran it, do you want to report it to the developers?";
			builder.setTitle("Warning")
			.setIcon(android.R.drawable.ic_dialog_alert)
			.setMessage(message)
			.setPositiveButton("Yes", this)
			.setNegativeButton("No", this);
			dialog = builder.create();
			break;
			
		
		case DIALOG_UNSUPPORTED_DEVICE:
			builder = new AlertDialog.Builder(this);
			builder.setTitle("Error")
			.setMessage(TERRALibrary.errorMessage)
			.setNegativeButton("OK", null);
			dialog = builder.create();
			break;

			}
		return dialog;
	}
	
	protected void sendLogs()
	{
		dismissDialog(DIALOG_REPORT_FORCE_CLOSE);
		collector.sendLog(developerEmail, "Error Log", "Log info");		
	}	
	

	protected void initApp()
	{
        assetManager = this.getResources().getAssets();
		
		//backup = new TERRACloudBackup();
		
		
		RelativeLayout.LayoutParams glParams = new RelativeLayout.LayoutParams(
			RelativeLayout.LayoutParams.FILL_PARENT, 
            RelativeLayout.LayoutParams.FILL_PARENT); 
			
        glView = new TERRAView(getApplication());		
		glView.setFocusable(true);
		glView.setFocusableInTouchMode(true);
		glView.setKeepScreenOn(true);
		
		topView = new MainView(this);
		topView.setFocusable(true);
		topView.setFocusableInTouchMode(true);
 
		topView.addView(glView, glParams);               
		this.setContentView(topView);		
		
		mSensorManager = (SensorManager) getSystemService(Context.SENSOR_SERVICE);		
		hasAccel = (mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER) != null);
		hasGyro = (mSensorManager.getDefaultSensor(Sensor.TYPE_GYROSCOPE) != null);
		hasCompass = (mSensorManager.getDefaultSensor(Sensor.TYPE_ORIENTATION) != null);
				
		String permission = "com.terra.minimon3d.PAYMENT_BROADCAST_PERMISSION";
		mp.MpUtils.enablePaymentBroadcast(this, permission);						
	
		int initOrientation = 0;
		/*switch (this.getRequestedOrientation())
		{
		case ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE:	initOrientation = 270;
		case ActivityInfo.SCREEN_ORIENTATION_REVERSE_LANDSCAPE:	initOrientation = 90;
		case ActivityInfo.SCREEN_ORIENTATION_PORTRAIT:	initOrientation = 0;
		case ActivityInfo.SCREEN_ORIENTATION_REVERSE_PORTRAIT:	initOrientation = 180;
		}*/
		initOrientation = 270;
		glView.setTargetOrientation(initOrientation);
	
		orientationListener = new OrientationEventListener(this, SensorManager.SENSOR_DELAY_NORMAL) 
		{ 
				@Override
				public void onOrientationChanged(int orientation) 
				{
					// Divide by 90 into an int to round, then multiply out to one of 5 positions, either 0,90,180,270,360. 
					orientation =  (90 * Math.round(orientation/90))%360; 
					
					// Convert 360 to 0
					if (orientation == 360)
					{
						orientation = 0;
					}				
					
					glView.setTargetOrientation(orientation);
				}
		};

		if (orientationListener.canDetectOrientation())
		{
			orientationListener.enable();
		}	
		
        /*
        // Create the Google Api Client with access to Plus and Games
        googleApiClient = new GoogleApiClient.Builder(this)
            .addConnectionCallbacks(this)
            .addOnConnectionFailedListener(this)
            .addApi(Plus.API).addScope(Plus.SCOPE_PLUS_LOGIN)
            .addApi(Games.API).addScope(Games.SCOPE_GAMES)
            .build();

        googleApiClient.connect(); */

		initialized = true;
	}

	private PackageInfo getPackageInfo() {
        PackageInfo pi = null;
        try {
             pi = this.getPackageManager().getPackageInfo(this.getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
        }
        return pi; 
    }
    
     public void showEULA() {
        PackageInfo versionInfo = getPackageInfo();

       //final String eulaKey = EULA_PREFIX + versionInfo.versionCode;
        final String eulaKey = EULA_PREFIX + this.getString(R.string.eula_version);
        final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
        boolean hasBeenShown = prefs.getBoolean(eulaKey, false);
        
        if(hasBeenShown){
            initApp();
            return;
        }

        // Show the Eula
        String title = this.getString(R.string.app_name) + " v" + versionInfo.versionName;
        
        //Includes the updates as well so users know what changed. 
        //String message = this.getString(R.string.updates) + "\n\n" + this.getString(R.string.eula);
        String message = this.getString(R.string.eula);

        AlertDialog.Builder builder = new AlertDialog.Builder(this)
                .setTitle(title)
                .setMessage(message)
                .setPositiveButton(android.R.string.ok, new Dialog.OnClickListener() {

                    @Override
                    public void onClick(DialogInterface dialogInterface, int i) {
                        // Mark this version as read.
                        SharedPreferences.Editor editor = prefs.edit();
                        editor.putBoolean(eulaKey, true);
                        editor.commit();
                        dialogInterface.dismiss();
                        
                        initApp();
                    }
                })
                .setNegativeButton(android.R.string.cancel, new Dialog.OnClickListener() {

                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        // Close the activity as they have declined the EULA
                        finish(); 
                    }
                    
                });
        builder.create().show();
    }
	
    @Override 
	protected void onCreate(Bundle icicle) {
        super.onCreate(icicle);
		instance = this;

		if (!TERRALibrary.LoadTERRA())
		{
			showDialog(DIALOG_UNSUPPORTED_DEVICE);
			return;
		}

        TERRALibrary.ApplicationInit(0);				
        
		TERRAActivity.instance.initBilling();				
		TERRAActivity.instance.initInterstitials();
        TERRAActivity.instance.initTapjoy();
                
        TERRAUtils.initAnalytics();				
		
		collector = new TERRACrashReporter(this);	
		/*if (collector.hasForceCloseHappened())
		{
			Log.w("App", "Collector: App was forced closed!");
			showDialog(DIALOG_REPORT_FORCE_CLOSE);
		} 			
		else*/
		{
			Log.w("App", "Collector: App was closed sucesfully!");		
			showEULA();			
		}
	}
	
	public boolean initAccelerometer()
	{
		if (!hasAccel)
			return false;

		if (usingAccelerometer)
			return true;
			
		if (mAccelerometer == null)
			mAccelerometer = mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
			
		mSensorManager.registerListener(this, mAccelerometer, SensorManager.SENSOR_DELAY_GAME);		
		usingAccelerometer = true;
			
		gravity[0] = 0.0f;
		gravity[1] = 0.0f;
		gravity[2] = 0.0f;
		
		
		return true;
	}

	public boolean initGyroscope()
	{
		if (!hasGyro)
			return false;
		
		if (usingGyroscope)
			return true;

			if (mGyroscope == null)		
			mGyroscope = mSensorManager.getDefaultSensor(Sensor.TYPE_GYROSCOPE);
		
		mSensorManager.registerListener(this, mGyroscope, SensorManager.SENSOR_DELAY_GAME);		
		usingGyroscope = true;
		
		return true;
	}

	public boolean initCompass()
	{
		if (!hasCompass) 
			return false;

		if (usingCompass) 
			return true;
			
		if (mCompass == null)			
			mCompass = mSensorManager.getDefaultSensor(Sensor.TYPE_ORIENTATION);		
			
		mSensorManager.registerListener(this, mCompass, SensorManager.SENSOR_DELAY_GAME);		
		usingCompass = true;
					
		return true;
	}

	public void stopAccelerometer()
	{
		if (mAccelerometer!=null && usingAccelerometer)
		{
			mSensorManager.unregisterListener(this, mAccelerometer);
			usingAccelerometer = false;
		}
	}

	public void stopGyroscope()
	{
		if (mGyroscope!=null && usingGyroscope)
		{
			mSensorManager.unregisterListener(this, mGyroscope);
			usingGyroscope = false;
		}
	}
	
	public void stopCompass()
	{
		if (mCompass != null && usingCompass)
		{
			mSensorManager.unregisterListener(this, mCompass);
			usingCompass = false;
		}
	}
	
    private ChartboostDelegate chartboostDelegate;
    
    public void initChartboost() 
    {
        String chartboostID = TERRALibrary.ApplicationChartboostGetKey();		
        String chartboostSecret = TERRALibrary.ApplicationChartboostGetSecret();		
        usingChartboost = false;

        if (chartboostID!=null && chartboostSecret!=null) {        
            try {
                Log.d("TERRA", "chartID: " +chartboostID);					
                Log.d("TERRA", "chartsecret: " +chartboostSecret);					
                Chartboost.startWithAppId(this, chartboostID, chartboostSecret);
                
                chartboostDelegate = new ChartboostDelegate() {
                    @Override
                    public void didCompleteRewardedVideo(String location, int reward) {
                        TERRAActivity.instance.glView.purchaseCode = 0;		
                        TERRAActivity.instance.glView.purchaseCredits = reward;
                    }
                    
                    @Override  
                    public void didFailToLoadInterstitial(String location, CBImpressionError error) {
                        TERRAActivity.instance.showRandomAd();
                    }                    
            
                };            
                
                Chartboost.setDelegate(chartboostDelegate);
                Chartboost.setImpressionsUseActivities(true);
                Chartboost.onCreate(this);        
            
                Log.d("TERRA", "Initialized chartboost interstitial ads");				
                usingChartboost = true;
            } catch (Exception e) {
                Log.d("TERRA", "Failed initializing chartboost ads...");					
                e.printStackTrace();
            }
        }
    }
    
    public void initAdMobInterstitial() {
		Log.d("TERRA", "Getting AdMobInterstials key...");		
		interstitialID = TERRALibrary.ApplicationAdMobInterstitialGetID();		
		if (interstitialID != null)
        {
            Log.d("TERRA", "Initialized adMob interstitial ads");				
        } 
        else
		{
			Log.d("TERRA", "Failed initializing adMob interstitial ads...");					
		}
    }
    
    public void initAdBuddiz() {
		String adbuddizID = TERRALibrary.ApplicationAdBuddizGetID();		
        adbuddizEnabled = false;
		if (adbuddizID != null) {
            try {
                AdBuddiz.setPublisherKey(adbuddizID);      
                AdBuddiz.setDelegate(this);
                AdBuddiz.cacheAds(this);		
                adbuddizEnabled = true;
                Log.d("TERRA", "Initialized adBuddiz interstitial ads");				
            } catch (Exception e) {
                e.printStackTrace();
            }
            
        } 
        
        if (!adbuddizEnabled) {
			Log.d("TERRA", "Failed initializing adBuddiz interstitial ads...");					            
		}
    }
    
    @Override
    public void didFailToShowAd(AdBuddizError error) { // no Ad was displayed 
        showRandomAd();
    }
 
    @Override
    public void didCacheAd() {   // an Ad was cached
    } 
    
    @Override
    public void didShowAd() {  // an Ad was displayed
    }  
    
    @Override
    public void didClick() { // the Ad was clicked
    }
    
    @Override
    public void didHideAd() {
    }   
    
    /*private final EventListener vungleListener = new EventListener() {
        @Override
        public void onVideoView(boolean isCompletedView, int watchedMillis, int videoDurationMillis) {
        // Called each time a video completes. isCompletedView is true if >= 80% of the video was watched.
        }
        
        @Override
        public void onAdStart() {
        // Called before playing an ad.
        }
        
        @Override
        public void onAdUnavailable(String reason) {
            TERRAActivity.instance.showRandomAd();
        }
        
        @Override
        public void onAdEnd(boolean wasCallToActionClicked) {
        // Called when the user leaves the ad and control is returned to your application.
        }
        
        @Override
        public void onCachedAdAvailable() {
        // Called when ad playability changes.
        }
    };    
    
    public void initVungle() {
        vunglePub = VunglePub.getInstance();
        
		String vungleID = TERRALibrary.ApplicationVungleGetID();		
        vungleEnabled = false;
		if (vungleID != null) {
            try {
                vunglePub.init(this, vungleID);
                vunglePub.setEventListener(vungleListener);            
                
                vungleEnabled = true;
                Log.d("TERRA", "Initialized vungle interstitial ads");				                
            } catch (Exception e) {
                e.printStackTrace();
            }
            
        } 
        
        if (!vungleEnabled) {
			Log.d("TERRA", "Failed initializing vungle interstitial ads...");					            
		}
    }*/
    
    
	public void initInterstitials() {   
        initAdMobInterstitial();
        initAdBuddiz();
		initChartboost();
        //initVungle();        
	}
	
    public static boolean isBillingAvailable(Context context) {
        final PackageManager packageManager = context.getPackageManager();
        final Intent intent = new Intent("com.android.vending.billing.InAppBillingService.BIND");
        List<ResolveInfo> list = packageManager.queryIntentServices(intent, 0);
        return list.size() > 0;
    }
    
	public void initBilling(){
        hasBilling = false;
        Log.d("App", "Initializing billing...");	
        
        if (!isBillingAvailable(this)) {
            Log.d("App", "Billing not available in this device...");	
            return;
        }
                
		//System.exit(0);		            
        billmaster = new PurchaseProcessor(this, this);
	}
	
    @Override 
	protected void onPause() 
	{
        if (usingChartboost) {
            Chartboost.onPause(this);
        }
    
		if (!initialized)
		{
			super.onPause();
			return;
		}
	
		synchronized (TERRAActivity.instance)
		{				
			stopAccelerometer();
			stopGyroscope();
			stopCompass();
				
			glView.paused = true;
			
			
			super.onPause();

			TERRAMusicPlayer.mute();

			if (glView!=null && glView.isInitialized() && !glView.terminated)
			{
				Log.d("App", "Begin native OnPause");		
				TERRALibrary.ApplicationPause();
				Log.d("App", "End native OnPause");		
			}		
			
			Log.d("App", "Begin OpenGL OnPause");		
			//glView.setVisibility(View.GONE);
			glView.onPause();
			Log.d("App", "End OpenGL OnPause");			
		}
    }

    @Override
    public void onStart() {
        super.onStart();
        
        if (usingChartboost) {
            Chartboost.onStart(this);
        }
    }    
    
    @Override 
	protected void onResume() 
	{
        if (usingChartboost) {
            Chartboost.onResume(this);
        }

		if (!initialized)
		{
			super.onResume();
			return;
		}
	
		synchronized (TERRAActivity.instance)
		{				
			super.onResume();
			glView.onResume();
							
			if (!glView.terminated)
			{
				TERRALibrary.ApplicationResume(); 
				Log.d("App", "TERRA.ONRESUME");
			}

			TERRAMusicPlayer.unmute();
			
			glView.stateChange = 1;
			glView.paused = false;

			
			if (mAccelerometer != null)
				initAccelerometer();

			if (mGyroscope != null)
				initGyroscope();
				
			if (mCompass != null)
				initCompass();
		}
    }

	/*@Override
	public void onWindowFocusChanged(boolean hasFocus) 
	{
		super.onWindowFocusChanged(hasFocus);

		synchronized (TERRAActivity.instance)
		{	
			if (hasFocus && glView.getVisibility() == View.GONE) 
			{
				glView.setVisibility(View.VISIBLE);
			}
		}
	}*/

    @Override
    public void onBackPressed() {
        // If an interstitial is on screen, close it.
        if (usingChartboost && Chartboost.onBackPressed())
            return;
        else
            super.onBackPressed();
    }
    
	@Override
    public void onStop() {
        super.onStop();
        
        if (usingChartboost){
            Chartboost.onStop(this);
        }
    }
    
	@Override
	protected void onDestroy() {	
	
        /*if (googleApiClient!=null)
        {
            googleApiClient.disconnect();
            googleApiClient = null;
        }*/
    
		collector.terminate();

		if (initialized)
		{
			synchronized (TERRAActivity.instance)
			{	
				glView.paused = true;
				glView.onPause();
				
				if (!glView.terminated)
				{
					TERRALibrary.ApplicationShutdown();			
					Log.d("App", "TERRA.ONDESTROY");
				}

				/*if (billmaster != null) 
				{
					billmaster.dispose();
				}*/
				billmaster = null;
			}
		}

        if (usingChartboost){
            Chartboost.onDestroy(this);
        }
		
		super.onDestroy();
		
		android.os.Process.killProcess(android.os.Process.myPid());
	}

	/*@Override
	public void onReceiveAd(Ad ad) 
	{
		Log.d("OK", "Received ad");
	}
	
	@Override
	public void onLeaveApplication(Ad ad)
	{
		Log.d("OK", "Ad clicked, leaving app");
	}

	@Override
	public void onPresentScreen(Ad ad)
	{
		Log.d("OK", "Ad clicked");
	}

	@Override
	public void onDismissScreen(Ad ad)
	{
		Log.d("OK", "Ad closed, resuming app");
	}
	
	@Override
	public void onFailedToReceiveAd(Ad ad, AdRequest.ErrorCode error)
	{
		Log.d("OK", "Ad failed");
	}*/

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) 
	{
		Log.d("ACTIVITYRESULT", "Request code:" +requestCode);
        
        if (billmaster!=null && billmaster.handleActivityResult(requestCode, resultCode, data)) {
            return;
        }        
		
		switch(requestCode) {
        /*case 123:	billingHelper.handleActivityResult(requestCode, resultCode, data);
					break;*/
                    
		case 124:	TERRAFacebook.facebook.authorizeCallback(requestCode, resultCode, data);
					break;
		case 125:	TERRAFacebook.finishPendingAction();
					break;
		case 126:	initApp();			
					break;

		default:
			super.onActivityResult(requestCode, resultCode, data);
		}
    }
	
//        return $MYKEY;

// billing implementation
    @Override
    public void onBillingInitialized() {
        Log.d("App", "In-app Billing set up!");
        hasBilling = true;
    }

    @Override
    public void onBillingDisabled() {
        Log.d("App", "In-app Billing failed!");
        hasBilling = false;
    }

    @Override
    public void onProductPurchased(String productId, TransactionDetails details) {
        Log.d("App", "Purchased: " + productId);
		glView.purchaseID = productId;
		glView.purchaseCode = 0;
        
        if (billmaster!=null)  {
            billmaster.consumePurchase(productId);
        }
    }

    @Override
    public void onBillingError(int errorCode, Throwable error) {
         // Called then some error occured. See Constants class for more details
        Log.d("App", "Error in purchase");
        glView.purchaseID = null;
        glView.purchaseCode = 3;
    }

    @Override
    public void onPurchaseHistoryRestored() {
    }
     		
	public static boolean canPurchase() {
		return hasBilling;
	}
		
	public static void purchase(String sku) {
		//sku = "android.test.purchased";
        
        if (TERRAActivity.instance.billmaster != null) {        
            TERRAActivity.instance.purchaseID = sku;
            TERRAActivity.instance.billmaster.purchase(sku);
         }
         else {
            glView.purchaseID = null;
            glView.purchaseCode = 3;
        }
	}

	public static void purchaseCredits()
	{
		/*TelephonyManager manager = (TelephonyManager)this.getSystemService(this.TELEPHONY_SERVICE);
        if(manager.getPhoneType() == TelephonyManager.PHONE_TYPE_NONE)
		{
			glView.purchaseCode = 4;		
			return;
        }*/
		
		String serviceID = null;
		String serviceSecret = null;
		
		serviceID = TERRALibrary.ApplicationIAPGetServiceID();
		serviceSecret = TERRALibrary.ApplicationIAPGetServiceSecret();
	
		if (serviceID == null || serviceSecret == null)
		{
			glView.purchaseCode = 5;					
			return;
		}
	
		PaymentRequest.PaymentRequestBuilder builder = new PaymentRequest.PaymentRequestBuilder();
        builder.setService(serviceID, serviceSecret);
        builder.setDisplayString("Tokens");      // shown on user receipt
        builder.setProductName("Tokens");  // non-consumable purchases are restored using this value
        builder.setConsumable(true);              // non-consumable items can be later restored
        //builder.setIcon(R.drawable.token);
        PaymentRequest pr = builder.build();
        instance.makePayment(pr);		
	}

    public static AssetManager getAssetManager()
    {        
        return assetManager;
    }
    
    public boolean showAdmobInterstitial() 
    {
		if (interstitialID==null || showedAdMob)
		{
			//Log.d("App", "Interstial ads not correctly setup!");		
			return false;
		}

		if (interstitial==null)
		{
			Log.d("App", "Creating interstial ad...");		
			interstitial = new InterstitialAd(this);
			interstitial.setAdUnitId(interstitialID);
		
			interstitial.setAdListener(new AdListener(){ 
                public void onAdLoaded() { 
                    interstitial.show(); 
                } 
                
                public void onAdFailedToLoad(int errorCode) {
                    showRandomAd();
                }
                
                });
		}
		
		Log.d("App", "Sending interstial ad request...");		
		// Criar a solicitação de anúncio.
		AdRequest adRequest = new AdRequest.Builder().addTestDevice("D2F6D48B12FBC367106C6134DE40A799").build();

		// Começar a carregar o anúncio intersticial.
		interstitial.loadAd(adRequest);
			
		/*if (interstitial!=null && interstitial.isReady()) 
		{
			interstitial.show();
		}*/
        
        showedAdMob = true;
        return true;
    }

    public boolean showChartboostInterstitial() 
    {      
        if (!usingChartboost || showedCharboost)  {
            return false;
        }        

        Log.d("TERRA", "Caching chartboost ads");				
        Chartboost.cacheInterstitial(CBLocation.LOCATION_DEFAULT);
        
        Log.d("TERRA", "Showing chartboost ads");				
        Chartboost.showInterstitial(CBLocation.LOCATION_DEFAULT);                
        
        showedCharboost = true;
        return true;
    }
        
    public boolean showChartboostInterstitialWithReward() 
    {      
        if (!usingChartboost)  {
            return false;
        }        
    
        Log.d("TERRA", "Caching chartboost ads");				
        Chartboost.cacheRewardedVideo(CBLocation.LOCATION_GAMEOVER);

        Log.d("TERRA", "Showing chartboost ads");				
        Chartboost.showRewardedVideo(CBLocation.LOCATION_GAMEOVER);
        
        return true;
    }
    
    public boolean showAdBuddizInterstitial()  {
        if (!adbuddizEnabled || showedAdBuddiz) {
            return false;
        }
        
        AdBuddiz.showAd(TERRAActivity.instance);
        showedAdBuddiz = true;
        return true;
    }
    
    /*public boolean showVungleInterstitial()  {
        if (!vungleEnabled || showedVungle) {
            return false;
        }
        
        AdConfig overrideConfig = new AdConfig();
        //overrideConfig.setIncentivized(true);
        overrideConfig.setSoundEnabled(true);        

        vunglePub.playAd(overrideConfig);
        
        showedVungle = true;
        return true;
    }*/
    
    
    private static int adTryCount = 0;
    
    public static void showRandomAd()
    {
        boolean firstTurn = (adTryCount<=0);
        
        adTryCount++;
        //instance.showChartboostInterstitial();
    
        int randomNum = (int)(Math.random()*8); 
        
        /*if (randomNum==2 && instance.usingChartboost) {
            instance.showChartboostInterstitialWithReward();
        }
        else*/
        
        //randomNum = 4;
        
        boolean result = false;
        
        switch (randomNum) {
        case 0: result = instance.showChartboostInterstitial(); break;
        case 1: result = instance.showChartboostInterstitial(); break;
        case 2:
        case 3: if (!firstTurn) result = instance.showAdBuddizInterstitial(); break;
        //case 4: if (!firstTurn) result = instance.showVungleInterstitial(); break;
               
        default:    
            result = instance.showAdmobInterstitial(); break;
        }
        
        if (!result && adTryCount<20) {
            showRandomAd();
        }

    }

	public static void showFullscreenAds()
	{	
        adTryCount = 0;
        instance.showedCharboost = false;
        instance.showedAdBuddiz = false;
        //instance.showedVungle = false;
        instance.showedAdMob = false;
        
        showRandomAd();
    }
    
    private Hashtable<String,Object> connectFlags = new Hashtable<String,Object>();

    private static String tapjoyAppKey = null;
    private static String tapjoyAppSecret = null;
    private static boolean tapjoyEnabled = false;
    
    public void initTapjoy()
    {        
        if (tapjoyAppKey!=null)
            return;
        
        tapjoyAppKey = TERRALibrary.ApplicationTapjoyGetKey();
        tapjoyAppSecret = TERRALibrary.ApplicationTapjoyGetSecret();
    
        connectFlags.put(TapjoyConnectFlag.ENABLE_LOGGING, "true");
        //connectFlags.put(TapjoyConnectFlag.USER_ID, userID);

        TapjoyConnect.requestTapjoyConnect(getApplicationContext(), tapjoyAppKey, tapjoyAppSecret, connectFlags, new TapjoyConnectNotifier()
        {
        @Override
        public void connectSuccess() {
            //The Connect call suceeded
                Log.d("App","The Connect call suceeded ");
                onTapJoyConnect();
                tapjoyEnabled = true;
            }

        @Override
        
        public void connectFail() {
            //The connect call failed
            Log.d("App", "The connect call failed");
            //TERRAActivity.instance.glView.APIResult(2, 31);
            tapjoyEnabled = false;
        }
        });        
    }
    
    public void onTapJoyConnect()
    {
		// Start preloading your TJEvent content as soon as possible
		directPlayEvent = new TJEvent(this, "video_unit", this);
		directPlayEvent.enablePreload(true);
		directPlayEvent.send();
    
		// Get notifications when Tapjoy views open or close.
		TapjoyConnect.getTapjoyConnectInstance().setTapjoyViewNotifier(new TapjoyViewNotifier()
		{
			@Override
			public void viewWillOpen(int viewType)
			{
                Log.d("App",  "A view is about to open");
			}
			
			@Override
			public void viewWillClose(int viewType)
			{
				Log.d("App", "A view is about to close");
			}
			
			@Override
			public void viewDidOpen(int viewType)
			{
				Log.d("App", "A view did open");
			}
			
			@Override
			public void viewDidClose(int viewType)
			{
				Log.d("App", "A view did close");
				
				// Best Practice: We recommend calling getTapPoints as often as possible so the user�s balance is always up-to-date.
				TapjoyConnect.getTapjoyConnectInstance().getTapPoints(instance);
			}
		});
		
		// Get notifications on video start, complete and error
		TapjoyConnect.getTapjoyConnectInstance().setVideoNotifier(new TapjoyVideoNotifier() {

			@Override
			public void videoStart() {
				Log.d("App",  "video has started");
			}

			@Override
			public void videoError(int statusCode) {
				Log.d("App", "there was an error with the video: " + statusCode);
			}

			@Override
			public void videoComplete() {
				Log.d("App",  "video has completed");
				
				// Best Practice: We recommend calling getTapPoints as often as possible so the user�s balance is always up-to-date.
				TapjoyConnect.getTapjoyConnectInstance().getTapPoints(instance);
			}
			
		});
    }

    private static TJEvent directPlayEvent;
	private static boolean dpEventContentIsAvailable;
    private static boolean shouldTransition;

    public static void showTapJoyOfferWall()
    {
        if (!tapjoyEnabled)
        {
            TERRAActivity.instance.glView.APIResult(2, 31);
            return;
        }
    
        TapjoyConnect.getTapjoyConnectInstance().showOffers(new TapjoyOffersNotifier()
					{
						@Override
						public void getOffersResponse()
						{
							TERRAActivity.instance.glView.APIResult(2,37);
						}
						
						@Override
						public void getOffersResponseFailed(String error)
						{
							TERRAActivity.instance.glView.APIResult(2,34); 
						}
					});        
    }

    public static void showTapJoyVideo()
    {
        if (!tapjoyEnabled)
        {
            TERRAActivity.instance.glView.APIResult(2, 31);
            return;
        }
    
        // Check if content is available and if it is ready to show
        if(dpEventContentIsAvailable)
        {
            if(directPlayEvent.isContentReady())
            {
                TERRAActivity.instance.glView.APIResult(2, 36);
                directPlayEvent.showContent();
                shouldTransition = true;
            }
            else
            {
                TERRAActivity.instance.glView.APIResult(2,35);
            }
                
        }
        else
        {
            TERRAActivity.instance.glView.APIResult(2,35);
        }
    }

    
    public static void spendTapJoyPoints(int ammount)
    {
        if (!tapjoyEnabled)
        {
            TERRAActivity.instance.glView.APIResult(2, 31);
            return;
        }
    
        TapjoyConnect.getTapjoyConnectInstance().spendTapPoints(ammount, new TapjoySpendPointsNotifier()
        {
            @Override
            public void getSpendPointsResponseFailed(String error)
            {
                TERRAActivity.instance.glView.APIResult(2, 32);
            }
            
            @Override
            public void getSpendPointsResponse(String currencyName, int pointTotal)
            {
                TERRAActivity.instance.glView.APIResult(2, 33);                
            }
        });
    }
    
	// TapjoyNotifier Methods
	@Override
	public void getUpdatePointsFailed(String error)
	{
		TERRAActivity.instance.glView.APIResult(2,30);
	}
	
	@Override
	public void getUpdatePoints(String currencyName, int pointTotal)
	{
		Log.d("App", "currencyName: " + currencyName);
		Log.d("App",  "pointTotal: " + pointTotal);
        
        synchronized (TERRAActivity.instance)
		{							
            TERRALibrary.ApplicationTapjoyCredits(pointTotal);
		}                
	}

	// TJEventCallback Methods
	@Override
	public void sendEventCompleted(TJEvent event, boolean contentAvailable) {
		// If content is not available you can note it here and act accordingly as best suited for your app
		dpEventContentIsAvailable = contentAvailable;
		Log.d("App", "Tapjoy send event 'video_unit' completed, contentAvailable: " + contentAvailable);
	}

	@Override
	public void sendEventFail(TJEvent event, TJError error) {
		Log.d("App",  "Tapjoy send event 'video_unit' failed with error: " + error.message);
	}

	@Override
	public void contentIsReady(TJEvent event, int status) {
		Log.d("App","Tapjoy direct play content is ready");
		switch (status) {
			case TJEventPreloadStatus.STATUS_PRELOAD_COMPLETE:
				// handle partial load of cache
				break;
			case TJEventPreloadStatus.STATUS_PRELOAD_PARTIAL:
				// Handle complete load of cache
				break;
			default:
				// Should never get here	
		}
	}

	@Override
	public void contentDidShow(TJEvent event) {
		Log.d("App", "Tapjoy direct play content did show");
	}

	@Override
	public void contentDidDisappear(TJEvent event) {
		Log.d("App", "Tapjoy direct play content did disappear");
			
		// Best Practice: We recommend calling getTapPoints as often as possible so the user's balance is always up-to-date.
		TapjoyConnect.getTapjoyConnectInstance().getTapPoints(instance);
		
		// Begin preloading the next event after the previous one is dismissed
		directPlayEvent = new TJEvent(this, "video_unit", this);
		directPlayEvent.enablePreload(true);
		directPlayEvent.send();
	}

	@Override
	public void didRequestAction(TJEvent event, TJEventRequest request) {
	}
    
    
    public static boolean unlockAchievement(String achieveID)
    {
        /*if (googleApiClient!=null &&  googleApiClient.isConnected()) 
        {
            Games.Achievements.unlock(googleApiClient, achieveID);
            return true;
        }*/
         
        return false;    
    }
    
}
