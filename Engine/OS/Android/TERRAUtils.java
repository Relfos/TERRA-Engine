package com.pascal.terra;

import android.app.Activity;
import java.util.Locale;
import android.content.res.AssetManager;
import java.net.*;
import android.util.Log;
import android.content.Intent;
import android.net.Uri;
import android.accounts.*;
import java.util.*;
import android.content.Context;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.view.inputmethod.InputMethodManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.widget.EditText;
import android.view.View;
import android.view.View.OnKeyListener;
import android.view.KeyEvent;
import java.io.*;
import android.widget.TextView;
import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.app.ActivityManager.RunningServiceInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageInfo;
import android.content.pm.ApplicationInfo;
import android.provider.Settings;
import android.provider.Settings.System;
import android.view.InputDevice;

//import android.app.backup.BackupManager;

import android.graphics.drawable.Drawable;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.Bitmap;

/*import com.purplebrain.adbuddiz.sdk.AdBuddiz;
import com.purplebrain.adbuddiz.sdk.AdBuddizDelegate;
import com.purplebrain.adbuddiz.sdk.AdBuddizError;*/

import com.google.android.gms.ads.*;

import com.flurry.android.FlurryAgent;

import mp.MpUtils;
import mp.PaymentActivity;
import mp.PaymentRequest;
import mp.PaymentResponse;

import com.google.ads.*;

public class TERRAUtils{    

	private static EditText editText = null;
	private static TERRAAdBanner adBanner = null;

	public static void logString(String s)
	{
		Log.w("TERRA", s);
	}

	public static void enableAds(String adMobID)
	{
		if (adBanner==null)
			adBanner = new TERRAAdBanner(adMobID);
	}

	public static void disableAds()
	{
		if (adBanner!=null)
		{
			adBanner.disable();
		}
	}

	public static void sendEmail(String dest, String subject, String body)
	{
		Uri emailUri = Uri.parse("mailto:"+dest);
		Intent intent = new Intent(Intent.ACTION_SENDTO, emailUri);
		//intent.setType("message/rfc822");
				
		if (subject!=null)
			intent.putExtra(Intent.EXTRA_SUBJECT, subject);
		
		if (body!=null)
			intent.putExtra(Intent.EXTRA_TEXT, body);

		TERRAActivity.instance.startActivity(Intent.createChooser(intent, "Send Email"));			 
	}
	
/*	public static void setDeveloperEmail(String email)
	{
		TERRAActivity.instance.developerEmail = email;
	}*/

	public static boolean initAccelerometer()
	{
		return TERRAActivity.instance.initAccelerometer();
	}

	public static boolean initGyroscope()
	{
		return TERRAActivity.instance.initGyroscope();
	}

	public static boolean initCompass()
	{
		return TERRAActivity.instance.initCompass();
	}

	public static void stopAccelerometer()
	{
		TERRAActivity.instance.stopAccelerometer();
	}

	public static void stopGyroscope()
	{
		TERRAActivity.instance.stopGyroscope();
	}

	public static void stopCompass()
	{
		TERRAActivity.instance.stopCompass();
	}
	
	public static boolean isAppRunning(String appPackage) 
	{
		if (appPackage == null) {
			return false;
		}
        
        try {
            final Context context = TERRAActivity.instance;
            final ActivityManager manager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
            final List<RunningAppProcessInfo> runningAppProcesses = manager.getRunningAppProcesses();
            for (final RunningAppProcessInfo app : runningAppProcesses) {
                if (appPackage.equals(app.processName)) {
                    return true;
                }
            }
            
            for (final RunningServiceInfo service : manager.getRunningServices(Integer.MAX_VALUE)) 
            {
                if (appPackage.equals(service.service.getClassName())) 
                {
                    return true;
                }
            }		
		} catch(Exception e)
		{						
			return false;
		}
		
		return false;
	}
	
	public static boolean isDebuggerAttached()
	{
		boolean debuggable = false;
	 
		final Context context = TERRAActivity.instance;
		final PackageManager pm = context.getPackageManager();
		try
		{
			ApplicationInfo appinfo = pm.getApplicationInfo(context.getPackageName(), 0);
			debuggable = (0 != (appinfo.flags &= ApplicationInfo.FLAG_DEBUGGABLE));
		}
		catch(Exception e)
		{						
			return false;
		}
		 
		return debuggable;
	}

	public static boolean isAppInstalled(String appPackage) 
	{
		if (appPackage == null) {
			return false;
		}
        
        try {
            final Context context = TERRAActivity.instance;
            final PackageManager pm = context.getPackageManager();
            //get a list of installed apps.
            List<ApplicationInfo> packages = pm.getInstalledApplications(PackageManager.GET_META_DATA);

            for (ApplicationInfo packageInfo : packages) 
            {			
                //Log.w("PACKAGE INSTALLED----------------->", packageInfo.packageName);
                if (appPackage.equals(packageInfo.packageName)) 
                {
                    return true;
                }
            }		
		} catch(Exception e)
		{						
			return false;
		}
         
		return false;
	}

	//private static InterstitialAd interstitial = null;
	
/*	public static void cacheFullscreenAds(String adMobID)
	{		
	
		// Create the interstitial.
		if (interstitial == null)
		{
			interstitial = new InterstitialAd(TERRAActivity.instance, adMobID);
		}

		// Create ad request.
		AdRequest adRequest = new AdRequest();

		// Begin loading your interstitial.
		interstitial.setAdListener(TERRAActivity.instance);
	}*/
	
	public static void showFullscreenAds()
	{		
		if (TERRAActivity.instance.glView.isOldDevice())
			return;
			
		TERRAActivity.instance.runOnUiThread( new Runnable () {
		@Override
			public void run() {
				TERRAActivity.instance.showFullscreenAds();
				}
			});
	}

	public static void showKeyboard(String s)
	{
		Log.d("TERRA", "Showing keyboard");
		TERRAActivity.instance.runOnUiThread(new Runnable() {
		public void run() {		
				if (editText ==null)
				{
					editText = new EditText(TERRAActivity.instance);
					editText.setBackgroundColor(0);
				}
				
				TERRAActivity.instance.topView.addView(editText);
				editText.setText(null);
				editText.requestFocus();
				
				try
				{				
					InputMethodManager imm = (InputMethodManager) TERRAActivity.instance.getSystemService(Context.INPUT_METHOD_SERVICE);
					imm.toggleSoftInput(InputMethodManager.SHOW_FORCED,0);	
				}
				catch (Exception e)
				{			
					Log.d("TERRA", "Keyboard error: "+ e.toString());
				}				
				
				editText.setOnKeyListener( new OnKeyListener() {
					public boolean onKey(View v, int keyCode, KeyEvent event) {
						Log.d("TERRA", "keycode "+keyCode);
						switch (keyCode) {						
						case KeyEvent.KEYCODE_DPAD_CENTER:
						case KeyEvent.KEYCODE_ENTER:
							saveEditText();
							return true;
						}

						return false;
					}

					private void saveEditText() 
					{
						String text = editText.getText().toString();
						char[] ch = text.toCharArray();
						
						synchronized (TERRAActivity.instance)
						{										
							for (int k = 0; k<100; k++)
								TERRALibrary.ApplicationKeyPress(8);
							for (char c : ch)
								TERRALibrary.ApplicationKeyPress((int)c);
						}

						InputMethodManager imm = (InputMethodManager) TERRAActivity.instance.getSystemService(Context.INPUT_METHOD_SERVICE);
						imm.hideSoftInputFromWindow(TERRAActivity.instance.getCurrentFocus().getWindowToken(), 0);
						TERRAActivity.instance.topView.removeView(editText);
					}
				});
		}
	});
		
	}
	
	public static String getDeviceID()
	{
		String deviceID = Settings.System.getString(TERRAActivity.instance.getContentResolver(),  Settings.System.ANDROID_ID);
		return deviceID;
	}

	
    public static String getInternalDir() 
	{
		return TERRAActivity.instance.getFilesDir().getAbsolutePath();
    }

    public static String getExternalDir() 
	{
		File ext = TERRAActivity.instance.getExternalFilesDir(null);
		if (ext!=null)
			return ext.getPath();
		else
			return null;
    }

	public static int getScreenWidth()
	{
		/*DisplayMetrics displaymetrics = new DisplayMetrics();
		TERRAActivity.instance.getWindowManager().getDefaultDisplay().getMetrics(displaymetrics);
		return displaymetrics.widthPixels;*/
		return TERRAActivity.instance.getResources().getDisplayMetrics().widthPixels;
	}

	public static int getScreenHeight()
	{
		/*DisplayMetrics displaymetrics = new DisplayMetrics();
		TERRAActivity.instance.getWindowManager().getDefaultDisplay().getMetrics(displaymetrics);
		return displaymetrics.heightPixels;*/
		return TERRAActivity.instance.getResources().getDisplayMetrics().heightPixels;
	}

	public static String getLanguage()
	{
		return Locale.getDefault().getLanguage();
	}
    
	public static String getCountry()
	{
		return Locale.getDefault().getCountry();
	}
    	
	public static AssetManager getAssetManager()
	{
		return TERRAActivity.instance.getAssets();
	}

	private static boolean isNetworkAvailable() 
	{
		ConnectivityManager connectivityManager = (ConnectivityManager) TERRAActivity.instance.getSystemService(Context.CONNECTIVITY_SERVICE);
		NetworkInfo[] netInfo = connectivityManager.getAllNetworkInfo();
		
		for (NetworkInfo ni : netInfo) 
		{
			String netName = ni.getTypeName();
			int netType = ni.getType();
			if (ni.isConnected())
			{
				Log.d("TERRA", "Found network: "+netName);
				if (netType==ConnectivityManager.TYPE_WIFI)
					return true;
				if (netType==ConnectivityManager.TYPE_MOBILE)
					return true;
				if (netType==ConnectivityManager.TYPE_ETHERNET)
					return true;
			}
		}
			return false;        
    }
	
	public static String getNetAddress(String name)
	{
		Log.d("TERRA", "Testing network connection ");
		if (!isNetworkAvailable())
			return null;
		try {
			Log.d("TERRA", "Resolving net address: "+name);
			InetAddress addr = InetAddress.getByName(name);
			String result = addr.getHostAddress();
			return result;
		} catch (Exception e)
		{
			Log.d("TERRA", "Address not found!");		
			return null; // return invalid address 
		}		
	}
	
	public static void openURL(String url)
	{
		Intent intent = new Intent(Intent.ACTION_VIEW);
		intent.setData(Uri.parse(url));
		TERRAActivity.instance.startActivity(intent);	
	}
		
	public static String installOpenAL(String sourceFile)
	{
		File mydir = TERRAActivity.instance.getDir("openal", Context.MODE_PRIVATE);
		Log.d("TERRA", "Getting install dir : " + mydir.getAbsolutePath());
		File fileWithinMyDir = new File(mydir, "libopenal.so");
		//PackageManager pm = TERRAActivity.instance.getPackageManager();
		//String dataDir = pm.getApplicationInfo(TERRAActivity.instance.getPackageName(), 0).dataDir;

		Log.d("TERRA", "Install OpenAL ");
        try {
			InputStream in = null;
			FileOutputStream out = new FileOutputStream(fileWithinMyDir);
			in = TERRAActivity.instance.getAssets().open(sourceFile);
			
			byte[] buffer = new byte[1024];
			int read;
			while((read = in.read(buffer)) != -1)
			{
				out.write(buffer, 0, read);
			}

          in.close();
          in = null;
          out.flush();
          out.close();
          out = null;
        } catch(Exception e) {
            Log.e("tag", e.getMessage());
        }
		
		String s = fileWithinMyDir.getAbsolutePath();
		Log.d("TERRA", "Installation done: "+s);
		return s; 
	}      	
	
	
	public static int getConnectedInputDevices()
	{
		int[] deviceIDs = InputDevice.getDeviceIds();
		int result = 0;
		for(int i=0;i<deviceIDs.length;i++)
		{
            InputDevice dev = InputDevice.getDevice(deviceIDs[i]);
			/*if (dev.isVirtual())
				continue;*/
			
			int sources = dev.getSources();
			if ((sources & InputDevice.SOURCE_CLASS_BUTTON) != 0) 
				result++;
        }	
		return result;
	}
	
	public static int getResourceIdByName(String className, String name) 
	{
		Class r = null;
		int id = 0;
			
		try 
		{
			String packageName = TERRAActivity.instance.getPackageName();
			r = Class.forName(packageName + ".R");

			Class[] classes = r.getClasses();
			Class desireClass = null;

			for (int i = 0; i < classes.length; i++) 
			{
				if (classes[i].getName().split("\\$")[1].equals(className)) 
				{
					desireClass = classes[i];
					break;
				}
			}

			if (desireClass != null) 
			{
				id = desireClass.getField(name).getInt(desireClass);
			}

		} 
		catch (Exception e) 
		{
			e.printStackTrace();
			
		}

		return id;
	}
	
	public static Drawable getDrawableFromAssets(String name, int width, int height)
	{
		InputStream stream = null;
		try
		{
			stream = TERRAActivity.instance.getAssets().open(name);
		
			stream.close();
			
			Drawable result = Drawable.createFromStream(stream, null);	
			DisplayMetrics metrics = TERRAActivity.instance.getResources().getDisplayMetrics();
			
			int sourceHeight = 320;
			int targetHeight = sourceHeight;
			
			switch (metrics.densityDpi)
			{
			case DisplayMetrics.DENSITY_LOW:
				targetHeight = 120;
				break;
				
			case DisplayMetrics.DENSITY_MEDIUM:
				targetHeight = 160;
				break;
				
			case DisplayMetrics.DENSITY_HIGH:
				targetHeight = 240;
				break;
				
			case DisplayMetrics.DENSITY_XHIGH:
				targetHeight = 320;
				break;

			/*case DisplayMetrics.DENSITY_XXHIGH:
				targetHeight = 480;
				break;

			case DisplayMetrics.DENSITY_XXXHIGH:
				targetHeight = 640;
				break;*/
				
			case DisplayMetrics.DENSITY_TV:
				targetHeight = 213;
				break;
				
			/*case DisplayMetrics.DENSITY_400:
				targetHeight = 400;
				break;*/
			}
			
			
			if (targetHeight == sourceHeight)
				return result;
			
			float scale = (float)targetHeight / (float)sourceHeight;
		
			Bitmap b = ((BitmapDrawable) result).getBitmap();
			Bitmap bitmapResized = Bitmap.createScaledBitmap(b, (int) (width * scale), (int)(height * scale), false);
			return new BitmapDrawable(TERRAActivity.instance.getResources(), bitmapResized);				
		}
        catch(Exception e) 
		{
            Log.e("tag", e.getMessage());
			return null;
        }			
	}
	

	public static void saveToCloud()
	{
		//BackupManager bm = new BackupManager(TERRAActivity.instance);
		//bm.dataChanged();
	}
	
	public static int getCPUCores()
	{
		return Runtime.getRuntime().availableProcessors();
	}

	public static String flurryID = null;	

   	public static void sendAnalytics(String eventName, String value)
	{
		if (flurryID == null)
			return;
	
		if (value != null)
		{
			HashMap<String, String> parameters = new HashMap<String, String>();
			parameters.put("Value", value);
			FlurryAgent.onEvent(eventName, parameters);
		}
		else
			FlurryAgent.onEvent(eventName);
	}

	public static void initAnalytics()
	{
		Log.d("Analytics", "Getting Flurry key...");		
		synchronized (TERRAActivity.instance)
		{					
			flurryID = TERRALibrary.ApplicationFlurryGetID();
		}
		
		if (flurryID != null)
		{
			Log.d("Analytics", "Initializing Flurry, with key: "+flurryID);		
			FlurryAgent.onStartSession(TERRAActivity.instance, flurryID);	
		}
		else
		{
			Log.d("Analytics", "Failed initializing Flurry...");		
		}
	}
    
    public static void shutdownAnalytics()
    {
		if (flurryID!=null)
			FlurryAgent.onEndSession(TERRAActivity.instance);
    }

    public static String getBundleVersion()
    {
        Context context = TERRAActivity.instance;
        try {
            String packageName = context.getPackageName();
            PackageManager pm = context.getPackageManager();        
            PackageInfo packageInfo = pm.getPackageInfo(packageName, 0);
            return packageInfo.versionName;        
        } catch(Exception e)
		{						
			return null;
		}
    }
    
    public static ClassLoader getClassLoader() {
        Context context = TERRAActivity.instance;
        return context.getClassLoader();
    }

     public static void spawnThread(int pointer) {
            final int arg = pointer;
            Thread myThread = new Thread(){
                public void run(){
                    TERRALibrary.ApplicationThreadExecute(arg);                        
                    }
            };
            myThread.start();
    }

	/*public static void msgBox(String msg)
	{
		AlertDialog alertDialog = new AlertDialog.Builder(
						AlertDialogActivity.this).create();

		// Setting Dialog Title
		alertDialog.setTitle("Alert");

		// Setting Dialog Message
		alertDialog.setMessage(msg);

		// Setting Icon to Dialog
		//alertDialog.setIcon(R.drawable.tick);

		// Setting OK Button
		alertDialog.setButton("OK", new DialogInterface.OnClickListener() {
				public void onClick(DialogInterface dialog, int which) {
				// Write your code here to execute after dialog closed
				//Toast.makeText(getApplicationContext(), "You clicked on OK", Toast.LENGTH_SHORT).show();
				}
		});

		// Showing Alert Message
		alertDialog.show();	
	}*/
}
