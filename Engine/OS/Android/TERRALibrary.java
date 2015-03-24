package com.pascal.terra;

import android.util.Log;
import android.os.Build;

import java.lang.Exception;
import java.io.IOException;
import java.io.InputStream;

// Wrapper for native library

public class TERRALibrary {

	public static String errorMessage = null;

	private static String ReadCPUinfo()
	{
		ProcessBuilder cmd;
		String result="";

		try{
			String[] args = {"/system/bin/cat", "/proc/cpuinfo"};
			cmd = new ProcessBuilder(args);

			Process process = cmd.start();
			InputStream in = process.getInputStream();
			byte[] re = new byte[1024];
			while(in.read(re) != -1){			
				result = result + new String(re);
			}
			in.close();
		} catch(IOException ex)
		{
			ex.printStackTrace();
			return "empty";
		}
		
		return result;
	}	
	
    public static boolean LoadTERRA()
	{
		String cpuType = Build.CPU_ABI.toLowerCase();

        Log.d("terra", "Detected cpu type: " + cpuType);
		
		String caps = ReadCPUinfo();
		
		String terraLib = null;
		
		boolean hasVFPV3 =  (caps.indexOf("vfpv3")>0 );
		
		boolean isARMV7 = (cpuType.equals("armv7l") || cpuType.equals("armeabi") || cpuType.equals("armeabi-v7a"));
		boolean isARMV5 = (cpuType.equals("arm") || cpuType.equals("armv5l") || cpuType.equals("armv6l") );
		boolean isX86 = (cpuType.equals("x86") || cpuType.equals("i386") || cpuType.equals("i486") || cpuType.equals("i586") || cpuType.equals("i686"));
		
		if (isARMV7 && hasVFPV3)
			terraLib = "terraARMV7";			
		else
		if (isARMV5 || (isARMV7 && !hasVFPV3)) 
			terraLib = "terraARMV5";
		else
		if (isX86) 
			terraLib = "terraX86";

		if (terraLib != null)
		{           	  
			Log.d("terra", "loading native library: "+terraLib);
			try 
			{
				System.loadLibrary(terraLib);    
			}
			catch (Exception e)
			{
				errorMessage = "Native code library failed to load.\n" + e.toString();
				Log.d("terra", errorMessage);
				return false;
			}		
			
			return true;
	    }
		else
		{
			errorMessage = "Unsupported CPU type: " + cpuType;
			return false;
		}
    }

	public static native void ApplicationInit(int isOUYA);
	public static native void ApplicationShutdown();
	public static native boolean ApplicationUpdate();
	public static native void ApplicationPause();
	public static native void ApplicationResume();
	public static native void ApplicationContextLost();
	
	public static native void ApplicationResize(int w, int h);
	public static native void ApplicationViewport(int x1, int y1, int x2, int y2);
		
	public static native void ApplicationTouchMove(int x, int y);
	public static native void ApplicationTouchBegin(int x, int y);
	public static native void ApplicationTouchEnd(int x, int y);

	public static native void ApplicationSetState(int state);
	
	public static native void ApplicationSetOrientation(int orientation);
	public static native int ApplicationGetOrientation();

	public static native void ApplicationOnAccelerometer(float x, float y, float z);
	public static native void ApplicationOnGyroscope(float x, float y, float z);
	public static native void ApplicationOnCompass(float heading, float pitch, float roll);
		
	public static native void ApplicationKeyPress(int key);
	public static native void ApplicationKeyDown(int key);
	public static native void ApplicationKeyUp(int key);
	
	public static native void ApplicationListFile(String filename, int size);
	
	public static native void ApplicationIAPConfirm(String iapid);
	public static native void ApplicationIAPError(int errorcode);
	public static native void ApplicationIAPCredits(int credits);
       
    public static native void ApplicationThreadExecute(int pointer);
	
	public static native String ApplicationFlurryGetID();
    public static native String ApplicationAdBuddizGetID();
    public static native String ApplicationVungleGetID();
    
	public static native String ApplicationAdMobInterstitialGetID();
	public static native String ApplicationIAPGetID();
	public static native String ApplicationIAPGetServiceID();
	public static native String ApplicationIAPGetServiceSecret();
        
    public static native String ApplicationTapjoyGetKey();
    public static native String ApplicationTapjoyGetSecret();
    public static native void ApplicationTapjoyCredits(int credits);

    public static native String ApplicationChartboostGetKey();
    public static native String ApplicationChartboostGetSecret();
    
	public static native void ApplicationAPIResult(int API, int code);
	
	public static native long cpuFeatures();
	public static native int cpuCount();
	public static native int cpuFamily();
}
