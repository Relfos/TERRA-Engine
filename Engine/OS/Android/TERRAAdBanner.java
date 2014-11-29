package com.pascal.terra;

import android.app.Activity;
import android.os.Bundle;
import android.content.Intent;
import android.content.Context;
import android.view.View;

import android.widget.RelativeLayout;
import android.widget.LinearLayout;

import android.util.Log;
import android.util.DisplayMetrics;
import android.util.TypedValue;

import java.lang.Runnable;

import com.google.android.gms.ads.*;

public class TERRAAdBanner extends AdListener implements  Runnable {
  private AdView adView;
  private String adID = null;
  private boolean disabled = false;
  
	public TERRAAdBanner(String adMobID)
	{	
		Log.d("AdMob", "Received adMob ID "+adMobID);
		this.adID = adMobID;

		/*Log.d("AdMob", "Launching adBannerActivity");
		Intent myBannerActivity = new Intent(nativeActivity, TERRAAdBanner.class);
		nativeActivity.startActivity(myBannerActivity);
		*/

		Log.d("AdMob", "Resizing viewport");
		DisplayMetrics displaymetrics = new DisplayMetrics();
		TERRAActivity.instance.getWindowManager().getDefaultDisplay().getMetrics(displaymetrics);
		
		int adSize = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 50, TERRAActivity.instance.getResources().getDisplayMetrics());
		
		synchronized (TERRAActivity.instance)
		{						
			TERRALibrary.ApplicationViewport(0, adSize, displaymetrics.widthPixels, displaymetrics.heightPixels);
		}

		Log.d("AdMob", "Calling myActivity.runOnUiThread");
		TERRAActivity.instance.runOnUiThread(this);
	}

	
	public void disable()
	{
		this.disabled = true;
		if (adView != null) 
		{
			DisplayMetrics displaymetrics = new DisplayMetrics();
			TERRAActivity.instance.getWindowManager().getDefaultDisplay().getMetrics(displaymetrics);
			TERRALibrary.ApplicationViewport(0, 0, displaymetrics.widthPixels, displaymetrics.heightPixels);
		
			TERRAActivity.instance.runOnUiThread(new Runnable() {
				@Override
				public void run() {
					adView.setEnabled(false);
					adView.setVisibility(View.GONE);
				}
			});		
		}
	}
		
	public void hide() 
	{
		if (adView != null) 
		{
			try 
			{
				DisplayMetrics displaymetrics = new DisplayMetrics();
				TERRAActivity.instance.getWindowManager().getDefaultDisplay().getMetrics(displaymetrics);
				
				synchronized (TERRAActivity.instance)
				{								
					TERRALibrary.ApplicationViewport(0, 0, displaymetrics.widthPixels, displaymetrics.heightPixels);
				}
	
				adView.destroy();
				adView = null;
			}
			catch (Exception e)
			{			
				Log.d("TERRA", "Adbanner error: "+ e.toString());
			}								
		}
	}

	@Override
	public void run()
	{
		Log.d("AdMob", "Creating adview");
		// Create the adView
		adView = new AdView(TERRAActivity.instance);
		adView.setAdUnitId(adID);
		adView.setAdSize(AdSize.BANNER);

		adView.setFocusable(true);
		adView.setFocusableInTouchMode(true);
			
		RelativeLayout.LayoutParams adParams = new RelativeLayout.LayoutParams(
			RelativeLayout.LayoutParams.FILL_PARENT, 
            RelativeLayout.LayoutParams.WRAP_CONTENT); 
		
		try
		{
			TERRAActivity.instance.topView.addView(adView, adParams);
		}
		catch (Exception e)
		{			
			Log.d("TERRA", "Adbanner error: "+ e.toString());
		}								
					
					
		Log.d("AdMob", "Setup adlistener");
		adView.setAdListener(this);
		loadNextAd();
	}
	
	public void loadNextAd()
	{
		if (this.disabled)
			return;
			
		Log.d("AdMob", "Creating request");
		AdRequest request = new AdRequest.Builder().addTestDevice("D2F6D48B12FBC367106C6134DE40A799").build();
		
		Log.d("AdMob", "Sending request");
		adView.loadAd(request);	
	}
	
  
	@Override
	public void onAdLoaded()
	{
		Log.d("AdMob", "Received ad!");
		
		/*RelativeLayout.LayoutParams glParams = new RelativeLayout.LayoutParams
		(displaymetrics.widthPixels, displaymetrics.heightPixels - 50);
		
		glParams.topMargin =  50;
		TERRAActivity.instance.glView.setLayoutParams(glParams);*/
	}
	
	@Override
	public void onAdFailedToLoad(int error)
	{
		TERRAActivity.instance.topView.removeView(adView);

		/*RelativeLayout.LayoutParams glParams = new RelativeLayout.LayoutParams
		(displaymetrics.widthPixels, displaymetrics.heightPixels);
		
		glParams.topMargin =  0;
		TERRAActivity.instance.glView.setLayoutParams(glParams);		
		*/
		
		/*DisplayMetrics displaymetrics = new DisplayMetrics();
		TERRAActivity.instance.getWindowManager().getDefaultDisplay().getMetrics(displaymetrics);		
		TERRALibrary.ApplicationViewport(0, 0, displaymetrics.widthPixels, displaymetrics.heightPixels );*/
		
		
		Log.d("AdMob", "failed to receive ad (" + error + ")");
				
		if (error == AdRequest.ERROR_CODE_NO_FILL)
			loadNextAd();
	}
	
	@Override
  	public void onAdOpened()
	{
	}
	
	@Override
	public void onAdClosed()
	{
		loadNextAd();
	}
	
	@Override
	public void onAdLeftApplication()
	{
	}	 
}