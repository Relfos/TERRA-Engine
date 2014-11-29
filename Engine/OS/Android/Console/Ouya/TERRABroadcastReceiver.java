package com.pascal.terra;

import android.content.Intent;
import android.util.Log;
import android.content.Context;
import android.content.BroadcastReceiver;
import tv.ouya.console.api.*;

public class TERRABroadcastReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        if(intent.getAction().equals(OuyaIntent.ACTION_MENUAPPEARING)) 
		{
				if (!TERRAActivity.ouyaMenuVisible)
				{
					TERRAActivity.ouyaMenuVisible = true;
					Log.d("App", "Menu detected, shutting down sound");							
					if (TERRAMusicPlayer.instance!=null)
					{
						//TERRAActivity.tempVolume = TERRAMusicPlayer.instance.getVolume();
						//TERRAMusicPlayer.instance.setVolume(0);
					}
				}
        }
    }
}