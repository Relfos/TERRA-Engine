package com.pascal.terra;

import android.content.res.AssetFileDescriptor;
import android.media.MediaPlayer;
import android.app.Activity;
import android.util.Log;
import java.io.IOException;

public class TERRAMusicPlayer {
    private MediaPlayer player = null;
	private boolean playing = false;
	private int volume;
	
	public static TERRAMusicPlayer instance;
    
    private final static int MAX_VOLUME = 100;
 
	public TERRAMusicPlayer()
	{	
		instance = this;
		volume = 50;
	}
	
	public void setTrack(String fileName)
	{
		try {
			System.out.println("Opening file descriptor for "+fileName);
			AssetFileDescriptor afd = TERRAActivity.instance.getAssets().openFd(fileName);
			if (player!=null)
			{
				System.out.println("Reseting player");
				player.reset();
			}
			else
			{
				System.out.println("Creating new player");
				player = new MediaPlayer();
			}
				
			System.out.println("Setting data source...");
			player.setDataSource(afd.getFileDescriptor(),afd.getStartOffset(),afd.getLength());
			player.prepare();	
			
		} catch (IOException e) {
			System.out.println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TERRAAudioPlayer exception in "+fileName+": "+e.getClass().getName());
			player = null;
		}	
		
		playing = false;
		if (player!=null)
			player.setLooping(true); // Set looping
    }
	
    public void play() 
	{
		if (playing || player==null)
			return;
        player.start();
		playing = true;
    }

    public void pause() 
	{
		if (!playing || player==null)
			return;
			
        player.pause();
		playing = false;
    }

	public int getVolume()
	{
		return volume;
	}
	
	public void setVolume(int volume)
	{
		if  (player!=null)
        {
            //float value = (float) (1 - (Math.log(MAX_VOLUME - volume) / Math.log(MAX_VOLUME)));
            float value = volume / 100.0f;
			player.setVolume(value, value);
        }
			
		this.volume = volume;
	}

    public void stop() 
	{
		if (!playing || player==null) 
			return;
        player.stop();
		playing = false;
    }

    public void release() 
	{
    }
    
    private static int tempVolume;
    private static boolean muted;
    
    public static void mute()
    {
        if (muted || instance == null)
            return;
            
        Log.d("App", "Pausing music ");
        tempVolume = instance.volume;
        instance.setVolume(0);
        muted = true;
    }
    
    public static void unmute()
    {
        if (!muted || instance == null)
            return;
                    
        Log.d("App", "Resuming music");
        instance.setVolume(tempVolume);
        muted = false;
    }
    
}
