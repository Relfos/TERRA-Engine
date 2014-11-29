package com.pascal.terra;

import android.content.res.AssetFileDescriptor;
import android.media.AudioFormat;
import android.media.AudioManager;
import android.media.AudioTrack;
import android.media.AudioTrack.OnPlaybackPositionUpdateListener;
import android.app.Activity;
import java.io.IOException;
import android.util.Log;

public class TERRAAudioTrack {
    private  AudioTrack track = null;
	private int size;
	private int channels;
    private boolean finished = false;
    private boolean paused = false;
	private boolean started = false;
	private int audioLength;
	
	public TERRAAudioTrack(int channels, int frequency, int size, short[] data)
	{	
		this.channels = size;
		this.size = size;

		int bufSize;
        if (channels == 2)
		{
            bufSize = AudioTrack.getMinBufferSize(frequency, AudioFormat.CHANNEL_CONFIGURATION_STEREO, AudioFormat.ENCODING_PCM_16BIT);		
		}
		else
		{
            bufSize = AudioTrack.getMinBufferSize(frequency, AudioFormat.CHANNEL_CONFIGURATION_MONO, AudioFormat.ENCODING_PCM_16BIT);		
		}	
		
		Log.d("AudioTrack", "Channels: "+channels);
		Log.d("AudioTrack", "Frequency: "+frequency);
		Log.d("AudioTrack", "TotalSize: "+size);
		Log.d("AudioTrack", "MinSize: "+bufSize);
			
		if (size>bufSize) 
			bufSize = size;
			
		if ((bufSize % 2)!=0)
			bufSize++;
			
		Log.d("AudioTrack", "BufSize: "+bufSize);		
			
		if (channels == 2)
		{
			track = new AudioTrack(AudioManager.STREAM_MUSIC, frequency, AudioFormat.CHANNEL_CONFIGURATION_STEREO, AudioFormat.ENCODING_PCM_16BIT, bufSize, AudioTrack.MODE_STATIC);
			audioLength = bufSize / 4;
		}
		else
		{
			track = new AudioTrack(AudioManager.STREAM_MUSIC, frequency, AudioFormat.CHANNEL_CONFIGURATION_MONO , AudioFormat.ENCODING_PCM_16BIT, bufSize, AudioTrack.MODE_STATIC);
			audioLength = bufSize / 2;
		}	
		
		track.write(data, 0, size / 2);
		paused = true;
		started = false;
    }
	
    public void play() 
	{
		if (track == null || !paused)
			return;
			
		Log.d("AudioTrack", "Starting playing");

			
		try {
        track.play();
		} catch (IllegalStateException e)
		{
			System.err.println("Caught Exception: " + e.getMessage());
			finished = false;
			paused = false;
			started = false;
			track = null;
			return;
		}

		Log.d("AudioTrack", "Playing now");
		finished = false;
		paused = false;
		started = true;
    }
	
	public void setVolume(float volume)
	{
		if (track == null)
			return;
		track.setStereoVolume(volume, volume);
	}
	
	public void setLoop(boolean loop)
	{
		if (track == null)
			return;
		int count;
		if (loop) count = -1; else count = 0;
		track.setLoopPoints(0, size, count);
	}

    public void stop() 
	{	
		if (track == null || paused || !started)
			return;
  
		track.stop();
		paused = true;
    }

    public void release() 
	{
		if (track == null)
			return;
		if (started)
		{
			track.stop();
		}
        track.release();
		track = null;
		paused = false;
		finished = true;
		//Log.d("AudioTrack", "TRACK RELEASED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
    }
	
	public int getState()
	{
		if (track == null || finished)
			return 0;

		int pos = track.getPlaybackHeadPosition();
		//Log.d("AudioTrack", "TESTING TRACK! length:"+audioLength+" pos:"+pos);
	
		if (pos>=audioLength)
		{
			finished = true;
			//Log.d("AudioTrack", "TRACK IS 2FINISHED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
			return 0;
		}
	
		if (paused)
			return 2;
			
		return 1;
	}
	
}
