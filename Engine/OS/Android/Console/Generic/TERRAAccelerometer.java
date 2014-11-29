package com.pascal.terra;

import android.app.Activity;
import android.content.Context;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;

public class TERRAAccelerometer implements SensorEventListener 
{
	private float lastX, lastY, lastZ;
	private boolean hasEvent;
	private boolean running;


	public TERRAAccelerometer() 
	{
		lastX = 0;
		lastY = 0;
		lastZ = 0;
		running = false;
		hasEvent = false;
		resume();
	}

	public void resume() 
	{
		if (running) return;
		
		running = true;
	}

	public void pause() 
	{
		if (!running) 
			return;
		running = false;
	}

	public void onAccuracyChanged(Sensor sensor, int accuracy) 
	{
	}

	@Override
	public void onSensorChanged(SensorEvent event) {
		lastX = event.values[0];
		lastY = event.values[1];
		lastZ = event.values[2];
		hasEvent = true;
	}
	
	public boolean hasEvents()
	{
		boolean result = hasEvent;
		hasEvent = false;
		return result;
	}
	
	public float getX()
	{
		return lastX;
	}

	public float getY()
	{
		return lastY;
	}

	public float getZ()
	{
		return lastZ;
	}
	
}