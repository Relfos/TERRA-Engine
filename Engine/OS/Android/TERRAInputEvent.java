package com.pascal.terra;

import android.content.Context;
import android.graphics.PixelFormat;
//import android.opengl.GLSurfaceView;
import android.util.AttributeSet;
import android.util.Log;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.Surface;
import android.view.Display;
import android.view.WindowManager;

import android.os.SystemClock;
import android.os.Process;

import java.util.LinkedList;
import java.util.Queue;


public class TERRAInputEvent
{        
   public enum InputType {
        TOUCH_BEGIN, TOUCH_END, TOUCH_MOVE, KEY_DOWN, KEY_UP
    }    

    public int x;
    public int y;
    public InputType type;
    public long time;

 
    public TERRAInputEvent(int x, int y, InputType type)
    {
        this.x = x;
        this.y = y;
        this.type = type;
        this.time = SystemClock.uptimeMillis();
    }
}
