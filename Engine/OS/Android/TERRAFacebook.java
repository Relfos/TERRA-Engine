package com.pascal.terra;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.LinearLayout;
import android.util.Log;
import android.content.Context;
import android.util.DisplayMetrics;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.net.Uri;

import org.json.JSONArray;
import org.json.JSONObject;

import java.lang.Runnable;

import com.facebook.android.*;
import com.facebook.android.Facebook.*;

public class TERRAFacebook  
{
	public interface IFacebookAction
	{		
		void Commit();
	}

	public static Facebook facebook = null;
	public static IFacebookAction callbackAction = null;
	private static SharedPreferences mPrefs;
	
	private static String facebookID = null;
	
	public TERRAFacebook(String facebookID)
	{
		this.facebookID = facebookID;		
	}	
	
	public static void finishPendingAction()
	{
		if (callbackAction == null)
			return;
			
		DoFacebookAction(facebookID, callbackAction);			
		
		callbackAction = null;
	}

	private static class FacebookTask extends AsyncTask<IFacebookAction, Integer, Integer> 
	{
	 @Override
     protected Integer doInBackground(IFacebookAction... actions) 
	 {
		Log.d("TERRA", "Starting facebook task");
        for (int i = 0; i < actions.length; i++)
		{
			Log.d("TERRA", "Committing action...");
			actions[i].Commit();
			Log.d("TERRA", "Action committed...");
        }
         return 0;
     }

	 @Override
     protected void onProgressUpdate(Integer... progress) 
	 {
		//do nothing
     }

	 @Override
     protected void onPostExecute(Integer result) 
	 {
         Log.d("TERRA", "Finished facebook task");
     }
 }
 	
	public class FacebookPost implements IFacebookAction
	{	
		private String msg;
		private String link;
		private String desc;
		private String imageURL;

		public FacebookPost(String msg, String link, String desc, String imageURL)
		{
			this.msg = msg;
			this.link = link;
			this.desc = desc;
			this.imageURL = imageURL;
		}
		
		public void Commit()
		{
			Log.d("TERRA", "Preparing facebook message");
			try {
				String response = facebook.request("me");
				Bundle parameters = new Bundle();
				
				if (msg!=null)
					parameters.putString("message", msg);
				if (desc!=null)
					parameters.putString("description", desc);
				if (link!=null)
					parameters.putString("link", link);
				if (imageURL!=null)
					parameters.putString("picture", imageURL);
					
					
				Log.d("TERRA", "Posting to facebook: " + msg);
				response = facebook.request("me/feed", parameters,  "POST");
				
				Log.d("TERRA", "got response: " + response);				
				if (response == null || response.equals("") || response.equals("false") || response.indexOf("error")>=0) 
				{
					Log.v("Error", "Facebook error!");
					TERRAActivity.instance.glView.APIResult(1, 2);
				}
				else			
					TERRAActivity.instance.glView.APIResult(1, 1);
			 } 
			 catch(Exception e) 
			 {
				Log.d("TERRA", "Facebook error....");
				e.printStackTrace();
				TERRAActivity.instance.glView.APIResult(1,  2);
			 }		
		}
	}
	
	public class FacebookLike implements IFacebookAction
	{	
		protected String page;
		protected String url;
		
		public FacebookLike(String page, String URL)
		{
			this.page = page;
			this.url = URL;
		}
		
		public void onFail(String pageid)
		{
				
			if (pageid!=null)
			{								
				Intent faceview = null;								
				/*try 
				{
					TERRAActivity.instance.getPackageManager().getPackageInfo("com.facebook.katana", 0);
					faceview = new Intent(Intent.ACTION_VIEW, Uri.parse("fb://profile/"+url));
				}
				catch (Exception e) */
				{
					faceview = new Intent(Intent.ACTION_VIEW);						
					faceview.setData(Uri.parse("https://www.facebook.com/"+url));					
				}						

				callbackAction = new FacebookLikeResult(page, url);
				TERRAActivity.instance.startActivityForResult(faceview, 125);
			}
			else
			{
				Log.v("Error", "Facebook Error!");
				TERRAActivity.instance.glView.APIResult(1, 2);
			}					
		}
		
		public void Commit()
		{
			try 
			{														
				String response = facebook.request("me");
							
				Log.d("TERRA", "obtaining ID for "+url);
				response = facebook.request(url);
				Log.d("TERRA", "got response: " + response);

				if (response == null || response.equals("") || response.equals("false") || response.indexOf("error")>=0) 
				{
					Log.v("Error", "Facebook Error!");
					TERRAActivity.instance.glView.APIResult(1, 2);
					return;
				}
				
				JSONObject jsonObject = new JSONObject(response);
				String pageid = null;
				if (jsonObject.has("id"))
				{
					pageid = jsonObject.getString("id");
					Log.d("TERRA", "Found ID: "+pageid);
				}									
				
				Log.d("TERRA", "Checking facebook like: "+ page);
				response = facebook.request("me/likes");
				Log.d("TERRA", "got response: " + response);
						   
				if (response == null || response.equals("") || response.equals("false") || response.indexOf("error")>=0) 
				{
					Log.v("Error", "Facebook Error!");
					TERRAActivity.instance.glView.APIResult(1, 2);
					return;
				}

				if (response.indexOf(page)>=0 || response.indexOf(pageid)>=0)
				{
					TERRAActivity.instance.glView.APIResult(1, 3);
					return;
				}
				else
				{	
					onFail(pageid);
				}
			}
			catch(Exception e) 
			{
				Log.d("TERRA", "Facebook error....");
				e.printStackTrace();
				TERRAActivity.instance.glView.APIResult(1, 2);
			}		
		}
	}

	public class FacebookLikeResult extends FacebookLike
	{			
		public FacebookLikeResult(String page, String URL)
		{
			super (page, URL);
		}
		
		@Override
		public void onFail(String pageid)
		{
			TERRAActivity.instance.glView.APIResult(1, 4);
		}
		
	}
	
	public static void DoFacebookAction(final String facebookID, final IFacebookAction action)
	{
		TERRAActivity.instance.runOnUiThread(new Runnable() {
		public void run() {		
			if (facebook==null)
			{
				Log.d("TERRA", "Initializing facebook: " + facebookID);
				facebook = new Facebook(facebookID);
				Log.d("TERRA", "Extending tokens");
			}
					
			facebook.extendAccessTokenIfNeeded(TERRAActivity.instance, null);
				
			Log.d("TERRA", "Handling tokens");
			mPrefs = TERRAActivity.instance.getPreferences(Context.MODE_PRIVATE);
			String access_token = mPrefs.getString("access_token", null);
			long expires = mPrefs.getLong("access_expires", 0);
						
			if(access_token != null) 
			{
				facebook.setAccessToken(access_token);
			}
			
			if(expires != 0) 
			{
				facebook.setAccessExpires(expires);
			}
			
			// Only call authorize if the access_token has expired.
			if(!facebook.isSessionValid()) 
			{	
				Log.d("TERRA", "Authorizing facebook");
			
				facebook.authorize(TERRAActivity.instance, new String[] {"publish_stream", "user_likes"}, 124, new DialogListener() {
					@Override
					public void onComplete(Bundle values) 
					{
						SharedPreferences.Editor editor = mPrefs.edit();
						editor.putString("access_token", facebook.getAccessToken());
						editor.putLong("access_expires", facebook.getAccessExpires());
						editor.commit();

						new FacebookTask().execute(action);						
					}

					@Override
					public void onFacebookError(FacebookError error) 
					{
						Log.d("TERRA", "Facebook error: "+error);
						TERRAActivity.instance.glView.APIResult(1, 5);
					}

					@Override
					public void onError(DialogError e) 
					{
						Log.d("TERRA", "Facebook error: "+e);
						TERRAActivity.instance.glView.APIResult(1, 5);
					}
					

					@Override
					public void onCancel() {}
				});	
			}
			else
			{
				new FacebookTask().execute(action);						
			}
		}});
	}
	
	
	public void post(String msg, String link, String desc, String imageURL)
	{
		Log.d("TERRA", "Posting facebook message: "+msg);
		IFacebookAction myAction = new FacebookPost(msg, link, desc, imageURL);
		DoFacebookAction(facebookID, myAction);
	}

	public void likePage(String page, String URL)
	{
		Log.d("TERRA", "Liking facebook page: "+page);
		IFacebookAction myAction = new FacebookLike(page, URL);
		DoFacebookAction(facebookID, myAction);
	}

}