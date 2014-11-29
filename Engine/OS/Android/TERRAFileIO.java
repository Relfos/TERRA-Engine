package com.pascal.terra;

import android.app.Activity;  
import android.content.res.Resources;  
import android.content.res.AssetManager;
import android.os.Bundle;  
import android.content.Context;
import android.util.Log;  


import java.lang.Exception;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TERRAFileIO {
    private Activity myActivity = null;
	private String fileName;
	private byte[] buffer = null;
	private int size;
    
    private static final String header = "apk:";
	
	private static Map<String, AssetManager> packageMap = new HashMap<String, AssetManager>();
	
	private static AssetManager getAssetManager(String fileName)
	{		
		int i = fileName.indexOf(header);
		int j = fileName.indexOf("/");
		if (i<0 || j<0)
			return TERRAActivity.instance.getAssets();
			
		String packageName = fileName.substring(i+header.length(), j);
		AssetManager manager = null;
		
		manager = (AssetManager) packageMap.get(packageName);
		if (manager!=null)
			return manager;
		
		try
		{
			Log.d("FileIO", "Opening external APK: " + packageName);
		
			Context context = TERRAActivity.instance.createPackageContext(packageName, Context.CONTEXT_IGNORE_SECURITY);
			manager = context.getAssets();
		}
		catch (Exception e)
		{
			Log.d("FileIO", "Error: " + e.getMessage());
			return null;
		}
		
		if (manager == null)
		{
			Log.d("FileIO", "Could not obtain a package manager...");
			return null;
		}
		
		Log.d("FileIO", "Got external package manager sucessfully!");
		packageMap.put(packageName, manager);
		return manager;		
	}
	
	public static void listFiles(String path, String filter, boolean wantsDirectory)
	{
		File dir = new File(path);  
		Log.d("FileIO", "Searching for files in path: " + path);
		
		String regex = filter.replace(".", "\\.*").replace("*", ".*");
		Log.d("FileIO", "Filter is " + regex);
		
		int count = 0;
		
		File files[] = dir.listFiles();			
			
		if (files != null)
		{
			Log.d("FileIO", "Trying listing from directory..."+files.length+" files");
		
			synchronized (TERRAActivity.instance)
			{						
				for (int i=0; i < files.length; i++)
				{
					boolean isDir = files[i].isDirectory();
					if (isDir == wantsDirectory)
					{
						String fileName = files[i].getName();

						Log.d("FileIO", "Testing " + fileName);
						if (fileName.matches(regex))
						{				
							int size = 0;
							count++;
							
							if (!isDir) 
								size = (int) files[i].length();
								
							Log.d("FileIO", "Listing " + fileName + " with size "+ size);						
							TERRALibrary.ApplicationListFile(fileName, size);
						}
					}
				}	
			}
			
			Log.d("FileIO", "Found "+ count+" files!");				
		}

		if (count<=0)
		{
			AssetManager assets = getAssetManager("");
			
			String filenames[] = null;
			
			try
			{
				filenames = assets.list(path);			
			}
			catch (IOException 	e)
			{
				filenames = null;
			}

			if (filenames != null)
			{
				Log.d("FileIO", "Trying listing from assets..."+filenames.length+" files");
				synchronized (TERRAActivity.instance)
				{								
					for (int i=0; i < filenames.length; i++)
					{
						String fileName = filenames[i];
						
						Log.d("FileIO", "Testing " + fileName);
						if (fileName.matches(regex))
						{							
							Log.d("FileIO", "Listing " + fileName);
							TERRALibrary.ApplicationListFile(fileName, 0);
						
							/*InputStream iS;  
							
							Log.d("FileIO", "Opening " + fileName);

							try	
							{
								iS = assets.open(filenames[i]);  
								
								boolean isOpen = iS!=null;
								Log.d("FileIO", "Open result " + isOpen);
								if (isOpen)
								{
									Log.d("FileIO", "Testing size...");
									int size = iS.available();
																
									Log.d("FileIO", "Listing " + fileName + " with size "+ size);
									TERRALibrary.ApplicationListFile(fileName, size);
									iS.close();
								}
								
							}
							catch (IOException 	e)
							{
								Log.d("FileIO", "error with "+fileName);
							}*/
						}
					}
				}				
			}
		}
		
		if (count<=0)
		{
			Log.d("FileIO", "No files found!");
		}
	}
	
	public static boolean fileExists(String fileName)
	{
		
		try
		{		
			AssetManager manager = getAssetManager(fileName);
			if (manager == null)
				return false;
			
			int i = fileName.indexOf(header);
			if (i>=0)
			{
				int j = fileName.indexOf("/");
				fileName = fileName.substring(j+1);
			}
	
			Log.d("FileIO", "JAVA:Testing " +fileName);
			InputStream iS;  
            iS = manager.open(fileName);  
			if (iS!=null)
			{
				Log.d("FileIO", fileName + " was found!");
				iS.close(); 
				return true;
			}
			
			Log.d("FileIO", fileName + " was not found!");
			return false;
		}
		catch (IOException 	e)
		{
			Log.d("FileIO", fileName + " was not found at this location!");
			return false;
		}
	}
    
	public TERRAFileIO(String name)
	{	
		this.myActivity = TERRAActivity.instance;
		this.fileName = name;

		try
		{		
			InputStream iS;  
  
			AssetManager manager = getAssetManager(this.fileName);
			if (manager!=null)
			{

				int i = fileName.indexOf(header);
				if (i>=0)
				{
					int j = fileName.indexOf("/");
					fileName = fileName.substring(j+1);
				}
				
				//get the file as a stream  
				Log.d("FileIO", "JAVA:Opening " +fileName);
				iS = manager.open(fileName);  
	  
				int bufsize = 1024;                 					
                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                buffer = new byte[bufsize];
                int len = 0;
                int offset = 0;
                size = 0;
                while ((len = iS.read(buffer)) != -1)
                {
                    bos.write(buffer, 0, len);
                    offset += len;
                }
                size = offset;
                buffer = bos.toByteArray();	
                                
                        
				/*int readBytes = 0;
				int offset = 0;

				while((readBytes = iS.read(buffer, offset, buffer.length - offset)) != -1)
				{
						if(offset - buffer.length == 0)
						{
								Log.d("FileIO", "Internal " + buffer.length +" byte Buffer is not enough for resource '" + fileName +"'. Consider increasing buffer size.");
								assert(false);
						}
						
						offset += readBytes;
				}
				size = offset;*/
				
				iS.close();
			} 
			else
			{
				size = -1;
			}			
		}
		catch (IOException 	e)
		{
			size = -1;
		}
      }
	
	public int getSize()
	{	
		return size;
	}
	
	public byte[] getContents()
	{
		return buffer;
	}
	
}
