package com.pascal.terra;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.os.Bundle;
 
import android.app.backup.BackupAgentHelper;
import android.app.backup.FileBackupHelper;

import mp.MpUtils;
  
public class TERRACloudBackup extends BackupAgentHelper 
{
	// A key to uniquely identify the set of backup data
    static final String FILES_BACKUP_KEY = "TERRA";

    // Allocate a helper and add it to the backup agent
    @Override
    public void onCreate() 
	{
        FileBackupHelper helper = new FileBackupHelper(this, "session");
        addHelper(FILES_BACKUP_KEY, helper);
    }
}