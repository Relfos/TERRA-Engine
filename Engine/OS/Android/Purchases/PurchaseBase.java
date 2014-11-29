package purchases;

import android.app.Activity;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

import java.lang.ref.WeakReference;

class PurchaseBase {

	private WeakReference<Activity> contextReference;

	public PurchaseBase(Activity context) {
		contextReference = new WeakReference<Activity>(context);
	}

	public Activity getContext() {
		return contextReference.get();
	}

	protected String getPreferencesBaseKey() {
		return contextReference.get().getPackageName() + "_preferences";
	}

	private SharedPreferences getPreferences() {
		if (contextReference.get() != null)
			return PreferenceManager.getDefaultSharedPreferences(contextReference.get());
		return null;
	}

	public void release() {
		if (contextReference != null)
			contextReference.clear();
	}

	protected boolean saveString(String key, String value) {
		SharedPreferences sp = getPreferences();
		if (sp != null) {
			SharedPreferences.Editor spe = sp.edit();
			spe.putString(key, value);
			spe.commit();
			return true;
		}
		return false;
	}

	protected String loadString(String key, String defValue) {
		SharedPreferences sp = getPreferences();
		if (sp != null)
			return sp.getString(key, defValue);
		return defValue;
	}

	protected boolean saveBoolean(String key, Boolean value) {
		SharedPreferences sp = getPreferences();
		if (sp != null) {
			SharedPreferences.Editor spe = sp.edit();
			spe.putBoolean(key, value);
			spe.commit();
			return true;
		}
		return false;
	}

	protected boolean loadBoolean(String key, boolean defValue) {
		SharedPreferences sp = getPreferences();
		if (sp != null)
			return sp.getBoolean(key, defValue);
		return defValue;
	}
}

