package purchases;

import android.app.Activity;
import android.text.TextUtils;
import android.util.Log;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Pattern;

class PurchaseCache extends PurchaseBase {
	private static final String ENTRY_DELIMITER = "#####";
	private static final String LINE_DELIMITER = ">>>>>";
	private static final String VERSION_KEY = ".version";

	private HashMap<String, PurchaseInfo> data;
	private String cacheKey;
	private String version;

	public PurchaseCache(Activity context, String key) {
		super(context);
		data = new HashMap<String, PurchaseInfo>();
		cacheKey = key;
		load();
	}

	private String getPreferencesCacheKey() {
		return getPreferencesBaseKey() + cacheKey;
	}

	private String getPreferencesVersionKey() {
		return getPreferencesCacheKey() + VERSION_KEY;
	}

	private void load() {
		for (String entry : loadString(getPreferencesCacheKey(), "").split(Pattern.quote(ENTRY_DELIMITER))) {
			if (!TextUtils.isEmpty(entry)) {
				String[] parts = entry.split(Pattern.quote(LINE_DELIMITER));
				if (parts.length > 2) {
					data.put(parts[0], new PurchaseInfo(parts[1], parts[2]));
				} else if (parts.length > 1) {
					data.put(parts[0], new PurchaseInfo(parts[1], null));
				}
			}
		}
		version = getCurrentVersion();
	}

	private void flush() {
		ArrayList<String> output = new ArrayList<String>();
		for (String productId : data.keySet()) {
			PurchaseInfo info = data.get(productId);
			output.add(productId + LINE_DELIMITER + info.responseData + LINE_DELIMITER + info.signature);
		}
		saveString(getPreferencesCacheKey(), TextUtils.join(ENTRY_DELIMITER, output));
		version = Long.toString(new Date().getTime());
		saveString(getPreferencesVersionKey(), version);
	}

	public boolean includesProduct(String productId) {
		reloadDataIfNeeded();
		return data.containsKey(productId);
	}

	public PurchaseInfo getDetails(String productId) {
		reloadDataIfNeeded();
		return data.containsKey(productId) ? data.get(productId) : null;
	}

	public void put(String productId, String details, String signature) {
		reloadDataIfNeeded();
		if (!data.containsKey(productId)) {
			data.put(productId, new PurchaseInfo(details, signature));
			flush();
		}
	}

	public void remove(String productId) {
		reloadDataIfNeeded();
		if (data.containsKey(productId)) {
			data.remove(productId);
			flush();
		}
	}

	public void clear() {
		reloadDataIfNeeded();
		data.clear();
		flush();
	}

	private String getCurrentVersion() {
		return loadString(getPreferencesVersionKey(), "0");
	}

	private void reloadDataIfNeeded() {
		if (!version.equalsIgnoreCase(getCurrentVersion())) {
			data.clear();
			load();
		}
	}

	public List<String> getContents() {
		return new ArrayList<String>(data.keySet());
	}

	@Override
	public String toString() {
		return TextUtils.join(", ", data.keySet());
	}
}
