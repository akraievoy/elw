package elw.dp.app;

import elw.dp.mips.TaskBean;
import org.akraievoy.gear.G4Io;
import org.codehaus.jackson.map.ObjectMapper;

import javax.net.ssl.HttpsURLConnection;
import javax.swing.*;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class RunnableLoadTask implements Runnable {
	private final CallbackLoadTask callback;
	private final ControllerSetup setup;
	private final ObjectMapper mapper = new ObjectMapper();

	public RunnableLoadTask(CallbackLoadTask callback, ControllerSetup setup) {
		this.callback = callback;
		this.setup = setup;
	}

	private String loadFile(final String slot, final String scope, final String id) {
		callback.updateStatus(
				"downloading: '" + slot+ "' '" + id + "'@'"+scope+"'" , null
		);

		final String strDl =
				setup.getBaseUrl() + "dl/mipsApplet.txt?elw_ctx=" + setup.getElwCtx() +
						"&sId=" + slot + "&s=" + scope + "&fId=" + id;
		final URL urlDl;
		try {
			urlDl = new URL(strDl);
		} catch (MalformedURLException e) {
			callback.updateStatus("download failed: '" + strDl+ "'", e);
			return null;
		}

		InputStream is = null;
		try {
			URLConnection ucDl = setupGet(urlDl);
			is = ucDl.getInputStream();

			return G4Io.dumpToString(is, "UTF-8");
		} catch (IOException e) {
			callback.updateStatus("download failed: '" + strDl+ "'", e);
			return null;
		} finally {
			if (is != null) {
				try {
					is.close();
				} catch (IOException e) {
					//	ignored
				}
			}
		}
	}

	private URLConnection setupGet(URL url) throws IOException {
		URLConnection uc = url.openConnection();
		uc.setRequestProperty("Cookie", setup.getUploadHeader());
		uc.setAllowUserInteraction(false);
		uc.setUseCaches(false);

		if (uc instanceof HttpURLConnection) {
			((HttpURLConnection) uc).setInstanceFollowRedirects(false);
		}
		if (uc instanceof HttpsURLConnection) {
			((HttpsURLConnection) uc).setInstanceFollowRedirects(false);
		}

		return uc;
	}

	public void run() {
		callback.updateStatus(
				"listing files from '" + setup.getBaseUrl()+ "'" , null
		);

		final String strList = setup.getBaseUrl() + "list?elw_ctx=" + setup.getElwCtx();
		final URL urlList;
		try {
			urlList = new URL(strList);
		} catch (MalformedURLException e) {
			callback.updateStatus("list failed: '" + strList+ "'", e);
			return;
		}

		final ResponseList respList;
		InputStream is = null;
		try {
			URLConnection ucList = setupGet(urlList);
			is = ucList.getInputStream();

			respList = mapper.readValue(is, ResponseList.class);
		} catch (IOException e) {
			callback.updateStatus("list failed: '" + strList+ "'", e);
			return;
		} finally {
			if (is != null) {
				try {
					is.close();
				} catch (IOException e) {
					//	ignored
				}
			}
		}

		final Map<String, String[]> slotStatement = respList.getData().get("statement");
		if (slotStatement == null) {
			callback.updateStatus("statement file listing not found", null);
			return;
		}
		final String[] fileIdsStatement = slotStatement.get("v");
		if (fileIdsStatement == null || fileIdsStatement.length == 0) {
			callback.updateStatus("statement file listing broken", null);
			return;
		}
		final String fileIdStatement = fileIdsStatement[fileIdsStatement.length - 1];
		final String statement = loadFile("statement", "v", fileIdStatement);
		if (statement == null) {
			return;
		}

		final Map<String, String[]> slotCode = respList.getData().get("code");
		if (slotCode == null) {
			callback.updateStatus("code file listing not found", null);
			return;
		}
		final String[] fileIdsCode = slotCode.get("s");
		if (fileIdsCode == null) {
			callback.updateStatus("code file listing broken", null);
			return;
		}
		final String solution;
		if (fileIdsCode.length > 0) {
			final String fileIdCode = fileIdsCode[fileIdsCode.length - 1];
			solution = loadFile("code", "s", fileIdCode);
			if (solution == null) {
				return;
			}
		} else {
			solution = comment(statement, 40);
		}

		final Map<String, String[]> slotTest = respList.getData().get("test");
		if (slotTest == null) {
			callback.updateStatus("test file listing not found", null);
			return;
		}
		final String[] fileIdsTest = slotTest.get("v");
		if (fileIdsTest == null || fileIdsTest.length == 0) {
			callback.updateStatus("test file listing broken", null);
			return;
		}
		final List<String> tests = new ArrayList<String>();
		for (String fileIdTest : fileIdsTest) {
			final String test = loadFile("test", "v", fileIdTest);
			if (test == null) {
				return;
			}
			tests.add(test);
		}

		callback.updateStatus("load sequence complete", null);
		callback.setTask(new TaskBean(
				statement, tests, solution
		));

		SwingUtilities.invokeLater(
				new Runnable() {
					public void run() {
						callback.onTaskLoadComplete();
					}
				}
		);
	}

	private static String comment(String statement, final int lineWidth) {
		final StringBuilder st = new StringBuilder(statement.replaceAll("\\s+", " ").trim());

		int pos = 0;
		while (pos + 1 < st.length()) {
			final String header = pos > 0 ? "\n# " : "# ";
			st.insert(pos, header);
			pos += header.length();

			int lineLen = 0;
			while (lineLen < lineWidth && pos + lineLen + 1 < st.length()) {
				int nextSpace = st.indexOf(" ", pos + lineLen);
				int nextPos = nextSpace >= 0 ? nextSpace + 1 : st.length() - 1;
				if (lineLen != 0 && nextPos - pos > lineWidth) {
					break;
				}

				lineLen = nextPos - pos;
			}

			pos += lineLen;
		}

		return st.toString();
	}

/*
	public static void main(String[] args) {
		for (int lw = 1; lw < 1000; lw++) {
			System.out.println(comment(
					"   aiuhiudfh asd haiufshiudhfk sjdfhk jhskdjfhk sjdf ksdjfh  \n" +
					"  ksfh ksdjfhkjhkjd hfskjdfhks jdfhksjdfhksjd fhkjhskjdfhskj dfhskdj fhkjhsk djf " +
					"  ksfh ksdjfhkjhk jdhfskjdfhksj xdfhk sjdfhksjdfhkjhskjdfhs kjdfhskdjfhkjhskdjf " +
					"  ksfh ksdjfhkjhkjdhfskjdfhks jdfhksjdfhksjdfhkjhskjdfhskjdf hskdjfhkjhskdjf " +
					"  ksfh ksdjfhkjhkjdhfskjdfhksjdfhksjdfhksjdfhkjhskjdfhskjdfhskdjfhkjhskdjf " +
					"ksdjfh ksdfjh skdfjh ksjdfh skdjfh ",
					lw
			));
		}
	}
*/

	protected void setupGet(URLConnection ucList) {
		ucList.setRequestProperty("Cookie", setup.getUploadHeader());
		ucList.setAllowUserInteraction(false);
		ucList.setUseCaches(false);
		if (ucList instanceof HttpURLConnection) {
			((HttpURLConnection) ucList).setInstanceFollowRedirects(false);
		}
		if (ucList instanceof HttpsURLConnection) {
			((HttpsURLConnection) ucList).setInstanceFollowRedirects(false);
		}
	}

	public static class ResponseList {
		protected String message;
		protected boolean success;
		protected Map<String, Map<String, String[]>> data = new HashMap<String, Map<String, String[]>>();

		private ResponseList() {
		}

		private Map<String, Map<String, String[]>> getData() {
			return data;
		}

		public void setData(Map<String, Map<String, String[]>> data) {
			this.data = data;
		}

		public String getMessage() {
			return message;
		}

		public void setMessage(String message) {
			this.message = message;
		}

		public boolean isSuccess() {
			return success;
		}

		public void setSuccess(boolean success) {
			this.success = success;
		}
	}
}
