package elw.vo;

public class AssignmentSetup {
	protected int codeMaxBytes;
	protected String codeDownloadPattern;
	protected String codeUploadRegex;
	protected String codeValidatorCN;
	protected String codeExportUriPattern;
	protected String reportGenerateUriPattern;
	protected int reportMaxBytes;
	protected String reportUploadRegex;

	public int getCodeMaxBytes() {
		return codeMaxBytes;
	}

	public void setCodeMaxBytes(int codeMaxBytes) {
		this.codeMaxBytes = codeMaxBytes;
	}

	public String getCodeDownloadPattern() {
		return codeDownloadPattern;
	}

	public void setCodeDownloadPattern(String codeDownloadPattern) {
		this.codeDownloadPattern = codeDownloadPattern;
	}

	public String getCodeUploadRegex() {
		return codeUploadRegex;
	}

	public int getReportMaxBytes() {
		return reportMaxBytes;
	}

	public void setReportMaxBytes(int reportMaxBytes) {
		this.reportMaxBytes = reportMaxBytes;
	}

	public String getReportUploadRegex() {
		return reportUploadRegex;
	}

	public void setReportUploadRegex(String reportUploadRegex) {
		this.reportUploadRegex = reportUploadRegex;
	}

	public void setCodeUploadRegex(String codeUploadRegex) {
		this.codeUploadRegex = codeUploadRegex;
	}

	public String getCodeValidatorCN() {
		return codeValidatorCN;
	}

	public void setCodeValidatorCN(String codeValidatorCN) {
		this.codeValidatorCN = codeValidatorCN;
	}

	public String getCodeExportUriPattern() {
		return codeExportUriPattern;
	}

	public void setCodeExportUriPattern(String codeExportUriPattern) {
		this.codeExportUriPattern = codeExportUriPattern;
	}

	public String getReportGenerateUriPattern() {
		return reportGenerateUriPattern;
	}

	public void setReportGenerateUriPattern(String reportGenerateUriPattern) {
		this.reportGenerateUriPattern = reportGenerateUriPattern;
	}
}
