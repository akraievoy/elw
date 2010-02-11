package ua.iasa.pathsim.domain.testing;

import java.util.*;

public class Task {
    String shortDesc;
    String description;

    String sampleSolution;

    List<TestingCase> cases= new ArrayList<TestingCase>();

    public String getDescription() {
        return description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    public String getSampleSolution() {
        return sampleSolution;
    }

    public void setSampleSolution(final String sampleSolution) {
        this.sampleSolution = sampleSolution;
    }

    public List<TestingCase> getCases() {
        return cases;
    }

    public void setCases(final List<TestingCase> cases) {
        this.cases = cases;
    }

    public String getShortDesc() {
        return shortDesc;
    }

    public void setShortDesc(final String shortDesc) {
        this.shortDesc = shortDesc;
    }

    public String getFullDefinition() {
        StringBuffer result = new StringBuffer();

        result.append("<html><head></head><body><a name='top' ></a>");

        header(result, getShortDesc());
        lineBreak(result);

        result.append(getDescription());
        lineBreak(result);
        lineBreak(result);

        final List<TestingCase> list = getCases();
        int index= 1;
        for (TestingCase tCase : list) {
            result.append("<table><tr><th>Input ").append(index).append("</th><th>Expected ").append(index).append("</th></tr>");

            result.append("<tr><td valign='top'>");
            renderTCase(result, tCase.getMemInput(), tCase.getRegsInput());
            result.append("</td><td valign='top'>");
            renderTCase(result, tCase.getMemExpected(), tCase.getRegsExpected());
            result.append("</td></tr></table>");
        }

        result.append("</body></html>");

        return result.toString();
    }

    static void renderTCase(final StringBuffer result, final String memInput, final String regsInput) {
        final String[] memInputStrings = memInput.split("\n");
        final String[] regsInputStrings = regsInput.split("\n");

        result.append("<table>");

        result.append("<tr><th>Memory</th><th>Registers</th></tr>");

        for (int lineIndex = 0; lineIndex < Math.max(memInputStrings.length, regsInputStrings.length); lineIndex++) {

            final String mem = lineIndex < memInputStrings.length ? memInputStrings[lineIndex] : "";
            final String reg = lineIndex < regsInputStrings.length ? regsInputStrings[lineIndex] : "";

            result.append("<tr><td><code>").append(mem).append("</code></td><td><code>").append(reg).append("</code></td></tr>");
        }

        result.append("</table>");
    }

    static void lineBreak(final StringBuffer result) {
        result.append("<br>");
    }

    static void header(final StringBuffer result, final String headerText) {
        result.append("<b>").append(headerText).append("</b></br>");
        lineBreak(result);
    }
}
