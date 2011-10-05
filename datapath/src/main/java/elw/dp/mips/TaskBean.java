package elw.dp.mips;

import java.util.ArrayList;
import java.util.List;

public class TaskBean {
    private String statement;
    private java.util.List<String> tests = new ArrayList<String>();
    private String solution;

    public TaskBean(String statement, List<String> tests, String solution) {
        this.solution = solution;
        this.statement = statement;
        this.tests = tests;
    }

    public String getSolution() {
        return solution;
    }

    public String getStatement() {
        return statement;
    }

    public List<String> getTests() {
        return tests;
    }

    public static boolean parseTest(String test, String[] regsText, String[] memText) {
        final String markRegs = "#REGISTERS";
        final String markMem = "#MEMORY";
        final int posRegs = test.indexOf(markRegs);
        final int posMem = test.indexOf(markMem);
        if (posRegs >= 0 && posMem > posRegs) {
            regsText[0] = test.substring(posRegs + markRegs.length(), posMem).trim();
            memText[0] = test.substring(posMem + markMem.length()).trim();
            return true;
        }

        return false;
    }
}
