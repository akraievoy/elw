package ua.iasa.pathsim.domain.testing;

public class TestingCase {
    protected static final int EXECUTION_LIMIT = 10000;

    String memInput;
    String regsInput;

    String memExpected;
    String regsExpected;

    long executionLimit;
    int ordinal;

    public String getMemInput() {
        return memInput;
    }

    public void setMemInput(final String memInput) {
        this.memInput = memInput;
    }

    public String getRegsInput() {
        return regsInput;
    }

    public void setRegsInput(final String regsInput) {
        this.regsInput = regsInput;
    }

    public String getMemExpected() {
        return memExpected;
    }

    public void setMemExpected(final String memExpected) {
        this.memExpected = memExpected;
    }

    public String getRegsExpected() {
        return regsExpected;
    }

    public void setRegsExpected(final String regsExpected) {
        this.regsExpected = regsExpected;
    }

    public long getExecutionLimit() {
        return executionLimit;
    }

    public void setExecutionLimit(final long executionLimit) {
        this.executionLimit = executionLimit;
    }

    public int getOrdinal() {
        return ordinal;
    }

    public void setOrdinal(final int ordinal) {
        this.ordinal = ordinal;
    }

    public String toString() {
        return "Test " + ordinal;
    }
}
