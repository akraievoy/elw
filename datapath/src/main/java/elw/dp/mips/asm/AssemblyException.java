package elw.dp.mips.asm;

public class AssemblyException extends Exception {
	private AssemblyException(String message) {
		super(message);
	}

	private AssemblyException(String message, Throwable cause) {
		super(message, cause);
	}

	public static AssemblyException formatUnsupported(String codeLine, final String syntax) {
		return new AssemblyException("Format not supported: '" + codeLine + "', correct syntax: '" + syntax + "'");
	}

	public static AssemblyException opNameUnsupported(final String opName, final String codeLine) {
		return new AssemblyException("Operation '" + opName + "' not supported: '" + codeLine + "'");
	}

	public static void ifExceeds(int lower, int actual, int upper, final String var) throws AssemblyException {
		if (lower > actual) {
			throw new AssemblyException(var + " = " + actual + " < " + lower);
		}
		if (upper < actual) {
			throw new AssemblyException(var + " = " + actual + " > " + upper);
		}
	}

	public static AssemblyException internalError(final String message) {
		return new AssemblyException("Internal error: " + message);
	}

	public static AssemblyException missingLabel(final String codeLine, final String labelGroup) {
		return new AssemblyException("Undefined label '" + labelGroup + "' in '" + codeLine + "'");
	}

	public static AssemblyException ambiguousLabel(final String label) {
		return new AssemblyException("Ambiguous label: '" + label + "'");
	}

	public static AssemblyException incorrectLabelList(final String labelList) {
		return new AssemblyException("Label list '" + labelList + "' syntax incorrect, expexted 'label1, label2'");
	}
}
