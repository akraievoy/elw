/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.asm;

import elw.dp.mips.Instruction;
import org.akraievoy.gear.G4Str;

import java.util.*;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public class MipsAssembler {
	private static final Logger log = Logger.getLogger(MipsAssembler.class.getName());

	static final String RE_LABEL = InstructionAssembler.RE_LABEL;
	static final Pattern PATTERN_LABELS = Pattern.compile("\\s*" + RE_LABEL + "(\\s*,\\s*" + RE_LABEL + ")*\\s*");

	ArrayList<Instruction> instructions = new ArrayList<Instruction>();
	ArrayList<String> codeLines = new ArrayList<String>();

	public String[] toArray(ArrayList<String> instructionComponents) {
		return instructionComponents.toArray(new String[instructionComponents.size()]);
	}

	public void assembleLoad(List<String> newCodeLines) {
		instructions.clear();
		codeLines.clear();
		codeLines.addAll(newCodeLines);

		String assemblyCode = null;
		try {
			final AssemblerRegistry assemblerRegistry = AssemblerRegistry.create();
			final Map<String, Integer> labelOffsets = new HashMap<String, Integer>();

			int instructionOffset = 0;
			for (String codeLine : newCodeLines) {
				assemblyCode = codeLine;

				final String labelList;
				final String asmCode;
				final int labelSepIndex = codeLine.indexOf(':');

				if (labelSepIndex >= 0) {
					labelList = codeLine.substring(0, labelSepIndex);
					asmCode = codeLine.substring(labelSepIndex + 1);
				} else {
					labelList = "";
					asmCode = codeLine;
				}

				if (!G4Str.isEmpty(labelList)) {
					final boolean correctSyntax = PATTERN_LABELS.matcher(labelList).matches();
					if (!correctSyntax) {
						throw AssemblyException.incorrectLabelList(labelList);
					}

					final String[] labels = labelList.trim().toLowerCase().split("\\s*,\\s*");

					for (String label : labels) {
						if (labelOffsets.get(label) != null) {
							throw AssemblyException.ambiguousLabel(label);
						}
						labelOffsets.put(label, instructionOffset);
					}
				}

				if (!G4Str.isEmpty(asmCode)) {
					instructions.add(assemblerRegistry.assemble(asmCode, instructionOffset));
					instructionOffset++;
				}
			}

			for (Instruction instruction : instructions) {
				assemblyCode = instruction.getCodeLine();
				instruction.getSetup().assembler.assembleDeps(instruction, labelOffsets);

				if (!instruction.isAssembled()) {
					throw AssemblyException.internalError("Instruction assembly incomplete: " + instruction);
				}
			}

			log.info("Instructions Successfully Assembled and Loaded into Memory!");
		} catch (Throwable t) {
			t.printStackTrace();
			log.warning(t.getClass().getName() + " '" + t.getMessage() + "' occured in '" + assemblyCode + "', assembly terminated");
		}
	}

	public int[] getInstructions() {
		final int[] instructionsArr = new int[instructions.size()];

		for (int instructionIndex = 0; instructionIndex < instructions.size(); instructionIndex++) {
			Instruction instruction = instructions.get(instructionIndex);
			instructionsArr[instructionIndex] = instruction.intValue();
		}

		return instructionsArr;
	}

	public List<Instruction> getInstructions2() {
		return Collections.unmodifiableList(instructions);
	}

	protected static void setComponent(final ArrayList<String> instructionComponents, final int index, final String value) {
		while (instructionComponents.size() <= index) {
			instructionComponents.add(null);
		}
		instructionComponents.set(index, value);
	}
}

