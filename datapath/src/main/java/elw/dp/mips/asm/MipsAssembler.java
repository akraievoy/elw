/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.asm;

import base.pattern.Result;
import elw.dp.mips.Instruction;
import org.akraievoy.gear.G4Str;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Pattern;

public class MipsAssembler {
	private static final Logger log = LoggerFactory.getLogger(MipsAssembler.class);

	protected static final String RE_LABEL = InstructionAssembler.RE_LABEL;
	protected static final Pattern PATTERN_LABELS = Pattern.compile("\\s*" + RE_LABEL + "(\\s*,\\s*" + RE_LABEL + ")*\\s*");

	protected final AssemblerRegistry assemblerRegistry = AssemblerRegistry.create();

	public Instruction[] assembleLoad(String[] codeLines, Result[] resRef) {
		final List<Instruction> instructions = new ArrayList<Instruction>();

		String assemblyCode = null;
		try {
			final Map<String, Integer> labelOffsets = new HashMap<String, Integer>();

			int instructionOffset = 0;
			for (String codeLine : codeLines) {
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
					final String message = "Instruction assembly incomplete: " + instruction;
					Result.failure(resRef, message);
					log.warn(message);
					return null;
				}
			}

			final String message = "Instructions Successfully Assembled and Loaded into Memory!";

			Result.success(resRef, message);
			log.info(message);
			return instructions.toArray(new Instruction[instructions.size()]);
		} catch (Throwable t) {

			final String message = t.getClass().getSimpleName() + " '" + t.getMessage() + "' occured in '" + assemblyCode + "', assembly terminated";
			Result.failure(resRef, message);
			log.warn(message);
			return null;
		}
	}
}

