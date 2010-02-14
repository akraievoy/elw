/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.asm;

import base.pattern.Result;
import elw.dp.mips.Instruction;
import elw.dp.mips.Reg;
import gnu.trove.TIntIntHashMap;
import org.akraievoy.gear.G;
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
					Result.failure(log, resRef, message);
					return null;
				}
			}

			final String message = "Instructions Successfully Assembled and Loaded into Memory!";

			Result.success(log, resRef, message);
			return instructions.toArray(new Instruction[instructions.size()]);
		} catch (Throwable t) {
			Result.failure(log, resRef,
				t.getClass().getSimpleName() + " '" + t.getMessage() + "' occured in '" + assemblyCode + "'"
			);
			return null;
		}
	}

	public TIntIntHashMap[] loadData(final String[] dataLines, Result[] resRef) {
		final TIntIntHashMap dataIn = new TIntIntHashMap();
		final TIntIntHashMap dataOut = new TIntIntHashMap();

		for (int lineNum = 0, dataLinesLength = dataLines.length; lineNum < dataLinesLength; lineNum++) {
			final String dataLine = dataLines[lineNum];
			final String line = dataLine.replaceAll("\\s+", "");

			if (line.length() == 0 || line.startsWith("#")) {
				continue;
			}

			final String prefix = "Memory(line " + lineNum + "): ";
			final String[] tokens = line.split(":");
			if (tokens.length != 3) {
				Result.failure(log, resRef, prefix + "must be in format addr:valueIn:valueOut");
				return null;
			}

			int address = 0;
			for (int t = 0; t < 3; t++) {
				final String token = tokens[t];

				if (t > 0 && token.length() == 0) {
					continue;
				}

				if (!Data.isNum(token, 32)) {
					Result.failure(log, resRef, prefix + "token#'" + t + "' must be a 32-bit number");
					return null;
				}

				final long value = Data.parse(token);

				if (t == 0) {
					if (value < 0) {
						Result.failure(log, resRef, prefix + "address must >= 0");
						return null;
					}
					if (value > Integer.MAX_VALUE) {
						final String maxHex = Integer.toString(Integer.MAX_VALUE, 16);
						Result.failure(log, resRef, prefix + "address '" + value + "' must be <= 0x" + maxHex);
						return null;
					}
					if (value % 4 > 0) {
						Result.failure(log, resRef, prefix + "address '" + value + "' must be word-aligned");
						return null;
					}

					address = (int) value;
				}

				(t == 1 ? dataIn : dataOut).put(address, (int) value);
			}
		}

		Result.success(log, resRef, "Data validated and loaded fine");
		return new TIntIntHashMap[]{dataIn, dataOut};
	}

	public TIntIntHashMap[] loadRegs(final String[] regsLines, Result[] resRef) {
		final TIntIntHashMap regsIn = new TIntIntHashMap();
		final TIntIntHashMap regsOut = new TIntIntHashMap();

		for (int lineNum = 0, regsLinesLength = regsLines.length; lineNum < regsLinesLength; lineNum++) {
			final String dataLine = regsLines[lineNum];
			final String line = dataLine.replaceAll("\\s+", "");

			if (line.length() == 0 || line.startsWith("#")) {
				continue;
			}

			final String prefix = "Registers(line " + lineNum + "): ";
			final String[] tokens = line.split(":");
			if (tokens.length != 3) {
				Result.failure(log, resRef, prefix + "must be in format register:valueIn:valueOut");
				return null;
			}

			final String regToken = tokens[0];
			final Reg reg;
			if (regToken.startsWith("$")) {
				final String regName = regToken.substring(1).toLowerCase();
				final Reg regByName = Reg.getByName().get(regName);
				if (regByName == null) {
					Result.failure(log, resRef, prefix + "refers to unknown register '"+regName+"'");
					return null;
				}
				reg = regByName;
			} else if (Data.isNum(regToken, 5)) {
				final long regNumLong = Data.parse(regToken);

				if (regNumLong < 0) {
					Result.failure(log, resRef, prefix + "refers to register with negative number '"+regNumLong+"'");
					return null;
				}
				if (regNumLong > Reg.values().length) {
					Result.failure(log, resRef, prefix + "refers to register with illegal number '"+regNumLong+"'");
					return null;
				}

				reg = Reg.values()[(int) regNumLong];
			} else {
				Result.failure(log, resRef, prefix + "register token must be either $name or valid number");
				return null;
			}

			if (!G.contains(Reg.publicRegs, reg) || G.contains(Reg.roRegs, reg)) {
				Result.failure(log, resRef, prefix + "register $"+reg.toString()+" is reserved/read-only");
				return null;
			}
			if (G.contains(Reg.autoRegs, reg)) {
				Result.failure(log, resRef, prefix + "register $"+reg.toString()+" is set/verified automatically");
				return null;
			}
			if (G.contains(Reg.tempRegs, reg)) {
				Result.failure(log, resRef, prefix + "register $"+reg.toString()+" is temporary");
				return null;
			}

			if (tokens[1].length() > 0) {
				if (!Data.isNum(tokens[1], 32)) {
					Result.failure(log, resRef, prefix + "input value must be a 32-bit number");
					return null;
				}
				regsIn.put(reg.ordinal(), (int) Data.parse(tokens[1]));
			}
			if (tokens[2].length() > 0) {
				if (!Data.isNum(tokens[2], 32)) {
					Result.failure(log, resRef, prefix + "output value must be a 32-bit number");
					return null;
				}
				regsOut.put(reg.ordinal(), (int) Data.parse(tokens[2]));
			}
		}

		Result.success(resRef, "Registers validated and loaded fine");
		return new TIntIntHashMap[] {regsIn, regsOut};
	}
}

