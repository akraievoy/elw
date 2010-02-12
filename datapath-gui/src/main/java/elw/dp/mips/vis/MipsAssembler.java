/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import elw.dp.mips.asm.Data;
import org.akraievoy.gear.G4Str;

import java.util.*;
import java.util.logging.Logger;

/**
 * TOAK general overview javadoc.
 *
 * @author Anton Kraievoy
 * @version $Id: MipsAssembler.java,v 1.1 2006/12/28 10:38:57 Anton S. Kraievoy Exp $
 */
public class MipsAssembler implements Assembler {
    private static final Logger log = Logger.getLogger(MipsAssembler.class.getName());

    protected ArrayList<String> instructions = new ArrayList<String>();
    protected ArrayList<String> codeLines = new ArrayList<String>();

    public boolean isRegister(String token) {
        return G4Str.contains(InstructionsModel.REGS, token.toUpperCase());
    }

    public boolean isConstantRegister(String token) {
        return "$zero".equals(token);
    }

    public int getRegisterNumber(String token) {
        return G4Str.indexOf(token.toUpperCase(), InstructionsModel.REGS);
    }

    public String fourFieldInstruction(String[] instructionComponents) {
        long temp = Data.hex2long(instructionComponents[0]) & 60;
        String word = Data.long2hex(temp / 4, 1);
        temp = (Data.hex2long(instructionComponents[0]) & 3) * 4;
        temp += Math.floor((Data.hex2long(instructionComponents[1]) & 24) / 8);
        word += Data.long2hex(temp, 1);
        temp = (Data.hex2long(instructionComponents[1]) & 7) * 2;
        temp += Math.floor((Data.hex2long(instructionComponents[2]) & 16) / 16);
        word += Data.long2hex(temp, 1);
        temp = Data.hex2long(instructionComponents[2]) & 15;
        word += Data.long2hex(temp, 1);
        temp = Data.hex2long(instructionComponents[3]);
        word += Data.long2hex(temp, 4);

        return word;
    }

    public String twoFieldInstruction(String[] instructionComponents) {
        long temp = Data.hex2long(instructionComponents[0]) & 60;
        String word = Data.long2hex(temp / 4, 1);
        temp = (Data.hex2long(instructionComponents[0]) & 3) * 4;
        word += Data.long2hex(temp, 1);
        temp = Data.hex2long(instructionComponents[1]);
        word += Data.long2hex(temp, 6);
        return word;
    }

    public String sixFieldInstruction(String[] instructionComponents) {
        long temp = Data.hex2long(instructionComponents[0]) & 60;
        String word = Data.long2hex(temp / 4, 1);

        temp = (Data.hex2long(instructionComponents[0]) & 3) * 4;
        temp += Math.floor((Data.hex2long(instructionComponents[1]) & 24) / 8);
        word += Data.long2hex(temp, 1);

        temp = (Data.hex2long(instructionComponents[1]) & 7) * 2;
        temp += Math.floor((Data.hex2long(instructionComponents[2]) & 16) / 16);
        word += Data.long2hex(temp, 1);

        temp = Data.hex2long(instructionComponents[2]) & 15;
        word += Data.long2hex(temp, 1);

        temp = (Data.hex2long(instructionComponents[3]) & 30) / 2;
        word += Data.long2hex(temp, 1);

        temp = (Data.hex2long(instructionComponents[3]) & 1) * 8;
        temp += Math.floor((Data.hex2long(instructionComponents[4]) & 28) / 4);
        word += Data.long2hex(temp, 1);

        temp = (Data.hex2long(instructionComponents[4]) & 3) * 4;
        temp += Math.floor((Data.hex2long(instructionComponents[5]) & 48) / 16);
        word += Data.long2hex(temp, 1);

        temp = Data.hex2long(instructionComponents[5]) & 15;
        word += Data.long2hex(temp, 1);
        return word;
    }

    public String[] immediateType(ArrayList<String> tokenList) {
        ArrayList<String> instructionComponents = new ArrayList<String>();
        String operation = tokenList.remove(0);
        if ("addi".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 0, "08");
        } else if ("andi".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 0, "0C");
        } else if ("ori".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 0, "0D");
        } else if ("xori".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 0, "0E");
        }
        String token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected in first!!");
        }
        if (isConstantRegister(token)) {
            throw new Error("$zero Register Cannot Be Changed!!");
        }
        setComponent(instructionComponents, 2, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected in second!!");
        }
        setComponent(instructionComponents, 1, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!Data.isHexPositive(token, 4)) {
            throw new Error("Immediate Value of at most 4 Hex Digits Expected!!");
        }
        setComponent(instructionComponents, 3, token.toUpperCase());
        if (tokenList.size() != 0) {
            throw new Error("Extra text follows instruction!!");
        }

        return toArray(instructionComponents);
    }

    public String[] toArray(ArrayList<String> instructionComponents) {
        return instructionComponents.toArray(new String[instructionComponents.size()]);
    }

    public String[] immediateShift(ArrayList<String> tokenList) {
        ArrayList<String> instructionComponents = new ArrayList<String>();
        String operation = tokenList.remove(0);
        if ("sll".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "00");
        } else if ("sra".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "03");
        } else if ("srl".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "02");
        }
        setComponent(instructionComponents, 0, "00");
        setComponent(instructionComponents, 1, "00");
        String token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!!");
        }
        if (isConstantRegister(token)) {
            throw new Error("$zero Register Cannot Be Changed!!");
        }
        setComponent(instructionComponents, 3, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!");
        }
        setComponent(instructionComponents, 2, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!Data.isHexPositive(token, 2)) {
            throw new Error("Immediate Value of at most 2 Hex Digits Expected!!");
        }
        if (Data.hex2long(token) > 31) {
            throw new Error("Shift amount must be in the range 0 to 31!!");
        }
        setComponent(instructionComponents, 4, token.toUpperCase());

        if (tokenList.size() != 0) {
            throw new Error("Extra text follows instruction!!");
        }

        return toArray(instructionComponents);
    }

    public String[] variableShift(ArrayList<String> tokenList) {
        ArrayList<String> instructionComponents = new ArrayList<String>();
        String operation = tokenList.remove(0);

        if ("sllv".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "04");
        } else if ("srav".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "07");
        } else if ("srlv".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "06");
        }
        setComponent(instructionComponents, 0, "00");
        setComponent(instructionComponents, 4, "00");

        String token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!!");
        }
        if (isConstantRegister(token)) {
            throw new Error("$zero Register Cannot Be Changed!!");
        }
        setComponent(instructionComponents, 3, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!");
        }
        setComponent(instructionComponents, 2, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!");
        }
        setComponent(instructionComponents, 1, Data.long2hex(getRegisterNumber(token), 2));

        if (tokenList.size() != 0) {
            throw new Error("Extra text follows instruction!!");
        }

        return toArray(instructionComponents);
    }

    public String[] dataTransfer(ArrayList<String> tokenList) {
        ArrayList<String> instructionComponents = new ArrayList<String>();
        String operation = tokenList.remove(0);
        if ("lw".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 0, "23");
        } else {
            setComponent(instructionComponents, 0, "2B");
        }
        String token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!!");
        }
        if ("lw".equalsIgnoreCase(operation) && isConstantRegister(token)) {
            throw new Error("$zero Register Cannot Be Changed!!");
        }
        setComponent(instructionComponents, 2, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!Data.isHexPositive(token, 3)) {
            throw new Error("Offset Expected!!");
        }
        setComponent(instructionComponents, 3, token.toUpperCase());

        token = tokenList.remove(0);
        if (!"(".equals(token)) {
            throw new Error("( Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!!");
        }
        setComponent(instructionComponents, 1, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!")".equals(token)) {
            throw new Error(") Expected!!");
        }
        if (tokenList.size() != 0) {
            throw new Error("Extra text follows instruction!!");
        }

        return toArray(instructionComponents);
    }

    public String[] rType(ArrayList<String> tokenList) {
        ArrayList<String> instructionComponents = new ArrayList<String>();
        String operation = tokenList.remove(0);

        if ("add".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "20");
        } else if ("and".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "24");
        } else if ("or".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "25");
        } else if ("sub".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "22");
        } else if ("slt".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "2A");
        } else if ("nor".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "27");
        } else if ("xor".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 5, "26");
        }
        setComponent(instructionComponents, 0, "00");
        setComponent(instructionComponents, 4, "00");

        String token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!!");
        }
        if (isConstantRegister(token)) {
            throw new Error("$zero Register Cannot Be Changed!!");
        }
        setComponent(instructionComponents, 3, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!");
        }
        setComponent(instructionComponents, 1, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!");
        }
        setComponent(instructionComponents, 2, Data.long2hex(getRegisterNumber(token), 2));

        if (tokenList.size() != 0) {
            throw new Error("Extra text follows instruction!!");
        }

        return toArray(instructionComponents);
    }

    public String[] branch(ArrayList<String> tokenList) {
        ArrayList<String> instructionComponents = new ArrayList<String>();
        String operation = tokenList.remove(0);

        if ("beq".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 0, "04");
        } else if ("bne".equalsIgnoreCase(operation)) {
            setComponent(instructionComponents, 0, "05");
        }

        String token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!!");
        }
        setComponent(instructionComponents, 1, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!isRegister(token)) {
            throw new Error("Register Expected!!");
        }
        setComponent(instructionComponents, 2, Data.long2hex(getRegisterNumber(token), 2));

        token = tokenList.remove(0);
        if (!",".equals(token)) {
            throw new Error("Comma Expected!!");
        }

        token = tokenList.remove(0);
        if (!Data.isHexPositive(token, 4)) {
            throw new Error("Address Offset of at Most 4 Hex Digits Expected!!");
        }
        setComponent(instructionComponents, 3, token.toUpperCase());

        if (tokenList.size() != 0) {
            throw new Error("Extra text follows instruction!!");
        }

        return toArray(instructionComponents);
    }

    public String[] jump(ArrayList<String> tokenList) {
        ArrayList<String> instructionComponents = new ArrayList<String>();
        setComponent(instructionComponents, 0, "02");

        tokenList.remove(0);
        String token = tokenList.remove(0);
        if (!Data.isHexPositive(token, 7)) {
            throw new Error("Address of at Most 7 Hex Digits Expected!!");
        }
        setComponent(instructionComponents, 1, token.toUpperCase());

        return toArray(instructionComponents);
    }

    public String parse(ArrayList<String> tokens) {
        final String c = tokens.get(0);
        final String instruction;
        if (G4Str.contains(new String[]{"ADD", "AND", "OR", "SUB", "SLT", "NOR", "XOR"}, c)) {
            instruction = sixFieldInstruction(rType(tokens));
        } else if (G4Str.contains(new String[]{"SLLV", "SRAV", "SRLV"}, c)) {
            instruction = sixFieldInstruction(variableShift(tokens));
        } else if (G4Str.contains(new String[]{"ADDI", "ANDI", "ORI", "XORI"}, c)) {
            instruction = fourFieldInstruction(immediateType(tokens));
        } else if (G4Str.contains(new String[]{"SLL", "SRA", "SRL"}, c)) {
            instruction = sixFieldInstruction(immediateShift(tokens));
        } else if (G4Str.contains(new String[]{"LW", "SW"}, c)) {
            instruction = fourFieldInstruction(dataTransfer(tokens));
        } else if (G4Str.contains(new String[]{"BEQ", "BNE"}, c)) {
            instruction = fourFieldInstruction(branch(tokens));
        } else if (G4Str.contains(new String[]{"J"}, c)) {
            instruction = twoFieldInstruction(jump(tokens));
        } else {throw new Error("A Valid Assembly Operation Expected!");}

        return instruction;
    }

    public ArrayList<String> extractTokens(String assemblyCode) {
        ArrayList<Character> instruction = new ArrayList<Character>();
        ArrayList<String> tokenList  = new ArrayList<String>();
        tokenList.clear();

        for (int i = 0; i < assemblyCode.length(); i++) {
            instruction.add(assemblyCode.charAt(i));
        }

        while (instruction.size() > 0) {
            if (Data.isLetter(instruction.get(0)) ||
                    Data.isDecDigit(instruction.get(0))) {
                String token = "";
                while (instruction.size() > 0 && (Data.isLetter(instruction.get(0)) || Data.isDecDigit(instruction.get(0)))) {
                    token += instruction.remove(0);
                }
                tokenList.add(token);
            } else if (instruction.get(0) == '$') {
                if (Data.isLetter(instruction.get(1))) {
                    String token = "";
                    token += instruction.remove(0);
                    while (instruction.size() > 0 && (Data.isLetter(instruction.get(0)) || Data.isDecDigit(instruction.get(0)))) {
                        token += instruction.remove(0);
                    }
                    tokenList.add(token);
                } else {
                    throw new Error("A Letter is Expected!");
                }
            } else {
                String token = "";
                switch (instruction.get(0)) {
                    case '(' :
                        token += instruction.remove(0);
                        tokenList.add(token);
                        break;
                    case ')' :
                        token += instruction.remove(0);
                        tokenList.add(token);
                        break;
                    case ',' :
                        token += instruction.remove(0);
                        tokenList.add(token);
                        break;
                    default :
                        throw new Error("Illegal character " + instruction.get(0));
                }
            }
            while (instruction.size() > 0 && (instruction.get(0) < 33 || instruction.get(0) > 176)) {
                instruction.remove(0);
            }
        }

        return tokenList;
    }

    public void assembleLoad(List<String> newCodeLines) {
        instructions.clear();
        codeLines.clear();
        codeLines.addAll(newCodeLines);

        String assemblyCode = null;
        try {
            for (String codeLine : newCodeLines) {
                assemblyCode = codeLine;
                final String instruction = parse(extractTokens(assemblyCode));
                instructions.add(instruction);
            }

            log.info("Instructions Successfully Assembled and Loaded into Memory!");
        } catch (Throwable t) {
            t.printStackTrace();
            log.warning(t.getClass().getName() + " '" + t.getMessage() + "' occured in '" + assemblyCode + "', assembly terminated");
        }
    }

    public String[] getCodeLines() {
        return toArray(codeLines);
    }

    public int[] getInstructions() {
        final int[] instructionsArr = new int[instructions.size()];

        for (int instructionIndex = 0; instructionIndex < instructions.size(); instructionIndex++) {
            String instruction = instructions.get(instructionIndex);
            instructionsArr[instructionIndex] = Data.hex2int(instruction);
        }

        return instructionsArr;
    }

    protected static void setComponent(final ArrayList<String> instructionComponents, final int index, final String value) {
        while (instructionComponents.size() <= index) {
            instructionComponents.add(null);
        }
        instructionComponents.set(index, value);
    }
}

