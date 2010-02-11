package ua.iasa.pathsim.domain.asm;

import com.bws.base.utils.*;
import java.io.*;
import ua.iasa.pathsim.domain.Instruction;

import java.util.*;

public class AssemblerRegistry {
    protected static String preprocess(final String codeLine) {
        final String trimmed = codeLine.trim();
        final String whiteSpaceNormalized = trimmed.replaceAll("\\s+", " ");
        final String lowerCase = whiteSpaceNormalized.toLowerCase();

        return lowerCase;
    }

    public static class InstructionSetup {
        public final String name;
        public final InstructionAssembler assembler;
        public final String template;
        public final String syntax;
        public final boolean unsigned;

        public InstructionSetup(String instructionName, InstructionAssembler assembler, String template, String syntax) {
            this.name = instructionName;
            this.assembler = assembler;
            this.template = template;
            this.syntax = syntax;
            this.unsigned= name.endsWith("u");
        }
    }

    Map<String, InstructionSetup> nameToSetup = new HashMap<String, InstructionSetup>();

    private AssemblerRegistry() {
    }

    InstructionSetup resolve(String codeLine) throws AssemblyException {
        final String opName = getOpName(codeLine);
        final InstructionSetup setup = nameToSetup.get(opName);

        if (setup == null) {
            throw AssemblyException.opNameUnsupported(opName, codeLine);
        }

        return setup;
    }

    public Instruction assemble(String codeLine, int instructionOffset) throws AssemblyException {
        final String preprocessed = preprocess(codeLine);
        final InstructionSetup setup = resolve(preprocessed);

        final Instruction template = new Instruction(setup, preprocessed, instructionOffset);
        final Instruction instruction = setup.assembler.assemble(template);

        return instruction;
    }

    public static String getOpName(String codeLine) {
        //  here we have to return first word, lowercase
        return codeLine.split(" ")[0];
    }

    public static AssemblerRegistry create() {
        Properties temples= new Properties();

        final InputStream templesStream = AssemblerRegistry.class.getResourceAsStream("templates.properties");
        Die.ifNull("templeStream", templesStream);

        try {
            temples.load(templesStream);
        } catch (IOException e) {
            throw Die.criticalConfigError(e);
        }

        return create(temples);
    }

    static AssemblerRegistry create(Properties templates) {
        final AssemblerRegistry assemblerRegistry = new AssemblerRegistry();
        final Set<Object> keys = templates.keySet();

        for (Object key : keys) {
            final String property = (String) key;
            if (property.endsWith(".syntax")) {
                final String name = property.substring(0, property.indexOf('.'));
                Die.ifEmpty("name", name);

                final String syntax = templates.getProperty(property);

                final String template = templates.getProperty(name + ".template");
                Die.ifEmpty(name + ".template", template);
                Die.ifFalse(template.length() == 32, "template.length == " + template.length() + " for " + name);

                final String assemblerClass = templates.getProperty(name + ".assembler");
                Die.ifEmpty(name + ".assembler", assemblerClass);

                final InstructionAssembler assembler = Vm.tryToCreateByName(assemblerClass, InstructionAssembler.class);
                Die.ifNull(assemblerClass + ", assembler for " + name, assembler);

                final InstructionSetup setup = new InstructionSetup(name, assembler, template, syntax);

                assemblerRegistry.setupInstruction(setup);
            }
        }

        return assemblerRegistry;
    }

    void setupInstruction(final InstructionSetup setup) {
        final InstructionSetup prevContent = nameToSetup.put(setup.name, setup);
        Die.ifNotNull("prevContent (" + setup.name + ")", prevContent);
    }
}
