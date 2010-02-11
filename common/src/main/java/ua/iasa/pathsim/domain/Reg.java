package ua.iasa.pathsim.domain;

import com.bws.base.utils.*;

import java.util.*;

public enum Reg {
    zero, at, v0, v1,
    a0, a1, a2, a3,

    t0, t1, t2, t3,
    t4, t5, t6, t7,

    s0, s1, s2, s3,
    s4, s5, s6, s7,

    t8, t9, k0, k1,
    gp, sp, fp, ra,

    pc, hi, lo; //  those are extended, usages should be cut off by validations

    public final static List<Reg> publicRegs = Arrays.asList(zero, v0, v1, a0, a1, a2, a3, t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, s2, s3, s4, s5, s6, s7, t8, t9, sp, ra);

    public static Reg fromString(String regGroup) {
        for (Reg reg : values()) {

            if (reg.toString().equalsIgnoreCase(regGroup)) {
                return reg;
            }
        }

        final Long numericValue = Str.parseLong(regGroup, null);

        if (numericValue != null) {

            final int regIndex = numericValue.intValue();
            Die.ifFalse(0 <= regIndex && regIndex < 32, "illegal number of register: " + regIndex);

            return Reg.values()[regIndex];

        }

        throw Die.unsupported("failed to parse as register name: '" + regGroup + "'");
    }
}
