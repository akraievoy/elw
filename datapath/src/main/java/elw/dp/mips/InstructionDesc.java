package elw.dp.mips;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface InstructionDesc {
    String template();

    String syntax();

    String writeRegs();

    boolean unsigned() default false;
}
