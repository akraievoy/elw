package elw.web;

import org.springframework.web.bind.annotation.PathVariable;

import java.beans.PropertyEditorSupport;
import java.lang.Enum;

/**
 * Simple bridging from {@link PathVariable} to {@link Enum}-based
 * method parameters.
 */
public class EnumPropertyEditor extends PropertyEditorSupport {
    private final Enum[] values;

    public EnumPropertyEditor(Enum[] values) {
        this.values = values;
    }

    @Override
    public void setAsText(String text) throws IllegalArgumentException {
        final Enum value = fromString(text, values);

        if (value != null) {
            setValue(value);
            return;
        }

        throw new IllegalArgumentException("no such ListStyle: " + text);
    }

    @Override
    public String getAsText() {
        final Enum listStyle =
                (Enum) getValue();

        return listStyle.toString();
    }
    
    public static <E extends Enum> E fromString(
            final String text, final E[] valuesArr
    ) {
        for (E value : valuesArr) {
            if (value.toString().equalsIgnoreCase(text)) {
                return value;
            }
        }

        return null;
    }
}
