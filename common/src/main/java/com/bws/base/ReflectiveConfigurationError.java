package com.bws.base;

/**
 * Фатальная ошибка, связанная с неправильной конфигурацией рефлектывных вызовов.
 * Тут уже ничего не поделаешь, так что обрабатывать не стоит. 
 *
 * @author Anton Kraievoy
 * @version $Id: ReflectiveConfigurationError.java,v 1.1 2006/12/25 14:17:01 Anton S. Kraievoy Exp $
 */
public class ReflectiveConfigurationError extends Error {
    public ReflectiveConfigurationError() {
    }

    public ReflectiveConfigurationError(Throwable cause) {
        super(cause);
    }

    public ReflectiveConfigurationError(String message) {
        super(message);
    }

    public ReflectiveConfigurationError(String message, Throwable cause) {
        super(message, cause);
    }
}

