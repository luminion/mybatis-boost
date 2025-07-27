package io.github.bootystar.mybatisplus.enhancer.exception;

/**
 * 条件映射异常
 *
 * @author bootystar
 */
public class ParamMappingException extends RuntimeException {

    public ParamMappingException(String message, Object... args) {
        super(String.format(message, args));
    }

}
