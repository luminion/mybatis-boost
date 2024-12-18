package io.github.bootystar.mybatisplus.enhance.expception;

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
