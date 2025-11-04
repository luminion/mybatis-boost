package io.github.luminion.mybatis.exception;

/**
 * 参数映射异常.
 * <p>
 * 当 SQL 条件映射过程中出现错误时抛出, 例如参数格式不正确或无法映射到数据库字段时.
 *
 * @author luminion
 * @since 1.0.0
 */
public class ParamMappingException extends RuntimeException {

    /**
     * 构造一个新的参数映射异常.
     *
     * @param message 异常消息模板
     * @param args    消息参数
     * @since 1.0.0
     */
    public ParamMappingException(String message, Object... args) {
        super(String.format(message, args));
    }

}