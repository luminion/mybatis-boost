package io.github.bootystar.mybatisplus.enhancer.query.core;

/**
 * SQL条件
 * @author bootystar
 */
public interface ISqlCondition {

    /**
     * 属性名
     *
     * @return {@link String }
     */
    String getField();

    /**
     * 运算符
     *
     * @return {@link String }
     */
    String getOperator();

    /**
     * 值
     *
     * @return {@link Object }
     */
    Object getValue();

}
