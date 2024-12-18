package io.github.bootystar.mybatisplus.enhance.query;

/**
 * SQL条件
 * @author bootystar
 */
public interface ISqlCondition {

    /**
     * 与其他条件关系是否为or
     *
     * @return boolean
     */
    boolean isOr();

    /**
     * 属性名
     *
     * @return {@link String }
     */
    String getField();

    /**
     * 运算符(=,>,<,!=,<>,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN),默认=
     *
     * @return {@link String }
     */
    String getOperator();

    /**
     * 值(若是多个值,如in ,则value为集合)
     *
     * @return {@link Object }
     */
    Object getValue();

}
