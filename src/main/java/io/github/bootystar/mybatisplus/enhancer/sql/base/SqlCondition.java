package io.github.bootystar.mybatisplus.enhancer.sql.base;

/**
 * SQL条件
 * @author bootystar
 */
public interface SqlCondition {

    boolean isOr();

    String getField();

    String getOperator();

    Object getValue();

}
