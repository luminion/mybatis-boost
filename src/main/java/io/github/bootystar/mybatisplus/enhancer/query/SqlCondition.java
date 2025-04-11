package io.github.bootystar.mybatisplus.enhancer.query;

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
