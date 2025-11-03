package io.github.luminion.mybatis.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.*;

import static io.github.luminion.mybatis.util.BoostUtils.*;

/**
 * SQL操作符枚举
 * <p>
 * 定义了常用的SQL操作符及其分类，用于SQL条件构建和验证
 *
 * @author luminion
 */
@Getter
@AllArgsConstructor
public enum SqlKeyword {

    /**
     * AND连接符
     */
    AND("AND"),
    /**
     * OR连接符
     */
    OR("OR"),

    /**
     * 等于操作符
     */
    EQ("="),
    /**
     * 不等于操作符
     */
    NE("<>"),
    /**
     * 小于操作符
     */
    LT("<"),
    /**
     * 小于等于操作符
     */
    LE("<="),
    /**
     * 大于操作符
     */
    GT(">"),
    /**
     * 大于等于操作符
     */
    GE(">="),
    /**
     * 模糊匹配操作符
     */
    LIKE("LIKE"),
    /**
     * 不模糊匹配操作符
     */
    NOT_LIKE("NOT LIKE"),

    /**
     * IS NULL操作符
     */
    IS_NULL("IS NULL"),
    /**
     * IS NOT NULL操作符
     */
    IS_NOT_NULL("IS NOT NULL"),

    /**
     * IN操作符
     */
    IN("IN"),
    /**
     * NOT IN操作符
     */
    NOT_IN("NOT IN"),

    /**
     * 位运算包含操作符
     */
    BIT_CONTAINS("&>"),
    /**
     * 位运算不包含操作符
     */
    BIT_NOT_CONTAINS("&="),

//    NOT("NOT"),
//    EXISTS("EXISTS"),
//    NOT_EXISTS("NOT EXISTS"),
//    BETWEEN("BETWEEN"),
//    NOT_BETWEEN("NOT BETWEEN"),
    ;
    /**
     * 操作符关键字
     */
    private final String keyword;



    /**
     * 替换连接符
     *
     * @param connector 连接符
     * @return {@link String} 标准化后的连接符
     * @throws IllegalArgumentException 当连接符非法时抛出
     */
    public static String replaceConnector(String connector) {
        if (connector == null || connector.isEmpty()) {
            return SqlKeyword.AND.getKeyword();
        }
        connector = connector.toUpperCase();
        if (SqlKeyword.AND.getKeyword().equals(connector) || SqlKeyword.OR.getKeyword().equals(connector)) {
            return connector;
        }
        throw new IllegalArgumentException("illegal operator: " + connector);
    }

    /**
     * 替换操作符
     *
     * @param operator 操作符
     * @return {@link String} 标准化后的操作符
     * @throws IllegalArgumentException 当操作符非法时抛出
     */
    public static String replaceOperator(String operator) {
        if (operator == null || operator.isEmpty()) {
            return SqlKeyword.EQ.getKeyword();
        }
        operator = operator.toUpperCase();
        switch (operator) {
            case "=":
            case "==":
            case "EQ":
                return SqlKeyword.EQ.getKeyword();
            case "<>":
            case "!=":
            case "NE":
                return SqlKeyword.NE.getKeyword();
            case "<":
            case "LT":
                return SqlKeyword.LT.getKeyword();
            case "<=":
            case "LE":
                return SqlKeyword.LE.getKeyword();
            case ">":
            case "GT":
                return SqlKeyword.GT.getKeyword();
            case ">=":
            case "GE":
                return SqlKeyword.GE.getKeyword();
            case "LIKE":
                return SqlKeyword.LIKE.getKeyword();
            case "NOT LIKE":
                return SqlKeyword.NOT_LIKE.getKeyword();
            case "IN":
                return SqlKeyword.IN.getKeyword();
            case "NOT IN":
                return SqlKeyword.NOT_IN.getKeyword();
            case "NULL":
            case "ISNULL":
            case "IS NULL":
                return SqlKeyword.IS_NULL.getKeyword();
            case "NOT NULL":
            case "IS NOT NULL":
                return SqlKeyword.IS_NOT_NULL.getKeyword();
            case "&>0":
            case "&>":
            case "BIT CONTAINS":
                return SqlKeyword.BIT_CONTAINS.getKeyword();
            case "&=0":
            case "&=":
            case "BIT NOT CONTAINS":
                return SqlKeyword.BIT_NOT_CONTAINS.getKeyword();
            default:
                throw new IllegalArgumentException("illegal operator: " + operator);
        }
    }


    /**
     * 判断是否为比较相等的操作符
     *
     * @param operator 操作符
     * @return boolean 是否为单参数操作符
     */
    public static boolean isEqOperator(String operator) {
        return SqlKeyword.EQ.getKeyword().equals(operator) || SqlKeyword.NE.getKeyword().equals(operator);
    }

    /**
     * 判断是否为比较大小的操作符(不包含等于和不等于)
     *
     * @param operator 操作符
     * @return boolean 是否为比较操作符
     */
    public static boolean isCompareOperator(String operator) {
        return SqlKeyword.LT.getKeyword().equals(operator) || SqlKeyword.LE.getKeyword().equals(operator) ||
                SqlKeyword.GT.getKeyword().equals(operator) || SqlKeyword.GE.getKeyword().equals(operator);
    }
    
    /**
     * 判断是否为模糊查询操作符
     *
     * @param operator 操作符
     * @return boolean 是否为LIKE操作符
     */
    public static boolean isLikeOperator(String operator) {
        return SqlKeyword.LIKE.getKeyword().equals(operator) || SqlKeyword.NOT_LIKE.getKeyword().equals(operator);
    }

    /**
     * 判断是否为in批量操作符
     *
     * @param operator 操作符
     * @return boolean 是否为多参数操作符
     */
    public static boolean isInOperator(String operator) {
        return SqlKeyword.IN.getKeyword().equals(operator) || SqlKeyword.NOT_IN.getKeyword().equals(operator);
    }

    /**
     * 判断是否为null相关操作符
     *
     * @param operator 操作符
     * @return boolean 是否为无参数操作符
     */
    public static boolean isNullOperator(String operator) {
        return SqlKeyword.IS_NULL.getKeyword().equals(operator) || SqlKeyword.IS_NOT_NULL.getKeyword().equals(operator);
    }

    /**
     * 判断是否为位操作符
     *
     * @param operator 操作符
     * @return boolean 是否为BIT操作符
     */
    public static boolean isBitOperator(String operator) {
        return SqlKeyword.BIT_CONTAINS.getKeyword().equals(operator) || SqlKeyword.BIT_NOT_CONTAINS.getKeyword().equals(operator);
    }

}