package io.github.bootystar.mybatisplus.enhance.enums;

import lombok.AllArgsConstructor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * SQL操作符
 *
 * @author bootystar
 */
@AllArgsConstructor
public enum SqlKeyword {

    AND("AND"),
    OR("OR"),

    EQ("="),
    NE("<>"),
    NE2("!="),
    GT(">"),
    GE(">="),
    LT("<"),
    LE("<="),
    LIKE("LIKE"),
    NOT_LIKE("NOT LIKE"),

    IS_NULL("IS NULL"),
    IS_NOT_NULL("IS NOT NULL"),

    IN("IN"),
    NOT_IN("NOT IN"),

//    NOT("NOT"),
//    EXISTS("EXISTS"),
//    NOT_EXISTS("NOT EXISTS"),
//    BETWEEN("BETWEEN"),
//    NOT_BETWEEN("NOT BETWEEN"),
    ;
    public final String keyword;

    public static final List<String> CONDITION_CONNECTORS;
    public static final List<String> CONDITION_OPERATORS_NONE;
    public static final List<String> CONDITION_OPERATORS_SINGLE;
    public static final List<String> CONDITION_OPERATORS_MULTI;
    public static final List<String> CONDITION_OPERATORS_ALL;
    public static final List<String> CONDITION_OPERATORS_LIKE;

    static {
        List<String> connector = Arrays.asList(AND.keyword, OR.keyword);
        CONDITION_CONNECTORS = Collections.unmodifiableList(connector);
        List<String> none = Arrays.asList(IS_NULL.keyword, IS_NOT_NULL.keyword);
        CONDITION_OPERATORS_NONE = Collections.unmodifiableList(none);
        List<String> single = Arrays.asList(EQ.keyword, NE.keyword, NE2.keyword, GT.keyword, GE.keyword, LT.keyword, LE.keyword, LIKE.keyword, NOT_LIKE.keyword);
        CONDITION_OPERATORS_SINGLE = Collections.unmodifiableList(single);
        List<String> multi = Arrays.asList(IN.keyword, NOT_IN.keyword);
        CONDITION_OPERATORS_MULTI = Collections.unmodifiableList(multi);
        List<String> all = new ArrayList<>();
        all.addAll(none);
        all.addAll(single);
        all.addAll(multi);
        CONDITION_OPERATORS_ALL = Collections.unmodifiableList(all);
        List<String> like = Arrays.asList(LIKE.keyword, NOT_LIKE.keyword);
        CONDITION_OPERATORS_LIKE = Collections.unmodifiableList(like);
    }

    public static String replaceConnector(String connector) {
        if (connector == null || connector.isEmpty()) {
            return AND.keyword;
        }
        connector = connector.toUpperCase();
        if (CONDITION_CONNECTORS.contains(connector)) {
            return connector;
        }
        throw new IllegalArgumentException("illegal operator: " + connector);
    }

    public static String replaceOperator(String operator) {
        if (operator == null || operator.isEmpty()) {
            return EQ.keyword;
        }
        operator = operator.toUpperCase();
        if (CONDITION_OPERATORS_ALL.contains(operator)) {
            if (NE2.keyword.equals(operator)) {
                return NE.keyword;
            }
            return operator;
        }
        throw new IllegalArgumentException("illegal operator: " + operator);
    }

    public static boolean isNoneArgOperator(String operator) {
        return CONDITION_OPERATORS_NONE.contains(operator);
    }

    public static boolean isSingleArgOperator(String operator) {
        return CONDITION_OPERATORS_SINGLE.contains(operator);
    }

    public static boolean isMultiArgOperator(String operator) {
        return CONDITION_OPERATORS_MULTI.contains(operator);
    }

    public static boolean isOperator(String operator) {
        return CONDITION_OPERATORS_ALL.contains(operator);
    }

    public static boolean isLikeOperator(String operator) {
        return CONDITION_OPERATORS_LIKE.contains(operator);
    }

}
