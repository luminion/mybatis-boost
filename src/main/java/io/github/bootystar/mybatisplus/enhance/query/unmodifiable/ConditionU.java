package io.github.bootystar.mybatisplus.enhance.query.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.ToString;

/**
 * 不可变条件
 *
 * @author bootystar
 */
@Getter
@EqualsAndHashCode
@ToString
public class ConditionU implements ISqlCondition {

    /**
     * 和上一个条件的关系是否为or
     */
    protected boolean or;

    /**
     * 字段名
     */
    protected String field;

    /**
     * 运算符(=,>,<,!=,<>,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN),默认=
     */
    protected String operator;

    /**
     * 值(若是多个值,如in ,则value为集合)
     */
    protected Object value;

    public ConditionU(boolean or, String field, String operator, Object value) {
        this.or = or;
        this.field = field;
        this.operator = SqlKeyword.replaceOperator(operator);
        this.value = value;
    }
}
