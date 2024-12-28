package io.github.bootystar.mybatisplus.enhance.query.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.SqlCondition;
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
public class SqlConditionU implements SqlCondition {

    protected boolean or;

    protected String field;

    protected String operator;

    protected Object value;

    public SqlConditionU(boolean or, String field, String operator, Object value) {
        this.or = or;
        this.field = field;
        this.operator = SqlKeyword.replaceOperator(operator);
        this.value = value;
    }
}
