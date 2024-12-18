package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 条件参数
 *
 * @author bootystar
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConditionG implements ISqlCondition {

    /**
     * 和上一个条件的关系是否为or(默认否, 为否时无需填写)
     */
    protected boolean or;

    /**
     * 属性名
     */
    protected String field;

    /**
     * 运算符(默认=,为=时无需填写)
     * <p>
     * (=,>,<,!=,<>,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN)
     * <p/>
     */
    protected String operator = SqlKeyword.EQ.keyword;

    /**
     * 值(若是多个值,如in ,则value为集合)
     */
    protected Object value;

    public ConditionG(String field, Object value) {
        this.field = field;
        this.value = value;
    }

    public ConditionG(String field, String operator, Object value) {
        this.field = field;
        this.operator = operator;
        this.value = value;
    }


    public static ConditionG of(ISqlCondition sqlCondition) {
        if (sqlCondition instanceof ConditionG) return (ConditionG) sqlCondition;
        return new ConditionG(sqlCondition.isOr(), sqlCondition.getField(), sqlCondition.getOperator(), sqlCondition.getValue());
    }


}
