package io.github.bootystar.mybatisplus.enhancer.query.entity;

import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import lombok.*;

/**
 * 条件参数
 *
 * @author bootystar
 */

@Getter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class SqlCondition implements ISqlCondition {

    protected String field;
    protected String operator;
    protected Object value;

    public SqlCondition(String field, Object value) {
        this.field = field;
        this.operator = SqlKeyword.EQ.keyword;
        this.value = value;
    }

    public static SqlCondition of(ISqlCondition sqlCondition) {
        return new SqlCondition(sqlCondition.getField(), sqlCondition.getOperator(), sqlCondition.getValue());
    }


}
