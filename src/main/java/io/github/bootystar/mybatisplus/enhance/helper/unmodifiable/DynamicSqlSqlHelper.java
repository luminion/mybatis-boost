package io.github.bootystar.mybatisplus.enhance.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.SqlConditionU;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author bootystar
 */
@Getter
public class DynamicSqlSqlHelper<T> extends UnmodifiableSqlHelper<T> {

    public DynamicSqlSqlHelper(ISqlTree sourceTree, Class<T> entityClass) {
        super(entityClass);
        initProperties(sourceTree);
    }

    @Override
    protected Collection<SqlConditionU> wrapConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<SqlConditionU> result = new ArrayList<>(conditions.size());
        for (ISqlCondition conditionO : conditions) {
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        return result;
    }

}
