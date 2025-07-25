package io.github.bootystar.mybatisplus.enhancer.sql.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlConditionTree;
import io.github.bootystar.mybatisplus.enhancer.sql.unmodifiable.SqlConditionU;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author bootystar
 */
@Getter
@Deprecated
public class DynamicSqlSqlHelper<T> extends UnmodifiableSqlHelper<T> {

    public DynamicSqlSqlHelper(SqlConditionTree sourceTree, Class<T> entityClass) {
        super(entityClass);
        initProperties(sourceTree);
    }

    @Override
    protected Collection<SqlConditionU> wrapConditions(Collection<? extends SqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<SqlConditionU> result = new ArrayList<>(conditions.size());
        for (SqlCondition conditionO : conditions) {
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        return result;
    }

}
