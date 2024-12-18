package io.github.bootystar.mybatisplus.enhance.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.ConditionU;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;

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
    protected Collection<ConditionU> wrapConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<ConditionU> result = new ArrayList<>(conditions.size());
        for (ISqlCondition conditionO : conditions) {
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        return result;
    }

}
