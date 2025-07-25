package io.github.bootystar.mybatisplus.enhancer.sql;

import java.util.List;

/**
 * @author bootystar
 */
@FunctionalInterface
public interface SqlConditionValidator {

    List<SqlCondition> validate(List<SqlCondition> sqlConditions, Class<?> entityClass);

}
