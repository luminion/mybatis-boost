package io.github.bootystar.mybatisplus.enhancer.sql.validator;

import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlCondition;

import java.util.List;

/**
 * @author bootystar
 */
@FunctionalInterface
public interface SqlConditionValidator {

    List<SqlCondition> validate(List<SqlCondition> sqlConditions, Class<?> entityClass);

}
