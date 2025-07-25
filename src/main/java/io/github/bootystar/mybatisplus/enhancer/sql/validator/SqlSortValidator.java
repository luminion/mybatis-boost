package io.github.bootystar.mybatisplus.enhancer.sql.validator;

import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlSort;

import java.util.List;

/**
 * @author bootystar
 */
@FunctionalInterface
public interface SqlSortValidator {

    List<SqlSort> validate(SqlSort sqlSort, Class<?> entityClass);
    
}
