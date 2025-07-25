package io.github.bootystar.mybatisplus.enhancer.sql;

import java.util.List;

/**
 * @author bootystar
 */
@FunctionalInterface
public interface SqlSortValidator {

    List<SqlSort> validate(SqlSort sqlSort, Class<?> entityClass);
    
}
