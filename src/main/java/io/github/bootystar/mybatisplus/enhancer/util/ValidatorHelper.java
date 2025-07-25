package io.github.bootystar.mybatisplus.enhancer.util;

import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.expception.ParamMappingException;
import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlSort;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public class ValidatorHelper {

    
    public static Collection<SqlSort> validateSorts(Collection<SqlSort> sqlSorts, Class<?> entityClass) {
        Map<String, String> field2JdbcColumnMap = MybatisPlusReflectHelper.field2JdbcColumnMap(entityClass);
        if (field2JdbcColumnMap.isEmpty()) {
            throw new ParamMappingException("entityClass %s has no field to convert, please check your configuration", entityClass.getName());
        }
        if (sqlSorts == null || sqlSorts.isEmpty()) {
            return sqlSorts;
        }
        Iterator<SqlSort> iterator = sqlSorts.iterator();
        while (iterator.hasNext()) {
            SqlSort next = iterator.next();
            String field = next.getField();
            if (field == null || field.isEmpty()) {
                log.info("sort field [{}] is null , it will be removed", field);
                iterator.remove();
                continue;
            }
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                log.info("sort field [{}] not exist in fieldMap , it will be removed", field);
                iterator.remove();
                continue;
            }
        }
        return sqlSorts;
    }
    
    public static Collection<SqlCondition> validateConditions(Collection<SqlCondition> sqlConditions, Class<?> entityClass) {
        Map<String, String> field2JdbcColumnMap = MybatisPlusReflectHelper.field2JdbcColumnMap(entityClass);
        Iterator<SqlCondition> iterator = sqlConditions.iterator();
        while (iterator.hasNext()){
            SqlCondition sqlCondition = iterator.next();
            String field = sqlCondition.getField();
            String operator = sqlCondition.getOperator();
            Object value = sqlCondition.getValue();
            if (field == null || field.isEmpty()) {
                log.warn("field is null or empty, condition will be removed: {}", sqlCondition);
                iterator.remove();
            }
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                log.info("condition field [{}] not exist in fieldMap , it will be removed and put into paramMap", field);
                map.putIfAbsent(field, value);
                return Optional.empty();
            }
            operator = SqlKeyword.replaceOperator(operator);
            if (!SqlKeyword.isNoneArgOperator(operator) && value == null) {
                log.info("condition field [{}] requires value but value is null, it will be removed and put into paramMap", field);
                map.putIfAbsent(field, "null");
                return Optional.empty();
            }
            if (SqlKeyword.isMultiArgOperator(operator)) {
                if (value instanceof Iterable) {
                    Iterable<?> iterable = (Iterable<?>) value;
                    if (!iterable.iterator().hasNext()) {
                        log.info("condition field [{}] requires collection but value is empty, it will be removed and put into paramMap", field);
                        map.putIfAbsent(field, value);
                        return Optional.empty();
                    }
                    // 使用新集合储存
                    ArrayList<Object> newContainer = new ArrayList<>();
                    for (Object o : iterable) {
                        newContainer.add(o);
                    }
                    value = newContainer;
                } else {
                    log.info("condition field [{}] requires collection but value is not iterable, it will be removed and put into paramMap", field);
                    map.putIfAbsent(field, value);
                    return Optional.empty();
                }
            }
            if (SqlKeyword.isLikeOperator(operator) && value instanceof String) {
                value = "%" + value + "%";
            }
            return Optional.of(new SqlConditionU(isOr, jdbcColumn, operator, value));

        }
        return null;
    }

    public static Collection<SqlCondition> validateConditionsWithSuffixFix(Collection<SqlCondition> sqlConditions, Class<?> entityClass, Map<String, String> field2JdbcColumnMap) {

        return null;
    }

    
}
