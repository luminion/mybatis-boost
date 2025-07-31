package io.github.bootystar.mybatisplus.enhancer.query.helper.processor;

import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.helper.AbstractSqlHelper;
import io.github.bootystar.mybatisplus.enhancer.query.helper.ISqlHelper;
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * @author bootystar
 */
@Slf4j
public abstract class DefaultProcessor {

    public static ISqlCondition validate(ISqlCondition sqlCondition, Map<String, String> field2JdbcColumnMap, Map<String, Object> unmapped) {
        String field = sqlCondition.getField();
        String operator = sqlCondition.getOperator();
        Object value = sqlCondition.getValue();
        if (field == null || field.isEmpty()) {
            return null;
        }
        String jdbcColumn = field2JdbcColumnMap.get(field);
        if (jdbcColumn == null) {
            log.info("condition field [{}] not exist in fieldMap , it will be removed and put into paramMap", field);
            unmapped.putIfAbsent(field, value);
            return null;
        }
        operator = SqlKeyword.replaceOperator(operator);
        if (!SqlKeyword.isNoneArgOperator(operator) && value == null) {
            log.info("condition field [{}] requires value but value is null, it will be removed and put into paramMap", field);
            unmapped.putIfAbsent(field, "");
            return null;
        }
        if (SqlKeyword.isMultiArgOperator(operator)) {
            boolean iterableValue = value instanceof Iterable;
            if (!iterableValue) {
                log.info("condition field [{}] requires collection but value is not iterable, it will be removed and put into paramMap", field);
                unmapped.putIfAbsent(field, value);
                return null;
            }
            Iterable<?> iterable = (Iterable<?>) value;
            if (!iterable.iterator().hasNext()) {
                log.info("condition field [{}] requires collection but value is empty, it will be removed and put into paramMap", field);
                unmapped.putIfAbsent(field, value);
                return null;
            }
        }
        if (SqlKeyword.isLikeOperator(operator) && value instanceof String) {
            value = "%" + value + "%";
        }
        return new SqlCondition(jdbcColumn, operator, value);
    }

    public static void warp(AbstractSqlHelper<?, ?> sqlHelper, Collection<ISqlCondition> sqlConditions, String symbol) {
        if (sqlHelper==null || sqlConditions==null || sqlConditions.isEmpty()) {
            return;
        }
        symbol = SqlKeyword.replaceConnector(symbol);
        if (SqlKeyword.AND.keyword.equals(symbol)) {
            sqlHelper.getConditions().addAll(sqlConditions);
            return;
        }
        SqlTree iSqlTrees = new SqlTree(sqlConditions, SqlKeyword.OR.keyword);
        sqlHelper.with(iSqlTrees);
    }

    public static <T> ISqlHelper<T> process(ISqlHelper<T> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        SqlHelper<T> resultHelper = SqlHelper.of(entityClass);
        Collection<ISqlSort> resultSorts = resultHelper.getSorts();
        Map<String, Object> unmapped = resultHelper.getUnmapped();
        Map<String, String> field2JdbcColumnMap = MybatisPlusReflectUtil.field2JdbcColumnMap(entityClass);
        for (ISqlTree currentHelper : rootHelper) {
            Collection<ISqlCondition> currentHelperConditions = currentHelper.getConditions();
            Iterator<ISqlCondition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<ISqlCondition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                ISqlCondition sqlCondition = conditionIterator.next();
                ISqlCondition validate = DefaultProcessor.validate(sqlCondition, field2JdbcColumnMap, unmapped);
                if (validate == null) {
                    continue;
                }
                validatedConditions.add(validate);
            }
            DefaultProcessor.warp(resultHelper, validatedConditions, currentHelper.getSymbol());
        }
        for (ISqlSort sort : rootHelper.getSorts()) {
            String field = sort.getField();
            if (field2JdbcColumnMap.containsKey(field)) {
                resultSorts.add(sort);
            }
        }
        return resultHelper;
    }
    
}
