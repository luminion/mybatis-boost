package io.github.bootystar.mybatisplus.enhancer.util;

import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.exception.ParamMappingException;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.helper.ISqlHelper;
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public class SqlValidateUtil {


    public static <T> ISqlHelper<T> validate(ISqlHelper<T> sqlHelper) {
        Class<T> entityClass = sqlHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        SqlHelper<T> result = SqlHelper.of(entityClass);
        Collection<ISqlCondition> resultConditions = result.getConditions();
        Collection<ISqlSort> resultSorts = result.getSorts();
        Map<String, Object> resultMap = result.getMap();

        Map<String, String> field2JdbcColumnMap = MybatisPlusReflectUtil.field2JdbcColumnMap(entityClass);
        for (ISqlTree helper : sqlHelper) {
            Collection<ISqlCondition> helperConditions = helper.getConditions();
            Iterator<ISqlCondition> conditionIterator = helperConditions.iterator();
            LinkedHashSet<ISqlCondition> filteredConditions = new LinkedHashSet<>(helperConditions.size());
            while (conditionIterator.hasNext()) {
                ISqlCondition sqlCondition = conditionIterator.next();
                String field = sqlCondition.getField();
                String operator = sqlCondition.getOperator();
                Object value = sqlCondition.getValue();
                if (field == null || field.isEmpty()) {
                    conditionIterator.remove();
                    continue;
                }
                String jdbcColumn = field2JdbcColumnMap.get(field);
                if (jdbcColumn == null) {
                    log.info("condition field [{}] not exist in fieldMap , it will be removed and put into paramMap", field);
                    resultMap.putIfAbsent(field, value);
                    conditionIterator.remove();
                    continue;
                }
                operator = SqlKeyword.replaceOperator(operator);
                if (!SqlKeyword.isNoneArgOperator(operator) && value == null) {
                    log.info("condition field [{}] requires value but value is null, it will be removed and put into paramMap", field);
                    resultMap.putIfAbsent(field, value);
                    conditionIterator.remove();
                    continue;
                }
                if (SqlKeyword.isMultiArgOperator(operator)) {
                    boolean iterableValue = value instanceof Iterable;
                    if (!iterableValue) {
                        log.info("condition field [{}] requires collection but value is not iterable, it will be removed and put into paramMap", field);
                        resultMap.putIfAbsent(field, value);
                        conditionIterator.remove();
                        continue;
                    }
                    Iterable<?> iterable = (Iterable<?>) value;
                    if (!iterable.iterator().hasNext()) {
                        log.info("condition field [{}] requires collection but value is empty, it will be removed and put into paramMap", field);
                        resultMap.putIfAbsent(field, value);
                        conditionIterator.remove();
                        continue;
                    }
                }
                if (SqlKeyword.isLikeOperator(operator) && value instanceof String) {
                    value = "%" + value + "%";
                    conditionIterator.remove();
                }
                helperConditions.add(new SqlCondition(jdbcColumn, operator, value));
            }
            if (filteredConditions.isEmpty()) {
                continue;
            }
            String symbol = helper.getSymbol();
            if (symbol == null) {
                symbol = SqlKeyword.AND.keyword;
            }
            if (SqlKeyword.OR.keyword.equals(symbol) && filteredConditions.size() > 1) {
                SqlTree iSqlTrees = new SqlTree(filteredConditions, SqlKeyword.OR.keyword);
                result.with(iSqlTrees);
                continue;
            }
            resultConditions.addAll(filteredConditions);
        }
        for (ISqlSort sort : resultSorts) {
            String field = sort.getField();
            if (field2JdbcColumnMap.containsKey(field)) {
                resultSorts.add(sort);
            }
        }
        return result;
    }

}
