package io.github.bootystar.mybatisplus.enhancer.query.helper.processor;

import io.github.bootystar.mybatisplus.enhancer.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.helper.ISqlHelper;
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public class FieldSuffixProcessor {
    private Map<String, String> suffix2OperatorMap;

    {
        suffix2OperatorMap = SqlExtraSuffix.DEFAULT_COMPLETE_MAP;
    }
    
    private final static FieldSuffixProcessor instance = new FieldSuffixProcessor();

    public static FieldSuffixProcessor of() {
        return instance;
    }
    
    public static FieldSuffixProcessor of(Map<String, String> suffix2OperatorMap) {
        FieldSuffixProcessor fieldSuffixProcessor = new FieldSuffixProcessor();
        fieldSuffixProcessor.suffix2OperatorMap = suffix2OperatorMap;
        return fieldSuffixProcessor;
    }

    public <T> ISqlHelper<T> process(ISqlHelper<T> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        SqlHelper<T> resultHelper = SqlHelper.of(entityClass);
        Collection<ISqlSort> resultSorts = resultHelper.getSorts();
        Map<String, Object> unmapped = resultHelper.getUnmapped();
        Map<String, String> field2JdbcColumnMap = MybatisPlusReflectUtil.field2JdbcColumnMap(entityClass);
        Set<String> suffixes = suffix2OperatorMap.keySet();
        for (ISqlTree currentHelper : rootHelper) {
            Collection<ISqlCondition> currentHelperConditions = currentHelper.getConditions();
            Iterator<ISqlCondition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<ISqlCondition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                ISqlCondition sqlCondition = conditionIterator.next();
                String field = sqlCondition.getField();
                String jdbcColumn = field2JdbcColumnMap.get(field);
                if (jdbcColumn == null) {
                    boolean isSuffixMatched = false;
                    for (String suffix : suffixes) {
                        if (field.endsWith(suffix)) {
                            isSuffixMatched = true;
                            String sourceFiled = field.substring(0, field.length() - suffix.length());
                            String operator = suffix2OperatorMap.get(suffix);
                            log.debug("condition field [{}] Matched suffix operator [{}]", field, operator);
                            SqlCondition suffixCondition = new SqlCondition(sourceFiled, operator, sqlCondition.getValue());
                            ISqlCondition validateSuffixCondition = DefaultProcessor.validate(suffixCondition, field2JdbcColumnMap, unmapped);
                            if (validateSuffixCondition == null) {
                                continue;
                            }
                            validatedConditions.add(validateSuffixCondition);
                            break;
                        }
                    }
                    if (isSuffixMatched){
                        continue;
                    }
                }
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
