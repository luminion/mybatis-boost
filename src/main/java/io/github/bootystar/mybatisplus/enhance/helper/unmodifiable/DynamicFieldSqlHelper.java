package io.github.bootystar.mybatisplus.enhance.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.ConditionU;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public class DynamicFieldSqlHelper<T> extends UnmodifiableSqlHelper<T> {

    private Map<String, String> suffix2OperatorMap = SqlExtraSuffix.DEFAULT_MAP;

    public DynamicFieldSqlHelper(ISqlTree tree, Class<T> entityClass, FieldSuffixBuilder suffixBuilder) {
        super(entityClass);
        if (tree == null) {
            throw new IllegalArgumentException("tree can't be null");
        }
        if (suffixBuilder != null) {
            this.suffix2OperatorMap = suffixBuilder.build();
        }
        initProperties(tree);
    }


    @Override
    protected Collection<ConditionU> wrapConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<ConditionU> result = new ArrayList<>();
        Set<String> suffixes = suffix2OperatorMap.keySet();
        for (ISqlCondition conditionO : conditions) {
            String field = conditionO.getField();
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                for (String suffix : suffixes) {
                    if (field.endsWith(suffix)) {
                        String sourceFiled = field.substring(0, field.length() - suffix.length());
                        String operator = suffix2OperatorMap.get(suffix);
                        wrap2JdbcColumnCondition(conditionO.isOr(), sourceFiled, operator, conditionO.getValue()).ifPresent(result::add);
                        break;
                    }
                }
                log.warn("condition field [{}] not exist in fieldMap , it will be removed and put into paramMap", field);
                this.map.putIfAbsent(field, conditionO.getValue());
                continue;
            }
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        return result;
    }

}
