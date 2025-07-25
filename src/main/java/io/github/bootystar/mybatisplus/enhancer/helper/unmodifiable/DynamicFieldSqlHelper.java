package io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhancer.builder.ExtraFieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhancer.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.enhancer.sql.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.sql.SqlTree;
import io.github.bootystar.mybatisplus.enhancer.sql.unmodifiable.SqlConditionU;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
@Deprecated
public class DynamicFieldSqlHelper<T> extends UnmodifiableSqlHelper<T> {

    private Map<String, String> suffix2OperatorMap = SqlExtraSuffix.DEFAULT_COMPLETE_MAP;

    public DynamicFieldSqlHelper(SqlTree tree, Class<T> entityClass) {
        super(entityClass);
        if (tree == null) {
            throw new IllegalArgumentException("tree can't be null");
        }
        initProperties(tree);
    }

    public DynamicFieldSqlHelper(SqlTree tree, Class<T> entityClass, ExtraFieldSuffixBuilder suffixBuilder) {
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
    protected Collection<SqlConditionU> wrapConditions(Collection<? extends SqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<SqlConditionU> result = new ArrayList<>();
        Set<String> suffixes = suffix2OperatorMap.keySet();
        for (SqlCondition conditionO : conditions) {
            String field = conditionO.getField();
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                boolean isSuffix = false;
                for (String suffix : suffixes) {
                    if (field.endsWith(suffix)) {
                        String sourceFiled = field.substring(0, field.length() - suffix.length());
                        String operator = suffix2OperatorMap.get(suffix);
                        log.debug("condition field [{}] Matched suffix operator [{}]", field, operator);
                        wrap2JdbcColumnCondition(conditionO.isOr(), sourceFiled, operator, conditionO.getValue()).ifPresent(result::add);
                        isSuffix = true;
                        break;
                    }
                }
                if (isSuffix) {
                    continue;
                }
                log.debug("condition field [{}] not exist neither in fieldMap or suffixMap , it will be removed and put into paramMap", field);
                this.map.putIfAbsent(field, conditionO.getValue());
                continue;
            }
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        return result;
    }

}
