package io.github.bootystar.mybatisplus.enhance.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.SqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.SqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.SqlConditionU;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public class DynamicFieldSqlHelper<T> extends UnmodifiableSqlHelper<T> {
    private static final String SUFFIX_PATTERN = "^[a-zA-Z0-9_$]+$";
    public static void check(String suffix ,String operator) {
        if (operator == null) {
            throw new IllegalArgumentException("operator can't be null");
        }
        if (!SqlKeyword.CONDITION_OPERATORS_ALL.contains(operator)){
            throw new IllegalArgumentException("illegal operator [" + operator + "] , not support this operator");
        }
        if (suffix == null) {
            throw new IllegalArgumentException("suffix can't be null");
        }
        if (!suffix.matches(SUFFIX_PATTERN)) {
            throw new IllegalArgumentException("illegal suffix [" + suffix + "] , field name cannot contain special characters");
//                throw new IllegalArgumentException("illegal suffix [" + suffix + "] , it does not match the regular expression:" + SUFFIX_PATTERN);
        }
    }


    private Map<String, String> suffix2OperatorMap = SqlExtraSuffix.DEFAULT_ALL_MAP;

    public DynamicFieldSqlHelper(SqlTree tree, Class<T> entityClass) {
        super(entityClass);
        if (tree == null) {
            throw new IllegalArgumentException("tree can't be null");
        }
        initProperties(tree);
    }

    public DynamicFieldSqlHelper(SqlTree tree, Class<T> entityClass, Map<String, String> suffix2OperatorMap) {
        super(entityClass);
        if (tree == null) {
            throw new IllegalArgumentException("tree can't be null");
        }
        if (suffix2OperatorMap != null) {
            for (Map.Entry<String, String> stringStringEntry : suffix2OperatorMap.entrySet()) {
                String key = stringStringEntry.getKey();
                String value = stringStringEntry.getValue();
                check(key,value);
            }
            this.suffix2OperatorMap = suffix2OperatorMap;
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
                for (String suffix : suffixes) {
                    if (field.endsWith(suffix)) {
                        String sourceFiled = field.substring(0, field.length() - suffix.length());
                        String operator = suffix2OperatorMap.get(suffix);
                        wrap2JdbcColumnCondition(conditionO.isOr(), sourceFiled, operator, conditionO.getValue()).ifPresent(result::add);
                        break;
                    }
                }
                log.info("condition field [{}] not exist in fieldMap , it will be removed and put into paramMap", field);
                this.map.putIfAbsent(field, conditionO.getValue());
                continue;
            }
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        return result;
    }

}
