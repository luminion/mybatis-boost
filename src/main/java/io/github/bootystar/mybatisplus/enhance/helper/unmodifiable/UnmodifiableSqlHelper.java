package io.github.bootystar.mybatisplus.enhance.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.expception.ParamMappingException;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlEntity;
import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.SqlConditionU;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.SqlEntityU;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.SqlSortU;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.SqlTreeU;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public abstract class UnmodifiableSqlHelper<T> extends SqlEntityU {

    /**
     * 实体类
     */
    @Getter
    protected final Class<T> entityClass;
    /**
     * 无效条件map
     * key: 传入属性名
     * value: 值
     */
    @Getter
    protected Map<String, Object> map = new LinkedHashMap<>();

    /**
     * 属性和字段映射
     */
    protected final Map<String, String> field2JdbcColumnMap;

    public UnmodifiableSqlHelper(Class<T> entityClass) {
        super(null, null, null);
        if (entityClass == null) {
            throw new ParamMappingException("entityClass class can not be null, please check your configuration");
        }
        this.entityClass = entityClass;
        Map<String, String> field2JdbcColumnMap = MybatisPlusReflectHelper.field2JdbcColumnMap(entityClass);
        if (field2JdbcColumnMap.isEmpty()) {
            throw new ParamMappingException("entityClass %s has no field to convert, please check your configuration", entityClass.getName());
        }
        this.field2JdbcColumnMap = field2JdbcColumnMap;
    }

    protected void initProperties(ISqlTree sqlTree) {
        // 不在迭代时直接赋值, 保留扩展空间
        SqlTreeU tree = recursionTree(sqlTree);
        if (tree == null) {
            return;
        }
        this.conditions = tree.getConditions();
        this.child = tree.getChild();
        this.sorts = validatedSorts(sqlTree);
    }

    protected SqlTreeU recursionTree(ISqlTree sqlTree) {
        if (sqlTree == null) {
            return null;
        }
        Collection<SqlConditionU> conditions = wrapConditions(sqlTree.getConditions());
        ISqlTree child = sqlTree.getChild();
        if (conditions == null || conditions.isEmpty()) {
            if (child != null) {
                String s = Optional.ofNullable(sqlTree.getConditions()).orElse(Collections.emptyList()).stream()
                        .map(e -> String.format("isOr:[%s] , field:[%s] , operator:[%s] ,value:[%s]", e.isOr(), e.getField(), e.getOperator(), e.getValue()))
                        .reduce((e1, e2) -> e1 + "\n" + e2).orElse("null");
                throw new ParamMappingException("mapped condition is empty, but sub-condition is not empty !!!\ncurrent condition source:\n%s", s);
            }
            return null;
        }
        SqlTreeU newChild = recursionTree(child);
        return new SqlTreeU(conditions, newChild);
    }

    protected Collection<SqlSortU> validatedSorts(ISqlTree sqlTree) {
        if (!(sqlTree instanceof ISqlEntity)) {
            return null;
        }
        Collection<? extends ISqlSort> sorts = ((ISqlEntity) sqlTree).getSorts();
        if (sorts == null || sorts.isEmpty()) {
            return null;
        }
        ArrayList<SqlSortU> validatedSorts = new ArrayList<>(sorts.size());
        for (ISqlSort sortO : sorts) {
            String field = sortO.getField();
            boolean desc = sortO.isDesc();
            if (field == null || field.isEmpty()) {
                log.info("sort field [{}] is null , it will be removed", field);
                continue;
            }
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                log.info("sort field [{}] not exist in fieldMap , it will be removed", field);
                continue;
            }
            validatedSorts.add(new SqlSortU(jdbcColumn, desc));
        }
        return validatedSorts;
    }

    // 由子类实现
    protected abstract Collection<SqlConditionU> wrapConditions(Collection<? extends ISqlCondition> conditions);

    public Optional<SqlConditionU> wrap2JdbcColumnCondition(ISqlCondition conditionO) {
        boolean or = conditionO.isOr();
        String operator = SqlKeyword.replaceOperator(conditionO.getOperator());
        String field = conditionO.getField();
        Object value = conditionO.getValue();
        return wrap2JdbcColumnCondition(or, field, operator, value);
    }

    public Optional<SqlConditionU> wrap2JdbcColumnCondition(boolean isOr, String field, String operator, Object value) {
        if (field == null || field.isEmpty()) {
            return Optional.empty();
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
        if (SqlKeyword.isLikeOperator(operator)) {
            value = "%" + value + "%";
        }
        return Optional.of(new SqlConditionU(isOr, jdbcColumn, operator, value));
    }

}
