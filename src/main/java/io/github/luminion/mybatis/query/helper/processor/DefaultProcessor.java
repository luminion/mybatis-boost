package io.github.luminion.mybatis.query.helper.processor;

import io.github.luminion.mybatis.enums.SqlKeyword;
import io.github.luminion.mybatis.query.core.ISqlCondition;
import io.github.luminion.mybatis.query.core.ISqlSort;
import io.github.luminion.mybatis.query.core.ISqlTree;
import io.github.luminion.mybatis.query.entity.SqlCondition;
import io.github.luminion.mybatis.query.entity.SqlSort;
import io.github.luminion.mybatis.query.entity.SqlTree;
import io.github.luminion.mybatis.query.helper.AbstractSqlHelper;
import io.github.luminion.mybatis.query.helper.ISqlHelper;
import io.github.luminion.mybatis.query.helper.SqlHelper;
import io.github.luminion.mybatis.util.BoostUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 默认 SQL 处理器.
 * <p>
 * 提供 SQL 条件的验证和处理功能, 包括字段映射验证、操作符验证等.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class DefaultProcessor {

    /**
     * 验证并处理单个 SQL 条件.
     * <p>
     * <ul>
     *     <li>验证字段是否存在于映射中.</li>
     *     <li>验证操作符和值的合法性.</li>
     *     <li>处理 LIKE 操作符的值.</li>
     *     <li>将不符合条件的字段或无法处理的条件放入 `extra` Map 中.</li>
     * </ul>
     *
     * @param sqlCondition             待验证的 SQL 条件
     * @param propertyToColumnAliasMap 属性名到数据库列别名的映射
     * @param extra                    用于存储额外参数的 Map
     * @return 验证并处理后的 {@link ISqlCondition}, 如果条件无效则返回 null
     * @since 1.0.0
     */
    public static ISqlCondition validateCondition(ISqlCondition sqlCondition, Map<String, String> propertyToColumnAliasMap, Map<String, Object> extra) {
        String field = sqlCondition.getField();
        String operator = sqlCondition.getOperator();
        Object value = sqlCondition.getValue();
        if (field == null || field.isEmpty()) {
            return null;
        }
        String jdbcColumn = propertyToColumnAliasMap.get(field);
        if (jdbcColumn == null) {
            log.debug("condition field [{}] not exist in fieldMap , it will be removed and put into paramMap", field);
            extra.putIfAbsent(field, value);
            return null;
        }
        operator = SqlKeyword.replaceOperator(operator);
        if (!SqlKeyword.isNullOperator(operator) && value == null) {
            log.info("condition field [{}] requires value but value is null, it will be removed and put into paramMap", field);
            extra.putIfAbsent(field, "null");
            return null;
        }
        if (SqlKeyword.isInOperator(operator)) {
            boolean iterableValue = value instanceof Iterable;
            if (!iterableValue) {
                log.debug("condition field [{}] requires collection but value is not iterable, it will be removed and put into paramMap", field);
                extra.putIfAbsent(field, value);
                return null;
            }
            Iterable<?> iterable = (Iterable<?>) value;
            if (!iterable.iterator().hasNext()) {
                log.debug("condition field [{}] requires collection but value is empty, it will be removed and put into paramMap", field);
                extra.putIfAbsent(field, value);
                return null;
            }
        }
        if (SqlKeyword.isBitOperator(operator)) {
            boolean isNumber = value instanceof Number;
            if (!isNumber) {
                log.debug("condition field [{}] requires number but value is not a number, it will be removed and put into paramMap", field);
                extra.putIfAbsent(field, value);
                return null;
            }
        }
        if (SqlKeyword.isLikeOperator(operator)) {
//            if(!(value instanceof String)){
//                log.debug("condition field [{}] requires string but value is not string, it will be removed and put into paramMap", field);
//                extra.putIfAbsent(field, value);
//                return null;
//            }
            if (!value.toString().contains("%")) {
                value = "%" + value + "%";
            }
        }
        return new SqlCondition(jdbcColumn, operator, value);
    }

    /**
     * 验证并处理单个 SQL 排序规则.
     *
     * @param sqlSort             待验证的 SQL 排序规则
     * @param field2JdbcColumnMap 属性名到数据库列名的映射
     * @return 验证并处理后的 {@link ISqlSort}, 如果排序字段无效则返回 null
     * @since 1.0.0
     */
    public static ISqlSort validateSort(ISqlSort sqlSort, Map<String, String> field2JdbcColumnMap) {
        String jdbcColumn = field2JdbcColumnMap.get(sqlSort.getField());
        if (jdbcColumn == null) {
            log.warn("sort field [{}] not exist in fieldMap , it will be removed", sqlSort.getField());
            return null;
        }
        return new SqlSort(jdbcColumn, sqlSort.isDesc());
    }

    /**
     * 将一组 SQL 条件包装到 {@link AbstractSqlHelper} 中.
     *
     * @param sqlHelper     目标 SQL 助手
     * @param sqlConditions 待包装的 SQL 条件集合
     * @param symbol        连接这些条件的逻辑符号 (AND/OR)
     * @since 1.0.0
     */
    public static void warpConditions(AbstractSqlHelper<?, ?> sqlHelper, Collection<ISqlCondition> sqlConditions, String symbol) {
        if (sqlHelper == null || sqlConditions == null || sqlConditions.isEmpty()) {
            return;
        }
        symbol = SqlKeyword.replaceConnector(symbol);
        if (SqlKeyword.AND.getKeyword().equals(symbol)) {
            sqlHelper.getConditions().addAll(sqlConditions);
            return;
        }
        SqlTree iSqlTrees = new SqlTree(sqlConditions, SqlKeyword.OR.getKeyword());
        sqlHelper.with(iSqlTrees);
    }

    /**
     * 将一组 SQL 排序规则包装到 {@link AbstractSqlHelper} 中.
     *
     * @param sqlHelper           目标 SQL 助手
     * @param sqlSorts            待包装的 SQL 排序规则集合
     * @param field2JdbcColumnMap 属性名到数据库列名的映射
     * @since 1.0.0
     */
    public static void wrapSorts(AbstractSqlHelper<?, ?> sqlHelper, Collection<ISqlSort> sqlSorts, Map<String, String> field2JdbcColumnMap) {
        for (ISqlSort sqlSort : sqlSorts) {
            ISqlSort iSqlSort = validateSort(sqlSort, field2JdbcColumnMap);
            if (iSqlSort != null) {
                sqlHelper.getSorts().add(iSqlSort);
            }
        }
    }

    /**
     * 处理并转换根 SQL 助手, 生成一个经过验证和映射的 SQL 助手.
     *
     * @param rootHelper 根 SQL 助手
     * @param <T>        实体类型
     * @return 处理后的 {@link ISqlHelper} 实例
     * @throws IllegalArgumentException 当无法获取实体类时抛出
     * @since 1.0.0
     */
    public static <T> ISqlHelper<T> process(ISqlHelper<T> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        SqlHelper<T> resultHelper = SqlHelper.of(entityClass);
        Map<String, String> field2JdbcColumnMap = BoostUtils.getPropertyToColumnAliasMap(entityClass);
        Map<String, Object> extraParams = resultHelper.getExtra();
        for (ISqlTree currentHelper : rootHelper) {
            Collection<ISqlCondition> currentHelperConditions = currentHelper.getConditions();
            Iterator<ISqlCondition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<ISqlCondition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                ISqlCondition sqlCondition = conditionIterator.next();
                ISqlCondition validate = DefaultProcessor.validateCondition(sqlCondition, field2JdbcColumnMap, extraParams);
                if (validate == null) {
                    continue;
                }
                validatedConditions.add(validate);
            }
            DefaultProcessor.warpConditions(resultHelper, validatedConditions, currentHelper.getConnector());
        }
        DefaultProcessor.wrapSorts(resultHelper, rootHelper.getSorts(), field2JdbcColumnMap);
        return resultHelper;
    }

}