package io.github.luminion.mybatisplus.enhancer.query.helper.processor;

import io.github.luminion.mybatisplus.enhancer.enums.SqlExtraSuffix;
import io.github.luminion.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.luminion.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.luminion.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.luminion.mybatisplus.enhancer.query.helper.ISqlHelper;
import io.github.luminion.mybatisplus.enhancer.query.helper.SqlHelper;
import io.github.luminion.mybatisplus.enhancer.util.BoostUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * 字段后缀处理器
 * <p>
 * 提供基于字段后缀的SQL条件处理功能，支持通过字段后缀自动识别SQL操作符
 *
 * @author luminion
 */
@Slf4j
public class FieldSuffixProcessor {
    /**
     * 后缀到操作符的映射关系
     */
    private final Map<String, String> suffix2OperatorMap;

    /**
     * 私有构造函数
     */
    private FieldSuffixProcessor() {
        this.suffix2OperatorMap = SqlExtraSuffix.DEFAULT_COMPLETE_MAP;
    }

    /**
     * 带参数的私有构造函数
     */
    private FieldSuffixProcessor(Map<String, String> suffix2OperatorMap) {
        if (suffix2OperatorMap == null) {
            throw new IllegalArgumentException("suffix2OperatorMap can't be null");
        }
        this.suffix2OperatorMap = suffix2OperatorMap;
    }

    /**
     * 单例实例
     */
    private static final FieldSuffixProcessor instance = new FieldSuffixProcessor();

    /**
     * 获取单例实例
     *
     * @return {@link FieldSuffixProcessor} 单例实例
     */
    public static FieldSuffixProcessor of() {
        return instance;
    }

    /**
     * 创建新的字段后缀处理器实例
     *
     * @param suffix2OperatorMap 后缀到操作符的映射关系
     * @return {@link FieldSuffixProcessor} 字段后缀处理器实例
     */
    public static FieldSuffixProcessor of(Map<String, String> suffix2OperatorMap) {
        return new FieldSuffixProcessor(suffix2OperatorMap);
    }

    /**
     * 处理SQL助手
     *
     * @param rootHelper 根SQL助手
     * @param <T>        实体类型
     * @return {@link ISqlHelper} 处理后的SQL助手
     * @throws IllegalArgumentException 当无法获取实体类时抛出
     */
    public <T> ISqlHelper<T> process(ISqlHelper<T> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        SqlHelper<T> resultHelper = SqlHelper.of(entityClass);
        Map<String, Object> extraParams = resultHelper.getExtra();
        Map<String, String> javaFieldToJdbcColumnMap = BoostUtils.javaFieldToJdbcColumnMap(entityClass);
        Set<String> suffixes = suffix2OperatorMap.keySet();
        for (ISqlTree currentHelper : rootHelper) {
            Collection<ISqlCondition> currentHelperConditions = currentHelper.getConditions();
            Iterator<ISqlCondition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<ISqlCondition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                ISqlCondition sqlCondition = conditionIterator.next();
                String field = sqlCondition.getField();
                String jdbcColumn = javaFieldToJdbcColumnMap.get(field);
                if (jdbcColumn == null) {
                    boolean isSuffixMatched = false;
                    for (String suffix : suffixes) {
                        if (field.endsWith(suffix)) {
                            isSuffixMatched = true;
                            String sourceFiled = field.substring(0, field.length() - suffix.length());
                            String operator = suffix2OperatorMap.get(suffix);
                            log.debug("condition field [{}] Matched suffix operator [{}]", field, operator);
                            SqlCondition suffixCondition = new SqlCondition(sourceFiled, operator, sqlCondition.getValue());
                            ISqlCondition validateSuffixCondition = DefaultProcessor.validateCondition(suffixCondition, javaFieldToJdbcColumnMap, extraParams);
                            if (validateSuffixCondition == null) {
                                continue;
                            }
                            validatedConditions.add(validateSuffixCondition);
                            break;
                        }
                    }
                    if (isSuffixMatched) {
                        continue;
                    }
                }
                ISqlCondition validate = DefaultProcessor.validateCondition(sqlCondition, javaFieldToJdbcColumnMap, extraParams);
                if (validate == null) {
                    continue;
                }
                validatedConditions.add(validate);
            }
            DefaultProcessor.warpConditions(resultHelper, validatedConditions, currentHelper.getConnector());
        }
        DefaultProcessor.wrapSorts(resultHelper, rootHelper.getSorts(), javaFieldToJdbcColumnMap);
        return resultHelper;
    }

}