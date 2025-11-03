package io.github.luminion.mybatis.query.helper.processor;

import io.github.luminion.mybatis.enums.SqlKeyword;
import io.github.luminion.mybatis.query.core.ISqlCondition;
import io.github.luminion.mybatis.query.core.ISqlTree;
import io.github.luminion.mybatis.query.entity.SqlCondition;
import io.github.luminion.mybatis.query.helper.ISqlHelper;
import io.github.luminion.mybatis.query.helper.SqlHelper;
import io.github.luminion.mybatis.util.BoostUtils;
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
     * 操作符后缀映射表
     */
    private static final Map<String, String> SUFFIX_TO_OPERATOR_MAP;

    static {
        Map<String, String> suffixMap = new LinkedHashMap<>();

        suffixMap.put("Ne", SqlKeyword.NE.getKeyword());
        suffixMap.put("_ne", SqlKeyword.NE.getKeyword());

        suffixMap.put("Lt", SqlKeyword.LT.getKeyword());
        suffixMap.put("_lt", SqlKeyword.LT.getKeyword());

        suffixMap.put("Le", SqlKeyword.LE.getKeyword());
        suffixMap.put("_le", SqlKeyword.LE.getKeyword());

        suffixMap.put("Gt", SqlKeyword.GT.getKeyword());
        suffixMap.put("_gt", SqlKeyword.GT.getKeyword());

        suffixMap.put("Ge", SqlKeyword.GE.getKeyword());
        suffixMap.put("_ge", SqlKeyword.GE.getKeyword());

        suffixMap.put("Like", SqlKeyword.LIKE.getKeyword());
        suffixMap.put("_like", SqlKeyword.LIKE.getKeyword());

        suffixMap.put("NotLike", SqlKeyword.NOT_LIKE.getKeyword());
        suffixMap.put("_not_like", SqlKeyword.NOT_LIKE.getKeyword());

        suffixMap.put("In", SqlKeyword.IN.getKeyword());
        suffixMap.put("_in", SqlKeyword.IN.getKeyword());

        suffixMap.put("NotIn", SqlKeyword.NOT_IN.getKeyword());
        suffixMap.put("_not_in", SqlKeyword.NOT_IN.getKeyword());

        suffixMap.put("IsNull", SqlKeyword.IS_NULL.getKeyword());
        suffixMap.put("_is_null", SqlKeyword.IS_NULL.getKeyword());

        suffixMap.put("IsNotNull", SqlKeyword.IS_NOT_NULL.getKeyword());
        suffixMap.put("_is_not_null", SqlKeyword.IS_NOT_NULL.getKeyword());

        suffixMap.put("BitContains", SqlKeyword.BIT_CONTAINS.getKeyword());
        suffixMap.put("_bit_contains", SqlKeyword.BIT_CONTAINS.getKeyword());

        suffixMap.put("BitNotContains", SqlKeyword.BIT_NOT_CONTAINS.getKeyword());
        suffixMap.put("_bit_not_contains", SqlKeyword.BIT_NOT_CONTAINS.getKeyword());

        SUFFIX_TO_OPERATOR_MAP = Collections.unmodifiableMap(suffixMap);
    }


    /**
     * 后缀到操作符的映射关系
     */
    private final Map<String, String> suffixToOperatorMap;

    /**
     * 私有构造函数
     */
    private FieldSuffixProcessor() {
        this.suffixToOperatorMap = SUFFIX_TO_OPERATOR_MAP;
    }

    /**
     * 带参数的私有构造函数
     */
    private FieldSuffixProcessor(Map<String, String> suffixToOperatorMap) {
        if (suffixToOperatorMap == null) {
            throw new IllegalArgumentException("suffix2OperatorMap can't be null");
        }
        this.suffixToOperatorMap = suffixToOperatorMap;
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
        Map<String, String> entityPropertyToColumnAliasMap = BoostUtils.getPropertyToColumnAliasMap(entityClass);
        Set<String> suffixes = suffixToOperatorMap.keySet();
        for (ISqlTree currentHelper : rootHelper) {
            Collection<ISqlCondition> currentHelperConditions = currentHelper.getConditions();
            Iterator<ISqlCondition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<ISqlCondition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                ISqlCondition sqlCondition = conditionIterator.next();
                String field = sqlCondition.getField();
                String jdbcColumn = entityPropertyToColumnAliasMap.get(field);
                if (jdbcColumn == null) {
                    boolean isSuffixMatched = false;
                    for (String suffix : suffixes) {
                        if (field.endsWith(suffix) && field.length() > suffix.length()) {
                            isSuffixMatched = true;
                            String sourceFiled = field.substring(0, field.length() - suffix.length());
                            String operator = suffixToOperatorMap.get(suffix);
                            log.debug("condition field [{}] Matched suffix operator [{}]", field, operator);
                            SqlCondition suffixCondition = new SqlCondition(sourceFiled, operator, sqlCondition.getValue());
                            ISqlCondition validateSuffixCondition = DefaultProcessor.validateCondition(suffixCondition, entityPropertyToColumnAliasMap, extraParams);
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
                ISqlCondition validate = DefaultProcessor.validateCondition(sqlCondition, entityPropertyToColumnAliasMap, extraParams);
                if (validate == null) {
                    continue;
                }
                validatedConditions.add(validate);
            }
            DefaultProcessor.warpConditions(resultHelper, validatedConditions, currentHelper.getConnector());
        }
        DefaultProcessor.wrapSorts(resultHelper, rootHelper.getSorts(), entityPropertyToColumnAliasMap);
        return resultHelper;
    }

}