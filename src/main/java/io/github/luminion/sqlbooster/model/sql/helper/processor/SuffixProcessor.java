package io.github.luminion.sqlbooster.model.sql.helper.processor;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.api.Condition;
import io.github.luminion.sqlbooster.model.api.Tree;
import io.github.luminion.sqlbooster.model.sql.SqlCondition;
import io.github.luminion.sqlbooster.model.sql.helper.BaseHelper;
import io.github.luminion.sqlbooster.model.sql.helper.SqlHelper;
import io.github.luminion.sqlbooster.util.BoostUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * 字段后缀处理器.
 * <p>
 * 提供基于字段后缀的 SQL 条件处理功能, 支持通过字段后缀自动识别 SQL 操作符.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public class SuffixProcessor {
    /**
     * 默认的后缀到操作符的映射表.
     */
    private static final Map<String, String> suffixMap = new HashMap<>();

    static {
        add("Ne", SqlKeyword.NE.getKeyword());
        add("Lt", SqlKeyword.LT.getKeyword());
        add("Le", SqlKeyword.LE.getKeyword());
        add("Gt", SqlKeyword.GT.getKeyword());
        add("Ge", SqlKeyword.GE.getKeyword());
        add("Like", SqlKeyword.LIKE.getKeyword());
        add("NotLike", SqlKeyword.NOT_LIKE.getKeyword());
        add("In", SqlKeyword.IN.getKeyword());
        add("NotIn", SqlKeyword.NOT_IN.getKeyword());
        add("Null", SqlKeyword.IS_NULL.getKeyword());
        add("IsNull", SqlKeyword.IS_NULL.getKeyword());
        add("NotNull", SqlKeyword.IS_NOT_NULL.getKeyword());
        add("IsNotNull", SqlKeyword.IS_NOT_NULL.getKeyword());
        add("BitIn", SqlKeyword.BIT_IN.getKeyword());
        add("BitNotIn", SqlKeyword.BIT_NOT_IN.getKeyword());
    }

    private static void add(String camelCase, String operator) {
        suffixMap.put(camelCase, operator);
        String underscore = BoostUtils.camelCaseToUnderscore(camelCase);
        suffixMap.put(underscore, operator);
    }


    /**
     * 当前处理器实例使用的后缀到操作符的映射.
     */
    private final Map<String, String> suffixToOperatorMap;

    /**
     * 私有构造函数, 使用默认的后缀映射.
     */
    private SuffixProcessor() {
        this.suffixToOperatorMap = SUFFIX_TO_OPERATOR_MAP;
    }

    /**
     * 私有构造函数, 使用自定义的后缀映射.
     *
     * @param suffixToOperatorMap 自定义的后缀到操作符的映射
     */
    private SuffixProcessor(Map<String, String> suffixToOperatorMap) {
        if (suffixToOperatorMap == null) {
            throw new IllegalArgumentException("suffix2OperatorMap can't be null");
        }
        this.suffixToOperatorMap = suffixToOperatorMap;
    }

    /**
     * 默认的单例实例.
     */
    private static final SuffixProcessor instance = new SuffixProcessor();

    /**
     * 获取默认的 {@link SuffixProcessor} 单例实例.
     *
     * @return 单例实例
     * @since 1.0.0
     */
    public static SuffixProcessor of() {
        return instance;
    }

    /**
     * 创建一个新的 {@link SuffixProcessor} 实例, 使用自定义的后缀映射.
     *
     * @param suffix2OperatorMap 自定义的后缀到操作符的映射
     * @return 新的 {@link SuffixProcessor} 实例
     * @since 1.0.0
     */
    public static SuffixProcessor of(Map<String, String> suffix2OperatorMap) {
        return new SuffixProcessor(suffix2OperatorMap);
    }

    /**
     * 处理 SQL 助手, 将字段后缀转换为对应的 SQL 操作符.
     *
     * @param rootHelper 根 SQL 助手
     * @param <T>        实体类型
     * @return 处理后的 {@link BaseHelper} 实例
     * @throws IllegalArgumentException 当无法获取实体类时抛出
     * @since 1.0.0
     */
    public <T> BaseHelper<T> process(BaseHelper<T> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        SqlHelper<T> resultHelper = SqlHelper.of(entityClass);
        Map<String, Object> extraParams = resultHelper.getExtra();
        Map<String, String> entityPropertyToColumnAliasMap = BoostUtils.getPropertyToColumnAliasMap(entityClass);
        Set<String> suffixes = suffixToOperatorMap.keySet();
        for (Tree currentHelper : rootHelper) {
            Collection<Condition> currentHelperConditions = currentHelper.getConditions();
            Iterator<Condition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<Condition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                Condition condition = conditionIterator.next();
                String field = condition.getField();
                String jdbcColumn = entityPropertyToColumnAliasMap.get(field);
                if (jdbcColumn == null) {
                    boolean isSuffixMatched = false;
                    for (String suffix : suffixes) {
                        if (field.endsWith(suffix) && field.length() > suffix.length()) {
                            isSuffixMatched = true;
                            String sourceFiled = field.substring(0, field.length() - suffix.length());
                            String operator = suffixToOperatorMap.get(suffix);
                            log.debug("condition field [{}] Matched suffix operator [{}]", field, operator);
                            SqlCondition suffixCondition = new SqlCondition(sourceFiled, operator, condition.getValue());
                            Condition validateSuffixCondition = BasicProcessor.validateCondition(suffixCondition, entityPropertyToColumnAliasMap, extraParams);
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
                Condition validate = BasicProcessor.validateCondition(condition, entityPropertyToColumnAliasMap, extraParams);
                if (validate == null) {
                    continue;
                }
                validatedConditions.add(validate);
            }
            BasicProcessor.warpConditions(resultHelper, validatedConditions, currentHelper.getConnector());
        }
        BasicProcessor.wrapSorts(resultHelper, rootHelper.getSorts(), entityPropertyToColumnAliasMap);
        return resultHelper;
    }

}