package io.github.bootystar.mybatisplus.enhancer.builder;

import io.github.bootystar.mybatisplus.enhancer.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;

import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 * 字段后缀构建器
 * <p>
 * 用于构建字段后缀与SQL操作符之间的映射关系，支持动态SQL查询中的字段后缀匹配功能
 *
 * @author bootystar
 */
public class ExtraFieldSuffixBuilder {
    private static final String SUFFIX_PATTERN = "^[a-zA-Z0-9_$]+$";
    private final LinkedHashMap<String, String> suffix2OperatorMap = new LinkedHashMap<>();

    /**
     * 检查后缀是否合法
     *
     * @param suffix 待检查的后缀
     * @throws IllegalArgumentException 当后缀为null或包含特殊字符时抛出
     */
    private void check(String suffix) {
        if (suffix == null) {
            throw new IllegalArgumentException("suffix can't be null");
        }
        if (!suffix.matches(SUFFIX_PATTERN)) {
            throw new IllegalArgumentException("illegal suffix [" + suffix + "] , field name cannot contain special characters");
//                throw new IllegalArgumentException("illegal suffix [" + suffix + "] , it does not match the regular expression:" + SUFFIX_PATTERN);
        }
    }

    /**
     * 添加常用的后缀
     *
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder defaultSimpleSuffix() {
        suffix2OperatorMap.putAll(SqlExtraSuffix.DEFAULT_SIMPLE_MAP);
        return this;
    }

    /**
     * 添加所有支持的后缀
     *
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder defaultCompleteSuffix() {
        suffix2OperatorMap.putAll(SqlExtraSuffix.DEFAULT_COMPLETE_MAP);
        return this;
    }

    /**
     * 添加不等于操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder ne(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NE.keyword);
        return this;
    }

    /**
     * 添加大于操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder gt(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.GT.keyword);
        return this;
    }

    /**
     * 添加大于等于操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder ge(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.GE.keyword);
        return this;
    }

    /**
     * 添加小于操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder lt(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LT.keyword);
        return this;
    }

    /**
     * 添加小于等于操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder le(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LE.keyword);
        return this;
    }

    /**
     * 添加模糊匹配操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder like(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LIKE.keyword);
        return this;
    }

    /**
     * 添加不模糊匹配操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder notLike(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NOT_LIKE.keyword);
        return this;
    }

    /**
     * 添加in操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder in(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IN.keyword);
        return this;
    }

    /**
     * 添加not in操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder notIn(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NOT_IN.keyword);
        return this;
    }

    /**
     * 添加is null操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder isNull(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IS_NULL.keyword);
        return this;
    }

    /**
     * 添加is not null操作符的后缀
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder } 构建器实例
     */
    public ExtraFieldSuffixBuilder isNotNull(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IS_NOT_NULL.keyword);
        return this;
    }

    /**
     * 构建后缀到操作符的映射关系
     *
     * @return {@link HashMap } 后缀到操作符的映射关系
     */
    public LinkedHashMap<String, String> build() {
        return suffix2OperatorMap;
    }


}