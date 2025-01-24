package io.github.bootystar.mybatisplus.enhance.builder;

import io.github.bootystar.mybatisplus.enhance.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;

import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 * @author bootystar
 */
public class ExtraFieldSuffixBuilder {
    private static final String SUFFIX_PATTERN = "^[a-zA-Z0-9_$]+$";
    private final LinkedHashMap<String, String> suffix2OperatorMap = new LinkedHashMap<>();

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
     * 添加常用的默认后缀
     *
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder defaultSuffix() {
        suffix2OperatorMap.putAll(SqlExtraSuffix.DEFAULT_SIMPLE_MAP);
        return this;
    }

    /**
     * 添加所有的默认后缀
     *
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder defaultSuffixFull() {
        suffix2OperatorMap.putAll(SqlExtraSuffix.DEFAULT_FULL_MAP);
        return this;
    }

    /**
     * 不等于
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder ne(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NE.keyword);
        return this;
    }

    /**
     * 大于
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder gt(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.GT.keyword);
        return this;
    }

    /**
     * 大于等于
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder ge(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.GE.keyword);
        return this;
    }

    /**
     * 小于
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder lt(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LT.keyword);
        return this;
    }

    /**
     * 小于等于
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder le(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LE.keyword);
        return this;
    }

    /**
     * 模糊匹配
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder like(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LIKE.keyword);
        return this;
    }

    /**
     * 不模糊匹配
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder notLike(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NOT_LIKE.keyword);
        return this;
    }

    /**
     * in
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder in(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IN.keyword);
        return this;
    }

    /**
     * not in
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder notIn(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NOT_IN.keyword);
        return this;
    }

    /**
     * is null
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder isNull(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IS_NULL.keyword);
        return this;
    }

    /**
     * is not null
     *
     * @param suffix 后缀
     * @return {@link ExtraFieldSuffixBuilder }
     */
    public ExtraFieldSuffixBuilder isNotNull(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IS_NOT_NULL.keyword);
        return this;
    }

    /**
     * 构建
     *
     * @return {@link HashMap }<{@link String }, {@link String }>
     */
    public LinkedHashMap<String, String> build() {
        return suffix2OperatorMap;
    }


}
