package io.github.bootystar.mybatisplus.enhance.builder;

import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author bootystar
 */
public class FieldSuffixBuilder {
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
     * 不等于
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder ne(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NE.keyword);
        return this;
    }

    /**
     * 大于
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder gt(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.GT.keyword);
        return this;
    }

    /**
     * 大于等于
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder ge(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.GE.keyword);
        return this;
    }

    /**
     * 小于
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder lt(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LT.keyword);
        return this;
    }

    /**
     * 小于等于
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder le(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LE.keyword);
        return this;
    }

    /**
     * 模糊匹配
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder like(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.LIKE.keyword);
        return this;
    }

    /**
     * 不模糊匹配
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder notLike(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NOT_LIKE.keyword);
        return this;
    }

    /**
     * in
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder in(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IN.keyword);
        return this;
    }

    /**
     * not in
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder notIn(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.NOT_IN.keyword);
        return this;
    }

    /**
     * is null
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder isNull(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IS_NULL.keyword);
        return this;
    }

    /**
     * is not null
     *
     * @param suffix 后缀
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    public FieldSuffixBuilder isNotNull(String suffix) {
        check(suffix);
        suffix2OperatorMap.put(suffix, SqlKeyword.IS_NOT_NULL.keyword);
        return this;
    }

    /**
     * 构建
     *
     * @return {@link HashMap }<{@link String }, {@link String }>
     * @author bootystar
     */
    public LinkedHashMap<String, String> build() {
        return suffix2OperatorMap;
    }


}
