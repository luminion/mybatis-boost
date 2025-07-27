package io.github.bootystar.mybatisplus.enhancer.query.helper;

import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlSort;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;

import java.util.Collection;
import java.util.Iterator;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author bootystar
 */
@SuppressWarnings({"unchecked", "unused"})
public interface ISqlHelperLambda<T, S extends ISqlHelperLambda<T, S>> extends ISqlHelper<T> {

    default S or(Function<S,S> function) {
        // todo or

        return (S) this;
    }
    
    
    /**
     * 等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S eq(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.EQ.keyword, value));
        return (S) this;
    }

    /**
     * 不等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S ne(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.NE.keyword, value));
        return (S) this;
    }

    /**
     * 大于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S gt(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.GT.keyword, value));
        return (S) this;
    }

    /**
     * 大于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S ge(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.GE.keyword, value));
        return (S) this;
    }

    /**
     * 小于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S lt(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.LT.keyword, value));
        return (S) this;
    }

    /**
     * 小于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S le(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.LE.keyword, value));
        return (S) this;
    }

    /**
     * 模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S like(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.LIKE.keyword, value));
        return (S) this;
    }

    /**
     * 不模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S notLike(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.NOT_LIKE.keyword, value));
        return (S) this;
    }

    /**
     * in查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S in(SFunction<T, R> getter, Collection<? extends R> value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.IN.keyword, value));
        return (S) this;
    }

    /**
     * notin查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S notIn(SFunction<T, R> getter, Collection<? extends R> value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.NOT_IN.keyword, value));
        return (S) this;
    }

    /**
     * 指定字段为空
     *
     * @param getter 对象getter方法
     * @return this
     */
    default S isNull(SFunction<T, ?> getter) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.IS_NULL.keyword, null));
        return (S) this;
    }

    /**
     * 指定字段不为空
     *
     * @param getter 对象getter方法
     * @return this
     */
    default S isNotNull(SFunction<T, ?> getter) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.IS_NOT_NULL.keyword, null));
        return (S) this;
    }

    default S orderByAsc(SFunction<T, ?> getter) {
        getSorts().add(new SqlSort(MybatisPlusReflectUtil.getterFieldName(getter), false));
        return (S) this;
    }

    default S orderByDesc(SFunction<T, ?> getter) {
        getSorts().add(new SqlSort(MybatisPlusReflectUtil.getterFieldName(getter), true));
        return (S) this;
    }

    /**
     * 位运算,具有指定值对应的位码
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S bitwiseWith(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.BITWISE_WITH.keyword, value));
        return (S) this;
    }

    /**
     * 位运算,不具有指定值对应的位码
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    default <R> S bitwiseWithout(SFunction<T, R> getter, R value) {
        getConditions().add(new SqlCondition(MybatisPlusReflectUtil.getterFieldName(getter), SqlKeyword.BITWISE_WITHOUT.keyword, value));
        return (S) this;
    }

}
