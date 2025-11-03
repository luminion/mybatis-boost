package io.github.luminion.mybatis.query.helper;

import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.enums.SqlKeyword;
import io.github.luminion.mybatis.query.entity.SqlCondition;
import io.github.luminion.mybatis.query.entity.SqlSort;
import io.github.luminion.mybatis.util.BoostUtils;

import java.util.Collection;
import java.util.function.Consumer;

/**
 * Lambda SQL助手接口
 * <p>
 * 提供基于Lambda表达式的SQL条件构建方法，支持链式调用
 *
 * @param <T> 实体类型
 * @param <S> 返回类型（用于支持链式调用）
 * @author luminion
 */
@SuppressWarnings({"unchecked", "unused"})
public interface ISqlHelperLambda<T, S extends ISqlHelperLambda<T, S>> extends ISqlHelper<T> {

    /**
     * 添加OR条件
     *
     * @param consumer 拼装或条件的函数
     * @return {@link S } 当前实例
     */
    S or(Consumer<S> consumer);

    /**
     * 等于条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S eq(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.EQ.getKeyword(), value));
        return (S) this;
    }

    /**
     * 不等于条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S ne(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.NE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 大于条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S gt(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.GT.getKeyword(), value));
        return (S) this;
    }

    /**
     * 大于等于条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S ge(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.GE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 小于条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S lt(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.LT.getKeyword(), value));
        return (S) this;
    }

    /**
     * 小于等于条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S le(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.LE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 模糊查询条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S like(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.LIKE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 不模糊查询条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S notLike(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.NOT_LIKE.getKeyword(), value));
        return (S) this;
    }

    /**
     * in查询条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S in(MethodReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.IN.getKeyword(), value));
        return (S) this;
    }

    /**
     * not in查询条件
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S notIn(MethodReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.NOT_IN.getKeyword(), value));
        return (S) this;
    }

    /**
     * 指定字段为空条件
     *
     * @param getter 对象getter方法
     * @return this
     */
    default S isNull(MethodReference<T, ?> getter) {
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.IS_NULL.getKeyword(), null));
        return (S) this;
    }

    /**
     * 指定字段不为空条件
     *
     * @param getter 对象getter方法
     * @return this
     */
    default S isNotNull(MethodReference<T, ?> getter) {
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.IS_NOT_NULL.getKeyword(), null));
        return (S) this;
    }

    /**
     * 按指定字段正序排序
     *
     * @param getter 对象getter方法
     * @return this
     */
    default S orderByAsc(MethodReference<T, ?> getter) {
        getSorts().add(new SqlSort(BoostUtils.getGetterPropertyName(getter), false));
        return (S) this;
    }

    /**
     * 按指定字段倒序排序
     *
     * @param getter 对象getter方法
     * @return this
     */
    default S orderByDesc(MethodReference<T, ?> getter) {
        getSorts().add(new SqlSort(BoostUtils.getGetterPropertyName(getter), true));
        return (S) this;
    }

    /**
     * 位运算条件，具有指定值对应的位码
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S bitWith(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.BIT_CONTAINS.getKeyword(), value));
        return (S) this;
    }

    /**
     * 位运算条件，不具有指定值对应的位码
     *
     * @param getter 对象getter方法
     * @param value  值
     * @param <R>    值的类型
     * @return this
     */
    default <R> S bitWithout(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.BIT_NOT_CONTAINS.getKeyword(), value));
        return (S) this;
    }

}