package io.github.luminion.mybatisplus.enhancer.query.helper;

import io.github.luminion.mybatisplus.enhancer.core.BoostCore;
import io.github.luminion.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.luminion.mybatisplus.enhancer.util.BoostUtils;

import java.util.function.Consumer;

/**
 * SQL助手实现类
 * <p>
 * 提供SQL构建的具体实现，支持链式调用和Lambda表达式
 *
 * @param <T> 实体类型
 * @author luminion
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends AbstractSqlHelper<T, SqlHelper<T>> {

    /**
     * 创建一个新的SQL助手实例
     *
     * @return {@link SqlHelper} SQL助手实例
     */
    public static <T> SqlHelper<T> of() {
        return new SqlHelper<>();
    }

    /**
     * 创建指定实体类型的SQL助手实例
     *
     * @param clazz 实体类
     * @return {@link SqlHelper} SQL助手实例
     */
    public static <T> SqlHelper<T> of(Class<T> clazz) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = clazz;
        return sqlHelper;
    }

    /**
     * 创建boostCore对应的SQL助手实例
     *
     * @param boostCore boostCore
     * @return {@link SqlHelper} SQL助手实例
     */
    public static <T, R> SqlHelper<T> of(BoostCore<T, R> boostCore) {
        return SqlHelper.of(BoostUtils.getEntityClass(boostCore));
    }


    /**
     * 添加OR条件
     *
     * @param sqlHelper 拼装或条件的函数
     * @return {@link SqlHelper} 当前实例
     */
    @Override
    public SqlHelper<T> or(Consumer<SqlHelper<T>> sqlHelper) {
        SqlHelper<T> child = new SqlHelper<>();
        child.connector = SqlKeyword.OR.keyword;
        sqlHelper.accept(child);
        this.addChild(child);
        return this;
    }

}