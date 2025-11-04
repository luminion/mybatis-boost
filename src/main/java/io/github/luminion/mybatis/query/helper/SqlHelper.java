package io.github.luminion.mybatis.query.helper;

import io.github.luminion.mybatis.core.BoostCore;
import io.github.luminion.mybatis.core.Booster;
import io.github.luminion.mybatis.enums.SqlKeyword;
import io.github.luminion.mybatis.util.BoostUtils;

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
     * @param booster booster
     * @return {@link SqlHelper} SQL助手实例
     */
    public static <T, R> SqlHelper<T> of(Booster<T, R> booster) {
        return SqlHelper.of(BoostUtils.getEntityClass(booster));
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
        child.connector = SqlKeyword.OR.getKeyword();
        sqlHelper.accept(child);
        this.addChild(child);
        return this;
    }

    /**
     * 创建boostCore对应的SQL助手实例
     *
     * @param boostCore 查询核心类
     * @return {@link BoostSqlHelper} boostCore对应的SQL助手实例
     */
    public <V, P> BoostSqlHelper<T, V, P> boost(BoostCore<T, V, P> boostCore) {
        return new BoostSqlHelper<>(boostCore);
    }

}