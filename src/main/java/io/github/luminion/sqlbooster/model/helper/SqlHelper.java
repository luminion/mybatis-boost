package io.github.luminion.sqlbooster.model.helper;

import io.github.luminion.sqlbooster.core.BoosterCore;
import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.api.ISqlEntity;
import io.github.luminion.sqlbooster.util.BoostUtils;

import java.util.function.Consumer;

/**
 * SQL 构建助手实现类.
 * <p>
 * 提供 SQL 查询构建的具体实现, 支持链式调用和 Lambda 表达式.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends AbstractSqlHelper<T, SqlHelper<T>> {

    /**
     * 创建一个新的 {@link SqlHelper} 实例.
     *
     * @param <T> 实体类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T> SqlHelper<T> of() {
        return new SqlHelper<>();
    }

    /**
     * 创建一个与指定实体类绑定的 {@link SqlHelper} 实例.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T> SqlHelper<T> of(Class<T> clazz) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = clazz;
        return sqlHelper;
    }

    /**
     * 创建一个指定 {@link ISqlEntity}的sqlhelper
     *
     * @param sqlEntity 源 {@link ISqlEntity} 实例
     * @param <T>       实体类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T> SqlHelper<T> of(ISqlEntity<T> sqlEntity) {
        if (sqlEntity == null) {
            return new SqlHelper<>();
        }
        if (sqlEntity instanceof SqlHelper) {
            return (SqlHelper<T>) sqlEntity;
        }
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        return sqlHelper.merge(sqlEntity);
    }

    /**
     * 创建一个与 {@link Booster} 实例的实体类型绑定的 {@link SqlHelper} 实例.
     *
     * @param booster Booster 实例
     * @param <T>     实体类型
     * @param <R>     VO 类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T, R> SqlHelper<T> of(Booster<T, R> booster) {
        return SqlHelper.of(BoostUtils.getEntityClass(booster));
    }

    /**
     * 添加一组 OR 连接的条件.
     *
     * @param sqlHelper 用于构建 OR 条件的 Consumer
     * @return 当前 {@link SqlHelper} 实例
     * @since 1.0.0
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
     * 设置实体类
     *
     * @param entityClass 实体类
     * @return 当前 {@link SqlHelper} 实例
     * @since 1.0.0
     */
    @SuppressWarnings("unchecked")
    public <R> SqlHelper<R> entity(Class<R> entityClass) {
        SqlHelper<R> sqlHelper = (SqlHelper<R>) this;
        sqlHelper.entityClass = entityClass;
        return sqlHelper;
    }

    /**
     * 设置实体类
     *
     * @param booster Booster 实例
     * @return 当前 {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public <V> SqlHelper<T> entity(Booster<T, V> booster) {
        this.entityClass = BoostUtils.getEntityClass(booster);
        return this;
    }

    /**
     * 将当前 {@link SqlHelper} 转换为 {@link BoostSqlHelper}.
     *
     * @param boosterCore {@link BoosterCore} 实例
     * @param <V>       VO 类型
     * @param <P>       分页对象类型
     * @return {@link BoostSqlHelper} 实例
     * @since 1.0.0
     */
    public <V, P> BoostSqlHelper<T, V> boost(BoosterCore<T, V> boosterCore) {
        this.entityClass = BoostUtils.getEntityClass(boosterCore);
        return new BoostSqlHelper<>(boosterCore);
    }

}