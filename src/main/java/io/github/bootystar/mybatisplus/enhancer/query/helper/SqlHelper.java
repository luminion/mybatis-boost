package io.github.bootystar.mybatisplus.enhancer.query.helper;

import java.util.function.Consumer;

/**
 * SQL助手实现类
 * <p>
 * 提供SQL构建的具体实现，支持链式调用和Lambda表达式
 *
 * @param <T> 实体类型
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends AbstractSqlHelper<T, SqlHelper<T>> {

    /**
     * 创建指定实体类型的SQL助手实例
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return {@link SqlHelper} SQL助手实例
     */
    public static <T> SqlHelper<T> of(Class<T> clazz) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = clazz;
        return sqlHelper;
    }

    /**
     * 添加OR条件
     *
     * @param consumer 拼装或条件的函数
     * @return {@link SqlHelper} 当前实例
     */
    @Override
    public SqlHelper<T> or(Consumer<SqlHelper<T>> consumer) {
        SqlHelper<T> child = new SqlHelper<>();
        consumer.accept(child);
        this.addChild(child);
        return this;
    }

}