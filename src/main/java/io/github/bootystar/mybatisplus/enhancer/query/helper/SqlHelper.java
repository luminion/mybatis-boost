package io.github.bootystar.mybatisplus.enhancer.query.helper;

import java.util.function.Consumer;

/**
 * sql助手
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends AbstractSqlHelper<T, SqlHelper<T>> {

    public static <T> SqlHelper<T> of(Class<T> clazz) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = clazz;
        return sqlHelper;
    }

    @Override
    public SqlHelper<T> or(Consumer<SqlHelper<T>> consumer) {
        SqlHelper<T> child = new SqlHelper<>();
        consumer.accept(child);
        this.addChild(child);
        return this;
    }

}
