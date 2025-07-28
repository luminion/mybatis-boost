package io.github.bootystar.mybatisplus.enhancer.query.helper;

import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlTree;

import java.util.function.Consumer;

/**
 * sql助手
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends AbstractSqlHelper<T, SqlHelper<T>> {

    @Override
    public SqlHelper<T> or(Consumer<SqlHelper<T>> consumer) {
        SqlHelper<T> child = new SqlHelper<>();
        consumer.accept(child);
        SqlTree current = this;
        while (current.getChild()!=null){
            current = current.getChild();
        }
        child.child=child;
        return this;
    }
    
}
