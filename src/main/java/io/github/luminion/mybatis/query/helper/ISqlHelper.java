package io.github.luminion.mybatis.query.helper;

import io.github.luminion.mybatis.query.core.ISqlEntity;

import java.util.function.Function;

/**
 * SQL 构建助手接口.
 * <p>
 * 定义了 SQL 构建助手的基本功能, 包括获取实体类、应用处理器等.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
public interface ISqlHelper<T> extends ISqlEntity<T> {

    /**
     * 获取与此 SQL 助手关联的实体类.
     *
     * @return 实体类
     * @since 1.0.0
     */
    Class<T> getEntityClass();
    
    /**
     * 应用一个处理器对当前的 SQL 助手进行转换或处理.
     *
     * @param processor 处理器函数
     * @return 处理后的 {@link ISqlHelper}
     * @since 1.0.0
     */
    default ISqlHelper<T> process(Function<ISqlHelper<T>,ISqlHelper<T>> processor){
        return processor.apply(this);
    }
    
}