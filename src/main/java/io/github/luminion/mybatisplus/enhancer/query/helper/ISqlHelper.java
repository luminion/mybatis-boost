package io.github.luminion.mybatisplus.enhancer.query.helper;

import io.github.luminion.mybatisplus.enhancer.query.core.ISqlEntity;

import java.util.function.Function;

/**
 * SQL助手接口
 * <p>
 * 定义SQL助手的基本功能，包括获取实体类、未映射参数等
 *
 * @param <T> 实体类型
 * @author luminion
 */
public interface ISqlHelper<T> extends ISqlEntity<T> {

    /**
     * 获取实体类
     *
     * @return 实体类
     */
    Class<T> getEntityClass();
    
    /**
     * 处理SQL助手
     *
     * @param processor 处理函数
     * @return {@link ISqlHelper} 处理后的SQL助手
     */
    default ISqlHelper<T> process(Function<ISqlHelper<T>,ISqlHelper<T>> processor){
        return processor.apply(this);
    }
    
}