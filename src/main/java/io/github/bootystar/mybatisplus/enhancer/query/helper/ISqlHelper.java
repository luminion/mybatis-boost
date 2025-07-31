package io.github.bootystar.mybatisplus.enhancer.query.helper;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;

import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author bootystar
 */
public interface ISqlHelper<T> extends ISqlEntity {

    Class<T> getEntityClass();

    Map<String, Object> getUnmapped();
    
    default ISqlHelper<T> process(Function<ISqlHelper<T>,ISqlHelper<T>> processor){
        return processor.apply(this);
    }
    
}
