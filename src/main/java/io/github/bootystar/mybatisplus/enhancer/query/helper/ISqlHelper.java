package io.github.bootystar.mybatisplus.enhancer.query.helper;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;

import java.util.function.Consumer;

/**
 * @author bootystar
 */
public interface ISqlHelper<T> extends ISqlEntity<T> {
    
    default ISqlHelper<T> process(Consumer<ISqlHelper<T>> processor){
        processor.accept(this);
        return this;
    }
    
}
