package io.github.bootystar.mybatisplus.enhancer.core;

import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelperWrapper;

/**
 * @author bootystar
 */
public interface DynamicService<T, V> extends EnhancedIService<T>, DynamicQueryService<V> {

    default SqlHelperWrapper<T, V> lambdaHelper() {
        return new SqlHelperWrapper<>(this);
    }
}
