package io.github.bootystar.mybatisplus.enhancer.core;

import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedExcel;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedIService;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;

/**
 * @author bootystar
 */
public interface DynamicService<V> extends EnhancedQuery<V>, EnhancedExcel, EnhancedIService {

}
