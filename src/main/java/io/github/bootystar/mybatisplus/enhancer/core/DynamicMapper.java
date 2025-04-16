package io.github.bootystar.mybatisplus.enhancer.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;

import java.util.List;

/**
 * mapper
 *
 * @author bootystar
 */
public interface DynamicMapper<V> extends EnhancedQuery<V>{

    List<V> voSelect(Object s, IPage<V> page);

}
