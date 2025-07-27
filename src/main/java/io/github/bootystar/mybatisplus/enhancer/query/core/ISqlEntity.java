package io.github.bootystar.mybatisplus.enhancer.query.core;

import java.util.Collection;
import java.util.Map;

/**
 * @author bootystar
 */
public interface ISqlEntity<T> extends ISqlTree{
    Collection<ISqlSort> getSorts();

    Class<T> getEntityClass();

    Map<String, Object> getMap();

}
