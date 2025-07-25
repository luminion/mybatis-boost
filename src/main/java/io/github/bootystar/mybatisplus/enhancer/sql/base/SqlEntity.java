package io.github.bootystar.mybatisplus.enhancer.sql.base;

import java.util.Collection;
import java.util.Map;

/**
 * @author bootystar
 */
public interface SqlEntity extends SqlConditionTree {

    /**
     * 排序条件列表
     */
    Collection<? extends SqlSort> getSorts();

    /**
     * 未映射的字段
     */
    Map<String,Object> getMap();

}
