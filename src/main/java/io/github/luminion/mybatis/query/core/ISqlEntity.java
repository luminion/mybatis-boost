package io.github.luminion.mybatis.query.core;

import java.util.Collection;
import java.util.Map;

/**
 * SQL 实体接口.
 * <p>
 * 扩展自 {@link ISqlTree} 接口, 增加了排序和额外查询条件的功能.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
public interface ISqlEntity<T> extends ISqlTree {

    /**
     * 获取排序规则列表.
     *
     * @return 排序规则列表
     * @since 1.0.0
     */
    Collection<ISqlSort> getSorts();

    /**
     * 获取不属于实体本身的额外查询条件.
     * <p>
     * key 为条件名, value 为条件值.
     *
     * @return 额外查询条件 Map
     * @since 1.0.0
     */
    Map<String,Object> getExtra();

}