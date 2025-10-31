package io.github.luminion.mybatisplus.enhancer.query.core;

import java.util.Collection;
import java.util.Map;

/**
 * SQL实体接口
 * <p>
 * 扩展自ISqlTree接口，增加了排序功能的定义
 *
 * @author luminion
 */
public interface ISqlEntity<T> extends ISqlTree {

    /**
     * 获取排序字段列表
     *
     * @return 排序字段列表
     */
    Collection<ISqlSort> getSorts();

    /**
     * 获取未映射的额外字段Map
     *
     * @return {@link Map }<{@link String },{@link Object }>
     */
    Map<String,Object> getExtra();

}