package io.github.luminion.mybatis.query.core;

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
     * 非本表字段的条件(key=条件名,value=条件值)
     *
     * @return 非本表字段的条件map
     */
    Map<String,Object> getExtra();

}