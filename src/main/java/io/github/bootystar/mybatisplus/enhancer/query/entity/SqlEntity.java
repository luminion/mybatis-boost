package io.github.bootystar.mybatisplus.enhancer.query.entity;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import lombok.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 可排序的条件树
 *
 * @author bootystar
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class SqlEntity<T> extends SqlTree implements ISqlEntity<T> {

    /**
     * 排序字段列表
     */
    protected Collection<ISqlSort> sorts;
    /**
     * 对用实体类, 用于Sql校验/处理
     */
    protected transient Class<T> entityClass;
    /**
     * 映射map, 无法自动映射的字段会存放到该map中
     */
    protected transient Map<String, Object> map;

    {
        this.sorts = new LinkedHashSet<>();
        this.map = new HashMap<>();
    }

}
