package io.github.luminion.mybatisplus.enhancer.query.entity;

import io.github.luminion.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.luminion.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.luminion.mybatisplus.enhancer.query.core.ISqlTree;
import lombok.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 可排序的条件树实体类
 * <p>
 * 扩展SqlTree类，增加了排序功能，用于表示包含排序信息的SQL条件树
 *
 * @author luminion
 */
@Getter
@ToString
@EqualsAndHashCode(callSuper = true)
public class SqlEntity<T> extends SqlTree implements ISqlEntity<T> {

    /**
     * 排序字段列表
     */
    protected Collection<ISqlSort> sorts;
    
    /**
     * 映射map，无法自动映射的字段会存放到该map中
     */
    protected transient Map<String, Object> extraParams;

    {
        this.sorts = new LinkedHashSet<>();
        this.extraParams = new HashMap<>();
    }

    /**
     * 添加子节点
     *
     * @param sqlTree SQL树
     * @return {@link SqlTree} 当前实例
     */
    @Override
    protected SqlTree addChild(ISqlTree sqlTree) {
        if (sqlTree==null){
            return this;
        }
        if (sqlTree instanceof SqlEntity){
            SqlEntity sqlEntity = (SqlEntity) sqlTree;
            this.sorts.addAll(sqlEntity.getSorts());
        }
        return super.addChild(sqlTree);
    }
}