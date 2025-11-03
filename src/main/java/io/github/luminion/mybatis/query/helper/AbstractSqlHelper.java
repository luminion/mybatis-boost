package io.github.luminion.mybatis.query.helper;

import io.github.luminion.mybatis.query.core.ISqlCondition;
import io.github.luminion.mybatis.query.core.ISqlSort;
import io.github.luminion.mybatis.query.core.ISqlTree;
import io.github.luminion.mybatis.query.entity.SqlCondition;
import io.github.luminion.mybatis.query.entity.SqlEntity;
import io.github.luminion.mybatis.util.ReflectUtils;
import lombok.Getter;

import java.util.Map;

/**
 * SQL助手抽象基类
 * <p>
 * 提供SQL构建的基本功能，包括条件添加、排序设置等，使用泛型支持链式调用
 *
 * @param <T> 实体类型
 * @param <S> 返回类型（用于支持链式调用）
 * @author luminion
 */
@Getter
@SuppressWarnings({"unused", "unchecked"})
public abstract class AbstractSqlHelper<T, S extends AbstractSqlHelper<T, S>> extends SqlEntity<T> implements ISqlHelperLambda<T, S> {

    /**
     * 对应实体类，用于SQL校验/处理
     */
    protected transient Class<T> entityClass;
    
    /**
     * 根据指定对象字段映射条件
     * <p>
     * 当对象为{@link ISqlTree}时，将对象作为子条件添加<br>
     * 当对象为{@link ISqlCondition}时，将对象作为条件添加<br>
     * 当对象为{@link ISqlSort}时，将对象作为排序添加<br>
     * 当对象为其他类型时，将对象字段映射为k,v，作为条件添加
     *
     * @param s 任意对象
     * @return {@link SqlHelper} SQL助手实例
     */
    @SuppressWarnings("unchecked")
    public S with(Object s) {
        if (s == null) {
            return (S) this;
        }
        if (s instanceof ISqlTree) {
            return (S) super.addChild((ISqlTree) s);
        }
        if (s instanceof ISqlCondition) {
            this.getConditions().add(((ISqlCondition) s));
            return (S) this;
        }
        if (s instanceof ISqlSort) {
            this.getSorts().add(((ISqlSort) s));
            return (S) this;
        }
        Map<?, ?> map = ReflectUtils.objectToMap(s);
        for (Map.Entry<?, ?> next : map.entrySet()) {
            Object key = next.getKey();
            Object value = next.getValue();
            SqlCondition condition = new SqlCondition(key.toString(), value);
            this.getConditions().add(condition);
        }
        return (S) this;
    }

}