package io.github.bootystar.mybatisplus.enhancer.query.helper;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlEntity;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;
import io.github.bootystar.mybatisplus.enhancer.util.ReflectUtil;

import java.util.Map;

/**
 * @author bootystar
 */
@SuppressWarnings({"unused", "unchecked"})
public abstract class AbstractSqlHelper<T, S extends AbstractSqlHelper<T, S>> extends SqlEntity<T> implements ISqlHelperLambda<T, S> {

    /**
     * 根据指定对象字段映射条件
     * 当对象为{@link ISqlTree}时, 将对象作为子条件添加
     * 当对象为{@link ISqlCondition}时, 将对象作为条件添加
     * 当对象为{@link ISqlSort}时, 将对象作为排序添加
     * 当对象为其他类型时, 将对象字段通过{@link MybatisPlusReflectUtil#objectToMap(Object)}映射为k,v, 作为条件添加
     *
     * @param s s
     * @return {@link SqlHelper}
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
        Map<?, ?> map = MybatisPlusReflectUtil.objectToMap(s);
        for (Map.Entry<?, ?> next : map.entrySet()) {
            Object key = next.getKey();
            Object value = next.getValue();
            SqlCondition condition = new SqlCondition(key.toString(), value);
            this.getConditions().add(condition);
        }
        return (S) this;
    }

}
