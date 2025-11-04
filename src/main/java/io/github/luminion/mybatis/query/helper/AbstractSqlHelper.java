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
 * SQL 构建助手抽象基类.
 * <p>
 * 提供了 SQL 构建的基本功能, 包括条件添加、排序设置等, 并通过泛型支持链式调用.
 *
 * @param <T> 实体类型
 * @param <S> 返回类型 (用于支持链式调用)
 * @author luminion
 * @since 1.0.0
 */
@Getter
@SuppressWarnings({"unused", "unchecked"})
public abstract class AbstractSqlHelper<T, S extends AbstractSqlHelper<T, S>> extends SqlEntity<T> implements ISqlHelperLambda<T, S> {

    /**
     * 关联的实体类, 用于 SQL 校验和处理.
     */
    protected transient Class<T> entityClass;
    
    /**
     * 根据指定的对象添加条件或排序规则.
     * <p>
     * - 如果对象是 {@link ISqlTree}, 则将其作为子条件树添加.
     * - 如果对象是 {@link ISqlCondition}, 则将其作为单个条件添加.
     * - 如果对象是 {@link ISqlSort}, 则将其作为排序规则添加.
     * - 对于其他类型的对象, 会将其字段和值映射为等值条件添加.
     *
     * @param s 包含条件或排序信息的对象
     * @return 当前 {@link AbstractSqlHelper} 实例
     * @since 1.0.0
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