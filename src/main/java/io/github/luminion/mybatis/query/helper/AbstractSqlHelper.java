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
public abstract class AbstractSqlHelper<T, S extends AbstractSqlHelper<T, S>> extends SqlEntity<T>
        implements ILambdaSqlHelper<T, S> {

    /**
     * 关联的实体类, 用于 SQL 校验和处理.
     */
    protected transient Class<T> entityClass;

    /**
     * 合并指定条件树的条件
     *
     * @param tree 条件树
     * @return 当前 {@link AbstractSqlHelper} 实例
     * @since 1.0.0
     */
    public S merge(ISqlTree tree) {
        if (tree != null) {
            super.addChild(tree);
        }
        return (S) this;
    }

    /**
     * 合并一个查询条件
     *
     * @param condition 查询条件
     * @return 当前 {@link AbstractSqlHelper} 实例
     * @since 1.0.0
     */
    public S merge(ISqlCondition condition) {
        if (condition != null) {
            this.getConditions().add(condition);
        }
        return (S) this;
    }

    /**
     * 合并一个排序规则
     *
     * @param sort 排序规则
     * @return 当前 {@link AbstractSqlHelper} 实例
     * @since 1.0.0
     */
    public S merge(ISqlSort sort) {
        if (sort != null) {
            this.getSorts().add(sort);
        }
        return (S) this;
    }

    /**
     * 从一个Map对象加载查询条件.
     * <p>
     * 会将Map中的所有键值对映射为等值查询条件.
     *
     * @param map 包含查询条件的Map对象
     * @return 当前 {@link AbstractSqlHelper} 实例
     * @since 1.0.0
     */
    public <K, V> S merge(Map<K, V> map) {
        for (Map.Entry<K, V> entry : map.entrySet()) {
            K key = entry.getKey();
            V value = entry.getValue();
            SqlCondition condition = new SqlCondition(key.toString(), value);
            this.getConditions().add(condition);
        }
        return (S) this;
    }

    /**
     * 从一个普通对象(如DTO)加载查询条件.
     * <p>
     * 会通过反射将对象的非空字段映射为等值查询条件.
     *
     * @param dto 包含查询条件的对象
     * @return 当前 {@link AbstractSqlHelper} 实例
     * @since 1.0.0
     */
    public S mergeObject(Object dto) {
        if (dto == null) {
            return (S) this;
        }
        Map<?, ?> map = ReflectUtils.objectToMap(dto);
        return merge(map);
    }

}