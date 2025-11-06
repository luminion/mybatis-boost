package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.api.Wrapper;
import io.github.luminion.sqlbooster.model.sql.SqlCondition;
import io.github.luminion.sqlbooster.model.sql.helper.SqlHelperBooster;
import io.github.luminion.sqlbooster.model.sql.helper.BaseHelper;
import io.github.luminion.sqlbooster.model.sql.helper.SqlHelper;
import io.github.luminion.sqlbooster.model.sql.helper.processor.FieldSuffixProcessor;
import io.github.luminion.sqlbooster.util.BoostUtils;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import org.apache.ibatis.exceptions.TooManyResultsException;
import org.springframework.util.ObjectUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Boost 核心引擎，提供 VO 查询能力的默认实现.
 *
 * @param <T> 数据库实体的类型。
 * @param <V> 要返回的视图对象 (VO) 的类型。
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterEngine<T, V> extends BoosterCore<T, V> {

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default T toEntity(Object source) {
        return ReflectUtils.toTarget(source, BoostUtils.getEntityClass(this));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default V toVo(Object source) {
        return ReflectUtils.toTarget(source, BoostUtils.getViewObjectClass(this));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default void voPreProcess(Wrapper<T> wrapper) {
        // do nothing here, only for override
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default void voPostProcess(List<V> records, Wrapper<T> wrapper, Page<V> page) {
        // do nothing here, only for override
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default V voById(Serializable id) {
        if (ObjectUtils.isEmpty(id)) {
            throw new IllegalArgumentException("id can't be null");
        }
        Class<T> clazz = BoostUtils.getEntityClass(this);
        String keyProperty = BoostUtils.getIdPropertyName(clazz);
        if (ObjectUtils.isEmpty(keyProperty)) {
            throw new IllegalArgumentException("can't find id property");
        }
        SqlCondition condition = new SqlCondition(keyProperty, SqlKeyword.EQ.getKeyword(), id);
        SqlHelper<T> sqlHelper = SqlHelper.of(this).merge(condition);
        return voUnique(sqlHelper);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> R voById(Serializable id, Class<R> targetType) {
        V v = voById(id);
        if (ObjectUtils.isEmpty(v)) {
            return null;
        }
        return ReflectUtils.toTarget(v, targetType);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default Optional<V> voByIdOpt(Serializable id) {
        return Optional.ofNullable(voById(id));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voByIdOpt(Serializable id, Class<R> targetType) {
        return Optional.ofNullable(voById(id, targetType));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default List<V> voListByIds(Collection<? extends Serializable> ids) {
        Class<T> entityClass = BoostUtils.getEntityClass(this);
        String idPropertyName = BoostUtils.getIdPropertyName(entityClass);
        SqlCondition sqlCondition = new SqlCondition(idPropertyName, SqlKeyword.IN.getKeyword(), ids);
        SqlHelper<T> sqlHelper = SqlHelper.of(this).merge(sqlCondition);
        return voList(sqlHelper);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> targetType) {
        List<V> vs = voListByIds(ids);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default V voFirst(Wrapper<T> wrapper) {
        List<V> vs = voList(wrapper);
        if (vs.isEmpty()) {
            return null;
        }
        return vs.get(0);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> R voFirst(Wrapper<T> wrapper, Class<R> targetType) {
        return ReflectUtils.toTarget(voFirst(wrapper), targetType);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default Optional<V> voFirstOpt(Wrapper<T> wrapper) {
        return Optional.ofNullable(voFirst(wrapper));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voFirstOpt(Wrapper<T> wrapper, Class<R> targetType) {
        return Optional.ofNullable(voFirst(wrapper, targetType));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default V voUnique(Wrapper<T> wrapper) {
        List<V> vs = voList(wrapper);
        if (vs.isEmpty()) {
            return null;
        }
        if (vs.size() > 1) {
            throw new TooManyResultsException("error query => expected one but found " + vs.size());
        }
        return vs.get(0);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> R voUnique(Wrapper<T> wrapper, Class<R> targetType) {
        return ReflectUtils.toTarget(voUnique(wrapper), targetType);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default Optional<V> voUniqueOpt(Wrapper<T> wrapper) {
        return Optional.ofNullable(voUnique(wrapper));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voUniqueOpt(Wrapper<T> wrapper, Class<R> targetType) {
        return Optional.ofNullable(voUnique(wrapper, targetType));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default List<V> voList() {
        return voList(null);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default List<V> voList(Wrapper<T> wrapper) {
        voPreProcess(wrapper);

        BaseHelper<T> sqlHelper = SqlHelper.of(wrapper).entity(this).process(FieldSuffixProcessor.of()::process);
        List<V> vs = selectByWrapper(sqlHelper, null);

        voPostProcess(vs, sqlHelper, null);
        return vs;
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> List<R> voList(Wrapper<T> wrapper, Class<R> targetType) {
        List<V> vs = voList(wrapper);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default Page<V> voPage(Wrapper<T> wrapper, int pageNum, int pageSize) {
        return voPage(wrapper, (long) pageNum, pageSize);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default Page<V> voPage(Wrapper<T> wrapper, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> Page<R> voPage(Wrapper<T> wrapper, int pageNum, int pageSize, Class<R> targetType) {
        return voPage(wrapper, (long) pageNum, pageSize, targetType);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> Page<R> voPage(Wrapper<T> wrapper, long pageNum, long pageSize, Class<R> targetType) {
        return voPage(wrapper, pageNum, pageSize).convertRecords(targetType);
    }

    /**
     * 获取 Lambda SQL 助手.
     *
     * @return SQL 助手
     * @since 1.0.0
     */
    default SqlHelperBooster<T, V> lambdaHelper() {
        return new SqlHelperBooster<>(this);
    }

    /**
     * 最终执行查询的方法.
     *
     * @param wrapper 查询条件
     * @param page    分页对象
     * @return 查询结果列表
     * @since 1.0.0
     */
    List<V> selectByWrapper(Wrapper<T> wrapper, Object page);
}
