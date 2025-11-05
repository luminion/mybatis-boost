package io.github.luminion.mybatis.core;

import io.github.luminion.mybatis.enums.SqlKeyword;
import io.github.luminion.mybatis.query.core.ISqlEntity;
import io.github.luminion.mybatis.query.entity.SqlCondition;
import io.github.luminion.mybatis.query.helper.BoostSqlHelper;
import io.github.luminion.mybatis.query.helper.ISqlHelper;
import io.github.luminion.mybatis.query.helper.SqlHelper;
import io.github.luminion.mybatis.query.helper.processor.FieldSuffixProcessor;
import io.github.luminion.mybatis.util.BoostUtils;
import io.github.luminion.mybatis.util.ReflectUtils;
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
public interface BoostEngine<T, V> extends BoostCore<T, V> {

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
    default void voPreProcess(ISqlEntity<T> sqlEntity) {
        // do nothing here, only for override
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default void voPostProcess(List<V> records, ISqlEntity<T> sqlEntity, P<V> page) {
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
    default V voFirst(ISqlEntity<T> sqlEntity) {
        List<V> vs = voList(sqlEntity);
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
    default <R> R voFirst(ISqlEntity<T> sqlEntity, Class<R> targetType) {
        return ReflectUtils.toTarget(voFirst(sqlEntity), targetType);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default Optional<V> voFirstOpt(ISqlEntity<T> sqlEntity) {
        return Optional.ofNullable(voFirst(sqlEntity));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voFirstOpt(ISqlEntity<T> sqlEntity, Class<R> targetType) {
        return Optional.ofNullable(voFirst(sqlEntity, targetType));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default V voUnique(ISqlEntity<T> sqlEntity) {
        List<V> vs = voList(sqlEntity);
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
    default <R> R voUnique(ISqlEntity<T> sqlEntity, Class<R> targetType) {
        return ReflectUtils.toTarget(voUnique(sqlEntity), targetType);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default Optional<V> voUniqueOpt(ISqlEntity<T> sqlEntity) {
        return Optional.ofNullable(voUnique(sqlEntity));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voUniqueOpt(ISqlEntity<T> sqlEntity, Class<R> targetType) {
        return Optional.ofNullable(voUnique(sqlEntity, targetType));
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
    default List<V> voList(ISqlEntity<T> sqlEntity) {
        voPreProcess(sqlEntity);
        FieldSuffixProcessor fieldSuffixProcessor = FieldSuffixProcessor.of();
        ISqlHelper<T> sqlHelper = SqlHelper.of(sqlEntity)
                .entity(this)
                .process(fieldSuffixProcessor::process);
        List<V> vs = selectBySqlEntity(sqlHelper, null);
        voPostProcess(vs, sqlEntity, null);
        return vs;
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> List<R> voList(ISqlEntity<T> sqlEntity, Class<R> targetType) {
        List<V> vs = voList(sqlEntity);
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
    default P<V> voPage(ISqlEntity<T> sqlEntity, int pageNum, int pageSize) {
        return voPage(sqlEntity, (long) pageNum, pageSize);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default P<V> voPage(ISqlEntity<T> sqlEntity, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> P<R> voPage(ISqlEntity<T> sqlEntity, int pageNum, int pageSize, Class<R> targetType) {
        return voPage(sqlEntity, (long) pageNum, pageSize, targetType);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default <R> P<R> voPage(ISqlEntity<T> sqlEntity, long pageNum, long pageSize, Class<R> targetType) {
        return voPage(sqlEntity, pageNum, pageSize).convertRecords(targetType);
    }

    /**
     * 获取 Lambda SQL 助手.
     *
     * @return SQL 助手
     * @since 1.0.0
     */
    default BoostSqlHelper<T, V> lambdaHelper() {
        return new BoostSqlHelper<>(this);
    }

    /**
     * 最终执行查询的方法.
     *
     * @param sqlEntity 查询条件
     * @param page      分页对象
     * @return 查询结果列表
     * @since 1.0.0
     */
    List<V> selectBySqlEntity(ISqlEntity<T> sqlEntity, P<V> page);
}
