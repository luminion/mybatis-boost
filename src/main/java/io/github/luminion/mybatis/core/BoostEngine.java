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
 * @param <P> 分页类
 * @author luminion
 * @since 1.0.0
 */
public interface BoostEngine<T, V, P> extends BoostCore<T, V, P> {

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default T toEntity(Object source) {
        return ReflectUtils.toTarget(source, BoostUtils.getEntityClass(this));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default V toVo(Object source) {
        return ReflectUtils.toTarget(source, BoostUtils.getViewObjectClass(this));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default void voPreProcess(ISqlEntity<T> params, P page) {
        // do nothing here, only for override
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default void voPostProcess(List<V> records, ISqlEntity<T> params, P page) {
        // do nothing here, only for override
    }

    /**
     * {@inheritDoc}
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
        SqlHelper<T> sqlHelper = SqlHelper.of(this).with(condition);
        return voUnique(sqlHelper);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> R voById(Serializable id, Class<R> voType) {
        V v = voById(id);
        if (ObjectUtils.isEmpty(v)) {
            return null;
        }
        return ReflectUtils.toTarget(v, voType);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default Optional<V> voByIdOpt(Serializable id) {
        return Optional.ofNullable(voById(id));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voByIdOpt(Serializable id, Class<R> voType) {
        return Optional.ofNullable(voById(id, voType));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default List<V> voListByIds(Collection<? extends Serializable> ids) {
        Class<T> entityClass = BoostUtils.getEntityClass(this);
        String idPropertyName = BoostUtils.getIdPropertyName(entityClass);
        SqlCondition sqlCondition = new SqlCondition(idPropertyName, SqlKeyword.IN.getKeyword(), ids);
        SqlHelper<T> sqlHelper = SqlHelper.of(this).with(sqlCondition);
        return voList(sqlHelper);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> voType) {
        List<V> vs = voListByIds(ids);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, voType))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default V voFirst(ISqlEntity<T> params) {
        List<V> vs = voList(params);
        if (vs.isEmpty()) {
            return null;
        }
        return vs.get(0);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> R voFirst(ISqlEntity<T> params, Class<R> voType) {
        return ReflectUtils.toTarget(voFirst(params), voType);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default Optional<V> voFirstOpt(ISqlEntity<T> params) {
        return Optional.ofNullable(voFirst(params));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voFirstOpt(ISqlEntity<T> params, Class<R> voType) {
        return Optional.ofNullable(voFirst(params, voType));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default V voUnique(ISqlEntity<T> params) throws TooManyResultsException {
        List<V> vs = voList(params);
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
     * @since 1.0.0
     */
    @Override
    default <R> R voUnique(ISqlEntity<T> params, Class<R> voType) throws TooManyResultsException {
        return ReflectUtils.toTarget(voUnique(params), voType);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default Optional<V> voUniqueOpt(ISqlEntity<T> params) throws TooManyResultsException {
        return Optional.ofNullable(voUnique(params));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> Optional<R> voUniqueOpt(ISqlEntity<T> params, Class<R> voType) {
        return Optional.ofNullable(voUnique(params, voType));
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default List<V> voList() {
        return voList(null);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default List<V> voList(ISqlEntity<T> params) {
        voPreProcess(params, null);
        FieldSuffixProcessor fieldSuffixProcessor = FieldSuffixProcessor.of();
        ISqlHelper<T> sqlHelper = SqlHelper.of(this)
                .with(params)
                .process(fieldSuffixProcessor::process);
        List<V> vs = selectBySqlEntity(sqlHelper, null);
        voPostProcess(vs, params, null);
        return vs;
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> List<R> voList(ISqlEntity<T> params, Class<R> voType) {
        List<V> vs = voList(params);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, voType))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default P voPage(ISqlEntity<T> params, int pageNum, int pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> P voPage(ISqlEntity<T> params, int pageNum, int pageSize, Class<R> voType) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default P voPage(ISqlEntity<T> params, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> P voPage(ISqlEntity<T> params, long pageNum, long pageSize, Class<R> voType) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    /**
     * 获取 Lambda SQL 助手.
     *
     * @return SQL 助手
     * @since 1.0.0
     */
    default BoostSqlHelper<T, V, P> lambdaHelper() {
        return new BoostSqlHelper<>( this);
    }
    

    /**
     * 最终执行查询的方法.
     *
     * @param params 查询条件
     * @param page   分页对象
     * @return 查询结果列表
     * @since 1.0.0
     */
    List<V> selectBySqlEntity(ISqlEntity<T> params, P page);
}
