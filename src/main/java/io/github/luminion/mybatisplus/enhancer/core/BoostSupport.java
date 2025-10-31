package io.github.luminion.mybatisplus.enhancer.core;

import io.github.luminion.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.luminion.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.luminion.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.luminion.mybatisplus.enhancer.query.helper.SqlHelper;
import io.github.luminion.mybatisplus.enhancer.util.BoostUtils;
import io.github.luminion.mybatisplus.enhancer.util.ReflectUtil;
import org.apache.ibatis.exceptions.TooManyResultsException;
import org.springframework.util.ObjectUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 定义vo层对象
 *
 * @author luminion
 */
public interface BoostSupport<T, V> extends BoostCore<T, V> {

    @Override
    default T toEntity(Object source) {
        return ReflectUtil.toTarget(source, BoostUtils.getEntityClass(this));
    }

    @Override
    default V toVo(Object source) {
        return ReflectUtil.toTarget(source, BoostUtils.getViewObjectClass(this));
    }

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
        SqlCondition condition = new SqlCondition(keyProperty, SqlKeyword.EQ.keyword, id);
        SqlHelper<T> sqlHelper = SqlHelper.of(this).with(condition);
        return voUnique(sqlHelper);
    }

    @Override
    default <R> R voById(Serializable id, Class<R> voType) {
        V v = voById(id);
        if (ObjectUtils.isEmpty(v)) {
            return null;
        }
        return ReflectUtil.toTarget(v, voType);
    }

    @Override
    default Optional<V> voByIdOpt(Serializable id) {
        return Optional.ofNullable(voById(id));
    }

    @Override
    default <R> Optional<R> voByIdOpt(Serializable id, Class<R> voType) {
        return Optional.ofNullable(voById(id, voType));
    }

    @Override
    default List<V> voListByIds(Collection<? extends Serializable> ids) {
        Class<T> entityClass = BoostUtils.getEntityClass(this);
        SFunction<T, Object> idPropertyGetter = BoostUtils.getIdPropertyGetter(entityClass);
        SqlHelper<T> sqlHelper = SqlHelper.of(this).in(idPropertyGetter, ids);
        return voList(sqlHelper);
    }

    @Override
    default <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> voType) {
        List<V> vs = voListByIds(ids);
        return vs.stream()
                .map(v -> ReflectUtil.toTarget(v, voType))
                .collect(Collectors.toList());
    }

    @Override
    default V voFirst(ISqlEntity<T> criteria) {
        List<V> vs = voList(criteria);
        if (vs.isEmpty()) {
            return null;
        }
        return vs.get(0);
    }

    @Override
    default <R> R voFirst(ISqlEntity<T> criteria, Class<R> voType) {
        return ReflectUtil.toTarget(voFirst(criteria), voType);
    }

    @Override
    default Optional<V> voFirstOpt(ISqlEntity<T> criteria) {
        return Optional.ofNullable(voFirst(criteria));
    }

    @Override
    default <R> Optional<R> voFirstOpt(ISqlEntity<T> criteria, Class<R> voType) {
        return Optional.ofNullable(voFirst(criteria, voType));
    }

    @Override
    default V voUnique(ISqlEntity<T> criteria) throws TooManyResultsException {
        List<V> vs = voList(criteria);
        if (vs.isEmpty()) {
            return null;
        }
        if (vs.size() >1){
            throw new TooManyResultsException("error query => expected one but found " + vs.size());
        }
        return vs.get(0);
    }

    @Override
    default <R> R voUnique(ISqlEntity<T> criteria, Class<R> voType) throws TooManyResultsException {
        return ReflectUtil.toTarget(voUnique(criteria), voType);
    }

    @Override
    default Optional<V> voUniqueOpt(ISqlEntity<T> criteria) throws TooManyResultsException {
        return Optional.ofNullable(voUnique(criteria));
    }

    @Override
    default <R> Optional<R> voUniqueOpt(ISqlEntity<T> criteria, Class<R> voType) {
        return Optional.ofNullable(voUnique(criteria, voType));
    }
    
    @Override
    default List<V> voList() {
        return voList(null);
    }

//    @Override
//    default List<V> voList(ISqlEntity<T> criteria) {
//        return new ArrayList<>();
//    }

    @Override
    default <R> List<R> voList(ISqlEntity<T> criteria, Class<R> voType) {
        List<V> vs = voList(criteria);
        return vs.stream()
                .map(v -> ReflectUtil.toTarget(v, voType))
                .collect(Collectors.toList());
    }
}
