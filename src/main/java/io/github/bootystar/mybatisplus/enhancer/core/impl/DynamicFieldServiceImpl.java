package io.github.bootystar.mybatisplus.enhancer.core.impl;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhancer.builder.ExtraFieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicService;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable.DynamicFieldSqlHelper;

import java.util.List;

/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class DynamicFieldServiceImpl<M extends BaseMapper<T> & DynamicMapper<V>, T, V> extends ServiceImpl<M, T> implements DynamicService<T, V> {

    protected ExtraFieldSuffixBuilder suffixBuilder;

    @Override
    @SuppressWarnings("unchecked")
    public List<V> voSelect(Object s, IPage<V> page) {
        DynamicFieldSqlHelper<T> sqlHelper;
        if (s instanceof DynamicFieldSqlHelper<?>) {
            DynamicFieldSqlHelper<?> unmodifiableSqlHelper = (DynamicFieldSqlHelper<?>) s;
            if (!super.getEntityClass().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (DynamicFieldSqlHelper<T>) s;
        } else {
            sqlHelper = new DynamicFieldSqlHelper<>(SqlHelper.of(s), super.getEntityClass(), suffixBuilder);
        }
        return getBaseMapper().voSelect(sqlHelper, page);
    }

}
