package io.github.bootystar.mybatisplus.enhancer.core.impl;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicService;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable.DynamicSqlSqlHelper;

import java.util.List;

/**
 * 动态查询实现
 *
 * @author bootystar
 */
public abstract class DynamicSqlServiceImpl<M extends BaseMapper<T> & DynamicMapper<V>, T, V> extends ServiceImpl<M, T> implements DynamicService<V> {

    @Override
    @SuppressWarnings("unchecked")
    public List<V> doSelect(Object s, IPage<V> page) {
        DynamicSqlSqlHelper<T> sqlHelper;
        if (s instanceof DynamicSqlSqlHelper<?>) {
            DynamicSqlSqlHelper<?> unmodifiableSqlHelper = (DynamicSqlSqlHelper<?>) s;
            if (!super.getEntityClass().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (DynamicSqlSqlHelper<T>) s;
        } else {
            sqlHelper = new DynamicSqlSqlHelper<>(SqlHelper.of(s), super.getEntityClass());
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

}
