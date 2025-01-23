package io.github.bootystar.mybatisplus.enhance.core.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhance.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicSqlSqlHelper;

import java.util.List;

/**
 * 动态查询实现
 *
 * @author bootystar
 */
public abstract class DynamicSqlServiceImpl<M extends DynamicMapper<T, V>, T, V> extends ServiceImpl<M, T> implements DynamicService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public List<V> doSelect(Object param, IPage<V> page) {
        DynamicSqlSqlHelper<T> sqlHelper;
        if (param instanceof DynamicSqlSqlHelper<?>) {
            DynamicSqlSqlHelper<?> unmodifiableSqlHelper = (DynamicSqlSqlHelper<?>) param;
            if (!super.getEntityClass().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (DynamicSqlSqlHelper<T>) param;
        } else {
            sqlHelper = new DynamicSqlSqlHelper<>(SqlHelper.of(param), super.getEntityClass());
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

}
