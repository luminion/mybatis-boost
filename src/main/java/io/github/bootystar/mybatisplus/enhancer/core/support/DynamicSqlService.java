package io.github.bootystar.mybatisplus.enhancer.core.support;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable.DynamicSqlSqlHelper;
import io.github.bootystar.mybatisplus.enhancer.util.CastHelper;

import java.util.List;
import java.util.Objects;

/**
 * @author bootystar
 */
public interface DynamicSqlService<V> extends EnhancedQuery<V> {

    @Override
    @SuppressWarnings({"unchecked", "rawtypes"})
    default List<V> doSelectVO(Object s, IPage<V> page) {
        DynamicSqlSqlHelper sqlHelper;
        IService iService = CastHelper.cast(this, IService.class);
        DynamicMapper dynamicMapper = CastHelper.cast(iService.getBaseMapper(), DynamicMapper.class);
        if (!Objects.equals(dynamicMapper.getVOClass(), getVOClass())) {
            throw new IllegalStateException("baseMapper has different vo class: " + dynamicMapper.getVOClass().getName());
        }
        if (s instanceof DynamicSqlSqlHelper<?>) {
            DynamicSqlSqlHelper<?> unmodifiableSqlHelper = (DynamicSqlSqlHelper<?>) s;
            if (!iService.getEntityClass().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (DynamicSqlSqlHelper<?>) s;
        } else {
            sqlHelper = new DynamicSqlSqlHelper(SqlHelper.of(s), iService.getEntityClass());
        }
        return dynamicMapper.doSelectVO(sqlHelper, page);
    }

}
