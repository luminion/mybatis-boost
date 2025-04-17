package io.github.bootystar.mybatisplus.enhancer.core.support;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.builder.ExtraFieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable.DynamicFieldSqlHelper;
import io.github.bootystar.mybatisplus.enhancer.util.CastHelper;

import java.util.List;
import java.util.Objects;

/**
 * @author bootystar
 */
public interface DynamicFieldService<V> extends EnhancedQuery<V> {

    default ExtraFieldSuffixBuilder getSuffixBuilder() {
        return null;
    }

    @Override
    @SuppressWarnings({"unchecked", "rawtypes"})
    default List<V> doSelectVO(Object s, IPage<V> page) {
        DynamicFieldSqlHelper<?> sqlHelper;
        IService iService = CastHelper.cast(this, IService.class);
        DynamicMapper dynamicMapper = CastHelper.cast(iService.getBaseMapper(), DynamicMapper.class);
        if (!Objects.equals(dynamicMapper.getVOClass(), getVOClass())) {
            throw new IllegalStateException("baseMapper has different vo class: " + dynamicMapper.getVOClass().getName());
        }
        if (s instanceof DynamicFieldSqlHelper<?>) {
            DynamicFieldSqlHelper<?> unmodifiableSqlHelper = (DynamicFieldSqlHelper<?>) s;
            if (!iService.getEntityClass().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (DynamicFieldSqlHelper<?>) s;
        } else {
            sqlHelper = new DynamicFieldSqlHelper<>(SqlHelper.of(s), iService.getEntityClass(), getSuffixBuilder());
        }
        return dynamicMapper.doSelectVO(sqlHelper, page);
    }

}
