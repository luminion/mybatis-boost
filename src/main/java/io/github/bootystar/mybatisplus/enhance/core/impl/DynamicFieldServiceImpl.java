package io.github.bootystar.mybatisplus.enhance.core.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhance.builder.ExtraFieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicFieldSqlHelper;

import java.util.List;

/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class DynamicFieldServiceImpl<M extends DynamicMapper<T, V>, T, V> extends ServiceImpl<M, T> implements DynamicService<T, V> {

    protected ExtraFieldSuffixBuilder suffixBuilder;

    @Override
    @SuppressWarnings("unchecked")
    public List<V> doSelect(Object s, IPage<V> page) {
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
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

}
