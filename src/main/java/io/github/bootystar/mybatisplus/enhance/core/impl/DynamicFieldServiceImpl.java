package io.github.bootystar.mybatisplus.enhance.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhance.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicFieldSqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.UnmodifiableSqlHelper;

import java.util.List;


/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class DynamicFieldServiceImpl<M extends DynamicMapper<T, V, UnmodifiableSqlHelper<T>>, T, V> extends ServiceImpl<M, T> implements DynamicService<T, V> {

    protected FieldSuffixBuilder suffixBuilder;

    {
        suffixBuilder = initSuffixBuilder();
    }

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
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

    protected FieldSuffixBuilder initSuffixBuilder() {
        return null;
    }

}
