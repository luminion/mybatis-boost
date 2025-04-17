package io.github.bootystar.mybatisplus.enhancer.core;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable.DynamicFieldSqlHelper;
import io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectHelper;

import java.util.List;

/**
 * mapper
 *
 * @author bootystar
 */
public interface DynamicMapper<T, V> extends BaseMapper<T>, EnhancedQuery<V> {

    List<V> selectVOList(UnmodifiableSqlHelper<T> s, IPage<V> page);

    @Override
    @SuppressWarnings("unchecked")
    default List<V> doSelectVO(Object s, IPage<V> page) {
        Class<T> aClass = (Class<T>) MybatisPlusReflectHelper.resolveTypeArguments(getClass(), BaseMapper.class)[0];
        return selectVOList(new DynamicFieldSqlHelper<>(SqlHelper.of(s), aClass), page);
    }
}
