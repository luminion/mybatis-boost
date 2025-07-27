package io.github.bootystar.mybatisplus.enhancer;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;
import io.github.bootystar.mybatisplus.enhancer.util.SqlValidateUtil;

import java.util.List;

/**
 * mapper
 *
 * @author bootystar
 */
public interface DynamicMapper<V> extends EnhancedQuery<V> {

    List<V> voQueryByXml(ISqlEntity s, IPage<V> page);

    @Override
    default List<V> voQuery(Object param, IPage<V> page) {
        Class<?> entityClass = MybatisPlusReflectUtil.resolveTypeArguments(getClass(), BaseMapper.class)[0];
        return voQueryByXml(SqlHelper.of(entityClass).validate(SqlValidateUtil::validate), page);
    }
    
}
