package io.github.bootystar.mybatisplus.enhancer.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.sql.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlEntity;

import java.util.List;

/**
 * mapper
 *
 * @author bootystar
 */
public interface DynamicMapper<V> extends EnhancedQuery<V> {

    List<V> voSelectByXml(SqlEntity s, IPage<V> page);

    @Override
    default List<V> voSelect(Object param, IPage<V> page) {
        if (param == null) {
            return voSelectByXml(null, page);
        }
        if (param instanceof SqlEntity) {
            return voSelectByXml((SqlEntity) param, page);
        }
        return voSelectByXml(SqlHelper.of(param), page);
    }
    
}
