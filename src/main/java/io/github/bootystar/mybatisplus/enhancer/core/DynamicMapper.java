package io.github.bootystar.mybatisplus.enhancer.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.query.SqlTree;

import java.util.List;

/**
 * mapper
 *
 * @author bootystar
 */
public interface DynamicMapper<V> extends EnhancedQuery<V> {

    List<V> voSelectByXml(SqlTree sqlTree, IPage<V> page);

    @Override
    default List<V> voSelect(Object param, IPage<V> page) {
        if (param == null) {
            return voSelectByXml(null, page);
        }
        if (param instanceof SqlTree) {
            return voSelectByXml((SqlTree) param, page);
        }
        return voSelectByXml(SqlHelper.of(param), page);
    }
    
}
