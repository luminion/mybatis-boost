package io.github.bootystar.mybatisplus.enhancer.sql.helper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;

import java.util.List;

/**
 * sql助手包装器
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class LambdaSqlHelperWrapper<T, V> extends LambdaSqlHelper<T, LambdaSqlHelperWrapper<T, V>> {

    private final EnhancedQuery<V> enhancedQuery;

    public <S extends IService<T> & EnhancedQuery<V>> LambdaSqlHelperWrapper(S baseService) {
        this.enhancedQuery = baseService;
    }

    public <S extends BaseMapper<T> & EnhancedQuery<V>> LambdaSqlHelperWrapper(S baseMapper) {
        this.enhancedQuery = baseMapper;
    }

    @Override
    protected LambdaSqlHelperWrapper<T, V> returnValue() {
        return this;
    }

    public V one() {
        return enhancedQuery.voByDTO(this);
    }

    public List<V> list() {
        return enhancedQuery.voList(this);
    }

    public IPage<V> page(Long current, Long size) {
        return enhancedQuery.voPage(this, current, size);
    }


}
