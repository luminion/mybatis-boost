package io.github.bootystar.mybatisplus.enhancer.query.helper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.EnhancedQuery;

import java.util.List;

/**
 * sql助手包装器
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelperWrapper<T, V> extends SqlHelper<T> {

    private final EnhancedQuery<V> enhancedQuery;

    public <S extends IService<T> & EnhancedQuery<V>> SqlHelperWrapper(S baseService) {
        this.enhancedQuery = baseService;
    }

    public <S extends BaseMapper<T> & EnhancedQuery<V>> SqlHelperWrapper(S baseMapper) {
        this.enhancedQuery = baseMapper;
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
