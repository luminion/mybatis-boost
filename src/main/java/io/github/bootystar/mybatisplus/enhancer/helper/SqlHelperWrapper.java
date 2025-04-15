package io.github.bootystar.mybatisplus.enhancer.helper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicQueryService;

import java.util.List;

/**
 * sql助手包装器
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelperWrapper<T, V, S extends IService<T> & DynamicQueryService<V>> extends AbstractSqlHelper<T, SqlHelperWrapper<T, V ,S>> {

    private final S baseService;

    public SqlHelperWrapper(S baseService) {
        this.baseService = baseService;
    }

    @Override
    protected SqlHelperWrapper<T, V ,S> returnValue() {
        return this;
    }

    public V one() {
        return baseService.voByDTO(this);
    }

    public List<V> list() {
        return baseService.voList(this);
    }

    public IPage<V> page(Long current, Long size) {
        return baseService.voPage(this, current, size);
    }


}
