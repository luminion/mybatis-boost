package io.github.bootystar.mybatisplus.enhance.helper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;

import java.util.List;

/**
 * sql助手包装器
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelperWrapper<T, V> extends AbstractSqlHelper<T, SqlHelperWrapper<T, V>> {

    private final DynamicService<T, V> baseService;

    public SqlHelperWrapper(DynamicService<T, V> baseService) {
        this.baseService = baseService;
    }

    @Override
    protected SqlHelperWrapper<T, V> returnValue() {
        return this;
    }

    public V one() {
        return baseService.oneByDTO(this);
    }

    public List<V> list() {
        return baseService.listByDTO(this);
    }

    public IPage<V> page(Long current, Long size) {
        return baseService.pageByDTO(this, current, size);
    }


}
