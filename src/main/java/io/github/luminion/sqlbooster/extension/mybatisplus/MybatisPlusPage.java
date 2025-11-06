package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.luminion.sqlbooster.core.Page;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.RequiredArgsConstructor;

import java.util.List;

/**
 * Mybatis-Plus 分页对象适配器.
 * <p>
 * 实现了 {@link Page} 接口, 用于将 Mybatis-Plus 的 {@link com.baomidou.mybatisplus.extension.plugins.pagination.Page} 对象适配为 Booster 的分页模型.
 *
 * @param <T> 记录的类型
 * @author luminion
 * @since 1.0.0
 */
@RequiredArgsConstructor
public class MybatisPlusPage<T> implements Page<T> {
    private final IPage<T> pageInfo;

    @Override
    public List<T> getRecords() {
        return pageInfo.getRecords();
    }

    @Override
    public long getTotal() {
        return pageInfo.getTotal();
    }

    @Override
    public long getCurrent() {
        return pageInfo.getCurrent();
    }

    @Override
    public long getSize() {
        return pageInfo.getSize();
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public <R> Page<R> convertRecords(Class<R> targetType) {
        IPage<R> convert = pageInfo.convert(e -> ReflectUtils.toTarget(e, targetType));
        return new MybatisPlusPage<>(convert);
    }

}
