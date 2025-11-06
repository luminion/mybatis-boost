package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.core.Page;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.RequiredArgsConstructor;

import java.util.List;

/**
 * PageHelper 分页适配对象
 *
 * @param <T> 记录的类型
 * @author luminion
 * @since 1.0.0
 */
@RequiredArgsConstructor
public class PageHelperPage<T> implements Page<T> {

    /**
     * PageHelper 的分页对象
     */
    private final PageInfo<T> pageInfo;

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public List<T> getRecords() {
        return pageInfo.getList();
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public long getTotal() {
        return pageInfo.getTotal();
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public long getCurrent() {
        return pageInfo.getPageNum();
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
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
        PageInfo<R> convert = pageInfo.convert(e -> ReflectUtils.toTarget(e, targetType));
        return new PageHelperPage<>(convert);
    }
}
