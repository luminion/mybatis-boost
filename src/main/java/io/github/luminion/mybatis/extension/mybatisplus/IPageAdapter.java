package io.github.luminion.mybatis.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.luminion.mybatis.core.P;
import io.github.luminion.mybatis.util.ReflectUtils;

import java.util.List;
import java.util.stream.Collectors;


/**
 * Mybatis-Plus 分页对象适配器.
 * <p>
 * 实现了 {@link P} 接口, 用于将 Mybatis-Plus 的 {@link Page} 对象适配为 Boost 的分页模型.
 *
 * @param <T> 记录的类型
 * @author luminion
 * @since 1.0.0
 */
public class IPageAdapter<T> extends Page<T> implements P<T> {
    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public IPageAdapter() {
        super();
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public IPageAdapter(long current, long size) {
        super(current, size);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public IPageAdapter(long current, long size, long total) {
        super(current, size, total);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public IPageAdapter(long current, long size, boolean searchCount) {
        super(current, size, searchCount);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public IPageAdapter(long current, long size, long total, boolean searchCount) {
        super(current, size, total, searchCount);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    @SuppressWarnings("unchecked")
    public <R> P<R> convertRecords(Class<R> targetType) {
        List<R> collect = this.getRecords().stream()
                .map(e -> ReflectUtils.toTarget(e, targetType))
                .collect(Collectors.toList());
        IPageAdapter<R> rp = (IPageAdapter<R>) this;
        rp.setRecords(collect);
        return rp;
    }
}
