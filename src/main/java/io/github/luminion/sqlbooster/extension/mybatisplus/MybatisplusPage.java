package io.github.luminion.sqlbooster.extension.mybatisplus;

import io.github.luminion.sqlbooster.core.Page;
import io.github.luminion.sqlbooster.util.ReflectUtils;

import java.util.List;
import java.util.stream.Collectors;


/**
 * Mybatis-Plus 分页对象适配器.
 * <p>
 * 实现了 {@link Page} 接口, 用于将 Mybatis-Plus 的 {@link com.baomidou.mybatisplus.extension.plugins.pagination.Page} 对象适配为 Boost 的分页模型.
 *
 * @param <T> 记录的类型
 * @author luminion
 * @since 1.0.0
 */
public class MybatisplusPage<T> extends com.baomidou.mybatisplus.extension.plugins.pagination.Page<T> implements Page<T> {
    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public MybatisplusPage() {
        super();
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public MybatisplusPage(long current, long size) {
        super(current, size);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public MybatisplusPage(long current, long size, long total) {
        super(current, size, total);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public MybatisplusPage(long current, long size, boolean searchCount) {
        super(current, size, searchCount);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    public MybatisplusPage(long current, long size, long total, boolean searchCount) {
        super(current, size, total, searchCount);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    @SuppressWarnings("unchecked")
    public <R> Page<R> convertRecords(Class<R> targetType) {
        List<R> collect = this.getRecords().stream()
                .map(e -> ReflectUtils.toTarget(e, targetType))
                .collect(Collectors.toList());
        MybatisplusPage<R> rp = (MybatisplusPage<R>) this;
        rp.setRecords(collect);
        return rp;
    }
}
