package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.core.Page;
import io.github.luminion.sqlbooster.extension.mybatisplus.MybatisPlusPage;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.RequiredArgsConstructor;

import java.util.List;

/**
 * @author luminion
 */
@RequiredArgsConstructor
public class PageHelperPage<T> implements Page<T> {
    private final PageInfo<T> pageInfo;

    @Override
    public List<T> getRecords() {
        return pageInfo.getList();
    }

    @Override
    public long getTotal() {
        return pageInfo.getTotal();
    }

    @Override
    public long getCurrent() {
        return pageInfo.getPageNum();
    }

    @Override
    public long getSize() {
        return pageInfo.getSize();
    }

    @Override
    public <R> Page<R> convertRecords(Class<R> targetType) {
        PageInfo<R> convert = pageInfo.convert(e -> ReflectUtils.toTarget(e, targetType));
        return new PageHelperPage<>(convert);
    }
}
