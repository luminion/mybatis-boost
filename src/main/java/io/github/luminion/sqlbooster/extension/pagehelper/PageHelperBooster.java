package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.Page;
import io.github.luminion.sqlbooster.model.api.Wrapper;
import io.github.luminion.sqlbooster.model.sql.helper.BaseHelper;
import io.github.luminion.sqlbooster.model.sql.helper.SqlHelper;
import io.github.luminion.sqlbooster.model.sql.helper.processor.FieldSuffixProcessor;

/**
 * 针对 PageHelper 的 BoosterEngine 扩展接口.
 * <p>
 * 集成了 {@link BoosterEngine} 的能力, 提供基于 PageHelper 的分页查询功能.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface PageHelperBooster<T, V> extends BoosterEngine<T, V> {
    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default Page<V> voPage(Wrapper<T> wrapper, long pageNum, long pageSize) {
        voPreProcess(wrapper);
        
        BaseHelper<T> sqlHelper = SqlHelper.of(wrapper).entity(this).process(FieldSuffixProcessor.of()::process);
        PageInfo<V> pageInfo = PageHelper.startPage((int) pageNum, (int) pageSize)
                .doSelectPageInfo(() -> selectByBooster(sqlHelper, null));
        PageHelperPage<V> page = new PageHelperPage<>(pageInfo);
        
        voPostProcess(page.getRecords(), sqlHelper, page);
        return page;
    }
}