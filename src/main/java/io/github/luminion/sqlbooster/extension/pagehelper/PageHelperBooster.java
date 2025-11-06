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
 * @author luminion
 */
public interface PageHelperBooster<T, V> extends BoosterEngine<T, V> {
    @Override
    default Page<V> voPage(Wrapper<T> wrapper, long pageNum, long pageSize) {
        voPreProcess(wrapper);
        
        BaseHelper<T> sqlHelper = SqlHelper.of(wrapper).entity(this).process(FieldSuffixProcessor.of()::process);
        PageInfo<V> pageInfo = PageHelper.startPage((int) pageNum, (int) pageSize)
                .doSelectPageInfo(() -> selectByWrapper(sqlHelper, null));
        PageHelperPage<V> page = new PageHelperPage<>(pageInfo);
        
        voPostProcess(page.getRecords(), sqlHelper, page);
        return page;
    }
}
