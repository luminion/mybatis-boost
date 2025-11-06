package io.github.luminion.sqlbooster.extension.mybatisplus;

import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.model.api.ISqlEntity;
import io.github.luminion.sqlbooster.model.helper.ISqlHelper;
import io.github.luminion.sqlbooster.model.helper.SqlHelper;
import io.github.luminion.sqlbooster.model.helper.processor.FieldSuffixProcessor;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 BoosterEngine 扩展接口. 实现分页方法
 * <p>
 * 集成了 {@link BoosterEngine} 的能力, 提供VO查询功能.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface IPageBooster<T, V> extends BoosterEngine<T, V> {

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    default IPageAdapter<V> voPage(ISqlEntity<T> sqlEntity, long pageNum, long pageSize) {
        IPageAdapter<V> page = new IPageAdapter<>(pageNum, pageSize);
        voPreProcess(sqlEntity);
        FieldSuffixProcessor fieldSuffixProcessor = FieldSuffixProcessor.of();
        ISqlHelper<T> sqlHelper = SqlHelper.of(sqlEntity)
                .entity(this)
                .process(fieldSuffixProcessor::process);
        List<V> vs = selectBySqlEntity(sqlHelper, page);
        page.setRecords(vs);
        voPostProcess(vs, sqlEntity, page);
        return page;
    }

}
