package io.github.luminion.mybatis.extension.mybatisplus;

import io.github.luminion.mybatis.core.BoostEngine;
import io.github.luminion.mybatis.query.core.ISqlEntity;
import io.github.luminion.mybatis.query.helper.ISqlHelper;
import io.github.luminion.mybatis.query.helper.SqlHelper;
import io.github.luminion.mybatis.query.helper.processor.FieldSuffixProcessor;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 BoostEngine 扩展接口. 实现分页方法
 * <p>
 * 集成了 {@link BoostEngine} 的能力, 提供VO查询功能.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface BoostMybatisPlusExtension<T, V> extends BoostEngine<T, V>{

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
        ISqlHelper<T> sqlHelper = SqlHelper.of(this)
                .with(sqlEntity)
                .process(fieldSuffixProcessor::process);
        List<V> vs = selectBySqlEntity(sqlHelper, page);
        voPostProcess(vs, sqlEntity, page);
        return page;
    }

}
