package io.github.luminion.mybatis.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;
import io.github.luminion.mybatis.core.BoostEngine;
import io.github.luminion.mybatis.query.core.ISqlEntity;
import io.github.luminion.mybatis.query.helper.ISqlHelper;
import io.github.luminion.mybatis.query.helper.SqlHelper;
import io.github.luminion.mybatis.query.helper.processor.FieldSuffixProcessor;
import io.github.luminion.mybatis.util.ReflectUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * 针对 Mybatis-Plus 的 IService 扩展接口.
 * <p>
 * 集成了 {@link BoostEngine} 的能力, 提供VO查询功能.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface BoostIService<T, V> extends BoostEngine<T, V>, IService<T> {

    @Override
    default PPage<V> voPage(ISqlEntity<T> params, int pageNum, int pageSize) {
        return voPage(params, (long)pageNum, pageSize);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default PPage<V> voPage(ISqlEntity<T> params, long pageNum, long pageSize) {
        PPage<V> page = new PPage<>(pageNum, pageSize);
        voPreProcess(params);
        FieldSuffixProcessor fieldSuffixProcessor = FieldSuffixProcessor.of();
        ISqlHelper<T> sqlHelper = SqlHelper.of(this)
                .with(params)
                .process(fieldSuffixProcessor::process);
        List<V> vs = selectBySqlEntity(sqlHelper, page);
        voPostProcess(vs, params);
        return page;
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default <R> PPage<R> voPage(ISqlEntity<T> params, int pageNum, int pageSize, Class<R> voType) {
        return voPage(params, (long) pageNum, pageSize, voType);
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    @SuppressWarnings("unchecked")
    default <R> PPage<R> voPage(ISqlEntity<T> params, long pageNum, long pageSize, Class<R> voType) {
        PPage<V> voPage = voPage(params, pageNum, pageSize);
        List<R> collect = voPage.getRecords().stream()
                .map(v -> ReflectUtils.toTarget(v, voType))
                .collect(Collectors.toList());
        PPage<R> rPage = (PPage<R>) voPage;
        rPage.setRecords(collect);
        return rPage;
    }
}
