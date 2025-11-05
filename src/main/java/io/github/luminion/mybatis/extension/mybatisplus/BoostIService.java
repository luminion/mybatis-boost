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

    /**
     * {@inheritDoc}
     *
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
        voPostProcess(vs, params, page);
        return page;
    }

}
