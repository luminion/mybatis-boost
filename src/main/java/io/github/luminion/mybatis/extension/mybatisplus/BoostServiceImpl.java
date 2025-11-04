package io.github.luminion.mybatis.extension.mybatisplus;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.luminion.mybatis.query.core.ISqlEntity;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 ServiceImpl的扩展.
 * <p>
 * 集成了 {@link io.github.luminion.mybatis.core.BoostEngine} 的能力, 为 Mybatis-Plus 的 Service 实现层提供 VO 查询功能.
 *
 * @param <M> Mapper 类型
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public abstract class BoostServiceImpl<M extends BoostBaseMapper<T, V>, T, V> extends ServiceImpl<M, T> implements BoostIService<T, V> {

    @Override
    public <R> IPage<R> voPage(ISqlEntity<T> params, int pageNum, int pageSize, Class<R> voType) {
        return BoostIService.super.voPage(params, pageNum, pageSize, voType);
    }

    @Override
    public <R> IPage<R> voPage(ISqlEntity<T> params, long pageNum, long pageSize, Class<R> voType) {
        return BoostIService.super.voPage(params, pageNum, pageSize, voType);
    }

    @Override
    public List<V> selectBySqlEntity(ISqlEntity<T> params, IPage<?> page) {
        return getBaseMapper().selectBySqlEntity(params, page);
    }
}
