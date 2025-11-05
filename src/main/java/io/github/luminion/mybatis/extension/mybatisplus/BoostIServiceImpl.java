package io.github.luminion.mybatis.extension.mybatisplus;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.luminion.mybatis.core.BoostMapper;
import io.github.luminion.mybatis.core.P;
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
public abstract class BoostIServiceImpl<M extends BaseMapper<T> & BoostMapper<T, V>, T, V> extends ServiceImpl<M, T> implements BoostIService<T, V> {

    @Override
    public List<V> selectBySqlEntity(ISqlEntity<T> params, P<?> page) {
        return getBaseMapper().selectBySqlEntity(params, page);
    }

}
