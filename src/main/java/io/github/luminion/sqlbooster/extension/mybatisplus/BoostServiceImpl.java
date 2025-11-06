package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.extension.mybatis.BoostMapper;
import io.github.luminion.sqlbooster.core.P;
import io.github.luminion.sqlbooster.model.api.ISqlEntity;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 ServiceImpl的扩展.
 * <p>
 * 集成了 {@link BoosterEngine} 的能力, 为 Mybatis-Plus 的 Service 实现层提供 VO 查询功能.
 *
 * @param <M> Mapper 类型
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public abstract class BoostServiceImpl<M extends BaseMapper<T> & BoostMapper<T, V>, T, V> extends ServiceImpl<M, T> implements IPageBooster<T, V> {

    @Override
    public List<V> selectBySqlEntity(ISqlEntity<T> sqlEntity, P<V> page) {
        return getBaseMapper().selectBySqlEntity(sqlEntity, page);
    }

}
