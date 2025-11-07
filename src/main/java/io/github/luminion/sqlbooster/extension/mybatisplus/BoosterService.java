package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;
import io.github.luminion.sqlbooster.model.api.Wrapper;

import java.util.List;

/**
 * 针对MyBatis-Plus 的 IService 扩展接口.
 * <p>
 * 集成了 {@link MybatisPlusBooster} 的能力,
 * 提供增强的数据库操作能力, 包括链式查询构建、Lambda 表达式支持等.
 *
 * @param <T> 实体类型
 * @param <V> VO(视图对象)类型
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterService<T, V> extends IService<T>, MybatisPlusBooster<T, V> {

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    default List<V> selectByBooster(Wrapper<T> wrapper, Object page) {
        BoosterBaseMapper<T, V> baseMapper = (BoosterBaseMapper<T, V>) getBaseMapper();
        return baseMapper.selectByBooster(wrapper, page);
    }
}