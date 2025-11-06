package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.extension.mybatis.BoostMapper;

/**
 * 针对 Mybatis-Plus 的 BaseMapper 扩展接口.
 * <p>
 * 集成了 {@link BoostMapper} 的能力, 提供VO查询功能.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface BoostBaseMapper<T, V> extends IPageBooster<T, V>, BoostMapper<T, V>, BaseMapper<T> {
}
