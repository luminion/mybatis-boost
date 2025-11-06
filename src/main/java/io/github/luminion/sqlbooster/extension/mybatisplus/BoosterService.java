package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;

/**
 * @author luminion
 */
public interface BoosterService<T, V> extends IService<T>, MybatisPlusBooster<T, V> {
}
