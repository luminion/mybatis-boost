package io.github.bootystar.mybatisplus.enhance.core;

import java.util.Map;

/**
 * 增强实体类
 *
 * @author bootystar
 */
public interface DynamicEntity {

    /**
     * 非实体类对应表的属性字段映射
     * key为实体类属性名
     * value为数据库字段名
     *
     * @return {@link Map }<{@link String }, {@link String }>
     * @author bootystar
     */
    Map<String, String> extraFieldColumnMap();

}
