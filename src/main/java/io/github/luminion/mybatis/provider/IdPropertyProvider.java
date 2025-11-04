package io.github.luminion.mybatis.provider;

/**
 * ID 属性提供者接口.
 * <p>
 * 用于根据实体类获取其主键属性的名称.
 *
 * @author luminion
 * @since 1.0.0
 */
@FunctionalInterface
public interface IdPropertyProvider {

    /**
     * 根据实体类获取主键属性名.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return 主键属性名
     * @since 1.0.0
     */
   <T> String getIdPropertyName(Class<T> clazz);
}
