package io.github.luminion.mybatis.provider;

/**
 * 用于获取指定类的ID字段属性名
 * @author luminion
 */
@FunctionalInterface
public interface IdPropertyProvider {
    
   <T> String getIdPropertyName(Class<T> clazz);
}
