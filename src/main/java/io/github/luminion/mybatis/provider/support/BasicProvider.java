package io.github.luminion.mybatis.provider.support;

import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.provider.BoostProvider;
import io.github.luminion.mybatis.util.ReflectUtils;

import java.util.Map;
import java.util.Set;

/**
 * @author luminion
 */
public class BasicProvider implements BoostProvider {
    @Override
    public <T, R> String getGetterColumnName(MethodReference<T, R> getter) {
        return null;
    }

    @Override
    public <T, R> String getGetterPropertyName(MethodReference<T, R> getter) {
        return null;
    }

    @Override
    public <T> String getIdColumnName(Class<T> clazz) {
        return null;
    }

    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        return null;
    }

    @Override
    public <T, R> MethodReference<T, R> getIdPropertyGetter(Class<T> clazz) {
        return null;
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnMap(Class<T> clazz) {
        Set<String> strings = ReflectUtils.fieldMap(clazz).keySet();
        return null;
    }

    @Override
    public <T> String getTableName(Class<T> clazz) {
        return null;
    }

    @Override
    public int getOrder() {
        return Integer.MAX_VALUE;
    }
}
