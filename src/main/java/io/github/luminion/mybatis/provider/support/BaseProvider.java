package io.github.luminion.mybatis.provider.support;

import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.provider.BoostProvider;

import java.util.Collections;
import java.util.Map;

/**
 * @author luminion
 */
public class BaseProvider implements BoostProvider {
    @Override
    public <T, R> String getGetterColumnName(MethodReference<T, R> getter) {
        return "";
    }

    @Override
    public <T, R> String getGetterPropertyName(MethodReference<T, R> getter) {
        return "";
    }

    @Override
    public <T> String getIdColumnName(Class<T> clazz) {
        return "";
    }

    @Override
    public <T, R> MethodReference<T, R> getIdPropertyGetter(Class<T> clazz) {
        return null;
    }

    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        return "";
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnMap(Class<T> clazz) {
        return Collections.emptyMap();
    }

    @Override
    public <T> String getTableName(Class<T> clazz) {
        return "";
    }

    @Override
    public int getOrder() {
        return Integer.MAX_VALUE;
    }
}
