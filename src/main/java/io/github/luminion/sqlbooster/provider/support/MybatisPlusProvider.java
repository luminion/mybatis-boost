package io.github.luminion.sqlbooster.provider.support;

import io.github.luminion.sqlbooster.core.MethodReference;
import io.github.luminion.sqlbooster.provider.BoostProvider;
import io.github.luminion.sqlbooster.util.ReflectUtils;

import java.util.Map;

/**
 * @author luminion
 */
public class MybatisPlusProvider implements BoostProvider {


    /**
     * {@inheritDoc}
     * 
     *
     * @since 1.0.0
     */
    @Override
    public <T, R> String getGetterPropertyName(MethodReference<T, R> getter) {
        return ReflectUtils.getGetterPropertyName(getter);
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        return "";
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public <T> Map<String, String> getPropertyToColumnMap(Class<T> clazz) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public <T> String getTableName(Class<T> clazz) {
        return "";
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public int getOrder() {
        return 0;
    }
}
