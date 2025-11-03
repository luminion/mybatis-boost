package io.github.luminion.mybatis.provider.support;

import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.provider.BoostProvider;
import io.github.luminion.mybatis.util.ReflectUtils;
import org.springframework.beans.BeanUtils;

import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * @author luminion
 */
public class BasicProvider implements BoostProvider {
    @Override
    public <T, R> String getGetterColumnName(MethodReference<T, R> getter) {
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
        String idPropertyName = getIdPropertyName(clazz);
        if (idPropertyName != null){
            String methodName = idPropertyName.substring(0, 1).toUpperCase() + idPropertyName.substring(1);
            PropertyDescriptor propertyDescriptor = BeanUtils.getPropertyDescriptor(clazz, methodName);
            if (propertyDescriptor!=null){
                Method readMethod = propertyDescriptor.getReadMethod();
                return new MethodReference<T, R>() {
                    @Override
                    @SuppressWarnings("unchecked")
                    public R apply(T t) {
                        try {
                            return (R) readMethod.invoke(t);
                        } catch (IllegalAccessException | InvocationTargetException e) {
                            throw new RuntimeException(e);
                        }
                    }
                };
            }
        }
        return null;
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnMap(Class<T> clazz) {
        Set<String> strings = ReflectUtils.fieldMap(clazz).keySet();
        return strings.stream().collect(Collectors.toMap(e->e, e->e));
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
