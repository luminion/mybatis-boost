package io.github.luminion.mybatis.provider.support;

import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.provider.BoostProvider;
import io.github.luminion.mybatis.util.BoostUtils;
import io.github.luminion.mybatis.util.ReflectUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * @author luminion
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class BasicProvider implements BoostProvider {
    private final boolean mapUnderscoreToCamelCase;

    @Override
    public <T> String getTableName(Class<T> clazz) {
        return BoostUtils.camelCaseToUnderscore(clazz.getName());
    }

    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        try {
            clazz.getMethod("getId");
            return "id";
        } catch (NoSuchMethodException e) {
            return null;
        }
    }

    @Override
    public <T, R> String getGetterPropertyName(MethodReference<T, R> getter) {
        try {
            return ReflectUtils.getGetterField(getter).getName();
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnMap(Class<T> clazz) {
        Set<String> strings = ReflectUtils.fieldMap(clazz).keySet();
        return strings.stream().collect(Collectors.toMap(e -> e,
                e -> mapUnderscoreToCamelCase ? BoostUtils.camelCaseToUnderscore(e) : e));
    }

    @Override
    public int getOrder() {
        return Integer.MAX_VALUE;
    }

}
