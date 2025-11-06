package io.github.luminion.sqlbooster.provider.support;

import io.github.luminion.sqlbooster.core.MethodReference;
import io.github.luminion.sqlbooster.provider.BoostProvider;
import io.github.luminion.sqlbooster.util.BoostUtils;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * 基础的 MybatisBoostProvider 实现
 * <p>
 * 提供了默认的表名、ID 属性名、getter 属性名和属性到列的映射逻辑.
 *
 * @author luminion
 * @since 1.0.0
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class MybatisProvider implements BoostProvider {

    /**
     * 是否将驼峰命名转换为下划线命名
     */
    private final boolean mapUnderscoreToCamelCase;

    /**
     * {@inheritDoc}
     * <p>
     * 默认实现是将类名从驼峰转换为下划线作为表名.
     *
     * @since 1.0.0
     */
    @Override
    public <T> String getTableName(Class<T> clazz) {
        String tableName = BoostUtils.camelCaseToUnderscore(clazz.getName());
        if (tableName.startsWith("_")){
            return tableName.substring(1);
        }
        return tableName;
    }

    /**
     * {@inheritDoc}
     * <p>
     * 默认实现是查找是否存在名为 "getId" 的方法, 如果存在则返回 "id".
     *
     * @since 1.0.0
     */
    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        try {
            clazz.getMethod("getId");
            return "id";
        } catch (NoSuchMethodException e) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * 默认实现是通过 {@link ReflectUtils#getGetterPropertyName(MethodReference)} 获取属性名.
     *
     * @since 1.0.0
     */
    @Override
    public <T, R> String getGetterPropertyName(MethodReference<T, R> getter) {
        try {
            return ReflectUtils.getGetterPropertyName(getter);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * 默认实现是将所有字段名根据 `mapUnderscoreToCamelCase` 配置转换为对应的列名.
     *
     * @since 1.0.0
     */
    @Override
    public <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz) {
        Set<String> strings = ReflectUtils.fieldMap(clazz).keySet();
        return strings.stream()
                .collect(Collectors.toMap(e -> e, e -> 
                        String.format("a.%s", mapUnderscoreToCamelCase ? BoostUtils.camelCaseToUnderscore(e) : e)));
    }

    /**
     * {@inheritDoc}
     * <p>
     * 返回 Integer.MAX_VALUE, 表示这是一个优先级最低的 Provider.
     *
     * @since 1.0.0
     */
    @Override
    public int getOrder() {
        return Integer.MAX_VALUE;
    }

}
