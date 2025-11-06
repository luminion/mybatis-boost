package io.github.luminion.sqlbooster.provider.support;

import com.baomidou.mybatisplus.core.metadata.TableFieldInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import io.github.luminion.sqlbooster.core.MethodReference;
import io.github.luminion.sqlbooster.provider.BoostProvider;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * MyBatis-Plus 提供者实现.
 * <p>
 * 基于 MyBatis-Plus 框架提供数据库元数据信息，包括表名、字段映射等.
 *
 * @author luminion
 * @since 1.0.0
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class MybatisPlusProvider implements BoostProvider {

    /**
     * {@inheritDoc}
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
        return TableInfoHelper.getTableInfo(clazz).getKeyProperty();
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz) {
        return TableInfoHelper.getTableInfo(clazz).getFieldList().stream()
                .collect(Collectors.toMap(TableFieldInfo::getProperty,
                        e -> String.format("a.%s", e.getColumn())));
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public <T> String getTableName(Class<T> clazz) {
        return TableInfoHelper.getTableInfo(clazz).getTableName();
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.0.0
     */
    @Override
    public int getOrder() {
        return Integer.MAX_VALUE;
    }
}
