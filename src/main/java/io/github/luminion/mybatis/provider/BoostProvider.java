package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.util.ReflectUtils;
import org.springframework.core.Ordered;

/**
 * @author luminion
 */
public interface BoostProvider extends Comparable<BoostProvider>, Ordered,
        TableNameProvider, PropertyToColumnMapProvider,
        IdColumProvider, IdPropertyProvider, IdPropertyGetterProvider,
        GetterColumnProvider, GetterPropertyProvider {


    @Override
    default <T, R> String getGetterPropertyName(MethodReference<T, R> getter){
        return ReflectUtils.getGetterField(getter).getName();
    }

    @Override
    default int compareTo(BoostProvider o) {
        return o.getOrder() - this.getOrder();
    }
}
