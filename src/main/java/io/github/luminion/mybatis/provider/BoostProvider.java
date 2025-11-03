package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.util.ReflectUtils;
import org.springframework.core.Ordered;

/**
 * @author luminion
 */
public interface BoostProvider extends Comparable<BoostProvider>, Ordered,
        TableNameProvider, IdPropertyProvider, GetterPropertyProvider, PropertyToColumnMapProvider {

    @Override
    default int compareTo(BoostProvider o) {
        return o.getOrder() - this.getOrder();
    }
}
