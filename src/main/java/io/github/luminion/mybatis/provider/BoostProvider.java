package io.github.luminion.mybatis.provider;

import org.springframework.core.Ordered;

/**
 * @author luminion
 */
public interface BoostProvider extends Comparable<BoostProvider>, Ordered,
        TableNameProvider, PropertyToColumnMapProvider,
        IdColumProvider, IdPropertyProvider, IdPropertyGetterProvider,
        GetterColumnProvider, GetterPropertyProvider {

    @Override
    default int compareTo(BoostProvider o) {
        return o.getOrder() - this.getOrder();
    }
}
