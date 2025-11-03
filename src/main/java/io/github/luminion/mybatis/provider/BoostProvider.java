package io.github.luminion.mybatis.provider;

/**
 * @author luminion
 */
public interface BoostProvider extends Comparable<BoostProvider>, 
        TableNameProvider, PropertyToColumnMapProvider, 
        IdColumProvider, IdPropertyProvider, IdPropertyGetterProvider, 
        GetterColumnProvider, GetterPropertyProvider {

}
