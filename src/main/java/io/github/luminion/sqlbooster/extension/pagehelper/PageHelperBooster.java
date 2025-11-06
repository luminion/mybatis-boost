package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageHelper;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.P;
import io.github.luminion.sqlbooster.model.api.ISqlEntity;

/**
 * @author luminion
 */
public interface PageHelperBooster<T, V> extends BoosterEngine<T, V> {
    @Override
    default P<V> voPage(ISqlEntity<T> sqlEntity, long pageNum, long pageSize) {
        PageHelper.startPage(pageNum, pageSize);
        
        return BoosterEngine.super.voPage(sqlEntity, pageNum, pageSize);
    }
}
