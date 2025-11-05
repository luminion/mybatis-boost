package io.github.luminion.mybatis.extension.pagehelper;

import com.github.pagehelper.PageHelper;
import io.github.luminion.mybatis.core.BoostEngine;
import io.github.luminion.mybatis.core.P;
import io.github.luminion.mybatis.query.core.ISqlEntity;

/**
 * @author luminion
 */
public interface PageHelperBooster<T, V> extends BoostEngine<T, V> {
    @Override
    default P<V> voPage(ISqlEntity<T> sqlEntity, long pageNum, long pageSize) {
        PageHelper.startPage(pageNum, pageSize);
        
        return BoostEngine.super.voPage(sqlEntity, pageNum, pageSize);
    }
}
