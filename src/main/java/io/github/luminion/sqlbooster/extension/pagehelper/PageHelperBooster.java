package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageHelper;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.Page;
import io.github.luminion.sqlbooster.model.api.Wrapper;

/**
 * @author luminion
 */
public interface PageHelperBooster<T, V> extends BoosterEngine<T, V> {
    @Override
    default Page<V> voPage(Wrapper<T> wrapper, long pageNum, long pageSize) {
        PageHelper.startPage((int) pageNum, (int) pageSize);
        // todo pageHelper
        return BoosterEngine.super.voPage(wrapper, pageNum, pageSize);
    }
}
