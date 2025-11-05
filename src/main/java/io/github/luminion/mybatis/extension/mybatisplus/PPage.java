package io.github.luminion.mybatis.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.luminion.mybatis.core.P;

/**
 * @author luminion
 */
public class PPage<T> extends Page<T> implements P<T> {
    public PPage() {
        super();
    }
    public PPage(long current, long size) {
        super(current, size);
    }

    public PPage(long current, long size, long total) {
        super(current, size, total);
    }

    public PPage(long current, long size, boolean searchCount) {
        super(current, size, searchCount);
    }

    public PPage(long current, long size, long total, boolean searchCount) {
        super(current, size, total, searchCount);
    }
}
