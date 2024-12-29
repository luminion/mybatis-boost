package io.github.bootystar.mybatisplus.generate.handler.support;

import com.baomidou.mybatisplus.generator.config.po.TableField;
import io.github.bootystar.mybatisplus.generate.handler.ExtraFieldStrategy;

/**
 * 忽略
 *
 * @author bootystar
 */
public class ExtraFieldStrategyAllAllow implements ExtraFieldStrategy {

    public boolean allowGenerate(String keyword, TableField field) {
        return true;
    }

}
