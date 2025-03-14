package io.github.bootystar.mybatisplus.generate.handler.support;

import com.baomidou.mybatisplus.generator.config.po.TableField;
import io.github.bootystar.mybatisplus.generate.handler.ExtraFieldGenerateStrategy;

/**
 *
 * @author bootystar
 */
public class ExtraFieldStrategyAllAllow implements ExtraFieldGenerateStrategy {

    public boolean allowGenerate(String keyword, TableField field) {
        return true;
    }

}
