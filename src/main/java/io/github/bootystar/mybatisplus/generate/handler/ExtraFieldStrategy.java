package io.github.bootystar.mybatisplus.generate.handler;

import com.baomidou.mybatisplus.generator.config.po.TableField;

/**
 * 额外字段策略
 *
 * @author bootystar
 */
@FunctionalInterface
public interface ExtraFieldStrategy {

    /**
     * 是否允许根据源字段生成sql关键字对应的额外字段
     *
     * @param keyword     sql关键字
     * @param sourceField 源字段信息
     * @return boolean
     */
    boolean allowGenerate(String keyword, TableField sourceField);

}
