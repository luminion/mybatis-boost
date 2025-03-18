package io.github.bootystar.mybatisplus.generate.strategy;

import com.baomidou.mybatisplus.generator.config.po.TableField;

/**
 * 额外字段策略
 *
 * @author bootystar
 */
@FunctionalInterface
public interface ExtraFieldGenerateStrategy {

    /**
     * 是否允许生成额外字段
     *
     * @param sqlOperator sql运算符(=, LIKE, IN 等)
     * @param tableField 表字段信息
     * @return 是否生成
     */
    boolean allowGenerate(String sqlOperator, TableField tableField);

}
