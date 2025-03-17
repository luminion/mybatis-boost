package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.builder.ExtraFieldSuffixBuilder;
import io.github.bootystar.mybatisplus.generate.handler.ExtraFieldGenerateStrategy;

import java.util.function.Consumer;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class ExtraCodeBuilder extends BaseEnhanceBuilder<ExtraCodeBuilder> {

    @Override
    public ExtraCodeBuilder extraFieldSuffixBuilder(Consumer<ExtraFieldSuffixBuilder> builderConsumer) {
        return super.extraFieldSuffixBuilder(builderConsumer);
    }

    @Override
    public ExtraCodeBuilder extraFieldStrategy(ExtraFieldGenerateStrategy strategy) {
        return super.extraFieldStrategy(strategy);
    }


}
