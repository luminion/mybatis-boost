package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;

import java.util.function.Consumer;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class ExtraCodeBuilder extends BaseEnhanceBuilder<ExtraCodeBuilder> {

    @Override
    public FieldSuffixBuilder getFieldSuffixBuilder() {
        return super.getFieldSuffixBuilder();
    }

    @Override
    public ExtraCodeBuilder fieldSuffixBuilder(Consumer<FieldSuffixBuilder> builderConsumer) {
        return super.fieldSuffixBuilder(builderConsumer);
    }


}
