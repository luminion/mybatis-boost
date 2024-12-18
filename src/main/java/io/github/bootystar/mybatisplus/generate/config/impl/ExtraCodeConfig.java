package io.github.bootystar.mybatisplus.generate.config.impl;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.generate.config.base.CustomConfig;

import java.util.function.Consumer;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class ExtraCodeConfig extends CustomConfig {

    public static class Builder extends CustomConfig.Builder<ExtraCodeConfig, Builder> {

        public Builder() {
            super(new ExtraCodeConfig());
        }

        /**
         * 获取字段后缀生成器
         *
         * @return {@link FieldSuffixBuilder }
         * @author bootystar
         */
        public FieldSuffixBuilder getFieldSuffixBuilder() {
            return getConfig().extraFieldSuffixBuilder;
        }

        /**
         * 指定后缀
         *
         * @param builderConsumer builder消费者
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder fieldSuffixBuilder(Consumer<FieldSuffixBuilder> builderConsumer) {
            builderConsumer.accept(this.getConfig().extraFieldSuffixBuilder);
            return getBuilder();
        }

    }
}
