package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.builder.ExtraFieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.generate.handler.ExtraFieldStrategy;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import lombok.Getter;

import java.util.function.Consumer;


/**
 * @author bootystar
 */
@Getter
public class DynamicFieldBuilder extends BaseEnhanceBuilder<DynamicFieldBuilder> {

    {
        mapperDTO = new ClassInfo(UnmodifiableSqlHelper.class);
    }

    @Override
    public DynamicFieldBuilder disableOverrideMethods() {
        return super.disableOverrideMethods();
    }

    @Override
    public DynamicFieldBuilder withMapSelectDTO() {
        return super.withMapSelectDTO();
    }

    @Override
    public DynamicFieldBuilder withSqlHelperSelectDTO() {
        return super.withSqlHelperSelectDTO();
    }

    @Override
    public DynamicFieldBuilder extraFieldSuffixBuilder(Consumer<ExtraFieldSuffixBuilder> builderConsumer) {
        return super.extraFieldSuffixBuilder(builderConsumer);
    }

    @Override
    public DynamicFieldBuilder extraFieldStrategy(ExtraFieldStrategy strategy) {
        return super.extraFieldStrategy(strategy);
    }
}


