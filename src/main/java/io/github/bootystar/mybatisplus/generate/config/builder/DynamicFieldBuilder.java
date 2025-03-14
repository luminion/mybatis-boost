package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.generate.handler.support.ExtraFieldSuffixBuilder;
import io.github.bootystar.mybatisplus.generate.handler.ExtraFieldGenerateStrategy;
import lombok.Getter;

import java.util.function.Consumer;


/**
 * @author bootystar
 */
@Getter
public class DynamicFieldBuilder extends BaseEnhanceBuilder<DynamicFieldBuilder> {

//    {
//        mapperDTO = new ClassInfo(UnmodifiableSqlHelper.class);
//    }

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
    public DynamicFieldBuilder extraFieldStrategy(ExtraFieldGenerateStrategy strategy) {
        return super.extraFieldStrategy(strategy);
    }
}


