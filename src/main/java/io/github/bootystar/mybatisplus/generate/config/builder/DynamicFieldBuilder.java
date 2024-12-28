package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.UnmodifiableSqlHelper;
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
    public FieldSuffixBuilder getFieldSuffixBuilder() {
        return super.getFieldSuffixBuilder();
    }

    @Override
    public DynamicFieldBuilder fieldSuffixBuilder(Consumer<FieldSuffixBuilder> builderConsumer) {
        return super.fieldSuffixBuilder(builderConsumer);
    }

}


