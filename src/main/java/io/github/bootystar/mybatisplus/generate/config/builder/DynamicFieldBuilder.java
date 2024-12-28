package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import lombok.Getter;

import java.util.Map;
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
        this.selectDTO = new ClassInfo(Map.class);
        return this.getBuilder();
    }

    @Override
    public DynamicFieldBuilder withSqlHelperSelectDTO() {
        this.selectDTO = new ClassInfo(SqlHelper.class);
        return this.getBuilder();
    }

    @Override
    public FieldSuffixBuilder getFieldSuffixBuilder() {
        return this.extraFieldSuffixBuilder;
    }

    @Override
    public DynamicFieldBuilder fieldSuffixBuilder(Consumer<FieldSuffixBuilder> builderConsumer) {
        builderConsumer.accept(this.extraFieldSuffixBuilder);
        return this.getBuilder();
    }

}


