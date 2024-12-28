package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.generate.config.core.CustomConfig;

import java.util.function.Consumer;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class ExtraCodeBuilder extends BaseEnhanceBuilder<ExtraCodeBuilder> {

    /**
     * 不生成查询方法
     *
     * @deprecated 因不具备默认实现, 该生成器必须显式生成查询方法及查询DTO, 故此设置项无效
     * @return {@link ExtraCodeBuilder }
     * @author bootystar
     */
    @Deprecated
    @Override
    public ExtraCodeBuilder disableSelect() {
//        this.generateSelect = false;
//        return this.getBuilder();
        throw new RuntimeException("not support disableSelect()");
    }

    @Override
    public FieldSuffixBuilder getFieldSuffixBuilder() {
        return super.getFieldSuffixBuilder();
    }

    @Override
    public ExtraCodeBuilder fieldSuffixBuilder(Consumer<FieldSuffixBuilder> builderConsumer) {
        return super.fieldSuffixBuilder(builderConsumer);
    }



}
