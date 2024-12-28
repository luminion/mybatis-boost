package io.github.bootystar.mybatisplus.generate.config.builder;

import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;

import java.util.Map;
import java.util.function.Consumer;

/**
 * 将非通用配置作为protected寄存
 * 规范子类的重写行为
 * @author bootystar
 */
public abstract class BaseEnhanceBuilder<B extends BaseEnhanceBuilder<B>> extends BaseBuilder<B> {

    /**
     * 不生成重写的方法
     *
     * @return {@code B }
     * @author bootystar
     */
    protected B disableOverrideMethods() {
        this.overrideMethods = false;
        return this.getBuilder();
    }

    /**
     * 使用Map作为查询方法入参DTO
     *
     * @return {@link B }
     * @author bootystar
     */
    protected B withMapSelectDTO() {
        this.selectDTO = new ClassInfo(Map.class);
        return this.getBuilder();
    }

    /**
     * 使用SqlHelper作为查询方法入参DTO
     *
     * @return {@link B }
     * @author bootystar
     */
    protected B withSqlHelperSelectDTO() {
        this.selectDTO = new ClassInfo(SqlHelper.class);
        return this.getBuilder();
    }

    /**
     * 获取字段后缀生成器
     *
     * @return {@link FieldSuffixBuilder }
     * @author bootystar
     */
    protected FieldSuffixBuilder getFieldSuffixBuilder() {
        return this.extraFieldSuffixBuilder;
    }

    /**
     * 后缀生成器的消费者模式
     *
     * @param builderConsumer builder消费者
     * @return {@link B }
     * @author bootystar
     */
    protected B fieldSuffixBuilder(Consumer<FieldSuffixBuilder> builderConsumer) {
        builderConsumer.accept(this.extraFieldSuffixBuilder);
        return this.getBuilder();
    }

}
