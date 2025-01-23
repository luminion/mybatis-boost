package io.github.bootystar.mybatisplus.generate;

import io.github.bootystar.mybatisplus.generate.config.builder.DynamicFieldBuilder;
import io.github.bootystar.mybatisplus.generate.config.builder.DynamicSqlBuilder;
import io.github.bootystar.mybatisplus.generate.config.builder.ExtraCodeBuilder;
import io.github.bootystar.mybatisplus.generate.generator.core.EnhanceGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.DynamicFieldGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.DynamicSqlGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.ExtraCodeGenerator;

/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public abstract class GeneratorHelper {

    /**
     * 额外代码生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link EnhanceGenerator }<{@link ExtraCodeBuilder }>
     * @author bootystar
     */
    public static EnhanceGenerator<ExtraCodeBuilder> extraCodeGenerator(String url, String username, String password) {
        return new ExtraCodeGenerator(url, username, password);
    }

    /**
     * 动态字段生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link EnhanceGenerator }<{@link DynamicFieldBuilder }>
     * @author bootystar
     */
    public static EnhanceGenerator<DynamicFieldBuilder> dynamicFieldGenerator(String url, String username, String password) {
        return new DynamicFieldGenerator(url, username, password);
    }

    /**
     * 动态sql生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link EnhanceGenerator }<{@link DynamicSqlBuilder }>
     * @author bootystar
     */
    public static EnhanceGenerator<DynamicSqlBuilder> dynamicSqlGenerator(String url, String username, String password) {
        return new DynamicSqlGenerator(url, username, password);
    }


}
