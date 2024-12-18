package io.github.bootystar.mybatisplus.generate.generator.core;

import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.Controller;
import com.baomidou.mybatisplus.generator.config.builder.Entity;
import com.baomidou.mybatisplus.generator.config.builder.Mapper;
import com.baomidou.mybatisplus.generator.config.builder.Service;
import io.github.bootystar.mybatisplus.generate.config.base.CustomConfig;

import java.util.function.Consumer;

/**
 * mybatis-plus增强生成器
 * @author bootystar
 */
public interface EnhanceGenerator<B extends CustomConfig.Builder<?, B>> {

    void execute(String... tableNames);

    EnhanceGenerator<B> dataSource(Consumer<DataSourceConfig.Builder> consumer);

    EnhanceGenerator<B> global(Consumer<GlobalConfig.Builder> consumer);

    EnhanceGenerator<B> pkg(Consumer<PackageConfig.Builder> consumer);

    EnhanceGenerator<B> strategy(Consumer<StrategyConfig.Builder> consumer);

    EnhanceGenerator<B> entity(Consumer<Entity.Builder> consumer);

    EnhanceGenerator<B> mapper(Consumer<Mapper.Builder> consumer);

    EnhanceGenerator<B> service(Consumer<Service.Builder> consumer);

    EnhanceGenerator<B> controller(Consumer<Controller.Builder> consumer);

    EnhanceGenerator<B> injection(Consumer<InjectionConfig.Builder> consumer);

    EnhanceGenerator<B> custom(Consumer<B> consumer);

    EnhanceGenerator<B> mapperXmlResource(String path);

    EnhanceGenerator<B> initialize();

    EnhanceGenerator<B> enableGlobalFileOverwrite();

}
