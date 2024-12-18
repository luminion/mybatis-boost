package io.github.bootystar.mybatisplus.generate.generator.core;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.*;
import com.baomidou.mybatisplus.generator.config.rules.DateType;
import com.baomidou.mybatisplus.generator.config.rules.DbColumnType;
import io.github.bootystar.mybatisplus.generate.config.base.CustomConfig;
import io.github.bootystar.mybatisplus.generate.engine.EnhanceVelocityTemplateEngine;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.JdbcType;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.function.Consumer;

/**
 * 自定义配置代码生成器-抽象父类
 *
 * @author bootystar
 */
@Getter
@Slf4j
@SuppressWarnings("unused")
public abstract class AbstractGenerator<C extends CustomConfig, B extends CustomConfig.Builder<C, B>> implements EnhanceGenerator<B> {
    protected DataSourceConfig.Builder dataSourceConfigBuilder;
    protected GlobalConfig.Builder globalConfigBuilder = new GlobalConfig.Builder();
    protected PackageConfig.Builder packageConfigBuilder = new PackageConfig.Builder();
    protected StrategyConfig.Builder strategyConfigBuilder = new StrategyConfig.Builder();
    protected InjectionConfig.Builder injectionConfigBuilder = new InjectionConfig.Builder();
    protected B customConfigBuilder;

    public AbstractGenerator(String url, String username, String password, B customConfigBuilder) {
        this.dataSourceConfigBuilder = new DataSourceConfig.Builder(url, username, password)
                .typeConvertHandler((globalConfig, typeRegistry, metaInfo) -> {
                    // 避免byte转换成Integer
                    if (JdbcType.TINYINT == metaInfo.getJdbcType()) {
                        return DbColumnType.INTEGER;
                    }
                    return typeRegistry.getColumnType(metaInfo);
                })
        ;
        this.customConfigBuilder = customConfigBuilder;
        this.globalConfigBuilder
                .dateType(DateType.TIME_PACK)
                .author(System.getProperty("user.name"))
                .outputDir(System.getProperty("user.dir") + "/src/main/java")
        ;
        this.packageConfigBuilder
                .parent("io.github.bootystar")
                .xml("mapper")
        ;
        this.strategyConfigBuilder.entityBuilder()
                .javaTemplate("/templates/base/entity.java")
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
                .mapperTemplate("/templates/base/mapper.java")
                .mapperXmlTemplate("/templates/base/mapper.xml")
        ;
        this.strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/templates/base/service.java")
                .serviceImplTemplate("/templates/base/serviceImpl.java")
        ;
        this.strategyConfigBuilder.controllerBuilder()
                .template("/templates/base/controller.java")
        ;
    }

    private void execute() {
        log.debug("==========================准备生成文件...==========================");
        // 初始化配置
        ConfigBuilder config = new ConfigBuilder(
                this.packageConfigBuilder.build(),
                this.dataSourceConfigBuilder.build(),
                this.strategyConfigBuilder.build(),
                null,
                this.globalConfigBuilder.build(),
                this.injectionConfigBuilder.build()
        );
        EnhanceVelocityTemplateEngine templateEngine = new EnhanceVelocityTemplateEngine(this.customConfigBuilder.build());
        templateEngine.setConfigBuilder(config);
        // 模板引擎初始化执行文件输出
        templateEngine.init(config).batchOutput().open();
        log.debug("==========================文件生成完成！！！==========================");
        System.out.println("\n" +
                "                 __         __    _                __                              __                             \n" +
                "  __ _   __ __  / /  ___ _ / /_  (_)  ___   ___   / / __ __  ___      ___   ___   / /  ___ _  ___  ____ ___   ____\n" +
                " /  ' \\ / // / / _ \\/ _ `// __/ / /  (_-<  / _ \\ / / / // / (_-<     / -_) / _ \\ / _ \\/ _ `/ / _ \\/ __// -_) / __/\n" +
                "/_/_/_/ \\_, / /_.__/\\_,_/ \\__/ /_/  /___/ / .__//_/  \\_,_/ /___/     \\__/ /_//_//_//_/\\_,_/ /_//_/\\__/ \\__/ /_/   \n" +
                "       /___/                             /_/                                                                      \n");

        System.out.println("execute success! check files in following folder:");
        String path = config.getPathInfo().get(OutputFile.parent);
        System.out.println(new File(path).getAbsolutePath());
    }

    /**
     * 执行
     *
     * @param tableNames 表名(不填为全部)
     * @author bootystar
     */
    @Override
    public void execute(String... tableNames) {
        if (tableNames != null && tableNames.length > 0) {
            strategyConfigBuilder.addInclude(Arrays.asList(tableNames));
        }
        execute();
    }

    @Override
    public EnhanceGenerator<B> dataSource(Consumer<DataSourceConfig.Builder> consumer) {
        consumer.accept(dataSourceConfigBuilder);
        return this;
    }

    @Override
    public EnhanceGenerator<B> global(Consumer<GlobalConfig.Builder> consumer) {
        consumer.accept(globalConfigBuilder);
        return this;
    }

    @Override
    public EnhanceGenerator<B> pkg(Consumer<PackageConfig.Builder> consumer) {
        consumer.accept(packageConfigBuilder);
        return this;
    }

    @Override
    public EnhanceGenerator<B> strategy(Consumer<StrategyConfig.Builder> consumer) {
        consumer.accept(strategyConfigBuilder);
        return this;
    }

    @Override
    public EnhanceGenerator<B> entity(Consumer<Entity.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.entityBuilder());
        return this;
    }

    @Override
    public EnhanceGenerator<B> mapper(Consumer<Mapper.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.mapperBuilder());
        return this;
    }

    @Override
    public EnhanceGenerator<B> service(Consumer<Service.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.serviceBuilder());
        return this;
    }

    @Override
    public EnhanceGenerator<B> controller(Consumer<Controller.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.controllerBuilder());
        return this;
    }

    @Override
    public EnhanceGenerator<B> injection(Consumer<InjectionConfig.Builder> consumer) {
        consumer.accept(injectionConfigBuilder);
        return this;
    }

    @Override
    public EnhanceGenerator<B> custom(Consumer<B> consumer) {
        consumer.accept(customConfigBuilder);
        return this;
    }

    /**
     * mapper.xml文件在项目根目录/src/main/resources/下的对应目录
     *
     * @param path 目录
     * @return {@link AbstractGenerator }<{@link B }>
     * @author bootystar
     */
    @Override
    public EnhanceGenerator<B> mapperXmlResource(String path) {
        String projectPath = System.getProperty("user.dir");
        if (path.startsWith("/")) {
            path = path.substring(1);
        }
        packageConfigBuilder.pathInfo(Collections.singletonMap(OutputFile.mapper, projectPath + "/src/main/resources/" + path));
        return this;
    }

    /**
     * 初始化常用配置项
     *
     * @return {@link AbstractGenerator }<{@link B }>
     * @author bootystar
     */
    @Override
    public EnhanceGenerator<B> initialize() {
        globalConfigBuilder
                .disableOpenDir()
        ;
        customConfigBuilder
                .editExcludeColumns("create_time", "update_time", "create_by", "update_by", "created_by", "updated_by", "create_at", "update_at", "created_at", "updated_at")
                .sortColumn("order", false)
                .sortColumn("rank", false)
                .sortColumn("sort", false)
                .sortColumn("seq", false)
                .sortColumn("sequence", false)
                .sortColumn("create_time", true)
                .sortColumn("id", true)
        ;
        strategyConfigBuilder.entityBuilder()
                .idType(IdType.ASSIGN_ID)
                .logicDeleteColumnName("deleted")
                .versionColumnName("version")
                .disableSerialVersionUID()
                .enableLombok()
        ;
        strategyConfigBuilder.mapperBuilder()
//                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
        ;
        strategyConfigBuilder.serviceBuilder()
//                .formatServiceFileName("%sService")
        ;
        strategyConfigBuilder.controllerBuilder()
                .enableRestStyle()
        ;
        return this;
    }


    /**
     * 启用全局文件覆盖
     *
     * @return {@link AbstractGenerator }<{@link B }>
     * @author bootystar
     */
    @Override
    public EnhanceGenerator<B> enableGlobalFileOverwrite() {
        customConfigBuilder
                .enableFileOverride()
        ;
        strategyConfigBuilder.entityBuilder()
                .enableFileOverride()
        ;
        strategyConfigBuilder.mapperBuilder()
                .enableFileOverride()
        ;
        strategyConfigBuilder.serviceBuilder()
                .enableFileOverride()
        ;
        strategyConfigBuilder.controllerBuilder()
                .enableFileOverride()
        ;
        return this;
    }

}
