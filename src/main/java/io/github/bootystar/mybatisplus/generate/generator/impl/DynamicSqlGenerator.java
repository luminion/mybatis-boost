package io.github.bootystar.mybatisplus.generate.generator.impl;

import io.github.bootystar.mybatisplus.enhance.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.core.impl.DynamicSqlServiceImpl;
import io.github.bootystar.mybatisplus.generate.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generate.generator.core.AbstractGenerator;

/**
 * 动态sql型生成器
 *
 * @author bootystar
 */
public class DynamicSqlGenerator extends AbstractGenerator<DynamicSqlConfig, DynamicSqlConfig.Builder> {

    public DynamicSqlGenerator(String url, String username, String password) {
        super(url, username, password, new DynamicSqlConfig.Builder());
        this.strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/templates/dynamic/service.java" )
                .serviceImplTemplate("/templates/dynamic/serviceImpl.java" )
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/templates/dynamic/mapper.java" )
                .mapperXmlTemplate("/templates/dynamic/mapper.xml" )
        ;
        this.strategyConfigBuilder.serviceBuilder()
                .superServiceClass(DynamicService.class)
                .superServiceImplClass(DynamicSqlServiceImpl.class)
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .superClass(DynamicMapper.class)
        ;
    }

}
