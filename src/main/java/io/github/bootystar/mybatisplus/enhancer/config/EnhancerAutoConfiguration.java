package io.github.bootystar.mybatisplus.enhancer.config;

import lombok.extern.slf4j.Slf4j;
import org.aopalliance.aop.Advice;
import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.BeansException;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author bootystar
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass({SqlSessionFactory.class})
public class EnhancerAutoConfiguration implements ApplicationContextAware {


    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        SqlSessionFactory sqlSessionFactory = applicationContext.getBean(SqlSessionFactory.class);
        Configuration configuration = sqlSessionFactory.getConfiguration();
        // 指定Mapper XML文件的路径
        String resource = "mapper/DynamicMapper.xml";
        try (InputStream inputStream = Resources.getResourceAsStream(resource)){
            // 创建XMLMapperBuilder并解析Mapper XML文件
            XMLMapperBuilder mapperBuilder = new XMLMapperBuilder(inputStream, configuration, resource, configuration.getSqlFragments());
            mapperBuilder.parse();
            log.debug("DynamicMapper sqlFragments Configured");
        } catch (IOException e) {
            log.error("error creating DynamicMapper sqlFragments",e);
        }
    }


}
