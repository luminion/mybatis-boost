package com.example.config;

import com.github.pagehelper.PageInterceptor;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Configuration;

/**
 * @author luminion
 */
@Configuration
public class PageHelperConfig implements ApplicationContextAware {


    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        SqlSessionFactory bean = applicationContext.getBean(SqlSessionFactory.class);
        org.apache.ibatis.session.Configuration configuration = bean.getConfiguration();
        configuration.addInterceptor(new PageInterceptor());
    }
}
