package io.github.bootystar.mybatisplus.generate.config.impl;

import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicSqlSqlHelper;
import io.github.bootystar.mybatisplus.generate.config.base.CustomConfig;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import lombok.Getter;

/**
 * @author bootystar
 */
@Getter
public class DynamicSqlConfig extends CustomConfig {

    {
        selectDTO = new ClassInfo(SqlHelper.class);
        mapperDTO = new ClassInfo(DynamicSqlSqlHelper.class);
    }

    public static class Builder extends CustomConfig.Builder<DynamicSqlConfig, Builder> {


        public Builder() {
            super(new DynamicSqlConfig());
        }

        /**
         * 不生成重写的方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public Builder disableOverrideMethods() {
            this.getConfig().overrideMethods = false;
            return this.getBuilder();
        }


    }


}


